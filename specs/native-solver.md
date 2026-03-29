# Native Constraint Solver for Igor

A pure Clojure propagator-based constraint solver, inspired by Gecode's
architecture, leveraging immutable data structures and Clojure's parallelism
as an alternate backend to MiniZinc.

## Architecture Overview

```
Igor Frontend (unchanged)
        |
        v
  Expansion + Flattening (existing)
        |
        v
  Backend Dispatch  {:solver :native} / {:solver :minizinc}
        |                       |
        v                       v
  native/engine.clj         solver.clj + adapter.clj
  native/domains.clj        (existing MiniZinc path)
  native/propagators.clj
  native/search.clj
  native/globals.clj
  native/graph.clj
```

### Core Concepts

**Store**: An immutable map `{decision-id -> domain}` representing the current
state of all variable domains. Analogous to a Gecode Space, but persistent data
structures give us structural sharing on "copy" for free.

**Domain**: A representation of the possible values for a variable. Three
concrete representations:
- `IntervalDomain` — `{:min n :max m}` for bounds consistency (O(1) operations)
- `EnumeratedDomain` — sorted-set of explicit values (for GAC-level propagation)
- `SetDomain` — `{:glb #{...} :lub #{...}}` for set variables (greatest lower
  bound / least upper bound)

**Propagator**: A map `{:id, :vars, :propagate-fn, :priority, :event-mask}`.
The `:propagate-fn` is a pure function `(store) -> {:store store' :events #{...}}`
or `::failed`. Propagators may use transients internally for performance but must
return persistent data at the boundary.

**Events**: When a domain is narrowed, events are generated:
- `:assigned` — domain reduced to single value
- `:bounds` — min or max changed
- `:domain` — any value removed (interior change)
- `:glb-change` — set GLB grew
- `:lub-change` — set LUB shrank

**Propagator Queue**: A priority queue of propagators awaiting execution. Lower
priority numbers run first (cheap propagators before expensive ones).

**Fixpoint Engine**: Repeatedly dequeues and runs propagators until the queue is
empty (fixpoint) or a propagator returns `::failed` (contradiction).

**Search**: DFS over the search tree. At each node, select an unassigned
variable, create branches (e.g., `x = v` and `x != v`), propagate to fixpoint
on each branch. Immutable store means branches share structure automatically.

---

## Reference Code

Cloned into `refs/` for implementation reference:

- `refs/gecode/` — Gecode source (MIT). Key directories:
  - `gecode/int/rel/` — simple relational propagators (eq, lq, le)
  - `gecode/int/distinct/` — alldifferent (val, bnd, dom consistency)
  - `gecode/int/element/` — element constraint
  - `gecode/int/linear/` — linear constraints
  - `gecode/int/circuit/` — circuit constraint
  - `gecode/int/extensional/` — table and regular
  - `gecode/set/` — set variable propagators
- `refs/libminizinc/` — libminizinc source (MPL-2.0). Key files:
  - `share/minizinc/std/fzn_*.mzn` — decompositions of all global/graph constraints

---

## Phases

### Phase 0: Backend Dispatch Infrastructure

**Goal**: Wire up a `:solver` option so `satisfy`, `minimize`, `maximize` can
dispatch to either `:minizinc` (existing) or `:native` (new) backend, with
`:minizinc` remaining the default.

**Files to modify**:
- `src/routaverra/igor/solver.clj` — extract a `solve-native` entry point
- `src/routaverra/igor.clj` — thread `:solver` option through public API

**Files to create**:
- `src/routaverra/igor/native/engine.clj` — stub that receives the flattened
  constraint list, decision map, bindings map, and objective/direction, then
  delegates to the propagation + search pipeline

**Design**:

```clojure
;; In solver.clj, the existing `solve` function handles MiniZinc generation.
;; Add a conditional dispatch at the top:

(defn solve [{:keys [solver] :as opts :or {solver :minizinc}}
             constraint objective]
  (case solver
    :minizinc (solve-minizinc opts constraint objective)
    :native   (native/solve-native opts constraint objective)))
```

The native entry point receives the same expanded/flattened constraint list
that the MiniZinc path uses. It extracts:
1. `merged-decisions` — `{Decision -> {Type -> witness}}`
2. `merged-bindings` — `{Decision -> [sorted-set, source]}`
3. `constraints` — flat list of term records (post-expansion, post-flattening)
4. `objective` / `direction` — for optimization

It then:
1. Builds the initial store from bindings (domains for each decision)
2. Compiles each constraint term into one or more propagators
3. Runs the fixpoint engine
4. Runs search if needed
5. Returns solution map `{Decision -> value}` (same format as MiniZinc path)

**Verification**: Existing MiniZinc tests still pass. A trivial native test
(e.g., `(satisfy (i/= x 5) {:solver :native})`) returns a correct solution.

---

### Phase 1: Domains and Fixpoint Engine

**Goal**: Implement domain representations and the core propagation fixpoint
loop. No search yet — just propagation on an already-constrained store.

**Files to create**:
- `src/routaverra/igor/native/domains.clj`
- `src/routaverra/igor/native/engine.clj`

#### 1a. Domain Representations

```clojure
(ns routaverra.igor.native.domains)

;; --- Integer Domains ---

;; IntervalDomain: just min/max bounds. O(1) for all operations.
;; Used by default for integer variables.
(defrecord IntervalDomain [min max])

;; EnumeratedDomain: explicit sorted set of values.
;; Used when interior values are removed (e.g., != constraint).
(defrecord EnumeratedDomain [values]) ; values is a sorted-set

;; --- Set Domains ---

;; SetDomain: GLB (must-be-in) and LUB (may-be-in).
;; GLB is a subset of LUB. When GLB = LUB, the set variable is assigned.
(defrecord SetDomain [glb lub]) ; both sorted-sets

;; --- Boolean Domains ---
;; Booleans are just EnumeratedDomain with #{true false}, #{true}, or #{false}.
;; Or IntervalDomain {0,1} mapped through.
;; Simplest: use a keyword tag:
;; {:type :bool, :values #{true false}}
```

**Domain operations** (all pure, return new domain or `::failed`):

```clojure
;; Integer domain protocol
(defprotocol IDomain
  (domain-min [d])
  (domain-max [d])
  (domain-size [d])
  (assigned? [d])
  (contains-value? [d v])
  (intersect [d other])       ; d AND other
  (remove-value [d v])        ; d \ {v}
  (restrict-min [d new-min])  ; d AND [new-min, +inf)
  (restrict-max [d new-max])  ; d AND (-inf, new-max]
  (domain-values [d])         ; seq of all values (lazy for intervals)
  (to-enumerated [d]))        ; convert to EnumeratedDomain

;; Set domain protocol
(defprotocol ISetDomain
  (glb [d])
  (lub [d])
  (set-assigned? [d])
  (require-element [d v])     ; move v into GLB
  (exclude-element [d v])     ; remove v from LUB
  (set-glb [d new-glb])       ; set GLB to at least new-glb
  (set-lub [d new-lub]))      ; set LUB to at most new-lub
```

**Event generation**: Each domain operation returns `[new-domain events]` where
events is a set of `#{:assigned :bounds :domain}`. If the domain becomes empty,
return `::failed`.

```clojure
(defn restrict-min [^IntervalDomain d new-min]
  (let [new-min (clojure.core/max (:min d) new-min)]
    (cond
      (> new-min (:max d)) ::failed
      (= new-min (:min d)) [d #{}]
      (= new-min (:max d)) [(->IntervalDomain new-min new-min) #{:assigned :bounds}]
      :else                [(->IntervalDomain new-min (:max d)) #{:bounds}])))
```

#### 1b. Store

```clojure
;; Store is just a persistent map: {decision-id -> domain}
;; Operations:

(defn make-store [decisions bindings]
  ;; Build initial store from decision types and bindings
  ...)

(defn update-domain [store decision-id new-domain]
  ;; Returns [store' events] or ::failed
  ...)

(defn get-domain [store decision-id]
  ...)
```

#### 1c. Fixpoint Engine

```clojure
(ns routaverra.igor.native.engine)

(defn propagate-fixpoint
  "Run propagators to fixpoint. Returns updated store or ::failed.

   propagators: vector of propagator maps
   store: initial store
   queue: initial set of propagator indices to run"
  [propagators store queue]
  (loop [store store
         queue queue]  ; priority queue (sorted-set of [priority prop-idx])
    (if (empty? queue)
      store
      (let [[_ prop-idx] (first queue)
            queue (disj queue (first queue))
            prop (nth propagators prop-idx)
            result ((:propagate-fn prop) store)]
        (if (= result ::failed)
          ::failed
          (let [{:keys [store events]} result
                ;; Find propagators affected by the events
                affected (affected-propagators propagators prop-idx events)
                queue (into queue (map (fn [idx]
                                         [(get-in propagators [idx :priority]) idx])
                                       affected))]
            (recur store queue)))))))
```

**Propagator-variable subscriptions**: A reverse index
`{decision-id -> {event-type -> #{prop-indices}}}` built at setup time.
When a propagator produces events for a variable, look up which other
propagators subscribe to that variable+event and enqueue them.

**Verification**:
- Unit tests for all domain operations (restrict-min, restrict-max,
  remove-value, intersect) including edge cases (empty domain, already
  assigned, no-op).
- Unit test for fixpoint engine with hand-built propagators:
  - Two variables x:[0,9], y:[0,9], propagators for `x + y = 10` and `x > y`.
    Verify fixpoint produces x:[6,9], y:[1,4].
  - Propagator that immediately fails. Verify `::failed` returned.
  - Propagator chain: x=y, y=z, z=5. Verify all assigned to 5.

---

### Phase 2: Basic Propagators

**Goal**: Implement propagators for all arithmetic, comparison, and logical
operations that Igor supports. These map 1:1 to the term records in
`terms/core.clj`.

**Files to create**:
- `src/routaverra/igor/native/propagators.clj`

**Approach**: A multimethod or protocol that takes a flattened term record and
returns one or more propagator maps. Each propagator map contains:

```clojure
{:id        (gensym "prop-")
 :vars      #{decision-ids...}     ; variables this propagator touches
 :events    {:x #{:bounds}         ; which events on which vars trigger this
             :y #{:bounds}}
 :priority  2                      ; lower = runs first
 :propagate-fn (fn [store] ...)}   ; the propagation function
```

#### Constraint Compilation

```clojure
(defmulti compile-constraint
  "Compile a term record into a seq of propagator maps."
  (fn [term var-lookup] (type term)))
```

`var-lookup` maps from Decision records to their store keys (decision ids).

#### Arithmetic Propagators (bounds consistency)

All arithmetic propagators operate on interval bounds. Reference:
`refs/gecode/gecode/int/linear/` and `refs/gecode/gecode/int/arithmetic/`.

**TermEquals** `(= x y)` or `(= x 5)`:
```clojure
;; Propagation: intersect domains of x and y.
;; For (= x 5): restrict x to {5}.
;; Events: subscribe to :bounds on both sides.
;; Priority: 1 (very cheap).
```

**TermNotEquals** `(!= x y)`:
```clojure
;; Propagation: if x is assigned to v, remove v from y's domain (and vice versa).
;; Only fires on :assigned events.
;; Priority: 1.
```

**TermGreater** `(> x y)`:
```clojure
;; Propagation:
;;   x.min = max(x.min, y.min + 1)
;;   y.max = min(y.max, x.max - 1)
;; Subscribe: :bounds on both.
;; Priority: 1.
;; Reference: Gecode Lq propagator (inverted).
```

**TermLess** `(< x y)`: mirror of TermGreater.

**TermGte** `(>= x y)`:
```clojure
;; Propagation:
;;   x.min = max(x.min, y.min)
;;   y.max = min(y.max, x.max)
;; Subscribe: :bounds on both.
;; Priority: 1.
```

**TermLte** `(<= x y)`: mirror of TermGte.

**TermPlus** `(= z (+ x y))` (flattened form):
```clojure
;; Propagation (bounds):
;;   z.min = x.min + y.min
;;   z.max = x.max + y.max
;;   x.min = z.min - y.max
;;   x.max = z.max - y.min
;;   y.min = z.min - x.max
;;   y.max = z.max - x.min
;; Subscribe: :bounds on x, y, z.
;; Priority: 2.
;; Reference: Gecode gecode/int/linear/bnd.hpp (EqBin)
```

**TermMinus** `(= z (- x y))`:
```clojure
;; Rewrite as z + y = x. Same propagation rules as Plus, rearranged.
```

**TermProduct** `(= z (* x y))`:
```clojure
;; Propagation (bounds):
;;   Compute all four products of bound combinations:
;;     {x.min*y.min, x.min*y.max, x.max*y.min, x.max*y.max}
;;   z.min = min of those, z.max = max of those.
;;   Inverse: x.min/max from z and y bounds (careful with division by zero).
;; Subscribe: :bounds on x, y, z.
;; Priority: 3.
;; Reference: Gecode gecode/int/arithmetic/mult.hpp
```

**TermDivide** `(= z (/ x y))`:
```clojure
;; Integer division. Similar bound products with floor division.
;; Must handle y containing 0 carefully.
;; Priority: 3.
```

**TermModulo** `(= z (mod x y))`:
```clojure
;; z in [0, |y|-1] when y > 0. Bounds propagation.
;; Priority: 3.
```

**TermRemainder** `(= z (rem x y))`:
```clojure
;; Truncated division remainder. Similar to mod with sign handling.
;; Priority: 3.
```

**TermInc/TermDec**: Rewrite as `(= z (+ x 1))` / `(= z (- x 1))`.

**TermMin** `(= z (min x y))`:
```clojure
;; z.min = min(x.min, y.min)
;; z.max = min(x.max, y.max)
;; x.min = z.min, y.min = z.min  (if z.min > x.min or y.min)
;; Priority: 2.
```

**TermMax** `(= z (max x y))`: mirror of TermMin.

**TermAbs** `(= z (abs x))`:
```clojure
;; z.min = if x.min >= 0 then x.min else if x.max <= 0 then -x.max else 0
;; z.max = max(-x.min, x.max)
;; Priority: 2.
```

**TermPow** `(= z (pow x n))`:
```clojure
;; n is always ground in Igor. Bounds propagation via nth-root for inverse.
;; Priority: 3.
```

#### Logical Propagators

Booleans are represented as integer domains {0,1} internally, where
0 = false, 1 = true.

**TermAnd** `(= z (and x y))`:
```clojure
;; z=1 -> x=1 AND y=1
;; x=0 OR y=0 -> z=0
;; z=0 AND x=1 -> y=0 (and vice versa)
;; Subscribe: :assigned on x, y, z.
;; Priority: 1.
```

**TermOr** `(= z (or x y))`:
```clojure
;; z=0 -> x=0 AND y=0
;; x=1 OR y=1 -> z=1
;; z=1 AND x=0 -> y=1 (and vice versa)
;; Priority: 1.
```

**TermNot** `(= z (not x))`:
```clojure
;; z = 1 - x. Intersect domains accordingly.
;; Priority: 1.
```

**TermImplies** `(implies x y)` = `(or (not x) y)`:
```clojure
;; x=1 -> y=1
;; y=0 -> x=0
;; Priority: 1.
```

**TermIff** `(= z (iff x y))`:
```clojure
;; z=1 iff x=y. Bidirectional equality for booleans.
;; Priority: 1.
```

#### Conditional Propagators

**TermIf** `(= z (if b x y))`:
```clojure
;; b=1 -> z=x (intersect z and x domains)
;; b=0 -> z=y (intersect z and y domains)
;; z outside x's domain -> b=0; z outside y's domain -> b=1
;; Priority: 2.
```

**TermCond**: Decomposed to nested TermIf during expansion.

#### Predicate Propagators

**TermEven** `(even? x)`: rewrite as `(= (mod x 2) 0)` — already handled by
expansion phase.

Similarly: `TermOdd`, `TermPos`, `TermNeg`, `TermZero`, `TermTrue`, `TermFalse`
— all expand to simpler constraints before reaching the native solver.

#### Reification

Many constraints appear in reified form after flattening:
`(= b (> x y))` means "b is true iff x > y".

This is handled naturally because flattening introduces `(= introduced_var expr)`,
so the equality propagator for `(= b (> x y))` needs to handle the case where
b is a boolean introduced variable and the RHS is a comparison term. The
compilation step recognizes this pattern and emits a reified propagator:

```clojure
;; Reified greater-than: b <-> (x > y)
;; b=1 -> propagate x > y normally
;; b=0 -> propagate x <= y
;; x.min > y.max -> b=1
;; x.max <= y.min -> b=0
```

**Verification**:
- Test each propagator in isolation with hand-crafted stores.
- Integration tests that compile + propagate simple constraints:
  - `(= x 5)` with `x:[0,9]` -> `x:5`
  - `(> x y)` with `x:[0,9], y:[0,9]` -> `x:[1,9], y:[0,8]`
  - `(= z (+ x y))` with all `[0,9]` -> `z:[0,18]`
  - `(and (= z (+ x y)) (= z 10) (> x y))` -> `x:[6,9], y:[1,4]`
- Cross-validate against MiniZinc: for each test, also solve with
  `:minizinc` backend and verify same solution set.

---

### Phase 3: Search

**Goal**: Implement DFS search with variable/value selection heuristics.
This makes the native solver actually capable of finding solutions.

**Files to create**:
- `src/routaverra/igor/native/search.clj`

**Files to modify**:
- `src/routaverra/igor/native/engine.clj` — integrate search into solve pipeline

#### Search Algorithm

```clojure
(defn solve-dfs
  "Depth-first search with propagation at each node.
   Returns first solution found, or nil."
  [propagators store subscriptions opts]
  (let [store (propagate-fixpoint propagators store (all-prop-indices propagators) subscriptions)]
    (cond
      (= store ::failed) nil
      (all-assigned? store) (extract-solution store)
      :else
      (let [;; Select variable to branch on
            var (select-variable store opts)
            ;; Select value to try first
            val (select-value store var opts)
            ;; Branch 1: var = val
            store1 (assign-value store var val)
            sol (solve-dfs propagators store1 subscriptions opts)]
        (if sol
          sol
          ;; Branch 2: var != val
          (let [store2 (remove-value-from-store store var val)]
            (when-not (= store2 ::failed)
              (solve-dfs propagators store2 subscriptions opts))))))))
```

#### Variable Selection Heuristics

```clojure
(defn select-variable [store opts]
  (case (:var-select opts :first-fail)
    :first-fail    ;; smallest domain first (most constrained)
    (apply min-key #(domain-size (get-domain store %))
           (unassigned-vars store))

    :input-order   ;; order of declaration
    (first (unassigned-vars store))

    :most-constrained  ;; same as first-fail, break ties by most propagators
    ...))
```

#### Value Selection

```clojure
(defn select-value [store var opts]
  (case (:val-select opts :min)
    :min    (domain-min (get-domain store var))
    :max    (domain-max (get-domain store var))
    :median (domain-median (get-domain store var))))
```

#### All-Solutions Search

```clojure
(defn solve-all-dfs
  "Find all solutions via DFS."
  [propagators store subscriptions opts]
  (let [store (propagate-fixpoint propagators store (all-prop-indices propagators) subscriptions)]
    (cond
      (= store ::failed) []
      (all-assigned? store) [(extract-solution store)]
      :else
      (let [var (select-variable store opts)
            val (select-value store var opts)
            store1 (assign-value store var val)
            sols1 (solve-all-dfs propagators store1 subscriptions opts)
            store2 (remove-value-from-store store var val)]
        (if (= store2 ::failed)
          sols1
          (into sols1 (solve-all-dfs propagators store2 subscriptions opts)))))))
```

#### Optimization (Branch and Bound)

```clojure
(defn solve-optimize
  "Branch-and-bound optimization.
   direction: :minimize or :maximize
   objective: decision-id of the objective variable"
  [propagators store subscriptions objective direction opts]
  (loop [store store
         best nil
         bound nil]
    (let [;; Add bound constraint if we have a previous solution
          store (if bound
                  (apply-bound store objective direction bound)
                  store)
          sol (solve-dfs propagators store subscriptions opts)]
      (if sol
        (let [obj-val (get sol objective)
              new-bound (case direction
                          :minimize (dec obj-val)
                          :maximize (inc obj-val))]
          (recur store sol new-bound))
        best))))

(defn apply-bound [store objective direction bound]
  "Add objective bound to store. Returns store or ::failed."
  (case direction
    :minimize (restrict-max-in-store store objective bound)
    :maximize (restrict-min-in-store store objective bound)))
```

**Verification**:
- N-Queens for N=4,5,6,7,8 — verify correct solution count with `satisfy-all`.
- SEND+MORE=MONEY — verify unique solution matches known answer.
- Simple optimization: `(minimize x (and (> x 3) (< x 10)))` -> x=4.
- Compare against MiniZinc on all tests.

---

### Phase 4: Global Constraint Propagators

**Goal**: Implement the critical global constraints that provide orders of
magnitude better pruning than decomposition.

**Files to create**:
- `src/routaverra/igor/native/globals.clj`

#### 4a. AllDifferent (Bounds Consistency)

Reference: `refs/gecode/gecode/int/distinct/bnd.hpp`

Algorithm: Hall intervals (Lopez-Ortiz et al., IJCAI 2003).

```
Given variables x1..xn with domains [l1,u1]..[ln,un]:
1. Sort by min, then by max.
2. Find Hall intervals: a set S of variables whose combined domain
   range has exactly |S| values. These values are "consumed" —
   other variables cannot use them.
3. Prune: for each variable not in a Hall set, remove Hall interval
   values from its domain bounds.
```

Complexity: O(n log n) per propagation call.

Priority: 3 (medium cost).

Subscribe: `:bounds` on all variables.

```clojure
(defn alldifferent-bounds-propagator [vars]
  {:id (gensym "alldiff-bnd-")
   :vars (set vars)
   :events (zipmap vars (repeat #{:bounds}))
   :priority 3
   :propagate-fn
   (fn [store]
     ;; 1. Extract bounds for each var
     ;; 2. Sort by min
     ;; 3. Find Hall intervals using union-find
     ;; 4. Prune bounds
     ;; Use transients internally for the union-find
     ...)})
```

#### 4b. Element Constraint

`(= z (nth array x))` — variable indexing into an array.

Reference: `refs/gecode/gecode/int/element/`

Propagation:
1. Remove from x's domain any index i where `array[i]` is not in z's domain.
2. Set z's domain to the union of `{array[i] | i in x's domain}`.
3. If x is assigned, set z = array[x].

Subscribe: `:domain` on x, `:bounds` on z (or `:domain` for GAC).

Priority: 3.

#### 4c. Count / Global Cardinality

`(= n (count s))` for set cardinality, or
`(count-eq xs v n)` — n occurrences of value v in array xs.

For set cardinality: `n.min = |GLB|`, `n.max = |LUB|`.

For value counting: bounds propagation based on min/max possible occurrences.

Priority: 3.

#### 4d. Table Constraint

`(table [x y z] [[1 2 3] [4 5 6] ...])` — tuple membership.

Reference: `refs/gecode/gecode/int/extensional/` (compact-table algorithm).

Algorithm (simplified):
1. Represent allowed tuples as a bitset.
2. For each variable, maintain a "support" bitset — tuples that are compatible
   with the current domain.
3. On propagation, AND all support bitsets. The result is the set of
   still-viable tuples.
4. For each variable, remove values that have no supporting tuple.

For small tuple counts, a simpler approach works:
1. Filter tuples to those consistent with current domains.
2. For each variable position, compute the set of values appearing in
   surviving tuples.
3. Intersect each variable's domain with that set.

Priority: 4 (can be expensive for large tables).

Subscribe: `:domain` on all variables.

**Verification**:
- AllDifferent: N-Queens solving time should improve dramatically vs. decomposed
  `!=` pairs. Verify same solutions.
- Element: `(= z (nth [10 20 30] x))` with x:[0,2] -> z:{10,20,30}.
- Table: verify against MiniZinc table constraint on same tuples.
- Run the existing `constraint_problems_test.clj` problems (magic square,
  set partition, SEND+MORE) with `:native` backend.

---

### Phase 5: Set Variable Propagators

**Goal**: Support set decision variables with GLB/LUB propagation.

**Files to create**:
- `src/routaverra/igor/native/sets.clj`

#### Set Domain Operations

Igor's set operations (from `terms/set.clj`):

**Intersection** `(= z (intersection s1 s2))`:
```
z.glb >= s1.glb AND s2.glb  (elements in both GLBs must be in z)
z.lub <= s1.lub AND s2.lub  (elements not in either LUB can't be in z)
s1.glb: if v in z.glb and v in s2.glb, then v must be in s1
s2.glb: symmetric
```

**Union** `(= z (union s1 s2))`:
```
z.glb >= s1.glb OR s2.glb
z.lub <= s1.lub OR s2.lub
If v in z.glb and v not in s2.lub, then v must be in s1.glb
```

**Difference** `(= z (difference s1 s2))`:
```
z.lub <= s1.lub \ s2.glb
z.glb >= s1.glb \ s2.lub
```

**Subset** `(subset? s1 s2)`:
```
s1.lub <= s2.lub  (nothing in s1 that can't be in s2)
s2.glb >= s1.glb  (everything definitely in s1 must be in s2)
```

**Contains** `(contains? s v)`:
```
v must be in s.glb (move to GLB if not already).
If v is a variable: v's domain intersected with s.lub.
```

**Count (cardinality)** `(= n (count s))`:
```
n.min = max(n.min, |s.glb|)
n.max = min(n.max, |s.lub|)
If n is assigned to k:
  If |s.glb| = k: s.lub = s.glb (no more elements)
  If |s.lub| = k: s.glb = s.lub (all elements required)
```

**Every/Some (quantifiers)**:
These are compiled by expanding the quantifier body for each element in the
set's LUB, with conditional activation based on set membership.

#### Set-Integer Channeling

When a set variable's elements are related to integer variables (e.g., via
`contains?` or `image`), channeling propagators keep them in sync.

**Verification**:
- `(subset? s1 s2)` with `s1.lub = #{1 2 3}, s2.lub = #{2 3 4}` ->
  `s1.lub = #{2 3}`.
- Set partition problems from `constraint_problems_test.clj`.
- Set cardinality propagation.
- Cross-validate with MiniZinc.

---

### Phase 6: Extensional Constraints

**Goal**: Support `table`, `regular`, and `cost-regular` constraints natively.

**Files to create**:
- `src/routaverra/igor/native/extensional.clj`

#### 6a. Table (already outlined in Phase 4d)

Promote from Phase 4 sketch to full implementation with bitset-based
compact-table if needed for performance.

#### 6b. Regular Constraint

`(regular [x1..xn] dfa)` — sequence must be accepted by a DFA.

Reference: `refs/gecode/gecode/int/extensional/` (layered graph algorithm).

Algorithm:
1. Build a layered graph: layer i has states reachable after consuming
   x1..xi.
2. Forward pass: starting from DFA start state, for each position i and
   each value v in xi's domain, check if transition(state, v) exists.
   Keep only reachable (state, layer) pairs.
3. Backward pass: starting from accept states at layer n, prune states
   that can't reach an accept state.
4. For each position i, remove values from xi's domain that have no
   valid transition from any reachable state at layer i to any reachable
   state at layer i+1.

Subscribe: `:domain` on all sequence variables.

Priority: 4.

#### 6c. Cost-Regular Constraint

`(cost-regular [x1..xn] cost dfa-with-costs)` — like regular but with
a cost variable tracking cumulative transition costs.

Extension of regular: same layered graph, but each arc also carries a cost.
Shortest/longest path through the layered graph gives bounds on the cost
variable.

**Verification**:
- DFA that accepts sequences with no two consecutive 1s.
- Cost-regular for shift scheduling (from Igor's extensional_test.clj).
- Cross-validate with MiniZinc.

---

### Phase 7: Graph Constraints

**Goal**: Native propagation for Igor's graph constraints. These decompose
into set variable propagation + graph algorithm filtering.

**Files to create**:
- `src/routaverra/igor/native/graph.clj`

Reference: `refs/libminizinc/share/minizinc/std/fzn_*.mzn` for decomposition
strategies.

Igor's graph model uses boolean decision variables for node/edge selection
(from `graph.clj`): `ns-vars` (bool per node) and `es-vars` (bool per edge).
The native solver represents these as integer domains {0,1}.

#### Strategy

There are two implementation strategies:

**Strategy A: Decomposition** (simpler, less pruning).
Decompose graph constraints into simpler constraints over the boolean node/edge
variables, following the MiniZinc `fzn_*.mzn` decompositions. This reuses the
Phase 2 propagators and requires no graph-specific propagator code.

**Strategy B: Custom propagators** (more work, better pruning).
Implement graph-specific propagators that reason about connectivity, paths, etc.
directly on the boolean variable domains, using graph algorithms internally.

**Recommended approach**: Start with Strategy A for all graph constraints
(since the decompositions are proven correct and available in
`refs/libminizinc/`), then selectively upgrade to Strategy B for constraints
where pruning matters most (connected, tree, circuit).

#### 7a. Subgraph (Strategy A)

Decomposition: for each edge e, `es[e] -> ns[from[e]] AND ns[to[e]]`.

Emits one implication propagator per edge. Very cheap.

#### 7b. Reachable / DReachable (Strategy A, then B)

**Strategy A decomposition** (from `fzn_dreachable_int.mzn`):
- Introduce `parent[n]` and `dist[n]` integer variables per node.
- Root: `ns[r] = 1, dist[r] = 0, parent[r] = r`.
- Each selected non-root node has an incoming edge from its parent.
- Distance increments along parent edges.
- Subgraph constraint on edges.

This creates O(N) integer variables and O(N*E) constraints, which is fine for
small-medium graphs.

**Strategy B** (custom propagator — upgrade later):
- On each propagation, run BFS/DFS on the LUB graph from root.
- Nodes not reachable in LUB: force ns[n] = 0.
- Edges not on any path from root to a required node: force es[e] = 0.
- Bridge edges (whose removal disconnects required nodes): force es[e] = 1.

#### 7c. Connected / DConnected (Strategy A)

Decomposition: `connected` = `reachable` with an existential root choice.
Introduce a variable r for the root, constrain it to be a selected node,
apply reachability from r.

#### 7d. DAG (Strategy A)

Decomposition (from `fzn_dag.mzn`):
- Introduce `dist[n]` variables.
- For each edge: `es[e] -> dist[from[e]] + 1 <= dist[to[e]]`.
- The strict ordering prevents cycles.

#### 7e. Tree / DTree (Strategy A, then B)

**Strategy A**: parent pointers + distance + edge count constraint
(from `fzn_dtree_int.mzn`):
- `sum(es) = sum(ns) - 1` (tree property)
- Reachability from root
- No cycles (implied by parent pointer structure)

**Strategy B** (custom propagator — upgrade later):
- Bridge detection via Tarjan's algorithm on LUB graph.
- Bridge edges between required components must be in GLB.
- Edges creating cycles in GLB must be excluded from LUB.

#### 7f. Path / DPath (Strategy A)

Decomposition (from `fzn_dpath_int.mzn`):
- Forward tree from source + backward tree from destination.
- Their intersection is the path.

Alternative (from `fzn_dpath_enum.mzn`): explicit source/dest degree
constraints + distance array.

#### 7g. Bounded Path (Strategy A)

Decomposition: `path(s, t, ns, es)` + `cost = sum(es[e] * weight[e])`.
Cost variable bounded by the objective.

#### 7h. Circuit (Strategy A, then B)

**Strategy A** (from `fzn_circuit.mzn`):
- Successor variables with `all_different`.
- No self-loops: `x[i] != i`.
- Order array to enforce single cycle: `all_different(order)`.

This benefits hugely from a good `alldifferent` propagator (Phase 4a).

**Strategy B** (custom propagator — from `refs/gecode/gecode/int/circuit/`):
- Successor model with SCC-based filtering.
- No-subtour detection via path following in current assignment.
- Strong connectivity check on LUB graph.

#### 7i. Weighted Spanning Tree (Strategy A)

Decomposition (from `fzn_wst.mzn`):
- Tree constraint with existential root.
- All nodes selected.
- Cost = sum of selected edge weights.

#### Summary Table

| Constraint | Strategy A (decompose) | Strategy B (custom) | When to upgrade |
|---|---|---|---|
| subgraph | trivial | not needed | never |
| reachable | parent+dist vars | BFS/bridge detection | large sparse graphs |
| connected | via reachable | via reachable-B | when reachable upgraded |
| dag | dist ordering | topological sort | rarely needed |
| tree/dtree | parent+dist+count | Tarjan bridges | medium graphs |
| path/dpath | dual tree | forward/backward reach | medium graphs |
| bounded-path | path + cost sum | Dijkstra filtering | optimization problems |
| circuit | alldiff + order | SCC + subtour elim | large TSP-like |
| weighted-spanning-tree | tree + cost sum | matroid filtering | optimization |

**Verification**:
- Run all tests from `graph_test.clj` with `:native` backend.
- Verify active-nodes and active-edges return correct values.
- Cross-validate every graph test against MiniZinc backend.
- Performance comparison on circuit (small TSP instances).

---

### Phase 8: Alternatives and Soft Constraints

**Goal**: Support constructive disjunction (alternatives) and soft constraints.

#### 8a. Alternatives

`(alternatives branch0 branch1 ...)` with `(choice handle)` returning the
selected branch index.

Implementation: introduce a choice variable c:[0, n-1]. For each branch i,
create a reified conjunction: `(c = i) -> branch_i_constraints`. This
decomposes to reified propagators from Phase 2.

#### 8b. Soft Constraints

`(soft constraint penalty)` with `(violation handle)` returning 0 or penalty.

Implementation: introduce a violation variable v:{0, penalty}. Create a
reified constraint: `constraint -> v=0`, `NOT constraint -> v=penalty`.
This is a standard reification.

**Verification**:
- Alternatives test from `alternatives_test.clj`.
- Soft constraint test from `soft_test.clj`.
- Optimization with soft constraints (minimize total violation).

---

### Phase 9: Advanced Search (Optional Enhancement)

**Goal**: Improve search performance with restart strategies and parallel
search. This phase is optional — the solver is functionally complete after
Phase 8.

#### 9a. Restart-Based Search

Implement geometric restart strategy:
```clojure
(defn solve-with-restarts [propagators store subscriptions opts]
  (let [base-cutoff (:restart-base opts 100)
        growth (:restart-growth opts 1.5)]
    (loop [cutoff base-cutoff
           iteration 0]
      (let [result (solve-dfs-limited propagators store subscriptions
                                      (assoc opts :node-limit cutoff))]
        (case (:status result)
          :solution (:solution result)
          :cutoff   (recur (long (* cutoff growth)) (inc iteration))
          :failed   nil)))))
```

#### 9b. Parallel Search

Exploit Clojure's immutable stores for embarrassingly parallel search:

```clojure
(defn solve-parallel [propagators store subscriptions opts]
  (let [;; Propagate root, then generate top-level branches
        store (propagate-fixpoint propagators store ...)
        var (select-variable store opts)
        branches (for [v (domain-values (get-domain store var))]
                   (assign-value store var v))]
    ;; Search branches in parallel
    (->> branches
         (pmap #(solve-dfs propagators % subscriptions opts))
         (filter some?)
         first)))
```

For deeper parallelism, use core.async with a work-stealing pattern.

#### 9c. Nogood Learning (Preparation for future LCG)

Add optional explanation tracking to propagators. Each domain reduction records
*why* it happened (which propagator, which variable bounds caused it). This is
the foundation for conflict-driven clause learning.

Not required for correctness — purely a performance enhancement for future work.

**Verification**:
- Restart search finds same solutions as basic DFS.
- Parallel search finds same solutions, faster on multi-core.
- Benchmark on harder problems (N-Queens N=12+, larger scheduling).

---

## File Layout Summary

```
src/routaverra/igor/
  native/
    engine.clj      -- Phase 0+1: store, fixpoint loop, solve-native entry point
    domains.clj      -- Phase 1: IntervalDomain, EnumeratedDomain, SetDomain
    propagators.clj  -- Phase 2: compile-constraint, arithmetic/logic/comparison
    search.clj       -- Phase 3: DFS, variable/value selection, optimization
    globals.clj      -- Phase 4: alldifferent, element, table
    sets.clj         -- Phase 5: set variable propagators
    extensional.clj  -- Phase 6: regular, cost-regular
    graph.clj        -- Phase 7: graph constraint decomposition/propagation
```

## Testing Strategy

Every phase includes its own verification criteria. Additionally:

1. **Cross-validation**: Every test should run on both `:minizinc` and
   `:native` backends, verifying identical solution sets (for satisfy-all)
   or valid solutions (for satisfy/optimize).

2. **Existing test suite**: The existing tests in `test/routaverra/igor/`
   are the ground truth. Phase 3+ should be able to run progressively more
   of these tests with `:native`.

3. **Performance benchmarks**: Not a correctness gate, but track solving
   time vs. MiniZinc to understand the gap and identify optimization targets.

A test helper:

```clojure
(defn verify-both-backends [constraint & {:keys [all?]}]
  (let [mzn-result (if all?
                     (set (i/satisfy-all constraint))
                     (i/satisfy constraint))
        native-result (if all?
                        (set (i/satisfy-all constraint {:solver :native}))
                        (i/satisfy constraint {:solver :native}))]
    (if all?
      (is (= mzn-result native-result))
      (do
        (is (some? native-result))
        (is (i/validate-solution constraint native-result))))))
```

## Dependency / Ordering Constraints

```
Phase 0 (dispatch) ─── must come first
    |
Phase 1 (domains + engine) ─── foundation for everything
    |
Phase 2 (basic propagators) ─── needed before search works
    |
Phase 3 (search) ─── makes the solver actually useful
    |
    ├── Phase 4 (globals) ─── can be done independently
    ├── Phase 5 (sets) ─── can be done independently
    ├── Phase 6 (extensional) ─── can be done independently
    |
Phase 7 (graph) ─── needs Phase 5 (set domains) for Strategy B
    |              ─── needs Phase 4 (alldifferent) for circuit
    |
Phase 8 (alternatives + soft) ─── needs Phase 2 (reification)
    |
Phase 9 (advanced search) ─── optional, independent
```

Phases 4, 5, and 6 are independent of each other and can be developed in
any order after Phase 3. Phase 7 depends on 4 (for circuit's alldifferent)
and conceptually on 5 (for Strategy B set propagation on graph constraints),
but Strategy A decompositions only need Phase 2+3.
