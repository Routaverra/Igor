Constraint programming for Clojure, backed by [MiniZinc](https://www.minizinc.org/).

> My freedom will be so much the greater and more meaningful the more narrowly I limit my field of action and the more I surround myself with obstacles. Whatever diminishes constraint, diminishes strength. The more constraints one imposes, the more one frees one's self of the chains that shackle the spirit.
>
> — Igor Stravinsky, *Poetics of Music*

Igor shadows Clojure's core operators (`+`, `=`, `and`, `every?`, etc.) to produce constraint expressions that are first-class and data-oriented. You declare variables with domains, compose constraints using familiar Clojure syntax, and industrial solvers (Gecode, OR-Tools, etc.) return solutions as plain maps. It supports integer arithmetic, keywords, set algebra, universal and existential quantification, extensional constraints (table, regular, cost-regular), and a graph constraint library — paths, spanning trees, circuits, connectivity — all composable and all optimizable via maximize/minimize.

## Installation

Add to your `deps.edn`:

```clojure
io.github.routaverra/igor {:git/sha "6b4d550cdc6c3b58f6d62938797dbe831e3fa684"}
```

## Requirements

- Clojure
- [MiniZinc](https://www.minizinc.org/software.html) installed and on PATH

## Quick Start

```clojure
(require '[routaverra.igor :as i])

;; Create two integer variables, each in {0..9}
(let [x (i/fresh-int (range 10))
      y (i/fresh-int (range 10))
      ;; State constraints: x + y = 10, x > y
      solution (i/satisfy (i/and (i/= (i/+ x y) 10)
                                 (i/> x y)))]
  [(solution x) (solution y)])
;; => [6 4]
```

Three steps: create variables with domains, build a constraint expression, call `satisfy`. The solution is a map from your variable records to their solved values.

Igor's operators (`i/+`, `i/=`, `i/and`, etc.) shadow their `clojure.core` counterparts. When all arguments are ground, they evaluate normally (`(i/+ 1 2 3)` => `6`). When any argument is a decision variable, they build an AST node instead. Nothing is sent to MiniZinc until you call `satisfy`, `maximize`, or `minimize`.

## Core Concepts

**Variables** — `fresh-int`, `fresh-bool`, `fresh-set`, `fresh-keyword` create typed decision variables. Domains are passed as collections:

```clojure
(i/fresh-int (range 10))           ; integer in {0..9}
(i/fresh-int #{1 3 5 7})           ; integer in {1,3,5,7}
(i/fresh-bool)                     ; boolean
(i/fresh-set (range 12))           ; subset of {0..11}
(i/fresh-keyword #{:red :blue})    ; keyword from a domain of keywords
```

**Solving** — `satisfy` returns one solution as `{Decision -> value}`. `satisfy-all` returns all solutions. `maximize` / `minimize` take an objective expression and a constraint:

```clojure
(i/satisfy constraint)              ; one solution
(i/satisfy-all constraint)          ; all solutions
(i/maximize objective constraint)   ; maximize objective
(i/minimize objective constraint)   ; minimize objective
```

All solving functions accept an options map with `{:async? true}` to return a `core.async` channel of solution maps. The channel closes when the solver finishes:

```clojure
(i/satisfy constraint {:async? true})      ; -> chan, one solution then closes
(i/satisfy-all constraint {:async? true})  ; -> chan, streams all solutions then closes
```

**Solution maps** — Solutions are maps keyed by the Decision records you created. Use `get` or apply the map as a function:

```clojure
(get solution x)    ; or
(solution x)
```

**Validation** — Cross-check solutions by evaluating the constraint AST in pure Clojure:

```clojure
(i/validate-solution constraint solution)  ; => true/false
```

The examples below use `solve`, a convenience that calls `satisfy` and then walks a form replacing every decision variable with its solved value — so `(i/solve constraint [x y])` returns e.g. `[3 7]` directly.

## Composability

Constraints are plain Clojure data. You compose them with ordinary functions — `map`, `apply`, `partition`, closures — the same patterns you already use.

### Constraint-producing functions

Any function that returns a constraint expression works with `map`, `apply`, and the rest of the sequence library.

```clojure
(defn exceeds-index [vars k]
  (i/> (nth vars k) k))

(let [n    5
      vars (vec (repeatedly n #(i/fresh-int (range 20))))]
  (i/solve (apply i/and (map #(exceeds-index vars %) (range n)))
           vars))
;; => e.g. [1 2 3 4 5] — each value exceeds its index
```

### Higher-order constraint builders

Pass constraint-producing functions to higher-order combinators the same way you would with any other data.

```clojure
(defn pairwise [f vars]
  (apply i/and (map (fn [[a b]] (f a b))
                    (partition 2 1 vars))))
```

Enforce "strictly increasing" and "gaps ≤ 3" on the same variables:

```clojure
(let [vars (vec (repeatedly 5 #(i/fresh-int (range 20))))]
  (i/solve (i/and (pairwise i/< vars)
                  (pairwise (fn [a b] (i/<= (i/- b a) 3)) vars))
           vars))
;; => e.g. [0 1 2 3 4]
```

### Composing constraint modules

Split a problem into independent constraint functions, then assemble them at the call site.

```clojure
(defn row-constraints [grid]
  (apply i/and (for [row grid]
                 (apply i/all-different row))))

(defn col-constraints [grid]
  (let [cols (apply map vector grid)]
    (apply i/and (for [col cols]
                   (apply i/all-different col)))))

(let [grid (vec (for [_ (range 4)]
                  (vec (repeatedly 4 #(i/fresh-int (range 1 5))))))]
  (i/solve (i/and (row-constraints grid)
                  (col-constraints grid))
           grid))
;; => a valid 4×4 Latin square
```

Each constraint function is independently testable, reusable across problems, and invisible to the solver — it just sees one conjunction of data.

## Examples

### SEND + MORE = MONEY

Each letter stands for a different digit (0–9). The goal: find the unique digit assignment that makes the addition work as ordinary base-10 arithmetic. The constraints are that all eight letters map to distinct digits, leading digits (`S` and `M`) can't be zero, and the place-value sums must balance.

```clojure
(let [d (range 10)
      ;; One decision variable per letter, each drawn from 0–9
      s (i/fresh-int d) e (i/fresh-int d) n (i/fresh-int d) d* (i/fresh-int d)
      m (i/fresh-int d) o (i/fresh-int d) r (i/fresh-int d) y  (i/fresh-int d)
      ;; Build the place-value numbers from their digits
      send  (i/+ (i/* s 1000) (i/* e 100) (i/* n 10) d*)
      more  (i/+ (i/* m 1000) (i/* o 100) (i/* r 10) e)
      money (i/+ (i/* m 10000) (i/* o 1000) (i/* n 100) (i/* e 10) y)]
  (i/solve (i/and (apply i/all-different [s e n d* m o r y])
                  (i/= (i/+ send more) money)
                  (i/> s 0)
                  (i/> m 0))
           [s e n d* m o r y]))
;; => [9 5 6 7 1 0 8 2]  — i.e. 9567 + 1085 = 10652
```

### Set Constraints with every?

```clojure
;; Find a subset of {0..11} such that no three consecutive
;; elements (mod 12) are all present — "cluster-free"
(let [x (i/fresh-set (range 12))
      cluster-free
      (i/every? x
        (fn [a]
          (i/implies (i/contains? x (i/mod (i/+ a 1) 12))
            (i/not (i/contains? x (i/mod (i/+ a 2) 12))))))]
  (get (i/satisfy cluster-free) x))
;; => e.g. #{0 1 3 4 6 7 9 10}
```

### Keywords

Keywords let you constrain symbolic values directly — no manual integer encoding. Both namespaced and non-namespaced keywords are supported.

```clojure
;; Graph coloring: assign colors to nodes so no adjacent pair shares a color
(let [edges [[0 1] [0 2] [1 2] [1 3] [2 4] [3 4]]
      colors (vec (repeatedly 5 #(i/fresh-keyword #{:red :green :blue})))
      sol (i/satisfy
           (->> edges
                (map (fn [[u v]] (i/not= (nth colors u) (nth colors v))))
                (apply i/and)))]
  (mapv sol colors))
;; => [:red :green :blue :blue :red]
```

Keywords work with sets too — `fresh-set` auto-detects keyword domains:

```clojure
;; Select a subset of features with dependency constraints
(let [features (i/fresh-set #{:wifi :bluetooth :nfc :gps :lte})
      sol (i/satisfy (i/and (i/contains? features :wifi)
                            (i/implies (i/contains? features :lte)
                                       (i/contains? features :gps))
                            (i/<= (i/count features) 3)
                            (i/contains? features :lte)))]
  (sol features))
;; => #{:wifi :gps :lte}
```

Different keyword variables can draw from different domains in the same problem:

```clojure
;; Product configuration — constraints read like business rules
(let [color (i/fresh-keyword #{:red :blue :black})
      trim  (i/fresh-keyword #{:sport :luxury :base})
      sol (i/satisfy (i/and (i/implies (i/= trim :sport) (i/not= color :blue))
                            (i/implies (i/= trim :luxury) (i/= color :black))
                            (i/= trim :sport)))]
  {:color (sol color) :trim (sol trim)})
;; => {:color :red, :trim :sport}
```

### Graph: Shortest Path

You define a graph as an edge list, then apply structural constraints — paths, trees, connectivity, circuits — and the solver finds subgraphs that satisfy them. The solver selects which nodes and edges are "active"; you read the result with `active-nodes` and `active-edges`.

```clojure
;; Weighted directed graph, minimize path cost from 0 to 3
(let [g      (i/digraph [[0 1 2] [0 2 4] [1 2 1] [1 3 7] [2 3 3]])
      cost   (i/fresh-int (range 1000))
      handle (i/bounded-dpath g 0 3 cost)
      sol    (i/minimize cost handle)]
  {:cost  (sol cost)
   :path  (i/active-edges handle sol)})
;; => {:cost 6, :path #{[0 1] [1 2] [2 3]}}
```

`bounded-dpath` constrains a directed path between two nodes and binds a cost variable to its total weight. The graph constraint functions return a *handle* — a constraint term that also carries the internal node/edge variables needed to read the solution.

### Table Constraint

`table` restricts a tuple of variables to a specific set of allowed combinations. Think of it as an extensional constraint: instead of writing arithmetic relations, you enumerate the valid tuples directly. Useful for lookup tables, allowed transitions, or any relationship easier to list than to express algebraically.

```clojure
;; Constrain [x y z] to one of three allowed triples
(let [x (i/fresh-int (range 10))
      y (i/fresh-int (range 10))
      z (i/fresh-int (range 10))
      allowed [[1 2 3] [4 5 6] [7 8 9]]]
  (i/solve (i/table [x y z] allowed) [x y z]))
;; => one of [1 2 3], [4 5 6], or [7 8 9]
```

### Regular Constraint (DFA)

`regular` constrains a sequence of variables to form a string accepted by a deterministic finite automaton. This is how you enforce sequential patterns — valid orderings, rhythmic patterns, protocol sequences — anything describable as a regular language. You define the DFA as a Clojure map with states, alphabet, transitions, start state, and accepting states.

```clojure
;; Constrain a 4-element sequence: must end with symbol 1
(let [dfa  {:states      2
            :alphabet    #{0 1}
            :transitions [{0 0, 1 1}    ; from state 0: symbol 0 -> state 0, symbol 1 -> state 1
                          {0 0, 1 1}]   ; from state 1: same
            :start       0
            :accept      #{1}}          ; only state 1 is accepting
      vars (vec (repeatedly 4 #(i/fresh-int #{0 1})))]
  (i/solve (i/regular vars dfa) vars))
;; => e.g. [0 1 0 1]  — last element is always 1
```

`cost-regular` extends this with per-transition costs. Each step through the DFA accumulates a cost, and the total is bound to a cost variable you can then optimize:

```clojure
;; 3-symbol sequence, minimize total cost
(let [dfa  {:states 1 :alphabet #{1 2 3}
            :transitions [{1 0, 2 0, 3 0}]   ; single state, all symbols loop back
            :start 0 :accept #{0}
            :costs [{1 5, 2 1, 3 3}]}         ; symbol 1 costs 5, symbol 2 costs 1, ...
      vars (vec (repeatedly 3 #(i/fresh-int #{1 2 3})))
      cost (i/fresh-int (range 100))
      sol  (i/minimize cost (i/cost-regular vars cost dfa))]
  (sol cost))
;; => 3 (all symbols = 2, each costs 1)
```

### Set Operations

```clojure
;; Find a set whose intersection with #{4 5 6 7 8 9} equals #{4 5 6}
(let [a   (i/fresh-set (range 12))
      b   (i/fresh-set (range 12))
      res (i/fresh-set (range 12))
      sol (i/satisfy
           (i/and (i/= b #{4 5 6 7 8 9})
                  (i/= res #{4 5 6})
                  (i/= res (i/intersection a b))))]
  (sol a))
;; => some superset of #{4 5 6} within {0..11}
```

### Image Comprehension

```clojure
;; Find a set X such that {x+1 | x ∈ X} = #{1 2 3}
(let [x   (i/fresh-set (range 12))
      sol (i/satisfy
           (i/= #{1 2 3}
                 (i/image x (fn [a] (i/+ a 1)))))]
  (sol x))
;; => #{0 1 2}
```

For more usage examples, see [test/routaverra/igor/constraint_problems_test.clj](test/routaverra/igor/constraint_problems_test.clj) — N-queens, magic squares, graph coloring, and other classic CSP benchmarks implemented in Igor.

## Operations Reference

In the tables below, `T` denotes a polymorphic type and `*` denotes variadic (two or more arguments).

### Variables

| Function | Description |
|----------|-------------|
| `fresh-int` | `(fresh-int domain)` — integer variable |
| `fresh-bool` | `(fresh-bool)` — boolean variable |
| `fresh-set` | `(fresh-set universe)` — set variable; universe can be integers or keywords |
| `fresh-keyword` | `(fresh-keyword domain)` — keyword variable from a set of keywords |

### Arithmetic

| Operator | Description | Input | Output | Shadows |
|----------|-------------|-------|--------|---------|
| `+` | Addition | Numeric* | Numeric | `clojure.core` |
| `-` | Subtraction | Numeric* | Numeric | `clojure.core` |
| `*` | Multiplication | Numeric* | Numeric | `clojure.core` |
| `/` | Integer division | Numeric* | Numeric | `clojure.core` |
| `inc` | Increment by 1 | Numeric | Numeric | `clojure.core` |
| `dec` | Decrement by 1 | Numeric | Numeric | `clojure.core` |
| `abs` | Absolute value | Numeric | Numeric | `clojure.core` |
| `pow` | Exponentiation | Numeric, Numeric | Numeric | — |
| `max` | Maximum | Numeric* | Numeric | `clojure.core` |
| `min` | Minimum | Numeric* | Numeric | `clojure.core` |
| `mod` | Modulo (Clojure semantics) | Numeric, Numeric | Numeric | `clojure.core` |
| `rem` | Remainder (truncated) | Numeric, Numeric | Numeric | `clojure.core` |

### Comparison

| Operator | Description | Input | Output | Shadows |
|----------|-------------|-------|--------|---------|
| `=` | Equality (Numeric, Bool, Set, or Keyword) | polymorphic* | Bool | `clojure.core` |
| `not=` | Inequality (Numeric, Bool, Set, or Keyword) | polymorphic* | Bool | `clojure.core` |
| `>` | Greater than | Numeric* | Bool | `clojure.core` |
| `<` | Less than | Numeric* | Bool | `clojure.core` |
| `>=` | Greater than or equal | Numeric* | Bool | `clojure.core` |
| `<=` | Less than or equal | Numeric* | Bool | `clojure.core` |

### Logic

| Operator | Description | Input | Output | Shadows |
|----------|-------------|-------|--------|---------|
| `and` | Conjunction | Bool* | Bool | `clojure.core` |
| `or` | Disjunction | Bool* | Bool | `clojure.core` |
| `not` | Negation | Bool | Bool | `clojure.core` |
| `implies` | Implication (test -> body) | Bool, Bool | Bool | — |
| `every?` | `(every? set-expr (fn [elem] bool-expr))` — universal quantification | Set, (T -> Bool) | Bool | `clojure.core` |
| `some` | `(some set-expr (fn [elem] bool-expr))` — existential quantification | Set, (T -> Bool) | Bool | `clojure.core` |

### Predicates

| Operator | Description | Input | Output | Shadows |
|----------|-------------|-------|--------|---------|
| `even?` | Even number | Numeric | Bool | `clojure.core` |
| `odd?` | Odd number | Numeric | Bool | `clojure.core` |
| `pos?` | Positive | Numeric | Bool | `clojure.core` |
| `neg?` | Negative | Numeric | Bool | `clojure.core` |
| `zero?` | Zero | Numeric | Bool | `clojure.core` |
| `true?` | Exactly true | Bool | Bool | `clojure.core` |
| `false?` | Exactly false | Bool | Bool | `clojure.core` |

### Conditionals

| Operator | Description | Input | Output | Shadows |
|----------|-------------|-------|--------|---------|
| `if` | `(if test then else)` | Bool, T, T | T | `clojure.core` |
| `cond` | `(cond test1 expr1 ... :else default)` | (Bool, T)*, T | T | `clojure.core` |

### Collections

| Operator | Description | Input | Output | Shadows |
|----------|-------------|-------|--------|---------|
| `nth` | Index into a vector of expressions | [T...], Numeric | T | `clojure.core` |
| `count` | Set cardinality | Set | Numeric | `clojure.core` |
| `contains?` | Set membership | Set, Numeric/Keyword | Bool | `clojure.core` |
| `all-different` | All arguments must take distinct values | Numeric/Keyword* | Bool | — |

### Set Operations

| Operator | Description | Input | Output | Shadows |
|----------|-------------|-------|--------|---------|
| `intersection` | Set intersection | Set* | Set | `clojure.set` |
| `difference` | Set difference | Set* | Set | `clojure.set` |
| `union` | Set union | Set* | Set | `clojure.set` |
| `sym-diff` | Symmetric difference | Set* | Set | — |
| `subset?` | Subset test | Set* | Bool | `clojure.set` |
| `superset?` | Superset test | Set* | Bool | `clojure.set` |
| `set<` | Strict lexicographic ordering | Set, Set | Bool | — |
| `set<=` | Lexicographic ordering | Set, Set | Bool | — |
| `image` | `(image set-expr (fn [elem] expr))` — set comprehension: `{f(x) \| x ∈ S}` | Set, (Numeric -> Numeric) | Set | — |

### Alternatives

`if` and `cond` select between *values*. `alternatives` selects between *constraint branches* — the solver picks exactly one branch to satisfy, and you can find out which one was chosen after solving. This is useful when each branch carries associated data (different variable sets, metadata, etc.) that you need to index into based on the solver's choice.

| Function | Description | Input | Output | Shadows |
|----------|-------------|-------|--------|---------|
| `alternatives` | `(alternatives & constraints)` — returns a handle that is both a constraint and a choice extractor | Bool* | Handle | — |
| `choice` | `(choice handle)` — the decision variable representing which branch was selected | Handle | Decision | — |

The handle returned by `alternatives` is a constraint — pass it directly to `satisfy`, `and`, etc. `choice` returns the decision variable for the selected branch index. Use it to build expressions that depend on the selection, or resolve it against a solution to find out which branch was chosen:

```clojure
(let [x (i/fresh-int (range 100))
      y (i/fresh-int (range 100))
      ;; Two possible constraint regimes
      alt (i/alternatives
            (i/and (i/< x 10) (i/< y 10))     ;; branch 0: both small
            (i/and (i/> x 90) (i/> y 90)))     ;; branch 1: both large
      ;; Use the choice variable to condition other constraints
      label (i/if (i/= (i/choice alt) 0) 1 2)
      sol (i/satisfy (i/and alt (i/= label 2)))]
  {:branch (get sol (i/choice alt))
   :x (sol x) :y (sol y)})
;; => {:branch 1, :x 91, :y 91}  — forced branch 1 via label constraint
```

### Extensional Constraints

These constrain variables by enumerating valid assignments rather than expressing them as arithmetic/logical formulas.

| Function | Description |
|----------|-------------|
| `table` | `(table [vars] [[tuples]])` — the tuple of variables must match one of the listed tuples |
| `regular` | `(regular [vars] dfa)` — the sequence of variable values must be accepted by the DFA |
| `cost-regular` | `(cost-regular [vars] cost dfa)` — like `regular`, plus each transition accumulates a cost; `cost` is bound to the total |

DFA maps use 0-indexed states:

```clojure
{:states      2                        ; number of states
 :alphabet    #{0 1}                   ; set of input symbols
 :transitions [{0 0, 1 1} {0 0, 1 1}] ; vec of {symbol -> next-state}, one map per state
 :start       0                        ; initial state
 :accept      #{1}                     ; accepting states
 :costs       [{0 1, 1 2} ...]         ; (cost-regular only) {symbol -> cost}, one map per state
 }
```

### Graph Constraints

You define a directed graph as an edge list, then apply constraints that ask the solver to select a subgraph satisfying structural properties — paths, trees, cycles, connectivity. The solver decides which nodes and edges are "active."

```clojure
(i/digraph [[0 1] [1 2] [2 0]])               ; unweighted edges [from to]
(i/digraph [[0 1 5] [1 2 3]])                  ; weighted edges [from to weight]
(i/digraph 5 [[0 1] [1 2]])                    ; explicit node count (for isolated nodes)
```

**Path / reachability** — find routes through the graph:

| Function | Description |
|----------|-------------|
| `path` / `dpath` | Select an undirected / directed path between two nodes |
| `bounded-path` / `bounded-dpath` | Path with a cost variable bound to the total edge weight |
| `reachable` / `dreachable` | Constrain that all active nodes are reachable from a root |

**Structure** — constrain the shape of the selected subgraph:

| Function | Description |
|----------|-------------|
| `subgraph` | Any valid subgraph (active edges only connect active nodes) |
| `connected` / `dconnected` | The active subgraph must be connected / strongly connected |
| `dag` | The active subgraph must have no directed cycles |
| `tree` / `dtree` | Spanning tree / directed arborescence rooted at a given node |
| `circuit` / `subcircuit` | Hamiltonian cycle visiting all nodes / a subset of nodes |
| `weighted-spanning-tree` | Spanning tree with a cost variable bound to total edge weight |
| `d-weighted-spanning-tree` | Directed spanning tree from a root with cost variable |

**Reading solutions** — graph constraints return a *handle* that carries internal node/edge decision variables. After solving, read which nodes and edges were selected:

| Function | Description |
|----------|-------------|
| `active-nodes` | `(active-nodes handle solution)` — set of selected node IDs |
| `active-edges` | `(active-edges handle solution)` — set of `[from to]` pairs |

All nodes are 0-indexed.

### Notation Rendering

| Function | Description |
|----------|-------------|
| `as` | `(as :name expr)` — attach a name for notation rendering |
| `render-notation` | `(render-notation expr :format :latex)` — render constraint as LaTeX or Unicode |
| `render-problem` | `(render-problem constraint :format :unicode :objective obj)` — render full problem statement |

### Solving

| Function | Description |
|----------|-------------|
| `satisfy` | `(satisfy constraint)` — one solution as `{Decision -> value}` |
| `satisfy-all` | `(satisfy-all constraint)` — all solutions |
| `maximize` | `(maximize objective constraint)` — maximize objective |
| `minimize` | `(minimize objective constraint)` — minimize objective |
| `resolve` | `(resolve solution form)` — walks `form`, replacing decisions with solved values and evaluating igor expressions |
| `solve` | `(solve constraint form)` — `satisfy` + `resolve` in one step; returns `nil` when unsatisfiable |
| `validate-solution` | `(validate-solution constraint solution)` — pure-Clojure cross-check |
| `decision?` | `(decision? x)` — true if `x` is a decision variable |
| `unresolved?` | `(unresolved? x)` — true if `x` contains unsolved decision variables |

## Solver Options

| Option | Description |
|--------|-------------|
| `:async?` | Return a `core.async` channel |

```clojure
;; Debug: write generated .mzn to scratch/mzn instead of solving
(binding [routaverra.igor.solver/*debug* true]
  (i/satisfy constraint))

;; Validate: cross-check each MiniZinc solution in Clojure
(binding [routaverra.igor.solver/*validate?* true]
  (i/satisfy constraint))
```

## Igor vs. core.logic

Both are constraint programming tools for Clojure; they target different problem shapes.

| | Igor | core.logic |
|-|------|------------|
| Engine | Compiles to MiniZinc; solves via Gecode, OR-Tools, etc. | miniKanren + CLP(FD) search in Clojure |
| Solutions | One map via `satisfy`, all via `satisfy-all` | Lazy stream |
| Types | Int, Bool, Set, Keyword (typed variables) | Untyped logic vars |
| Arithmetic | `+` `-` `*` `/` `mod` `rem` `abs` `min` `max` `inc` `dec` `pow` | `fd/+` `fd/-` only |
| Global constraints | `all-different`, `regular`, `cost-regular`, `table`, graph library | No |
| Optimization | `maximize` / `minimize` | No |
| Set / graph algebra | Full (intersection, union, spanning tree, circuit, ...) | No |
| Relational queries | No — one-way compilation | Yes — run programs backwards |
| Dependencies | MiniZinc on PATH | Pure Clojure |

Use Igor when you need optimization, global constraints, or set/graph reasoning. Use core.logic for relational/bidirectional programming and problems that fit miniKanren's interleaving search.

## Running Tests

```bash
clj -A:test -m kaocha.runner
```
