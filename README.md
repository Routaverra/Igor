Constraint programming for Clojure, backed by [MiniZinc](https://www.minizinc.org/).

> My freedom will be so much the greater and more meaningful the more narrowly I limit my field of action and the more I surround myself with obstacles. Whatever diminishes constraint, diminishes strength. The more constraints one imposes, the more one frees one's self of the chains that shackle the spirit.
>
> — Igor Stravinsky, *Poetics of Music*

Igor shadows Clojure's core operators (`+`, `=`, `and`, `every?`, etc.) to produce constraint expressions that are first-class and data-oriented. You declare variables with domains, compose constraints using familiar Clojure syntax, and industrial solvers (Gecode, OR-Tools, etc.) return solutions as plain maps. It supports integer arithmetic, set algebra, universal quantification, extensional constraints (table, regular, cost-regular), and a graph constraint library — paths, spanning trees, circuits, connectivity — all composable and all optimizable via maximize/minimize.

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

**Variables** — `fresh-int`, `fresh-bool`, `fresh-set` create typed decision variables. `fresh` creates an untyped variable whose type is inferred from usage. Domains are passed as collections:

```clojure
(i/fresh-int (range 10))     ; integer in {0..9}
(i/fresh-int #{1 3 5 7})     ; integer in {1,3,5,7}
(i/fresh-bool)               ; boolean
(i/fresh-set (range 12))     ; subset of {0..11}
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

## Composability

Constraints are plain Clojure data. You compose them with ordinary functions — `map`, `apply`, `partition`, closures — the same patterns you already use.

### Constraint-producing functions

Any function that returns a constraint expression works with `map`, `apply`, and the rest of the sequence library.

```clojure
(defn exceeds-index [vars k]
  (i/> (nth vars k) k))

(let [n    5
      vars (vec (repeatedly n #(i/fresh-int (range 20))))
      sol  (i/satisfy (apply i/and (map #(exceeds-index vars %) (range n))))]
  (mapv sol vars))
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
(let [vars (vec (repeatedly 5 #(i/fresh-int (range 20))))
      sol  (i/satisfy (i/and (pairwise i/< vars)
                             (pairwise (fn [a b] (i/<= (i/- b a) 3)) vars)))]
  (mapv sol vars))
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
                  (vec (repeatedly 4 #(i/fresh-int (range 1 5))))))
      sol  (i/satisfy (i/and (row-constraints grid)
                             (col-constraints grid)))]
  (mapv (fn [row] (mapv sol row)) grid))
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
      money (i/+ (i/* m 10000) (i/* o 1000) (i/* n 100) (i/* e 10) y)
      solution (i/satisfy
                (i/and (apply i/all-different [s e n d* m o r y])
                       (i/= (i/+ send more) money)
                       (i/> s 0)
                       (i/> m 0)))]
  (mapv solution [s e n d* m o r y]))
;; => [9 5 6 7 1 0 8 2]  — i.e. 9567 + 1085 = 10652
```

### Set Constraints with every?

```clojure
;; Find a subset of {0..11} such that no three consecutive
;; elements (mod 12) are all present — "cluster-free"
(let [x (i/fresh)
      cluster-free
      (i/every? (i/bind (range 12) x)
        (fn [a]
          (i/when (i/contains? x (i/mod (i/+ a 1) 12))
            (i/not (i/contains? x (i/mod (i/+ a 2) 12))))))]
  (get (i/satisfy cluster-free) x))
;; => e.g. #{0 1 3 4 6 7 9 10}
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
      allowed [[1 2 3] [4 5 6] [7 8 9]]
      sol (i/satisfy (i/table [x y z] allowed))]
  [(sol x) (sol y) (sol z)])
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
      vars (vec (repeatedly 4 #(i/fresh-int #{0 1})))
      sol  (i/satisfy (i/regular vars dfa))]
  (mapv sol vars))
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
(let [x   (i/fresh)
      sol (i/satisfy
           (i/= #{1 2 3}
                 (i/image (i/bind (range 12) x)
                   (fn [a] (i/+ a 1)))))]
  (sol x))
;; => #{0 1 2}
```

For more usage examples, see [test/routaverra/igor/constraint_problems_test.clj](test/routaverra/igor/constraint_problems_test.clj) — N-queens, magic squares, graph coloring, and other classic CSP benchmarks implemented in Igor.

## Operations Reference

### Variables

| Function | Description |
|----------|-------------|
| `fresh` | Untyped decision variable (type inferred from usage) |
| `fresh-int` | `(fresh-int domain)` — integer variable |
| `fresh-bool` | `(fresh-bool)` — boolean variable |
| `fresh-set` | `(fresh-set universe)` — set variable (subset of universe) |
| `bind` | `(bind coll decision)` — for ints, constrains the domain (allowed values); for sets, constrains the universe (allowed members) |

### Arithmetic (Numeric -> Numeric)

| | | | |
|---|---|---|---|
| `+` | `-` | `*` | `/` (integer div) |
| `inc` | `dec` | `abs` | `pow` |
| `max` | `min` | `mod` | `rem` |

### Comparison (Numeric -> Bool)

| | | | |
|---|---|---|---|
| `=` | `not=` | `>` | `<` |
| `>=` | `<=` | | |

`=` and `not=` also accept Bool and Set arguments.

### Logic (Bool -> Bool)

| Function | Signature |
|----------|-----------|
| `and` | `(and & args)` |
| `or` | `(or & args)` |
| `not` | `(not x)` |
| `when` | `(when test body)` — implication (test -> body) |
| `if` | `(if test then else)` — polymorphic return type |
| `cond` | `(cond test1 expr1 ... :else default)` |

### Predicates

| Numeric -> Bool | Bool -> Bool |
|-----------------|--------------|
| `even?` `odd?` | `true?` `false?` |
| `pos?` `neg?` `zero?` | |

### Collections

| Function | Types | Description |
|----------|-------|-------------|
| `nth` | `(nth [T...] Numeric) -> T` | Index into a vector of expressions |
| `count` | `(count Set) -> Numeric` | Set cardinality |
| `contains?` | `(contains? Set Numeric) -> Bool` | Set membership |
| `all-different` | `(all-different & Numeric) -> Bool` | All arguments must take distinct values |

### Set Operations

| Set -> Set | Set -> Bool |
|------------|-------------|
| `intersection` | `subset?` |
| `difference` | `superset?` |
| `sym-diff` | `set<` (strict lex) |
| `union` | `set<=` (lex) |

### Iteration

| Function | Description |
|----------|-------------|
| `every?` | `(every? set-expr (fn [elem] bool-expr))` — true when the constraint holds for every element in the set |
| `image` | `(image set-expr (fn [elem] expr))` — builds a new set by applying a function to each element: `{f(x) \| x ∈ S}` |

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
| `satisfy` | `(satisfy constraint)` — one solution |
| `satisfy-all` | `(satisfy-all constraint)` — all solutions |
| `maximize` | `(maximize objective constraint)` |
| `minimize` | `(minimize objective constraint)` |
| `validate-solution` | `(validate-solution constraint solution)` — pure-Clojure cross-check |

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
| Types | Int, Bool, Set (typed variables) | Untyped logic vars |
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
