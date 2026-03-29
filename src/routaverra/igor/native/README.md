# Native Constraint Solver

A pure Clojure constraint solver for Igor, inspired by the Gecode
architecture. Used as an alternative backend to MiniZinc.

## How constraint solving works

A constraint satisfaction problem has three parts: **variables** with
**domains** (the values each variable can take), and **constraints**
(relationships between variables that must hold).

For example: variables x and y, both in domain {1..10}, with constraint
x + y = 7. The solver's job is to find assignments of values to variables
that satisfy all constraints simultaneously.

### Propagation

The key insight of propagator-based solvers is that you can narrow variable
domains by reasoning about constraints *without* trying every combination.

Given x:{1..10}, y:{1..10}, and x + y = 7:
- x can be at most 6 (since y is at least 1), so x narrows to {1..6}
- y can be at most 6 by the same reasoning, so y narrows to {1..6}
- Now x can be at least 1 (since y is at most 6 and 7 - 6 = 1) — no change
- No further narrowing is possible. We've reached a **fixpoint**.

Each constraint has a **propagator** — a function that looks at the current
domains of its variables and tightens them. The engine runs propagators
repeatedly until nothing changes (fixpoint) or a domain becomes empty
(failure, meaning the constraints are unsatisfiable).

A propagator is a pure function: it takes the current store (the map of
all variable domains) and returns a new store with tighter domains, or
`::failed` if it detects a contradiction.

### Events and scheduling

Running every propagator after every change is wasteful. Instead, the
engine tracks **events** — what changed about a domain after a propagator
ran:

- `:bounds` — the min or max changed
- `:assigned` — the domain narrowed to a single value
- `:domain` — any value was removed

Each propagator declares which events on which variables it cares about.
An arithmetic propagator like x + y = z subscribes to `:bounds` on all
three variables. A not-equals propagator x != y only subscribes to
`:assigned` — it can only act when one side is fixed to a single value.

The engine maintains a reverse index (subscriptions) so that when variable
x's bounds change, it can immediately look up which propagators need to
re-run.

The propagator **priority** determines execution order within the queue.
Cheap propagators (equality, comparisons — priority 1) run before expensive
ones (multiplication, division — priority 3). This lets simple constraints
prune the search space before heavier reasoning kicks in.

### Search

Propagation alone usually can't solve a problem — it reaches a fixpoint
where domains are narrowed but not all variables are assigned. At that
point, the solver must **guess**: pick an unassigned variable, pick a value,
and explore.

This is depth-first search with propagation at every node:

1. If any domain is empty: **backtrack** (this branch is dead).
2. If all variables are assigned: **solution found**.
3. Otherwise: pick the unassigned variable with the smallest domain
   (**first-fail** heuristic — branch on the most constrained variable
   first, since it's most likely to fail and prune the tree).
4. Try its minimum value. Set the variable to that value, propagate to
   fixpoint, recurse.
5. If that fails: remove the value from the domain, propagate, recurse.

The critical property that makes this efficient in Clojure: the store is
an **immutable persistent map**. Branching doesn't require copying the
store or maintaining an undo stack — each branch gets a structurally-shared
variant of the store automatically. This is where Clojure's persistent data
structures replace what Gecode implements with explicit trailing/copying.

### Optimization

For minimize/maximize, the solver uses **branch and bound**: find any
solution, record the objective value, add a constraint that the next
solution must be strictly better, and search again. Repeat until no
better solution exists.

## Architecture

### Pipeline

```
User constraint
        |
   prepare-model (solver.clj) — shared by both backends
   - type inference
   - term expansion (n-ary to binary, predicates to arithmetic)
   - conjunctive flattening
   - decision/binding collection
        |
   ┌────┴────┐
   |         |
 MiniZinc  Native
 backend   backend
```

**Conjunctive flattening** is the critical preprocessing step. It transforms
a nested constraint tree into a flat list of simple terms, each operating on
only variables and constants:

```
(= total (+ (* price qty) tax))
```
becomes:
```
impl1 = (* price qty)      ;; simple: two variables
impl2 = (+ impl1 tax)      ;; simple: variable + variable
(= total impl2)            ;; simple: variable = variable
```

Each introduced variable (`impl1`, `impl2`) gets its own domain that
propagation will narrow. This flattening means propagators never need to
recursively evaluate sub-expressions — they just look up variable bounds
and do arithmetic.

### Namespaces

**`domains.clj`** — Domain representations. The `IDomain` protocol defines
operations like `restrict-min`, `restrict-max`, `remove-value`. Two
implementations: `IntervalDomain` for contiguous ranges (O(1) bounds
operations), `EnumeratedDomain` for sparse value sets. Every domain
operation returns `[new-domain events]` or `::failed`.

**`fixpoint.clj`** — The propagation loop. Maintains a priority queue
(sorted-set of `[priority, propagator-index]`). Dequeues propagators, runs
them, diffs the store to detect events, enqueues affected propagators via
the subscription index. Terminates at fixpoint (empty queue) or failure.

**`propagators.clj`** — Compiles constraint terms into propagator maps.
Two multimethods dispatched on term type: `compile-constraint` for top-level
constraints, `compile-defining-equality` for introduced-variable definitions.
Contains propagators for arithmetic, comparisons, logical connectives,
conditionals, and reified constraints.

**`search.clj`** — DFS with first-fail variable selection.
`solve-dfs` for first solution, `solve-all-dfs` for enumeration,
`solve-optimize` for branch-and-bound.

**`engine.clj`** — Entry point. Builds the initial store from decision
bindings, wires up compilation + propagation + search, extracts solutions
back to `{Decision -> value}` format.

### Representation choices

**Booleans as integers.** `true`/`false` are `1`/`0` in the store. This
lets boolean and arithmetic propagation share the same domain machinery.
A reified constraint like `b <-> (x > y)` is just arithmetic on b's {0,1}
domain. Values are converted back at solution extraction.

**Keywords as integers.** The sorted set of all keywords in the problem
gives a stable bidirectional mapping. Keyword domains become
EnumeratedDomains of integer indices.

**Propagators are pure functions.** `(fn [store] -> store' | ::failed)`.
No mutation, no side effects. The engine handles event detection by diffing
the store externally. This keeps propagators simple to write and reason
about.

### Relationship to Gecode

This solver follows Gecode's conceptual architecture:

| Gecode concept | Clojure equivalent |
|---|---|
| Space (mutable variable store) | Immutable map `{Decision -> domain}` |
| Space::clone() for branching | Structural sharing (free with persistent maps) |
| Propagator class with `propagate()` | Map with `:propagate-fn` |
| Advisor/event subscription | `{Decision -> {event -> #{prop-indices}}}` |
| Brancher | `select-variable` + `select-value` in search.clj |
| DFS engine with commit/recomputation | Recursive `solve-dfs` with immutable stores |
| Branch-and-bound | `solve-optimize` loop with bound tightening |

The main structural difference: Gecode uses mutable spaces with explicit
copying and trailing for backtracking. This solver uses Clojure's immutable
persistent data structures, which give copy-on-branch semantics for free
through structural sharing.
