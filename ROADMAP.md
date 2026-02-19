# Igor Clojure SDK Roadmap

## Current State

Igor-clj is the canonical reference SDK for Igor, a functional constraint programming system. It currently supports:

- **Scalar types**: integers (bounded domains), booleans, finite sets
- **Flat constraint models**: arithmetic, comparison, logical connectives, conditional branching, quantifiers over sets
- **Operator shadowing**: `igor.core` shadows `clojure.core` operators (`+`, `-`, `*`, `=`, `<`, `>`, etc.) so constraint expressions read like ordinary Clojure
- **Two solve modes**: `satisfy` (find any solution) and `maximize` (optimize an objective)
- **Composable terms**: all constraint expressions are immutable values that compose via `and`, `or`
- **Variable-index access**: `nth` enables indexed lookup into a vector of expressions using a decision variable as the index, expanding to a `cond` chain at solve time

### Problem coverage

The test suite demonstrates igor across the major constraint programming problem categories:

| Category | Examples |
|---|---|
| Arithmetic CSPs | SEND+MORE=MONEY, magic square, Pythagorean triples, simultaneous equations |
| Combinatorial search | N-queens, Latin square |
| Graph/routing | Hamiltonian cycle, TSP (shortest cycle), graph coloring |
| Scheduling | Job sequencing with deadlines, task assignment |
| Packing/covering | 0-1 knapsack, bin packing, set partition, set covering |
| Planning/reachability | Farmer-wolf-goat-cabbage river crossing |
| Optimization | Production planning (integer LP), voice leading minimization |
| Boolean satisfiability | Circuit SAT |
| Set constraints | Subset chains, set images via quantifiers |
| Permutation/design | Involutions, all-interval trichords |

## Near-Term

### Native array support

The current `nth` operator works by expanding to a `cond` chain, which is correct but generates O(n) constraints per access. For problems with larger arrays (n > ~20), this becomes impractical. A native array representation would:

- Emit MiniZinc `array` declarations and use the built-in `element` constraint
- Support multi-dimensional indexing
- Enable global constraints like `alldifferent` over arrays

### Additional global constraints

MiniZinc provides global constraints that are currently expressed manually via pairwise decomposition:

- `alldifferent` (currently: O(n^2) pairwise `not=`)
- `circuit` (currently: manual successor chain + position tracking)
- `cumulative` (for scheduling with resource constraints)
- `table` (extensional constraints via allowed tuples)

Adding these as first-class igor terms would improve both expressiveness and solver performance.

## Long-Term Vision

Igor should feel like a **dynamic vector/dictionary constraint language** -- a system where you write constraint models using the same nested data structures (vectors, maps) that you use in ordinary Clojure, but where the values are uncertain and resolved by a solver.

### Structured constraint data

The goal is to support nested, EDN/JSON-like data as constraint variables:

```clojure
;; A vector of n decision variables
(i/vector (range 10) 5)  ;; => [fresh fresh fresh fresh fresh]

;; A map with scalar keys and decision values
(i/map {:name (i/fresh)
        :age  (i/fresh-int (range 120))
        :role (i/fresh)})

;; Nested structures
(i/vector-of (fn [_] (i/map {:x (i/fresh-int (range 100))
                              :y (i/fresh-int (range 100))}))
             10)
```

Constraints over these structures would use the same vocabulary as Clojure data transformations:

```clojure
;; "all x-coordinates are distinct"
(apply i/and
  (for [i (range n) j (range (inc i) n)]
    (i/not= (get-in points [i :x])
            (get-in points [j :x]))))

;; "total weight <= capacity"
(i/<= (apply i/+ (map :weight items)) capacity)
```

### Key design principles

- **Untyped decisions**: `(i/fresh)` creates a decision whose type is inferred from constraints applied to it (already partially true today)
- **Dictionary keys are scalars**: consistent with the Transit protocol spec -- map keys are strings, keywords, or numbers. No composite keys.
- **Nested structure, flat solving**: the SDK unrolls nested data structures into flat MiniZinc variables with constraints preserving structural relationships. This is a major engineering effort but invisible to the user.
- **Functional composition**: models are values. You compute your model through data transformations (`map`, `filter`, `reduce`, `merge`) rather than declaring it statically.

### Migration path

The current `nth`-via-cond-expansion is a stepping stone. The progression is:

1. **Current**: `nth` expands to `cond` chain (works for small arrays)
2. **Next**: native `array` type that emits MiniZinc array declarations
3. **Later**: `vector` and `map` types that unroll to flat arrays with structural constraints
4. **Eventually**: nested structures with recursive unrolling

Each step preserves backward compatibility -- existing models continue to work unchanged.

## Broader Context

Igor-clj is one of three planned SDKs (Clojure, Python, TypeScript/JavaScript) that share a common Transit wire format and delegate solving to a stateless Rust binary with embedded Gecode and Chuffed solvers. See `igor-executive-summary.md` for the full architecture.

The Clojure SDK serves as the canonical reference implementation. Its constraint vocabulary and functional composition patterns define the portable API that other SDKs replicate in their host language idioms.
