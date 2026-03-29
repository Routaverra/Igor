# TODO

## Higher-order constraint patterns

### Cumulative (resource scheduling)
The classic parallel-machine scheduling constraint. Each task has a start time, duration, and resource demand; total demand at any time point cannot exceed capacity. MiniZinc has native `cumulative` support.

Natural fit for the handle pattern: create task handles bundling start/duration/end variables, pass them to a `cumulative` constraint. Current scheduling tests (job sequencing, task assignment) work around this with manual cumulative sums, but that only handles single-machine / sequential problems.

### ~~Soft constraints~~ (done)
Implemented. `(i/soft constraint penalty)` returns a handle; `(i/violation handle)` returns the cost variable (0 when satisfied, penalty when violated). Penalty can be a fixed number or a decision variable. Composes with `minimize` and other constraints.

## Type system

### Vector and list types
Add variable-length and fixed-length array types backed by MiniZinc arrays. Unlocks native support for: global cardinality (gcc), sort, increasing/decreasing, lexicographic ordering, and other array-based global constraints.

### ~~Keywords as enum type~~ (done)
Implemented. Keywords map to a single sorted MiniZinc enum. Supports `fresh-keyword`, keyword sets via `fresh-set`, `=`, `not=`, `contains?`, `all-different`. Both namespaced and non-namespaced keywords work.
