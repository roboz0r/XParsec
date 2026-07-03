// Second fixture file for the cross-file MERGE case: the global `interface Widget`
// accretes a second method here. TS merges it (in global scope) into the SAME `Widget`
// symbol declared in `globals.d.ts`, so the symbol-based enumerator emits ONE `Widget`
// carrying BOTH `a` (from `globals.d.ts`) and `b` (here). A per-file statement walk
// would instead emit two partial `Widget`s and miss the merge — the trap Step 2 avoids.

interface Widget {
  b(): void;
}
