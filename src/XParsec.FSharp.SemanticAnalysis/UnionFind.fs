namespace XParsec.FSharp.SemanticAnalysis

// Union-find primitives over TypeVar, used by Algorithm J in
// Passes/Unification.fs.
//
// The TypeVar class itself carries Parent/Rank fields directly (see
// SemanticInfo.fs); this module just contains the find/union operations
// over them. Keeping the algorithm separate from the data lets us swap in
// alternatives later (e.g. path-halving instead of path-compression) without
// touching the data definition.
//
// Not implemented yet — passes/unification will drive the exact shape.

module UnionFind =

    /// Returns the representative TypeVar for the equivalence class containing
    /// `tv`. Performs path compression as a side effect.
    let find (tv: TypeVar) : TypeVar =
        // TODO: classic find-with-path-compression.
        // Walk Parent chain; rewrite intermediate Parents to point at the root.
        tv

    /// Unify two TypeVars into the same equivalence class. After this returns,
    /// `find a = find b`. Does NOT solve constraints — the caller is
    /// responsible for resolving Link/Units/Region compatibility before or
    /// after calling, and for firing on-unified callbacks.
    let union (a: TypeVar) (b: TypeVar) : unit =
        // TODO: rank-based union — the lower-rank root points at the higher-rank
        // root; on equal rank, increment the new root's rank.
        ignore (a, b)
