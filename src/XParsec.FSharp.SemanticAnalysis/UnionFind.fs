namespace XParsec.FSharp.SemanticAnalysis

// Algorithm decoupled from data (Parent/Rank fields live on TypeVar in
// SemanticInfo.fs) so we can swap path-compression for path-halving later
// without touching the record definition.

module UnionFind =

    /// Iterative rather than recursive to avoid stack pressure on long chains.
    let find (tv: TypeVar) : TypeVar =
        let mutable root = tv
        let mutable continueLoop = true

        while continueLoop do
            match root.Parent with
            | ValueNone -> continueLoop <- false
            | ValueSome p -> root <- p

        let mutable cursor = tv

        while not (System.Object.ReferenceEquals(cursor, root)) do
            match cursor.Parent with
            | ValueNone ->
                // Unreachable post-phase-1; defensive terminate.
                cursor <- root
            | ValueSome next ->
                cursor.Parent <- ValueSome root
                cursor <- next

        root

    /// Does NOT resolve Link / Units / IfaceBounds / SrtpBounds — the caller
    /// (Unification) handles compatibility checks and on-unified callbacks.
    let union (a: TypeVar) (b: TypeVar) : unit =
        let rootA = find a
        let rootB = find b

        if not (System.Object.ReferenceEquals(rootA, rootB)) then
            if rootA.Rank < rootB.Rank then rootA.Parent <- ValueSome rootB
            elif rootA.Rank > rootB.Rank then rootB.Parent <- ValueSome rootA
            else
                rootB.Parent <- ValueSome rootA
                rootA.Rank <- rootA.Rank + 1

    let inSameClass (a: TypeVar) (b: TypeVar) : bool =
        System.Object.ReferenceEquals(find a, find b)
