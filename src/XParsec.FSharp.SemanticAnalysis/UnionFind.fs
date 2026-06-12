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
    /// The surviving root inherits `min` of the two roots' Levels so the
    /// representative remains authoritative for Rémy's level-based
    /// generalisation.
    let union (a: TypeVar) (b: TypeVar) : unit =
        let rootA = find a
        let rootB = find b

        if not (System.Object.ReferenceEquals(rootA, rootB)) then
            let mergedLevel = min rootA.Level rootB.Level

            let survivor =
                if rootA.Rank < rootB.Rank then
                    rootA.Parent <- ValueSome rootB
                    rootB
                elif rootA.Rank > rootB.Rank then
                    rootB.Parent <- ValueSome rootA
                    rootA
                else
                    rootB.Parent <- ValueSome rootA
                    rootA.Rank <- rootA.Rank + 1
                    rootA

            survivor.Level <- mergedLevel

    let inSameClass (a: TypeVar) (b: TypeVar) : bool =
        System.Object.ReferenceEquals(find a, find b)

    /// Follow union-find roots + `.Link` to the concrete *head* of a type: the
    /// shared core of the union-find walk. Resolves only the head constructor —
    /// nested type arguments are left untouched (`Unification.zonk` layers the
    /// recursive argument rebuild on top of this; `Inline` needs only the head).
    /// A root carrying a `Units` measure stops the follow so the measure rides on
    /// the returned `TyVar`, matching `zonk`. The single home of the root-following
    /// walk, so every caller shares it rather than re-deriving the chase.
    let rec headZonk (t: SemType) : SemType =
        match t with
        | TyVar tv ->
            let root = find tv

            match root.Link with
            | ValueSome target when root.Units.IsNone -> headZonk target
            | _ -> TyVar root
        | _ -> t
