namespace XParsec.FSharp.SemanticAnalysis

// Algorithm decoupled from data: the union-find slots (`parent`/`rank`/`level`/
// `link`/`units`) live id-indexed on the per-file `TypeStore`, not on the `TypeVar`
// node. Every entry point takes the `store` so it can read/write those arrays and
// still RETURN the root `TypeVar` handle (looked up from the store's id table) that
// the reference-identity call sites depend on.

module UnionFind =

    /// Iterative rather than recursive to avoid stack pressure on long chains.
    let find (store: TypeStore) (tv: TypeVar) : TypeVar =
        let mutable root = tv
        let mutable continueLoop = true

        while continueLoop do
            match store.Parent root with
            | ValueNone -> continueLoop <- false
            | ValueSome p -> root <- p

        let mutable cursor = tv

        while not (System.Object.ReferenceEquals(cursor, root)) do
            match store.Parent cursor with
            | ValueNone ->
                // Unreachable post-phase-1; defensive terminate.
                cursor <- root
            | ValueSome next ->
                store.SetParent(cursor, ValueSome root)
                cursor <- next

        root

    /// Does NOT resolve Link / Units / Constraints / SrtpBounds — the caller
    /// (Unification) handles compatibility checks and on-unified callbacks.
    /// The surviving root inherits `min` of the two roots' Levels so the
    /// representative remains authoritative for Rémy's level-based
    /// generalisation.
    let union (store: TypeStore) (a: TypeVar) (b: TypeVar) : unit =
        let rootA = find store a
        let rootB = find store b

        if not (System.Object.ReferenceEquals(rootA, rootB)) then
            let mergedLevel = min (store.Level rootA) (store.Level rootB)

            let survivor =
                if store.Rank rootA < store.Rank rootB then
                    store.SetParent(rootA, ValueSome rootB)
                    rootB
                elif store.Rank rootA > store.Rank rootB then
                    store.SetParent(rootB, ValueSome rootA)
                    rootA
                else
                    store.SetParent(rootB, ValueSome rootA)
                    store.SetRank(rootA, store.Rank rootA + 1)
                    rootA

            store.SetLevel(survivor, mergedLevel)

    let inSameClass (store: TypeStore) (a: TypeVar) (b: TypeVar) : bool =
        System.Object.ReferenceEquals(find store a, find store b)

    /// Follow union-find roots + `.Link` to the concrete *head* of a type: the
    /// shared core of the union-find walk. Resolves only the head constructor —
    /// nested type arguments are left untouched (`Unification.zonk` layers the
    /// recursive argument rebuild on top of this; `Inline` needs only the head).
    /// A root carrying a `Units` measure stops the follow so the measure rides on
    /// the returned `TyVar`, matching `zonk`. The single home of the root-following
    /// walk, so every caller shares it rather than re-deriving the chase.
    let rec headZonk (store: TypeStore) (t: SemType) : SemType =
        match t with
        | TyVar tv ->
            let root = find store tv

            match store.Link root with
            | ValueSome target when (store.Units root).IsNone -> headZonk store target
            | _ -> TyVar root
        | _ -> t
