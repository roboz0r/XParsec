namespace XParsec.FSharp.SemanticAnalysis

/// A `TyVarId` KNOWN to be its union-find representative; `find` is its only producer.
[<Struct>]
type Rep =
    private
    | Rep of root: TyVarId

    member this.Id = let (Rep id) = this in id

[<RequireQualifiedAccess>]
module internal PayloadJoin =
    /// Skips a `loser` item whose `Kind` already appears: two same-`Kind` constraints
    /// discharge to the same predicate.
    let constraintsByKind (winner: SemanticConstraint list) (loser: SemanticConstraint list) : SemanticConstraint list =
        let mutable acc = winner

        for c in loser do
            if not (acc |> List.exists (fun existing -> existing.Kind = c.Kind)) then
                acc <- c :: acc

        acc

/// The storage core every deferred-constraint payload family shares: a grow-only list
/// keyed by the metavar's representative id. Insertion order is preserved.
[<Sealed>]
type PayloadList<'T>(combine: 'T list -> 'T list -> 'T list) =
    let table = System.Collections.Generic.Dictionary<int, 'T list>()

    let at (root: Rep) : 'T list =
        match table.TryGetValue(int root.Id) with
        | true, xs -> xs
        | _ -> []

    member _.Items(root: Rep) : 'T list = at root

    member _.IsEmpty(root: Rep) : bool = List.isEmpty (at root)

    /// Replace `root`'s items; an empty list drops the entry.
    member _.Set(root: Rep, items: 'T list) : unit =
        if List.isEmpty items then
            table.Remove(int root.Id) |> ignore
        else
            table.[int root.Id] <- items

    member this.Prepend(root: Rep, item: 'T) : unit = this.Set(root, item :: at root)

    member this.Append(root: Rep, item: 'T) : unit = this.Set(root, at root @ [ item ])

    member this.Join(winner: Rep, loser: Rep) : unit =
        let l = at loser

        if not (List.isEmpty l) then
            this.Set(winner, combine (at winner) l)
            table.Remove(int loser.Id) |> ignore

/// A `PayloadList` plus a reference-keyed `solved` set: a discharged item is recorded, not
/// removed. One shared item stamped on several typars discharges once, hence `'T : not struct`.
[<Sealed>]
type BoundTable<'T when 'T: not struct>(combine: 'T list -> 'T list -> 'T list) =
    let items = PayloadList<'T>(combine)

    let solved = System.Collections.Generic.HashSet<'T>(HashIdentity.Reference)

    member _.Live(root: Rep) : 'T list =
        items.Items root |> List.filter (fun x -> not (solved.Contains x))

    member _.IsEmpty(root: Rep) : bool = items.IsEmpty root
    member _.Prepend(root: Rep, item: 'T) : unit = items.Prepend(root, item)

    member _.Join(winner: Rep, loser: Rep) : unit = items.Join(winner, loser)

    member _.Solve(item: 'T) : unit = solved.Add item |> ignore

    member _.IsSolved(item: 'T) : bool = solved.Contains item

/// The metavar arena for one file: it mints `TyVarId`s with dense, monotone ids and owns
/// the id-indexed arrays behind them. Grow-only; ids are never reused, so a `TyVarId` is a
/// stable array index.
[<Sealed>]
type TypeStore() =
    let mutable nextId = 0
    let mutable capacity = 0
    // `parent.[i] = i` marks a ROOT; any other entry points one step up the tree.
    let mutable parent: int[] = Array.empty
    let mutable rank: int[] = Array.empty
    // Authoritative ON THE ROOT (a `Rep` from `UnionFind.find`): Rémy level, the
    // solution link, the measure carrier, and the quantified flag.
    let mutable level: int[] = Array.empty
    let mutable link: SemType voption[] = Array.empty
    let mutable units: MeasureTerm voption[] = Array.empty
    let mutable quantified: bool[] = Array.empty
    // Write-once region id, NOT migrated on union, so it stays a per-node cell keyed by
    // raw `TyVarId`.
    let mutable region: RegionId[] = Array.empty

    let growStore (arr: 'T[]) (usedCount: int) (newCap: int) (fill: 'T) : 'T[] =
        let n = Array.create newCap fill
        System.Array.Copy(arr, n, usedCount)
        n

    // Every parallel array grows together off one `capacity`, so a dense id in range for
    // one is in range for all.
    let ensureCapacity (needed: int) : unit =
        if needed > capacity then
            let newCap = max needed (max 4 (capacity * 2))
            parent <- growStore parent capacity newCap 0
            rank <- growStore rank capacity newCap 0
            level <- growStore level capacity newCap 0
            link <- growStore link capacity newCap ValueNone
            units <- growStore units capacity newCap ValueNone
            quantified <- growStore quantified capacity newCap false
            region <- growStore region capacity newCap RegionId.Unknown
            capacity <- newCap

    member _.NewTypeVar() : TyVarId =
        let id = nextId
        ensureCapacity (id + 1)
        parent.[id] <- id
        rank.[id] <- 0
        level.[id] <- 0
        link.[id] <- ValueNone
        units.[id] <- ValueNone
        quantified.[id] <- false
        region.[id] <- RegionId.Unknown
        nextId <- nextId + 1
        LanguagePrimitives.Int32WithMeasure<tyVarId> id

    member _.Count = nextId

    // Keyed by raw `TyVarId`, not by `Rep`: `find` legitimately walks non-roots.

    /// `ValueNone` ≡ `tv` is its own root.
    member _.Parent(tv: TyVarId) : TyVarId voption =
        let i = int tv
        let p = parent.[i]

        if p = i then
            ValueNone
        else
            ValueSome(LanguagePrimitives.Int32WithMeasure<tyVarId> p)

    member _.SetParent(tv: TyVarId, p: TyVarId voption) : unit =
        parent.[int tv] <-
            match p with
            | ValueSome r -> int r
            | ValueNone -> int tv

    member _.Rank(tv: TyVarId) : int = rank.[int tv]
    member _.SetRank(tv: TyVarId, r: int) : unit = rank.[int tv] <- r

    /// Let-depth at which the root was minted (Rémy's levels).
    member _.Level(r: Rep) : int = level.[int r.Id]
    member _.SetLevel(r: Rep, v: int) : unit = level.[int r.Id] <- v

    /// The solution / substitution reached from this root; `ValueNone` while free.
    member _.Link(r: Rep) : SemType voption = link.[int r.Id]
    member _.SetLink(r: Rep, v: SemType voption) : unit = link.[int r.Id] <- v

    /// Measure constraint on the root when it is a numeric type; `ValueNone` otherwise.
    member _.Units(r: Rep) : MeasureTerm voption = units.[int r.Id]
    member _.SetUnits(r: Rep, v: MeasureTerm voption) : unit = units.[int r.Id] <- v

    /// True once some binding's `TypeScheme` quantifies this class. Such a class is a type
    /// PARAMETER of the enclosing signature, so a later pass that settles leftover inference
    /// vars must leave it free. A union carries the flag onto the surviving root.
    member _.Quantified(r: Rep) : bool = quantified.[int r.Id]
    member _.MarkQuantified(r: Rep) : unit = quantified.[int r.Id] <- true

    member _.Region(tv: TyVarId) : RegionId = region.[int tv]
    member _.SetRegion(tv: TyVarId, r: RegionId) : unit = region.[int tv] <- r

    /// SRTP member-trait bounds, keyed by representative.
    member val Srtp = BoundTable<MemberSignature>(fun winner loser -> loser @ winner) with get

    /// Type-parameter constraints, keyed by representative. A `[<Struct>]`
    /// `SemanticConstraint` has no reference identity, so this family rewrites its
    /// remainder through `Set` instead of marking items solved.
    member val Constraints = PayloadList<SemanticConstraint>(PayloadJoin.constraintsByKind) with get

    /// Deferred dot-accesses parked on a still-free object argument, keyed by representative.
    member val Pda = BoundTable<DeferredMemberAccess>(fun winner loser -> loser @ winner) with get

    /// Default-constraint chains (`default ^T : …`), keyed by representative. A chain is
    /// consumed WHOLESALE, so this family clears per-tv through `Set`.
    member val Defaults = PayloadList<SemType>(fun winner loser -> winner @ loser) with get

    member this.MergePayloads(winner: Rep, loser: Rep) : unit =
        this.Constraints.Join(winner, loser)
        this.Srtp.Join(winner, loser)
        this.Pda.Join(winner, loser)
        this.Defaults.Join(winner, loser)

/// Union-find over the per-file `TypeStore`. In this file because `Rep`'s case is private
/// above, so only code here can mint one.
module UnionFind =

    let find (store: TypeStore) (tv: TyVarId) : Rep =
        let mutable root = tv
        let mutable continueLoop = true

        while continueLoop do
            match store.Parent root with
            | ValueNone -> continueLoop <- false
            | ValueSome p -> root <- p

        let mutable cursor = tv

        while cursor <> root do
            match store.Parent cursor with
            | ValueNone ->
                // A non-root always has a parent; terminate rather than loop.
                cursor <- root
            | ValueSome next ->
                store.SetParent(cursor, ValueSome root)
                cursor <- next

        Rep root

    /// Merges the two classes only: `Link` / `Units` / payload are the caller's business.
    /// The surviving root inherits the `min` of the two roots' levels, and is quantified
    /// when either root was.
    let union (store: TypeStore) (a: TyVarId) (b: TyVarId) : unit =
        let rootA = find store a
        let rootB = find store b

        if rootA <> rootB then
            let mergedLevel = min (store.Level rootA) (store.Level rootB)
            let mergedQuantified = store.Quantified rootA || store.Quantified rootB

            let survivor =
                if store.Rank rootA.Id < store.Rank rootB.Id then
                    store.SetParent(rootA.Id, ValueSome rootB.Id)
                    rootB
                elif store.Rank rootA.Id > store.Rank rootB.Id then
                    store.SetParent(rootB.Id, ValueSome rootA.Id)
                    rootA
                else
                    store.SetParent(rootB.Id, ValueSome rootA.Id)
                    store.SetRank(rootA.Id, store.Rank rootA.Id + 1)
                    rootA

            store.SetLevel(survivor, mergedLevel)

            if mergedQuantified then
                store.MarkQuantified survivor

    let inSameClass (store: TypeStore) (a: TyVarId) (b: TyVarId) : bool = find store a = find store b

    /// Follow union-find roots + `.Link` until the OUTERMOST type constructor is concrete;
    /// nested type arguments are left untouched. A root carrying a `Units` measure stops
    /// the follow, so the measure rides on the returned `TyVar`.
    let rec zonkShallow (store: TypeStore) (t: SemType) : SemType =
        match t with
        | TyVar tv ->
            let root = find store tv

            match store.Link root with
            | ValueSome target when (store.Units root).IsNone -> zonkShallow store target
            | _ -> TyVar root.Id
        | _ -> t
