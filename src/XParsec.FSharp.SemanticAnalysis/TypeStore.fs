namespace XParsec.FSharp.SemanticAnalysis

/// A `TyVarId` KNOWN to be its union-find representative. The only producer is
/// `UnionFind.find`. The root-authoritative store cells
/// (`Link` / `Level` / `Units` and the obligation side-tables) take a `Rep`, so
/// "read authoritative state off a non-root" stops type-checking — enforces "call `find` first"
/// Structure (`Parent` / `Rank`) and the write-once `Region` legitimately touch non-roots and
/// stay on raw `TyVarId`.
[<Struct>]
type Rep =
    private
    | Rep of root: TyVarId

    member this.Id = let (Rep id) = this in id

/// The `union` merge combinator for the one deferred-constraint payload family whose
/// join is non-trivial. Each family's combinator is baked into its owning
/// `PayloadList` / `BoundTable` at construction (below), so the union seam
/// (`TypeStore.MergePayloads`) never re-specifies a family's merge order — the
/// ordering invariant lives with the family that owns it. Replaces the former
/// `EngineCore.joinConstraints`.
[<RequireQualifiedAccess>]
module internal PayloadJoin =
    /// Constraints: fold `loser`'s items into `winner`, skipping any whose `Kind`
    /// already appears — two same-`Kind` constraints discharge to the same predicate,
    /// so keeping both would fire the diagnostic twice for one rule.
    let constraintsByKind (winner: SemanticConstraint list) (loser: SemanticConstraint list) : SemanticConstraint list =
        let mutable acc = winner

        for c in loser do
            if not (acc |> List.exists (fun existing -> existing.Kind = c.Kind)) then
                acc <- c :: acc

        acc

/// The **storage core** every deferred-constraint payload family shares: a grow-only
/// per-representative list keyed by the metavar's representative id. Every method takes
/// a `Rep`, so payload can only be read/written under a proven root. The backing
/// dictionary is PRIVATE — the only writes are `Set`/`Prepend`/`Append`/`Join`, so a
/// family has no off-seam setter once its node slot is gone (the single-mutation-seam
/// guarantee). `Join` folds a `union` loser's items into the winner via the family's
/// `combine` (fixed at construction), order preserved so diagnostics stay deterministic.
[<Sealed>]
type PayloadList<'T>(combine: 'T list -> 'T list -> 'T list) =
    let table = System.Collections.Generic.Dictionary<int, 'T list>()

    let at (root: Rep) : 'T list =
        match table.TryGetValue(int root.Id) with
        | true, xs -> xs
        | _ -> []

    /// Every item accrued under `root`, in insertion / merge order.
    member _.Items(root: Rep) : 'T list = at root

    member _.IsEmpty(root: Rep) : bool = List.isEmpty (at root)

    /// Replace `root`'s items; an empty list drops the entry so an ungrounded-but-empty
    /// root leaves no residue. THE single write path.
    member _.Set(root: Rep, items: 'T list) : unit =
        if List.isEmpty items then
            table.Remove(int root.Id) |> ignore
        else
            table.[int root.Id] <- items

    /// Accrue `item` at the head (`item :: node.slot`).
    member this.Prepend(root: Rep, item: 'T) : unit = this.Set(root, item :: at root)

    /// Accrue `item` at the tail (`node.slot @ [item]`).
    member this.Append(root: Rep, item: 'T) : unit = this.Set(root, at root @ [ item ])

    /// The `union` join: fold `loser`'s items into `winner` via the family's baked
    /// `combine` (winnerItems → loserItems → joined), then empty `loser`. The
    /// combinator is a construction-time property of the family, so no caller
    /// re-specifies the merge order. Associative/idempotent set-union in spirit.
    member this.Join(winner: Rep, loser: Rep) : unit =
        let l = at loser

        if not (List.isEmpty l) then
            this.Set(winner, combine (at winner) l)
            table.Remove(int loser.Id) |> ignore

/// A payload family whose items DISCHARGE ONE AT A TIME through a shared reference
/// identity — SRTP bounds and deferred dot-accesses. Wraps the grow-only
/// `PayloadList` with a reference-keyed `solved` set: a discharged item is recorded
/// (never removed), so a discharge reads only the `Live` items, never rewrites a
/// shrinking remainder, and never re-fires an item. Because `solved` keys by
/// reference, ONE shared item stamped on several participating typars discharges
/// exactly once, and the marking survives a `union` remap (the item objects are
/// unchanged). `'T` is therefore a reference type.
[<Sealed>]
type BoundTable<'T when 'T: not struct>(combine: 'T list -> 'T list -> 'T list) =
    let items = PayloadList<'T>(combine)

    let solved = System.Collections.Generic.HashSet<'T>(HashIdentity.Reference)

    member _.Items(root: Rep) : 'T list = items.Items root

    /// The still-undischarged items under `root`, in insertion / merge order.
    member _.Live(root: Rep) : 'T list =
        items.Items root |> List.filter (fun x -> not (solved.Contains x))

    member _.IsEmpty(root: Rep) : bool = items.IsEmpty root
    member _.Prepend(root: Rep, item: 'T) : unit = items.Prepend(root, item)

    member _.Join(winner: Rep, loser: Rep) : unit = items.Join(winner, loser)

    /// Record `item` as discharged so `Live` skips it from now on.
    member _.Solve(item: 'T) : unit = solved.Add item |> ignore

    member _.IsSolved(item: 'T) : bool = solved.Contains item

/// The metavar **arena** for one file: the single authority that mints `TyVarId`s
/// with dense, monotone ids, and the owner of the id-indexed structural
/// arrays (union-find `parent`/`rank`, the root-authoritative `level`/`link`/`units`,
/// the write-once `region`) plus the deferred-constraint side-tables. A metavar IS
/// its `TyVarId`; every slot is read/written HERE, id-indexed. Grow-only per file;
/// ids are never reused, so a `TyVarId` is a stable array index. One instance lives
/// on each `PassContext`.
[<Sealed>]
type TypeStore() =
    let mutable nextId = 0
    let mutable capacity = 0
    // Union-find structure. `parent.[i] = i` marks a ROOT (the `ValueNone ≡ self`
    // convention `find` / `union` read); any other entry points one step up the
    // tree, path-compressed by `find`.
    let mutable parent: int[] = Array.empty
    let mutable rank: int[] = Array.empty
    // Authoritative ON THE ROOT (a `Rep` from `UnionFind.find`): Rémy level, the
    // solution link, and the measure carrier.
    let mutable level: int[] = Array.empty
    let mutable link: SemType voption[] = Array.empty
    let mutable units: MeasureTerm voption[] = Array.empty
    // Write-once region id — NOT migrated on union (unlike the root-authoritative
    // slots), so it stays a plain per-node cell keyed by raw `TyVarId`.
    let mutable region: RegionId[] = Array.empty

    // Amortized-doubling copy-grow of one parallel arena array. Written ONCE and
    // shared by every slot below, not duplicated per array.
    let growStore (arr: 'T[]) (usedCount: int) (newCap: int) (fill: 'T) : 'T[] =
        let n = Array.create newCap fill
        System.Array.Copy(arr, n, usedCount)
        n

    // Every parallel array grows together off one `capacity`, so a single bounds
    // check backs them all and a dense id is always in range.
    let ensureCapacity (needed: int) : unit =
        if needed > capacity then
            let newCap = max needed (max 4 (capacity * 2))
            parent <- growStore parent capacity newCap 0
            rank <- growStore rank capacity newCap 0
            level <- growStore level capacity newCap 0
            link <- growStore link capacity newCap ValueNone
            units <- growStore units capacity newCap ValueNone
            region <- growStore region capacity newCap RegionId.Unknown
            capacity <- newCap

    /// Mint a fresh metavar carrying the next dense id. THE single construction
    /// seam — every `TyVarId` in a file is born here so its id indexes this store.
    /// A fresh var is its own union-find root (`parent.[id] = id`) at level 0,
    /// unlinked, un-measured, region-unknown.
    member _.NewTypeVar() : TyVarId =
        let id = nextId
        ensureCapacity (id + 1)
        parent.[id] <- id
        rank.[id] <- 0
        level.[id] <- 0
        link.[id] <- ValueNone
        units.[id] <- ValueNone
        region.[id] <- RegionId.Unknown
        nextId <- nextId + 1
        LanguagePrimitives.Int32WithMeasure<tyVarId> id

    /// Count of metavars minted so far (the dense id upper bound).
    member _.Count = nextId

    // --- Union-find structure. Owned by `UnionFind`; do not poke elsewhere. Keyed by
    //     raw `TyVarId` because `find` legitimately walks non-roots. ---

    /// `ValueNone` ≡ `tv` is its own root, the convention the `find` / `union`
    /// algorithm reads off the `parent` array.
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

    // --- Authoritative on the union-find root: a `Rep` (from `UnionFind.find`) is the
    //     only key, so a stale non-root read cannot be written. ---

    /// Let-depth at which the root was minted (Rémy's levels). `union` propagates
    /// `min` of the two roots' levels to the survivor; `occursAndAdjust` lowers a
    /// reachable level; generalisation quantifies roots whose level exceeds the
    /// enclosing scope.
    member _.Level(r: Rep) : int = level.[int r.Id]
    member _.SetLevel(r: Rep, v: int) : unit = level.[int r.Id] <- v

    /// The solution / substitution reached from this root; `ValueNone` while free.
    member _.Link(r: Rep) : SemType voption = link.[int r.Id]
    member _.SetLink(r: Rep, v: SemType voption) : unit = link.[int r.Id] <- v

    /// Measure constraint on the root when it is a numeric type. `ValueNone` for the
    /// overwhelming majority (function types, tuples, non-numeric values); merged on
    /// union by `mergeUnits`. `headZonk` / `substituteWith` STOP following `Link` at a
    /// measure-bearing root so the measure rides on the returned `TyVar`.
    member _.Units(r: Rep) : MeasureTerm voption = units.[int r.Id]
    member _.SetUnits(r: Rep, v: MeasureTerm voption) : unit = units.[int r.Id] <- v

    // --- Write-once region id; NOT migrated on union, so any node has one valid cell
    //     and it stays keyed by raw `TyVarId`. ---

    member _.Region(tv: TyVarId) : RegionId = region.[int tv]
    member _.SetRegion(tv: TyVarId, r: RegionId) : unit = region.[int tv] <- r

    /// SRTP member-trait bounds, keyed by representative — the store home of the
    /// former `TypeVar.SrtpBounds` slot. Grow-only + `solved` replaces the shared
    /// `MemberSignature.Resolved` dedup flag. `union` join carries `loser @ winner`.
    member val Srtp = BoundTable<MemberSignature>(fun winner loser -> loser @ winner) with get

    /// Type-parameter constraints, keyed by representative — the store home of the
    /// former `TypeVar.Constraints` slot. A `[<Struct>]` `SemanticConstraint` carries
    /// no reference identity, and `dischargeConstraints`' compositional `propagateToFreeArgs`
    /// depends on value independence, so this family keeps its per-discharge remainder
    /// (rewritten through `Set`, off-node) rather than a reference `solved` set; the
    /// `union` join dedups by `Kind`.
    member val Constraints = PayloadList<SemanticConstraint>(PayloadJoin.constraintsByKind) with get

    /// Deferred dot-accesses parked on a still-free receiver, keyed by representative
    /// — the store home of the former `TypeVar.PendingDotAccess` slot. Grow-only +
    /// `solved`: an access resolved once the receiver grounds is recorded, so a
    /// re-discharge and the leftover-unresolved check see only the live (unsolved) ones.
    /// `union` join carries `loser @ winner`.
    member val Pda = BoundTable<DeferredMemberAccess>(fun winner loser -> loser @ winner) with get

    /// Default-constraint chains (`default ^T : …`), keyed by representative — the
    /// store home of the former `TypeVar.Defaults` slot. Generalisation consumes a
    /// TyVar's chain WHOLESALE (clearing it when a default fires or nothing is left to
    /// chase), so this family keeps its per-tv clear (through `Set`) rather than a
    /// per-item `solved` set; the `union` join preserves `winner @ loser`.
    member val Defaults = PayloadList<SemType>(fun winner loser -> winner @ loser) with get

    /// Fold a `union` loser's deferred-constraint payload into the surviving
    /// representative — one associative set-union join per family, each via the
    /// combinator baked into its table (Constraints dedup by `Kind`; SRTP / pending
    /// dot-accesses `loser @ winner`; defaults `winner @ loser`). Payload lives only
    /// under the representative. THE single payload-merge seam for `UnionFind.union`.
    member this.MergePayloads(winner: Rep, loser: Rep) : unit =
        this.Constraints.Join(winner, loser)
        this.Srtp.Join(winner, loser)
        this.Pda.Join(winner, loser)
        this.Defaults.Join(winner, loser)

/// Union-find over the per-file `TypeStore`. `find` is the SOLE producer of `Rep`
/// (the private case above is unconstructible outside this file), so every
/// root-authoritative read is gated behind a real path-compression walk. Lives here
/// (rather than a separate file) precisely so `find` can mint `Rep`.
module UnionFind =

    /// Iterative rather than recursive to avoid stack pressure on long chains.
    /// Path-compresses, then hands back the root WRAPPED as a `Rep`.
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
                // Unreachable post-phase-1; defensive terminate.
                cursor <- root
            | ValueSome next ->
                store.SetParent(cursor, ValueSome root)
                cursor <- next

        Rep root

    /// Does NOT resolve Link / Units / Constraints / SrtpBounds — the caller
    /// (Unification) handles compatibility checks and on-unified callbacks.
    /// The surviving root inherits `min` of the two roots' Levels so the
    /// representative remains authoritative for Rémy's level-based
    /// generalisation.
    let union (store: TypeStore) (a: TyVarId) (b: TyVarId) : unit =
        let rootA = find store a
        let rootB = find store b

        if rootA <> rootB then
            let mergedLevel = min (store.Level rootA) (store.Level rootB)

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

    let inSameClass (store: TypeStore) (a: TyVarId) (b: TyVarId) : bool = find store a = find store b

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
            | _ -> TyVar root.Id
        | _ -> t
