namespace XParsec.FSharp.SemanticAnalysis

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
/// per-representative list keyed by the metavar's id. The backing dictionary is
/// PRIVATE — the only writes are `Set`/`Prepend`/`Append`/`Join`, so a family has no
/// off-seam setter once its node slot is gone (the single-mutation-seam guarantee).
/// Payload lives only under the representative id; `Join` folds a `union` loser's
/// items into the winner via the family's `combine` (fixed at construction), order
/// preserved so diagnostics stay deterministic.
[<Sealed>]
type PayloadList<'T>(combine: 'T list -> 'T list -> 'T list) =
    let table = System.Collections.Generic.Dictionary<int, 'T list>()

    let at (root: TyVarId) : 'T list =
        match table.TryGetValue(int root) with
        | true, xs -> xs
        | _ -> []

    /// Every item accrued under `root`, in insertion / merge order.
    member _.Items(root: TyVarId) : 'T list = at root

    member _.IsEmpty(root: TyVarId) : bool = List.isEmpty (at root)

    /// Replace `root`'s items; an empty list drops the entry so an ungrounded-but-empty
    /// root leaves no residue. THE single write path.
    member _.Set(root: TyVarId, items: 'T list) : unit =
        if List.isEmpty items then
            table.Remove(int root) |> ignore
        else
            table.[int root] <- items

    /// Accrue `item` at the head (`item :: node.slot`).
    member this.Prepend(root: TyVarId, item: 'T) : unit = this.Set(root, item :: at root)

    /// Accrue `item` at the tail (`node.slot @ [item]`).
    member this.Append(root: TyVarId, item: 'T) : unit = this.Set(root, at root @ [ item ])

    /// The `union` join: fold `loser`'s items into `winner` via the family's baked
    /// `combine` (winnerItems → loserItems → joined), then empty `loser`. The
    /// combinator is a construction-time property of the family, so no caller
    /// re-specifies the merge order. Associative/idempotent set-union in spirit.
    member this.Join(winner: TyVarId, loser: TyVarId) : unit =
        let l = at loser

        if not (List.isEmpty l) then
            this.Set(winner, combine (at winner) l)
            table.Remove(int loser) |> ignore

/// A payload family whose items DISCHARGE ONE AT A TIME through a shared reference
/// identity — SRTP bounds and deferred dot-accesses. Wraps the grow-only
/// `PayloadList` with a reference-keyed `solved` set: a discharged item is recorded
/// (never removed), so a drain reads only the `Live` items, never rewrites a
/// shrinking remainder, and never re-fires an item. Because `solved` keys by
/// reference, ONE shared item stamped on several participating typars discharges
/// exactly once, and the marking survives a `union` remap (the item objects are
/// unchanged). `'T` is therefore a reference type.
[<Sealed>]
type BoundTable<'T when 'T: not struct>(combine: 'T list -> 'T list -> 'T list) =
    let items = PayloadList<'T>(combine)

    let solved = System.Collections.Generic.HashSet<'T>(HashIdentity.Reference)

    member _.Items(root: TyVarId) : 'T list = items.Items root

    /// The still-undischarged items under `root`, in insertion / merge order.
    member _.Live(root: TyVarId) : 'T list =
        items.Items root |> List.filter (fun x -> not (solved.Contains x))

    member _.IsEmpty(root: TyVarId) : bool = items.IsEmpty root
    member _.Prepend(root: TyVarId, item: 'T) : unit = items.Prepend(root, item)

    member _.Join(winner: TyVarId, loser: TyVarId) : unit = items.Join(winner, loser)

    /// Record `item` as discharged so `Live` skips it from now on.
    member _.Solve(item: 'T) : unit = solved.Add item |> ignore

    member _.IsSolved(item: 'T) : bool = solved.Contains item

/// The metavar **arena** for one file: the single authority that mints `TypeVar`
/// handles with dense, monotone ids, and the owner of the id-indexed structural
/// arrays (union-find `parent`/`rank`, the root-authoritative `level`/`link`/`units`,
/// the write-once `region`) plus the deferred-constraint side-tables. The node is a
/// thin handle carrying only its `Id`; every former slot is read/written HERE. Grow-only
/// per file; ids are never reused, so a `TyVarId` is a stable array index. One instance
/// lives on each `PassContext`.
[<Sealed>]
type TypeStore() =
    let mutable nextId = 0
    let mutable capacity = 0
    // Union-find structure. `parent.[i] = i` marks a ROOT (the former
    // `TypeVar.Parent : TypeVar voption` `ValueNone ≡ self` convention); any other
    // entry points one step up the tree, path-compressed by `find`.
    let mutable parent: int[] = Array.empty
    let mutable rank: int[] = Array.empty
    // Authoritative ON THE ROOT (`UnionFind.find` first): Rémy level, the solution
    // link, and the measure carrier.
    let mutable level: int[] = Array.empty
    let mutable link: SemType voption[] = Array.empty
    let mutable units: MeasureTerm voption[] = Array.empty
    // Write-once region id — NOT migrated on union (unlike the root-authoritative
    // slots), so it stays a plain per-node cell.
    let mutable region: RegionId[] = Array.empty
    // id -> handle, so `find` / `union` still RETURN the root `TypeVar` node the
    // reference-identity call sites expect.
    let mutable nodes: TypeVar[] = Array.empty

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
            nodes <- growStore nodes capacity newCap Unchecked.defaultof<TypeVar>
            capacity <- newCap

    /// Mint a fresh metavar handle carrying the next dense id. THE single
    /// construction seam — every `TypeVar` in a file is born here so its id
    /// indexes this store. A fresh var is its own union-find root (`parent.[id] = id`)
    /// at level 0, unlinked, un-measured, region-unknown.
    member _.NewTypeVar() : TypeVar =
        let id = nextId
        ensureCapacity (id + 1)
        let tv = TypeVar(LanguagePrimitives.Int32WithMeasure<tyVarId> id)
        parent.[id] <- id
        rank.[id] <- 0
        level.[id] <- 0
        link.[id] <- ValueNone
        units.[id] <- ValueNone
        region.[id] <- RegionId.Unknown
        nodes.[id] <- tv
        nextId <- nextId + 1
        tv

    /// Count of metavars minted so far (the dense id upper bound).
    member _.Count = nextId

    /// The root `TypeVar` handle for a dense id — how `find` / `union` recover the
    /// node from the `parent` array.
    member _.Node(id: TyVarId) : TypeVar = nodes.[int id]

    // --- Union-find structure. Owned by `UnionFind`; do not poke elsewhere. ---

    /// `ValueNone` ≡ `tv` is its own root, preserving the exact convention the
    /// `find` / `union` algorithm reads off the former `TypeVar.Parent` slot.
    member _.Parent(tv: TypeVar) : TypeVar voption =
        let i = int tv.Id
        let p = parent.[i]
        if p = i then ValueNone else ValueSome nodes.[p]

    member _.SetParent(tv: TypeVar, p: TypeVar voption) : unit =
        parent.[int tv.Id] <-
            match p with
            | ValueSome r -> int r.Id
            | ValueNone -> int tv.Id

    member _.Rank(tv: TypeVar) : int = rank.[int tv.Id]
    member _.SetRank(tv: TypeVar, r: int) : unit = rank.[int tv.Id] <- r

    // --- Authoritative on the union-find root (`UnionFind.find` first). ---

    /// Let-depth at which the root was minted (Rémy's levels). `union` propagates
    /// `min` of the two roots' levels to the survivor; `occursAndAdjust` lowers a
    /// reachable level; generalisation quantifies roots whose level exceeds the
    /// enclosing scope.
    member _.Level(tv: TypeVar) : int = level.[int tv.Id]
    member _.SetLevel(tv: TypeVar, v: int) : unit = level.[int tv.Id] <- v

    /// The solution / substitution reached from this root; `ValueNone` while free.
    member _.Link(tv: TypeVar) : SemType voption = link.[int tv.Id]
    member _.SetLink(tv: TypeVar, v: SemType voption) : unit = link.[int tv.Id] <- v

    /// Measure constraint on the root when it is a numeric type. `ValueNone` for the
    /// overwhelming majority (function types, tuples, non-numeric values); merged on
    /// union by `mergeUnits`. `headZonk` / `substituteWith` STOP following `Link` at a
    /// measure-bearing root so the measure rides on the returned `TyVar`.
    member _.Units(tv: TypeVar) : MeasureTerm voption = units.[int tv.Id]
    member _.SetUnits(tv: TypeVar, v: MeasureTerm voption) : unit = units.[int tv.Id] <- v

    // --- Write-once region id; NOT migrated on union. ---

    member _.Region(tv: TypeVar) : RegionId = region.[int tv.Id]
    member _.SetRegion(tv: TypeVar, r: RegionId) : unit = region.[int tv.Id] <- r

    /// SRTP member-trait bounds, keyed by representative id — the store home of the
    /// former `TypeVar.SrtpBounds` slot. Grow-only + `solved` replaces the shared
    /// `MemberSignature.Resolved` dedup flag. `union` join carries `loser @ winner`.
    member val Srtp = BoundTable<MemberSignature>(fun winner loser -> loser @ winner) with get

    /// Type-parameter constraints, keyed by representative id — the store home of the
    /// former `TypeVar.Constraints` slot. A `[<Struct>]` `SemanticConstraint` carries
    /// no reference identity, and `drainConstraints`' compositional `propagateToFreeArgs`
    /// depends on value independence, so this family keeps its per-drain remainder
    /// (rewritten through `Set`, off-node) rather than a reference `solved` set; the
    /// `union` join dedups by `Kind`.
    member val Constraints = PayloadList<SemanticConstraint>(PayloadJoin.constraintsByKind) with get

    /// Deferred dot-accesses parked on a still-free receiver, keyed by representative
    /// id — the store home of the former `TypeVar.PendingDotAccess` slot. Grow-only +
    /// `solved`: an access resolved once the receiver grounds is recorded, so a
    /// re-drain and the leftover-unresolved check see only the live (unsolved) ones.
    /// `union` join carries `loser @ winner`.
    member val Pda = BoundTable<DeferredMemberAccess>(fun winner loser -> loser @ winner) with get

    /// Default-constraint chains (`default ^T : …`), keyed by representative id — the
    /// store home of the former `TypeVar.Defaults` slot. Generalisation consumes a
    /// TyVar's chain WHOLESALE (clearing it when a default fires or nothing is left to
    /// chase), so this family keeps its per-tv clear (through `Set`) rather than a
    /// per-item `solved` set; the `union` join preserves `winner @ loser`.
    member val Defaults = PayloadList<SemType>(fun winner loser -> winner @ loser) with get

    /// Fold a `union` loser's deferred-constraint payload into the surviving
    /// representative — one associative set-union join per family, each via the
    /// combinator baked into its table (Constraints dedup by `Kind`; SRTP / pending
    /// dot-accesses `loser @ winner`; defaults `winner @ loser`). Payload lives only
    /// under the representative id. THE single payload-merge seam for `UnionFind.union`.
    member this.MergePayloads(winner: TyVarId, loser: TyVarId) : unit =
        this.Constraints.Join(winner, loser)
        this.Srtp.Join(winner, loser)
        this.Pda.Join(winner, loser)
        this.Defaults.Join(winner, loser)
