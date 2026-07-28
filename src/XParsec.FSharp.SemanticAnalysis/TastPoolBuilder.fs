namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec.FSharp.Parser

// A STACKED pool: an append-only overlay opened over an immutable `FrozenPools`, so a
// consumer that MINTS nodes (both backends do — every TAST→TAST lowering, the CLR's
// eta-bridging and verdict retype, the JS `substVar` splice) can derive a tree without the
// canonical pool being copied or mutated.
//
// The stacking invariant, which everything here rests on and nothing may break:
//
//   * ids `0 .. n-1` address the BASE columns, ids `>= n` the overlay's own rows — one flat
//     id space, one comparison to tell the layers apart, and the base arrays are neither
//     copied nor touched.
//   * BASE IDS ARE PRESERVED EXACTLY. So an edge minted in the overlay may name a base
//     node BY ITS OWN ID, and any id a consumer cached before the overlay existed stays
//     valid — which is what makes it sound to stack rather than remap.
//
// The read side therefore never learns there are two layers: it resolves an id and gets a
// column value. That is the property `TastAccessor` sits on, and the reason a mint
// site can splice base subtrees into overlay nodes for free.
//
// The overlay is held as ARRAYS OF ROWS, not columns: it is small, is appended one whole
// node at a time, and is read node-at-a-time by the minting site; holding it as rows makes
// a row indivisible at the cost of one indirection on an overlay read.
//
// A builder is WRITE-ONLY DOWNWARD: it is opened over a base, appended to during one
// emission, and dropped. There is no collapse back to a plain `FrozenPools` — nothing
// stores or ships a derived tree (`Freeze.run` is the only producer of the stored form),
// so the only way OUT of a builder is `declTree`, the DU drain the cross-unit inline
// wire needs.

/// An append-only overlay stacked over an immutable base `FrozenPools`. See the file
/// header for the stacking invariant. Not thread-safe: a builder belongs to one backend's
/// emit of one file.
///
/// Reference equality, because it is an IDENTITY (a mutable append target), not a value:
/// two builders over the same base are different pools and their ids are not
/// interchangeable, and a node handle carrying one must stay cheap to compare and hash.
[<ReferenceEquality>]
type PoolBuilder =
    private
        {
            Base: FrozenPools
            /// The layer boundaries — the base column lengths, cached so a read is one
            /// comparison. An id below the boundary is a base id; at or above it, the
            /// overlay row at `id - boundary`.
            ExprBase: int
            PatBase: int
            DeclBase: int
            BinderBase: int
            OvExprs: ResizeArray<ExprRow>
            OvPats: ResizeArray<PatRow>
            OvDecls: ResizeArray<DeclRow>
            /// The overlay binder column — `internBinder` is its sole appender.
            OvBinderKeys: ResizeArray<NodeKey>
            /// `NodeKey` to `BinderId` over BOTH layers, seeded from the base pool's
            /// `BinderKeys`. Interning is keyed by the NodeKey, so a minted reference and
            /// the binder's own defining node land on the same id whichever is walked
            /// first.
            BinderIndex: Dictionary<NodeKey, BinderId>
        }

/// A node HANDLE: a dense pool id together with the pool that resolves it. One generic
/// type for all three domains — `'Id` is `ExprPoolId` / `PatPoolId` / `DeclPoolId`.
///
/// The pool rides the handle rather than being a parameter of every accessor, which is
/// what lets a consumer speak in whole nodes (`e.Body`, `arm.Guard`) exactly as it did
/// when a node WAS the tree, and lets pools that are not a file's tree exist alongside
/// it (an `.fsi`-minted `ValRepr`'s patterns index into their own).
///
/// Equality is the pool's identity plus the id (hence `ReferenceEquality` on
/// `PoolBuilder`), so two ids denote the same node only when they came from the same
/// pool. That is why the identity tables an assembly's emit carries key on the HANDLE
/// (`LayoutModel.UnitLayout.FunVerdicts`, `EmitTypes.EmitContext`'s `…ByNode` set):
/// a bare `ExprPoolId` would be sound only for as long as no two units' tables met,
/// since every pool numbers from 0 and a foreign id would name a different node rather
/// than miss.
[<Struct; NoComparison>]
type Handle<'Id> = { Pool: PoolBuilder; Id: 'Id }

[<RequireQualifiedAccess>]
module TastPoolBuilder =

    /// Open an overlay over `pools`. The base is never copied or mutated; the eager work is
    /// one dictionary over the binder column (a few hundred entries next to tens of
    /// thousands of tree nodes), which every minted `Var` reference resolves through.
    let openOver (pools: FrozenPools) : PoolBuilder =
        let index = Dictionary<NodeKey, BinderId>(pools.BinderKeys.Length)

        for i in 0 .. pools.BinderKeys.Length - 1 do
            index.[pools.BinderKeys.[i]] <- BinderId i

        {
            Base = pools
            ExprBase = pools.ExprPayloads.Length
            PatBase = pools.PatPayloads.Length
            DeclBase = pools.DeclPayloads.Length
            BinderBase = pools.BinderKeys.Length
            OvExprs = ResizeArray()
            OvPats = ResizeArray()
            OvDecls = ResizeArray()
            OvBinderKeys = ResizeArray()
            BinderIndex = index
        }

    /// A builder over no base at all — for nodes that belong to no frozen tree: an
    /// EXTERNAL symbol's `ValRepr` patterns are minted from an `.fsi` contract
    /// (`TastLower.externalValRepr`) and index into no file, yet are read through the
    /// same accessor as any other pattern, so they need a pool of their own.
    let openEmpty () : PoolBuilder = openOver FrozenPools.empty

    // ── the stacked read surface ────────────────────────────────────────────
    //
    // One accessor per column, each resolving a FLAT id across the two layers with a single
    // comparison. These are the per-domain equivalents of reading `FrozenPools.ExprPayloads.[i]`
    // and friends directly, and are what `TastAccessor` reads instead, so it never has to know
    // which layer answered.
    //
    // The layer arithmetic itself is the three `read*` resolvers below and nowhere else.
    // It is the file header's stacking invariant written as code, so an accessor cannot
    // hold a different version of it — which is the whole risk of a surface where every
    // member would otherwise restate the same comparison and the same `- Base` offset.
    // `inline` + `InlineIfLambda`, so a read is still one comparison and one array index.

    let inline private readExpr
        (b: PoolBuilder)
        (ExprPoolId i)
        ([<InlineIfLambda>] ofBase: FrozenPools -> int -> 'a)
        ([<InlineIfLambda>] ofRow: ExprRow -> 'a)
        : 'a =
        if i < b.ExprBase then
            ofBase b.Base i
        else
            ofRow b.OvExprs.[i - b.ExprBase]

    let inline private readPat
        (b: PoolBuilder)
        (PatPoolId i)
        ([<InlineIfLambda>] ofBase: FrozenPools -> int -> 'a)
        ([<InlineIfLambda>] ofRow: PatRow -> 'a)
        : 'a =
        if i < b.PatBase then
            ofBase b.Base i
        else
            ofRow b.OvPats.[i - b.PatBase]

    let inline private readDecl
        (b: PoolBuilder)
        (DeclPoolId i)
        ([<InlineIfLambda>] ofBase: FrozenPools -> int -> 'a)
        ([<InlineIfLambda>] ofRow: DeclRow -> 'a)
        : 'a =
        if i < b.DeclBase then
            ofBase b.Base i
        else
            ofRow b.OvDecls.[i - b.DeclBase]

    let exprTy (b: PoolBuilder) (id: ExprPoolId) : FrozenType =
        readExpr b id (fun p i -> p.ExprTys.[i]) (fun r -> r.Ty)

    let exprTok (b: PoolBuilder) (id: ExprPoolId) : SyntaxToken =
        readExpr b id (fun p i -> p.ExprToks.[i]) (fun r -> r.Tok)

    let exprChildren (b: PoolBuilder) (id: ExprPoolId) : ExprPoolId[] =
        readExpr b id (fun p i -> p.ExprChildren.[i]) (fun r -> r.Children)

    let exprPatChildren (b: PoolBuilder) (id: ExprPoolId) : PatPoolId[] =
        readExpr b id (fun p i -> p.ExprPatChildren.[i]) (fun r -> r.PatChildren)

    let exprVarBinder (b: PoolBuilder) (id: ExprPoolId) : BinderId voption =
        readExpr b id (fun p i -> p.ExprVarBinder.[i]) (fun r -> r.VarBinder)

    let exprPayload (b: PoolBuilder) (id: ExprPoolId) : ExprPayload =
        readExpr b id (fun p i -> p.ExprPayloads.[i]) (fun r -> r.Payload)

    /// The node's shape tag — projected from the payload column, not read from one of its
    /// own, so it is the tag of the payload this very node carries.
    let exprShape (b: PoolBuilder) (id: ExprPoolId) : ExprShape = ExprPayload.shape (exprPayload b id)

    /// The whole row at `id` — the base columns gathered, or the overlay row as stored.
    /// This is what a rewrite starts from, and is reached through `copyExprWith` (which
    /// hands the row to an edit function), never directly.
    let private exprRow (b: PoolBuilder) (id: ExprPoolId) : ExprRow =
        readExpr
            b
            id
            (fun p i ->
                {
                    Ty = p.ExprTys.[i]
                    Tok = p.ExprToks.[i]
                    Children = p.ExprChildren.[i]
                    PatChildren = p.ExprPatChildren.[i]
                    VarBinder = p.ExprVarBinder.[i]
                    Payload = p.ExprPayloads.[i]
                }
            )
            (fun r -> r)

    let patTy (b: PoolBuilder) (id: PatPoolId) : FrozenType =
        readPat b id (fun p i -> p.PatTys.[i]) (fun r -> r.Ty)

    let patTok (b: PoolBuilder) (id: PatPoolId) : SyntaxToken =
        readPat b id (fun p i -> p.PatToks.[i]) (fun r -> r.Tok)

    let patChildren (b: PoolBuilder) (id: PatPoolId) : PatPoolId[] =
        readPat b id (fun p i -> p.PatChildren.[i]) (fun r -> r.Children)

    let patPayload (b: PoolBuilder) (id: PatPoolId) : PatPayload =
        readPat b id (fun p i -> p.PatPayloads.[i]) (fun r -> r.Payload)

    /// The pattern's shape tag — see `exprShape`.
    let patShape (b: PoolBuilder) (id: PatPoolId) : PatShape = PatPayload.shape (patPayload b id)

    /// The whole row at `id` — see `exprRow`.
    let private patRow (b: PoolBuilder) (id: PatPoolId) : PatRow =
        readPat
            b
            id
            (fun p i ->
                {
                    Ty = p.PatTys.[i]
                    Tok = p.PatToks.[i]
                    Children = p.PatChildren.[i]
                    Payload = p.PatPayloads.[i]
                }
            )
            (fun r -> r)

    let declExprChildren (b: PoolBuilder) (id: DeclPoolId) : ExprPoolId[] =
        readDecl b id (fun p i -> p.DeclExprChildren.[i]) (fun r -> r.ExprChildren)

    let declPatChildren (b: PoolBuilder) (id: DeclPoolId) : PatPoolId[] =
        readDecl b id (fun p i -> p.DeclPatChildren.[i]) (fun r -> r.PatChildren)

    let declPayload (b: PoolBuilder) (id: DeclPoolId) : DeclPayload =
        readDecl b id (fun p i -> p.DeclPayloads.[i]) (fun r -> r.Payload)

    /// The declaration's shape tag — see `exprShape`.
    let declShape (b: PoolBuilder) (id: DeclPoolId) : DeclShape = DeclPayload.shape (declPayload b id)

    /// The whole row at `id` — see `exprRow`.
    let private declRow (b: PoolBuilder) (id: DeclPoolId) : DeclRow =
        readDecl
            b
            id
            (fun p i ->
                {
                    ExprChildren = p.DeclExprChildren.[i]
                    PatChildren = p.DeclPatChildren.[i]
                    Payload = p.DeclPayloads.[i]
                }
            )
            (fun r -> r)

    /// The `NodeKey` a binder id names — base or minted.
    let binderKey (b: PoolBuilder) (BinderId i) : NodeKey =
        if i < b.BinderBase then
            b.Base.BinderKeys.[i]
        else
            b.OvBinderKeys.[i - b.BinderBase]

    /// The naming triple a binder id names — projected from that binder's key, not read
    /// from a column of its own. `BinderNaming.ofKey` is a total function of the key, so
    /// a stored naming column would put the same three bits on the wire twice.
    let binderNaming (b: PoolBuilder) (id: BinderId) : BinderNaming = BinderNaming.ofKey (binderKey b id)

    /// The `BinderId` a `NodeKey` names, or `ValueNone` when no definition site in this
    /// pool introduced it. The READ-ONLY counterpart of `internBinder`: a lookup answers
    /// only with a binder the pooled tree actually bears, so a side table consulted
    /// through it cannot silently name a node that does not exist — which is why a
    /// consumer resolving a key to an id must come through here and not `internBinder`
    /// (whose mint-on-miss would manufacture the very identity the check is for).
    let tryBinderId (b: PoolBuilder) (k: NodeKey) : BinderId voption =
        match b.BinderIndex.TryGetValue k with
        | true, id -> ValueSome id
        | false, _ -> ValueNone

    // The size of the expr and binder id spaces: every id below the count resolves, and
    // the next append takes the count itself. Only these two exist because only these
    // two are asked — the question a caller has is "did this rewrite append?" (of the
    // expr space) and "did this mint a binder?" (of the binder space). The pat and decl
    // counts would be surface for symmetry's sake.

    let exprCount (b: PoolBuilder) : int = b.ExprBase + b.OvExprs.Count
    let binderCount (b: PoolBuilder) : int = b.BinderBase + b.OvBinderKeys.Count

    /// The file's decl roots, in source order — the base pool's, since nothing derives a
    /// new top-level decl IN PLACE: a whole-decl rewrite (`TastAccessor.mapDeclExpr`,
    /// `ClosureVerdictRewrite.retypeDecl`) returns the derived id and its caller carries
    /// it, so the root array is never repointed. Copied, so a caller cannot reach into
    /// the immutable base through it.
    let roots (b: PoolBuilder) : DeclPoolId[] = Array.copy b.Base.Roots

    // ── append primitives ───────────────────────────────────────────────────

    /// Append a row to the overlay and return its FLAT id (past the base boundary, so it
    /// can never collide with a base id).
    let appendExpr (b: PoolBuilder) (row: ExprRow) : ExprPoolId =
        let id = b.ExprBase + b.OvExprs.Count
        b.OvExprs.Add row
        ExprPoolId id

    let appendPat (b: PoolBuilder) (row: PatRow) : PatPoolId =
        let id = b.PatBase + b.OvPats.Count
        b.OvPats.Add row
        PatPoolId id

    let appendDecl (b: PoolBuilder) (row: DeclRow) : DeclPoolId =
        let id = b.DeclBase + b.OvDecls.Count
        b.OvDecls.Add row
        DeclPoolId id

    /// The `BinderId` a `NodeKey` names, minting one in the overlay if the base pool never
    /// interned it. Idempotent in the key, which is what lets a minted `Var` reference
    /// resolve before (or without) its defining pattern being appended: whichever site
    /// arrives first creates the entry, and the entry is the key either way.
    let internBinder (b: PoolBuilder) (k: NodeKey) : BinderId =
        match tryBinderId b k with
        | ValueSome id -> id
        | ValueNone ->
            let id = BinderId(b.BinderBase + b.OvBinderKeys.Count)
            b.BinderIndex.Add(k, id)
            b.OvBinderKeys.Add k
            id

    // ── row copies: rewrite without a per-case match ────────────────────────
    //
    // The payoff of the columnar form. A child substitution (what `TastAccessor.mapChildren`
    // does) or a retype (what a closure-verdict rewrite does) is a row copy with one field
    // replaced — no match on the node's shape, no per-case reconstruction, and every
    // untouched child edge is carried across as the id it already was.
    //
    // Each returns the ORIGINAL id when nothing changed, so a rewrite that walks a whole
    // tree and touches nothing appends nothing and leaves every cached id pointing at the
    // same node.

    /// Append a copy of row `id` with `edit` applied — the ONE rewrite primitive, since
    /// every rewrite (substitute the children, retype, replace the payload) is a field
    /// of the same row and the unchanged-row test is the same comparison. Substituting a
    /// node's own children for rewritten ones is the whole of a generic child-mapping
    /// rewrite; a retype touches `Ty` and nothing else.
    let copyExprWith (b: PoolBuilder) (id: ExprPoolId) (edit: ExprRow -> ExprRow) : ExprPoolId =
        let row = exprRow b id
        let row' = edit row
        if ExprRow.same row' row then id else appendExpr b row'

    /// Append a copy of decl row `id` with `edit` applied — see `copyExprWith`.
    let copyDeclWith (b: PoolBuilder) (id: DeclPoolId) (edit: DeclRow -> DeclRow) : DeclPoolId =
        let row = declRow b id
        let row' = edit row
        if DeclRow.same row' row then id else appendDecl b row'

    /// Copy the pattern subtree at `id` into `dest`, mapping every type it carries through
    /// `fTy` — the node types (the `PatTys` column) and the types a payload embeds
    /// (`PatPayload.mapTys`). The COLUMN-level retype: a row copy per node with `Ty`
    /// replaced and the children repointed at their copies, so shape / tok / payload ride
    /// across with no per-case match on the pattern's form.
    ///
    /// `dest` may be a different pool from `b` — a consumer handing out a DERIVED tree
    /// (`FrozenSignature`'s axis re-map) owns its own pool, since the copy is no node of
    /// the source file. Without this a retype had to drain the subtree to the DU, map it
    /// there, and re-pool it, which is a round trip through the representation the pools
    /// replaced.
    let rec copyPatTreeInto
        (dest: PoolBuilder)
        (fTy: FrozenType -> FrozenType)
        (b: PoolBuilder)
        (id: PatPoolId)
        : PatPoolId =
        let row = patRow b id
        let kids = row.Children |> Array.map (copyPatTreeInto dest fTy b)

        // A `NamedSimple` copy introduces the SAME binder as its original, so the
        // destination interns it and the copy names it by DEST's id: a retype changes
        // types, never identity, and a binder id means nothing in another pool.
        let payload =
            match PatPayload.mapTys fTy row.Payload with
            | PatPayload.NamedSimple binder -> PatPayload.NamedSimple(internBinder dest (binderKey b binder))
            | p -> p

        appendPat
            dest
            { row with
                Ty = fTy row.Ty
                Children = kids
                Payload = payload
            }

    // ── the DU bridge, both directions ──────────────────────────────────────

    /// Fill in the `Var` reference edge of a row the pooling walk just appended. Private,
    /// and sound only there: the row is the overlay's own, freshly added, and the walk
    /// cannot have supplied the edge itself (binder ids are the sink's to assign). A base
    /// id would index before the overlay's start and fault.
    let private setVarBinder (b: PoolBuilder) (ExprPoolId i) (binder: BinderId) : unit =
        let j = i - b.ExprBase

        b.OvExprs.[j] <-
            { b.OvExprs.[j] with
                VarBinder = ValueSome binder
            }

    /// The overlay's pooling sink. The walk is `TastPools`' — the one that built the base
    /// pool — so the tree shape is known in exactly one place; only the destination and the
    /// binder-id assignment differ.
    let private sinkOf (b: PoolBuilder) : TastPools.PoolSink =
        {
            // The overlay's binder index is keyed by `NodeKey` because it must also admit a
            // REFERENCE (`VarRef` below, a minted binder pattern), which names its binder
            // that way; the walk's definition sites widen into it.
            InternBinder = BinderKey.identity >> internBinder b
            AddExpr = appendExpr b
            AddPat = appendPat b
            AddDecl = appendDecl b
            OnExprPooled =
                fun ev ->
                    match ev with
                    | TastPools.PooledEvent.VarRef(binder, id) -> setVarBinder b id (internBinder b binder)
                    // The overlay keeps no lambda id space: `FunVerdicts` is the frozen
                    // file's own table and a minted lambda has no verdict in it.
                    | TastPools.PooledEvent.LambdaPooled _ -> ()
        }

    /// Pool a freshly minted DU subtree into the overlay and return its flat id. This is
    /// the bridge that lets a mint site keep CONSTRUCTING `Frozen.TExpr` values while
    /// nothing WALKS a DU any more: it hands the node over and gets an id back, so it can
    /// move to native row appends on its own schedule.
    let appendExprTree (b: PoolBuilder) (e: Frozen.TExpr) : ExprPoolId = TastPools.poolExpr (sinkOf b) e

    /// The DU subtree a pattern id denotes, resolved across BOTH layers, node-for-node
    /// (`TastUnpool.substitutePat` re-authors each node from its row, exactly as `ofPools`
    /// does for a whole pool).
    ///
    /// This direction exists for the one channel whose far end is still DU-typed: an
    /// inline template crosses the PACKAGE wire as a `Frozen.TDecl` (`Frozen.TInlineBody`),
    /// and a pool id means nothing outside the pool that issued it, the id space being
    /// file-scoped. Private, and reached through `declTree`: a consumer moving a pattern
    /// between POOLS wants `copyPatTreeInto`, which stays in the columns, and one reading a
    /// node of this file's tree wants the accessor.
    let rec private patTree (b: PoolBuilder) (id: PatPoolId) : Frozen.TPat =
        let row = patRow b id

        TastUnpool.substitutePat (binderKey b) row.Ty row.Tok row.Payload (row.Children |> Array.map (patTree b))

    /// The DU subtree an expression id denotes — see `patTree`. A `Var`'s binder edge is
    /// resolved back through the pool's own binder column, so a reference minted in the
    /// overlay names the same `NodeKey` it would have read. Reached through `declTree`:
    /// the cross-unit wire carries whole declarations, never a bare expression.
    let rec private exprTree (b: PoolBuilder) (id: ExprPoolId) : Frozen.TExpr =
        let row = exprRow b id

        TastUnpool.substituteExpr
            (binderKey b)
            row.Ty
            row.Tok
            row.VarBinder
            row.Payload
            (row.Children |> Array.map (exprTree b))
            (row.PatChildren |> Array.map (patTree b))

    /// The DU subtree a declaration id denotes — see `patTree`.
    let declTree (b: PoolBuilder) (id: DeclPoolId) : Frozen.TDecl =
        let row = declRow b id

        TastUnpool.substituteDecl
            (binderKey b)
            (exprTree b)
            row.Payload
            (row.ExprChildren |> Array.map (exprTree b))
            (row.PatChildren |> Array.map (patTree b))
