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
/// pool. No consumer keys a dictionary on a handle TODAY — the six identity tables key
/// on the bare `ExprPoolId` and get their disambiguation from being per-unit
/// (`LayoutModel.UnitLayout.FunVerdicts`) — but a handle is the key that would not need
/// that argument.
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
            ExprBase = pools.ExprShapes.Length
            PatBase = pools.PatShapes.Length
            DeclBase = pools.DeclShapes.Length
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
    // comparison. These are the per-domain equivalents of reading `FrozenPools.ExprShapes.[i]`
    // and friends directly, and are what `TastAccessor` reads
    // instead, so it never has to know which layer answered.

    let exprShape (b: PoolBuilder) (ExprPoolId i) : ExprShape =
        if i < b.ExprBase then
            b.Base.ExprShapes.[i]
        else
            b.OvExprs.[i - b.ExprBase].Shape

    let exprTy (b: PoolBuilder) (ExprPoolId i) : FrozenType =
        if i < b.ExprBase then
            b.Base.ExprTys.[i]
        else
            b.OvExprs.[i - b.ExprBase].Ty

    let exprTok (b: PoolBuilder) (ExprPoolId i) : SyntaxToken =
        if i < b.ExprBase then
            b.Base.ExprToks.[i]
        else
            b.OvExprs.[i - b.ExprBase].Tok

    let exprChildren (b: PoolBuilder) (ExprPoolId i) : ExprPoolId[] =
        if i < b.ExprBase then
            b.Base.ExprChildren.[i]
        else
            b.OvExprs.[i - b.ExprBase].Children

    let exprPatChildren (b: PoolBuilder) (ExprPoolId i) : PatPoolId[] =
        if i < b.ExprBase then
            b.Base.ExprPatChildren.[i]
        else
            b.OvExprs.[i - b.ExprBase].PatChildren

    let exprVarBinder (b: PoolBuilder) (ExprPoolId i) : BinderId voption =
        if i < b.ExprBase then
            b.Base.ExprVarBinder.[i]
        else
            b.OvExprs.[i - b.ExprBase].VarBinder

    let exprPayload (b: PoolBuilder) (ExprPoolId i) : ExprPayload =
        if i < b.ExprBase then
            b.Base.ExprPayloads.[i]
        else
            b.OvExprs.[i - b.ExprBase].Payload

    /// The whole row at `id` — the base columns gathered, or the overlay row as stored.
    /// This is what a rewrite starts from, and is reached through `copyExprWith` (which
    /// hands the row to an edit function), never directly.
    let private exprRow (b: PoolBuilder) (id: ExprPoolId) : ExprRow =
        let (ExprPoolId i) = id

        if i < b.ExprBase then
            {
                Shape = b.Base.ExprShapes.[i]
                Ty = b.Base.ExprTys.[i]
                Tok = b.Base.ExprToks.[i]
                Children = b.Base.ExprChildren.[i]
                PatChildren = b.Base.ExprPatChildren.[i]
                VarBinder = b.Base.ExprVarBinder.[i]
                Payload = b.Base.ExprPayloads.[i]
            }
        else
            b.OvExprs.[i - b.ExprBase]

    let patShape (b: PoolBuilder) (PatPoolId i) : PatShape =
        if i < b.PatBase then
            b.Base.PatShapes.[i]
        else
            b.OvPats.[i - b.PatBase].Shape

    let patTy (b: PoolBuilder) (PatPoolId i) : FrozenType =
        if i < b.PatBase then
            b.Base.PatTys.[i]
        else
            b.OvPats.[i - b.PatBase].Ty

    let patTok (b: PoolBuilder) (PatPoolId i) : SyntaxToken =
        if i < b.PatBase then
            b.Base.PatToks.[i]
        else
            b.OvPats.[i - b.PatBase].Tok

    let patChildren (b: PoolBuilder) (PatPoolId i) : PatPoolId[] =
        if i < b.PatBase then
            b.Base.PatChildren.[i]
        else
            b.OvPats.[i - b.PatBase].Children

    let patPayload (b: PoolBuilder) (PatPoolId i) : PatPayload =
        if i < b.PatBase then
            b.Base.PatPayloads.[i]
        else
            b.OvPats.[i - b.PatBase].Payload

    /// The whole row at `id` — see `exprRow`.
    let private patRow (b: PoolBuilder) (id: PatPoolId) : PatRow =
        let (PatPoolId i) = id

        if i < b.PatBase then
            {
                Shape = b.Base.PatShapes.[i]
                Ty = b.Base.PatTys.[i]
                Tok = b.Base.PatToks.[i]
                Children = b.Base.PatChildren.[i]
                Payload = b.Base.PatPayloads.[i]
            }
        else
            b.OvPats.[i - b.PatBase]

    let declShape (b: PoolBuilder) (DeclPoolId i) : DeclShape =
        if i < b.DeclBase then
            b.Base.DeclShapes.[i]
        else
            b.OvDecls.[i - b.DeclBase].Shape

    let declExprChildren (b: PoolBuilder) (DeclPoolId i) : ExprPoolId[] =
        if i < b.DeclBase then
            b.Base.DeclExprChildren.[i]
        else
            b.OvDecls.[i - b.DeclBase].ExprChildren

    let declPatChildren (b: PoolBuilder) (DeclPoolId i) : PatPoolId[] =
        if i < b.DeclBase then
            b.Base.DeclPatChildren.[i]
        else
            b.OvDecls.[i - b.DeclBase].PatChildren

    let declPayload (b: PoolBuilder) (DeclPoolId i) : DeclPayload =
        if i < b.DeclBase then
            b.Base.DeclPayloads.[i]
        else
            b.OvDecls.[i - b.DeclBase].Payload

    /// The whole row at `id` — see `exprRow`.
    let private declRow (b: PoolBuilder) (id: DeclPoolId) : DeclRow =
        let (DeclPoolId i) = id

        if i < b.DeclBase then
            {
                Shape = b.Base.DeclShapes.[i]
                ExprChildren = b.Base.DeclExprChildren.[i]
                PatChildren = b.Base.DeclPatChildren.[i]
                Payload = b.Base.DeclPayloads.[i]
            }
        else
            b.OvDecls.[i - b.DeclBase]

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
    // The payoff of the columnar form. A child substitution (what `TastLower.mapChildren`
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
        if row' = row then id else appendExpr b row'


    /// Append a copy of decl row `id` with `edit` applied — see `copyExprWith`.
    let copyDeclWith (b: PoolBuilder) (id: DeclPoolId) (edit: DeclRow -> DeclRow) : DeclPoolId =
        let row = declRow b id
        let row' = edit row
        if row' = row then id else appendDecl b row'

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
        // destination interns it: a reference minted against `dest` must resolve, and a
        // retype changes types, never identity.
        match row.Payload with
        | PatPayload.NamedSimple binding -> internBinder dest binding |> ignore
        | _ -> ()

        appendPat
            dest
            { row with
                Ty = fTy row.Ty
                Children = kids
                Payload = PatPayload.mapTys fTy row.Payload
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
            InternBinder = internBinder b >> ignore
            AddExpr = appendExpr b
            AddPat = appendPat b
            AddDecl = appendDecl b
            OnExprPooled =
                fun _ varBinding id ->
                    match varBinding with
                    | ValueSome k -> setVarBinder b id (internBinder b k)
                    | ValueNone -> ()
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
        TastUnpool.substitutePat row.Ty row.Tok row.Payload (row.Children |> Array.map (patTree b))

    /// The DU subtree an expression id denotes — see `patTree`. A `Var`'s binder edge is
    /// resolved back through the pool's own binder column, so a reference minted in the
    /// overlay names the same `NodeKey` it would have read. Reached through `declTree`:
    /// the cross-unit wire carries whole declarations, never a bare expression.
    let rec private exprTree (b: PoolBuilder) (id: ExprPoolId) : Frozen.TExpr =
        let row = exprRow b id

        TastUnpool.substituteExpr
            row.Ty
            row.Tok
            (row.VarBinder |> ValueOption.map (binderKey b))
            row.Payload
            (row.Children |> Array.map (exprTree b))
            (row.PatChildren |> Array.map (patTree b))

    /// The DU subtree a declaration id denotes — see `patTree`.
    let declTree (b: PoolBuilder) (id: DeclPoolId) : Frozen.TDecl =
        let row = declRow b id

        TastUnpool.substituteDecl
            (exprTree b)
            row.Payload
            (row.ExprChildren |> Array.map (exprTree b))
            (row.PatChildren |> Array.map (patTree b))
