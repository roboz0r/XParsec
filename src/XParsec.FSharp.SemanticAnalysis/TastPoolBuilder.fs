namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec.FSharp.Lexer
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
            OvDecls: ResizeArray<DeclRow>
            OvPats: ResizeArray<PatRow>
            /// How many binders this overlay has handed out — a COUNT and no column,
            /// because a minted binder has nothing a column could hold: no source spells
            /// it, so its naming is its slot (`BinderNaming.Minted`) and its anchor is
            /// `Anchor.nowhere`. `mintBinder` is its sole appender.
            mutable OvBinderCount: int
            /// The binder key `declTree` hands a DU-typed consumer for each binder of this
            /// pool, and the counter it mints them from. Per BUILDER, not per drain: two
            /// drains of one subtree are two views of the same binders, so they must name
            /// them alike — see `declTree` for why the builder is also the LARGEST scope
            /// either needs.
            DrainedBinderKeys: Dictionary<BinderId, NodeKey>
            mutable DrainCount: int
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

    /// Open an overlay over `pools`. The base is never copied, mutated, or indexed: an id
    /// resolves by comparison against the layer boundary, so opening a builder is four
    /// field reads.
    let openOver (pools: FrozenPools) : PoolBuilder =
        {
            Base = pools
            ExprBase = pools.ExprPayloads.Length
            PatBase = pools.PatPayloads.Length
            DeclBase = pools.DeclPayloads.Length
            BinderBase = pools.BinderNames.Length
            OvExprs = ResizeArray()
            OvPats = ResizeArray()
            OvDecls = ResizeArray()
            OvBinderCount = 0
            DrainedBinderKeys = Dictionary()
            DrainCount = 0
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

    /// The node's type, across both layers. The BASE column holds a row id of the base
    /// pool's own type table and is resolved through it; an OVERLAY row holds the type
    /// itself, because a lowering that retypes a node (`ClosureVerdictRewrite`,
    /// `copyPatTreeInto`) mints a type the frozen unit never interned — and the base table
    /// is immutable, which is what makes stacking sound in the first place.
    let exprTy (b: PoolBuilder) (id: ExprPoolId) : FrozenType =
        readExpr b id (fun p i -> p.Types.[p.ExprTys.[i]]) (fun r -> r.Ty)

    /// Where the node SITS: its anchor token's index in the file's `Lexed`, or `ValueNone`
    /// where no source spells it (an overlay-minted node, an `.fsi` contract's
    /// reconstructed pattern). The column stores that absence as the negative space of the
    /// index; this is where it is decoded, so no reader downstream meets a raw negative.
    let exprTok (b: PoolBuilder) (id: ExprPoolId) : Anchor =
        readExpr b id (fun p i -> p.ExprToks.[i]) (fun r -> r.Tok)

    // The child edges, by POSITION and as a whole list. The base column is CSR
    // (`ChildColumn`), so `count`/`item` read the flat id array where the array forms have
    // to cut a row out of it — which is why every consumer that wants ONE named child (an
    // `App`'s fn/arg, a `Let`'s value/body) goes through the positional pair. The array
    // forms are for the views whose payload IS a list and for a generic walk.

    let exprChildCount (b: PoolBuilder) (id: ExprPoolId) : int =
        readExpr b id (fun p i -> ChildColumn.count p.ExprChildren i) (fun r -> r.Children.Length)

    let exprChild (b: PoolBuilder) (id: ExprPoolId) (k: int) : ExprPoolId =
        readExpr b id (fun p i -> ChildColumn.item p.ExprChildren i k) (fun r -> r.Children.[k])

    let exprPatChild (b: PoolBuilder) (id: ExprPoolId) (k: int) : PatPoolId =
        readExpr b id (fun p i -> ChildColumn.item p.ExprPatChildren i k) (fun r -> r.PatChildren.[k])

    let exprChildren (b: PoolBuilder) (id: ExprPoolId) : ExprPoolId[] =
        readExpr b id (fun p i -> ChildColumn.slice p.ExprChildren i) (fun r -> r.Children)

    let exprPatChildren (b: PoolBuilder) (id: ExprPoolId) : PatPoolId[] =
        readExpr b id (fun p i -> ChildColumn.slice p.ExprPatChildren i) (fun r -> r.PatChildren)

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
                    Ty = p.Types.[p.ExprTys.[i]]
                    Tok = p.ExprToks.[i]
                    Children = ChildColumn.slice p.ExprChildren i
                    PatChildren = ChildColumn.slice p.ExprPatChildren i
                    VarBinder = p.ExprVarBinder.[i]
                    Payload = p.ExprPayloads.[i]
                }
            )
            (fun r -> r)

    /// The pattern twin of `exprTy`.
    let patTy (b: PoolBuilder) (id: PatPoolId) : FrozenType =
        readPat b id (fun p i -> p.Types.[p.PatTys.[i]]) (fun r -> r.Ty)

    /// The pattern twin of `exprTok`.
    let patTok (b: PoolBuilder) (id: PatPoolId) : Anchor =
        readPat b id (fun p i -> p.PatToks.[i]) (fun r -> r.Tok)

    /// The pattern twin of `exprChild`.
    let patChild (b: PoolBuilder) (id: PatPoolId) (k: int) : PatPoolId =
        readPat b id (fun p i -> ChildColumn.item p.PatChildren i k) (fun r -> r.Children.[k])

    let patChildren (b: PoolBuilder) (id: PatPoolId) : PatPoolId[] =
        readPat b id (fun p i -> ChildColumn.slice p.PatChildren i) (fun r -> r.Children)

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
                    Ty = p.Types.[p.PatTys.[i]]
                    Tok = p.PatToks.[i]
                    Children = ChildColumn.slice p.PatChildren i
                    Payload = p.PatPayloads.[i]
                }
            )
            (fun r -> r)

    /// The declaration twins of `exprChild` — one expr / pat root by position.
    let declExprChild (b: PoolBuilder) (id: DeclPoolId) (k: int) : ExprPoolId =
        readDecl b id (fun p i -> ChildColumn.item p.DeclExprChildren i k) (fun r -> r.ExprChildren.[k])

    let declPatChild (b: PoolBuilder) (id: DeclPoolId) (k: int) : PatPoolId =
        readDecl b id (fun p i -> ChildColumn.item p.DeclPatChildren i k) (fun r -> r.PatChildren.[k])

    let declExprChildren (b: PoolBuilder) (id: DeclPoolId) : ExprPoolId[] =
        readDecl b id (fun p i -> ChildColumn.slice p.DeclExprChildren i) (fun r -> r.ExprChildren)

    let declPatChildren (b: PoolBuilder) (id: DeclPoolId) : PatPoolId[] =
        readDecl b id (fun p i -> ChildColumn.slice p.DeclPatChildren i) (fun r -> r.PatChildren)

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
                    ExprChildren = ChildColumn.slice p.DeclExprChildren i
                    PatChildren = ChildColumn.slice p.DeclPatChildren i
                    Payload = p.DeclPayloads.[i]
                }
            )
            (fun r -> r)

    /// How a binder id is SPELLED, across both layers: the base pool's naming column, or
    /// `Minted` for a binder this overlay handed out — a lowering's own binder is spelled
    /// by no source, so its slot is all there is to name it by.
    let binderNaming (b: PoolBuilder) (id: BinderId) : BinderNaming =
        let (BinderId i) = id

        if i < b.BinderBase then
            FrozenPools.binderNaming b.Base id
        else
            BinderNaming.Minted id

    /// Where a binder's name is spelled, across both layers — the anchor a span or a
    /// line/column is taken from. `Anchor.nowhere` where no node spells the binder: an
    /// overlay-minted one, and a declaration's pattern-less key slot (see
    /// `FrozenPools.BinderToks`).
    let binderTok (b: PoolBuilder) (id: BinderId) : Anchor =
        let (BinderId i) = id

        if i < b.BinderBase then
            b.Base.BinderToks.[i]
        else
            Anchor.nowhere

    // The size of the expr and binder id spaces: every id below the count resolves, and
    // the next append takes the count itself. Only these two exist because only these
    // two are asked — the question a caller has is "did this rewrite append?" (of the
    // expr space) and "did this mint a binder?" (of the binder space). The pat and decl
    // counts would be surface for symmetry's sake.

    let exprCount (b: PoolBuilder) : int = b.ExprBase + b.OvExprs.Count
    let binderCount (b: PoolBuilder) : int = b.BinderBase + b.OvBinderCount

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

    /// A binder id belonging to NO node of the base pool — the definition site a lowering
    /// introduces (a `use`'s dispose temporary, an eta bridge's parameter). Past the layer
    /// boundary, so it can never collide with a base binder.
    ///
    /// It takes no argument, and cannot: a binder's identity IS its slot, so there is
    /// nothing to intern AGAINST. A mint site that needs its new binder referenced hands
    /// the returned id to every reference it builds, which is also what makes a reference
    /// to a base binder unable to accidentally mint one.
    let mintBinder (b: PoolBuilder) : BinderId =
        let id = BinderId(b.BinderBase + b.OvBinderCount)
        b.OvBinderCount <- b.OvBinderCount + 1
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

    // The same row copies WITHOUT the unchanged-row shortcut. A rewrite wants the shortcut —
    // it is what keeps a cached id naming the same node. A COPY does not: an inline entry's
    // body is copied once per call site precisely so the two copies are different nodes, and
    // a node whose row happens to be unchanged (a constant already anchored where the copy
    // moves it) would otherwise collapse the two back onto one id — and with it the binders
    // the copies were freshened to keep apart.

    /// Append a FRESH copy of expr row `id` with `edit` applied, however little moved.
    let copyExprFresh (b: PoolBuilder) (id: ExprPoolId) (edit: ExprRow -> ExprRow) : ExprPoolId =
        appendExpr b (edit (exprRow b id))

    /// The pattern twin of `copyExprFresh`.
    let copyPatFresh (b: PoolBuilder) (id: PatPoolId) (edit: PatRow -> PatRow) : PatPoolId =
        appendPat b (edit (patRow b id))

    /// The file this pool's `Anchor`s index (`FrozenPools.Origin`) — the base's, an overlay
    /// deriving nodes onto the very file it was opened over. What a consumer holding a stated
    /// domain compares against to learn whether it is this unit's own or a producer's.
    let origin (b: PoolBuilder) : OriginFile = b.Base.Origin

    /// How many resolved-specialization entries the table holds — the bound every
    /// `SpecializationId` an edge carries is inside.
    let specializationCount (b: PoolBuilder) : int = b.Base.Specializations.Length

    /// The resolved-specialization entry a `SpecializationId` names, bounds-checked. The
    /// entries are the BASE pool's — an overlay derives nodes, never table entries — so an
    /// id past the array is a minting bug rather than a graph shape, and every walk of the
    /// table faults on it identically instead of inventing its own message.
    let specialization (b: PoolBuilder) (spec: SpecializationId) : PooledSpecialization =
        let (SpecializationId i) = spec

        if i < 0 || i >= b.Base.Specializations.Length then
            failwithf "TastPoolBuilder: specialization %d is out of range (%d entries)" i b.Base.Specializations.Length

        b.Base.Specializations.[i]

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

        // The copy is a node of DEST, and a binder id means nothing outside the pool that
        // issued it — a source binder cannot be lent to another pool, only re-introduced
        // there. So a `NamedSimple` copy introduces a binder of dest's own, exactly as the
        // `.fsi` extractor mints one for a contract parameter.
        let payload =
            match PatPayload.mapTys fTy row.Payload with
            | PatPayload.NamedSimple _ -> PatPayload.NamedSimple(mintBinder dest)
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
    let private sinkOf (b: PoolBuilder) : TastPools.PoolSink<Anchor, BinderId> =
        {
            // The tree being poured in is ALREADY in this pool's identity space, so a
            // definition site IS its id and a reference resolves without a lookup: there
            // is no second spelling of a binder for the two to disagree about.
            InternBinder = BinderKey.identity
            // Likewise already in the stored form: the tree names positions by the very
            // index the columns hold, so nothing here is held to anchoring on a real token.
            Anchor = id
            AddExpr = appendExpr b
            AddPat = appendPat b
            AddDecl = appendDecl b
            OnExprPooled =
                fun ev ->
                    match ev with
                    | TastPools.PooledEvent.VarRef(binder, id) -> setVarBinder b id binder
                    // The overlay keeps no lambda id space: `FunVerdicts` is the frozen
                    // file's own table and a minted lambda has no verdict in it.
                    | TastPools.PooledEvent.LambdaPooled _ -> ()
        }

    /// Pool a freshly minted DU subtree into the overlay and return its flat id. This is
    /// the bridge that lets a mint site keep CONSTRUCTING `Pooled.TExpr` values while
    /// nothing WALKS a DU any more: it hands the node over and gets an id back, so it can
    /// move to native row appends on its own schedule.
    let appendExprTree (b: PoolBuilder) (e: Pooled.TExpr) : ExprPoolId = TastPools.poolExpr (sinkOf b) e

    /// The DU subtree a pattern id denotes, resolved across BOTH layers, node-for-node
    /// (`TastUnpool.substitutePat` re-authors each node from its row, exactly as `ofPools`
    /// does for a whole pool).
    ///
    /// This direction exists for the one channel whose far end is still DU-typed: an
    /// inline template crosses the PACKAGE wire as a `Wire.TDecl`, and a pool id means
    /// nothing outside the pool that issued it, the id space being file-scoped. Private, and
    /// reached through `declTree`: a consumer moving a pattern between POOLS wants
    /// `copyPatTreeInto`, which stays in the columns, and one reading a node of this file's
    /// tree wants the accessor.
    ///
    /// `rename` is the drain's binder freshener — see `declTree`.
    let rec private patTree (rename: BinderId -> NodeKey) (b: PoolBuilder) (at: PatPoolId) : Wire.TPat =
        let row = patRow b at

        TastUnpool.substitutePat
            rename
            row.Ty
            (ForeignAnchor.ofAnchor row.Tok)
            row.Payload
            (row.Children |> Array.map (patTree rename b))

    /// The DU subtree an expression id denotes — see `patTree`. Reached through `declTree`:
    /// the cross-unit wire carries whole declarations, never a bare expression.
    let rec private exprTree (rename: BinderId -> NodeKey) (b: PoolBuilder) (at: ExprPoolId) : Wire.TExpr =
        let row = exprRow b at

        TastUnpool.substituteExpr
            rename
            ForeignAnchor.ofAnchor
            row.Ty
            (ForeignAnchor.ofAnchor row.Tok)
            row.VarBinder
            row.Payload
            (row.Children |> Array.map (exprTree rename b))
            (row.PatChildren |> Array.map (patTree rename b))

    /// The DU subtree a declaration id denotes — see `patTree`.
    ///
    /// Its binders are RE-MINTED, not lent. The columns name a binder by its slot, and a
    /// slot means nothing outside the pool that issued it, so there is no identity here to
    /// hand a DU-typed consumer; what such a consumer needs of one is distinctness within
    /// the drained subtree plus equality between a binder and the references to it, and a
    /// counter-minted key gives both. Naming no position, it also cannot be mistaken for a
    /// key that resolves against some unit's tree.
    ///
    /// The counter and the rename map are the BUILDER's, which is the largest scope either
    /// needs. Two drains that reach one binder must name it alike — they are two views of
    /// the same definition site, and a consumer splicing both has to see that — and a
    /// builder covers every drain of one pool. Two builders may hand out the same key, and
    /// that is harmless: a drained body is renamed again by `Inline.spliceAt` against the
    /// CONSUMING unit's counter before it lands, so no two of them ever meet unfreshened.
    ///
    /// Its anchors are the PRODUCER's, passed through unchanged and merely re-typed: the drain
    /// widens each `Anchor` to a `ForeignAnchor`, which says the integers index THIS pool's
    /// file and not the consumer's. Nothing here blanks or rebases them, and nothing should —
    /// they are the only record of where the body was written, and the marking is what forces
    /// a consumer to say which file it is reading them against (`OriginSources.tokenAt`) or to
    /// give the body a position of its own (`InlineThaw.body`).
    let declTree (b: PoolBuilder) (at: DeclPoolId) : Wire.TDecl =
        let rename (binder: BinderId) : NodeKey =
            match b.DrainedBinderKeys.TryGetValue binder with
            | true, k -> k
            | false, _ ->
                b.DrainCount <- b.DrainCount + 1
                let k = NodeKey.ofSyntheticCounter b.DrainCount NodeKind.SynthDrainedBinder
                b.DrainedBinderKeys.[binder] <- k
                k

        let row = declRow b at

        TastUnpool.substituteDecl
            rename
            ForeignAnchor.ofAnchor
            (exprTree rename b)
            row.Payload
            (row.ExprChildren |> Array.map (exprTree rename b))
            (row.PatChildren |> Array.map (patTree rename b))
