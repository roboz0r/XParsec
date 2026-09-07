namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// An append-only overlay stacked over an immutable base `FrozenPools`, so a consumer that
/// MINTS nodes derives a tree without the base being copied. Reference equality because it
/// is an append target, not a value. Two builders over one base have unrelated ids.
[<ReferenceEquality>]
type PoolBuilder =
    private
        {
            Base: FrozenPools
            /// The layer boundaries, the base column lengths: an id below one is a base id;
            /// at or above it, the overlay row at `id - boundary`.
            ExprBase: int
            PatBase: int
            DeclBase: int
            BoundVarBase: int
            OvExprs: ResizeArray<ExprRow>
            OvDecls: ResizeArray<DeclRow>
            OvPats: ResizeArray<PatRow>
            /// How many bound variables this overlay has handed out. A COUNT rather than a column,
            /// because no source spells a minted bound variable.
            mutable OvBoundVarCount: int
            /// The overlay-minted bound variables bound by a mutable `NamedSimple` pattern row.
            OvMutableBoundVars: HashSet<BoundVarId>
            /// The bound variable key `unpoolDecl` hands a DU-typed consumer for each bound variable. Per
            /// BUILDER, not per unpool: two unpools of one subtree are two views of the same
            /// bound variables and must key them alike.
            UnpooledBoundVarKeys: Dictionary<BoundVarId, NodeKey>
            mutable UnpoolCount: int
            /// The base pool's `ModuleMembers` by bound variable, indexed on first read.
            ModuleMemberIndex: Lazy<IReadOnlyDictionary<BoundVarId, ModuleBindingInfo>>
            /// The base pool's `LocalSchemes` by bound variable, indexed on first read.
            LocalSchemeIndex: Lazy<IReadOnlyDictionary<BoundVarId, LocalScheme>>
        }

/// A node HANDLE: a dense pool id together with the pool that resolves it. Equality is the
/// pool's identity plus the id, so two pools that both number from 0 cannot be confused.
[<Struct; NoComparison>]
type Handle<'Id> = { Pool: PoolBuilder; Id: 'Id }

[<RequireQualifiedAccess>]
module TastPoolBuilder =

    let openOver (pools: FrozenPools) : PoolBuilder =
        {
            Base = pools
            ExprBase = pools.ExprPayloads.Length
            PatBase = pools.PatPayloads.Length
            DeclBase = pools.DeclPayloads.Length
            BoundVarBase = pools.BoundVarNames.Length
            OvExprs = ResizeArray()
            OvPats = ResizeArray()
            OvDecls = ResizeArray()
            OvBoundVarCount = 0
            OvMutableBoundVars = HashSet()
            UnpooledBoundVarKeys = Dictionary()
            UnpoolCount = 0
            ModuleMemberIndex = lazy (DenseTable.index pools.ModuleMembers)
            LocalSchemeIndex = lazy (DenseTable.index pools.LocalSchemes)
        }

    /// A builder over no base at all, for nodes belonging to no frozen tree: an EXTERNAL
    /// symbol's `ValRepr` patterns are minted from a contract and index into no file.
    let openEmpty () : PoolBuilder = openOver FrozenPools.empty

    // ── the stacked read surface: one accessor per column ───────────────────
    // The layer arithmetic lives in the four `read*` resolvers below and nowhere else.

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

    /// The bound-variable resolver: a base-pool id reads the base column, an overlay-minted
    /// one has no row.
    let inline private readBoundVar
        (b: PoolBuilder)
        (id: BoundVarId)
        ([<InlineIfLambda>] ofBase: FrozenPools -> BoundVarId -> 'a)
        ([<InlineIfLambda>] ofMinted: BoundVarId -> 'a)
        : 'a =
        let (BoundVarId i) = id
        if i < b.BoundVarBase then ofBase b.Base id else ofMinted id

    /// The node's type. The BASE column holds a row id of the base pool's type table; an
    /// OVERLAY row holds the type itself, a retyped node minting types the base never had.
    let exprTy (b: PoolBuilder) (id: ExprPoolId) : FrozenType =
        readExpr b id (fun p i -> p.Types.[p.ExprTys.[i]]) (fun r -> r.Ty)

    /// Where the node SITS: the index of the token that spells it, or `Anchor.nowhere` where
    /// no source does (an overlay-minted node, a contract's rebuilt pattern).
    let exprTok (b: PoolBuilder) (id: ExprPoolId) : Anchor =
        readExpr b id (fun p i -> p.ExprToks.[i]) (fun r -> r.Tok)

    // The child edges, by POSITION and as a whole list. The array forms cut a fresh row out
    // of the flat base column, so a consumer wanting ONE named child uses `exprChild`.

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

    let exprVarBoundVar (b: PoolBuilder) (id: ExprPoolId) : BoundVarId voption =
        readExpr b id (fun p i -> p.ExprVarBoundVar.[i]) (fun r -> r.VarBoundVar)

    let exprPayload (b: PoolBuilder) (id: ExprPoolId) : ExprPayload =
        readExpr b id (fun p i -> p.ExprPayloads.[i]) (fun r -> r.Payload)

    /// The node's shape tag, projected from the payload column rather than stored.
    let exprShape (b: PoolBuilder) (id: ExprPoolId) : ExprShape = ExprPayload.shape (exprPayload b id)

    /// The whole row at `id`: the base columns gathered, or the overlay row as stored.
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
                    VarBoundVar = p.ExprVarBoundVar.[i]
                    Payload = p.ExprPayloads.[i]
                }
            )
            (fun r -> r)

    let patTy (b: PoolBuilder) (id: PatPoolId) : FrozenType =
        readPat b id (fun p i -> p.Types.[p.PatTys.[i]]) (fun r -> r.Ty)

    let patTok (b: PoolBuilder) (id: PatPoolId) : Anchor =
        readPat b id (fun p i -> p.PatToks.[i]) (fun r -> r.Tok)

    let patChild (b: PoolBuilder) (id: PatPoolId) (k: int) : PatPoolId =
        readPat b id (fun p i -> ChildColumn.item p.PatChildren i k) (fun r -> r.Children.[k])

    let patChildren (b: PoolBuilder) (id: PatPoolId) : PatPoolId[] =
        readPat b id (fun p i -> ChildColumn.slice p.PatChildren i) (fun r -> r.Children)

    let patPayload (b: PoolBuilder) (id: PatPoolId) : PatPayload =
        readPat b id (fun p i -> p.PatPayloads.[i]) (fun r -> r.Payload)

    let patShape (b: PoolBuilder) (id: PatPoolId) : PatShape = PatPayload.shape (patPayload b id)

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

    let declShape (b: PoolBuilder) (id: DeclPoolId) : DeclShape = DeclPayload.shape (declPayload b id)

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

    /// How a bound variable id is SPELLED: the base pool's naming column, or `Minted` for a bound variable
    /// this overlay handed out, which no source spells.
    let boundVarNaming (b: PoolBuilder) (id: BoundVarId) : BoundVarNaming =
        readBoundVar b id FrozenPools.boundVarNaming BoundVarNaming.Minted

    /// Where a bound variable's name is spelled. `Anchor.nowhere` where no node spells it: an
    /// overlay-minted bound variable, or a declaration's pattern-less key slot.
    let boundVarTok (b: PoolBuilder) (id: BoundVarId) : Anchor =
        readBoundVar b id (fun p (BoundVarId i) -> p.BoundVarToks.[i]) (fun _ -> Anchor.nowhere)

    /// Whether the bound variable is bound by a `let mutable`.
    let boundVarIsMutable (b: PoolBuilder) (id: BoundVarId) : bool =
        readBoundVar b id FrozenPools.boundVarIsMutable b.OvMutableBoundVars.Contains

    // The size of the expr and bound variable id spaces: the next append takes the count itself.

    let exprCount (b: PoolBuilder) : int = b.ExprBase + b.OvExprs.Count
    let boundVarCount (b: PoolBuilder) : int = b.BoundVarBase + b.OvBoundVarCount

    /// The file's decl roots, in source order. They are the BASE pool's: a whole-decl rewrite
    /// returns the derived id for its caller to carry rather than repointing this array.
    let roots (b: PoolBuilder) : EqArray<DeclPoolId> = b.Base.Roots

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

        match row.Payload with
        | PatPayload.NamedSimple(boundVar, true) -> b.OvMutableBoundVars.Add boundVar |> ignore
        | _ -> ()

        PatPoolId id

    let appendDecl (b: PoolBuilder) (row: DeclRow) : DeclPoolId =
        let id = b.DeclBase + b.OvDecls.Count
        b.OvDecls.Add row
        DeclPoolId id

    /// A bound variable id belonging to NO node of the base pool: the definition site a lowering
    /// introduces. It takes no argument: a bound variable's identity IS its slot, nothing to intern.
    let mintBoundVar (b: PoolBuilder) : BoundVarId =
        let id = BoundVarId(b.BoundVarBase + b.OvBoundVarCount)
        b.OvBoundVarCount <- b.OvBoundVarCount + 1
        id

    // ── row copies: rewrite without a per-case match ────────────────────────
    // A child substitution or a retype is a row copy with one field replaced, every untouched
    // child edge carried across as the id it was.

    /// Append a copy of row `id` with `edit` applied, returning the ORIGINAL id when the edit
    /// changed nothing, so a rewrite that touches nothing appends nothing, and every cached
    /// id still points at the same node.
    let copyExprWith (b: PoolBuilder) (id: ExprPoolId) (edit: ExprRow -> ExprRow) : ExprPoolId =
        let row = exprRow b id
        let row' = edit row
        if ExprRow.same row' row then id else appendExpr b row'

    /// Append a copy of decl row `id` with `edit` applied; see `copyExprWith`.
    let copyDeclWith (b: PoolBuilder) (id: DeclPoolId) (edit: DeclRow -> DeclRow) : DeclPoolId =
        let row = declRow b id
        let row' = edit row
        if DeclRow.same row' row then id else appendDecl b row'

    // The same row copies WITHOUT the unchanged-row shortcut: an inline body is copied once
    // per call site precisely so the copies are different nodes, which the shortcut would
    // collapse onto one id.

    /// Append a FRESH copy of expr row `id` with `edit` applied, however little moved.
    let copyExprFresh (b: PoolBuilder) (id: ExprPoolId) (edit: ExprRow -> ExprRow) : ExprPoolId =
        appendExpr b (edit (exprRow b id))

    /// The pattern twin of `copyExprFresh`.
    let copyPatFresh (b: PoolBuilder) (id: PatPoolId) (edit: PatRow -> PatRow) : PatPoolId =
        appendPat b (edit (patRow b id))

    /// The file this pool's `Anchor`s index: the base's, because an overlay derives nodes onto
    /// the very file it was opened over.
    let path (b: PoolBuilder) : AssemblyFilePath = b.Base.Path

    /// This file's OWN intrinsic-repr type declarations. A backend reads it to tell a
    /// declaration of a PLATFORM REPRESENTATION, which already exists on the target, from one
    /// it must emit.
    let intrinsicBindings (b: PoolBuilder) : EqDict<TypeKey, IntrinsicBindingInfo> = b.Base.Residue.IntrinsicBindings

    /// This file's OWN `[<Global>]` bindings. The target already owns the thing declared, so
    /// a backend emits nothing for one.
    let globalValueKeys (b: PoolBuilder) : EqSet<SymbolKey> = b.Base.Residue.GlobalValueKeys

    /// This file's module-level bindings by bound variable: the SYMBOL identity behind a `let` decl's
    /// name, which the columns address only positionally. Indexed once per builder.
    let moduleMembers (b: PoolBuilder) : IReadOnlyDictionary<BoundVarId, ModuleBindingInfo> =
        b.ModuleMemberIndex.Force()

    /// The module-level binding a root `let` decl's bound variable identifies. Every named root
    /// binding has an entry; a miss is a decl pruned without its entry.
    let moduleMemberOf (b: PoolBuilder) (boundVar: BoundVarId) : ModuleBindingInfo =
        match (moduleMembers b).TryGetValue boundVar with
        | true, info -> info
        | _ ->
            failwithf
                "TastPoolBuilder.moduleMemberOf: root binding %O has no ModuleMembers entry; Elaborate records one for every named root binding, so prune the decl where its entry is pruned"
                boundVar

    /// The bound on every `SpecializationId` an edge can carry.
    let specializationCount (b: PoolBuilder) : int = b.Base.Specializations.Length

    /// The resolved-specialization entry a `SpecializationId` identifies, bounds-checked. The
    /// entries are the BASE pool's: an overlay derives nodes, never table entries.
    let specialization (b: PoolBuilder) (spec: SpecializationId) : PooledSpecialization =
        let (SpecializationId i) = spec

        if i < 0 || i >= b.Base.Specializations.Length then
            failwithf "TastPoolBuilder: specialization %d is out of range (%d entries)" i b.Base.Specializations.Length

        b.Base.Specializations.[i]

    /// Copy the pattern subtree at `id` into `dest`, mapping every type it carries through
    /// `fTy`: the node types and the types a payload embeds. `dest` may be a pool other
    /// than `b`.
    let rec copyPatTreeInto
        (dest: PoolBuilder)
        (fTy: FrozenType -> FrozenType)
        (b: PoolBuilder)
        (id: PatPoolId)
        : PatPoolId =
        let row = patRow b id
        let kids = row.Children |> Array.map (copyPatTreeInto dest fTy b)

        // A bound variable id means nothing outside the pool that issued it, so a `NamedSimple` copy
        // introduces a bound variable of DEST's own.
        let payload =
            match PatPayload.mapTys fTy row.Payload with
            | PatPayload.NamedSimple(_, isMutable) -> PatPayload.NamedSimple(mintBoundVar dest, isMutable)
            | p -> p

        appendPat
            dest
            { row with
                Ty = fTy row.Ty
                Children = kids
                Payload = payload
            }

    // ── the DU bridge, both directions ──────────────────────────────────────

    /// Fill in the `Var` reference edge of a row the pooling walk just appended. Sound only
    /// there: a base id would index before the overlay's start and fault.
    let private setVarBoundVar (b: PoolBuilder) (ExprPoolId i) (boundVar: BoundVarId) : unit =
        let j = i - b.ExprBase

        b.OvExprs.[j] <-
            { b.OvExprs.[j] with
                VarBoundVar = ValueSome boundVar
            }

    /// The overlay's pooling sink: the same walk that built the base pool, differing only in
    /// where a row lands and how a bound variable id is assigned.
    let private sinkOf (b: PoolBuilder) : TastPools.IPoolSink<Anchor, BoundVarId> =
        { new TastPools.IPoolSink<Anchor, BoundVarId> with
            // The tree being poured in is already in this pool's identity space, so a
            // definition site IS its id and needs no lookup.
            member _.InternBoundVar boundVar = BoundVarKey.identity boundVar
            // Likewise already in the stored form: the tree identifies positions by the very index
            // the columns hold.
            member _.Anchor anchor = anchor
            member _.AddExpr row = appendExpr b row
            member _.AddPat row = appendPat b row
            member _.AddDecl row = appendDecl b row

            member _.OnExprPooled ev =
                match ev with
                | TastPools.PooledEvent.VarRef(boundVar, id) -> setVarBoundVar b id boundVar
                // `FunVerdicts` is the frozen file's own table and a minted lambda has no
                // verdict in it.
                | TastPools.PooledEvent.LambdaPooled _ -> ()
        }

    /// Pool a freshly minted DU subtree into the overlay: the bridge for a mint site that
    /// still CONSTRUCTS `Pooled.TExpr` values.
    let appendExprTree (b: PoolBuilder) (e: Pooled.TExpr) : ExprPoolId = TastPools.poolExpr (sinkOf b) e

    /// The DU subtree a pattern id denotes, resolved across BOTH layers. This direction is
    /// for the one channel whose far end is still DU-typed: an inline template crossing the
    /// PACKAGE wire, where a pool id would mean nothing.
    let rec private patTree (rename: BoundVarId -> NodeKey) (b: PoolBuilder) (at: PatPoolId) : Wire.TPat =
        let row = patRow b at

        TastUnpool.substitutePat rename row.Ty row.Tok row.Payload (row.Children |> Array.map (patTree rename b))

    /// The DU subtree an expression id denotes. Reached through `unpoolDecl`: the cross-file
    /// wire carries whole declarations, never a bare expression.
    let rec private exprTree (rename: BoundVarId -> NodeKey) (b: PoolBuilder) (at: ExprPoolId) : Wire.TExpr =
        let row = exprRow b at

        TastUnpool.substituteExpr
            rename
            row.Ty
            row.Tok
            row.VarBoundVar
            row.Payload
            (row.Children |> Array.map (exprTree rename b))
            (row.PatChildren |> Array.map (patTree rename b))

    /// The DU subtree a declaration id denotes, with the generalised body-locals it declares.
    /// Bound variables are RE-MINTED, not lent: a slot means nothing outside the pool that
    /// issued it, and a DU consumer needs only distinctness plus equality between a bound
    /// variable and its references.
    let unpoolDecl (b: PoolBuilder) (at: DeclPoolId) : Wire.UnpooledDecl =
        let schemes = b.LocalSchemeIndex.Force()
        let locals = ResizeArray<BoundVarKey * LocalScheme>()

        let rename (boundVar: BoundVarId) : NodeKey =
            match b.UnpooledBoundVarKeys.TryGetValue boundVar with
            | true, k -> k
            | false, _ ->
                b.UnpoolCount <- b.UnpoolCount + 1
                let k = NodeKey.ofSyntheticCounter b.UnpoolCount NodeKind.SynthUnpooledBoundVar
                b.UnpooledBoundVarKeys.[boundVar] <- k

                match schemes.TryGetValue boundVar with
                | true, scheme -> locals.Add(BoundVarKey.ofPatKey k, scheme)
                | _ -> ()

                k

        let row = declRow b at

        // `rename` fills `locals` as it goes, so the table is complete only once the tree is.
        let tree =
            TastUnpool.substituteDecl
                rename
                (exprTree rename b)
                row.Payload
                (row.ExprChildren |> Array.map (exprTree rename b))
                (row.PatChildren |> Array.map (patTree rename b))

        {
            Decl = tree
            LocalSchemes = Map.ofSeq locals
        }
