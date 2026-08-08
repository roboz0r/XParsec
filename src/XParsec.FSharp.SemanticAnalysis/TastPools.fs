namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

[<RequireQualifiedAccess>]
module TastPools =

    open TastPoolShapes

    /// A fact a row cannot carry, handed to the sink as the walk appends the node it
    /// belongs to.
    [<RequireQualifiedAccess>]
    type PooledEvent<'id> =
        /// A `Var`'s reference edge, which the walk cannot fill: binder ids are the sink's
        /// to assign, and a `Var` may name a binder the walk has not reached.
        | VarRef of binder: 'id * at: ExprPoolId
        /// A pooled lambda's id, with the anchor its `LambdaKey` is minted from.
        | LambdaPooled of anchor: Anchor * at: ExprPoolId

    /// Where a pooling walk PUTS its rows. Generic in how the walked tree names a binder
    /// (`'id`: `NodeKey` from source, `BinderId` from an already-pooled tree) and in how it
    /// spells a position (`'tok`).
    type PoolSink<'tok, 'id> =
        {
            /// Called for every binder a walked node INTRODUCES (a `NamedSimple` pattern's
            /// binding, a `ForTo` loop variable), before that node's row is added.
            /// Idempotent in the binder.
            InternBinder: BinderKeyG<'id> -> BinderId
            /// How the walked tree's spelling of a position becomes the stored anchor. A
            /// property of the DESTINATION: a node of a frozen FILE must anchor on a real
            /// lexed token, while an overlay's rows belong to no file.
            Anchor: 'tok -> Anchor
            AddExpr: ExprRow -> ExprPoolId
            AddPat: PatRow -> PatPoolId
            AddDecl: DeclRow -> DeclPoolId
            /// Called after a node's row is added, only for the nodes that carry one.
            OnExprPooled: PooledEvent<'id> -> unit
        }

    /// Pool a pattern subtree post-order: a node's children are pooled before the node
    /// itself, so every child id its row names already resolves.
    let rec poolPat (sink: PoolSink<'tok, 'id>) (p: TPatG<FrozenType, 'tok, 'id>) : PatPoolId =
        // Interned first: the payload names this binder by the id the intern hands back.
        let binder = BinderKey.ofPat p |> ValueOption.map sink.InternBinder
        let kids = patChildren p |> Array.map (poolPat sink)

        sink.AddPat
            {
                Ty = TastWalk.patTy p
                Tok = sink.Anchor(TastWalk.patTok p)
                Children = kids
                Payload = patPayload binder p
            }

    /// Pool an expression subtree post-order (see `poolPat`), its owned sub-patterns
    /// included.
    let rec poolExpr (sink: PoolSink<'tok, 'id>) (e: TExprG<FrozenType, 'tok, 'id>) : ExprPoolId =
        // A `ForTo` binds its loop variable with no pattern node behind it, so the intern
        // cannot ride `poolPat`.
        let binder = BinderKey.ofExpr e |> ValueOption.map sink.InternBinder
        let exprKids = exprChildren e |> Array.map (poolExpr sink)
        let patKids = exprPatChildren e |> Array.map (poolPat sink)

        let row =
            {
                Ty = TastWalk.exprTy e
                Tok = sink.Anchor(TastWalk.exprTok e)
                Children = exprKids
                PatChildren = patKids
                VarBinder = ValueNone
                Payload = exprPayload sink.Anchor binder e
            }

        let id = sink.AddExpr row

        match e with
        | TExprG.Var(binding = binding) -> sink.OnExprPooled(PooledEvent.VarRef(binding, id))
        | TExprG.Lambda _ -> sink.OnExprPooled(PooledEvent.LambdaPooled(row.Tok, id))
        | _ -> ()

        id

    /// A frozen declaration's fields MINUS its child expr/pat roots. A `Type` decl's
    /// member/preamble/ctor bodies are pooled through the sink and their IDS kept in the
    /// slots that held the trees.
    let private declPayload (sink: PoolSink<'tok, 'id>) (d: TDeclG<FrozenType, 'tok, 'id>) : DeclPayload =
        match d with
        | TDeclG.Let(isInline = isInline; ty = ty) -> DeclPayload.Let {| IsInline = isInline; Ty = ty |}
        | TDeclG.Expression(ty = ty) -> DeclPayload.Expression ty
        | TDeclG.Type td ->
            DeclPayload.Type(
                TastConvert.typeDecl
                    {
                        Ty = id
                        Tok = sink.Anchor
                        Id = sink.InternBinder
                        Body = poolExpr sink
                    }
                    td
            )

    /// Pool a declaration, its expr/pat roots (see `poolPat`) and — for a `Type` decl —
    /// its member bodies, which the payload names by id rather than surfacing as children.
    let poolDecl (sink: PoolSink<'tok, 'id>) (d: TDeclG<FrozenType, 'tok, 'id>) : DeclPoolId =
        let struct (exprKids, patKids) =
            match d with
            | TDeclG.Let(binding = binding; value = value) ->
                struct ([| poolExpr sink value |], [| poolPat sink binding |])
            | TDeclG.Expression(expr = expr) -> struct ([| poolExpr sink expr |], [||])
            | TDeclG.Type _ -> struct ([||], [||])

        sink.AddDecl
            {
                ExprChildren = exprKids
                PatChildren = patKids
                Payload = declPayload sink d
            }

    /// The SOURCE arity of every module binding, read off the columns just filled — so a
    /// tuple group's pattern IS the lambda parameter node it was peeled from, not a copy.
    let private bindingValReprs (pools: FrozenPools) : DenseTable<BinderId, PooledValRepr> =
        let unLambda (ExprPoolId i) =
            match pools.ExprPayloads.[i] with
            | ExprPayload.Lambda ->
                ValueSome(struct (ChildColumn.item pools.ExprPatChildren i 0, ChildColumn.item pools.ExprChildren i 0))
            | _ -> ValueNone

        let facts (PatPoolId i) : ArgGroups.ParamPatFacts<BinderId> =
            {
                Shape = PatPayload.shape pools.PatPayloads.[i]
                Ty = pools.Types.[pools.PatTys.[i]]
                Binder =
                    match pools.PatPayloads.[i] with
                    | PatPayload.NamedSimple binder -> ValueSome binder
                    | _ -> ValueNone
                ConstValue =
                    match pools.PatPayloads.[i] with
                    | PatPayload.Const value -> ValueSome value
                    | _ -> ValueNone
            }

        [|
            for DeclPoolId d in pools.Roots do
                match pools.DeclPayloads.[d] with
                | DeclPayload.Let _ ->
                    let (PatPoolId pattern) = ChildColumn.item pools.DeclPatChildren d 0

                    match pools.PatPayloads.[pattern] with
                    // Only a simple binder has a side-table identity.
                    | PatPayload.NamedSimple binder ->
                        let groups, body =
                            ArgGroups.peel unLambda facts (ChildColumn.item pools.DeclExprChildren d 0)

                        let (ExprPoolId b) = body

                        yield
                            binder,
                            {
                                // A plain value has no lambda groups: an empty-`Groups` entry.
                                Typars = FrozenPools.typarArity pools binder
                                Groups = groups
                                ResultTy = pools.Types.[pools.ExprTys.[b]]
                            }
                    | _ -> ()
                | DeclPayload.Expression _
                | DeclPayload.Type _ -> ()
        |]

    /// Pool a tree, assigning each reachable node a dense id and recording its child edges
    /// as ids. `spellingOf` fills the two binder columns, `anchor` narrows the tree's
    /// spelling of a position to the stored form, `origin` is the file those indices index.
    let private fill
        (origin: OriginFile)
        (spellingOf: BinderKeyG<'id> -> BinderSpelling)
        (anchor: 'tok -> Anchor)
        (file: TastFileG<FrozenType, 'tok, 'id>)
        : FrozenPools =
        // A node's `ty` column holds a row index into this table.
        let typeTable = FrozenTypeTableBuilder()

        // The expression pool, index-aligned by `ExprPoolId`: one append per node, per column.
        let exprTys = ResizeArray<TypeId>()
        let exprToks = ResizeArray<Anchor>()
        let exprChildrenCol = ChildColumnBuilder<ExprPoolId>()
        let exprPatChildrenCol = ChildColumnBuilder<PatPoolId>()
        let exprPayloads = ResizeArray<ExprPayload>()

        // Each `Var`'s expr id + the binder it references. A `Var` may name a binder pooled
        // after it (a forward / mutually-recursive reference), so the enumeration must
        // complete before the edge can be resolved.
        let varBindings = ResizeArray<struct (int * 'id)>()

        // The pattern pool, index-aligned by `PatPoolId`.
        let patTys = ResizeArray<TypeId>()
        let patToks = ResizeArray<Anchor>()
        let patChildrenCol = ChildColumnBuilder<PatPoolId>()
        let patPayloads = ResizeArray<PatPayload>()

        // The declaration pool, index-aligned by `DeclPoolId`.
        let declExprChildrenCol = ChildColumnBuilder<ExprPoolId>()
        let declPatChildrenCol = ChildColumnBuilder<PatPoolId>()
        let declPayloads = ResizeArray<DeclPayload>()

        // The binder pool: each definition site the walk reaches takes a dense `BinderId` on
        // first encounter, its two columns recording the source spelling and where. A
        // `BinderId` may have no pooled pattern (a `this` slot has none).
        let binderNames = ResizeArray<string>()
        let binderToks = ResizeArray<Anchor>()
        let binderIds = System.Collections.Generic.Dictionary<'id, BinderId>()

        // A pooled lambda's `ExprPoolId` paired with the key its `FunVerdicts` entry is filed
        // under. A LIST, not a key→id map: an inline template and the function it was stashed
        // from share a token, so one key stamps several lambdas.
        let lambdaSlots = ResizeArray<struct (ExprPoolId * LambdaKey)>()

        let internBinder (binder: BinderKeyG<'id>) : BinderId =
            let k = BinderKey.identity binder

            match binderIds.TryGetValue k with
            | true, id -> id
            | false, _ ->
                let id = BinderId binderNames.Count
                binderIds.Add(k, id)
                let sp = spellingOf binder
                binderNames.Add sp.Name
                binderToks.Add sp.At
                id

        // Rows land at the end of the column builders, so a node's id is the count at the
        // moment it is added. `ExprRow.VarBinder` is dropped here and filled by the second
        // pass below.
        let sink: PoolSink<'tok, 'id> =
            {
                InternBinder = internBinder
                Anchor = anchor
                AddExpr =
                    fun row ->
                        let id = exprPayloads.Count
                        exprTys.Add(typeTable.Intern row.Ty)
                        exprToks.Add row.Tok
                        exprChildrenCol.Add row.Children
                        exprPatChildrenCol.Add row.PatChildren
                        exprPayloads.Add row.Payload
                        ExprPoolId id
                AddPat =
                    fun row ->
                        let id = patPayloads.Count
                        patTys.Add(typeTable.Intern row.Ty)
                        patToks.Add row.Tok
                        patChildrenCol.Add row.Children
                        patPayloads.Add row.Payload
                        PatPoolId id
                AddDecl =
                    fun row ->
                        let id = declPayloads.Count
                        declExprChildrenCol.Add row.ExprChildren
                        declPatChildrenCol.Add row.PatChildren
                        declPayloads.Add row.Payload
                        DeclPoolId id
                OnExprPooled =
                    fun ev ->
                        match ev with
                        | PooledEvent.VarRef(binder, ExprPoolId id) -> varBindings.Add(struct (id, binder))
                        | PooledEvent.LambdaPooled(anchor, id) -> lambdaSlots.Add(struct (id, LambdaKey anchor))
            }

        let roots = file.Decls |> EqArray.toArray |> Array.map (poolDecl sink)

        // The inline vocabulary, pooled as its OWN roots: a template is a different tree from
        // the emitted function of the same name. Ordinary pooled decls, so the walk reaches a
        // template's binders too.
        let inlineTemplates =
            file.InlineBodies
            |> EqArray.toArray
            |> Array.map (fun iv ->
                {
                    Key = iv.Key
                    Decl = poolDecl sink iv.Body.Decl
                    ParamAttrs = iv.Body.ParamAttrs
                }
            )

        // Pooled as its own roots and in SLOT ORDER: a `SpecializationId` names a position in
        // this array, not in the pool the decls land in.
        let specializations =
            file.Specializations
            |> EqArray.toArray
            |> Array.map (fun s ->
                {
                    Key = s.Key
                    Origin = s.Origin
                    Decl = poolDecl sink s.Decl
                }
            )

        let internedBinderId (k: 'id) : BinderId voption =
            match binderIds.TryGetValue k with
            | true, id -> ValueSome id
            | false, _ -> ValueNone

        let binderIdOfRef (referent: string) (k: 'id) : BinderId =
            match internedBinderId k with
            | ValueSome id -> id
            | ValueNone ->
                failwithf "TastPools.toPools: %s key %O references a binder no definition site introduced" referent k

        let binderIdOf (referent: string) (b: BinderKeyG<'id>) : BinderId =
            let k = BinderKey.identity b

            match internedBinderId k with
            | ValueSome id -> id
            | ValueNone ->
                failwithf
                    "TastPools.toPools: %s entry %O names a binder no declaration in the frozen file introduces — prune the entry where its declaration is pruned"
                    referent
                    k

        // `FunVerdicts` onto the lambda id space, driven from the ID side: every pooled lambda
        // is offered the key it was stamped with, so all the lambdas one key stamps take the
        // verdict.
        let funVerdicts =
            let matched = System.Collections.Generic.HashSet<LambdaKey>()

            let rows =
                [|
                    for struct (id, k) in lambdaSlots do
                        match Map.tryFind k file.FunVerdicts with
                        | Some v ->
                            matched.Add k |> ignore
                            yield id, v
                        | None -> ()
                |]

            for KeyValue(k, _) in file.FunVerdicts do
                if not (matched.Contains k) then
                    failwithf "TastPools.toPools: FunVerdicts key %O names no pooled lambda" k

            rows

        // Second pass, the enumeration now complete: route each `Var`'s reference edge to its
        // binder's dense id. `ValueNone` at every non-`Var` slot.
        let exprVarBinder: BinderId voption[] = Array.create exprPayloads.Count ValueNone

        for (struct (id, key)) in varBindings do
            exprVarBinder.[id] <- ValueSome(binderIdOfRef "Var" key)

        // The resolver is a parameter so that a fault names the table holding the key.
        let remapSideTable (resolve: 'k -> 'dense) (m: Map<'k, 'v>) : ('dense * 'v)[] =
            m |> Map.toArray |> Array.map (fun (k, v) -> resolve k, v)

        // A per-binder SCALAR goes into a column instead: the producer's key is resolved here
        // and then DROPPED, the fact landing at the binder's own slot.
        let binderColumn (referent: string) (m: Map<BinderKeyG<'id>, 'v>) : BinderColumn<'v> =
            let col = Array.create binderNames.Count ValueNone

            for KeyValue(k, v) in m do
                let (BinderId i) = binderIdOf referent k
                col.[i] <- ValueSome v

            col

        // Every column is snapshotted here; the derived table below only READS the pools.
        let pools =
            {
                Origin = origin
                Types = FrozenTypeTable.OfRows typeTable.Rows
                ExprTys = exprTys.ToArray()
                ExprToks = exprToks.ToArray()
                ExprChildren = exprChildrenCol.ToColumn()
                ExprPatChildren = exprPatChildrenCol.ToColumn()
                ExprVarBinder = exprVarBinder
                ExprPayloads = exprPayloads.ToArray()
                PatTys = patTys.ToArray()
                PatToks = patToks.ToArray()
                PatChildren = patChildrenCol.ToColumn()
                PatPayloads = patPayloads.ToArray()
                DeclExprChildren = declExprChildrenCol.ToColumn()
                DeclPatChildren = declPatChildrenCol.ToColumn()
                DeclPayloads = declPayloads.ToArray()
                Roots = roots
                InlineTemplates = inlineTemplates
                Specializations = specializations
                BinderNames = binderNames.ToArray()
                BinderToks = binderToks.ToArray()
                Residue =
                    {
                        Diagnostics = file.Diagnostics
                        IntrinsicReprKeys = file.IntrinsicReprKeys
                        GlobalValueKeys = file.GlobalValueKeys
                        Accessibility = file.Accessibility
                    }
                ModuleMembers = remapSideTable (binderIdOf "ModuleMembers") file.ModuleMembers
                ClosureReprs = remapSideTable (binderIdOf "ClosureReprs") file.ClosureReprs
                FunVerdicts = funVerdicts
                GenericFnSchemes = remapSideTable (binderIdOf "GenericFnSchemes") file.GenericFnSchemes
                // Derived below, off the pools themselves.
                BindingValReprs = [||]
                BindingTyparArities = binderColumn "BindingTyparArities" file.BindingTyparArities
            }

        { pools with
            BindingValReprs = bindingValReprs pools
        }

    /// Pool a source-shaped frozen file: its tokens become indices against `origin`, and
    /// `spellings` records how the source writes each binder's name, verbatim.
    let toPools (origin: OriginFile) (spellings: BinderKey -> BinderSpelling) (file: Frozen.TastFile) : FrozenPools =
        fill origin spellings Anchor.ofToken file

    /// Pool a tree that was UNPOOLED from `pools`: the spelling and the origin come back off
    /// the pool it came out of, and its anchors are already in the stored form.
    let rePool (pools: FrozenPools) (file: Pooled.TastFile) : FrozenPools =
        let spellingOf (b: BinderKeyG<BinderId>) =
            let (BinderId i) = BinderKey.identity b

            {
                Name = pools.BinderNames.[i]
                At = pools.BinderToks.[i]
            }

        fill pools.Origin spellingOf id file
