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
        /// A `Var`'s reference edge, which the walk cannot fill: bound variable ids are the sink's
        /// to assign, and a `Var` may reference a bound variable the walk has not reached.
        | VarRef of boundVar: 'id * at: ExprPoolId
        /// A pooled lambda's id, with the anchor its `LambdaKey` is minted from.
        | LambdaPooled of anchor: Anchor * at: ExprPoolId

    /// Where a pooling walk PUTS its rows. Generic in how the walked tree identifies a bound variable
    /// (`'id`: `NodeKey` from source, `BoundVarId` from an already-pooled tree) and in how it
    /// spells a position (`'tok`).
    type IPoolSink<'tok, 'id> =
        /// Called for every bound variable a walked node INTRODUCES (a `NamedSimple` pattern's
        /// binding, a `ForTo` loop variable), before that node's row is added.
        /// Idempotent in the bound variable.
        abstract InternBoundVar: BoundVarKeyG<'id> -> BoundVarId
        /// How the walked tree's spelling of a position becomes the stored anchor. A
        /// property of the DESTINATION: a node of a frozen FILE must anchor on a real
        /// lexed token, while an overlay's rows belong to no file.
        abstract Anchor: 'tok -> Anchor
        abstract AddExpr: ExprRow -> ExprPoolId
        abstract AddPat: PatRow -> PatPoolId
        abstract AddDecl: DeclRow -> DeclPoolId
        /// Called after a node's row is added, only for the nodes that carry one.
        abstract OnExprPooled: PooledEvent<'id> -> unit

    /// Pool a pattern subtree post-order: a node's children are pooled before the node
    /// itself, so every child id its row carries already resolves.
    let rec poolPat (sink: IPoolSink<'tok, 'id>) (p: TPatG<FrozenType, 'tok, 'id>) : PatPoolId =
        // Interned first: the payload identifies this bound variable by the id the intern hands back.
        let boundVar =
            BoundVarKey.ofPat p |> ValueOption.map (fun k -> sink.InternBoundVar k)

        let kids = patChildren p |> Array.map (poolPat sink)

        sink.AddPat
            {
                Ty = TastWalk.patTy p
                Tok = sink.Anchor(TastWalk.patTok p)
                Children = kids
                Payload = patPayload boundVar p
            }

    /// Pool an expression subtree post-order (see `poolPat`), its owned sub-patterns
    /// included.
    let rec poolExpr (sink: IPoolSink<'tok, 'id>) (e: TExprG<FrozenType, 'tok, 'id>) : ExprPoolId =
        // A `ForTo` binds its loop variable with no pattern node behind it, so the intern
        // happens here rather than in `poolPat`.
        let boundVar =
            BoundVarKey.ofExpr e |> ValueOption.map (fun k -> sink.InternBoundVar k)

        let exprKids = exprChildren e |> Array.map (poolExpr sink)
        let patKids = exprPatChildren e |> Array.map (poolPat sink)

        let row =
            {
                Ty = TastWalk.exprTy e
                Tok = sink.Anchor(TastWalk.exprTok e)
                Children = exprKids
                PatChildren = patKids
                VarBoundVar = ValueNone
                Payload = exprPayload (fun t -> sink.Anchor t) boundVar e
            }

        let id = sink.AddExpr row

        match e with
        | TExprG.Var(boundVar = boundVar) -> sink.OnExprPooled(PooledEvent.VarRef(boundVar, id))
        | TExprG.Lambda _ -> sink.OnExprPooled(PooledEvent.LambdaPooled(row.Tok, id))
        | _ -> ()

        id

    /// A frozen declaration's fields MINUS its child expr/pat roots. A `Type` decl's
    /// member/preamble/ctor bodies are pooled through the sink and their IDS kept in the
    /// slots that held the trees.
    let private declPayload (sink: IPoolSink<'tok, 'id>) (d: TDeclG<FrozenType, 'tok, 'id>) : DeclPayload =
        match d with
        | TDeclG.Let(isInline = isInline; ty = ty) -> DeclPayload.Let {| IsInline = isInline; Ty = ty |}
        | TDeclG.Expression(ty = ty) -> DeclPayload.Expression ty
        | TDeclG.Type td ->
            DeclPayload.Type(
                TastConvert.typeDecl
                    {
                        Ty = id
                        Tok = fun t -> sink.Anchor t
                        Id = fun k -> sink.InternBoundVar k
                        Body = poolExpr sink
                    }
                    td
            )

    /// Pool a declaration, its expr/pat roots (see `poolPat`) and, for a `Type` decl,
    /// its member bodies, which the payload references by id rather than surfacing as children.
    let poolDecl (sink: IPoolSink<'tok, 'id>) (d: TDeclG<FrozenType, 'tok, 'id>) : DeclPoolId =
        let struct (exprKids, patKids) =
            match d with
            | TDeclG.Let(pattern = pattern; value = value) ->
                struct ([| poolExpr sink value |], [| poolPat sink pattern |])
            | TDeclG.Expression(expr = expr) -> struct ([| poolExpr sink expr |], [||])
            | TDeclG.Type _ -> struct ([||], [||])

        sink.AddDecl
            {
                ExprChildren = exprKids
                PatChildren = patKids
                Payload = declPayload sink d
            }

    /// The SOURCE arity of every module binding, read off the columns just filled, so a
    /// tuple group's pattern IS the lambda parameter node it was peeled from, not a copy.
    let private bindingValReprs (pools: FrozenPools) : DenseTable<BoundVarId, PooledValRepr> =
        let unLambda (ExprPoolId i) =
            match pools.ExprPayloads.[i] with
            | ExprPayload.Lambda ->
                ValueSome(struct (ChildColumn.item pools.ExprPatChildren i 0, ChildColumn.item pools.ExprChildren i 0))
            | _ -> ValueNone

        let facts (PatPoolId i) : ArgGroups.ParamPatFacts<BoundVarId> =
            {
                Shape = PatPayload.shape pools.PatPayloads.[i]
                Ty = pools.Types.[pools.PatTys.[i]]
                BoundVar =
                    match pools.PatPayloads.[i] with
                    | PatPayload.NamedSimple boundVar -> ValueSome boundVar
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
                    // Only a simple bound variable has a side-table identity.
                    | PatPayload.NamedSimple boundVar ->
                        let groups, body =
                            ArgGroups.peel unLambda facts (ChildColumn.item pools.DeclExprChildren d 0)

                        let (ExprPoolId b) = body

                        yield
                            boundVar,
                            {
                                // A plain value has no lambda groups: an empty-`Groups` entry.
                                Typars = FrozenPools.typarArity pools boundVar
                                Groups = groups
                                ResultTy = pools.Types.[pools.ExprTys.[b]]
                            }
                    | _ -> ()
                | DeclPayload.Expression _
                | DeclPayload.Type _ -> ()
        |]

    /// Pool a tree, assigning each reachable node a dense id and recording its child edges
    /// as ids. `identOf` fills the two bound-variable columns, `anchor` narrows the tree's
    /// spelling of a position to the stored form, `path` is the file those indices index.
    /// `entryPoint` prefixes fault messages with the public caller's name.
    let private fill
        (entryPoint: string)
        (path: AssemblyFilePath)
        (identOf: BoundVarKeyG<'id> -> BoundVarIdent)
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

        // Each `Var`'s expr id + the bound variable it references. A `Var` may reference one pooled
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

        // The bound variable pool: each definition site the walk reaches takes a dense `BoundVarId` on
        // first encounter, its two columns recording the source spelling and where. A
        // `BoundVarId` may have no pooled pattern (a `this` slot has none).
        let boundVarNames = ResizeArray<string>()
        let boundVarToks = ResizeArray<Anchor>()
        let boundVarIds = System.Collections.Generic.Dictionary<'id, BoundVarId>()

        // A pooled lambda's `ExprPoolId` paired with the key its `FunVerdicts` entry is filed
        // under. A LIST, not a key→id map: an inline template and the function it was stashed
        // from share a token, so one key stamps several lambdas.
        let lambdaSlots = ResizeArray<struct (ExprPoolId * LambdaKey)>()

        let internBoundVar (boundVar: BoundVarKeyG<'id>) : BoundVarId =
            let k = BoundVarKey.identity boundVar

            match boundVarIds.TryGetValue k with
            | true, id -> id
            | false, _ ->
                let id = BoundVarId boundVarNames.Count
                boundVarIds.Add(k, id)
                let ident = identOf boundVar
                boundVarNames.Add ident.Text
                boundVarToks.Add ident.At
                id

        // Rows land at the end of the column builders, so a node's id is the count at the
        // moment it is added. `ExprRow.VarBoundVar` is dropped here and filled by the second
        // pass below.
        let sink =
            { new IPoolSink<'tok, 'id> with
                member _.InternBoundVar boundVar = internBoundVar boundVar
                member _.Anchor tok = anchor tok

                member _.AddExpr row =
                    let id = exprPayloads.Count
                    exprTys.Add(typeTable.Intern row.Ty)
                    exprToks.Add row.Tok
                    exprChildrenCol.Add row.Children
                    exprPatChildrenCol.Add row.PatChildren
                    exprPayloads.Add row.Payload
                    ExprPoolId id

                member _.AddPat row =
                    let id = patPayloads.Count
                    patTys.Add(typeTable.Intern row.Ty)
                    patToks.Add row.Tok
                    patChildrenCol.Add row.Children
                    patPayloads.Add row.Payload
                    PatPoolId id

                member _.AddDecl row =
                    let id = declPayloads.Count
                    declExprChildrenCol.Add row.ExprChildren
                    declPatChildrenCol.Add row.PatChildren
                    declPayloads.Add row.Payload
                    DeclPoolId id

                member _.OnExprPooled ev =
                    match ev with
                    | PooledEvent.VarRef(boundVar, ExprPoolId id) -> varBindings.Add(struct (id, boundVar))
                    | PooledEvent.LambdaPooled(anchor, id) -> lambdaSlots.Add(struct (id, LambdaKey anchor))
            }

        let roots = file.Decls |> EqArray.map (poolDecl sink)

        // The inline vocabulary, pooled as its OWN roots: a template is a different tree from
        // the emitted function of the same name. Ordinary pooled decls, so the walk reaches a
        // template's bound variables too.
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

        // Pooled as its own pattern and expression roots, in SLOT ORDER: a `SpecializationId`
        // identifies a position in this array, not in the pools the nodes land in.
        let specializations =
            file.Specializations
            |> EqArray.toArray
            |> Array.map (fun s ->
                {
                    Key = s.Key
                    Path = s.Path
                    Pat = poolPat sink s.Pat
                    Value = poolExpr sink s.Value
                }
            )

        let internedBoundVarId (k: 'id) : BoundVarId voption =
            match boundVarIds.TryGetValue k with
            | true, id -> ValueSome id
            | false, _ -> ValueNone

        let boundVarIdOfRef (referent: string) (k: 'id) : BoundVarId =
            match internedBoundVarId k with
            | ValueSome id -> id
            | ValueNone ->
                failwithf
                    "%s: %s key %O references a bound variable no definition site introduced"
                    entryPoint
                    referent
                    k

        let tryBoundVarIdOf (b: BoundVarKeyG<'id>) : BoundVarId voption =
            internedBoundVarId (BoundVarKey.identity b)

        // `FunVerdicts` onto the lambda id space, driven from the ID side: every pooled lambda
        // is offered the key it was stamped with, so all the lambdas one key stamps take the
        // verdict, and a key naming no pooled lambda is projected away like a side table's.
        let funVerdicts =
            [|
                for struct (id, k) in lambdaSlots do
                    match Map.tryFind k file.FunVerdicts with
                    | Some v -> yield id, v
                    | None -> ()
            |]

        // Second pass, the enumeration now complete: route each `Var`'s reference edge to its
        // bound variable's dense id. `ValueNone` at every non-`Var` slot.
        let exprVarBoundVar: BoundVarId voption[] =
            Array.create exprPayloads.Count ValueNone

        for (struct (id, key)) in varBindings do
            exprVarBoundVar.[id] <- ValueSome(boundVarIdOfRef "Var" key)

        // A side table annotates declarations from outside the tree, so it is PROJECTED onto the
        // pooled bound variables: an entry keyed by one the pool never interned is dropped. That
        // is the shape a file whose elaboration dropped a declaration arrives in.
        let remapSideTable (resolve: 'k -> 'dense voption) (m: Map<'k, 'v>) : ('dense * 'v)[] =
            m
            |> Map.toArray
            |> Array.choose (fun (k, v) ->
                match resolve k with
                | ValueSome dense -> Some(dense, v)
                | ValueNone -> None
            )

        // A per-bound-variable SCALAR goes into a column instead: the producer's key is resolved here
        // and then DROPPED, the fact landing at the bound variable's own slot. Projected like
        // `remapSideTable`.
        let boundVarColumn (m: Map<BoundVarKeyG<'id>, 'v>) : BoundVarColumn<'v> =
            let col = Array.create boundVarNames.Count ValueNone

            for KeyValue(k, v) in m do
                match tryBoundVarIdOf k with
                | ValueSome(BoundVarId i) -> col.[i] <- ValueSome v
                | ValueNone -> ()

            col

        // Every column is snapshotted here; the derived table below only READS the pools.
        let pools =
            {
                Path = path
                Types = FrozenTypeTable.OfRows typeTable.Rows
                ExprTys = exprTys.ToArray()
                ExprToks = exprToks.ToArray()
                ExprChildren = exprChildrenCol.ToColumn()
                ExprPatChildren = exprPatChildrenCol.ToColumn()
                ExprVarBoundVar = exprVarBoundVar
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
                BoundVarNames = boundVarNames.ToArray()
                BoundVarToks = boundVarToks.ToArray()
                Residue =
                    {
                        Diagnostics = file.Diagnostics
                        IntrinsicBindings = file.IntrinsicBindings
                        GlobalValueKeys = file.GlobalValueKeys
                        CompiledModuleNames = file.CompiledModuleNames
                        AutoOpenModules = file.AutoOpenModules
                        Accessibility = file.Accessibility
                    }
                ModuleMembers = remapSideTable tryBoundVarIdOf file.ModuleMembers
                ClosureReprs = remapSideTable tryBoundVarIdOf file.ClosureReprs
                FunVerdicts = funVerdicts
                GenericFnSchemes = remapSideTable tryBoundVarIdOf file.GenericFnSchemes
                // Derived below, off the pools themselves.
                BindingValReprs = [||]
                BindingTyparArities = boundVarColumn file.BindingTyparArities
            }

        { pools with
            BindingValReprs = bindingValReprs pools
        }

    /// Pool a source-shaped frozen file: its tokens become indices against `path`, and
    /// `idents` records how the source writes each bound variable's name, verbatim.
    let toPools (path: AssemblyFilePath) (idents: BoundVarKey -> BoundVarIdent) (file: Frozen.TastFile) : FrozenPools =
        fill "TastPools.toPools" path idents Anchor.ofToken file

    /// Pool a tree that was UNPOOLED from `pools`: the names and the path come back off
    /// the pool it came out of, and its anchors are already in the stored form.
    let rePool (pools: FrozenPools) (file: Pooled.TastFile) : FrozenPools =
        let identOf (b: BoundVarKeyG<BoundVarId>) =
            let (BoundVarId i) = BoundVarKey.identity b

            {
                Text = pools.BoundVarNames.[i]
                At = pools.BoundVarToks.[i]
            }

        fill "TastPools.rePool" pools.Path identOf id file
