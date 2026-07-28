namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Parser

// The FILL direction of the id-indexable frozen pools: `toPools` walks the `Frozen.*` DU
// assigning each node a dense id and recording its child edges as ids. The drain —
// `substitute*` and `ofPools`, rebuilding the DU from the columns alone — is
// `TastUnpool.fs`; the wire-shape types are split by scope, one NODE
// (`ExprPoolId`/`ExprPayload`/`ExprRow`/…, `TastPoolNodes.fs`) versus the whole FILE
// (`FrozenPools` and its side-table containers, `TastPoolTypes.fs`).
//
// `toPools` is the LAST step of the freeze (`Freeze.run`) and its only production caller.
// The corpus-wide `TastUnpool.ofPools ∘ toPools = id` is the proof that the columns carry
// the whole tree.

[<RequireQualifiedAccess>]
module TastPools =

    // ── the DU vocabulary ───────────────────────────────────────────────────
    //
    // Payload projection and child enumeration read off a `Frozen.*` node. This is the
    // LAST place a frozen DU is walked — every consumer reads the columns — so the tree
    // shape is written down exactly once, here, and the child ORDER these produce IS
    // the order of the `ExprChildren`/`ExprPatChildren`/`PatChildren` columns. Each
    // match is exhaustive with no catch-all: a new `TExprG`/`TPatG`/`TDeclG` case
    // fails to compile rather than silently escaping the pool. A node's SHAPE tag is not
    // among them — it is `ExprPayload.shape` of the payload these produce, never a
    // second match over the DU.

    /// The immediate child *expressions*, in evaluation order. Sub-patterns are NOT
    /// children (see `exprPatChildren`); composite carriers with no node identity of
    /// their own (match arms, format segments, static-opt clauses) are descended into
    /// so every reachable sub-expression appears exactly once.
    let exprChildren (e: TExprG<FrozenType, SyntaxToken, 'id>) : TExprG<FrozenType, SyntaxToken, 'id>[] =
        let acc = ResizeArray<TExprG<FrozenType, SyntaxToken, 'id>>()

        match e with
        | TExprG.Const _
        | TExprG.Var _
        | TExprG.External _
        | TExprG.Null _
        | TExprG.StaticPropertyGet _
        | TExprG.StaticFieldGet _ -> ()
        | TExprG.Lambda(body = body) -> acc.Add body
        | TExprG.App(fn = fn; arg = arg) ->
            acc.Add fn
            acc.Add arg
        | TExprG.Let(value = value; body = body) ->
            acc.Add value
            acc.Add body
        | TExprG.Use(value = value; body = body) ->
            acc.Add value
            acc.Add body
        | TExprG.IfThenElse(cond = cond; thenExpr = thenExpr; elseExpr = elseExpr) ->
            acc.Add cond
            acc.Add thenExpr
            acc.Add elseExpr
        | TExprG.Tuple(items = items)
        | TExprG.Sequential(items = items) ->
            for x in items do
                acc.Add x
        | TExprG.While(cond = cond; body = body) ->
            acc.Add cond
            acc.Add body
        | TExprG.ForTo(startExpr = startExpr; endExpr = endExpr; body = body) ->
            acc.Add startExpr
            acc.Add endExpr
            acc.Add body
        | TExprG.ForIn(source = source; body = body) ->
            acc.Add source
            acc.Add body
        | TExprG.Match(scrutinee = scrutinee; arms = arms) ->
            acc.Add scrutinee

            for arm in arms do
                match arm.Guard with
                | ValueSome g -> acc.Add g
                | ValueNone -> ()

                acc.Add arm.Body
        | TExprG.TryWith(body = body; arms = arms) ->
            acc.Add body

            for arm in arms do
                match arm.Guard with
                | ValueSome g -> acc.Add g
                | ValueNone -> ()

                acc.Add arm.Body
        | TExprG.TryFinally(body = body; cleanup = cleanup) ->
            acc.Add body
            acc.Add cleanup
        | TExprG.Assignment(lhs = lhs; rhs = rhs) ->
            acc.Add lhs
            acc.Add rhs
        | TExprG.Range(startExpr = startExpr; step = step; stopExpr = stopExpr) ->
            acc.Add startExpr

            match step with
            | Some s -> acc.Add s
            | None -> ()

            acc.Add stopExpr
        | TExprG.RecordCons(fields = fields) ->
            for (_, v) in fields do
                acc.Add v
        | TExprG.RecordClone(source = source; overrides = overrides) ->
            acc.Add source

            for (_, v) in overrides do
                acc.Add v
        | TExprG.FieldGet(receiver = receiver) -> acc.Add receiver
        | TExprG.FieldSet(receiver = receiver; value = value) ->
            acc.Add receiver
            acc.Add value
        | TExprG.UnionCons(args = args) ->
            for x in args do
                acc.Add x
        | TExprG.New(args = args) ->
            for x in args do
                acc.Add x
        | TExprG.MethodCall(receiver = receiver; args = args) ->
            acc.Add receiver

            for x in args do
                acc.Add x
        | TExprG.PropertyGet(receiver = receiver) -> acc.Add receiver
        | TExprG.StaticMethodCall(args = args) ->
            for x in args do
                acc.Add x
        | TExprG.StaticFieldSet(value = value) -> acc.Add value
        | TExprG.ExternalMember(receiver = receiver) ->
            match receiver with
            | ValueSome r -> acc.Add r
            | ValueNone -> ()
        | TExprG.Format(sink = sink; segments = segments) ->
            match sink with
            | FormatSinkG.ToWriter(writer = writer) -> acc.Add writer
            | FormatSinkG.ToBuilder builder -> acc.Add builder
            | FormatSinkG.ToStdOut _
            | FormatSinkG.ToStdErr _
            | FormatSinkG.ToString -> ()

            for seg in segments do
                match seg with
                | FormatSegG.Lit _ -> ()
                | FormatSegG.Hole(_, value) -> acc.Add value
                | FormatSegG.DynHole hole ->
                    match hole.Width with
                    | ValueSome w -> acc.Add w
                    | ValueNone -> ()

                    match hole.Precision with
                    | ValueSome p -> acc.Add p
                    | ValueNone -> ()

                    acc.Add hole.Value
                | FormatSegG.CallbackHole(residue = residue) -> acc.Add residue
        | TExprG.ILIntrinsic(args = args) ->
            for x in args do
                acc.Add x
        | TExprG.StaticOptimization(clauses = clauses; defaultExpr = defaultExpr) ->
            for clause in clauses do
                acc.Add clause.Body

            acc.Add defaultExpr
        | TExprG.Upcast(source = source) -> acc.Add source
        | TExprG.Downcast(source = source) -> acc.Add source
        | TExprG.TypeTest(source = source) -> acc.Add source
        | TExprG.TraitCall(args = args) ->
            for x in args do
                acc.Add x

        acc.ToArray()

    /// The immediate child *patterns* an expression owns directly, in source order —
    /// the binders (`Lambda`/`Let`/`Use`/`ForIn`) and the per-arm scrutinee patterns
    /// (`Match`/`TryWith`) that are part of THIS node. They are NOT reachable through
    /// `exprChildren` (which yields only sub-expressions). Only the six binder/arm
    /// shapes own patterns. `ForTo`'s loop variable is a bare binder, not a pattern, so
    /// it is not a pat child — it rides the node's own payload.
    let exprPatChildren (e: TExprG<FrozenType, SyntaxToken, 'id>) : TPatG<FrozenType, SyntaxToken, 'id>[] =
        let acc = ResizeArray<TPatG<FrozenType, SyntaxToken, 'id>>()

        match e with
        | TExprG.Const _
        | TExprG.Var _
        | TExprG.External _
        | TExprG.App _
        | TExprG.IfThenElse _
        | TExprG.Tuple _
        | TExprG.Sequential _
        | TExprG.While _
        | TExprG.ForTo _
        | TExprG.TryFinally _
        | TExprG.Assignment _
        | TExprG.Null _
        | TExprG.Range _
        | TExprG.RecordCons _
        | TExprG.RecordClone _
        | TExprG.FieldGet _
        | TExprG.FieldSet _
        | TExprG.UnionCons _
        | TExprG.New _
        | TExprG.MethodCall _
        | TExprG.PropertyGet _
        | TExprG.StaticMethodCall _
        | TExprG.StaticPropertyGet _
        | TExprG.StaticFieldGet _
        | TExprG.StaticFieldSet _
        | TExprG.ExternalMember _
        | TExprG.Format _
        | TExprG.ILIntrinsic _
        | TExprG.StaticOptimization _
        | TExprG.Upcast _
        | TExprG.Downcast _
        | TExprG.TypeTest _
        | TExprG.TraitCall _ -> ()
        | TExprG.Lambda(param = param) -> acc.Add param
        | TExprG.Let(binding = binding) -> acc.Add binding
        | TExprG.Use(binding = binding) -> acc.Add binding
        | TExprG.ForIn(pat = pat) -> acc.Add pat
        | TExprG.Match(arms = arms) ->
            for arm in arms do
                acc.Add arm.Pat
        | TExprG.TryWith(arms = arms) ->
            for arm in arms do
                acc.Add arm.Pat

        acc.ToArray()

    /// The immediate sub-patterns, in source order (patterns own no child expressions).
    let patChildren (p: TPatG<FrozenType, SyntaxToken, 'id>) : TPatG<FrozenType, SyntaxToken, 'id>[] =
        let acc = ResizeArray<TPatG<FrozenType, SyntaxToken, 'id>>()

        match p with
        | TPatG.NamedSimple _
        | TPatG.Wildcard _
        | TPatG.Const _
        | TPatG.Null _
        | TPatG.EnumCase _ -> ()
        | TPatG.Tuple(items = items)
        | TPatG.Or(alts = items) ->
            for x in items do
                acc.Add x
        | TPatG.Record(fields = fields) ->
            for (_, sub) in fields do
                acc.Add sub
        | TPatG.Union(fields = fields) ->
            for x in fields do
                acc.Add x
        | TPatG.TypeTestAs(inner = inner) -> acc.Add inner

        acc.ToArray()

    /// The dense id of the binder the node being pooled INTRODUCES. `BinderKey.ofPat` /
    /// `BinderKey.ofExpr` answer `ValueSome` for exactly the two payload cases that name
    /// their own binder (`NamedSimple`, `ForTo`), and `poolPat`/`poolExpr` intern that
    /// answer before building the payload — so a miss is those two statements of "which
    /// node binds" having drifted apart, not a defect of the tree.
    let private introducedBinder (site: string) (binder: BinderId voption) : BinderId =
        match binder with
        | ValueSome id -> id
        | ValueNone -> failwithf "TastPools.%s: the node's payload names a binder the walk interned none for" site

    /// The residual payload of a frozen expression node — its fields MINUS `ty`/`tok`, the
    /// child expr ids (`exprChildren`), the owned pat ids (`exprPatChildren`), and the `Var`
    /// binder id. The exact inverse of `substituteExpr`, mirroring `FrozenCodec.writeExprPayload`
    /// for what each case emits beyond those. Exhaustive on the DU with no catch-all, so a
    /// new `TExprG` case fails to compile here.
    ///
    /// `binder` is the dense id of the binder this node introduces (`introducedBinder`) —
    /// `ForTo`'s loop variable and nothing else.
    ///
    /// Public as the DU-domain counterpart of the `ExprPayloads` column: the pool-build
    /// gate checks a pooled node against the payload the DU node projects to, which is
    /// the whole residual rather than just its tag.
    let exprPayload (binder: BinderId voption) (e: TExprG<FrozenType, SyntaxToken, 'id>) : ExprPayload =
        // Per-arm guard-presence flags — the only residual structure a `Match`/`TryWith`
        // records (the arm pats/guards/bodies themselves ride the child columns); this is
        // what `substituteExpr.buildArms` re-nests them by.
        let armGuards (arms: EqArray<TMatchArmG<_, _>>) =
            arms |> EqArray.toArray |> Array.map (fun arm -> arm.Guard.IsSome)

        match e with
        | TExprG.Const(value = value) -> ExprPayload.Const value
        | TExprG.Var _ -> ExprPayload.Var
        | TExprG.External(compiledName = compiledName; key = key) ->
            ExprPayload.External
                {|
                    CompiledName = compiledName
                    Key = key
                |}
        | TExprG.Lambda _ -> ExprPayload.Lambda
        | TExprG.App _ -> ExprPayload.App
        | TExprG.Let _ -> ExprPayload.Let
        | TExprG.Use(dispose = dispose) -> ExprPayload.Use dispose
        | TExprG.IfThenElse _ -> ExprPayload.IfThenElse
        | TExprG.Tuple _ -> ExprPayload.Tuple
        | TExprG.Sequential _ -> ExprPayload.Sequential
        | TExprG.While _ -> ExprPayload.While
        | TExprG.ForTo(identTok = identTok) ->
            ExprPayload.ForTo
                {|
                    Var = introducedBinder "exprPayload" binder
                    IdentTok = identTok
                |}
        | TExprG.ForIn(enumerator = enumerator) -> ExprPayload.ForIn enumerator
        | TExprG.Match(arms = arms) -> ExprPayload.Match(armGuards arms)
        | TExprG.TryWith(arms = arms) -> ExprPayload.TryWith(armGuards arms)
        | TExprG.TryFinally _ -> ExprPayload.TryFinally
        | TExprG.Assignment _ -> ExprPayload.Assignment
        | TExprG.Null _ -> ExprPayload.Null
        | TExprG.Range(step = step) -> ExprPayload.Range step.IsSome
        | TExprG.RecordCons(fields = fields) -> ExprPayload.RecordCons(fields |> EqArray.toArray |> Array.map fst)
        | TExprG.RecordClone(overrides = overrides) ->
            ExprPayload.RecordClone(overrides |> EqArray.toArray |> Array.map fst)
        | TExprG.FieldGet(fieldName = fieldName) -> ExprPayload.FieldGet fieldName
        | TExprG.FieldSet(fieldName = fieldName) -> ExprPayload.FieldSet fieldName
        | TExprG.UnionCons(caseName = caseName) -> ExprPayload.UnionCons caseName
        | TExprG.New(className = className; key = key) -> ExprPayload.New {| ClassName = className; Key = key |}
        | TExprG.MethodCall(key = key; via = via) -> ExprPayload.MethodCall {| Key = key; Via = via |}
        | TExprG.PropertyGet(key = key; via = via) -> ExprPayload.PropertyGet {| Key = key; Via = via |}
        | TExprG.StaticMethodCall(key = key) -> ExprPayload.StaticMethodCall key
        | TExprG.StaticPropertyGet(key = key) -> ExprPayload.StaticPropertyGet key
        | TExprG.StaticFieldGet(declKey = declKey; fieldName = fieldName) ->
            ExprPayload.StaticFieldGet
                {|
                    DeclKey = declKey
                    FieldName = fieldName
                |}
        | TExprG.StaticFieldSet(declKey = declKey; fieldName = fieldName) ->
            ExprPayload.StaticFieldSet
                {|
                    DeclKey = declKey
                    FieldName = fieldName
                |}
        | TExprG.ExternalMember(receiver = receiver; key = key; memberName = memberName; storage = storage) ->
            ExprPayload.ExternalMember
                {|
                    HasReceiver = receiver.IsSome
                    Key = key
                    MemberName = memberName
                    Storage = storage
                |}
        | TExprG.Format(sink = sink; segments = segments) ->
            let sink' =
                match sink with
                | FormatSinkG.ToStdOut newline -> FormatSinkShape.ToStdOut newline
                | FormatSinkG.ToStdErr newline -> FormatSinkShape.ToStdErr newline
                | FormatSinkG.ToWriter(newline = newline) -> FormatSinkShape.ToWriter newline
                | FormatSinkG.ToBuilder _ -> FormatSinkShape.ToBuilder
                | FormatSinkG.ToString -> FormatSinkShape.ToString

            let segments' =
                segments
                |> EqArray.toArray
                |> Array.map (fun seg ->
                    match seg with
                    | FormatSegG.Lit s -> FormatSegShape.Lit s
                    | FormatSegG.Hole(spec, _) -> FormatSegShape.Hole spec
                    | FormatSegG.DynHole hole ->
                        FormatSegShape.DynHole(hole.Width.IsSome, hole.Precision.IsSome, hole.Spec)
                    | FormatSegG.CallbackHole(spec, _) -> FormatSegShape.CallbackHole spec
                )

            ExprPayload.Format {| Sink = sink'; Segments = segments' |}
        | TExprG.ILIntrinsic(opCode = opCode; typeOperand = typeOperand) ->
            ExprPayload.ILIntrinsic
                {|
                    OpCode = opCode
                    TypeOperand = typeOperand
                |}
        | TExprG.StaticOptimization(clauses = clauses) ->
            ExprPayload.StaticOptimization(clauses |> EqArray.toArray |> Array.map (fun clause -> clause.Constraints))
        | TExprG.Upcast _ -> ExprPayload.Upcast
        | TExprG.Downcast _ -> ExprPayload.Downcast
        | TExprG.TypeTest(testTy = testTy) -> ExprPayload.TypeTest testTy
        | TExprG.TraitCall(receiver = receiver; memberName = memberName) ->
            ExprPayload.TraitCall
                {|
                    Receiver = receiver
                    MemberName = memberName
                |}

    /// The residual payload of a frozen pattern node — its fields MINUS `ty`/`tok` and the
    /// child sub-pat ids (`patChildren`). The exact inverse of `substitutePat`, mirroring
    /// `FrozenCodec.writePatPayload`. Exhaustive with no catch-all, so a new `TPat` case fails to
    /// compile here. `binder` is as `exprPayload`'s — here it is `NamedSimple`'s own binder.
    /// Public for the same reason as `exprPayload`.
    let patPayload (binder: BinderId voption) (p: TPatG<FrozenType, SyntaxToken, 'id>) : PatPayload =
        match p with
        | TPatG.NamedSimple _ -> PatPayload.NamedSimple(introducedBinder "patPayload" binder)
        | TPatG.Wildcard _ -> PatPayload.Wildcard
        | TPatG.Null _ -> PatPayload.Null
        | TPatG.Tuple _ -> PatPayload.Tuple
        | TPatG.Or _ -> PatPayload.Or
        | TPatG.Const(value = value) -> PatPayload.Const value
        | TPatG.Record(fields = fields) -> PatPayload.Record(fields |> EqArray.toArray |> Array.map fst)
        | TPatG.Union(caseName = caseName) -> PatPayload.Union caseName
        | TPatG.TypeTestAs(testTy = testTy) -> PatPayload.TypeTestAs testTy
        | TPatG.EnumCase(enumKey = enumKey; caseName = caseName) ->
            PatPayload.EnumCase
                {|
                    EnumKey = enumKey
                    CaseName = caseName
                |}

    /// The id-keyed record a row cannot carry, reported as the walk appends the node it
    /// belongs to. One case per such record, so the datum a case carries is the one that
    /// case needs and there is no argument that is meaningful only under some other
    /// field's value. The DU node is deliberately NOT passed: a sink must be expressible
    /// without one, since the columns are Node-sufficient.
    [<RequireQualifiedAccess>]
    type PooledEvent<'id> =
        /// A `Var`'s reference edge. The walk cannot fill it — binder ids are the sink's
        /// to assign, and a `Var` may name a binder the walk has not reached — so it
        /// reports the reference as the tree writes it and the sink resolves it onto
        /// `ExprVarBinder`.
        | VarRef of binder: 'id * at: ExprPoolId
        /// A source lambda's slot in the lambda id space, with the token
        /// `LambdaKey.ofAnchor` computes its key from. The TOKEN and not the key: this walk
        /// is shared with the overlay sink, which mints nodes anchored on no lexed token —
        /// and drops the event unread, so the mint must not happen before the sink asks.
        | LambdaPooled of tok: SyntaxToken * at: ExprPoolId

    /// A definition site as a pooling sink sees it: the binder, plus the token that SPELLS
    /// it. The anchor is `ValueNone` at a declaration's pattern-less key slot — those hold
    /// a binder key and no token (`TastConvert.typeDecl`'s `fId` is handed nothing else),
    /// so there is none to offer, and that is the whole of the mint domain the naming
    /// column cannot anchor.
    [<Struct>]
    type BinderSite<'id> =
        {
            Binder: BinderKeyG<'id>
            Anchor: SyntaxToken voption
        }

    /// Where a pooling walk PUTS the rows it produces. The walk itself — which nodes
    /// exist, in what order, and which child edges they carry — is `poolExpr`/`poolPat`/
    /// `poolDecl` below and exists exactly once; a sink decides only where a row lands and
    /// how a binder id is assigned. `toPools` fills a fresh pool with one; an overlay
    /// builder appends to a stacked one with another.
    ///
    /// Generic in the identity the WALKED tree names binders by: a source-shaped file
    /// names them by `NodeKey` and an already-pooled tree by `BinderId`, and both are
    /// poured into the same dense columns.
    type PoolSink<'id> =
        {
            /// Called for every binder a walked node INTRODUCES (a `NamedSimple` pattern's
            /// binding, a `ForTo` loop variable, a declaration shape's pattern-less key
            /// slot), before the node's row is added, and answering with the dense id that
            /// binder took — which is what the node's own payload then names it by.
            /// Idempotent in the binder.
            InternBinder: BinderSite<'id> -> BinderId
            AddExpr: ExprRow -> ExprPoolId
            AddPat: PatRow -> PatPoolId
            AddDecl: DeclRow -> DeclPoolId
            /// Called once per pooled expr node that carries an id-keyed record, after
            /// its row is added. Not called at all for a node that carries none.
            OnExprPooled: PooledEvent<'id> -> unit
        }

    [<RequireQualifiedAccess>]
    module PoolSink =

        /// The sink's interning as `TastConvert.typeDecl` wants it: a declaration's key
        /// slots are the sites NO node spells, so this is the one place an anchorless site
        /// is built.
        let internSlot (sink: PoolSink<'id>) (b: BinderKeyG<'id>) : BinderId =
            sink.InternBinder { Binder = b; Anchor = ValueNone }

    /// Pool a pattern subtree post-order: a node's children are pooled before the node
    /// itself, so every child id its row names already resolves.
    let rec poolPat (sink: PoolSink<'id>) (p: TPatG<FrozenType, SyntaxToken, 'id>) : PatPoolId =
        // Interned BEFORE the payload is built: the payload names this binder by the id
        // the intern hands back, so there is one identity rather than a key and an id.
        // A pattern SPELLS the binder it introduces, so its own anchor is the name's.
        let binder =
            BinderKey.ofPat p
            |> ValueOption.map (fun b ->
                sink.InternBinder
                    {
                        Binder = b
                        Anchor = ValueSome(TastWalk.patTok p)
                    }
            )

        let kids = patChildren p |> Array.map (poolPat sink)

        sink.AddPat
            {
                Ty = TastWalk.patTy p
                Tok = TastWalk.patTok p
                Children = kids
                Payload = patPayload binder p
            }

    /// Pool an expression subtree post-order (see `poolPat`), its owned sub-patterns
    /// included.
    let rec poolExpr (sink: PoolSink<'id>) (e: TExprG<FrozenType, SyntaxToken, 'id>) : ExprPoolId =
        // A `ForTo` binds its loop variable with no pattern node behind it, so the
        // intern cannot ride `poolPat` — and the token that spells it is the loop
        // variable's own, not the node's anchor (which is the `for` keyword).
        let binder =
            BinderKey.siteOfExpr e
            |> ValueOption.map (fun (struct (b, identTok)) ->
                sink.InternBinder
                    {
                        Binder = b
                        Anchor = ValueSome identTok
                    }
            )

        let exprKids = exprChildren e |> Array.map (poolExpr sink)
        let patKids = exprPatChildren e |> Array.map (poolPat sink)

        let row =
            {
                Ty = TastWalk.exprTy e
                Tok = TastWalk.exprTok e
                Children = exprKids
                PatChildren = patKids
                VarBinder = ValueNone
                Payload = exprPayload binder e
            }

        let id = sink.AddExpr row

        match e with
        | TExprG.Var(binding = binding) -> sink.OnExprPooled(PooledEvent.VarRef(binding, id))
        | TExprG.Lambda _ -> sink.OnExprPooled(PooledEvent.LambdaPooled(row.Tok, id))
        | _ -> ()

        id

    /// The residual payload of a frozen declaration node — its fields MINUS the child
    /// expr/pat roots. The exact inverse of `substituteDecl`. Exhaustive with no catch-all,
    /// so a new `TDecl` case fails to compile here.
    ///
    /// A `Type` decl is where the payload does real work: its member/preamble/ctor bodies
    /// are pooled through the sink and the declaration keeps their IDS in the slots that
    /// held the trees, and its pattern-less binder slots (`this`, member/ctor parameters,
    /// ctor locals) likewise keep the dense id the intern hands back (`PooledTypeDecl`).
    /// `TastConvert.typeDecl` supplies the traversal — the same one the `'ty` freeze runs —
    /// so the seven body slots and the seven key slots are enumerated in one place.
    ///
    /// The re-filing IS the interning: a slot is offered to the sink exactly where its id
    /// replaces it, so no slot can be rewritten without having been interned and none can
    /// be interned without being rewritten.
    let private declPayload (sink: PoolSink<'id>) (d: TDeclG<FrozenType, SyntaxToken, 'id>) : DeclPayload =
        match d with
        | TDeclG.Let(isInline = isInline; ty = ty) -> DeclPayload.Let {| IsInline = isInline; Ty = ty |}
        | TDeclG.Expression(ty = ty) -> DeclPayload.Expression ty
        | TDeclG.Type td -> DeclPayload.Type(TastConvert.typeDecl id (PoolSink.internSlot sink) (poolExpr sink) td)

    /// Pool a declaration, its expr/pat roots (see `poolPat`) and — for a `Type` decl —
    /// its member bodies, which the payload names by id rather than surfacing as children.
    let poolDecl (sink: PoolSink<'id>) (d: TDeclG<FrozenType, SyntaxToken, 'id>) : DeclPoolId =
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

    /// The SOURCE arity of every module binding, READ OFF THE POOLED SPINE — the columns
    /// `toPools` has just filled, so a tuple group's pattern IS the lambda-spine node it
    /// was peeled from and not a re-pooled copy of it. Derived rather than carried: it is a
    /// projection of the tree, and the only way for a projection to disagree with what it
    /// projects is for the two to be computed apart.
    ///
    /// `ArgGroups.peel` is the walk; the readers below are the only column-domain part.
    ///
    /// Nothing here resolves a key: the binder a head pattern introduces is already the
    /// dense id its payload carries, so the table is keyed by the very id the pooled node
    /// bears rather than by a lookup that could miss.
    let private bindingValReprs (pools: FrozenPools) : DenseTable<BinderId, PooledValRepr> =
        let unLambda (ExprPoolId i) =
            match pools.ExprPayloads.[i] with
            | ExprPayload.Lambda -> ValueSome(struct (pools.ExprPatChildren.[i].[0], pools.ExprChildren.[i].[0]))
            | _ -> ValueNone

        let facts (PatPoolId i) : ArgGroups.ParamPatFacts<BinderId> =
            {
                Shape = PatPayload.shape pools.PatPayloads.[i]
                Ty = pools.PatTys.[i]
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
                    let (PatPoolId head) = pools.DeclPatChildren.[d].[0]

                    match pools.PatPayloads.[head] with
                    // Only a simple binder has a side-table identity; a destructuring or
                    // wildcard head introduces none and needs none (see `BinderKey.ofPat`).
                    | PatPayload.NamedSimple binder ->
                        let groups, body = ArgGroups.peel unLambda facts pools.DeclExprChildren.[d].[0]
                        let (ExprPoolId b) = body

                        yield
                            binder,
                            {
                                // A plain value has no lambda groups and records an
                                // empty-`Groups` entry, which the file→file signature
                                // projection reads as "not a function".
                                //
                                // `pools` already carries the filled column, so the width
                                // comes from the very one it ships with rather than from a
                                // second NodeKey-keyed copy of it.
                                Typars = FrozenPools.typarArity pools binder
                                Groups = groups
                                ResultTy = pools.ExprTys.[b]
                            }
                    | _ -> ()
                | DeclPayload.Expression _
                | DeclPayload.Type _ -> ()
        |]

    /// The identifier `source` spells at a definition site, or the EMPTY string where none
    /// is spelled there — a binder the freeze MINTED (a class's `this`/`base`, a freshened
    /// inline binder) has no source name at all, and a counter-minted key names no place in
    /// the file to look for one.
    ///
    /// THE reading of a binder's name, made once, at the freeze, so the text a backend
    /// emits is a column value and not a re-scan of a source file the backend may no longer
    /// hold (a unit emitted from its cache blob has none). The text is verbatim: mangling it
    /// for a target dialect's reserved words or illegal characters belongs to the backend.
    let identifierAt (source: string) (k: NodeKey) : string =
        let isStart (c: char) = System.Char.IsLetter c || c = '_'

        let isCont (c: char) =
            System.Char.IsLetterOrDigit c || c = '_' || c = '\''

        if
            k.IsSynthetic
            || k.Offset < 0
            || k.Offset >= source.Length
            || not (isStart source.[k.Offset])
        then
            ""
        else
            let mutable i = k.Offset

            while i < source.Length && isCont source.[i] do
                i <- i + 1

            source.Substring(k.Offset, i - k.Offset)

    /// Pool a tree, assigning each reachable node a dense id and recording its child edges
    /// as ids. Generic in the identity the tree names binders by, because the fill runs in
    /// BOTH directions of the round-trip: a source-shaped file arrives naming them by
    /// `NodeKey`, a file drained back out of the columns by `BinderId`, and the columns are
    /// the same columns either way.
    ///
    /// `nameOf` is the only thing that differs — how the naming column is filled — because
    /// it is the one fact the identity does not carry: a `NodeKey` finds it in the source
    /// text, a `BinderId` in the pool that issued it. See `toPools` and `rePool`.
    let private fill (nameOf: 'id -> string) (file: TastFileG<FrozenType, SyntaxToken, 'id>) : FrozenPools =
        // The expression pool as parallel column builders (struct-of-arrays); all are
        // appended together per node so they stay index-aligned by `ExprPoolId`.
        let exprTys = ResizeArray<FrozenType>()
        let exprToks = ResizeArray<SyntaxToken>()
        let exprChildrenCol = ResizeArray<ExprPoolId[]>()
        let exprPatChildrenCol = ResizeArray<PatPoolId[]>()
        let exprPayloads = ResizeArray<ExprPayload>()

        // Each `Var`'s expr id + the binder it references, captured in pass 1 and resolved
        // to a `BinderId` in pass 2 — a `Var` may name a binder pooled after it (a forward /
        // mutually-recursive reference), so the enumeration must complete first.
        let varBindings = ResizeArray<struct (int * 'id)>()

        // The pattern pool as parallel column builders (struct-of-arrays), index-aligned by
        // `PatPoolId`.
        let patTys = ResizeArray<FrozenType>()
        let patToks = ResizeArray<SyntaxToken>()
        let patChildrenCol = ResizeArray<PatPoolId[]>()
        let patPayloads = ResizeArray<PatPayload>()

        // The declaration pool as parallel column builders, index-aligned by `DeclPoolId`.
        let declExprChildrenCol = ResizeArray<ExprPoolId[]>()
        let declPatChildrenCol = ResizeArray<PatPoolId[]>()
        let declPayloads = ResizeArray<DeclPayload>()

        // The binder pool: each distinct definition site the walk reaches takes a dense
        // `BinderId` on first encounter, and its two columns record what the slot alone
        // cannot say — how the source spells it, and where. The introducing sites are
        // enumerated by the `BinderKey` projections (`ofPat` / `siteOfExpr`, plus a
        // declaration shape's key slots, which are typed by one) as the trees are walked,
        // so nothing re-derives which nodes bind — and the side tables remapped below were
        // filed through those same projections.
        //
        // The WALKED tree's identity is the interning KEY and stops here: nothing of it
        // reaches the columns, so a source-shaped file's `NodeKey`s — a grammar-versioned
        // node kind included — cannot be read back off a frozen file. It is what makes the
        // intern idempotent while the walk still speaks whatever identity it arrived in.
        //
        // The enumeration spans the whole FILE, because a side table may key on a binder in
        // any of its trees — and every tree the file bears is now pooled, so ONE walk covers
        // them all. Correspondingly a `BinderId` need not have a pooled pattern node behind
        // it (a `this` slot has none anywhere) — the binder space is dense and independent,
        // inverted by position.
        let binderNames = ResizeArray<string>()
        let binderToks = ResizeArray<SyntaxToken voption>()
        let binderIds = System.Collections.Generic.Dictionary<'id, BinderId>()

        // The lambda id space: a source lambda's dense id IS its `ExprPoolId` (positional —
        // every `Lambda` expr is already in the `Expr*` columns), paired with the key its
        // `FunVerdicts` entry is filed under (`LambdaKey.ofAnchor`).
        //
        // A LIST, not a key→id map, because the key is one-to-MANY over this space and a map
        // could only keep one of the nodes. Two lambdas share a key whenever they share a
        // source token, which is routine in two ways: the published TEMPLATE is a second tree
        // over the same source as the emitted function it was stashed from, and an inline
        // splice moves a whole body onto the CALL SITE's one token (`Inline.spliceAt`), so
        // every lambda in it lands on that token together. The verdict belongs to ALL of
        // them; a map would have silently given it to whichever was pooled last, and left
        // every other copy to emit as an ordinary heap closure.
        let lambdaSlots = ResizeArray<struct (ExprPoolId * LambdaKey)>()

        let internBinder (site: BinderSite<'id>) : BinderId =
            let k = BinderKey.identity site.Binder

            match binderIds.TryGetValue k with
            | true, id -> id
            | false, _ ->
                let id = BinderId binderNames.Count
                binderIds.Add(k, id)
                binderNames.Add(nameOf k)
                binderToks.Add site.Anchor
                id

        // THE anchor of a node of THIS FILE — its `ExprToks`/`PatToks` value — as it enters
        // the column, checked to be a real lexed token.
        //
        // No node of a frozen file can anchor on a VIRTUAL token, so the anchor columns
        // need no sentinel. Every anchor rule (`CstKeys.firstTokenOfExpr`/`firstTokenOfPat`)
        // takes a keyword, a real operator, a real opening delimiter, or recurses; recovery
        // synthesises only CLOSING delimiters (`ParsingHelpers.pEnclosed` passes the real
        // `l` through), and the only virtual-IDENTIFIER producer (`recoverLongIdent`) serves
        // `open`/`namespace`/`module` headers, for which there is no frozen decl at all.
        // That is N individually-correct choices; this is the one place they are all
        // answerable, so an arm that comes to pick a virtual token is caught here rather
        // than by whichever consumer first asks the anchor for text or a position.
        //
        // It is a property of a node OF A FILE, which is why it lives on this sink rather
        // than in the shared walk: `TastPoolBuilder`'s overlay keeps its own rows and never
        // appends to these columns, so what it mints is not a node of this file and owes
        // this nothing. The one production mint that does anchor on a virtual token is the
        // contract parameter (`TastLower.externalValRepr`) — a `.fsi` `val` has no lambda
        // tree, so its reconstructed pats are anchored at offset 0 and no reader ever asks
        // them for a position.
        let anchor (tok: SyntaxToken) : SyntaxToken =
            match tok.Index with
            | TokenIndex.Regular _ -> tok
            | TokenIndex.Virtual ->
                failwithf
                    "TastPools.toPools: a frozen node anchors on the VIRTUAL token %A, which names no place in the source"
                    tok

        // The sink: rows land at the end of the column builders, so a node's id is the
        // count at the moment it is added. `ExprRow.VarBinder` is dropped here — the `Var`
        // reference edge cannot resolve until the binder enumeration is complete, so it is
        // recorded as pending and filled by the second pass below.
        let sink: PoolSink<'id> =
            {
                InternBinder = internBinder
                AddExpr =
                    fun row ->
                        let id = exprPayloads.Count
                        exprTys.Add row.Ty
                        exprToks.Add(anchor row.Tok)
                        exprChildrenCol.Add row.Children
                        exprPatChildrenCol.Add row.PatChildren
                        exprPayloads.Add row.Payload
                        ExprPoolId id
                AddPat =
                    fun row ->
                        let id = patPayloads.Count
                        patTys.Add row.Ty
                        patToks.Add(anchor row.Tok)
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
                // A `Var`'s binder reference resolves in pass 2 (see `varBindings`); a
                // lambda's positional identity is its slot, stamped so `FunVerdicts`
                // (lambda-expression-keyed) resolves onto it.
                OnExprPooled =
                    fun ev ->
                        match ev with
                        | PooledEvent.VarRef(binder, ExprPoolId id) -> varBindings.Add(struct (id, binder))
                        | PooledEvent.LambdaPooled(tok, id) -> lambdaSlots.Add(struct (id, LambdaKey.ofAnchor tok))
            }

        let roots = file.Decls |> EqArray.toArray |> Array.map (poolDecl sink)

        // The inline vocabulary, pooled as its OWN roots. A template is a different tree
        // from the emitted function of the same name — the unwalked snapshot `Freeze`
        // published — so the two are pooled independently and neither is derived from the
        // other. Both are ordinary pooled decls; the binder enumeration therefore reaches a
        // template's binders as part of the normal walk, with no intern-only side pass.
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

        // THE lookup: the dense id the enumeration above interned a key under. Written
        // once because both faults below ARE this lookup missing; what differs is only
        // what a miss means, and that is what each of them says.
        let internedBinderId (k: 'id) : BinderId voption =
            match binderIds.TryGetValue k with
            | true, id -> ValueSome id
            | false, _ -> ValueNone

        // Resolve a REFERENCE to the binder it names — a `Var`'s binding edge, or a row
        // derived off the columns. A miss means the referent was introduced by no
        // definition site the walk covers: an incomplete binder enumeration, which is
        // exactly the failure the id-resolution gate exists to surface. A reference is
        // written by whoever resolved the name, so it arrives as a bare `NodeKey`.
        let binderIdOfRef (referent: string) (k: 'id) : BinderId =
            match internedBinderId k with
            | ValueSome id -> id
            | ValueNone ->
                failwithf "TastPools.toPools: %s key %O references a binder no definition site introduced" referent k

        // SIDE-TABLE REACHABILITY — and THE POLICY every binder-keyed table is held to.
        //
        // `BinderKey`'s private constructor already makes a key naming a NON-binder
        // unwritable. What is left is the other half: a key naming a real binder whose
        // declaration was dropped before the freeze. The entry then addresses nothing, and
        // its reader gets a silent miss where a fact was recorded.
        //
        // The policy is FAULT BY DEFAULT. A new binder-keyed table inherits this check by
        // being remapped at all, and the fix for a fault is at the PRODUCER: prune the
        // entry where the declaration is pruned, the way `Elaborate.translateModuleElem`
        // records no binder for a binding it elides. Dropping the surplus key here instead
        // is permitted only where the producer can argue, AT ITSELF, that its surplus is
        // inert — `Regions.closureReprSnapshot` is the one table that does, and it filters
        // in its own body, which is where such an argument has to live to stay checkable
        // against the code it is about.
        let binderIdOf (referent: string) (b: BinderKeyG<'id>) : BinderId =
            let k = BinderKey.identity b

            match internedBinderId k with
            | ValueSome id -> id
            | ValueNone ->
                failwithf
                    "TastPools.toPools: %s entry %O names a binder no declaration in the frozen file introduces — prune the entry where its declaration is pruned"
                    referent
                    k

        // `FunVerdicts` onto the lambda id space, driven from the ID side: every pooled
        // lambda is offered the key it was stamped with, and a lambda whose key carries a
        // verdict takes a row. Driving it from the KEY side instead is what a `Map` remap
        // would do, and it can only pick ONE lambda per key — see `lambdaSlots` for why
        // that is the wrong arity.
        //
        // The reachability check the key side did own still holds, and is made explicitly:
        // a verdict key that stamped no pooled lambda is a table entry addressing nothing,
        // the same defect `binderIdOf` faults on for the binder-keyed tables.
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

        // Second pass: now the enumeration is complete, route each `Var`'s reference edge
        // to its binder's dense id — the sparse `ExprVarBinder` column (`ValueNone` at
        // every non-`Var` slot).
        let exprVarBinder: BinderId voption[] = Array.create exprPayloads.Count ValueNone

        for (struct (id, key)) in varBindings do
            exprVarBinder.[id] <- ValueSome(binderIdOfRef "Var" key)

        // One generic remap over the BINDER-keyed side tables, parameterized by the key
        // resolver so an unresolvable key says which table holds it (each call site applies
        // the resolver to its table's name first). `FunVerdicts` does not come through here:
        // its key space is one-to-many over the ids it maps to, so it is built from the id
        // side above.
        let remapSideTable (resolve: 'k -> 'dense) (m: Map<'k, 'v>) : ('dense * 'v)[] =
            m |> Map.toArray |> Array.map (fun (k, v) -> resolve k, v)

        // A per-binder SCALAR goes into a COLUMN instead (`BinderColumn`): the producer's
        // key is resolved here — through the same `binderIdOf`, so the reachability policy
        // above applies to it unchanged — and then DROPPED, the fact landing at the binder's
        // own slot. What that removes is the stored key, and with it the possibility of a
        // stale entry surviving the freeze at all.
        let binderColumn (referent: string) (m: Map<BinderKeyG<'id>, 'v>) : BinderColumn<'v> =
            let col = Array.create binderNames.Count ValueNone

            for KeyValue(k, v) in m do
                let (BinderId i) = binderIdOf referent k
                col.[i] <- ValueSome v

            col

        // Every column is snapshotted here and NOTHING below appends: the derived table
        // that follows only READS the pools, so the record's field order carries no
        // correctness weight.
        let pools =
            {
                ExprTys = exprTys.ToArray()
                ExprToks = exprToks.ToArray()
                ExprChildren = exprChildrenCol.ToArray()
                ExprPatChildren = exprPatChildrenCol.ToArray()
                ExprVarBinder = exprVarBinder
                ExprPayloads = exprPayloads.ToArray()
                PatTys = patTys.ToArray()
                PatToks = patToks.ToArray()
                PatChildren = patChildrenCol.ToArray()
                PatPayloads = patPayloads.ToArray()
                DeclExprChildren = declExprChildrenCol.ToArray()
                DeclPatChildren = declPatChildrenCol.ToArray()
                DeclPayloads = declPayloads.ToArray()
                Roots = roots
                InlineTemplates = inlineTemplates
                BinderNames = binderNames.ToArray()
                BinderToks = binderToks.ToArray()
                Residue =
                    {
                        Diagnostics = file.Diagnostics
                        IntrinsicReprKeys = file.IntrinsicReprKeys
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

    /// Pool a source-shaped frozen file — the freeze's last step. `source` is the file's
    /// own text, read only to fill the binder naming column (`identifierAt`).
    let toPools (source: string) (file: Frozen.TastFile) : FrozenPools = fill (identifierAt source) file

    /// Pool a tree DRAINED from `pools` (`TastUnpool.ofPools`) — the fill direction of the
    /// round-trip, which is what makes `ofPools` checkable at all: the columns are
    /// tree-sufficient exactly when re-pooling their own drain reproduces them.
    ///
    /// The naming column cannot be re-read from source here, a `BinderId` naming no
    /// position in a file; it is carried over from the pools the tree came out of, which is
    /// the one place it was ever read.
    let rePool (pools: FrozenPools) (file: Pooled.TastFile) : FrozenPools =
        fill (fun (BinderId i) -> pools.BinderNames.[i]) file
