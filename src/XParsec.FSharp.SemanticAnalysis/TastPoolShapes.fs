namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer

// What ONE node PROJECTS TO on its way into the columns: the child edges it owns, in the
// order the columns record them, and the residual payload left once `ty`/`tok`/the child
// ids/the binder id are lifted out of it. Pure, total, and free of any pool — where those
// projections LAND is `TastPools.fs`, and what they land in is `TastPoolNodes.fs`.
//
// This is the LAST place a `Frozen.*` DU is walked — every consumer reads the columns —
// so the tree shape is written down exactly once, here, and the child ORDER these produce
// IS the order of the `ExprChildren`/`ExprPatChildren`/`PatChildren` columns. Each match
// is exhaustive with no catch-all: a new `TExprG`/`TPatG`/`TDeclG` case fails to compile
// rather than silently escaping the pool. A node's SHAPE tag is not among them — it is
// `ExprPayload.shape` of the payload these produce, never a second match over the DU.
//
// Generic in the tree's token and identity axes, because the fill runs in both directions
// of the round trip and the shape of a node is the same either way.

module TastPoolShapes =

    /// The immediate child *expressions*, in evaluation order. Sub-patterns are NOT
    /// children (see `exprPatChildren`); composite carriers with no node identity of
    /// their own (match arms, format segments, static-opt clauses) are descended into
    /// so every reachable sub-expression appears exactly once.
    let exprChildren (e: TExprG<FrozenType, 'tok, 'id>) : TExprG<FrozenType, 'tok, 'id>[] =
        let acc = ResizeArray<TExprG<FrozenType, 'tok, 'id>>()

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
    let exprPatChildren (e: TExprG<FrozenType, 'tok, 'id>) : TPatG<FrozenType, 'tok, 'id>[] =
        let acc = ResizeArray<TPatG<FrozenType, 'tok, 'id>>()

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
    let patChildren (p: TPatG<FrozenType, 'tok, 'id>) : TPatG<FrozenType, 'tok, 'id>[] =
        let acc = ResizeArray<TPatG<FrozenType, 'tok, 'id>>()

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
        | ValueNone -> failwithf "TastPoolShapes.%s: the node's payload names a binder the walk interned none for" site

    /// The residual payload of a frozen expression node — its fields MINUS `ty`/`tok`, the
    /// child expr ids (`exprChildren`), the owned pat ids (`exprPatChildren`), and the `Var`
    /// binder id. The exact inverse of `substituteExpr`, mirroring `FrozenCodec.writeExprPayload`
    /// for what each case emits beyond those. Exhaustive on the DU with no catch-all, so a
    /// new `TExprG` case fails to compile here.
    ///
    /// `binder` is the dense id of the binder this node introduces (`introducedBinder`) —
    /// `ForTo`'s loop variable and nothing else. `anchor` narrows the walked tree's tokens
    /// to the stored index (the sink's, see `PoolSink.Anchor`); a `ForTo`'s `identTok` is
    /// the one anchor a payload carries.
    ///
    /// Public as the DU-domain counterpart of the `ExprPayloads` column: the pool-build
    /// gate checks a pooled node against the payload the DU node projects to, which is
    /// the whole residual rather than just its tag.
    let exprPayload
        (anchor: 'tok -> Anchor)
        (binder: BinderId voption)
        (e: TExprG<FrozenType, 'tok, 'id>)
        : ExprPayload =
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
                    IdentTok = anchor identTok
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

            // A hole carries an anchor of its own, so it is narrowed here exactly as a
            // node's is — `ExprPayload.format` widens it back.
            let spec = TastConvert.hole id anchor

            let segments' =
                segments
                |> EqArray.toArray
                |> Array.map (fun seg ->
                    match seg with
                    | FormatSegG.Lit s -> FormatSegShape.Lit s
                    | FormatSegG.Hole(h, _) -> FormatSegShape.Hole(spec h)
                    | FormatSegG.DynHole hole ->
                        FormatSegShape.DynHole(hole.Width.IsSome, hole.Precision.IsSome, spec hole.Spec)
                    | FormatSegG.CallbackHole(h, _) -> FormatSegShape.CallbackHole(spec h)
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
    let patPayload (binder: BinderId voption) (p: TPatG<FrozenType, 'tok, 'id>) : PatPayload =
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
