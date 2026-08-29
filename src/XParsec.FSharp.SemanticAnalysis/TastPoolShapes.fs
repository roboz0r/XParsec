namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer

// What ONE node PROJECTS TO on its way into the columns: the child edges it owns, in the
// order the columns record them, and the residual payload left once `ty`/`tok`/the child
// ids/the bound variable id are lifted out of it. Pure and free of any pool.

module TastPoolShapes =

    /// The immediate child *expressions*, in evaluation order. Sub-patterns are NOT
    /// children; composite carriers with no node identity of their own, namely match arms,
    /// format segments and static-opt clauses, are descended into, each sub-expression once.
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
        | TExprG.ArrayLit(elems = items)
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
        | TExprG.FieldGet(objArg = objArg) -> acc.Add objArg
        | TExprG.FieldSet(objArg = objArg; value = value) ->
            acc.Add objArg
            acc.Add value
        | TExprG.UnionCons(args = args) ->
            for x in args do
                acc.Add x
        | TExprG.New(args = args) ->
            for x in args do
                acc.Add x
        | TExprG.MethodCall(objArg = objArg; args = args) ->
            acc.Add objArg

            for x in args do
                acc.Add x
        | TExprG.PropertyGet(objArg = objArg) -> acc.Add objArg
        | TExprG.StaticMethodCall(args = args) ->
            for x in args do
                acc.Add x
        | TExprG.StaticFieldSet(value = value) -> acc.Add value
        | TExprG.ExternalMember(objArg = objArg) ->
            match objArg with
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
        | TExprG.CallerExpr(body = body) -> acc.Add body
        | TExprG.TraitCall(args = args) ->
            for x in args do
                acc.Add x
        // The ENTRY is not a child: it is a separate pool root, shared by every call site
        // that references it. Only the call's own argument expressions belong to this node.
        | TExprG.InlineCall(args = args) ->
            for x in args do
                acc.Add x

        acc.ToArray()

    /// The immediate child *patterns* an expression owns directly, in source order: the
    /// bound variables (`Lambda`/`Let`/`Use`/`ForIn`) and the per-arm scrutinee patterns
    /// (`Match`/`TryWith`). `ForTo`'s loop variable is a field of the node, not a pattern.
    let exprPatChildren (e: TExprG<FrozenType, 'tok, 'id>) : TPatG<FrozenType, 'tok, 'id>[] =
        let acc = ResizeArray<TPatG<FrozenType, 'tok, 'id>>()

        match e with
        | TExprG.Const _
        | TExprG.Var _
        | TExprG.External _
        | TExprG.App _
        | TExprG.IfThenElse _
        | TExprG.Tuple _
        | TExprG.ArrayLit _
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
        | TExprG.TraitCall _
        | TExprG.InlineCall _
        | TExprG.CallerExpr _ -> ()
        | TExprG.Lambda(param = param) -> acc.Add param
        | TExprG.Let(pattern = pattern) -> acc.Add pattern
        | TExprG.Use(pattern = pattern) -> acc.Add pattern
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

    /// The dense id of the bound variable the node being pooled INTRODUCES, at `NamedSimple` and
    /// `ForTo` and no other case. A miss is the pooling walk and the payload projection
    /// disagreeing about which node binds, not a defect of the tree.
    let private introducedBoundVar (boundVar: BoundVarId voption) : BoundVarId =
        match boundVar with
        | ValueSome id -> id
        | ValueNone ->
            failwith
                "TastPoolShapes.introducedBoundVar: the node's payload names a bound variable the walk interned none for"

    /// The residual payload of a frozen expression node: its fields MINUS `ty`/`tok`, the
    /// child expr and owned pat ids, and the `Var` bound variable id. `anchor` narrows a walked
    /// token to its stored index; `ForTo`'s `identTok` is the one anchor a payload carries.
    let exprPayload
        (anchor: 'tok -> Anchor)
        (boundVar: BoundVarId voption)
        (e: TExprG<FrozenType, 'tok, 'id>)
        : ExprPayload =
        // Per-arm guard-presence flags, the only residual structure a `Match`/`TryWith`
        // records; the arm pats, guards and bodies themselves are stored in the child columns.
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
        | TExprG.ArrayLit _ -> ExprPayload.ArrayLit
        | TExprG.Sequential _ -> ExprPayload.Sequential
        | TExprG.While _ -> ExprPayload.While
        | TExprG.ForTo(identTok = identTok) ->
            ExprPayload.ForTo
                {|
                    Var = introducedBoundVar boundVar
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
        | TExprG.StaticMethodCall(key = key; declArgs = declArgs) ->
            ExprPayload.StaticMethodCall {| Key = key; DeclArgs = declArgs |}
        | TExprG.StaticPropertyGet(key = key; declArgs = declArgs) ->
            ExprPayload.StaticPropertyGet {| Key = key; DeclArgs = declArgs |}
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
        | TExprG.ExternalMember(
            objArg = objArg; key = key; memberName = memberName; storage = storage; argGroupWidths = argGroupWidths) ->
            ExprPayload.ExternalMember
                {|
                    HasObjArg = objArg.IsSome
                    Key = key
                    MemberName = memberName
                    Storage = storage
                    ArgGroupWidths = argGroupWidths
                |}
        | TExprG.Format(sink = sink; segments = segments) ->
            let sink' =
                match sink with
                | FormatSinkG.ToStdOut newline -> FormatSinkShape.ToStdOut newline
                | FormatSinkG.ToStdErr newline -> FormatSinkShape.ToStdErr newline
                | FormatSinkG.ToWriter(newline = newline) -> FormatSinkShape.ToWriter newline
                | FormatSinkG.ToBuilder _ -> FormatSinkShape.ToBuilder
                | FormatSinkG.ToString -> FormatSinkShape.ToString

            // A hole carries an anchor of its own, narrowed here exactly as a node's is.
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
        | TExprG.TraitCall(supportTys = supportTys; memberName = memberName) ->
            ExprPayload.TraitCall
                {|
                    SupportTys = supportTys
                    MemberName = memberName
                |}
        | TExprG.InlineCall(spec = spec; path = path) -> ExprPayload.InlineCall {| Path = path; Spec = spec |}
        | TExprG.CallerExpr(path = path) -> ExprPayload.CallerExpr path

    /// The residual payload of a frozen pattern node: its fields MINUS `ty`/`tok` and the
    /// child sub-pat ids. `boundVar` is `NamedSimple`'s own bound variable, and no other case's.
    let patPayload (boundVar: BoundVarId voption) (p: TPatG<FrozenType, 'tok, 'id>) : PatPayload =
        match p with
        | TPatG.NamedSimple _ -> PatPayload.NamedSimple(introducedBoundVar boundVar)
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
