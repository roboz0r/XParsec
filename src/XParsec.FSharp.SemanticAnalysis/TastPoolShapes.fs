namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer

// What ONE node PROJECTS TO on its way into the columns: the child edges it owns, in the
// order the columns record them, and the residual payload left once `ty`/`tok`/the child
// ids/the bound variable id are lifted out of it. Pure and free of any pool.

module TastPoolShapes =

    /// The immediate child *expressions*, in evaluation order, each flagged as in tail position
    /// of `e`. Sub-patterns are NOT children; composite carriers with no node identity of their
    /// own, namely match arms, format segments and static-opt clauses, are descended into, each
    /// sub-expression once.
    let exprChildEdges (e: TExprG<FrozenType, 'tok, 'id>) : struct (TExprG<FrozenType, 'tok, 'id> * bool)[] =
        let acc = ResizeArray<struct (TExprG<FrozenType, 'tok, 'id> * bool)>()
        let add (x: TExprG<FrozenType, 'tok, 'id>) = acc.Add(struct (x, false))
        let addTail (x: TExprG<FrozenType, 'tok, 'id>) = acc.Add(struct (x, true))

        match e with
        | TExprG.Const _
        | TExprG.Var _
        | TExprG.External _
        | TExprG.Unresolved _
        | TExprG.Null _
        | TExprG.StaticPropertyGet _
        | TExprG.StaticFieldGet _ -> ()
        | TExprG.Lambda(body = body) -> add body
        | TExprG.App(fn = fn; arg = arg) ->
            add fn
            add arg
        | TExprG.Let(value = value; body = body) ->
            add value
            addTail body
        | TExprG.Use(value = value; body = body) ->
            add value
            add body
        | TExprG.IfThenElse(cond = cond; thenExpr = thenExpr; elseExpr = elseExpr) ->
            add cond
            addTail thenExpr
            addTail elseExpr
        | TExprG.Tuple(items = items)
        | TExprG.ArrayLit(elems = items) ->
            for x in items do
                add x
        | TExprG.Sequential(items = items) ->
            let last = items.Length - 1

            items |> EqArray.iteri (fun i x -> if i = last then addTail x else add x)
        | TExprG.While(cond = cond; body = body) ->
            add cond
            add body
        | TExprG.ForTo(startExpr = startExpr; endExpr = endExpr; body = body) ->
            add startExpr
            add endExpr
            add body
        | TExprG.ForIn(source = source; body = body) ->
            add source
            add body
        | TExprG.Match(scrutinee = scrutinee; arms = arms) ->
            add scrutinee

            for arm in arms do
                match arm.Guard with
                | ValueSome g -> add g
                | ValueNone -> ()

                addTail arm.Body
        | TExprG.TryWith(body = body; arms = arms) ->
            add body

            for arm in arms do
                match arm.Guard with
                | ValueSome g -> add g
                | ValueNone -> ()

                add arm.Body
        | TExprG.TryFinally(body = body; cleanup = cleanup) ->
            add body
            add cleanup
        | TExprG.Assignment(lhs = lhs; rhs = rhs) ->
            add lhs
            add rhs
        | TExprG.Range(startExpr = startExpr; step = step; stopExpr = stopExpr) ->
            add startExpr

            match step with
            | Some s -> add s
            | None -> ()

            add stopExpr
        | TExprG.RecordCons(fields = fields) ->
            for (_, v) in fields do
                add v
        | TExprG.RecordClone(source = source; overrides = overrides) ->
            add source

            for (_, v) in overrides do
                add v
        | TExprG.FieldGet(objArg = objArg) -> add objArg
        | TExprG.FieldSet(objArg = objArg; value = value) ->
            add objArg
            add value
        | TExprG.UnionCons(args = args) ->
            for x in args do
                add x
        | TExprG.New(args = args) ->
            for x in args do
                add x
        | TExprG.MethodCall(objArg = objArg; args = args) ->
            add objArg

            for x in args do
                add x
        | TExprG.PropertyGet(objArg = objArg) -> add objArg
        | TExprG.StaticMethodCall(args = args) ->
            for x in args do
                add x
        | TExprG.StaticFieldSet(value = value) -> add value
        | TExprG.ExternalMember(objArg = objArg) ->
            match objArg with
            | ValueSome r -> add r
            | ValueNone -> ()
        | TExprG.Format(sink = sink; segments = segments) ->
            match sink with
            | FormatSinkG.ToWriter(writer = writer) -> add writer
            | FormatSinkG.ToBuilder builder -> add builder
            | FormatSinkG.ToStdOut _
            | FormatSinkG.ToStdErr _
            | FormatSinkG.ToString -> ()

            for seg in segments do
                match seg with
                | FormatSegG.Lit _ -> ()
                | FormatSegG.Hole(_, value) -> add value
                | FormatSegG.DynHole hole ->
                    match hole.Width with
                    | ValueSome w -> add w
                    | ValueNone -> ()

                    match hole.Precision with
                    | ValueSome p -> add p
                    | ValueNone -> ()

                    add hole.Value
                | FormatSegG.CallbackHole(residue = residue) -> add residue
        | TExprG.ILIntrinsic(args = args) ->
            for x in args do
                add x
        | TExprG.StaticOptimization(clauses = clauses; defaultExpr = defaultExpr) ->
            for clause in clauses do
                add clause.Body

            add defaultExpr
        | TExprG.Upcast(source = source) -> add source
        | TExprG.Downcast(source = source) -> add source
        | TExprG.TypeTest(source = source) -> add source
        | TExprG.CallerExpr(body = body) -> add body
        | TExprG.TraitCall(args = args) ->
            for x in args do
                add x
        // The ENTRY is not a child: it is a separate pool root, shared by every call site
        // that references it. Only the call's own argument expressions belong to this node.
        | TExprG.InlineCall(args = args) ->
            for x in args do
                add x

        acc.ToArray()

    /// The immediate child *expressions*, in evaluation order: `exprChildEdges` without the
    /// tail flags.
    let exprChildren (e: TExprG<FrozenType, 'tok, 'id>) : TExprG<FrozenType, 'tok, 'id>[] =
        exprChildEdges e |> Array.map (fun struct (c, _) -> c)

    /// The immediate child *patterns* an expression owns directly, in source order: the
    /// bound variables (`Lambda`/`Let`/`Use`/`ForIn`) and the per-arm scrutinee patterns
    /// (`Match`/`TryWith`). `ForTo`'s loop variable is a field of the node, not a pattern.
    let exprPatChildren (e: TExprG<FrozenType, 'tok, 'id>) : TPatG<FrozenType, 'tok, 'id>[] =
        let acc = ResizeArray<TPatG<FrozenType, 'tok, 'id>>()

        match e with
        | TExprG.Const _
        | TExprG.Var _
        | TExprG.External _
        | TExprG.Unresolved _
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
    /// child expr and owned pat ids, and the `Var` bound variable id. `Let` / `App` take the
    /// `NonRecursive` / `Call` defaults the pooling walk then classifies; `anchor` narrows a
    /// walked token to its stored index, and `ForTo`'s `identTok` is the one anchor carried.
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
        | TExprG.External(key = key) -> ExprPayload.External key
        | TExprG.Unresolved _ -> ExprPayload.Unresolved
        | TExprG.Lambda _ -> ExprPayload.Lambda
        | TExprG.App _ -> ExprPayload.App AppKind.Call
        | TExprG.Let(isRec = isRec) -> ExprPayload.Let(isRec, Recursion.NonRecursive)
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
        | TPatG.NamedSimple(isMutable = isMutable) -> PatPayload.NamedSimple(introducedBoundVar boundVar, isMutable)
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
