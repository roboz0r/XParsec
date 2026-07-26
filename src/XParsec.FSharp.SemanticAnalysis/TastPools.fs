namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Parser

// The interconversion logic filling and draining the id-indexable frozen pools; the pool
// wire-shape types (`ExprPoolId`/`ExprPayload`/`FrozenPools`/…) and the design rationale
// live in `TastPoolTypes.fs`. `toPools` walks the DU assigning each node a dense id and
// recording its child edges as ids; `ofPools` inverts, rebuilding the DU from the columns
// alone. `toPools` is the LAST step of the freeze (`Freeze.run`) and its only production
// caller; `ofPools` has none — it is what makes the interconversion CHECKABLE, and the
// corpus-wide `ofPools ∘ toPools = id` is the proof that the columns carry the whole tree.

[<RequireQualifiedAccess>]
module TastPools =

    // ── the DU vocabulary ───────────────────────────────────────────────────
    //
    // Shape tag and child enumeration read off a `Frozen.*` node. This is the LAST
    // place a frozen DU is walked — every consumer reads the columns — so the tree
    // shape is written down exactly once, here, and the child ORDER these produce IS
    // the order of the `ExprChildren`/`ExprPatChildren`/`PatChildren` columns. Each
    // match is exhaustive with no catch-all: a new `TExprG`/`TPatG`/`TDeclG` case
    // fails to compile rather than silently escaping the pool.

    let exprShape (e: Frozen.TExpr) : ExprShape =
        match e with
        | TExprG.Const _ -> ExprShape.Const
        | TExprG.Var _ -> ExprShape.Var
        | TExprG.External _ -> ExprShape.External
        | TExprG.Lambda _ -> ExprShape.Lambda
        | TExprG.App _ -> ExprShape.App
        | TExprG.Let _ -> ExprShape.Let
        | TExprG.Use _ -> ExprShape.Use
        | TExprG.IfThenElse _ -> ExprShape.IfThenElse
        | TExprG.Tuple _ -> ExprShape.Tuple
        | TExprG.Sequential _ -> ExprShape.Sequential
        | TExprG.While _ -> ExprShape.While
        | TExprG.ForTo _ -> ExprShape.ForTo
        | TExprG.ForIn _ -> ExprShape.ForIn
        | TExprG.Match _ -> ExprShape.Match
        | TExprG.TryWith _ -> ExprShape.TryWith
        | TExprG.TryFinally _ -> ExprShape.TryFinally
        | TExprG.Assignment _ -> ExprShape.Assignment
        | TExprG.Null _ -> ExprShape.Null
        | TExprG.Range _ -> ExprShape.Range
        | TExprG.RecordCons _ -> ExprShape.RecordCons
        | TExprG.RecordClone _ -> ExprShape.RecordClone
        | TExprG.FieldGet _ -> ExprShape.FieldGet
        | TExprG.FieldSet _ -> ExprShape.FieldSet
        | TExprG.UnionCons _ -> ExprShape.UnionCons
        | TExprG.New _ -> ExprShape.New
        | TExprG.MethodCall _ -> ExprShape.MethodCall
        | TExprG.PropertyGet _ -> ExprShape.PropertyGet
        | TExprG.StaticMethodCall _ -> ExprShape.StaticMethodCall
        | TExprG.StaticPropertyGet _ -> ExprShape.StaticPropertyGet
        | TExprG.StaticFieldGet _ -> ExprShape.StaticFieldGet
        | TExprG.StaticFieldSet _ -> ExprShape.StaticFieldSet
        | TExprG.ExternalMember _ -> ExprShape.ExternalMember
        | TExprG.Format _ -> ExprShape.Format
        | TExprG.ILIntrinsic _ -> ExprShape.ILIntrinsic
        | TExprG.StaticOptimization _ -> ExprShape.StaticOptimization
        | TExprG.Upcast _ -> ExprShape.Upcast
        | TExprG.Downcast _ -> ExprShape.Downcast
        | TExprG.TypeTest _ -> ExprShape.TypeTest
        | TExprG.TraitCall _ -> ExprShape.TraitCall

    /// The immediate child *expressions*, in evaluation order. Sub-patterns are NOT
    /// children (see `exprPatChildren`); composite carriers with no node identity of
    /// their own (match arms, format segments, static-opt clauses) are descended into
    /// so every reachable sub-expression appears exactly once.
    let exprChildren (e: Frozen.TExpr) : Frozen.TExpr[] =
        let acc = ResizeArray<Frozen.TExpr>()

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
                | Some g -> acc.Add g
                | None -> ()

                acc.Add arm.Body
        | TExprG.TryWith(body = body; arms = arms) ->
            acc.Add body

            for arm in arms do
                match arm.Guard with
                | Some g -> acc.Add g
                | None -> ()

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
    /// shapes own patterns. `ForTo`'s loop variable is a `NodeKey`, not a pattern, so
    /// it is not a pat child.
    let exprPatChildren (e: Frozen.TExpr) : Frozen.TPat[] =
        let acc = ResizeArray<Frozen.TPat>()

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

    /// Public because the source-arity peel runs on BOTH sides of the freeze: `Freeze`
    /// groups a binding's parameters off the DU spine it has just built, the backends
    /// off the pooled one, and both classify a parameter through the one rule
    /// (`TastLower.argGroupOfParam`) — which needs this tag in the DU domain too.
    let patShape (p: Frozen.TPat) : PatShape =
        match p with
        | TPatG.NamedSimple _ -> PatShape.NamedSimple
        | TPatG.Wildcard _ -> PatShape.Wildcard
        | TPatG.Tuple _ -> PatShape.Tuple
        | TPatG.Const _ -> PatShape.Const
        | TPatG.Record _ -> PatShape.Record
        | TPatG.Union _ -> PatShape.Union
        | TPatG.TypeTestAs _ -> PatShape.TypeTestAs
        | TPatG.Null _ -> PatShape.Null
        | TPatG.EnumCase _ -> PatShape.EnumCase
        | TPatG.Or _ -> PatShape.Or

    /// The immediate sub-patterns, in source order (patterns own no child expressions).
    let patChildren (p: Frozen.TPat) : Frozen.TPat[] =
        let acc = ResizeArray<Frozen.TPat>()

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

    let declShape (d: Frozen.TDecl) : DeclShape =
        match d with
        | TDeclG.Let _ -> DeclShape.Let
        | TDeclG.Expression _ -> DeclShape.Expression
        | TDeclG.Type _ -> DeclShape.Type

    /// The residual payload of a frozen expression node — its fields MINUS `ty`/`tok`, the
    /// child expr ids (`exprChildren`), the owned pat ids (`exprPatChildren`), and the `Var`
    /// binder id. The exact inverse of `substituteExpr`, mirroring `FrozenCodec.writeExprPayload`
    /// for what each case emits beyond those. Exhaustive on the DU with no catch-all, so a
    /// new `TExprG` case fails to compile here.
    let private exprPayload (e: Frozen.TExpr) : ExprPayload =
        // Per-arm guard-presence flags — the only residual structure a `Match`/`TryWith`
        // records (the arm pats/guards/bodies themselves ride the child columns); this is
        // what `substituteExpr.buildArms` re-nests them by.
        let armGuards (arms: EqArray<Frozen.TMatchArm>) =
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
        | TExprG.ForTo(var = var; identTok = identTok) -> ExprPayload.ForTo {| Var = var; IdentTok = identTok |}
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
    /// compile here.
    let private patPayload (p: Frozen.TPat) : PatPayload =
        match p with
        | TPatG.NamedSimple(binding = binding) -> PatPayload.NamedSimple binding
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

    /// Where a pooling walk PUTS the rows it produces. The walk itself — which nodes
    /// exist, in what order, and which child edges they carry — is `poolExpr`/`poolPat`/
    /// `poolDecl` below and exists exactly once; a sink decides only where a row lands and
    /// how a binder id is assigned. `toPools` fills a fresh pool with one; an overlay
    /// builder appends to a stacked one with another.
    type PoolSink =
        {
            /// Called for every binder a walked node INTRODUCES (a `NamedSimple` pattern's
            /// binding, a `ForTo` loop variable), before the node's row is added.
            InternBinder: NodeKey -> unit
            AddExpr: ExprRow -> ExprPoolId
            AddPat: PatRow -> PatPoolId
            AddDecl: DeclRow -> DeclPoolId
            /// Called with each pooled expr node's row, the binder `NodeKey` it
            /// references when it is a `Var` (`ValueNone` at every other shape), and the
            /// id its row took. The hook for the id-keyed records a row cannot carry: a
            /// `Var`'s binder reference (which the walk leaves `ValueNone`, binder ids
            /// being the sink's to assign) and a lambda's entry in the lambda id space
            /// (recoverable from the row's `Tok`). The DU node is deliberately NOT
            /// passed: a sink must be expressible without one, since the columns are
            /// Node-sufficient.
            OnExprPooled: ExprRow -> NodeKey voption -> ExprPoolId -> unit
        }

    /// Pool a pattern subtree post-order: a node's children are pooled before the node
    /// itself, so every child id its row names already resolves.
    let rec poolPat (sink: PoolSink) (p: Frozen.TPat) : PatPoolId =
        match TastWalk.patBinder p with
        | ValueSome k -> sink.InternBinder k
        | ValueNone -> ()

        let kids = patChildren p |> Array.map (poolPat sink)

        sink.AddPat
            {
                Shape = patShape p
                Ty = TastWalk.patTy p
                Tok = TastWalk.patTok p
                Children = kids
                Payload = patPayload p
            }

    /// Pool an expression subtree post-order (see `poolPat`), its owned sub-patterns
    /// included.
    let rec poolExpr (sink: PoolSink) (e: Frozen.TExpr) : ExprPoolId =
        // A `ForTo` binds its loop variable with no pattern node behind it, so the
        // intern cannot ride `poolPat`.
        let varBinding =
            match e with
            | TExprG.ForTo(var = var) ->
                sink.InternBinder var
                ValueNone
            | TExprG.Var(binding = binding) -> ValueSome binding
            | _ -> ValueNone

        let exprKids = exprChildren e |> Array.map (poolExpr sink)
        let patKids = exprPatChildren e |> Array.map (poolPat sink)

        let row =
            {
                Shape = exprShape e
                Ty = TastWalk.exprTy e
                Tok = TastWalk.exprTok e
                Children = exprKids
                PatChildren = patKids
                VarBinder = ValueNone
                Payload = exprPayload e
            }

        let id = sink.AddExpr row
        sink.OnExprPooled row varBinding id
        id

    /// The residual payload of a frozen declaration node — its fields MINUS the child
    /// expr/pat roots. The exact inverse of `substituteDecl`. Exhaustive with no catch-all,
    /// so a new `TDecl` case fails to compile here.
    ///
    /// A `Type` decl is where the payload does real work: its member/preamble/ctor bodies
    /// are pooled through the sink and the declaration keeps their IDS in the slots that
    /// held the trees (`PooledTypeDecl`). `TastConvert.typeDecl` supplies the traversal —
    /// the same one the `'ty` freeze runs — so the seven body slots are enumerated in one
    /// place. Its pattern-less binder slots (`this`, member/ctor parameters, ctor locals)
    /// are interned FIRST, because a body may name any of them by `Var` and a reference
    /// whose binder was never interned faults in `toPools`.
    let private declPayload (sink: PoolSink) (d: Frozen.TDecl) : DeclPayload =
        match d with
        | TDeclG.Let(isInline = isInline; ty = ty) -> DeclPayload.Let {| IsInline = isInline; Ty = ty |}
        | TDeclG.Expression(ty = ty) -> DeclPayload.Expression ty
        | TDeclG.Type td ->
            for k in TTypeDeclG.boundKeys td do
                sink.InternBinder k

            DeclPayload.Type(TastConvert.typeDecl id (poolExpr sink) td)

    /// Pool a declaration, its expr/pat roots (see `poolPat`) and — for a `Type` decl —
    /// its member bodies, which the payload names by id rather than surfacing as children.
    let poolDecl (sink: PoolSink) (d: Frozen.TDecl) : DeclPoolId =
        let struct (exprKids, patKids) =
            match d with
            | TDeclG.Let(binding = binding; value = value) ->
                struct ([| poolExpr sink value |], [| poolPat sink binding |])
            | TDeclG.Expression(expr = expr) -> struct ([| poolExpr sink expr |], [||])
            | TDeclG.Type _ -> struct ([||], [||])

        sink.AddDecl
            {
                Shape = declShape d
                ExprChildren = exprKids
                PatChildren = patKids
                Payload = declPayload sink d
            }

    /// Pool the frozen tree of `file.Decls`, assigning each reachable node a dense id
    /// and recording its child edges as ids.
    let toPools (file: Frozen.TastFile) : FrozenPools =
        // The expression pool as parallel column builders (struct-of-arrays); all are
        // appended together per node so they stay index-aligned by `ExprPoolId`.
        let exprShapes = ResizeArray<ExprShape>()
        let exprTys = ResizeArray<FrozenType>()
        let exprToks = ResizeArray<SyntaxToken>()
        let exprChildrenCol = ResizeArray<ExprPoolId[]>()
        let exprPatChildrenCol = ResizeArray<PatPoolId[]>()
        let exprPayloads = ResizeArray<ExprPayload>()

        // Each `Var`'s expr id + its binding NodeKey, captured in pass 1 and resolved to a
        // `BinderId` in pass 2 — a `Var` may name a binder pooled after it (a forward /
        // mutually-recursive reference), so the enumeration must complete first.
        let varBindings = ResizeArray<struct (int * NodeKey)>()

        // The pattern pool as parallel column builders (struct-of-arrays), index-aligned by
        // `PatPoolId`.
        let patShapes = ResizeArray<PatShape>()
        let patTys = ResizeArray<FrozenType>()
        let patToks = ResizeArray<SyntaxToken>()
        let patChildrenCol = ResizeArray<PatPoolId[]>()
        let patPayloads = ResizeArray<PatPayload>()

        // The declaration pool as parallel column builders, index-aligned by `DeclPoolId`.
        let declShapes = ResizeArray<DeclShape>()
        let declExprChildrenCol = ResizeArray<ExprPoolId[]>()
        let declPatChildrenCol = ResizeArray<PatPoolId[]>()
        let declPayloads = ResizeArray<DeclPayload>()

        // The binder pool as two parallel columns: each distinct NodeKey a definition site
        // introduces, interned to a dense `BinderId` on first encounter, alongside the
        // naming triple sourced from that same key. The introducing sites are enumerated
        // off the accessor (pattern / loop binders) and off `TTypeDeclG.boundKeys` (a type
        // declaration's pattern-less key slots) as the trees are walked, so nothing
        // re-derives which nodes bind.
        //
        // The enumeration spans the whole FILE, because a side table may key on a binder in
        // any of its trees — and every tree the file bears is now pooled, so ONE walk covers
        // them all. Correspondingly a `BinderId` need not have a pooled pattern node behind
        // it (a `this` slot has none anywhere) — the binder space is dense and independent,
        // inverted by position.
        let binderKeys = ResizeArray<NodeKey>()
        let binderNamings = ResizeArray<BinderNaming>()
        let binderIds = System.Collections.Generic.Dictionary<NodeKey, BinderId>()

        // The lambda id space: a source lambda's dense id IS its `ExprPoolId` (positional
        // — every `Lambda` expr is already in `Exprs`). `FunVerdicts`, the one side table
        // keyed by a lambda-EXPRESSION key rather than a binder, resolves against this map;
        // it is recorded under the SAME `TastWalk.lambdaKey` codegen looks the verdict up
        // by, so the pool key space matches the DU lookup key by construction.
        let lambdaIds = System.Collections.Generic.Dictionary<NodeKey, ExprPoolId>()

        let internBinder (k: NodeKey) : unit =
            match binderIds.TryGetValue k with
            | true, _ -> ()
            | false, _ ->
                binderIds.Add(k, BinderId binderKeys.Count)
                binderKeys.Add k
                // The naming slot stays aligned with `binderKeys` by appending in lockstep.
                binderNamings.Add(BinderNaming.ofKey k)

        // The sink: rows land at the end of the column builders, so a node's id is the
        // count at the moment it is added. `ExprRow.VarBinder` is dropped here — the `Var`
        // reference edge cannot resolve until the binder enumeration is complete, so it is
        // recorded as pending and filled by the second pass below.
        let sink: PoolSink =
            {
                InternBinder = internBinder
                AddExpr =
                    fun row ->
                        let id = exprShapes.Count
                        exprShapes.Add row.Shape
                        exprTys.Add row.Ty
                        exprToks.Add row.Tok
                        exprChildrenCol.Add row.Children
                        exprPatChildrenCol.Add row.PatChildren
                        exprPayloads.Add row.Payload
                        ExprPoolId id
                AddPat =
                    fun row ->
                        let id = patShapes.Count
                        patShapes.Add row.Shape
                        patTys.Add row.Ty
                        patToks.Add row.Tok
                        patChildrenCol.Add row.Children
                        patPayloads.Add row.Payload
                        PatPoolId id
                AddDecl =
                    fun row ->
                        let id = declShapes.Count
                        declShapes.Add row.Shape
                        declExprChildrenCol.Add row.ExprChildren
                        declPatChildrenCol.Add row.PatChildren
                        declPayloads.Add row.Payload
                        DeclPoolId id
                // A `Var`'s binder reference resolves in pass 2 (see `varBindings`); a
                // lambda's positional identity is its slot, stamped so `FunVerdicts`
                // (lambda-expression-keyed) resolves onto it.
                OnExprPooled =
                    fun row varBinding (ExprPoolId id) ->
                        match row.Shape with
                        | ExprShape.Var -> varBindings.Add(struct (id, varBinding.Value))
                        // The same key `TastWalk.lambdaKey` computes, off the row's token.
                        | ExprShape.Lambda -> lambdaIds.[NodeKey.ofToken row.Tok NodeKind.ExprLambda] <- ExprPoolId id
                        | _ -> ()
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

        // A binding's `ValRepr` tuple-group patterns, pooled through the same walk. They
        // are STRUCTURALLY the lambda-spine pats `peelValRepr` read off the frozen tree,
        // but they take their own pool entries rather than resolving to the tree's: the
        // build keeps no node→id index (identity after freeze is positional, and a `ValRepr`
        // is derived, not shared), so re-pooling is what keeps the pat columns the single
        // home for every pattern. Done HERE and not in the record below: `Map.map` is eager
        // and appends to the pat/binder builders, which the record's earlier fields have
        // already snapshotted by the time it would run there.
        let pooledValReprs =
            file.BindingValReprs
            |> Map.map (fun _ vr -> TastConvert.valRepr id (poolPat sink) vr)

        // Resolve a reference/side-table key to the binder it names. A miss means the
        // referent was introduced by no definition site the walk covers — an incomplete
        // binder enumeration, which is exactly the failure the id-resolution gate exists
        // to surface.
        let binderIdOf (referent: string) (k: NodeKey) : BinderId =
            match binderIds.TryGetValue k with
            | true, id -> id
            | false, _ ->
                failwithf "TastPools.toPools: %s key %O references a binder no definition site introduced" referent k

        // The lambda-key analogue: a `FunVerdicts` key that names no pooled lambda is the
        // honest failure a lambda-keyed entry naming no pooled lambda should be.
        let lambdaIdOf (referent: string) (k: NodeKey) : ExprPoolId =
            match lambdaIds.TryGetValue k with
            | true, id -> id
            | false, _ -> failwithf "TastPools.toPools: %s key %O names no pooled lambda" referent k

        // Second pass: now the enumeration is complete, route each `Var`'s reference edge
        // to its binder's dense id — the sparse `ExprVarBinder` column (`ValueNone` at
        // every non-`Var` slot).
        let exprVarBinder: BinderId voption[] = Array.create exprShapes.Count ValueNone

        for (struct (id, key)) in varBindings do
            exprVarBinder.[id] <- ValueSome(binderIdOf "Var" key)

        // One generic remap over the side tables, parameterized by the key resolver: the
        // binder-keyed tables pass `binderIdOf`, `FunVerdicts` passes `lambdaIdOf`. A second
        // resolver, not a second remap, so the two id spaces share one enumeration. The
        // table's NAME rides along so an unresolvable key says which table holds it.
        let remapSideTable (name: string) (resolve: string -> NodeKey -> 'id) (m: Map<NodeKey, 'v>) : ('id * 'v)[] =
            m |> Map.toArray |> Array.map (fun (k, v) -> resolve name k, v)

        {
            ExprShapes = exprShapes.ToArray()
            ExprTys = exprTys.ToArray()
            ExprToks = exprToks.ToArray()
            ExprChildren = exprChildrenCol.ToArray()
            ExprPatChildren = exprPatChildrenCol.ToArray()
            ExprVarBinder = exprVarBinder
            ExprPayloads = exprPayloads.ToArray()
            PatShapes = patShapes.ToArray()
            PatTys = patTys.ToArray()
            PatToks = patToks.ToArray()
            PatChildren = patChildrenCol.ToArray()
            PatPayloads = patPayloads.ToArray()
            DeclShapes = declShapes.ToArray()
            DeclExprChildren = declExprChildrenCol.ToArray()
            DeclPatChildren = declPatChildrenCol.ToArray()
            DeclPayloads = declPayloads.ToArray()
            Roots = roots
            InlineTemplates = inlineTemplates
            BinderKeys = binderKeys.ToArray()
            BinderNamings = binderNamings.ToArray()
            Residue =
                {
                    Diagnostics = file.Diagnostics
                    IntrinsicReprKeys = file.IntrinsicReprKeys
                    Accessibility = file.Accessibility
                }
            ModuleMembers = remapSideTable "ModuleMembers" binderIdOf file.ModuleMembers
            TopLevelNames = remapSideTable "TopLevelNames" binderIdOf file.TopLevelNames
            ClosureReprs = remapSideTable "ClosureReprs" binderIdOf file.ClosureReprs
            FunVerdicts = remapSideTable "FunVerdicts" lambdaIdOf file.FunVerdicts
            GenericFnSchemes = remapSideTable "GenericFnSchemes" binderIdOf file.GenericFnSchemes
            BindingValReprs = remapSideTable "BindingValReprs" binderIdOf pooledValReprs
            BindingTyparArities = remapSideTable "BindingTyparArities" binderIdOf file.BindingTyparArities
        }

    // ── the inverse: rebuild the DU trees from the pools ────────────────────
    //
    // `substituteExpr` re-authors a node from its columns — `ty`/`tok`, the resolved
    // `Var` binder, the `ExprPayload` residual scalars/structure — and the ALREADY-REBUILT
    // child subtrees, with NO template node (the expr pool holds none). The children are
    // consumed in the exact order `TastPools.exprChildren`/`exprPatChildren` enumerated
    // them (`nextE`/`nextP` are order cursors) — the one coupling the round-trip gate
    // proves. The match on `ExprPayload` is exhaustive with no catch-all (the inverse of
    // `exprPayload`), so a new shape fails to compile here. `substitutePat`/`substituteDecl`
    // re-author from their own payload + rebuilt children the same way — no template node,
    // now that the pat and decl pools are columnar too.
    //
    // They are the NODE-level inverse, and public because two walks drive them: `ofPools`
    // below (every root of a whole pool) and `TastPoolBuilder`'s subtree drain (one node
    // id across the base/overlay stack), which serves the one channel whose far end is
    // still DU-typed — a package's inline template.

    let substituteExpr
        (ty: FrozenType)
        (tok: SyntaxToken)
        (varBinding: NodeKey voption)
        (payload: ExprPayload)
        (es: Frozen.TExpr[])
        (ps: Frozen.TPat[])
        : Frozen.TExpr =
        let mutable ei = 0
        let mutable pi = 0

        let nextE () =
            let x = es.[ei] in
            ei <- ei + 1
            x

        let nextP () =
            let x = ps.[pi] in
            pi <- pi + 1
            x

        // Re-nest the flat child columns into `Match`/`TryWith` arms: each arm draws its
        // pat, then its optional guard (present per `guardPresent`), then its body — the
        // exact order `TastPools.exprChildren`/`exprPatChildren` enumerated them, which
        // `exprPayload.armGuards` recorded the flags for. `Match`'s scrutinee / `TryWith`'s
        // body are consumed by the caller BEFORE this, so the cursors are already advanced.
        let buildArms (guardPresent: bool[]) : EqArray<Frozen.TMatchArm> =
            guardPresent
            |> Array.map (fun hasGuard ->
                let pat = nextP ()
                let guard = if hasGuard then Some(nextE ()) else None
                let body = nextE ()

                {
                    Pat = pat
                    Guard = guard
                    Body = body
                }
            )
            |> EqArray.ofArray

        match payload with
        // `binding` is supplied from the dense id, so the round-trip exercises the remap.
        | ExprPayload.Var ->
            match varBinding with
            | ValueSome k -> TExprG.Var(k, ty, tok)
            | ValueNone -> failwith "TastPools.ofPools: a Var entry carries no resolved binder id"
        | ExprPayload.Const value -> TExprG.Const(value, ty, tok)
        | ExprPayload.External p -> TExprG.External(p.CompiledName, p.Key, ty, tok)
        | ExprPayload.Null -> TExprG.Null(ty, tok)
        | ExprPayload.StaticPropertyGet key -> TExprG.StaticPropertyGet(key, ty, tok)
        | ExprPayload.StaticFieldGet p -> TExprG.StaticFieldGet(p.DeclKey, p.FieldName, ty, tok)
        | ExprPayload.Lambda ->
            let param = nextP ()
            let body = nextE ()
            TExprG.Lambda(param, body, ty, tok)
        | ExprPayload.App ->
            let fn = nextE ()
            let arg = nextE ()
            TExprG.App(fn, arg, ty, tok)
        | ExprPayload.Let ->
            let binding = nextP ()
            let value = nextE ()
            let body = nextE ()
            TExprG.Let(binding, value, body, ty, tok)
        | ExprPayload.Use dispose ->
            let binding = nextP ()
            let value = nextE ()
            let body = nextE ()
            TExprG.Use(binding, value, body, dispose, ty, tok)
        | ExprPayload.IfThenElse ->
            let cond = nextE ()
            let thenExpr = nextE ()
            let elseExpr = nextE ()
            TExprG.IfThenElse(cond, thenExpr, elseExpr, ty, tok)
        | ExprPayload.Tuple -> TExprG.Tuple(EqArray.ofArray es, ty, tok)
        | ExprPayload.Sequential -> TExprG.Sequential(EqArray.ofArray es, ty, tok)
        | ExprPayload.While ->
            let cond = nextE ()
            let body = nextE ()
            TExprG.While(cond, body, ty, tok)
        | ExprPayload.ForTo p ->
            let startExpr = nextE ()
            let endExpr = nextE ()
            let body = nextE ()
            TExprG.ForTo(p.Var, p.IdentTok, startExpr, endExpr, body, ty, tok)
        | ExprPayload.ForIn enumerator ->
            let pat = nextP ()
            let source = nextE ()
            let body = nextE ()
            TExprG.ForIn(pat, source, body, enumerator, ty, tok)
        | ExprPayload.Match guardPresent ->
            let scrutinee = nextE ()
            TExprG.Match(scrutinee, buildArms guardPresent, ty, tok)
        | ExprPayload.TryWith guardPresent ->
            let body = nextE ()
            TExprG.TryWith(body, buildArms guardPresent, ty, tok)
        | ExprPayload.TryFinally ->
            let body = nextE ()
            let cleanup = nextE ()
            TExprG.TryFinally(body, cleanup, ty, tok)
        | ExprPayload.Assignment ->
            let lhs = nextE ()
            let rhs = nextE ()
            TExprG.Assignment(lhs, rhs, ty, tok)
        | ExprPayload.Range hasStep ->
            let startExpr = nextE ()
            let step' = if hasStep then Some(nextE ()) else None
            let stopExpr = nextE ()
            TExprG.Range(startExpr, step', stopExpr, ty, tok)
        | ExprPayload.RecordCons fieldNames ->
            let fields' =
                fieldNames |> Array.map (fun name -> (name, nextE ())) |> EqArray.ofArray

            TExprG.RecordCons(fields', ty, tok)
        | ExprPayload.RecordClone overrideNames ->
            let source = nextE ()

            let overrides' =
                overrideNames |> Array.map (fun name -> (name, nextE ())) |> EqArray.ofArray

            TExprG.RecordClone(source, overrides', ty, tok)
        | ExprPayload.FieldGet fieldName ->
            let receiver = nextE ()
            TExprG.FieldGet(receiver, fieldName, ty, tok)
        | ExprPayload.FieldSet fieldName ->
            let receiver = nextE ()
            let value = nextE ()
            TExprG.FieldSet(receiver, fieldName, value, ty, tok)
        | ExprPayload.UnionCons caseName -> TExprG.UnionCons(caseName, EqArray.ofArray es, ty, tok)
        | ExprPayload.New p -> TExprG.New(p.ClassName, p.Key, EqArray.ofArray es, ty, tok)
        | ExprPayload.MethodCall p ->
            let receiver = nextE ()
            // The remaining `es` (after the receiver) are exactly the args, in order.
            let args = es.[ei..] |> EqArray.ofArray
            TExprG.MethodCall(receiver, p.Key, p.Via, args, ty, tok)
        | ExprPayload.PropertyGet p ->
            let receiver = nextE ()
            TExprG.PropertyGet(receiver, p.Key, p.Via, ty, tok)
        | ExprPayload.StaticMethodCall key -> TExprG.StaticMethodCall(key, EqArray.ofArray es, ty, tok)
        | ExprPayload.StaticFieldSet p ->
            let value = nextE ()
            TExprG.StaticFieldSet(p.DeclKey, p.FieldName, value, ty, tok)
        | ExprPayload.ExternalMember p ->
            let receiver' = if p.HasReceiver then ValueSome(nextE ()) else ValueNone
            TExprG.ExternalMember(receiver', p.Key, p.MemberName, p.Storage, ty, tok)
        | ExprPayload.Format p ->
            // The sink child (writer / builder) is consumed BEFORE the segment children —
            // the order `exprChildren` yields, which the segment loop then continues.
            let sink' =
                match p.Sink with
                | FormatSinkShape.ToWriter newline -> FormatSinkG.ToWriter(nextE (), newline)
                | FormatSinkShape.ToBuilder -> FormatSinkG.ToBuilder(nextE ())
                | FormatSinkShape.ToStdOut newline -> FormatSinkG.ToStdOut newline
                | FormatSinkShape.ToStdErr newline -> FormatSinkG.ToStdErr newline
                | FormatSinkShape.ToString -> FormatSinkG.ToString

            let segments' =
                p.Segments
                |> Array.map (fun seg ->
                    match seg with
                    | FormatSegShape.Lit s -> FormatSegG.Lit s
                    | FormatSegShape.Hole spec -> FormatSegG.Hole(spec, nextE ())
                    | FormatSegShape.DynHole(hasWidth, hasPrecision, spec) ->
                        let width = if hasWidth then ValueSome(nextE ()) else ValueNone
                        let precision = if hasPrecision then ValueSome(nextE ()) else ValueNone
                        let value = nextE ()

                        FormatSegG.DynHole
                            {
                                Width = width
                                Precision = precision
                                Spec = spec
                                Value = value
                            }
                    | FormatSegShape.CallbackHole spec -> FormatSegG.CallbackHole(spec, nextE ())
                )
                |> EqArray.ofArray

            TExprG.Format(sink', segments', ty, tok)
        | ExprPayload.ILIntrinsic p -> TExprG.ILIntrinsic(p.OpCode, p.TypeOperand, EqArray.ofArray es, ty, tok)
        | ExprPayload.StaticOptimization clauseConstraints ->
            let clauses' =
                clauseConstraints
                |> Array.map (fun constraints ->
                    {
                        Constraints = constraints
                        Body = nextE ()
                    }
                )
                |> EqArray.ofArray

            let defaultExpr = nextE ()
            TExprG.StaticOptimization(clauses', defaultExpr, ty, tok)
        | ExprPayload.Upcast -> TExprG.Upcast(nextE (), ty, tok)
        | ExprPayload.Downcast -> TExprG.Downcast(nextE (), ty, tok)
        | ExprPayload.TypeTest testTy ->
            let source = nextE ()
            TExprG.TypeTest(source, testTy, ty, tok)
        | ExprPayload.TraitCall p -> TExprG.TraitCall(p.Receiver, p.MemberName, EqArray.ofArray es, ty, tok)

    let substitutePat (ty: FrozenType) (tok: SyntaxToken) (payload: PatPayload) (ps: Frozen.TPat[]) : Frozen.TPat =
        match payload with
        // `binding` is supplied from the payload, the pat analogue of `ForTo.var` — it is
        // interned so `Var` references resolve, yet reconstructed verbatim from here.
        | PatPayload.NamedSimple binding -> TPatG.NamedSimple(binding, ty, tok)
        | PatPayload.Wildcard -> TPatG.Wildcard(ty, tok)
        | PatPayload.Null -> TPatG.Null(ty, tok)
        | PatPayload.Const value -> TPatG.Const(value, ty, tok)
        | PatPayload.EnumCase p -> TPatG.EnumCase(p.EnumKey, p.CaseName, ty, tok)
        | PatPayload.Tuple -> TPatG.Tuple(EqArray.ofArray ps, ty, tok)
        | PatPayload.Or -> TPatG.Or(EqArray.ofArray ps, ty, tok)
        | PatPayload.Union caseName -> TPatG.Union(caseName, EqArray.ofArray ps, ty, tok)
        | PatPayload.TypeTestAs testTy -> TPatG.TypeTestAs(testTy, ps.[0], ty, tok)
        | PatPayload.Record fieldNames ->
            // The field names pair off with the sub-pat children in the SAME order
            // `patChildren` enumerated the record's fields.
            let fields' =
                Array.map2 (fun name sub -> (name, sub)) fieldNames ps |> EqArray.ofArray

            TPatG.Record(fields', ty, tok)

    /// A `Type` decl's bodies are named by id INSIDE the payload's declaration shape (not
    /// by the child columns), so this direction needs the id→expr resolver too — the same
    /// `TastConvert.typeDecl` traversal, run at the inverse body mapping.
    let substituteDecl
        (fromExpr: ExprPoolId -> Frozen.TExpr)
        (payload: DeclPayload)
        (es: Frozen.TExpr[])
        (ps: Frozen.TPat[])
        : Frozen.TDecl =
        match payload with
        | DeclPayload.Let p -> TDeclG.Let(ps.[0], es.[0], p.IsInline, p.Ty)
        | DeclPayload.Expression ty -> TDeclG.Expression(es.[0], ty)
        | DeclPayload.Type td -> TDeclG.Type(TastConvert.typeDecl id fromExpr td)

    /// A dense `BinderId`-keyed side table as the `Map<NodeKey,_>` it was re-keyed FROM,
    /// resolving each id back through the binder column. Public because two callers need
    /// it and neither should re-derive the resolution: `ofPools` (rebuilding the whole
    /// file) and a consumer whose own downstream API is still `NodeKey`-keyed and so
    /// cannot take the id form (`Layout.buildUnit` feeding the CLR holder plan / closure
    /// discovery). Prefer the id form where the consumer holds a `BinderId` — this
    /// direction re-admits keys that name nothing.
    let binderKeyedMap (pools: FrozenPools) (dense: (BinderId * 'v)[]) : Map<NodeKey, 'v> =
        dense
        |> Array.map (fun (BinderId i, v) -> pools.BinderKeys.[i], v)
        |> Map.ofArray

    /// Rebuild the `Frozen.TastFile` DU from the pools — the inverse of `toPools`. The
    /// `Decls` are re-authored from the pool roots and the side tables re-keyed back
    /// through the binder/lambda id spaces; only the three `Residue` fields are carried
    /// through verbatim, having no pooled form.
    ///
    /// It has NO production caller: the freeze yields pools, every consumer reads pools,
    /// and `FrozenCodec` stores pools. What it exists for is the OBLIGATION the pools owe
    /// — that the columns are tree-sufficient. `ofPools (toPools f) = f` structurally,
    /// over the whole corpus, is the proof that nothing of the tree was lost on the way
    /// into the columns, and it is checkable only because the DU is still expressible.
    /// (Corpus gates: `TastPoolsTests`, `Codegen.Clr.Tests/TestHelpers.fs`'s
    /// `PoolRoundTripped` codegen-invariance.) It is also the drain a test reaches for
    /// when asserting on whole decl trees, which the accessor's per-node reads do not
    /// serve.
    let ofPools (pools: FrozenPools) : Frozen.TastFile =
        // Resolve a dense id back to the binder NodeKey it names — the inverse of the
        // `toPools` interning. This is the resolution the reference remap and the side
        // tables both invert through.
        let binderKey (BinderId i) : NodeKey = pools.BinderKeys.[i]

        // The inverse of the lambda id space: a lambda's `ExprPoolId` back to the `NodeKey`
        // codegen looks its verdict up under. With the Node gone, recompute that key from
        // the lambda's `ExprToks` column — the same `NodeKey.ofToken … ExprLambda`
        // `TastWalk.lambdaKey` computes, and the construction `toPools` keyed it by.
        let lambdaKeyOf (ExprPoolId i) : NodeKey =
            NodeKey.ofToken pools.ExprToks.[i] NodeKind.ExprLambda

        let rec fromPat (PatPoolId i) : Frozen.TPat =
            let ps = pools.PatChildren.[i] |> Array.map fromPat
            substitutePat pools.PatTys.[i] pools.PatToks.[i] pools.PatPayloads.[i] ps

        let rec fromExpr (ExprPoolId i) : Frozen.TExpr =
            let es = pools.ExprChildren.[i] |> Array.map fromExpr
            let ps = pools.ExprPatChildren.[i] |> Array.map fromPat
            let varBinding = pools.ExprVarBinder.[i] |> ValueOption.map binderKey
            substituteExpr pools.ExprTys.[i] pools.ExprToks.[i] varBinding pools.ExprPayloads.[i] es ps

        let fromDecl (DeclPoolId i) : Frozen.TDecl =
            let es = pools.DeclExprChildren.[i] |> Array.map fromExpr
            let ps = pools.DeclPatChildren.[i] |> Array.map fromPat
            substituteDecl fromExpr pools.DeclPayloads.[i] es ps

        let decls = pools.Roots |> Array.map fromDecl |> EqArray.ofArray

        // The vocabulary rebuilds from its own roots — a distinct tree from the emitted
        // function of the same name, never re-derived from it.
        let inlineBodies =
            pools.InlineTemplates
            |> Array.map (fun t ->
                {
                    Key = t.Key
                    Body =
                        {
                            Decl = fromDecl t.Decl
                            ParamAttrs = t.ParamAttrs
                        }
                }
            )
            |> EqArray.ofArray

        // Rebuild a side table from its dense form, resolving each `BinderId` back to its
        // NodeKey. Reconstructing the maps here (rather than retaining the source file's) is
        // what makes the round-trip prove the key remap, not just the decl trees.
        // The binder-keyed tables go through the shared `binderKeyedMap`; `FunVerdicts` is
        // the one table on the lambda id space, so it inverts through `lambdaKeyOf`.
        let rebuildSideTable (resolve: 'id -> NodeKey) (dense: ('id * 'v)[]) : Map<NodeKey, 'v> =
            dense |> Array.map (fun (id, v) -> resolve id, v) |> Map.ofArray

        {
            Decls = decls
            Diagnostics = pools.Residue.Diagnostics
            IntrinsicReprKeys = pools.Residue.IntrinsicReprKeys
            ModuleMembers = binderKeyedMap pools pools.ModuleMembers
            TopLevelNames = binderKeyedMap pools pools.TopLevelNames
            ClosureReprs = binderKeyedMap pools pools.ClosureReprs
            FunVerdicts = rebuildSideTable lambdaKeyOf pools.FunVerdicts
            GenericFnSchemes = binderKeyedMap pools pools.GenericFnSchemes
            InlineBodies = inlineBodies
            Accessibility = pools.Residue.Accessibility
            // Both halves invert: the key through the binder pool, the tuple-group pats
            // through the pat columns.
            BindingValReprs =
                pools.BindingValReprs
                |> Array.map (fun (binder, vr) -> binderKey binder, TastConvert.valRepr id fromPat vr)
                |> Map.ofArray
            BindingTyparArities = binderKeyedMap pools pools.BindingTyparArities
        }
