namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Parser

// The interconversion logic filling and draining the id-indexable frozen pools; the pool
// wire-shape types (`ExprPoolId`/`ExprPayload`/`FrozenPools`/…) and the design rationale
// live in `TastPoolTypes.fs`. `toPools` walks the DU assigning each node a dense id and
// recording its child edges as ids; `ofPools` inverts, rebuilding the DU from the columns
// alone. They are proven INTERCONVERTIBLE over the corpus, and they are the seam the STORED
// wire form sits on: `FrozenCodec.flatten` is `toPools` then the column writers, `thaw` their
// inverse then `ofPools`.

[<RequireQualifiedAccess>]
module TastPools =

    /// The residual payload of a frozen expression node — its fields MINUS `ty`/`tok`, the
    /// child expr ids (`exprChildren`), the owned pat ids (`exprPatChildren`), and the `Var`
    /// binder id. The exact inverse of `substituteExpr`, mirroring `FrozenCodec.writeExpr`
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
    /// `FrozenCodec.writePat`. Exhaustive with no catch-all, so a new `TPat` case fails to
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

    /// The residual payload of a frozen declaration node — its fields MINUS the child
    /// expr/pat roots. The exact inverse of `substituteDecl`, mirroring `FrozenCodec.writeDecl`.
    /// Exhaustive with no catch-all, so a new `TDecl` case fails to compile here.
    let private declPayload (d: Frozen.TDecl) : DeclPayload =
        match d with
        | TDeclG.Let(isInline = isInline; ty = ty) -> DeclPayload.Let {| IsInline = isInline; Ty = ty |}
        | TDeclG.Expression(ty = ty) -> DeclPayload.Expression ty
        | TDeclG.Type td -> DeclPayload.Type td

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
            /// Called with each expr node and the id its row took. The hook for the
            /// id-keyed records a row cannot carry: a `Var`'s binder reference (which the
            /// walk leaves `ValueNone`, binder ids being the sink's to assign) and a
            /// lambda's entry in the lambda id space.
            OnExprPooled: Frozen.TExpr -> ExprPoolId -> unit
        }

    /// Pool a pattern subtree post-order: a node's children are pooled before the node
    /// itself, so every child id its row names already resolves. The child enumeration is
    /// the accessor's — no tree-shape knowledge is duplicated here.
    let rec poolPat (sink: PoolSink) (p: Frozen.TPat) : PatPoolId =
        match TastAccessor.patBinder p with
        | ValueSome k -> sink.InternBinder k
        | ValueNone -> ()

        let kids = TastAccessor.patChildren p |> Array.map (poolPat sink)

        sink.AddPat
            {
                Shape = TastAccessor.patKind p
                Ty = TastAccessor.patTy p
                Tok = TastAccessor.patTok p
                Children = kids
                Payload = patPayload p
            }

    /// Pool an expression subtree post-order (see `poolPat`), its owned sub-patterns
    /// included.
    let rec poolExpr (sink: PoolSink) (e: Frozen.TExpr) : ExprPoolId =
        match TastAccessor.exprKind e with
        | ExprShape.ForTo -> sink.InternBinder (TastAccessor.exprForTo e).Var
        | _ -> ()

        let exprKids = TastAccessor.exprChildren e |> Array.map (poolExpr sink)
        let patKids = TastAccessor.exprPatChildren e |> Array.map (poolPat sink)

        let id =
            sink.AddExpr
                {
                    Shape = TastAccessor.exprKind e
                    Ty = TastAccessor.exprTy e
                    Tok = TastAccessor.exprTok e
                    Children = exprKids
                    PatChildren = patKids
                    VarBinder = ValueNone
                    Payload = exprPayload e
                }

        sink.OnExprPooled e id
        id

    /// Pool a declaration and its expr/pat roots (see `poolPat`). A `Type` decl surfaces
    /// no children — its member bodies ride the payload opaquely.
    let poolDecl (sink: PoolSink) (d: Frozen.TDecl) : DeclPoolId =
        let struct (exprKids, patKids) =
            match TastAccessor.declKind d with
            | DeclShape.Let ->
                let v = TastAccessor.declLet d
                struct ([| poolExpr sink v.Value |], [| poolPat sink v.Binding |])
            | DeclShape.Expression -> struct ([| poolExpr sink (TastAccessor.declExpression d) |], [||])
            | DeclShape.Type -> struct ([||], [||])

        sink.AddDecl
            {
                Shape = TastAccessor.declKind d
                ExprChildren = exprKids
                PatChildren = patKids
                Payload = declPayload d
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

        // The binder pool as two parallel columns: each distinct NodeKey a `NamedSimple`
        // pattern or `ForTo` loop variable introduces, interned to a dense `BinderId` on
        // first encounter, alongside the naming triple sourced from that same key. The
        // introducing sites are enumerated off the accessor as the tree is walked, so
        // nothing re-derives which nodes bind.
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
                    fun e (ExprPoolId id) ->
                        match TastAccessor.exprKind e with
                        | ExprShape.Var -> varBindings.Add(struct (id, TastAccessor.exprVarBinding e))
                        | ExprShape.Lambda -> lambdaIds.[TastWalk.lambdaKey e] <- ExprPoolId id
                        | _ -> ()
            }

        let roots = file.Decls |> EqArray.toArray |> Array.map (poolDecl sink)

        // Resolve a reference/side-table key to the binder it names. A miss means the
        // referent was minted by no `NamedSimple`/`ForTo` node — an incomplete binder
        // enumeration, which is exactly the failure the id-resolution gate exists to
        // surface.
        let binderIdOf (k: NodeKey) : BinderId =
            match binderIds.TryGetValue k with
            | true, id -> id
            | false, _ -> failwithf "TastPools.toPools: %O references a binder no NamedSimple/ForTo node introduced" k

        // The lambda-key analogue: a `FunVerdicts` key that names no pooled lambda is the
        // honest failure a lambda-keyed entry naming no pooled lambda should be.
        let lambdaIdOf (k: NodeKey) : ExprPoolId =
            match lambdaIds.TryGetValue k with
            | true, id -> id
            | false, _ -> failwithf "TastPools.toPools: FunVerdicts key %O names no pooled lambda" k

        // Second pass: now the enumeration is complete, route each `Var`'s reference edge
        // to its binder's dense id — the sparse `ExprVarBinder` column (`ValueNone` at
        // every non-`Var` slot).
        let exprVarBinder: BinderId voption[] = Array.create exprShapes.Count ValueNone

        for (struct (id, key)) in varBindings do
            exprVarBinder.[id] <- ValueSome(binderIdOf key)

        // One generic remap over the side tables, parameterized by the key resolver: the
        // binder-keyed tables pass `binderIdOf`, `FunVerdicts` passes `lambdaIdOf`. A second
        // resolver, not a second remap, so the two id spaces share one enumeration.
        let remapSideTable (resolve: NodeKey -> 'id) (m: Map<NodeKey, 'v>) : ('id * 'v)[] =
            m |> Map.toArray |> Array.map (fun (k, v) -> resolve k, v)

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
            BinderKeys = binderKeys.ToArray()
            BinderNamings = binderNamings.ToArray()
            Residue =
                {
                    Diagnostics = file.Diagnostics
                    IntrinsicReprKeys = file.IntrinsicReprKeys
                    InlineBodies = file.InlineBodies
                    Accessibility = file.Accessibility
                }
            ModuleMembers = remapSideTable binderIdOf file.ModuleMembers
            TopLevelNames = remapSideTable binderIdOf file.TopLevelNames
            ClosureReprs = remapSideTable binderIdOf file.ClosureReprs
            FunVerdicts = remapSideTable lambdaIdOf file.FunVerdicts
            GenericFnSchemes = remapSideTable binderIdOf file.GenericFnSchemes
            BindingValReprs = remapSideTable binderIdOf file.BindingValReprs
            BindingTyparArities = remapSideTable binderIdOf file.BindingTyparArities
        }

    // ── the inverse: rebuild the DU trees from the pools ────────────────────
    //
    // `substituteExpr` re-authors a node from its columns — `ty`/`tok`, the resolved
    // `Var` binder, the `ExprPayload` residual scalars/structure — and the ALREADY-REBUILT
    // child subtrees, with NO template node (the expr pool holds none). The children are
    // consumed in the exact order `TastAccessor.exprChildren`/`exprPatChildren` enumerated
    // them (`nextE`/`nextP` are order cursors) — the one coupling the round-trip gate
    // proves. The match on `ExprPayload` is exhaustive with no catch-all (the inverse of
    // `exprPayload`), so a new shape fails to compile here. `substitutePat`/`substituteDecl`
    // re-author from their own payload + rebuilt children the same way — no template node,
    // now that the pat and decl pools are columnar too.

    let private substituteExpr
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
        // exact order `TastAccessor.exprChildren`/`exprPatChildren` enumerated them, which
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

    let private substitutePat
        (ty: FrozenType)
        (tok: SyntaxToken)
        (payload: PatPayload)
        (ps: Frozen.TPat[])
        : Frozen.TPat =
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

    let private substituteDecl (payload: DeclPayload) (es: Frozen.TExpr[]) (ps: Frozen.TPat[]) : Frozen.TDecl =
        match payload with
        | DeclPayload.Let p -> TDeclG.Let(ps.[0], es.[0], p.IsInline, p.Ty)
        | DeclPayload.Expression ty -> TDeclG.Expression(es.[0], ty)
        | DeclPayload.Type td -> TDeclG.Type td

    /// Rebuild the `Frozen.TastFile` DU from the pools — the inverse of `toPools`. The
    /// `Decls` are re-authored from the pool roots and the side tables re-keyed back
    /// through the binder/lambda id spaces (the interconversion under test); only the
    /// four `Residue` fields are carried through verbatim, having no pooled form.
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
            substituteDecl pools.DeclPayloads.[i] es ps

        let decls = pools.Roots |> Array.map fromDecl |> EqArray.ofArray

        // Rebuild a side table from its dense form, resolving each `BinderId` back to its
        // NodeKey. Reconstructing the maps here (rather than retaining the source file's) is
        // what makes the round-trip prove the key remap, not just the decl trees.
        // One generic rebuild, parameterized by the inverse key resolver: the binder-keyed
        // tables pass `binderKey`, `FunVerdicts` passes `lambdaKeyOf`.
        let rebuildSideTable (resolve: 'id -> NodeKey) (dense: ('id * 'v)[]) : Map<NodeKey, 'v> =
            dense |> Array.map (fun (id, v) -> resolve id, v) |> Map.ofArray

        {
            Decls = decls
            Diagnostics = pools.Residue.Diagnostics
            IntrinsicReprKeys = pools.Residue.IntrinsicReprKeys
            ModuleMembers = rebuildSideTable binderKey pools.ModuleMembers
            TopLevelNames = rebuildSideTable binderKey pools.TopLevelNames
            ClosureReprs = rebuildSideTable binderKey pools.ClosureReprs
            FunVerdicts = rebuildSideTable lambdaKeyOf pools.FunVerdicts
            GenericFnSchemes = rebuildSideTable binderKey pools.GenericFnSchemes
            InlineBodies = pools.Residue.InlineBodies
            Accessibility = pools.Residue.Accessibility
            BindingValReprs = rebuildSideTable binderKey pools.BindingValReprs
            BindingTyparArities = rebuildSideTable binderKey pools.BindingTyparArities
        }
