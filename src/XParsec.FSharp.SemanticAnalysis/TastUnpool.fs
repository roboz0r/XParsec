namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Parser

// The DRAIN direction of the frozen pools: columns back to the `Frozen.*` DU. Its
// inverse — the DU vocabulary, the pooling walk and `toPools` — is `TastPools.fs`, and
// the wire-shape types are `TastPoolTypes.fs`. Split from the fill so the two directions
// are separately readable and the compiler's dependency edge says which way the data
// flows: this file reads `TastPools`, never the reverse.
//
// Two of its three exports have production callers, and for narrow reasons:
//
//   * `substituteExpr`/`substitutePat`/`substituteDecl` — the NODE-level inverse, driven
//     by `TastPoolBuilder`'s subtree drain (`declTree`) for the one channel whose far end
//     is still DU-typed: a package's inline template crosses the wire as a
//     `Frozen.TDecl`, a pool id being meaningless outside the pool that issued it.
//   * `binderKeyedMap` — a dense side table back as the `Map<NodeKey,_>` it was re-keyed
//     from, for a consumer whose own downstream API is still `NodeKey`-keyed
//     (`Layout.buildUnit` feeding the CLR holder plan / closure discovery).
//
// `ofPools` — the whole-file drain — has NONE, and that is the point of it: it is what
// makes the columns' tree-sufficiency CHECKABLE.

[<RequireQualifiedAccess>]
module TastUnpool =

    /// Re-author one expression node from its columns — `ty`/`tok`, the resolved `Var`
    /// binder, the `ExprPayload` residual scalars/structure — and its ALREADY-REBUILT
    /// child subtrees, with NO template node (the expr pool holds none). The children are
    /// consumed in the exact order `TastPools.exprChildren`/`exprPatChildren` enumerated
    /// them (`nextE`/`nextP` are order cursors) — the one coupling the round-trip gate
    /// proves. The match on `ExprPayload` is exhaustive with no catch-all (the inverse of
    /// `TastPools.exprPayload`), so a new shape fails to compile here.
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

        // The arm / format re-nesting is `ExprPayload.arms` / `ExprPayload.format` — the
        // one walk over the flat child columns, shared with the accessor's views, so the
        // two directions cannot disagree about the order the columns are consumed in.
        let buildArms (guardPresent: bool[]) : EqArray<Frozen.TMatchArm> =
            ExprPayload.arms guardPresent nextP nextE |> EqArray.ofArray

        match payload with
        // `binding` is supplied from the dense id, so the round-trip exercises the remap.
        | ExprPayload.Var ->
            match varBinding with
            | ValueSome k -> TExprG.Var(k, ty, tok)
            | ValueNone -> failwith "TastUnpool.ofPools: a Var entry carries no resolved binder id"
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
            let sink', segments' = ExprPayload.format p.Sink p.Segments nextE
            TExprG.Format(sink', EqArray.ofArray segments', ty, tok)
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

    /// Re-author one pattern node from its own payload + rebuilt sub-patterns — see
    /// `substituteExpr`; exhaustive against `TastPools.patPayload` the same way.
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
            // No `BindingValReprs`: the DU does not carry one. It is a PROJECTION of the
            // lambda spine, so `toPools` re-derives it off the columns rather than the DU
            // ferrying it across — which is also why the round trip does not have to
            // reconstruct it to stay faithful.
            BindingTyparArities = binderKeyedMap pools pools.BindingTyparArities
        }
