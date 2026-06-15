namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Parser

/// Platform-neutral TAST lowering, shared by both codegen backends (CLR and JS).
/// The two codegen projects must not reference each other, so the lowering logic
/// that traffics only in `Frozen.TExpr` / `FrozenType` / `NodeKey` lives here, in
/// `SemanticAnalysis`, where both backends already depend.
///
/// What stays backend-local is the *operator-finish* pass: `lower` takes a
/// `finishOps` knob so each backend supplies its own. The CLR backend passes
/// `EmitLower.expandBuiltinOps` (collapse saturated operators to stack-machine
/// `ILIntrinsic`); the JS backend keeps operators as something it can emit as a
/// `BinaryExpression` / template, with no CIL collapse.
module TastLower =

    /// One flattened parameter of a top-level function lowered to a static method.
    /// A simple binder's `Slot` key is referenced directly by the body (it resolves
    /// to the parameter's slot); a destructuring tuple parameter (`fun (a, b) -> …`)
    /// carries `Pat = Some …` and a synthetic `Slot`, whose value the backend
    /// spills to a local and `bindPattern`s into the leaf bindings. Platform-neutral
    /// (`Frozen.TPat` / `FrozenType` / `NodeKey` only); the CLR-shaped `StaticFn`
    /// that carries it stays in `Codegen.Clr`.
    type StaticParam =
        {
            Slot: NodeKey
            Ty: FrozenType
            Pat: Frozen.TPat option
        }

    let typeOfExpr (e: Frozen.TExpr) : FrozenType =
        match e with
        | TExprG.Const(ty = ty)
        | TExprG.Var(ty = ty)
        | TExprG.External(ty = ty)
        | TExprG.Lambda(ty = ty)
        | TExprG.App(ty = ty)
        | TExprG.Let(ty = ty)
        | TExprG.Use(ty = ty)
        | TExprG.IfThenElse(ty = ty)
        | TExprG.Tuple(ty = ty)
        | TExprG.Sequential(ty = ty)
        | TExprG.While(ty = ty)
        | TExprG.ForTo(ty = ty)
        | TExprG.ForIn(ty = ty)
        | TExprG.Match(ty = ty)
        | TExprG.TryWith(ty = ty)
        | TExprG.TryFinally(ty = ty)
        | TExprG.Assignment(ty = ty)
        | TExprG.Null(ty = ty)
        | TExprG.Range(ty = ty)
        | TExprG.RecordCons(ty = ty)
        | TExprG.RecordClone(ty = ty)
        | TExprG.FieldGet(ty = ty)
        | TExprG.FieldSet(ty = ty)
        | TExprG.UnionCons(ty = ty)
        | TExprG.New(ty = ty)
        | TExprG.MethodCall(ty = ty)
        | TExprG.PropertyGet(ty = ty)
        | TExprG.StaticMethodCall(ty = ty)
        | TExprG.StaticPropertyGet(ty = ty)
        | TExprG.StaticFieldGet(ty = ty)
        | TExprG.ExternalMember(ty = ty)
        | TExprG.Format(ty = ty)
        | TExprG.ILIntrinsic(ty = ty)
        | TExprG.StaticOptimization(ty = ty)
        | TExprG.Upcast(ty = ty)
        | TExprG.Downcast(ty = ty)
        | TExprG.TraitCall(ty = ty)
        | TExprG.TypeTest(ty = ty) -> ty

    let typeOfPat (p: Frozen.TPat) : FrozenType =
        match p with
        | TPatG.NamedSimple(ty = ty)
        | TPatG.Wildcard(ty = ty)
        | TPatG.Tuple(ty = ty)
        | TPatG.Const(ty = ty)
        | TPatG.Record(ty = ty)
        | TPatG.Union(ty = ty)
        | TPatG.TypeTestAs(ty = ty) -> ty

    /// Resolve a nominal receiver type to its `(SymbolKey, type-args)` pair
    /// (was a projected string name). The
    /// project-local emitted-type tables key by this `SymbolKey` directly; the
    /// external provider lookups derive the qualified compiled name from it via
    /// `SymbolKeyOps.qualifiedName`. Returns `ValueNone` if the type isn't a
    /// user-defined or external nominal type (e.g. a `TyVar` that should have been
    /// zonked away by now).
    let inline receiverShape (ty: FrozenType) : (SymbolKey * FrozenType list) voption =
        match ty with
        | FTUnion(n, args)
        | FTRecord(n, args)
        | FTClass(n, args) -> ValueSome(n, EqArray.toList args)
        | _ -> ValueNone

    /// Recover a generic static method's per-typar instantiation at a call site
    /// (R3): structurally match each declared parameter type (`defTys`, carrying
    /// the method's typar `TypeVar`s) against the actual argument type. First
    /// occurrence wins. A recursive self-call yields the method's own typars
    /// (encoded `!!i`); an external call yields concrete types.
    let matchInstantiation (typarCount: int) (defTys: FrozenType list) (actualTys: FrozenType list) : FrozenType list =
        let result = Array.create typarCount ValueNone

        let rec go (defT: FrozenType) (actT: FrozenType) =
            match defT, actT with
            // A freeze-quantified method typar: the index is on
            // the node, so the recovered instantiation is index-keyed. `act` may
            // itself be a `FTTypar(Method, j)` — the enclosing generic context's
            // typar — which the `MethodSpec` then encodes verbatim.
            | FTTypar(TyparAxis.Method, i), act ->
                if i >= 0 && i < typarCount && result.[i].IsNone then
                    result.[i] <- ValueSome act
            | FTFun(a1, r1), FTFun(a2, r2) ->
                go a1 a2
                go r1 r2
            | FTTuple xs, FTTuple ys when xs.Length = ys.Length ->
                for i in 0 .. xs.Length - 1 do
                    go xs.[i] ys.[i]
            | FTRecord(_, xs), FTRecord(_, ys) when xs.Length = ys.Length ->
                for i in 0 .. xs.Length - 1 do
                    go xs.[i] ys.[i]
            | FTUnion(_, xs), FTUnion(_, ys) when xs.Length = ys.Length ->
                for i in 0 .. xs.Length - 1 do
                    go xs.[i] ys.[i]
            | FTClass(_, xs), FTClass(_, ys) when xs.Length = ys.Length ->
                for i in 0 .. xs.Length - 1 do
                    go xs.[i] ys.[i]
            // A generic intrinsic (notably the array `[]<!!i>`) carries its element
            // structurally; recurse so the element typar is recovered.
            | FTConst(_, xs), FTConst(_, ys) when xs.Length = ys.Length ->
                for i in 0 .. xs.Length - 1 do
                    go xs.[i] ys.[i]
            | _ -> ()

        List.iter2 go defTys actualTys

        [
            for i in 0 .. typarCount - 1 ->
                match result.[i] with
                | ValueSome t -> t
                | ValueNone -> failwithf "Emit: could not infer instantiation for static-method type parameter %d" i
        ]

    /// The single structural recursion the lowering map, the closure collector,
    /// and the free-variable walk all share (the latter two via `iterChildren`).
    let mapChildren (f: Frozen.TExpr -> Frozen.TExpr) (e: Frozen.TExpr) : Frozen.TExpr =
        match e with
        | TExprG.Const _
        | TExprG.Var _
        | TExprG.External _
        | TExprG.Null _
        | TExprG.StaticPropertyGet _
        | TExprG.StaticFieldGet _ -> e
        | TExprG.Lambda(p, b, t, tk) -> TExprG.Lambda(p, f b, t, tk)
        | TExprG.App(fn, a, t, tk) -> TExprG.App(f fn, f a, t, tk)
        | TExprG.Let(p, v, b, t, tk) -> TExprG.Let(p, f v, f b, t, tk)
        | TExprG.Use(p, v, b, dispose, t, tk) -> TExprG.Use(p, f v, f b, dispose, t, tk)
        | TExprG.IfThenElse(c, th, el, t, tk) -> TExprG.IfThenElse(f c, f th, f el, t, tk)
        | TExprG.Tuple(xs, t, tk) -> TExprG.Tuple(EqArray.map f xs, t, tk)
        | TExprG.Sequential(xs, t, tk) -> TExprG.Sequential(EqArray.map f xs, t, tk)
        | TExprG.While(c, b, t, tk) -> TExprG.While(f c, f b, t, tk)
        | TExprG.ForTo(v, s, e2, b, t, tk) -> TExprG.ForTo(v, f s, f e2, f b, t, tk)
        | TExprG.ForIn(p, src, b, en, t, tk) -> TExprG.ForIn(p, f src, f b, en, t, tk)
        | TExprG.Match(sc, arms, t, tk) ->
            TExprG.Match(
                f sc,
                arms
                |> EqArray.map (fun a ->
                    { a with
                        Guard = Option.map f a.Guard
                        Body = f a.Body
                    }
                ),
                t,
                tk
            )
        | TExprG.TryWith(b, arms, t, tk) ->
            TExprG.TryWith(
                f b,
                arms
                |> EqArray.map (fun a ->
                    { a with
                        Guard = Option.map f a.Guard
                        Body = f a.Body
                    }
                ),
                t,
                tk
            )
        | TExprG.TryFinally(b, c, t, tk) -> TExprG.TryFinally(f b, f c, t, tk)
        | TExprG.Assignment(l, r, t, tk) -> TExprG.Assignment(f l, f r, t, tk)
        | TExprG.Range(s, step, stop, t, tk) -> TExprG.Range(f s, Option.map f step, f stop, t, tk)
        | TExprG.RecordCons(fields, t, tk) -> TExprG.RecordCons(EqArray.map (fun (n, v) -> n, f v) fields, t, tk)
        | TExprG.RecordClone(src, ov, t, tk) -> TExprG.RecordClone(f src, EqArray.map (fun (n, v) -> n, f v) ov, t, tk)
        | TExprG.FieldGet(r, n, t, tk) -> TExprG.FieldGet(f r, n, t, tk)
        | TExprG.FieldSet(r, n, v, t, tk) -> TExprG.FieldSet(f r, n, f v, t, tk)
        | TExprG.UnionCons(c, args, t, tk) -> TExprG.UnionCons(c, EqArray.map f args, t, tk)
        | TExprG.New(c, args, t, tk) -> TExprG.New(c, EqArray.map f args, t, tk)
        | TExprG.MethodCall(r, k, via, args, t, tk) -> TExprG.MethodCall(f r, k, via, EqArray.map f args, t, tk)
        | TExprG.PropertyGet(r, k, via, t, tk) -> TExprG.PropertyGet(f r, k, via, t, tk)
        | TExprG.StaticMethodCall(k, args, t, tk) -> TExprG.StaticMethodCall(k, EqArray.map f args, t, tk)
        | TExprG.ExternalMember(r, k, n, isProp, t, tk) ->
            TExprG.ExternalMember(ValueOption.map f r, k, n, isProp, t, tk)
        | TExprG.Format(sink, segs, t, tk) ->
            let sink =
                match sink with
                | FormatSinkG.ToWriter w -> FormatSinkG.ToWriter(f w)
                | FormatSinkG.ToBuilder w -> FormatSinkG.ToBuilder(f w)
                | other -> other

            let segs =
                segs
                |> EqArray.map (fun seg ->
                    match seg with
                    | FormatSegG.Lit _ -> seg
                    | FormatSegG.Hole(h, a) -> FormatSegG.Hole(h, f a)
                )

            TExprG.Format(sink, segs, t, tk)
        | TExprG.ILIntrinsic(op, operand, args, t, tk) -> TExprG.ILIntrinsic(op, operand, EqArray.map f args, t, tk)
        | TExprG.StaticOptimization(clauses, def, t, tk) ->
            TExprG.StaticOptimization(clauses |> EqArray.map (fun cl -> { cl with Body = f cl.Body }), f def, t, tk)
        | TExprG.Upcast(src, t, tk) -> TExprG.Upcast(f src, t, tk)
        | TExprG.Downcast(src, t, tk) -> TExprG.Downcast(f src, t, tk)
        | TExprG.TraitCall(recv, n, args, t, tk) -> TExprG.TraitCall(recv, n, EqArray.map f args, t, tk)
        | TExprG.TypeTest(src, testTy, t, tk) -> TExprG.TypeTest(f src, testTy, t, tk)

    /// Reuses `mapChildren`, discarding the rebuilt tree — only the one-shot
    /// discovery / free-variable pre-passes call this.
    let iterChildren (f: Frozen.TExpr -> unit) (e: Frozen.TExpr) : unit =
        mapChildren
            (fun c ->
                f c
                c
            )
            e
        |> ignore

    /// Source of synthetic `NodeKey`s for placeholder lambda-parameter slots —
    /// the unit binder (`fun () -> …`) and the tuple binder (`fun (a, b) -> …`).
    /// The body never references the key (a unit value is dropped; a tuple is
    /// destructured into its leaf bindings), but a fresh per-call key lets the
    /// `args.[key]` dict still allocate the slot for the value the
    /// caller pushes without clashing with other binders. `Interlocked` keeps it
    /// safe across the parallel test runner.
    let mutable private paramSynthCounter = 0

    let private mintSyntheticParamKey () : NodeKey =
        let c = System.Threading.Interlocked.Increment(&paramSynthCounter)
        NodeKey.ofSynthetic c NodeKind.SynthLambdaBody

    let mintUnitParamKey () : NodeKey = mintSyntheticParamKey ()

    /// The placeholder key for a destructuring tuple lambda parameter — its
    /// tuple value is `bindPattern`ed into the real leaf
    /// bindings, so the key itself is never referenced (Step 5).
    let mintTupleParamKey () : NodeKey = mintSyntheticParamKey ()

    /// The synthetic key for a `use _ = e` binder. The value is still bound to a
    /// local (it is the resource the `finally` disposes), but `_` gives the body
    /// no name to reference it, so the slot is keyed off a fresh placeholder.
    let mintUseBinderKey () : NodeKey = mintSyntheticParamKey ()

    /// Peel a curried `Lambda` chain of simple (`NamedSimple`), unit-pattern
    /// (`TPatG.Const(Unit, _)`, from `fun () -> …`), or destructuring tuple
    /// (`fun (a, b) -> …`) parameters. A unit binder gets a synthetic placeholder
    /// `NodeKey` (the body never references it) so the static-method emission
    /// still allocates a slot for the unit value the caller pushes. A
    /// tuple binder likewise gets a synthetic `Slot` and carries its `Pat` so the
    /// emission `bindPattern`s the leaf bindings out of the value. Any
    /// other pattern stops the peel.
    let rec peelLambda (e: Frozen.TExpr) : StaticParam list * Frozen.TExpr =
        match e with
        | TExprG.Lambda(TPatG.NamedSimple(k, pty, _), body, _, _) ->
            let ps, b = peelLambda body
            { Slot = k; Ty = pty; Pat = None } :: ps, b
        | TExprG.Lambda(TPatG.Const(TConstValue.Unit, pty, _), body, _, _) ->
            let ps, b = peelLambda body

            {
                Slot = mintUnitParamKey ()
                Ty = pty
                Pat = None
            }
            :: ps,
            b
        | TExprG.Lambda((TPatG.Tuple(_, pty, _) as pat), body, _, _) ->
            let ps, b = peelLambda body

            {
                Slot = mintTupleParamKey ()
                Ty = pty
                Pat = Some pat
            }
            :: ps,
            b
        | _ -> [], e

    let private isFunTy (t: FrozenType) : bool =
        match t with
        | FTFun _ -> true
        | _ -> false

    /// Lower a decl list into a closure-bearing, External-value-free tree. After
    /// this, every `TExprG.Lambda` is a function value and every `External` is
    /// either a call head or has non-function type.
    ///
    /// Inline expansion (local + cross-package `let inline` splicing, beta
    /// reduction, `StaticOptimization` resolution) is no longer done here: it ran
    /// pre-freeze in `Passes.InlineExpansion`, so the
    /// frozen decls reaching codegen carry no `External(inlineName)` call heads and
    /// no `StaticOptimization` nodes. Inline TEMPLATES (`TDeclG.Let(isInline)`) are
    /// still dropped here. What remains codegen-only is (1) eta-reifying an
    /// `External` function VALUE into a closure (it must run after the front end,
    /// where closures are a codegen concept) and (2) the closing `finishOps` pass,
    /// supplied by the backend, that finishes any saturated operators the inline
    /// pass left un-ground (`13 &&& 11`, `a = b` with a generic operand) — for the
    /// CLR that collapses to inline IL, for JS it keeps a template / `BinaryExpr`.
    let lower (finishOps: Frozen.TExpr -> Frozen.TExpr) (decls: EqArray<Frozen.TDecl>) : Frozen.TDecl list =
        // Build-wide monotone counter for eta parameters, so independent
        // eta-reifications never share a NodeKey.
        let mutable counter = 0

        let mint () =
            let k = NodeKey.ofSynthetic counter NodeKind.SynthInlineExpansion
            counter <- counter + 1
            k

        // Eta-reify `External(name, a -> … -> r)` used as a value into
        // `fun p0 -> … -> name p0 …`, turning a function name into a closure.
        // `tok` is the source `External` value node's token; every synthesised
        // wrapper (params, applications, lambdas) inherits it.
        let etaExpand (name: string) (ty: FrozenType) (tok: SyntaxToken) : Frozen.TExpr =
            let rec arrows t =
                match t with
                | FTFun(a, b) ->
                    let ps, r = arrows b
                    (a :: ps), r
                | _ -> [], t

            let paramTys, retTy = arrows ty
            let kts = paramTys |> List.map (fun pty -> mint (), pty)

            let rec applyAll acc accTy ks =
                match ks, accTy with
                | [], _ -> acc
                | (k, pty) :: rest, FTFun(_, resTy) ->
                    applyAll (TExprG.App(acc, TExprG.Var(k, pty, tok), resTy, tok)) resTy rest
                | _ -> failwith "Emit: eta-reification arity mismatch"

            let appBody = applyAll (TExprG.External(name, ValueNone, ty, tok)) ty kts

            kts
            |> List.foldBack (fun (k, pty) (innerBody, innerTy) ->
                let lamTy = FTFun(pty, innerTy)
                TExprG.Lambda(TPatG.NamedSimple(k, pty, tok), innerBody, lamTy, tok), lamTy
            )
            <| (appBody, retTy)
            |> fst

        let rec lowerExpr (e: Frozen.TExpr) : Frozen.TExpr =
            match e with
            | TExprG.App _ ->
                let head, spineArgs = TastWalk.collectSpine [] e

                // An `External` head is a recipe / built-in-operator call, so it
                // stays in call position and is not eta-reified; the args are
                // values. The inline pass already expanded any spliceable head.
                let head' =
                    match head with
                    | TExprG.External _ -> head
                    | _ -> lowerExpr head

                TastWalk.rebuildApp head' [ for (a, t, tk) in spineArgs -> lowerExpr a, t, tk ]
            | TExprG.External(name, _, ty, tok) when isFunTy ty -> etaExpand name ty tok
            | _ -> mapChildren lowerExpr e

        // Split a folded top-level statement sequence back into standalone decls
        // in source order (module-representation §10.3 Stage 2). The parser folds
        // consecutive top-level statements/lets into ONE `TDecl.Expression` whose
        // expr is a `Sequential` / `let … in …` chain, so a value *after* a
        // statement (a *trailing* value) arrives nested as a `TExpr.Let` the
        // Program-value collector never sees and stays a `Main` local. Peeling the
        // chain here lets each trailing `let` become its own `TDecl.Let`, which
        // flows into the already-built `ProgramMainValues` / `stsfld` path.
        //
        // Only the outermost statement *spine* is peeled — `Sequential` items and
        // the continuation (`body`) of a top-level `let … in …`. Sub-expressions
        // (application args, lambda bodies, match arms) are NOT descended into, so
        // a genuinely-local `let` nested inside an expression is left intact.
        let rec flattenTopLevel (e: Frozen.TExpr) : Frozen.TDecl list =
            match e with
            | TExprG.Sequential(items, _, _) ->
                [
                    for it in EqArray.toList items do
                        yield! flattenTopLevel it
                ]
            | TExprG.Let(pat, value, body, _, _) ->
                // The bound value is itself an expression (not a statement spine) —
                // keep it whole; only the `body` continuation is more top-level decls.
                TDeclG.Let(pat, value, false, typeOfExpr value) :: flattenTopLevel body
            | _ -> [ TDeclG.Expression(e, typeOfExpr e) ]

        // Eta lowering surfaces operator applications (an eta-reified `(+)`);
        // `finishOps` then finishes every saturated one — a closing phase so it
        // sees them all.
        let result = ResizeArray<Frozen.TDecl>()

        let lowerOne (d: Frozen.TDecl) =
            match d with
            | TDeclG.Let(_, _, true, _) -> ()
            | TDeclG.Let(p, value, false, t) -> result.Add(TDeclG.Let(p, finishOps (lowerExpr value), false, t))
            | TDeclG.Expression(e, t) -> result.Add(TDeclG.Expression(finishOps (lowerExpr e), t))
            // Type declarations are emitted as metadata, not through the expr stream.
            | TDeclG.Type _ -> ()

        for d in decls do
            match d with
            // A top-level statement decl may be a folded sequence — split it first,
            // so a trailing `let` reaches the collector as a standalone decl.
            | TDeclG.Expression(e, _) ->
                for fd in flattenTopLevel e do
                    lowerOne fd
            | _ -> lowerOne d

        List.ofSeq result
