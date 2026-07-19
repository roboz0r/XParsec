namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// Platform-neutral TAST lowering, shared by both codegen backends (CLR and JS).
/// The two codegen projects must not reference each other, so the lowering logic
/// that traffics only in `Frozen.TExpr` / `FrozenType` / `NodeKey` lives here, in
/// `SemanticAnalysis`, where both backends already depend.
///
/// Lowering is backend-uniform: no operator reaches it needing a per-backend finish.
/// `Passes.InlineExpansion` splices every operator's contract body by `SymbolKey`
/// pre-freeze, so what arrives here is already the body — an `ILIntrinsic` clause, a
/// `StaticMethodCall`, or a diagnostic that stopped the compile.
module TastLower =

    /// One flattened parameter of a top-level function lowered to a static method.
    /// A simple binder's `Slot` key is referenced directly by the body (it resolves
    /// to the parameter's slot); a destructuring tuple parameter (`fun (a, b) -> …`)
    /// carries `Pat = Some …` and a synthetic `Slot`, whose value the backend
    /// spills to a local and `bindPattern`s into the leaf bindings. Platform-neutral
    /// (`Frozen.TPat` / `FrozenType` / `NodeKey` only); the CLR-shaped `StaticFn`
    /// that carries it stays in `Codegen.Clr`. The shape lives in `Tast.fs`
    /// (`StaticParamG`, generic over `'ty`/`'tok`); this is the frozen instantiation.
    type StaticParam = Frozen.StaticParam

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
        | TExprG.StaticFieldSet(ty = ty)
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
        | TPatG.TypeTestAs(ty = ty)
        | TPatG.Null(ty = ty)
        | TPatG.EnumCase(ty = ty)
        | TPatG.Or(ty = ty) -> ty

    /// Resolve a nominal receiver type to its `(SymbolKey, type-args)` pair
    /// (was a projected string name). The
    /// project-local emitted-type tables key by this `SymbolKey` directly; the
    /// external provider lookups derive the qualified compiled name from it via
    /// `SymbolKeyOps.qualifiedName`. Returns `ValueNone` if the type isn't a
    /// user-defined or external nominal type (e.g. a `TyVar` that should have been
    /// zonked away by now).
    let inline receiverShape (ty: FrozenType) : (TypeKey * FrozenType list) voption =
        match ty with
        | FTUnion(n, args)
        | FTRecord(n, args)
        | FTClass(n, args) -> ValueSome(n, EqArray.toList args)
        | _ -> ValueNone

    /// As `matchInstantiation`, but returns the recovery array WITH `ValueNone`
    /// holes for typars that no parameter/result mentions — a phantom constraint
    /// typar (e.g. `fold`'s enumerator `'E`) is unrecoverable by param-matching and
    /// must be solved separately from its bounds at the call site (EmitCall).
    let matchInstantiationPartial
        (typarCount: int)
        (defTys: FrozenType list)
        (actualTys: FrozenType list)
        : FrozenType voption[] =
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
        result

    /// Recover a generic static method's per-typar instantiation at a call site:
    /// structurally match each declared parameter type (`defTys`, carrying
    /// the method's typar `TypeVar`s) against the actual argument type. First
    /// occurrence wins. A recursive self-call yields the method's own typars
    /// (encoded `!!i`); an external call yields concrete types. Strict: every
    /// typar must be recovered by param-matching (the `EmitExpr` caller has no
    /// phantom typars).
    let matchInstantiation (typarCount: int) (defTys: FrozenType list) (actualTys: FrozenType list) : FrozenType list =
        let result = matchInstantiationPartial typarCount defTys actualTys

        [
            for i in 0 .. typarCount - 1 ->
                match result.[i] with
                | ValueSome t -> t
                | ValueNone -> failwithf "Emit: could not infer instantiation for static-method type parameter %d" i
        ]

    /// The call-site PHANTOM-typar solve, shared by the
    /// project-local static-fn call (`EmitCall.buildAppCall`) and the EXTERNAL
    /// module-fn call (`ClrRecipes.emitExternalCall`); `tryWitness` is the only
    /// head-specific seam (project-local `env.Classes` vs the external provider's
    /// `FrozenInterfaces`), so the solve itself is head-agnostic.
    ///
    /// `instArr` already carries every signature-reachable method typar (recovered by
    /// `matchInstantiationPartial` / the encoder's `recoverOpenTypars`); a phantom
    /// constraint typar (`fold`'s enumerator `'E`, present only in a
    /// `'S :> IStructSeq<'T,'E>` bound, in no param/result) is still `ValueNone`. For
    /// each `Coercion(ci, target)` whose constrained typar `ci` is already resolved
    /// (e.g. `'S := instArr.[idx_S]`, the node-key-rewritten `<closure>$`-bearing arg
    /// type), `tryWitness` walks the constrained type's interface impl for `target`'s
    /// interface and the typars `target` mentions (incl. `'E`) are structurally
    /// recovered from the concrete witness. Iterate to a fixpoint — one bound's target
    /// may mention a typar another bound just solved.
    ///
    /// The witness is AUTHORITATIVE for a bound-mentioned typar even when the signature
    /// already recovered it: a combinator carrying `'E` in BOTH its bound AND its result
    /// can have a stale arrow in the result occurrence (a chained source's `'TFunc`
    /// buried in its frozen enumerator type), whereas the witness reads it from the
    /// source's ACTUAL `<closure>$`-bearing seq impl — so every bound-mentioned index is
    /// overridden with the witness value. Mutates `instArr` in place.
    let solvePhantomTypars
        (typarCount: int)
        (constraints: FrozenConstraint list)
        (tryWitness: FrozenType -> TypeKey -> EqArray<FrozenType> voption)
        (instArr: FrozenType voption[])
        : unit =
        if not (List.isEmpty constraints) then
            // Indices the bounds can recover — those the witness may overwrite.
            let boundMentioned = System.Collections.Generic.HashSet<int>()

            let rec mention (t: FrozenType) =
                match t with
                | FTTypar(TyparAxis.Method, i) -> boundMentioned.Add i |> ignore
                | t -> FrozenType.iterChildren mention t

            for c in constraints do
                match c with
                | FrozenConstraint.Coercion(_, target) -> mention target

            let mutable changed = true

            while changed do
                changed <- false

                for c in constraints do
                    match c with
                    | FrozenConstraint.Coercion(ci, target) ->
                        if ci >= 0 && ci < instArr.Length then
                            match instArr.[ci], target with
                            | ValueSome receiver, FTClass(ifaceKey, _) ->
                                match tryWitness receiver ifaceKey with
                                | ValueSome witnessArgs ->
                                    let holes =
                                        matchInstantiationPartial
                                            typarCount
                                            [ target ]
                                            [ FTClass(ifaceKey, witnessArgs) ]

                                    for j in 0 .. instArr.Length - 1 do
                                        match holes.[j] with
                                        | ValueSome t when
                                            instArr.[j] <> ValueSome t
                                            && (instArr.[j] = ValueNone || boundMentioned.Contains j)
                                            ->
                                            instArr.[j] <- ValueSome t
                                            changed <- true
                                        | _ -> ()
                                | ValueNone -> ()
                            | _ -> ()

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
        | TExprG.ForTo(v, it, s, e2, b, t, tk) -> TExprG.ForTo(v, it, f s, f e2, f b, t, tk)
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
        | TExprG.StaticFieldSet(k, n, v, t, tk) -> TExprG.StaticFieldSet(k, n, f v, t, tk)
        | TExprG.UnionCons(c, args, t, tk) -> TExprG.UnionCons(c, EqArray.map f args, t, tk)
        | TExprG.New(c, k, args, t, tk) -> TExprG.New(c, k, EqArray.map f args, t, tk)
        | TExprG.MethodCall(r, k, via, args, t, tk) -> TExprG.MethodCall(f r, k, via, EqArray.map f args, t, tk)
        | TExprG.PropertyGet(r, k, via, t, tk) -> TExprG.PropertyGet(f r, k, via, t, tk)
        | TExprG.StaticMethodCall(k, args, t, tk) -> TExprG.StaticMethodCall(k, EqArray.map f args, t, tk)
        | TExprG.ExternalMember(r, k, n, isProp, t, tk) ->
            TExprG.ExternalMember(ValueOption.map f r, k, n, isProp, t, tk)
        | TExprG.Format(sink, segs, t, tk) ->
            let sink =
                match sink with
                | FormatSinkG.ToWriter(w, nl) -> FormatSinkG.ToWriter(f w, nl)
                | FormatSinkG.ToBuilder w -> FormatSinkG.ToBuilder(f w)
                | other -> other

            let segs =
                segs
                |> EqArray.map (fun seg ->
                    match seg with
                    | FormatSegG.Lit _ -> seg
                    | FormatSegG.Hole(h, a) -> FormatSegG.Hole(h, f a)
                    | FormatSegG.DynHole d ->
                        FormatSegG.DynHole
                            { d with
                                Width = ValueOption.map f d.Width
                                Precision = ValueOption.map f d.Precision
                                Value = f d.Value
                            }
                    | FormatSegG.CallbackHole(spec, residue) -> FormatSegG.CallbackHole(spec, f residue)
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

    /// True when any immediate child of `e` satisfies `p` — the exists-over-children
    /// primitive the recursive `TExpr` search predicates build on (mirrors
    /// `FrozenType.existsChild` / `SemType.existsChild`). `p` is not invoked on further
    /// children once one has matched, so a `p` that recurses short-circuits the descent.
    let existsChild (p: Frozen.TExpr -> bool) (e: Frozen.TExpr) : bool =
        let mutable found = false

        iterChildren
            (fun c ->
                if not found then
                    found <- p c
            )
            e

        found

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
        // A counter, not a source offset — `ofSyntheticCounter` puts it in the negative
        // half of the offset slot so the key can never be read back as a position.
        NodeKey.ofSyntheticCounter c NodeKind.SynthLambdaBody

    let mintUnitParamKey () : NodeKey = mintSyntheticParamKey ()

    /// The placeholder key for a destructuring tuple lambda parameter — its
    /// tuple value is `bindPattern`ed into the real leaf
    /// bindings, so the key itself is never referenced.
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

    // ----------------------------------------------------------------------
    // Compiled-form representation
    //
    // Two preserved artifacts for a function / method:
    //   * `ValRepr`      — the SOURCE arity (the `ValReprInfo` analogue): the
    //                      curried groups, each group's tuple structure, the typar
    //                      arity, and the SOURCE (non-erased) result type. A caller
    //                      reconciles its application spine against this.
    //   * `CompiledForm` — the flat CLR/JS signature DERIVED from a `ValRepr`:
    //                      tuple groups flattened (full F#, one level), a lone unit
    //                      group erased, a unit result mapped to `void`.
    //
    // `peelValRepr` is `peelLambda` recast as the `ValRepr` builder (it walks the
    // same curried lambda + tuple-pattern structure); `compiledOf` is the
    // `GetValReprTypeInCompiledForm` analogue. `ValRepr` is the load-bearing
    // artifact (the in-assembly `gather` and the cross-assembly `ExternalSymbol`
    // both carry it); `CompiledForm` is derived from it on demand by `compiledOf`,
    // never stored — it is fully determined by the `ValRepr`.
    // ----------------------------------------------------------------------

    // The source-arity / compiled-form types live in `Tast.fs` (`ArgGroupG`,
    // `ValReprG`, `CompiledReturnG`, `CompiledFormG`, generic over `'ty`/`'tok`);
    // these are the frozen instantiations the builders below produce.
    type ArgGroup = Frozen.ArgGroup
    type ValRepr = Frozen.ValRepr
    type CompiledReturn = Frozen.CompiledReturn
    type CompiledForm = Frozen.CompiledForm

    let private isUnitFrozen (t: FrozenType) : bool =
        match t with
        | FTUnit -> true
        | _ -> false

    /// Peel up to `n` top-level `->` arrows off a frozen type (all of them when
    /// `n < 0`), returning each as a `(domain, codomain)` pair in order. One home for
    /// the `peelN` / `arrows` / `decurryFrozen` walks that were copied across
    /// `VesperLib`, `ClrRecipes`, and `EmitClosures`.
    let rec peelArrows (n: int) (t: FrozenType) : (FrozenType * FrozenType) list =
        if n = 0 then
            []
        else
            match t with
            | FTFun(a, b) -> (a, b) :: peelArrows (if n < 0 then -1 else n - 1) b
            | _ -> []

    /// Rebuild a frozen type by transforming its top-level type-argument vector.
    /// Covers every nominal-with-args shape (`FTConst`/`FTRecord`/`FTUnion`/`FTClass`
    /// and the tuple's element vector); leaves (`FTFun`/`FTOr`/`FTTypar`/`FTUnknown`)
    /// pass through unchanged. The one combinator the codegen verdict rewrites share
    /// for "replace some of a nominal's args" — the per-arg transform `f` decides what
    /// (position match, recursion, etc.).
    let mapFrozenArgs (f: EqArray<FrozenType> -> EqArray<FrozenType>) (t: FrozenType) : FrozenType =
        match t with
        | FTClass(key, args) -> FTClass(key, f args)
        | FTRecord(key, args) -> FTRecord(key, f args)
        | FTUnion(key, args) -> FTUnion(key, f args)
        | FTConst(key, args) -> FTConst(key, f args)
        | FTTuple items -> FTTuple(f items)
        | _ -> t

    /// `peelArrows` projected to the F#-form `(parameter types, residual result)`:
    /// `decurryFrozen`'s shape (`n < 0`, peel all) and the contract peelers (`n`
    /// groups). The residual is the type after the peeled arrows.
    let peelArrowDomains (n: int) (t: FrozenType) : FrozenType list * FrozenType =
        match peelArrows n t with
        | [] -> [], t
        | levels -> List.map fst levels, snd (List.last levels)

    /// The lone-`()` group shape (`[GUnit]`): the only arity that erases to zero
    /// compiled params (F#'s `[[]]` rule, read off the arity not the type). The one
    /// definition of the test the flatten / call-site / adapter paths share.
    let isLoneUnitGroup (groups: ArgGroup list) : bool =
        match groups with
        | [ ArgGroupG.GUnit _ ] -> true
        | _ -> false

    /// Every source group is a plain single binder — the only shape whose flat
    /// params map one-to-one onto the source applications (a self-tail-call can
    /// trampoline; flat == curried, so a value-use needs no adapter beyond aliasing).
    let allSimpleGroups (groups: ArgGroup list) : bool =
        groups
        |> List.forall (
            function
            | ArgGroupG.GSimple _ -> true
            | _ -> false
        )

    /// Does a value-use of a function with these source groups need a curried
    /// adapter (its flat call shape differs from the curried one)? Only for arity ≥ 2
    /// or a tuple group; a single `GSimple` / lone `GUnit` is flat-==-curried.
    let needsCurryAdapter (groups: ArgGroup list) : bool =
        List.length groups >= 2
        || groups
           |> List.exists (
               function
               | ArgGroupG.GTuple _ -> true
               | _ -> false
           )

    /// The flat compiled parameter-TYPE vector of a source group list, given each
    /// group's (already type-correct) parameter type — the
    /// `GetValReprTypeInCompiledForm` flatten rule expressed over types: a lone `()`
    /// group erases to nothing; a tuple group expands to its `FTTuple` elements (full
    /// F#, one level); every other group contributes its one type. `compiledOf`
    /// follows the identical shape over `StaticParam`s; the cross-assembly member-ref
    /// encoder (`ClrRecipes.emitExternalCall`) routes its open-template-peeled types
    /// through here, so the erase/flatten rule is written once.
    let flattenGroupShape (groups: ArgGroup list) (groupParamTys: FrozenType list) : FrozenType list =
        if isLoneUnitGroup groups then
            []
        else
            List.zip groups groupParamTys
            |> List.collect (fun (g, pt) ->
                match g with
                | ArgGroupG.GUnit _
                | ArgGroupG.GSimple _ -> [ pt ]
                | ArgGroupG.GTuple _ ->
                    match pt with
                    | FTTuple xs -> EqArray.toList xs
                    | _ -> [ pt ]
            )

    /// Peel a curried `Lambda` chain into its source `ArgGroup`s and the residual
    /// body (the `peelLambda` walk, recording groups rather than flattened params).
    /// A tuple group keeps its whole pattern — flattening is `compiledOf`'s job, so
    /// the source grouping survives here.
    let rec peelValRepr (e: Frozen.TExpr) : ArgGroup list * Frozen.TExpr =
        match e with
        | TExprG.Lambda(TPatG.NamedSimple(k, pty, _), body, _, _) ->
            let gs, b = peelValRepr body
            ArgGroupG.GSimple(k, pty) :: gs, b
        | TExprG.Lambda(TPatG.Const(TConstValue.Unit, pty, _), body, _, _) ->
            let gs, b = peelValRepr body
            ArgGroupG.GUnit pty :: gs, b
        | TExprG.Lambda((TPatG.Tuple _ as pat), body, _, _) ->
            let gs, b = peelValRepr body
            ArgGroupG.GTuple pat :: gs, b
        | _ -> [], e

    /// Build the SOURCE `ValRepr` for a function value (`typars` = its generic
    /// arity), returning the residual body the backend emits. `ResultTy` is the
    /// residual body's type — the source result, before any unit→void normalisation.
    let valReprOf (typars: int) (e: Frozen.TExpr) : ValRepr * Frozen.TExpr =
        let groups, body = peelValRepr e

        {
            Typars = typars
            Groups = groups
            ResultTy = typeOfExpr body
        },
        body

    /// Flatten one source tuple element to a compiled parameter (one level): a
    /// simple binder becomes a direct arg slot the body references; a wildcard a
    /// slotted-but-unnamed arg; anything else keeps its pattern for the backend to
    /// destructure (a nested tuple element stays one `ValueTuple` param).
    let private flattenTupleItem (p: Frozen.TPat) : StaticParam =
        match p with
        | TPatG.NamedSimple(k, ty, _) -> { Slot = k; Ty = ty; Pat = None }
        | TPatG.Wildcard(ty, _) ->
            {
                Slot = mintSyntheticParamKey ()
                Ty = ty
                Pat = None
            }
        | other ->
            {
                Slot = mintSyntheticParamKey ()
                Ty = typeOfPat other
                Pat = Some other
            }

    /// Derive the flat `CompiledForm` from a source `ValRepr` — the
    /// `GetValReprTypeInCompiledForm` analogue. Full F# tuple flattening (one
    /// level); a LONE unit group (`[GUnit]`) erases to zero params (a unit group
    /// among others stays a `ValueTuple` param); a unit result becomes `RVoid`.
    let compiledOf (vr: ValRepr) : CompiledForm =
        let flattenGroup (g: ArgGroup) : StaticParam list =
            match g with
            | ArgGroupG.GUnit ty ->
                [
                    {
                        Slot = mintUnitParamKey ()
                        Ty = ty
                        Pat = None
                    }
                ]
            | ArgGroupG.GSimple(k, ty) -> [ { Slot = k; Ty = ty; Pat = None } ]
            | ArgGroupG.GTuple(TPatG.Tuple(items, _, _)) -> [ for it in EqArray.toList items -> flattenTupleItem it ]
            | ArgGroupG.GTuple _ -> failwith "peelValRepr: GTuple must carry a TPatG.Tuple pattern"

        let ps =
            if isLoneUnitGroup vr.Groups then
                [] // lone unit group erased (F#'s `[[]]` rule, off the arity)
            else
                vr.Groups |> List.collect flattenGroup

        {
            Params = ps
            Return =
                (if isUnitFrozen vr.ResultTy then
                     CompiledReturnG.RVoid
                 else
                     CompiledReturnG.RValue vr.ResultTy)
        }

    /// A virtual token for the wildcard leaves of a contract-reconstructed tuple
    /// group (below): a `.fsi` `val` has no source position for the synthesised
    /// pattern, so anchor it at offset 0 — the analogue of the synthetic `NodeKey`s.
    let private contractPatTok =
        SyntaxToken.virtualToken (PositionedToken.Create(Token.VirtualApp, 0))

    /// Build the SOURCE `ValRepr` for an EXTERNAL (contract-extracted) function. A
    /// `.fsi` `val` gives each curried group's arity (`CurriedSig`/`ArgsSpec`) but
    /// has no lambda tree to `peelValRepr`, so the groups are reconstructed from
    /// `(arity, paramTy)` pairs (arity = the `*`-separated width within the group):
    /// arity ≥ 2 → `GTuple` over the group's `FTTuple` param, its elements rebuilt
    /// as anonymous `Wildcard` leaves (a contract names them, but the compiled form
    /// does not); arity-1 `unit` → `GUnit` (the `let f () = …` lone-erasable shape);
    /// arity-1 other → `GSimple`. Feeding the result to `compiledOf` keeps the
    /// flatten / lone-unit-erase / unit→void rule single-sourced — the cross-assembly
    /// consumer derives the same compiled form an in-assembly function does from its
    /// own `gather`ed `ValRepr`.
    let externalValRepr (typars: int) (groups: (int * FrozenType) list) (resultTy: FrozenType) : ValRepr =
        let groupOf (arity: int, pty: FrozenType) : ArgGroup =
            if arity >= 2 then
                match pty with
                | FTTuple elems ->
                    let items = elems |> EqArray.map (fun e -> TPatG.Wildcard(e, contractPatTok))
                    ArgGroupG.GTuple(TPatG.Tuple(items, pty, contractPatTok))
                | _ ->
                    // A ≥2-width group is always an `FTTuple` (translateArgsSpec); keep a
                    // single param defensively rather than fabricate one.
                    ArgGroupG.GSimple(mintSyntheticParamKey (), pty)
            elif isUnitFrozen pty then
                ArgGroupG.GUnit pty
            else
                ArgGroupG.GSimple(mintSyntheticParamKey (), pty)

        {
            Typars = typars
            Groups = groups |> List.map groupOf
            ResultTy = resultTy
        }

    /// Lower a decl list into a closure-bearing tree. Every `TExprG.Lambda` is a
    /// function value and every `External` is a call head or has non-function type —
    /// but that is now an INPUT invariant, not something this establishes.
    ///
    /// Nothing operator- or inline-shaped survives to here. `Passes.InlineExpansion`
    /// splices every `let inline` body (local and cross-package) by `SymbolKey`,
    /// beta-reduces it, resolves its `StaticOptimization` clauses, and eta-reifies
    /// every `External` used as a VALUE — including inside member bodies, which this
    /// lowering never walks. So the frozen decls arriving here carry no inline call
    /// heads, no `StaticOptimization` nodes, and no `External` function values.
    ///
    /// What is left is the flattening: drop inline TEMPLATES
    /// (`TDeclG.Let(isInline)`) and `type` decls (emitted as metadata), and split a
    /// nested `let` chain into a flat top-level decl list.
    let lower (decls: EqArray<Frozen.TDecl>) : Frozen.TDecl list =
        let rec lowerExpr (e: Frozen.TExpr) : Frozen.TExpr =
            match e with
            | TExprG.App _ ->
                let head, spineArgs = TastWalk.collectSpine [] e

                // An `External` head is a recipe call: it stays in call position (the
                // inline pass already spliced any head that had a body), and only the
                // args are lowered.
                let head' =
                    match head with
                    | TExprG.External _ -> head
                    | _ -> lowerExpr head

                TastWalk.rebuildApp head' [ for (a, t, tk) in spineArgs -> lowerExpr a, t, tk ]
            | _ -> mapChildren lowerExpr e

        // Split a folded top-level statement sequence back into standalone decls
        // in source order. The parser folds
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

        let result = ResizeArray<Frozen.TDecl>()

        let lowerOne (d: Frozen.TDecl) =
            match d with
            | TDeclG.Let(_, _, true, _) -> ()
            | TDeclG.Let(p, value, false, t) -> result.Add(TDeclG.Let(p, lowerExpr value, false, t))
            | TDeclG.Expression(e, t) -> result.Add(TDeclG.Expression(lowerExpr e, t))
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
