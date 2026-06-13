namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open XParsec.FSharp.SemanticAnalysis
open EmitTypes

module EmitLower =
    let typeOfExpr (e: Frozen.TExpr) : FrozenType =
        match e with
        | TExprG.Const(_, ty) -> ty
        | TExprG.Var(_, ty) -> ty
        | TExprG.External(_, _, ty) -> ty
        | TExprG.Lambda(_, _, ty) -> ty
        | TExprG.App(_, _, ty) -> ty
        | TExprG.Let(_, _, _, ty) -> ty
        | TExprG.Use(_, _, _, _, ty) -> ty
        | TExprG.IfThenElse(_, _, _, ty) -> ty
        | TExprG.Tuple(_, ty) -> ty
        | TExprG.Sequential(_, ty) -> ty
        | TExprG.While(_, _, ty) -> ty
        | TExprG.ForTo(_, _, _, _, ty) -> ty
        | TExprG.ForIn(_, _, _, _, ty) -> ty
        | TExprG.Match(_, _, ty) -> ty
        | TExprG.TryWith(_, _, ty) -> ty
        | TExprG.TryFinally(_, _, ty) -> ty
        | TExprG.Assignment(_, _, ty) -> ty
        | TExprG.Null ty -> ty
        | TExprG.Range(_, _, _, ty) -> ty
        | TExprG.RecordCons(_, ty) -> ty
        | TExprG.RecordClone(_, _, ty) -> ty
        | TExprG.FieldGet(_, _, ty) -> ty
        | TExprG.FieldSet(_, _, _, ty) -> ty
        | TExprG.UnionCons(_, _, ty) -> ty
        | TExprG.New(_, _, ty) -> ty
        | TExprG.MethodCall(_, _, _, _, ty) -> ty
        | TExprG.PropertyGet(_, _, _, ty) -> ty
        | TExprG.StaticMethodCall(_, _, ty) -> ty
        | TExprG.StaticPropertyGet(_, ty) -> ty
        | TExprG.StaticFieldGet(_, _, ty) -> ty
        | TExprG.ExternalMember(_, _, _, _, ty) -> ty
        | TExprG.Format(_, _, ty) -> ty
        | TExprG.ILIntrinsic(_, _, _, ty) -> ty
        | TExprG.StaticOptimization(_, _, ty) -> ty
        | TExprG.Upcast(_, ty) -> ty
        | TExprG.Downcast(_, ty) -> ty
        | TExprG.TraitCall(_, _, _, ty) -> ty
        | TExprG.TypeTest(_, _, ty) -> ty

    let typeOfPat (p: Frozen.TPat) : FrozenType =
        match p with
        | TPatG.NamedSimple(_, ty)
        | TPatG.Wildcard ty
        | TPatG.Tuple(_, ty)
        | TPatG.Const(_, ty)
        | TPatG.Record(_, ty)
        | TPatG.Union(_, _, ty)
        | TPatG.TypeTestAs(_, _, ty) -> ty

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
    let private mapChildren (f: Frozen.TExpr -> Frozen.TExpr) (e: Frozen.TExpr) : Frozen.TExpr =
        match e with
        | TExprG.Const _
        | TExprG.Var _
        | TExprG.External _
        | TExprG.Null _
        | TExprG.StaticPropertyGet _
        | TExprG.StaticFieldGet _ -> e
        | TExprG.Lambda(p, b, t) -> TExprG.Lambda(p, f b, t)
        | TExprG.App(fn, a, t) -> TExprG.App(f fn, f a, t)
        | TExprG.Let(p, v, b, t) -> TExprG.Let(p, f v, f b, t)
        | TExprG.Use(p, v, b, dispose, t) -> TExprG.Use(p, f v, f b, dispose, t)
        | TExprG.IfThenElse(c, th, el, t) -> TExprG.IfThenElse(f c, f th, f el, t)
        | TExprG.Tuple(xs, t) -> TExprG.Tuple(EqArray.map f xs, t)
        | TExprG.Sequential(xs, t) -> TExprG.Sequential(EqArray.map f xs, t)
        | TExprG.While(c, b, t) -> TExprG.While(f c, f b, t)
        | TExprG.ForTo(v, s, e2, b, t) -> TExprG.ForTo(v, f s, f e2, f b, t)
        | TExprG.ForIn(p, src, b, en, t) -> TExprG.ForIn(p, f src, f b, en, t)
        | TExprG.Match(sc, arms, t) ->
            TExprG.Match(
                f sc,
                arms
                |> EqArray.map (fun a ->
                    { a with
                        Guard = Option.map f a.Guard
                        Body = f a.Body
                    }
                ),
                t
            )
        | TExprG.TryWith(b, arms, t) ->
            TExprG.TryWith(
                f b,
                arms
                |> EqArray.map (fun a ->
                    { a with
                        Guard = Option.map f a.Guard
                        Body = f a.Body
                    }
                ),
                t
            )
        | TExprG.TryFinally(b, c, t) -> TExprG.TryFinally(f b, f c, t)
        | TExprG.Assignment(l, r, t) -> TExprG.Assignment(f l, f r, t)
        | TExprG.Range(s, step, stop, t) -> TExprG.Range(f s, Option.map f step, f stop, t)
        | TExprG.RecordCons(fields, t) -> TExprG.RecordCons(EqArray.map (fun (n, v) -> n, f v) fields, t)
        | TExprG.RecordClone(src, ov, t) -> TExprG.RecordClone(f src, EqArray.map (fun (n, v) -> n, f v) ov, t)
        | TExprG.FieldGet(r, n, t) -> TExprG.FieldGet(f r, n, t)
        | TExprG.FieldSet(r, n, v, t) -> TExprG.FieldSet(f r, n, f v, t)
        | TExprG.UnionCons(c, args, t) -> TExprG.UnionCons(c, EqArray.map f args, t)
        | TExprG.New(c, args, t) -> TExprG.New(c, EqArray.map f args, t)
        | TExprG.MethodCall(r, k, via, args, t) -> TExprG.MethodCall(f r, k, via, EqArray.map f args, t)
        | TExprG.PropertyGet(r, k, via, t) -> TExprG.PropertyGet(f r, k, via, t)
        | TExprG.StaticMethodCall(k, args, t) -> TExprG.StaticMethodCall(k, EqArray.map f args, t)
        | TExprG.ExternalMember(r, k, n, isProp, t) -> TExprG.ExternalMember(ValueOption.map f r, k, n, isProp, t)
        | TExprG.Format(sink, segs, t) ->
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

            TExprG.Format(sink, segs, t)
        | TExprG.ILIntrinsic(op, operand, args, t) -> TExprG.ILIntrinsic(op, operand, EqArray.map f args, t)
        | TExprG.StaticOptimization(clauses, def, t) ->
            TExprG.StaticOptimization(clauses |> EqArray.map (fun cl -> { cl with Body = f cl.Body }), f def, t)
        | TExprG.Upcast(src, t) -> TExprG.Upcast(f src, t)
        | TExprG.Downcast(src, t) -> TExprG.Downcast(f src, t)
        | TExprG.TraitCall(recv, n, args, t) -> TExprG.TraitCall(recv, n, EqArray.map f args, t)
        | TExprG.TypeTest(src, testTy, t) -> TExprG.TypeTest(f src, testTy, t)

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
    /// `args.[key]` dict still allocate the `ldarg.1` slot for the value the
    /// caller pushes without clashing with other binders. `Interlocked` keeps it
    /// safe across the parallel test runner.
    let mutable private paramSynthCounter = 0

    let private mintSyntheticParamKey () : NodeKey =
        let c = System.Threading.Interlocked.Increment(&paramSynthCounter)
        NodeKey.ofSynthetic c NodeKind.SynthLambdaBody

    let mintUnitParamKey () : NodeKey = mintSyntheticParamKey ()

    /// The placeholder key for a destructuring tuple lambda parameter — its
    /// `ldarg.1` `ValueTuple`n` value is `bindPattern`ed into the real leaf
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
    /// still allocates an `ldarg` slot for the unit value the caller pushes. A
    /// tuple binder likewise gets a synthetic `Slot` and carries its `Pat` so the
    /// emission `bindPattern`s the leaf bindings out of the `ldarg` value. Any
    /// other pattern stops the peel.
    let rec peelLambda (e: Frozen.TExpr) : StaticParam list * Frozen.TExpr =
        match e with
        | TExprG.Lambda(TPatG.NamedSimple(k, pty), body, _) ->
            let ps, b = peelLambda body
            { Slot = k; Ty = pty; Pat = None } :: ps, b
        | TExprG.Lambda(TPatG.Const(TConstValue.Unit, pty), body, _) ->
            let ps, b = peelLambda body

            {
                Slot = mintUnitParamKey ()
                Ty = pty
                Pat = None
            }
            :: ps,
            b
        | TExprG.Lambda((TPatG.Tuple(_, pty) as pat), body, _) ->
            let ps, b = peelLambda body

            {
                Slot = mintTupleParamKey ()
                Ty = pty
                Pat = Some pat
            }
            :: ps,
            b
        | _ -> [], e

    /// Fallback inline-IL bodies for built-in operators, expressed as the
    /// `TExprG.ILIntrinsic` the general path emits. A saturated `External(opName)`
    /// use site is rewritten to the matching body here by `expandBuiltinOps`, so
    /// codegen owns no per-operator dispatch. Bodies are monomorphic at the
    /// use-site type (primitive clauses share an opcode; no `when ^T : …`).
    ///
    /// DELETE-WHEN-COMPLETE: operator `.fs` bodies in `ops-platform.fs` now win on
    /// the primary path (the `=`/`<>` contract body is spliced pre-freeze by
    /// `Passes.InlineExpansion` at every ground use site). This table only still
    /// serves the residue the pass leaves: an *un-ground* operator operand
    /// (`let f a b = a = b`) and an eta-reified operator value, until those route
    /// through the inline bodies too.
    module private BuiltinOps =

        let private ilBin (op: string) : EqArray<Frozen.TExpr> -> FrozenType -> Frozen.TExpr =
            fun operands retTy -> TExprG.ILIntrinsic(op, ValueNone, operands, retTy)

        /// `not (# op … #)`, realised as `ceq (# op … #) false` — the derived ops
        /// with no direct opcode (`<>` = `not =`, `<=` = `not >`, `>=` = `not <`).
        let private ilBinNot (op: string) : EqArray<Frozen.TExpr> -> FrozenType -> Frozen.TExpr =
            fun operands retTy ->
                let inner = TExprG.ILIntrinsic(op, ValueNone, operands, retTy)

                TExprG.ILIntrinsic(
                    "ceq",
                    ValueNone,
                    EqArray.ofList [ inner; TExprG.Const(TConstValue.Bool false, retTy) ],
                    retTy
                )

        /// compiled name → (arity, body builder over the operand expressions).
        /// `&&` / `||` are intentionally absent — they short-circuit and freeze to
        /// `IfThenElse`, not an opcode. Ordering uses `clt`/`cgt` (IEEE on floats,
        /// O7); bitwise/shift use the signed/default IL form (the `ops-platform.fs`
        /// contract bodies, with narrow-int/unsigned refinements, win at a ground
        /// use site — these serve the un-ground fallback).
        let private table: Map<string, int * (EqArray<Frozen.TExpr> -> FrozenType -> Frozen.TExpr)> =
            Map
                [
                    "op_Equality", (2, ilBin "ceq")
                    "op_Inequality", (2, ilBinNot "ceq")
                    "op_LessThan", (2, ilBin "clt")
                    "op_GreaterThan", (2, ilBin "cgt")
                    "op_LessThanOrEqual", (2, ilBinNot "cgt")
                    "op_GreaterThanOrEqual", (2, ilBinNot "clt")
                    "op_Addition", (2, ilBin "add")
                    "op_Subtraction", (2, ilBin "sub")
                    "op_Multiply", (2, ilBin "mul")
                    "op_Division", (2, ilBin "div")
                    "op_Modulus", (2, ilBin "rem")
                    "op_UnaryNegation", (1, ilBin "neg")
                    "op_BitwiseAnd", (2, ilBin "and")
                    "op_BitwiseOr", (2, ilBin "or")
                    "op_ExclusiveOr", (2, ilBin "xor")
                    "op_LeftShift", (2, ilBin "shl")
                    "op_RightShift", (2, ilBin "shr")
                    "op_LogicalNot", (1, ilBin "not")
                ]

        /// True when `name` is a built-in operator applied to exactly its arity —
        /// the saturated use site rewritten to inline IL. A partial application
        /// (`(=) 1`) is left as a call head for the eta path.
        let isSaturated (name: string) (spineLen: int) : bool =
            match Map.tryFind name table with
            | Some(arity, _) -> spineLen = arity
            | None -> false

        /// Build the operator's inline-IL body, splicing the (already-rewritten)
        /// operand expressions directly. `retTy` is the application's result type.
        let buildApp (name: string) (opArgs: EqArray<Frozen.TExpr>) (retTy: FrozenType) : Frozen.TExpr =
            let _, makeInner = table.[name]
            makeInner opArgs retTy

    /// Rewrite every saturated built-in operator application to its inline-IL
    /// body, so it emits through the single `TExprG.ILIntrinsic` path. Run as the
    /// closing phase of `lower` and over type-member bodies (which never pass
    /// through `lower`). The splice is direct — each body uses each operand exactly
    /// once — so no binder is introduced and closure/free-variable analysis is
    /// undisturbed.
    let rec expandBuiltinOps (e: Frozen.TExpr) : Frozen.TExpr =
        match e with
        | TExprG.App _ ->
            let head, spine = TastWalk.collectSpine [] e

            match head with
            | TExprG.External(name, _, _) when BuiltinOps.isSaturated name (List.length spine) ->
                let retTy = snd (List.last spine)
                let opArgs = EqArray.ofSeq (seq { for (a, _) in spine -> expandBuiltinOps a })
                BuiltinOps.buildApp name opArgs retTy
            | _ -> mapChildren expandBuiltinOps e
        | _ -> mapChildren expandBuiltinOps e

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
    /// where closures are a codegen concept) and (2) the closing `expandBuiltinOps`
    /// pass that collapses every saturated built-in operator left un-ground by the
    /// inline pass (`13 &&& 11`, `a = b` with a generic operand) to inline IL.
    let lower (decls: EqArray<Frozen.TDecl>) : Frozen.TDecl list =
        // Build-wide monotone counter for eta parameters, so independent
        // eta-reifications never share a NodeKey.
        let mutable counter = 0

        let mint () =
            let k = NodeKey.ofSynthetic counter NodeKind.SynthInlineExpansion
            counter <- counter + 1
            k

        // Eta-reify `External(name, a -> … -> r)` used as a value into
        // `fun p0 -> … -> name p0 …`, turning a function name into a closure.
        let etaExpand (name: string) (ty: FrozenType) : Frozen.TExpr =
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
                | (k, pty) :: rest, FTFun(_, resTy) -> applyAll (TExprG.App(acc, TExprG.Var(k, pty), resTy)) resTy rest
                | _ -> failwith "Emit: eta-reification arity mismatch"

            let appBody = applyAll (TExprG.External(name, ValueNone, ty)) ty kts

            kts
            |> List.foldBack (fun (k, pty) (innerBody, innerTy) ->
                let lamTy = FTFun(pty, innerTy)
                TExprG.Lambda(TPatG.NamedSimple(k, pty), innerBody, lamTy), lamTy
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

                TastWalk.rebuildApp head' [ for (a, t) in spineArgs -> lowerExpr a, t ]
            | TExprG.External(name, _, ty) when isFunTy ty -> etaExpand name ty
            | _ -> mapChildren lowerExpr e

        // Eta lowering surfaces operator applications (an eta-reified `(+)`);
        // `expandBuiltinOps` then collapses every saturated one to inline IL — a
        // closing phase so it sees them all.
        let result = ResizeArray<Frozen.TDecl>()

        for d in decls do
            match d with
            | TDeclG.Let(_, _, true, _) -> ()
            | TDeclG.Let(p, value, false, t) -> result.Add(TDeclG.Let(p, expandBuiltinOps (lowerExpr value), false, t))
            | TDeclG.Expression(e, t) -> result.Add(TDeclG.Expression(expandBuiltinOps (lowerExpr e), t))
            // Type declarations are emitted as metadata, not through the expr stream.
            | TDeclG.Type _ -> ()

        List.ofSeq result
