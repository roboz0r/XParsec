namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open XParsec.FSharp.SemanticAnalysis

module EmitLower =
    let typeOfExpr (e: TExpr) : SemType =
        match e with
        | TExpr.Const(_, ty) -> ty
        | TExpr.Var(_, ty) -> ty
        | TExpr.External(_, _, ty) -> ty
        | TExpr.Lambda(_, _, ty) -> ty
        | TExpr.App(_, _, ty) -> ty
        | TExpr.Let(_, _, _, ty) -> ty
        | TExpr.Use(_, _, _, _, ty) -> ty
        | TExpr.IfThenElse(_, _, _, ty) -> ty
        | TExpr.Tuple(_, ty) -> ty
        | TExpr.Sequential(_, ty) -> ty
        | TExpr.While(_, _, ty) -> ty
        | TExpr.ForTo(_, _, _, _, ty) -> ty
        | TExpr.ForIn(_, _, _, _, ty) -> ty
        | TExpr.Match(_, _, ty) -> ty
        | TExpr.TryWith(_, _, ty) -> ty
        | TExpr.TryFinally(_, _, ty) -> ty
        | TExpr.Assignment(_, _, ty) -> ty
        | TExpr.Null ty -> ty
        | TExpr.Range(_, _, _, ty) -> ty
        | TExpr.RecordCons(_, ty) -> ty
        | TExpr.RecordClone(_, _, ty) -> ty
        | TExpr.FieldGet(_, _, ty) -> ty
        | TExpr.FieldSet(_, _, _, ty) -> ty
        | TExpr.UnionCons(_, _, ty) -> ty
        | TExpr.New(_, _, ty) -> ty
        | TExpr.MethodCall(_, _, _, _, ty) -> ty
        | TExpr.PropertyGet(_, _, _, ty) -> ty
        | TExpr.StaticMethodCall(_, _, ty) -> ty
        | TExpr.StaticPropertyGet(_, ty) -> ty
        | TExpr.StaticFieldGet(_, _, ty) -> ty
        | TExpr.ExternalMember(_, _, _, _, ty) -> ty
        | TExpr.Format(_, _, ty) -> ty
        | TExpr.ILIntrinsic(_, _, ty) -> ty
        | TExpr.StaticOptimization(_, _, ty) -> ty
        | TExpr.Upcast(_, ty) -> ty
        | TExpr.Downcast(_, ty) -> ty
        | TExpr.TypeTest(_, _, ty) -> ty

    let typeOfPat (p: TPat) : SemType =
        match p with
        | TPat.NamedSimple(_, ty)
        | TPat.Wildcard ty
        | TPat.Tuple(_, ty)
        | TPat.Const(_, ty)
        | TPat.Record(_, ty)
        | TPat.Union(_, _, ty) -> ty

    /// Resolve a nominal receiver type to its `(SymbolKey, type-args)` pair
    /// (was a projected string name). The
    /// project-local emitted-type tables key by this `SymbolKey` directly; the
    /// external provider lookups derive the qualified compiled name from it via
    /// `ExternalSymbols.qualifiedName`. Returns `ValueNone` if the type isn't a
    /// user-defined or external nominal type (e.g. a `TyVar` that should have been
    /// zonked away by now).
    let inline receiverShape (ty: SemType) : (SymbolKey * SemType list) voption =
        match ty with
        | TyUnion(n, args)
        | TyRecord(n, args)
        | TyClass(n, args) -> ValueSome(n, EqArray.toList args)
        | _ -> ValueNone

    /// Resolve a `SemType`'s `TypeVar` links to their representatives. A free
    /// `TypeVar` stays a `TyVar root`; a solved one resolves to its concrete shape.
    let rec zonk (t: SemType) : SemType =
        match t with
        | TyVar tv ->
            let r = UnionFind.find tv

            match r.Link with
            | ValueSome target -> zonk target
            | ValueNone -> TyVar r
        | TyFun(a, b) -> TyFun(zonk a, zonk b)
        | TyTuple xs -> TyTuple(EqArray.map zonk xs)
        | TyRecord(n, xs) -> TyRecord(n, EqArray.map zonk xs)
        | TyUnion(n, xs) -> TyUnion(n, EqArray.map zonk xs)
        | TyClass(n, xs) -> TyClass(n, EqArray.map zonk xs)
        | TyConst(n, xs) -> TyConst(n, EqArray.map zonk xs)
        | TyUnknown _ -> t
        // A frozen open typar is already ground (no links to chase).
        | TempTypar _ -> t

    /// Recover a generic static method's per-typar instantiation at a call site
    /// (R3): structurally match each declared parameter type (`defTys`, carrying
    /// the method's typar `TypeVar`s) against the actual argument type. First
    /// occurrence wins. A recursive self-call yields the method's own typars
    /// (encoded `!!i`); an external call yields concrete types.
    let matchInstantiation (typarCount: int) (defTys: SemType list) (actualTys: SemType list) : SemType list =
        let result = Array.create typarCount ValueNone

        let rec go (defT: SemType) (actT: SemType) =
            match zonk defT, zonk actT with
            // A freeze-quantified method typar (frozen-type-plan 2B): the index is on
            // the node, so the recovered instantiation is index-keyed. `act` may
            // itself be a `TempTypar(Method, j)` — the enclosing generic context's
            // typar — which the `MethodSpec` then encodes verbatim.
            | TempTypar(TyparAxis.Method, i), act ->
                if i >= 0 && i < typarCount && result.[i].IsNone then
                    result.[i] <- ValueSome act
            | TyFun(a1, r1), TyFun(a2, r2) ->
                go a1 a2
                go r1 r2
            | TyTuple xs, TyTuple ys when xs.Length = ys.Length ->
                for i in 0 .. xs.Length - 1 do
                    go xs.[i] ys.[i]
            | TyRecord(_, xs), TyRecord(_, ys) when xs.Length = ys.Length ->
                for i in 0 .. xs.Length - 1 do
                    go xs.[i] ys.[i]
            | TyUnion(_, xs), TyUnion(_, ys) when xs.Length = ys.Length ->
                for i in 0 .. xs.Length - 1 do
                    go xs.[i] ys.[i]
            | TyClass(_, xs), TyClass(_, ys) when xs.Length = ys.Length ->
                for i in 0 .. xs.Length - 1 do
                    go xs.[i] ys.[i]
            // A generic intrinsic (notably the array `[]<!!i>`) carries its element
            // structurally; recurse so the element typar is recovered.
            | TyConst(_, xs), TyConst(_, ys) when xs.Length = ys.Length ->
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
    let private mapChildren (f: TExpr -> TExpr) (e: TExpr) : TExpr =
        match e with
        | TExpr.Const _
        | TExpr.Var _
        | TExpr.External _
        | TExpr.Null _
        | TExpr.StaticPropertyGet _
        | TExpr.StaticFieldGet _ -> e
        | TExpr.Lambda(p, b, t) -> TExpr.Lambda(p, f b, t)
        | TExpr.App(fn, a, t) -> TExpr.App(f fn, f a, t)
        | TExpr.Let(p, v, b, t) -> TExpr.Let(p, f v, f b, t)
        | TExpr.Use(p, v, b, dispose, t) -> TExpr.Use(p, f v, f b, dispose, t)
        | TExpr.IfThenElse(c, th, el, t) -> TExpr.IfThenElse(f c, f th, f el, t)
        | TExpr.Tuple(xs, t) -> TExpr.Tuple(EqArray.map f xs, t)
        | TExpr.Sequential(xs, t) -> TExpr.Sequential(EqArray.map f xs, t)
        | TExpr.While(c, b, t) -> TExpr.While(f c, f b, t)
        | TExpr.ForTo(v, s, e2, b, t) -> TExpr.ForTo(v, f s, f e2, f b, t)
        | TExpr.ForIn(p, src, b, en, t) -> TExpr.ForIn(p, f src, f b, en, t)
        | TExpr.Match(sc, arms, t) ->
            TExpr.Match(
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
        | TExpr.TryWith(b, arms, t) ->
            TExpr.TryWith(
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
        | TExpr.TryFinally(b, c, t) -> TExpr.TryFinally(f b, f c, t)
        | TExpr.Assignment(l, r, t) -> TExpr.Assignment(f l, f r, t)
        | TExpr.Range(s, step, stop, t) -> TExpr.Range(f s, Option.map f step, f stop, t)
        | TExpr.RecordCons(fields, t) -> TExpr.RecordCons(EqArray.map (fun (n, v) -> n, f v) fields, t)
        | TExpr.RecordClone(src, ov, t) -> TExpr.RecordClone(f src, EqArray.map (fun (n, v) -> n, f v) ov, t)
        | TExpr.FieldGet(r, n, t) -> TExpr.FieldGet(f r, n, t)
        | TExpr.FieldSet(r, n, v, t) -> TExpr.FieldSet(f r, n, f v, t)
        | TExpr.UnionCons(c, args, t) -> TExpr.UnionCons(c, EqArray.map f args, t)
        | TExpr.New(c, args, t) -> TExpr.New(c, EqArray.map f args, t)
        | TExpr.MethodCall(r, k, via, args, t) -> TExpr.MethodCall(f r, k, via, EqArray.map f args, t)
        | TExpr.PropertyGet(r, k, via, t) -> TExpr.PropertyGet(f r, k, via, t)
        | TExpr.StaticMethodCall(k, args, t) -> TExpr.StaticMethodCall(k, EqArray.map f args, t)
        | TExpr.ExternalMember(r, k, n, isProp, t) -> TExpr.ExternalMember(ValueOption.map f r, k, n, isProp, t)
        | TExpr.Format(sink, segs, t) ->
            let sink =
                match sink with
                | FormatSink.ToWriter w -> FormatSink.ToWriter(f w)
                | FormatSink.ToBuilder w -> FormatSink.ToBuilder(f w)
                | other -> other

            let segs =
                segs
                |> EqArray.map (fun seg ->
                    match seg with
                    | FormatSeg.Lit _ -> seg
                    | FormatSeg.Hole(h, a) -> FormatSeg.Hole(h, f a)
                )

            TExpr.Format(sink, segs, t)
        | TExpr.ILIntrinsic(op, args, t) -> TExpr.ILIntrinsic(op, EqArray.map f args, t)
        | TExpr.StaticOptimization(clauses, def, t) ->
            TExpr.StaticOptimization(clauses |> EqArray.map (fun cl -> { cl with Body = f cl.Body }), f def, t)
        | TExpr.Upcast(src, t) -> TExpr.Upcast(f src, t)
        | TExpr.Downcast(src, t) -> TExpr.Downcast(f src, t)
        | TExpr.TypeTest(src, testTy, t) -> TExpr.TypeTest(f src, testTy, t)

    /// Reuses `mapChildren`, discarding the rebuilt tree — only the one-shot
    /// discovery / free-variable pre-passes call this.
    let iterChildren (f: TExpr -> unit) (e: TExpr) : unit =
        mapChildren
            (fun c ->
                f c
                c
            )
            e
        |> ignore

    /// Source of synthetic `NodeKey`s for unit-parameter binders (`fun () -> …`).
    /// The body never references the key, but a fresh per-call key lets the
    /// `args.[key]` dict still allocate an `ldarg` slot for the unit value the
    /// caller pushes without clashing with other binders. `Interlocked` keeps it
    /// safe across the parallel test runner.
    let mutable private unitParamSynthCounter = 0

    let mintUnitParamKey () : NodeKey =
        let c = System.Threading.Interlocked.Increment(&unitParamSynthCounter)
        NodeKey.ofSynthetic c NodeKind.SynthLambdaBody

    /// Peel a curried `Lambda` chain of simple (`NamedSimple`) or unit-pattern
    /// (`TPat.Const(Unit, _)`, from `fun () -> …`) parameters. A unit binder
    /// gets a synthetic placeholder `NodeKey` (the body never references it)
    /// so the static-method emission still allocates an `ldarg` slot for the
    /// unit value the caller pushes. Any other pattern stops the peel.
    let rec peelLambda (e: TExpr) : (NodeKey * SemType) list * TExpr =
        match e with
        | TExpr.Lambda(TPat.NamedSimple(k, pty), body, _) ->
            let ps, b = peelLambda body
            (k, pty) :: ps, b
        | TExpr.Lambda(TPat.Const(TConstValue.Unit, pty), body, _) ->
            let ps, b = peelLambda body
            (mintUnitParamKey (), pty) :: ps, b
        | _ -> [], e

    /// Fallback inline-IL bodies for built-in operators, expressed as the
    /// `TExpr.ILIntrinsic` the general path emits. A saturated `External(opName)`
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

        let private ilBin (op: string) : EqArray<TExpr> -> SemType -> TExpr =
            fun operands retTy -> TExpr.ILIntrinsic(op, operands, retTy)

        /// `not (# op … #)`, realised as `ceq (# op … #) false` — the derived ops
        /// with no direct opcode (`<>` = `not =`, `<=` = `not >`, `>=` = `not <`).
        let private ilBinNot (op: string) : EqArray<TExpr> -> SemType -> TExpr =
            fun operands retTy ->
                let inner = TExpr.ILIntrinsic(op, operands, retTy)
                TExpr.ILIntrinsic("ceq", EqArray.ofList [ inner; TExpr.Const(TConstValue.Bool false, retTy) ], retTy)

        /// compiled name → (arity, body builder over the operand expressions).
        /// `&&` / `||` are intentionally absent — they short-circuit and freeze to
        /// `IfThenElse`, not an opcode. Ordering uses `clt`/`cgt` (IEEE on floats,
        /// O7); bitwise/shift use the signed/default IL form (the `ops-platform.fs`
        /// contract bodies, with narrow-int/unsigned refinements, win at a ground
        /// use site — these serve the un-ground fallback).
        let private table: Map<string, int * (EqArray<TExpr> -> SemType -> TExpr)> =
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
        let buildApp (name: string) (opArgs: EqArray<TExpr>) (retTy: SemType) : TExpr =
            let _, makeInner = table.[name]
            makeInner opArgs retTy

    /// Rewrite every saturated built-in operator application to its inline-IL
    /// body, so it emits through the single `TExpr.ILIntrinsic` path. Run as the
    /// closing phase of `lower` and over type-member bodies (which never pass
    /// through `lower`). The splice is direct — each body uses each operand exactly
    /// once — so no binder is introduced and closure/free-variable analysis is
    /// undisturbed.
    let rec expandBuiltinOps (e: TExpr) : TExpr =
        match e with
        | TExpr.App _ ->
            let head, spine = TastWalk.collectSpine [] e

            match head with
            | TExpr.External(name, _, _) when BuiltinOps.isSaturated name (List.length spine) ->
                let retTy = snd (List.last spine)
                let opArgs = EqArray.ofSeq (seq { for (a, _) in spine -> expandBuiltinOps a })
                BuiltinOps.buildApp name opArgs retTy
            | _ -> mapChildren expandBuiltinOps e
        | _ -> mapChildren expandBuiltinOps e

    let private isFunTy (t: SemType) : bool =
        match t with
        | TyFun _ -> true
        | _ -> false

    /// Lower a decl list into a closure-bearing, External-value-free tree. After
    /// this, every `TExpr.Lambda` is a function value and every `External` is
    /// either a call head or has non-function type.
    ///
    /// Inline expansion (local + cross-package `let inline` splicing, beta
    /// reduction, `StaticOptimization` resolution) is no longer done here: it ran
    /// pre-freeze in `Passes.InlineExpansion` (frozen-type-plan 3A-1), so the
    /// frozen decls reaching codegen carry no `External(inlineName)` call heads and
    /// no `StaticOptimization` nodes. Inline TEMPLATES (`TDecl.Let(isInline)`) are
    /// still dropped here. What remains codegen-only is (1) eta-reifying an
    /// `External` function VALUE into a closure (it must run after the front end,
    /// where closures are a codegen concept) and (2) the closing `expandBuiltinOps`
    /// pass that collapses every saturated built-in operator left un-ground by the
    /// inline pass (`13 &&& 11`, `a = b` with a generic operand) to inline IL.
    let lower (decls: EqArray<TDecl>) : TDecl list =
        // Build-wide monotone counter for eta parameters, so independent
        // eta-reifications never share a NodeKey.
        let mutable counter = 0

        let mint () =
            let k = NodeKey.ofSynthetic counter NodeKind.SynthInlineExpansion
            counter <- counter + 1
            k

        // Eta-reify `External(name, a -> … -> r)` used as a value into
        // `fun p0 -> … -> name p0 …`, turning a function name into a closure.
        let etaExpand (name: string) (ty: SemType) : TExpr =
            let rec arrows t =
                match t with
                | TyFun(a, b) ->
                    let ps, r = arrows b
                    (a :: ps), r
                | _ -> [], t

            let paramTys, retTy = arrows ty
            let kts = paramTys |> List.map (fun pty -> mint (), pty)

            let rec applyAll acc accTy ks =
                match ks, accTy with
                | [], _ -> acc
                | (k, pty) :: rest, TyFun(_, resTy) -> applyAll (TExpr.App(acc, TExpr.Var(k, pty), resTy)) resTy rest
                | _ -> failwith "Emit: eta-reification arity mismatch"

            let appBody = applyAll (TExpr.External(name, ValueNone, ty)) ty kts

            kts
            |> List.foldBack (fun (k, pty) (innerBody, innerTy) ->
                let lamTy = TyFun(pty, innerTy)
                TExpr.Lambda(TPat.NamedSimple(k, pty), innerBody, lamTy), lamTy
            )
            <| (appBody, retTy)
            |> fst

        let rec lowerExpr (e: TExpr) : TExpr =
            match e with
            | TExpr.App _ ->
                let head, spineArgs = TastWalk.collectSpine [] e

                // An `External` head is a recipe / built-in-operator call, so it
                // stays in call position and is not eta-reified; the args are
                // values. The inline pass already expanded any spliceable head.
                let head' =
                    match head with
                    | TExpr.External _ -> head
                    | _ -> lowerExpr head

                TastWalk.rebuildApp head' [ for (a, t) in spineArgs -> lowerExpr a, t ]
            | TExpr.External(name, _, ty) when isFunTy ty -> etaExpand name ty
            | _ -> mapChildren lowerExpr e

        // Eta lowering surfaces operator applications (an eta-reified `(+)`);
        // `expandBuiltinOps` then collapses every saturated one to inline IL — a
        // closing phase so it sees them all.
        let result = ResizeArray<TDecl>()

        for d in decls do
            match d with
            | TDecl.Let(_, _, true, _) -> ()
            | TDecl.Let(p, value, false, t) -> result.Add(TDecl.Let(p, expandBuiltinOps (lowerExpr value), false, t))
            | TDecl.Expression(e, t) -> result.Add(TDecl.Expression(expandBuiltinOps (lowerExpr e), t))
            // Type declarations are emitted as metadata, not through the expr stream.
            | TDecl.Type _ -> ()

        List.ofSeq result
