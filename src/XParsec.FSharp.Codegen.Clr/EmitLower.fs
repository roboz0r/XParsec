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
        | TExpr.IfThenElse(_, _, _, ty) -> ty
        | TExpr.Tuple(_, ty) -> ty
        | TExpr.Sequential(_, ty) -> ty
        | TExpr.While(_, _, ty) -> ty
        | TExpr.ForTo(_, _, _, _, ty) -> ty
        | TExpr.ForIn(_, _, _, ty) -> ty
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
        | TExpr.MethodCall(_, _, _, ty) -> ty
        | TExpr.PropertyGet(_, _, ty) -> ty
        | TExpr.StaticMethodCall(_, _, _, ty) -> ty
        | TExpr.StaticPropertyGet(_, _, ty) -> ty
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

    /// Resolve a nominal receiver type to its (declared-name, type-args) pair.
    /// Returns `ValueNone` if the type isn't a user-defined or external nominal
    /// type (e.g., a `TyVar` that should have been zonked away by now). The
    /// `TyClass` arm passes through so future class receivers reach the right
    /// downstream lookup without revisiting every call site (H6).
    let inline receiverShape (ty: SemType) : (string * SemType list) voption =
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
        | TyConst _ -> t

    /// Recover a generic static method's per-typar instantiation at a call site
    /// (R3): structurally match each declared parameter type (`defTys`, carrying
    /// the method's typar `TypeVar`s) against the actual argument type. First
    /// occurrence wins. A recursive self-call yields the method's own typars
    /// (encoded `!!i`); an external call yields concrete types.
    let matchInstantiation (typars: TypeVar list) (defTys: SemType list) (actualTys: SemType list) : SemType list =
        let roots = typars |> List.map UnionFind.find
        let result = Array.create roots.Length ValueNone

        let rec go (defT: SemType) (actT: SemType) =
            match zonk defT, zonk actT with
            | TyVar tv, act ->
                let r = UnionFind.find tv

                match roots |> List.tryFindIndex (fun x -> System.Object.ReferenceEquals(x, r)) with
                | Some i ->
                    if result.[i].IsNone then
                        result.[i] <- ValueSome act
                | None -> ()
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
            | _ -> ()

        List.iter2 go defTys actualTys

        [
            for i in 0 .. roots.Length - 1 ->
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
        | TExpr.IfThenElse(c, th, el, t) -> TExpr.IfThenElse(f c, f th, f el, t)
        | TExpr.Tuple(xs, t) -> TExpr.Tuple(EqArray.map f xs, t)
        | TExpr.Sequential(xs, t) -> TExpr.Sequential(EqArray.map f xs, t)
        | TExpr.While(c, b, t) -> TExpr.While(f c, f b, t)
        | TExpr.ForTo(v, s, e2, b, t) -> TExpr.ForTo(v, f s, f e2, f b, t)
        | TExpr.ForIn(p, src, b, t) -> TExpr.ForIn(p, f src, f b, t)
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
        | TExpr.MethodCall(r, n, args, t) -> TExpr.MethodCall(f r, n, EqArray.map f args, t)
        | TExpr.PropertyGet(r, n, t) -> TExpr.PropertyGet(f r, n, t)
        | TExpr.StaticMethodCall(c, n, args, t) -> TExpr.StaticMethodCall(c, n, EqArray.map f args, t)
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

    /// Peel a curried `App` chain into its head and the arguments paired with
    /// each `App` node's *result* type.
    let rec collectSpine (acc: (TExpr * SemType) list) (e: TExpr) : TExpr * (TExpr * SemType) list =
        match e with
        | TExpr.App(fn, arg, ty) -> collectSpine ((arg, ty) :: acc) fn
        | head -> head, acc

    let private rebuildApp (head: TExpr) (args: (TExpr * SemType) list) : TExpr =
        List.fold (fun acc (arg, resTy) -> TExpr.App(acc, arg, resTy)) head args

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

    /// Beta-reduce a curried lambda (an inline expansion's output) against its
    /// spine args, lowering each application to a `TExpr.Let`. Lambda count must
    /// match spine-arg count for a fully applied call.
    let rec private betaReduce (fn: TExpr) (args: (TExpr * SemType) list) : TExpr =
        match fn, args with
        | _, [] -> fn
        | TExpr.Lambda(TPat.NamedSimple(k, paramTy), lamBody, _), (arg, _) :: rest ->
            let reduced = betaReduce lamBody rest
            TExpr.Let(TPat.NamedSimple(k, paramTy), arg, reduced, typeOfExpr reduced)
        | TExpr.Lambda(param, _, _), _ -> failwithf "Emit: inline parameter destructuring is out of scope: %A" param
        | _, _ :: _ -> failwith "Emit: over-application of an inline function"

    /// Fallback inline-IL bodies for built-in operators, expressed as the
    /// `TExpr.ILIntrinsic` the general path emits. A saturated `External(opName)`
    /// use site is rewritten to the matching body here by `expandBuiltinOps`, so
    /// codegen owns no per-operator dispatch. Bodies are monomorphic at the
    /// use-site type (primitive clauses share an opcode; no `when ^T : …`).
    ///
    /// DELETE-WHEN-COMPLETE: operator `.fs` bodies in `ops-platform.fs` now win on
    /// the primary path (the `=`/`<>` contract body is spliced by `lowerWith`
    /// first). This table only still serves union member-bodies and eta-reified
    /// operator values until those route through the inline bodies too.
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
            let head, spine = collectSpine [] e

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

    /// Does `e` contain a `TExpr.StaticOptimization` anywhere? An inline body that
    /// does must be expanded with the call site's type arguments (so the clause
    /// resolves against the monomorphised operand type); a body that doesn't keeps
    /// the existing zero-type-arg expansion path unchanged.
    let rec private containsStaticOpt (e: TExpr) : bool =
        match e with
        | TExpr.StaticOptimization _ -> true
        | _ ->
            let mutable found = false
            iterChildren (fun c -> found <- found || containsStaticOpt c) e
            found

    /// A (zonked) `SemType` with no free `TyVar` anywhere — fully monomorphic, so
    /// codegen can encode it. The cross-package equality/`hash` inline bodies reach
    /// `EqualityComparer<^T>`, which can only be emitted when `^T` is ground; an
    /// unpinned operand (`let f a b = a = b`) leaves it free and must fall back to
    /// `BuiltinOps` instead (type-args-bug.md DoD §3).
    let rec private isGroundType (t: SemType) : bool =
        match zonk t with
        | TyVar _ -> false
        | TyConst _ -> true
        | TyFun(a, b) -> isGroundType a && isGroundType b
        | TyTuple xs -> EqArray.forall isGroundType xs
        | TyRecord(_, xs)
        | TyUnion(_, xs)
        | TyClass(_, xs) -> EqArray.forall isGroundType xs

    /// Recover an inline binding's type arguments at a call site by matching its
    /// declared parameter types (carrying the quantified typars) against the actual
    /// spine-arg types — like `matchInstantiation`, but **tolerant**: a typar the
    /// params don't pin is left as its own `TyVar` so the catch-all `when ^T : ^T`
    /// clause still selects. Returned in `Inline.quantifiedTypars` order.
    let private deriveInlineTypeArgs (declTy: SemType) (spineArgs: (TExpr * SemType) list) : SemType[] =
        let typars = Inline.quantifiedTypars declTy

        if typars.Length = 0 then
            [||]
        else
            let roots = typars |> Array.map UnionFind.find
            let result = Array.create roots.Length ValueNone

            let rec go (defT: SemType) (actT: SemType) =
                match zonk defT, zonk actT with
                | TyVar tv, act ->
                    let r = UnionFind.find tv

                    match roots |> Array.tryFindIndex (fun x -> System.Object.ReferenceEquals(x, r)) with
                    | Some i ->
                        if result.[i].IsNone then
                            result.[i] <- ValueSome act
                    | None -> ()
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
                | _ -> ()

            let rec peelParams n t =
                if n <= 0 then
                    []
                else
                    match zonk t with
                    | TyFun(a, b) -> a :: peelParams (n - 1) b
                    | _ -> []

            let rec pairGo ps acts =
                match ps, acts with
                | p :: ps', a :: acts' ->
                    go p a
                    pairGo ps' acts'
                | _ -> ()

            let nArgs = List.length spineArgs

            pairGo (peelParams nArgs declTy) [ for (a, _) in spineArgs -> typeOfExpr a ]

            // Pair the result position too: `failwith`'s only typar `'T` sits in
            // the *return* (`string -> 'T`), so the param walk above leaves it
            // unbound. The last spine arg's recorded type is the whole
            // application's result (`collectSpine` pairs each arg with its
            // `App` node's result), so unifying it against `declTy`'s return
            // position grounds the result typars.
            let rec returnAfter n t =
                if n <= 0 then
                    t
                else
                    match zonk t with
                    | TyFun(_, b) -> returnAfter (n - 1) b
                    | _ -> t

            if nArgs > 0 then
                let declRetTy = returnAfter nArgs declTy
                let actualRetTy = spineArgs |> List.last |> snd
                go declRetTy actualRetTy

            Array.mapi
                (fun i v ->
                    match v with
                    | ValueSome t -> t
                    | ValueNone -> TyVar roots.[i]
                )
                result

    /// Splice cross-package inline bodies into a single expression — the subset of
    /// `lowerWith` that type-member bodies need (they are emitted straight from
    /// `tast.Decls` and never pass through `lower`, so a `failwith` / `raise` head
    /// would otherwise reach codegen un-spliced). External-head splice only — no
    /// local-inline expansion, eta-reification, or closure discovery. Compose with
    /// `expandBuiltinOps` (run this first, then the operator → IL rewrite).
    let spliceExternalInlinesInExpr (externalInlines: Map<string, TDecl>) (e: TExpr) : TExpr =
        if Map.isEmpty externalInlines then
            e
        else
            let mutable counter = 0

            let mint () =
                let k = NodeKey.ofSynthetic counter NodeKind.SynthInlineExpansion
                counter <- counter + 1
                k

            let expandAt (decl: TDecl) (spineArgs: (TExpr * SemType) list) : TExpr =
                match decl with
                | TDecl.Let(_, _, _, declTy) ->
                    Inline.inlineExpand decl (deriveInlineTypeArgs declTy spineArgs)
                    |> Inline.freshen mint
                | _ -> failwith "Emit: external inline body must be a TDecl.Let"

            let argsGround (decl: TDecl) (spineArgs: (TExpr * SemType) list) : bool =
                match decl with
                | TDecl.Let(_, _, _, declTy) -> deriveInlineTypeArgs declTy spineArgs |> Array.forall isGroundType
                | _ -> false

            let rec walk (e: TExpr) : TExpr =
                match e with
                | TExpr.App _ ->
                    let head, spineArgs = collectSpine [] e

                    match head with
                    | TExpr.External(name, _, _) when
                        externalInlines.ContainsKey name
                        && (argsGround externalInlines.[name] spineArgs
                            || not (BuiltinOps.isSaturated name (List.length spineArgs)))
                        ->
                        walk (betaReduce (expandAt externalInlines.[name] spineArgs) spineArgs)
                    | _ -> mapChildren walk e
                | _ -> mapChildren walk e

            walk e

    /// Lower a decl list into a closure-bearing, inline-free, External-value-free
    /// tree. After this, every `TExpr.Lambda` is a function value and every
    /// `External` is either a call head or has non-function type. Inline bindings
    /// are dropped (fully expanded at their use sites).
    ///
    /// `externalInlines` maps a referenced package's inline `val` name to its
    /// frozen body (a cross-package inline — milestone M, loaded by
    /// `SymbolProviders.inlineBodies`). A saturated `External(name)` call head
    /// found in that map is expanded in place exactly like a local `let inline`,
    /// so `hash 5` becomes the `EqualityComparer<int>.Default.GetHashCode 5`
    /// `ExternalMember` nodes the frozen body already carries (emitted by P4).
    let lowerWith (externalInlines: Map<string, TDecl>) (decls: EqArray<TDecl>) : TDecl list =
        let inlines = Dictionary<NodeKey, TDecl>()

        for d in decls do
            match d with
            | TDecl.Let(TPat.NamedSimple(b, _), _, true, _) -> inlines.[b] <- d
            | _ -> ()

        // Build-wide monotone counter for freshened inline binders and eta
        // parameters, so independent expansions never share a NodeKey.
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

        let expandInline (k: NodeKey) : TExpr =
            Inline.inlineExpand inlines.[k] [||] |> Inline.freshen mint

        // An inline whose body carries a static optimization is expanded with the
        // call site's type arguments so the `when ^T : …` clause resolves against
        // the monomorphised operand type (prereq 3); all other inlines keep the
        // zero-type-arg path (`expandInline`) unchanged.
        let expandInlineAt (k: NodeKey) (spineArgs: (TExpr * SemType) list) : TExpr =
            match inlines.[k] with
            | TDecl.Let(_, value, _, declTy) when containsStaticOpt value ->
                Inline.inlineExpand inlines.[k] (deriveInlineTypeArgs declTy spineArgs)
                |> Inline.freshen mint
            | _ -> expandInline k

        // A cross-package inline (milestone M): expand the frozen referenced body
        // with the call site's type arguments — always derived (unlike a local
        // inline, whose non-static-opt path leaves typars abstract), because the
        // body's `EqualityComparer<'T>` `ExternalMember` nodes need `'T` pinned to
        // a concrete type before P4 can encode them.
        let expandExternalInlineAt (decl: TDecl) (spineArgs: (TExpr * SemType) list) : TExpr =
            match decl with
            | TDecl.Let(_, _, _, declTy) ->
                Inline.inlineExpand decl (deriveInlineTypeArgs declTy spineArgs)
                |> Inline.freshen mint
            | _ -> failwith "Emit: external inline body must be a TDecl.Let"

        // A cross-package inline whose static-opt fall-clause rides
        // `EqualityComparer<^T>` (the `=`/`<>`/`hash` family) can only be expanded
        // when the call site pins `^T` to a ground type — otherwise the comparer
        // can't encode the free `!0`. An unpinned operand (`let f a b = a = b`)
        // leaves the External call head in place so the closing `expandBuiltinOps`
        // routes it to `Emit.BuiltinOps`'s `ceq` instead (type-args-bug.md DoD §3).
        let externalInlineArgsGround (decl: TDecl) (spineArgs: (TExpr * SemType) list) : bool =
            match decl with
            | TDecl.Let(_, _, _, declTy) -> deriveInlineTypeArgs declTy spineArgs |> Array.forall isGroundType
            | _ -> false

        let rec lowerExpr (e: TExpr) : TExpr =
            match e with
            | TExpr.App _ ->
                let head, spineArgs = collectSpine [] e

                match head with
                | TExpr.Var(k, _) when inlines.ContainsKey k ->
                    lowerExpr (betaReduce (expandInlineAt k spineArgs) spineArgs)
                | TExpr.External(name, _, _) when
                    externalInlines.ContainsKey name
                    && (externalInlineArgsGround externalInlines.[name] spineArgs
                        || not (BuiltinOps.isSaturated name (List.length spineArgs)))
                    ->
                    // Splice the inline body unless an *un-ground* operand could
                    // still benefit from the `BuiltinOps` fallback (the
                    // `EqualityComparer<^T>` encoding issue — type-args-bug.md
                    // DoD §3). An inline with no `BuiltinOps` recipe (`failwith`,
                    // `raise`) splices unconditionally: its body lowers to an
                    // `ILIntrinsic "throw"` whose IL doesn't reference the
                    // result typar, so an unground call-site type is fine.
                    lowerExpr (betaReduce (expandExternalInlineAt externalInlines.[name] spineArgs) spineArgs)
                | _ ->
                    // An `External` head is a recipe call, so it stays in call
                    // position and is not eta-reified; the args are values.
                    let head' =
                        match head with
                        | TExpr.External _ -> head
                        | _ -> lowerExpr head

                    rebuildApp head' [ for (a, t) in spineArgs -> lowerExpr a, t ]
            | TExpr.External(name, _, ty) when isFunTy ty -> etaExpand name ty
            | TExpr.Var(k, _) when inlines.ContainsKey k -> lowerExpr (expandInline k)
            | _ -> mapChildren lowerExpr e

        // Inline / eta lowering surfaces operator applications (an inline body's
        // `+`, an eta-reified `(+)`); `expandBuiltinOps` then collapses every
        // saturated one to inline IL — a closing phase so it sees them all.
        let result = ResizeArray<TDecl>()

        for d in decls do
            match d with
            | TDecl.Let(_, _, true, _) -> ()
            | TDecl.Let(p, value, false, t) -> result.Add(TDecl.Let(p, expandBuiltinOps (lowerExpr value), false, t))
            | TDecl.Expression(e, t) -> result.Add(TDecl.Expression(expandBuiltinOps (lowerExpr e), t))
            // Type declarations are emitted as metadata, not through the expr stream.
            | TDecl.Type _ -> ()

        List.ofSeq result

    /// `lowerWith` with no cross-package inline bodies — the pure-local-inline
    /// path (every caller that does not reference a manifest with `impl` bodies).
    let lower (decls: EqArray<TDecl>) : TDecl list = lowerWith Map.empty decls
