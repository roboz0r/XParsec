namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

// The TAST walker: `TExpr` → IL, via the depth-tracked untyped `Cil` helpers
// (the dynamic compiled-name → recipe dispatch can't preserve the phantom
// stack types across the provider boundary).
//
// A `lower` pre-pass expands every `inline` reference and eta-reifies every
// function-typed `External` used as a value into an explicit lambda chain — so
// afterwards every `TExpr.Lambda` is a function value, realised at runtime as
// an `FSharpFunc\`2` subclass. `discoverClosures` enumerates those lambdas
// leaves-first with their captured free variables. Variable resolution is
// per-method: in `Main` a `Var` is a local; in a closure `Invoke` it is the
// parameter (`ldarg.1`) or a capture (`ldarg.0; ldfld`).

module Emit =

    /// One `FSharpFunc\`2<ParamTy, ResultTy>` subclass. `Node` (the originating
    /// `TExpr.Lambda`) is matched by reference identity in the *lowered* tree
    /// that both discovery and emission share. `Captures` order = field order =
    /// ctor-arg order = the order pushed at the construction site.
    type Closure =
        {
            Node: TExpr
            Name: string
            ParamKey: NodeKey
            ParamTy: SemType
            ResultTy: SemType
            Body: TExpr
            Captures: (NodeKey * SemType) list
            /// The binding key of the `let [rec] f = <this lambda>` this is the
            /// value of, when any. A recursive self-reference resolves to `this`
            /// (`ldarg.0`) in the `Invoke` body rather than being captured, so
            /// there's no self-capture chicken-and-egg at construction.
            /// `ValueNone` for an anonymous lambda.
            SelfKey: NodeKey voption
        }

    /// One case of an emitted union: its runtime `Tag`, the static factory
    /// `TExpr.UnionCons` `call`s, and its payload field handles in declaration order.
    type EmittedCase =
        {
            Tag: int
            Factory: EntityHandle
            Fields: EntityHandle list
        }

    /// An augmentation member emitted onto a union's `TypeDefinition`. A
    /// property's `Handle` is its `get_<name>` method; `Arity` excludes `this`.
    /// `Handle` is the member's `Def` token — used directly for a monomorphic
    /// union. A *generic* union reaches the member through a `MemberRef` on the
    /// instantiated `TypeSpec` (R2), built from `MetaName` + the signature
    /// (`ParamTys` / `RetTy`, in declaring-typar markers).
    type EmittedMember =
        {
            Handle: EntityHandle
            IsStatic: bool
            Arity: int
            MetaName: string
            ParamTys: SemType list
            RetTy: SemType
        }

    /// A union emitted into this assembly: the shared `int` discriminant field,
    /// each case's emission handles, and its augmentation members by name.
    ///
    /// `Typars` is the declaring type's generic parameters; **empty ⇒ a
    /// monomorphic union** (a single sealed class with `Def`-token member
    /// access). A *generic* union reaches its members through
    /// `ICodegenProvider.GenericUnionMemberRef` (a `MemberRef` on the
    /// instantiated `TypeSpec`) rather than the `Def`-token `TagField` /
    /// `EmittedCase.Factory` / `EmittedCase.Fields` (valid only for the
    /// monomorphic case). `Name` is the registry key the provider mints refs against.
    type EmittedUnion =
        {
            Name: string
            Typars: string list
            TagField: EntityHandle
            Cases: Dictionary<string, EmittedCase>
            Members: Dictionary<string, EmittedMember>
        }

    /// A top-level function binding lowered to a **static method**:
    /// `let [rec] f p0 p1 … = body` becomes `static <ResultTy> f(p0, p1, …)`,
    /// the curried parameters flattened to method parameters. Eligible only when
    /// the function never escapes as a value and captures no module-level local —
    /// see `collectStaticFns`. A recursive self-call is a direct `call` of the
    /// method's own handle, so no closure self-reference is needed.
    type StaticFn =
        {
            Key: NodeKey
            Name: string
            /// `Some(namespace, holderName)` when this binding came from a named
            /// `module Foo = …` (R3 deferred): it emits as a public static method
            /// named `Name` on a `Foo`/`FooModule` holder type rather than on the
            /// anonymous "Program" holder (which carries the `None` functions). The
            /// front-end records this in `TastFile.ModuleMembers`.
            Holder: (string option * string) option
            Params: (NodeKey * SemType) list
            Body: TExpr
            ResultTy: SemType
        }

    /// The emission handle + shape of a static-method function, resolved before
    /// any body is built (the `MethodDefinition` handle is *predicted* from the
    /// row order). A call site `f a b` `call`s `Handle` with the first `Arity`
    /// args, then `Invoke`s the result with any remainder.
    ///
    /// A *generic* static method (`fold`, R3) carries its type-parameter
    /// `TypeVar`s (the free vars of its signature, by union-find root) and its
    /// declared parameter types: a call site recovers the per-typar instantiation
    /// by matching `ParamTys` against the actual argument types, then `call`s a
    /// `MethodSpec` instead of the bare `MethodDefinition`. Empty `Typars` ⇒ a
    /// monomorphic method (a plain `call`).
    type StaticMethodRef =
        {
            Handle: EntityHandle
            Arity: int
            ResultTy: SemType
            Typars: TypeVar list
            ParamTys: SemType list
        }

    let private typeOfExpr (e: TExpr) : SemType =
        match e with
        | TExpr.Const(_, ty) -> ty
        | TExpr.Var(_, ty) -> ty
        | TExpr.External(_, ty) -> ty
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
        | TExpr.Format(_, _, ty) -> ty

    let private typeOfPat (p: TPat) : SemType =
        match p with
        | TPat.NamedSimple(_, ty)
        | TPat.Wildcard ty
        | TPat.Tuple(_, ty)
        | TPat.Const(_, ty)
        | TPat.Record(_, ty)
        | TPat.Union(_, _, ty) -> ty

    /// Resolve a `SemType`'s `TypeVar` links to their representatives (the codegen
    /// project keeps its own copy rather than depend on the `Passes` namespace —
    /// the `ClrProvider` has the same private helper). A free `TypeVar` stays a
    /// `TyVar root`; a solved one resolves through to its concrete shape.
    let rec zonk (t: SemType) : SemType =
        match t with
        | TyVar tv ->
            let r = UnionFind.find tv

            match r.Link with
            | ValueSome target -> zonk target
            | ValueNone -> TyVar r
        | TyFun(a, b) -> TyFun(zonk a, zonk b)
        | TyTuple xs -> TyTuple(List.map zonk xs)
        | TyRecord(n, xs) -> TyRecord(n, List.map zonk xs)
        | TyUnion(n, xs) -> TyUnion(n, List.map zonk xs)
        | TyClass(n, xs) -> TyClass(n, List.map zonk xs)
        | TyConst _ -> t

    /// Recover a generic static method's per-typar instantiation at a call site
    /// (R3): structurally match each declared parameter type (`defTys`, carrying
    /// the method's typar `TypeVar`s) against the actual argument type, recording
    /// the actual sub-type that lands on each typar. First occurrence wins. For a
    /// recursive self-call the actuals reference the method's own typars, so the
    /// result is those typars (encoded `!!i`); for an external call they are
    /// concrete. `typars` is the ordered typar set (`StaticMethodRef.Typars`).
    let private matchInstantiation
        (typars: TypeVar list)
        (defTys: SemType list)
        (actualTys: SemType list)
        : SemType list =
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
            | TyTuple xs, TyTuple ys when xs.Length = ys.Length -> List.iter2 go xs ys
            | TyRecord(_, xs), TyRecord(_, ys) when xs.Length = ys.Length -> List.iter2 go xs ys
            | TyUnion(_, xs), TyUnion(_, ys) when xs.Length = ys.Length -> List.iter2 go xs ys
            | TyClass(_, xs), TyClass(_, ys) when xs.Length = ys.Length -> List.iter2 go xs ys
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
        | TExpr.StaticPropertyGet _ -> e
        | TExpr.Lambda(p, b, t) -> TExpr.Lambda(p, f b, t)
        | TExpr.App(fn, a, t) -> TExpr.App(f fn, f a, t)
        | TExpr.Let(p, v, b, t) -> TExpr.Let(p, f v, f b, t)
        | TExpr.IfThenElse(c, th, el, t) -> TExpr.IfThenElse(f c, f th, f el, t)
        | TExpr.Tuple(xs, t) -> TExpr.Tuple(List.map f xs, t)
        | TExpr.Sequential(xs, t) -> TExpr.Sequential(List.map f xs, t)
        | TExpr.While(c, b, t) -> TExpr.While(f c, f b, t)
        | TExpr.ForTo(v, s, e2, b, t) -> TExpr.ForTo(v, f s, f e2, f b, t)
        | TExpr.ForIn(p, src, b, t) -> TExpr.ForIn(p, f src, f b, t)
        | TExpr.Match(sc, arms, t) ->
            TExpr.Match(
                f sc,
                arms
                |> List.map (fun a ->
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
                |> List.map (fun a ->
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
        | TExpr.RecordCons(fields, t) -> TExpr.RecordCons([ for (n, v) in fields -> n, f v ], t)
        | TExpr.RecordClone(src, ov, t) -> TExpr.RecordClone(f src, [ for (n, v) in ov -> n, f v ], t)
        | TExpr.FieldGet(r, n, t) -> TExpr.FieldGet(f r, n, t)
        | TExpr.FieldSet(r, n, v, t) -> TExpr.FieldSet(f r, n, f v, t)
        | TExpr.UnionCons(c, args, t) -> TExpr.UnionCons(c, List.map f args, t)
        | TExpr.New(c, args, t) -> TExpr.New(c, List.map f args, t)
        | TExpr.MethodCall(r, n, args, t) -> TExpr.MethodCall(f r, n, List.map f args, t)
        | TExpr.PropertyGet(r, n, t) -> TExpr.PropertyGet(f r, n, t)
        | TExpr.StaticMethodCall(c, n, args, t) -> TExpr.StaticMethodCall(c, n, List.map f args, t)
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

    /// Reuses `mapChildren`, discarding the rebuilt tree — only the one-shot
    /// discovery / free-variable pre-passes call this.
    let private iterChildren (f: TExpr -> unit) (e: TExpr) : unit =
        mapChildren
            (fun c ->
                f c
                c
            )
            e
        |> ignore

    /// Peel a curried `App` chain into its head and the arguments paired with
    /// each `App` node's *result* type.
    let rec private collectSpine (acc: (TExpr * SemType) list) (e: TExpr) : TExpr * (TExpr * SemType) list =
        match e with
        | TExpr.App(fn, arg, ty) -> collectSpine ((arg, ty) :: acc) fn
        | head -> head, acc

    let private rebuildApp (head: TExpr) (args: (TExpr * SemType) list) : TExpr =
        List.fold (fun acc (arg, resTy) -> TExpr.App(acc, arg, resTy)) head args

    /// Peel a curried `Lambda` chain of simple (`NamedSimple`) parameters. A
    /// non-`NamedSimple` parameter (or a non-lambda) stops the peel, so the
    /// "arity" is the count of leading simple-param lambdas.
    let rec private peelLambda (e: TExpr) : (NodeKey * SemType) list * TExpr =
        match e with
        | TExpr.Lambda(TPat.NamedSimple(k, pty), body, _) ->
            let ps, b = peelLambda body
            (k, pty) :: ps, b
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

    // ---- Lowering: inline expansion + External-as-value eta-reification ----

    let private isFunTy (t: SemType) : bool =
        match t with
        | TyFun _ -> true
        | _ -> false

    /// Lower a decl list into a closure-bearing, inline-free, External-value-free
    /// tree. After this, every `TExpr.Lambda` is a function value and every
    /// `External` is either a call head or has non-function type. Inline bindings
    /// are dropped (fully expanded at their use sites).
    let lower (decls: TDecl list) : TDecl list =
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

            let appBody = applyAll (TExpr.External(name, ty)) ty kts

            kts
            |> List.foldBack (fun (k, pty) (innerBody, innerTy) ->
                let lamTy = TyFun(pty, innerTy)
                TExpr.Lambda(TPat.NamedSimple(k, pty), innerBody, lamTy), lamTy
            )
            <| (appBody, retTy)
            |> fst

        let expandInline (k: NodeKey) : TExpr =
            Inline.inlineExpand inlines.[k] [||] |> Inline.freshen mint

        let rec lowerExpr (e: TExpr) : TExpr =
            match e with
            | TExpr.App _ ->
                let head, spineArgs = collectSpine [] e

                match head with
                | TExpr.Var(k, _) when inlines.ContainsKey k -> lowerExpr (betaReduce (expandInline k) spineArgs)
                | _ ->
                    // An `External` head is a recipe call, so it stays in call
                    // position and is not eta-reified; the args are values.
                    let head' =
                        match head with
                        | TExpr.External _ -> head
                        | _ -> lowerExpr head

                    rebuildApp head' [ for (a, t) in spineArgs -> lowerExpr a, t ]
            | TExpr.External(name, ty) when isFunTy ty -> etaExpand name ty
            | TExpr.Var(k, _) when inlines.ContainsKey k -> lowerExpr (expandInline k)
            | _ -> mapChildren lowerExpr e

        decls
        |> List.choose (fun d ->
            match d with
            | TDecl.Let(_, _, true, _) -> None
            | TDecl.Let(p, value, false, t) -> Some(TDecl.Let(p, lowerExpr value, false, t))
            | TDecl.Expression(e, t) -> Some(TDecl.Expression(lowerExpr e, t))
            // Type declarations are emitted as metadata, not through the expr stream.
            | TDecl.Type _ -> None
        )

    // ---- Closure discovery + capture analysis ----

    let private patKeys (p: TPat) : NodeKey list =
        let rec go p =
            match p with
            | TPat.NamedSimple(k, _) -> [ k ]
            | TPat.Wildcard _
            | TPat.Const _ -> []
            | TPat.Tuple(items, _) -> List.collect go items
            | TPat.Record(fields, _) -> fields |> List.collect (fun (_, sub) -> go sub)
            | TPat.Union(_, fields, _) -> List.collect go fields

        go p

    /// The free variables of a closure body, in first-occurrence order — drives
    /// capture field order. `staticFnKeys` are excluded: a reference to a
    /// static-method function is a direct `call`, not a captured value.
    let private freeVars
        (staticFnKeys: HashSet<NodeKey>)
        (paramKey: NodeKey)
        (selfKey: NodeKey voption)
        (body: TExpr)
        : (NodeKey * SemType) list =
        let bound = HashSet<NodeKey>()
        bound.Add paramKey |> ignore
        bound.UnionWith staticFnKeys // static-method references are calls, not captures

        match selfKey with
        | ValueSome k -> bound.Add k |> ignore // the recursive self isn't captured — it's `this`
        | ValueNone -> ()

        let acc = ResizeArray<NodeKey * SemType>()
        let seen = HashSet<NodeKey>()

        let scoped (keys: NodeKey list) (k: unit -> unit) =
            let added = keys |> List.filter bound.Add
            k ()

            for key in added do
                bound.Remove key |> ignore

        let rec go (e: TExpr) =
            match e with
            | TExpr.Var(key, ty) ->
                if not (bound.Contains key) && seen.Add key then
                    acc.Add(key, ty)
            | TExpr.Lambda(p, b, _) -> scoped (patKeys p) (fun () -> go b)
            | TExpr.Let(p, v, b, _) ->
                go v
                scoped (patKeys p) (fun () -> go b)
            | TExpr.ForTo(var, s, e2, b, _) ->
                go s
                go e2
                scoped [ var ] (fun () -> go b)
            | TExpr.ForIn(p, src, b, _) ->
                go src
                scoped (patKeys p) (fun () -> go b)
            | TExpr.Match(sc, arms, _) ->
                go sc

                for arm in arms do
                    scoped
                        (patKeys arm.Pat)
                        (fun () ->
                            arm.Guard |> Option.iter go
                            go arm.Body
                        )
            | TExpr.TryWith(b, arms, _) ->
                go b

                for arm in arms do
                    scoped
                        (patKeys arm.Pat)
                        (fun () ->
                            arm.Guard |> Option.iter go
                            go arm.Body
                        )
            | _ -> iterChildren go e

        go body
        List.ofSeq acc

    // ---- Static-method classification (P3b) ----

    /// Like `freeVars` but keeps only keys (no types, no static-method exclusion):
    /// the capture test in `collectStaticFns` must *see* every referenced binding.
    let private freeVarKeys (boundKeys: NodeKey seq) (body: TExpr) : HashSet<NodeKey> =
        let bound = HashSet<NodeKey>(boundKeys)
        let acc = HashSet<NodeKey>()

        let scoped (keys: NodeKey list) (k: unit -> unit) =
            let added = keys |> List.filter bound.Add
            k ()

            for key in added do
                bound.Remove key |> ignore

        let rec go (e: TExpr) =
            match e with
            | TExpr.Var(key, _) ->
                if not (bound.Contains key) then
                    acc.Add key |> ignore
            | TExpr.Lambda(p, b, _) -> scoped (patKeys p) (fun () -> go b)
            | TExpr.Let(p, v, b, _) ->
                go v
                scoped (patKeys p) (fun () -> go b)
            | TExpr.ForTo(var, s, e2, b, _) ->
                go s
                go e2
                scoped [ var ] (fun () -> go b)
            | TExpr.ForIn(p, src, b, _) ->
                go src
                scoped (patKeys p) (fun () -> go b)
            | TExpr.Match(sc, arms, _) ->
                go sc

                for arm in arms do
                    scoped
                        (patKeys arm.Pat)
                        (fun () ->
                            arm.Guard |> Option.iter go
                            go arm.Body
                        )
            | TExpr.TryWith(b, arms, _) ->
                go b

                for arm in arms do
                    scoped
                        (patKeys arm.Pat)
                        (fun () ->
                            arm.Guard |> Option.iter go
                            go arm.Body
                        )
            | _ -> iterChildren go e

        go body
        acc

    /// Classify which top-level function bindings can be emitted as **static
    /// methods** rather than closures. A candidate is `let [rec] f p0 … = body`
    /// whose value peels to at least one simple parameter. Eligible only when:
    ///   1. it never *escapes* — every use is a saturated call, so it is never
    ///      needed as a function value (a bare/under-applied reference forces a closure).
    ///   2. it captures no module-level local — its free variables (minus its
    ///      parameters and self) are all themselves eligible static functions
    ///      (direct `call`s). A value-local reference would need a capture field,
    ///      which a static method has no `this` to hold.
    /// Rule 2 is a fixpoint, resolved by removing offenders until stable.
    let collectStaticFns
        (moduleMembers: Map<uint64, ModuleMemberInfo>)
        (decls: TDecl list)
        : StaticFn list * HashSet<NodeKey> =
        let candidates = Dictionary<NodeKey, (NodeKey * SemType) list * TExpr>()
        let order = ResizeArray<NodeKey>()

        for d in decls do
            match d with
            | TDecl.Let(TPat.NamedSimple(k, _), value, _, _) ->
                match peelLambda value with
                | (_ :: _ as ps), body ->
                    candidates.[k] <- (ps, body)
                    order.Add k
                | [], _ -> ()
            | _ -> ()

        let arity k =
            let ps, _ = candidates.[k]
            List.length ps

        // Escape analysis: a candidate used as a value or under-applied escapes.
        let escapes = HashSet<NodeKey>()

        let rec walkUses (e: TExpr) =
            match e with
            | TExpr.Var(k, _) when candidates.ContainsKey k -> escapes.Add k |> ignore
            | TExpr.App _ ->
                let head, args = collectSpine [] e

                match head with
                | TExpr.Var(k, _) when candidates.ContainsKey k ->
                    if List.length args < arity k then
                        escapes.Add k |> ignore

                    for (a, _) in args do
                        walkUses a
                | _ ->
                    walkUses head

                    for (a, _) in args do
                        walkUses a
            | _ -> iterChildren walkUses e

        for d in decls do
            match d with
            | TDecl.Let(_, value, _, _) -> walkUses value
            | TDecl.Expression(e, _) -> walkUses e
            | TDecl.Type _ -> ()

        // Each candidate's capture set (free vars minus its own params), tested by rule 2.
        let bodyFree =
            Dictionary<NodeKey, HashSet<NodeKey>>(
                seq {
                    for k in order do
                        let ps, body = candidates.[k]
                        KeyValuePair(k, freeVarKeys (ps |> List.map fst) body)
                }
            )

        // Fixpoint: from the non-escaping candidates, drop any whose free vars
        // reach outside the eligible set (own self-reference allowed).
        let eligible = HashSet<NodeKey>(order |> Seq.filter (escapes.Contains >> not))
        let mutable changed = true

        while changed do
            changed <- false

            for k in List.ofSeq eligible do
                let free = bodyFree.[k]

                let captures = free |> Seq.exists (fun v -> v <> k && not (eligible.Contains v))

                if captures && eligible.Remove k then
                    changed <- true

        let staticFns =
            [
                for k in order do
                    if eligible.Contains k then
                        let ps, body = candidates.[k]

                        // A binding inside a named module emits with its source
                        // name on its holder type (R3 deferred); a top-level
                        // function keeps the anonymous `fn$<offset>` name on the
                        // "Program" holder (`Holder = None`).
                        let name, holder =
                            match Map.tryFind k.Raw moduleMembers with
                            | Some info -> info.Name, Some(info.Namespace, info.Holder)
                            | None -> sprintf "fn$%d" k.Offset, None

                        yield
                            {
                                Key = k
                                Name = name
                                Holder = holder
                                Params = ps
                                Body = body
                                ResultTy = typeOfExpr body
                            }
            ]

        staticFns, eligible

    /// Enumerate every `Lambda` in the lowered tree leaves-first (a closure
    /// before any closure that constructs it), with its capture set; the returned
    /// dictionary maps each lambda node (by reference) to its `Closure`.
    /// `staticFnKeys`' outer lambdas are *not* closures (only their bodies are
    /// walked for inner closures), since a reference to one is a direct call.
    let discoverClosures
        (staticFnKeys: HashSet<NodeKey>)
        (decls: TDecl list)
        : Closure list * Dictionary<TExpr, Closure> =
        let order = ResizeArray<TExpr>()
        let lookup = Dictionary<TExpr, Closure>(HashIdentity.Reference)
        let mutable counter = 0

        // `selfKey` is the binding key when this node is the immediate value of a
        // `let f = …` lambda — a recursive self-reference resolves to `this`.
        let rec go (selfKey: NodeKey voption) (e: TExpr) =
            (match e with
             | TExpr.Let(TPat.NamedSimple(k, _), (TExpr.Lambda _ as v), body, _) ->
                 go (ValueSome k) v
                 go ValueNone body
             | _ -> iterChildren (go ValueNone) e) // children (and inner lambdas) first → leaves-first

            match e with
            | TExpr.Lambda(TPat.NamedSimple(p, pty), body, lamTy) ->
                let resultTy =
                    match lamTy with
                    | TyFun(_, r) -> r
                    | _ -> failwithf "Emit: closure type is not a function: %A" lamTy

                let c =
                    {
                        Node = e
                        Name = sprintf "<closure>$%d" counter
                        ParamKey = p
                        ParamTy = pty
                        ResultTy = resultTy
                        Body = body
                        Captures = freeVars staticFnKeys p selfKey body
                        SelfKey = selfKey
                    }

                counter <- counter + 1
                lookup.[e] <- c
                order.Add e
            | TExpr.Lambda(p, _, _) -> failwithf "Emit: closure parameter destructuring is out of scope: %A" p
            | _ -> ()

        for d in decls do
            match d with
            // A static-method function's lambda is not a closure, but its body
            // may still construct inner closures — walk only the body.
            | TDecl.Let(TPat.NamedSimple(k, _), value, _, _) when staticFnKeys.Contains k ->
                let _, body = peelLambda value
                go ValueNone body
            | TDecl.Let(TPat.NamedSimple(k, _), value, _, _) -> go (ValueSome k) value
            | TDecl.Let(_, value, _, _) -> go ValueNone value
            | TDecl.Expression(e, _) -> go ValueNone e
            | TDecl.Type _ -> ()

        [ for n in order -> lookup.[n] ], lookup

    // ---- The walker ----

    /// Per-method codegen context. `Slots` maps the current method's locals to
    /// slot indices; `Args` maps a method parameter to its `ldarg` index (closure
    /// `Invoke`: `this` is 0, the parameter 1; static method: flattened params
    /// 0…N-1; `Main`: none). `CaptureFields` resolves a closure's captures. The
    /// shared dictionaries `ClosureByNode` / `CtorHandleByNode` / `StaticMethods`
    /// resolve a `Lambda` value, its (leaves-first) ctor handle, and a top-level
    /// function reference to a direct `call`.
    type private EmitEnv =
        {
            Provider: ICodegenProvider
            Ctx: MetadataContext
            Slots: Dictionary<NodeKey, int>
            ClosureByNode: Dictionary<TExpr, Closure>
            CtorHandleByNode: Dictionary<TExpr, EntityHandle>
            Args: Dictionary<NodeKey, int>
            /// The recursive self of the current closure `Invoke` body — resolved
            /// to `this` (`ldarg.0`). `ValueNone` in `Main` / a static method
            /// (whose self-call is a direct `call`).
            SelfKey: NodeKey voption
            CaptureFields: Dictionary<NodeKey, EntityHandle>
            Unions: Dictionary<string, EmittedUnion>
            StaticMethods: Dictionary<NodeKey, StaticMethodRef>
        }

    let private applyRecipe (il: Il) (recipe: CallRecipe) : unit =
        recipe.Emit il
        il.Adjust(recipe.Pushes - recipe.ArgCount)

    /// Load a variable for the current method: a method parameter (`ldarg.i`),
    /// the recursive self of a closure (`this`, `ldarg.0`), a capture
    /// (`ldarg.0; ldfld`), or a local slot (`ldloc`).
    let private emitVarLoad (env: EmitEnv) (il: Il) (key: NodeKey) : unit =
        match env.Args.TryGetValue key with
        | true, i -> Cil.emitLdarg il i
        | false, _ ->

            match env.SelfKey with
            | ValueSome s when s = key -> Cil.emitLdarg il 0 // `this` — the recursive self
            | _ ->
                match env.CaptureFields.TryGetValue key with
                | true, field ->
                    Cil.emitLdarg il 0
                    Cil.emitLdfld il field
                | false, _ ->
                    match env.Slots.TryGetValue key with
                    | true, slot -> Cil.emitLdloc il slot
                    | false, _ -> failwithf "Emit: no binding for variable %O" key

    /// Test a pattern against the value already stored in local `scrutSlot`:
    /// branch to `nextLabel` on mismatch, and bind any pattern variables. A
    /// `Const` compares (`bne.un` skips the arm); `Wildcard` / `NamedSimple`
    /// always match (the latter aliases its binding to `scrutSlot`, so
    /// `emitVarLoad` resolves it to the same local — no copy). Union / tuple /
    /// record patterns land in later rung-2 slices.
    let rec private emitMatchTest (env: EmitEnv) (il: Il) (scrutSlot: int) (nextLabel: LabelHandle) (pat: TPat) : unit =
        match pat with
        | TPat.Wildcard _ -> ()
        | TPat.NamedSimple(binding, _) -> env.Slots.[binding] <- scrutSlot
        | TPat.Const(value, _) ->
            Cil.emitLdloc il scrutSlot

            match value with
            | TConstValue.Int n -> Cil.emitLdcI4 il n
            | TConstValue.Bool b -> Cil.emitLdcI4 il (if b then 1 else 0)
            | TConstValue.Byte n -> Cil.emitLdcI4 il (int n)
            | TConstValue.Char c -> Cil.emitLdcI4 il (int c)
            | other -> failwithf "Emit: match on constant %A is out of scope" other

            Cil.emitBneUn il nextLabel
        | TPat.Union(caseName, subPats, ty) ->
            let typeName, tyArgs =
                match ty with
                | TyUnion(n, xs)
                | TyRecord(n, xs) -> n, xs
                | other -> failwithf "Emit: union pattern with non-union type %A" other

            match env.Unions.TryGetValue typeName with
            | true, u ->
                let c = u.Cases.[caseName]

                // Tag / field access is a `Def` token for a monomorphic union, but
                // a `MemberRef` on the instantiated `TypeSpec` for a generic one
                // (`List<int>::_tag` etc.) — see `EmittedUnion.Typars` (P3d.4).
                let tagRef =
                    if List.isEmpty u.Typars then
                        u.TagField
                    else
                        env.Provider.GenericUnionMemberRef(typeName, tyArgs, UnionMember.Tag)

                // Skip the arm unless `scrut._tag = case.Tag`.
                Cil.emitLdloc il scrutSlot
                Cil.emitLdfld il tagRef
                Cil.emitLdcI4 il c.Tag
                Cil.emitBneUn il nextLabel

                // Extract each non-wildcard field into a fresh local, then test
                // its sub-pattern (a named sub-pattern just aliases that local).
                subPats
                |> List.iteri (fun i subPat ->
                    match subPat with
                    | TPat.Wildcard _ -> ()
                    | _ ->
                        let fieldRef =
                            if List.isEmpty u.Typars then
                                c.Fields.[i]
                            else
                                env.Provider.GenericUnionMemberRef(typeName, tyArgs, UnionMember.Field(caseName, i))

                        let fldSlot = il.DeclareLocal(typeOfPat subPat)
                        Cil.emitLdloc il scrutSlot
                        Cil.emitLdfld il fieldRef
                        Cil.emitStloc il fldSlot
                        emitMatchTest env il fldSlot nextLabel subPat
                )
            | false, _ -> failwithf "Emit: no emitted union for match on '%s'" typeName
        | other -> failwithf "Emit: match pattern is out of scope: %A" other

    /// The fallthrough a `match` reaches when no arm matched — `throw new
    /// System.Exception("…")`. An exhaustive match never reaches it at runtime,
    /// but it keeps the emitted IL well-formed (and gives a non-exhaustive one
    /// defined behaviour).
    let private emitMatchFailure (env: EmitEnv) (il: Il) : unit =
        Cil.emitLdstr il (env.Ctx.UserString "The match cases were incomplete")
        Cil.emitNewobj il env.Provider.ExceptionCtor 1
        Cil.emitThrow il

    /// Resolve the member-call handle for an instance access on `receiverTy`
    /// (P3d.3, generalised to generic unions in R2). A monomorphic union uses the
    /// member's `Def` token directly; a *generic* union goes through a `MemberRef`
    /// on the receiver's instantiated `TypeSpec` (`List<int>::get_Head`). Only
    /// emitted unions carry members today (records / concrete classes are P3e), so
    /// a non-union receiver is a gap.
    let private resolveInstanceMember (env: EmitEnv) (receiverTy: SemType) (name: string) : EntityHandle =
        let typeName, tyArgs =
            match receiverTy with
            | TyUnion(n, xs)
            | TyRecord(n, xs) -> n, xs
            | other -> failwithf "Emit: member '%s' access on non-union receiver %A" name other

        match env.Unions.TryGetValue typeName with
        | true, u ->
            match u.Members.TryGetValue name with
            | true, m ->
                if List.isEmpty u.Typars then
                    m.Handle
                else
                    env.Provider.GenericUnionMemberRef(
                        typeName,
                        tyArgs,
                        UnionMember.Member(m.MetaName, false, m.ParamTys, m.RetTy)
                    )
            | false, _ -> failwithf "Emit: union '%s' has no emitted member '%s'" typeName name
        | false, _ -> failwithf "Emit: no emitted union for member access on '%s'" typeName

    /// The static-member equivalent. Generic-union *static* augmentation members
    /// are out of scope in R2 (a static member's typars aren't tied to the type's
    /// via `this`, so the front-end leaves them un-remapped — the type's generic
    /// `Cons` / `Empty` come from its case factories instead), so a generic union
    /// fails here loudly rather than minting a malformed `Def` call.
    let private resolveStaticMember (env: EmitEnv) (typeName: string) (name: string) : EntityHandle =
        match env.Unions.TryGetValue typeName with
        | true, u ->
            match u.Members.TryGetValue name with
            | true, m ->
                if List.isEmpty u.Typars then
                    m.Handle
                else
                    failwithf
                        "Emit: generic-union static augmentation member '%s.%s' is out of scope (R2)"
                        typeName
                        name
            | false, _ -> failwithf "Emit: union '%s' has no emitted static member '%s'" typeName name
        | false, _ -> failwithf "Emit: no emitted union for static member access on '%s'" typeName

    /// `failwith "msg"` resolves through the symbol provider to this name; the
    /// backend lowers it to a BCL-only `throw new System.Exception(msg)` (P3d.3),
    /// rather than the FSharp.Core `Operators.FailWith` recipe.
    let private isFailwith (name: string) : bool =
        name = "failwith" || name.EndsWith ".failwith" || name.EndsWith "FailWith"

    /// The cold printf path (`printfn "%A"` …). Its `PrintFormatLine` recipe leaves
    /// an FSharp.Core `FSharpFunc` printer on the stack, so the printer is applied
    /// via `FSharpFunc::Invoke`, not `Vesper.Fun::Invoke` (R1 leaves this FSharp.Core
    /// island alone until the printf engine — handoff §R9).
    let private isColdPrintf (name: string) : bool =
        name = "printfn" || name.EndsWith ".printfn"

    let rec private emitExpr (env: EmitEnv) (il: Il) (e: TExpr) : unit =
        match e with
        | TExpr.Const(TConstValue.String s, _) -> Cil.emitLdstr il (env.Ctx.UserString s)
        | TExpr.Const(TConstValue.Int n, _) -> Cil.emitLdcI4 il n
        | TExpr.Const(TConstValue.Bool b, _) -> Cil.emitLdcI4 il (if b then 1 else 0)
        | TExpr.Const(TConstValue.Byte n, _) -> Cil.emitLdcI4 il (int n)
        | TExpr.Const(TConstValue.Float x, _) -> Cil.emitLdcR8 il x
        | TExpr.Const(TConstValue.Char c, _) -> Cil.emitLdcI4 il (int c)
        | TExpr.Const(TConstValue.Decimal d, _) ->
            // Materialise via `Decimal..ctor(lo, mid, hi, isNegative, scale)` from
            // the value's bit representation — the same shape F#/Roslyn emit.
            let bits = System.Decimal.GetBits d
            let flags = bits.[3]
            Cil.emitLdcI4 il bits.[0] // lo
            Cil.emitLdcI4 il bits.[1] // mid
            Cil.emitLdcI4 il bits.[2] // hi
            Cil.emitLdcI4 il (if flags < 0 then 1 else 0) // sign (high bit of flags)
            Cil.emitLdcI4 il ((flags >>> 16) &&& 0xFF) // scale
            Cil.emitNewobj il env.Provider.DecimalCtor 5

        | TExpr.Var(binding, _) -> emitVarLoad env il binding

        | TExpr.Let(TPat.NamedSimple(binding, ty), value, body, _) ->
            let slot = il.DeclareLocal ty
            env.Slots.[binding] <- slot
            emitExpr env il value
            Cil.emitStloc il slot
            emitExpr env il body
        | TExpr.Let(pat, _, _, _) -> failwithf "Emit: destructuring let-binding is out of scope: %A" pat

        | TExpr.Sequential(items, _) ->
            // Every item but the last is a unit-typed statement: emit it and
            // discard whatever value it leaves (popping back to the pre-item
            // depth); the last item leaves the sequence's result.
            let n = List.length items

            items
            |> List.iteri (fun i it ->
                if i = n - 1 then
                    emitExpr env il it
                else
                    let baseDepth = il.Depth
                    emitExpr env il it

                    while il.Depth > baseDepth do
                        Cil.emitPop il
            )

        | TExpr.IfThenElse(cond, thenExpr, elseExpr, _) ->
            // `<cond>; brfalse else; <then>; br end; else: <else>; end:`. Both
            // arms leave one value, so the linear depth tracker (which follows
            // only the then-arm) is reset to the post-`brfalse` base before the
            // else-arm — see `Il.SetDepth`.
            let elseLabel = Cil.defineLabel il
            let endLabel = Cil.defineLabel il
            emitExpr env il cond
            Cil.emitBrFalse il elseLabel
            let baseDepth = il.Depth
            emitExpr env il thenExpr
            Cil.emitBr il endLabel
            il.SetDepth baseDepth
            Cil.markLabel il elseLabel
            emitExpr env il elseExpr
            Cil.markLabel il endLabel

        | TExpr.Match(scrutinee, arms, _) ->
            // Evaluate the scrutinee once into a local, then test each arm in
            // order: on a mismatch branch to the next arm; on a match (and a
            // passing guard) emit the body and branch to the shared end. The
            // depth tracker is reset to the post-scrutinee base before each arm
            // and before the end label (every body leaves one result) — see
            // `Il.SetDepth`.
            let scrutSlot = il.DeclareLocal(typeOfExpr scrutinee)
            emitExpr env il scrutinee
            Cil.emitStloc il scrutSlot
            let baseDepth = il.Depth
            let endLabel = Cil.defineLabel il

            for arm in arms do
                let nextLabel = Cil.defineLabel il
                emitMatchTest env il scrutSlot nextLabel arm.Pat

                match arm.Guard with
                | Some g ->
                    emitExpr env il g
                    Cil.emitBrFalse il nextLabel
                | None -> ()

                emitExpr env il arm.Body
                Cil.emitBr il endLabel
                il.SetDepth baseDepth
                Cil.markLabel il nextLabel

            emitMatchFailure env il
            il.SetDepth(baseDepth + 1)
            Cil.markLabel il endLabel

        | TExpr.Lambda _ ->
            // A function value: construct its closure. Captures are pushed via
            // the *current* resolver (a local in `Main`, the param or a capture
            // inside an enclosing closure), then `newobj` its ctor.
            match env.ClosureByNode.TryGetValue e with
            | true, closure ->
                for (k, _) in closure.Captures do
                    emitVarLoad env il k

                match env.CtorHandleByNode.TryGetValue e with
                | true, ctor -> Cil.emitNewobj il ctor (List.length closure.Captures)
                | false, _ -> failwith "Emit: closure constructor not yet emitted (leaves-first ordering broken)"
            | false, _ -> failwith "Emit: a Lambda value was not discovered as a closure"

        | TExpr.New(className, args, ty) ->
            for a in args do
                emitExpr env il a

            let tyArgs =
                match ty with
                | TyClass(_, xs) -> xs
                | _ -> []

            match env.Provider.TryEmitCtor(className, tyArgs) with
            | ValueSome recipe -> Cil.emitNewobj il recipe.Handle recipe.ArgCount
            | ValueNone -> failwithf "Emit: no constructor recipe for '%s'" className

        | TExpr.App _ ->
            let head, spineArgs = collectSpine [] e

            match head with
            | TExpr.External(name, _) when isFailwith name ->
                // `failwith "msg"` → `ldstr msg; newobj System.Exception(string);
                // throw`. BCL-only (the provider's `ExceptionCtor`), and terminal
                // — `throw` ends the path, so it tolerates a value position the
                // way a non-exhaustive `match` fallthrough does (P3d.3).
                match spineArgs with
                | (arg, _) :: _ ->
                    emitExpr env il arg
                    Cil.emitNewobj il env.Provider.ExceptionCtor 1
                    Cil.emitThrow il
                | [] -> failwith "Emit: failwith with no argument"
            | TExpr.External(name, _) ->
                // The recipe reads its generic instantiation from the head's
                // full curried type (`fnTy`).
                match env.Provider.TryEmitCall(name, typeOfExpr head) with
                | ValueSome recipe ->
                    let leading, rest = List.splitAt recipe.ArgCount spineArgs

                    for (a, _) in leading do
                        emitExpr env il a

                    applyRecipe il recipe

                    // Whatever the recipe left on the stack — a function value
                    // the rest of the spine is applied to.
                    let funcTy =
                        match List.tryLast leading with
                        | Some(_, ty) -> ty
                        | None -> typeOfExpr head

                    // The cold printf printer is an FSharp.Core `FSharpFunc`, so it
                    // is applied via `FSharpFunc::Invoke`; every other recipe result
                    // is a native `Vesper.Fun` (R1).
                    if isColdPrintf name then
                        foldInvokeFSharpFunc env il funcTy rest
                    else
                        foldInvoke env il funcTy rest
                | ValueNone -> failwithf "Emit: no call recipe for external '%s'" name

            | TExpr.Var(k, _) when env.StaticMethods.ContainsKey k ->
                // A top-level function emitted as a static method (P3b): `call`
                // it with the first `Arity` args (always present — a non-saturated
                // use would have escaped to a closure, see `collectStaticFns`),
                // then `Invoke` the result with any remainder. A *generic* static
                // method (R3) `call`s a `MethodSpec` instantiating it — recovered
                // by matching its declared parameter types against the actual
                // argument types (recursion yields the method's own typars ⇒ `!!i`).
                let sm = env.StaticMethods.[k]
                let leading, rest = List.splitAt sm.Arity spineArgs

                for (a, _) in leading do
                    emitExpr env il a

                let callHandle =
                    if List.isEmpty sm.Typars then
                        sm.Handle
                    else
                        // Each spine arg's *own* type (`collectSpine` pairs it with
                        // the application's *result* type instead), matched against
                        // the declared parameter types to recover the instantiation.
                        let actualTys = leading |> List.map (fun (a, _) -> typeOfExpr a)
                        let inst = matchInstantiation sm.Typars sm.ParamTys actualTys
                        env.Provider.StaticFnMethodSpec(sm.Handle, inst)

                Cil.emitCall il callHandle sm.Arity 1
                foldInvoke env il sm.ResultTy rest

            | _ ->
                // The head is itself a function value (a closure local or a
                // partially applied result): emit it, then `Invoke` each arg.
                emitExpr env il head
                foldInvoke env il (typeOfExpr head) spineArgs

        | TExpr.UnionCons(caseName, args, ty) ->
            let typeName, tyArgs =
                match ty with
                | TyRecord(n, xs)
                | TyUnion(n, xs) -> n, xs
                | other -> failwithf "Emit: UnionCons with non-union type %A" other

            for a in args do
                emitExpr env il a

            match env.Unions.TryGetValue typeName with
            | true, u ->
                // Our own emitted union: `call` the case's static factory (the
                // fields are already on the stack in declaration order). A
                // monomorphic factory is a `Def` token; a generic one is a
                // `MemberRef` on the instantiated `TypeSpec` (`List<int>::Cons`).
                let factoryRef =
                    if List.isEmpty u.Typars then
                        u.Cases.[caseName].Factory
                    else
                        env.Provider.GenericUnionMemberRef(typeName, tyArgs, UnionMember.Factory caseName)

                Cil.emitCall il factoryRef (List.length args) 1
            | false, _ ->
                // The provider's special-case (FSharp.Core list) for `[]` / `::`.
                match env.Provider.TryEmitUnionCons(typeName, caseName, tyArgs) with
                | ValueSome recipe -> applyRecipe il recipe
                | ValueNone -> failwithf "Emit: no union-cons recipe for %s.%s" typeName caseName

        | TExpr.PropertyGet(receiver, name, _) ->
            // Instance property read (P3d.3): load the receiver, `call` the
            // union's `get_<name>` (the receiver is its sole argument). On a
            // generic union the call goes through a `MemberRef` on the receiver's
            // `TypeSpec` (`List<int>::get_Head`) (R2).
            let handle = resolveInstanceMember env (typeOfExpr receiver) name
            emitExpr env il receiver
            Cil.emitCall il handle 1 1

        | TExpr.MethodCall(receiver, name, args, _) ->
            // Instance method call (P3d.3): receiver then args, `call` the
            // member (non-virtual — the union is sealed).
            let handle = resolveInstanceMember env (typeOfExpr receiver) name
            emitExpr env il receiver

            for a in args do
                emitExpr env il a

            Cil.emitCall il handle (1 + List.length args) 1

        | TExpr.StaticPropertyGet(className, name, _) ->
            let handle = resolveStaticMember env className name
            Cil.emitCall il handle 0 1

        | TExpr.StaticMethodCall(className, name, args, _) ->
            let handle = resolveStaticMember env className name

            for a in args do
                emitExpr env il a

            Cil.emitCall il handle (List.length args) 1

        | TExpr.Format(sink, segments, _) -> emitFormat env il sink segments

        | other -> failwithf "Emit: unsupported expression: %A" other

    /// Lower a `TExpr.Format` to the `Vesper.Formatter` write-through handler: a
    /// ref-struct local constructed in place, then each segment folded
    /// left-to-right (`AppendLiteral` for a literal run, `AppendFormatted<T>`
    /// for a hole — its arg evaluated *here*, at its position), then a trailing
    /// newline (printfn-style sinks) and flush, or `ToStringAndClear` for the
    /// string sink. The node yields a value: the `unit` (null) of the writing
    /// sinks, or the result string of `sprintf`. Not a `CallRecipe` — the recipe
    /// model can't interleave literals/args around a ref-struct local + sink.
    and private emitFormat (env: EmitEnv) (il: Il) (sink: FormatSink) (segments: EqArray<FormatSeg>) : unit =
        let fh = env.Provider.FormatHandles()
        let slot = il.DeclareLocal fh.HandlerLocal

        // Capacity hints for the ctor; the handler grows past them as needed, so
        // they need not be exact.
        let mutable litLen = 0
        let mutable holeCount = 0

        for seg in segments do
            match seg with
            | FormatSeg.Lit s -> litLen <- litLen + s.Length
            | FormatSeg.Hole _ -> holeCount <- holeCount + 1

        // Construct in place: `ldloca h; ldc litLen; ldc holeCount; <sink?>; call .ctor`.
        Cil.emitLdloca il slot
        Cil.emitLdcI4 il litLen
        Cil.emitLdcI4 il holeCount

        match sink with
        | FormatSink.ToString -> Cil.emitCall il fh.CtorString 3 0
        | FormatSink.ToStdOut _ ->
            Cil.emitCall il fh.ConsoleOut 0 1
            Cil.emitCall il fh.CtorWriter 4 0
        | FormatSink.ToStdErr _ ->
            Cil.emitCall il fh.ConsoleError 0 1
            Cil.emitCall il fh.CtorWriter 4 0
        | FormatSink.ToWriter w ->
            emitExpr env il w
            Cil.emitCall il fh.CtorWriter 4 0
        | FormatSink.ToBuilder _ -> failwith "Emit: bprintf (ToBuilder) is not yet supported"

        for seg in segments do
            match seg with
            | FormatSeg.Lit s ->
                Cil.emitLdloca il slot
                Cil.emitLdstr il (env.Ctx.UserString s)
                Cil.emitCall il fh.AppendLiteral 2 0
            | FormatSeg.Hole(hole, arg) ->
                match hole.Kind with
                | PrintfSpec.HoleKind.Formatted ->
                    Cil.emitLdloca il slot
                    emitExpr env il arg

                    // Push optional args in the C# parameter order: alignment, then format.
                    match hole.Alignment with
                    | Some a -> Cil.emitLdcI4 il a
                    | None -> ()

                    match hole.Format with
                    | Some f -> Cil.emitLdstr il (env.Ctx.UserString f)
                    | None -> ()

                    let handle = fh.AppendFormatted(hole.Ty, hole.Alignment.IsSome, hole.Format.IsSome)

                    let argc =
                        2
                        + (if hole.Alignment.IsSome then 1 else 0)
                        + (if hole.Format.IsSome then 1 else 0)

                    Cil.emitCall il handle argc 0

                | PrintfSpec.HoleKind.BoolText
                | PrintfSpec.HoleKind.Octal
                | PrintfSpec.HoleKind.Unsigned ->
                    // A dedicated handler member `(value, int alignment)` — no
                    // .NET format string. The alignment is always pushed (0 ⇒ no
                    // padding); `%u`'s `int`→`uint` is a free CLI-stack
                    // reinterpret, so the arg is emitted unchanged.
                    let handle =
                        match hole.Kind with
                        | PrintfSpec.HoleKind.BoolText -> fh.AppendBool
                        | PrintfSpec.HoleKind.Octal -> fh.AppendOctal
                        | _ -> fh.AppendUnsigned

                    Cil.emitLdloca il slot
                    emitExpr env il arg
                    Cil.emitLdcI4 il (defaultArg hole.Alignment 0)
                    Cil.emitCall il handle 3 0

                | PrintfSpec.HoleKind.ZeroPaddedFloat ->
                    // `AppendZeroPaddedFloat(value, "F<prec>", width)` — the
                    // `"F<prec>"` body rides in `Format`, the field width in
                    // `Alignment` (both guaranteed present by `tryHoleFormat`).
                    let fmt =
                        match hole.Format with
                        | Some f -> f
                        | None -> failwith "Emit: ZeroPaddedFloat hole missing its format string"

                    let width =
                        match hole.Alignment with
                        | Some w -> w
                        | None -> failwith "Emit: ZeroPaddedFloat hole missing its width"

                    Cil.emitLdloca il slot
                    emitExpr env il arg
                    Cil.emitLdstr il (env.Ctx.UserString fmt)
                    Cil.emitLdcI4 il width
                    Cil.emitCall il fh.AppendZeroPaddedFloat 4 0

        match sink with
        | FormatSink.ToString ->
            // Leaves the built string on the stack (the `sprintf` result).
            Cil.emitLdloca il slot
            Cil.emitCall il fh.ToStringAndClear 1 1
        | FormatSink.ToStdOut nl
        | FormatSink.ToStdErr nl ->
            if nl then
                Cil.emitLdloca il slot
                Cil.emitLdstr il (env.Ctx.UserString "\n")
                Cil.emitCall il fh.AppendLiteral 2 0

            Cil.emitLdloca il slot
            Cil.emitCall il fh.Flush 1 0
            Cil.emitLdnull il // unit value
        | FormatSink.ToWriter _ ->
            Cil.emitLdloca il slot
            Cil.emitCall il fh.Flush 1 0
            Cil.emitLdnull il // unit value
        | FormatSink.ToBuilder _ -> failwith "Emit: bprintf (ToBuilder) is not yet supported"

    /// Apply each remaining argument to the function value on the stack via
    /// `FSharpFunc.Invoke`, threading the running function type.
    and private foldInvoke (env: EmitEnv) (il: Il) (funcTy0: SemType) (args: (TExpr * SemType) list) : unit =
        let mutable funcTy = funcTy0

        for (arg, resTy) in args do
            match env.Provider.TryEmitInvoke funcTy with
            | ValueSome recipe ->
                emitExpr env il arg
                applyRecipe il recipe
                funcTy <- resTy
            | ValueNone -> failwithf "Emit: cannot apply argument to value of type %A" funcTy

    /// Apply a curried FSharp.Core `FSharpFunc` value (the cold printf printer)
    /// argument by argument via `FSharpFunc::Invoke` — the FSharpFunc twin of
    /// `foldInvoke` (R1; retargeted with the printf engine, handoff §R9).
    and private foldInvokeFSharpFunc (env: EmitEnv) (il: Il) (funcTy0: SemType) (args: (TExpr * SemType) list) : unit =
        let mutable funcTy = funcTy0

        for (arg, resTy) in args do
            match env.Provider.TryEmitFSharpFuncInvoke funcTy with
            | ValueSome recipe ->
                emitExpr env il arg
                applyRecipe il recipe
                funcTy <- resTy
            | ValueNone -> failwithf "Emit: cannot apply argument to FSharpFunc value of type %A" funcTy

    /// Emit an expression as a statement: evaluate it and discard any value.
    let private emitStatement (env: EmitEnv) (il: Il) (e: TExpr) : unit =
        emitExpr env il e

        while il.Depth > 0 do
            Cil.emitPop il

    /// Build the `Main` body from the *lowered* decls. Each top-level `let`
    /// binds a `Main` local — except a function lowered to a static method (P3b),
    /// which has no value here; each effectful expression is emitted in source
    /// order; then `ldc.i4.0; ret`. (Inline bindings were removed by `lower`.)
    let emitMain
        (provider: ICodegenProvider)
        (ctx: MetadataContext)
        (closureByNode: Dictionary<TExpr, Closure>)
        (ctorHandleByNode: Dictionary<TExpr, EntityHandle>)
        (unions: Dictionary<string, EmittedUnion>)
        (staticMethods: Dictionary<NodeKey, StaticMethodRef>)
        (decls: TDecl list)
        (il: Il)
        : unit =
        let env =
            {
                Provider = provider
                Ctx = ctx
                Slots = Dictionary<NodeKey, int>()
                ClosureByNode = closureByNode
                CtorHandleByNode = ctorHandleByNode
                Args = Dictionary<NodeKey, int>()
                SelfKey = ValueNone
                CaptureFields = Dictionary<NodeKey, EntityHandle>()
                Unions = unions
                StaticMethods = staticMethods
            }

        for d in decls do
            match d with
            | TDecl.Expression(e, _) -> emitStatement env il e
            // A function emitted as a static method has no Main local.
            | TDecl.Let(TPat.NamedSimple(binding, _), _, _, _) when staticMethods.ContainsKey binding -> ()
            | TDecl.Let(TPat.NamedSimple(binding, _), value, _, ty) ->
                let slot = il.DeclareLocal ty
                env.Slots.[binding] <- slot
                emitExpr env il value
                Cil.emitStloc il slot
            | TDecl.Let _ -> ()
            | TDecl.Type _ -> ()

        Cil.emitLdcI4 il 0
        Cil.emitRet il

    /// Build a closure's `Invoke` body: evaluate its (lowered) body under a
    /// resolver mapping the parameter to `ldarg.1` and each capture to its
    /// field, leaving the result on the stack, then `ret`.
    let emitClosureInvoke
        (provider: ICodegenProvider)
        (ctx: MetadataContext)
        (closureByNode: Dictionary<TExpr, Closure>)
        (ctorHandleByNode: Dictionary<TExpr, EntityHandle>)
        (unions: Dictionary<string, EmittedUnion>)
        (staticMethods: Dictionary<NodeKey, StaticMethodRef>)
        (closure: Closure)
        (captureFields: Dictionary<NodeKey, EntityHandle>)
        (il: Il)
        : unit =
        let args = Dictionary<NodeKey, int>()
        args.[closure.ParamKey] <- 1 // `this` is 0; the single applied parameter is 1

        let env =
            {
                Provider = provider
                Ctx = ctx
                Slots = Dictionary<NodeKey, int>()
                ClosureByNode = closureByNode
                CtorHandleByNode = ctorHandleByNode
                Args = args
                SelfKey = closure.SelfKey
                CaptureFields = captureFields
                Unions = unions
                StaticMethods = staticMethods
            }

        emitExpr env il closure.Body
        Cil.emitRet il

    /// Build a static-method function's body (P3b): bind each flattened
    /// parameter to its `ldarg` index (a static method has no `this`, so the
    /// first parameter is `ldarg.0`), evaluate the body leaving its result on the
    /// stack, then `ret`. A recursive self-call resolves to a direct `call`
    /// through `staticMethods` (the `App` arm), so no self-binding is needed.
    let emitStaticMethod
        (provider: ICodegenProvider)
        (ctx: MetadataContext)
        (closureByNode: Dictionary<TExpr, Closure>)
        (ctorHandleByNode: Dictionary<TExpr, EntityHandle>)
        (unions: Dictionary<string, EmittedUnion>)
        (staticMethods: Dictionary<NodeKey, StaticMethodRef>)
        (fn: StaticFn)
        (il: Il)
        : unit =
        let args = Dictionary<NodeKey, int>()
        fn.Params |> List.iteri (fun i (k, _) -> args.[k] <- i)

        let env =
            {
                Provider = provider
                Ctx = ctx
                Slots = Dictionary<NodeKey, int>()
                ClosureByNode = closureByNode
                CtorHandleByNode = ctorHandleByNode
                Args = args
                SelfKey = ValueNone
                CaptureFields = Dictionary<NodeKey, EntityHandle>()
                Unions = unions
                StaticMethods = staticMethods
            }

        emitExpr env il fn.Body
        Cil.emitRet il

    /// Build a union augmentation member's body (P3d.3). An instance member's
    /// `this` is `ldarg.0` (`thisKey`), its parameters `ldarg.1…`; a static
    /// member's parameters start at `ldarg.0`. The body leaves its result on the
    /// stack, then `ret`. Member bodies don't synthesise closures (the closure
    /// discovery pass walks only value/expression decls), so an empty
    /// closure/ctor map is passed.
    let emitMember
        (provider: ICodegenProvider)
        (ctx: MetadataContext)
        (closureByNode: Dictionary<TExpr, Closure>)
        (ctorHandleByNode: Dictionary<TExpr, EntityHandle>)
        (unions: Dictionary<string, EmittedUnion>)
        (staticMethods: Dictionary<NodeKey, StaticMethodRef>)
        (thisKey: NodeKey voption)
        (prms: (NodeKey * SemType) list)
        (body: TExpr)
        (il: Il)
        : unit =
        let args = Dictionary<NodeKey, int>()

        let baseIdx =
            match thisKey with
            | ValueSome k ->
                args.[k] <- 0 // `this`
                1
            | ValueNone -> 0

        prms |> List.iteri (fun i (k, _) -> args.[k] <- baseIdx + i)

        let env =
            {
                Provider = provider
                Ctx = ctx
                Slots = Dictionary<NodeKey, int>()
                ClosureByNode = closureByNode
                CtorHandleByNode = ctorHandleByNode
                Args = args
                SelfKey = ValueNone
                CaptureFields = Dictionary<NodeKey, EntityHandle>()
                Unions = unions
                StaticMethods = staticMethods
            }

        emitExpr env il body
        Cil.emitRet il

    /// Build a closure's `.ctor` body: chain to the `FSharpFunc\`2` base ctor,
    /// then store each capture argument into its field.
    let emitClosureCtor (baseCtor: EntityHandle) (fields: EntityHandle list) (il: Il) : unit =
        Cil.emitLdarg il 0
        Cil.emitCall il baseCtor 1 0 // call instance void base::.ctor()

        fields
        |> List.iteri (fun i field ->
            Cil.emitLdarg il 0
            Cil.emitLdarg il (i + 1)
            Cil.emitStfld il field
        )

        Cil.emitRet il

    /// Build a union case's static factory body: allocate via the union's
    /// parameterless ctor, stamp the discriminant `tag`, store each factory
    /// parameter into its field, and return the object. `fieldHandles` are in
    /// declaration order = the factory's parameter order (static `ldarg.i`).
    let emitUnionFactory
        (unionCtor: EntityHandle)
        (tag: int)
        (tagField: EntityHandle)
        (fieldHandles: EntityHandle list)
        (il: Il)
        : unit =
        Cil.emitNewobj il unionCtor 0
        Cil.emitDup il
        Cil.emitLdcI4 il tag
        Cil.emitStfld il tagField

        fieldHandles
        |> List.iteri (fun i field ->
            Cil.emitDup il
            Cil.emitLdarg il i
            Cil.emitStfld il field
        )

        Cil.emitRet il
