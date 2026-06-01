namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open XParsec.FSharp.SemanticAnalysis
open EmitTypes
open EmitLower

module EmitClosures =
    let private patKeys (p: TPat) : NodeKey list =
        let acc = ResizeArray<NodeKey>()

        let rec go p =
            match p with
            | TPat.NamedSimple(k, _) -> acc.Add k
            | TPat.Wildcard _
            | TPat.Const _ -> ()
            | TPat.Tuple(items, _) ->
                for sub in items do
                    go sub
            | TPat.Record(fields, _) ->
                for (_, sub) in fields do
                    go sub
            | TPat.Union(_, fields, _) ->
                for sub in fields do
                    go sub

        go p
        List.ofSeq acc

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
            | TExpr.Let(p, v, b, _)
            | TExpr.Use(p, v, b, _, _) ->
                go v
                scoped (patKeys p) (fun () -> go b)
            | TExpr.ForTo(var, s, e2, b, _) ->
                go s
                go e2
                scoped [ var ] (fun () -> go b)
            | TExpr.ForIn(p, src, b, _, _) ->
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
            | TExpr.Let(p, v, b, _)
            | TExpr.Use(p, v, b, _, _) ->
                go v
                scoped (patKeys p) (fun () -> go b)
            | TExpr.ForTo(var, s, e2, b, _) ->
                go s
                go e2
                scoped [ var ] (fun () -> go b)
            | TExpr.ForIn(p, src, b, _, _) ->
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

    /// A generic static method's type parameters (R3): the distinct free
    /// `TypeVar` roots of its signature (parameter types, then result type), in
    /// first-appearance order. Empty ⇒ a monomorphic method, emitted unchanged.
    /// These same `TypeVar` objects appear in the method's body, so the
    /// backend's ambient typar set (`ClrProvider.SetMethodTypars`) maps them to
    /// `!!i`. A closure walked from this fn's body inherits this set on its
    /// `Closure.Typars` (function-representation-plan §Generic closures, C1).
    let staticFnTypars (fn: StaticFn) : TypeVar list =
        let seen = HashSet<TypeVar>(HashIdentity.Reference)
        let acc = ResizeArray<TypeVar>()

        let rec go (t: SemType) =
            match zonk t with
            | TyVar tv ->
                let r = UnionFind.find tv

                if seen.Add r then
                    acc.Add r
            | TyFun(a, b) ->
                go a
                go b
            | TyConst(_, xs)
            | TyTuple xs
            | TyRecord(_, xs)
            | TyUnion(_, xs)
            | TyClass(_, xs) ->
                for x in xs do
                    go x
            | TyUnknown _ -> ()

        for (_, pty) in fn.Params do
            go pty

        go fn.ResultTy
        List.ofSeq acc

    /// Enumerate every `Lambda` in the lowered tree leaves-first (a closure before
    /// any closure that constructs it), with its capture set; returns a dictionary
    /// mapping each lambda node (by reference) to its `Closure`. `staticFnKeys`'
    /// outer lambdas are *not* closures (only their bodies are walked for inner
    /// closures), since a reference to one is a direct call. A closure walked from
    /// a generic static fn's body inherits that fn's `staticFnTypars` on its
    /// `Closure.Typars`; an inner closure inherits the enclosing closure's set.
    let discoverClosures
        (staticFnKeys: HashSet<NodeKey>)
        (staticFnTypars: IReadOnlyDictionary<NodeKey, TypeVar list>)
        (decls: TDecl list)
        : Closure list * Dictionary<TExpr, Closure> =
        let order = ResizeArray<TExpr>()
        let lookup = Dictionary<TExpr, Closure>(HashIdentity.Reference)
        let mutable counter = 0

        // `selfKey` is the binding key when this node is the immediate value of a
        // `let f = …` lambda — a recursive self-reference resolves to `this`.
        // `currentTypars` is the ambient typar set inherited from the enclosing
        // static method (or, for inner closures, the enclosing closure verbatim).
        let rec go (currentTypars: TypeVar list) (selfKey: NodeKey voption) (e: TExpr) =
            (match e with
             | TExpr.Let(TPat.NamedSimple(k, _), (TExpr.Lambda _ as v), body, _) ->
                 go currentTypars (ValueSome k) v
                 go currentTypars ValueNone body
             | _ -> iterChildren (go currentTypars ValueNone) e) // children (and inner lambdas) first → leaves-first

            let registerClosure (p: NodeKey) (pty: SemType) (body: TExpr) (lamTy: SemType) =
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
                        Typars = currentTypars
                    }

                counter <- counter + 1
                lookup.[e] <- c
                order.Add e

            match e with
            | TExpr.Lambda(TPat.NamedSimple(p, pty), body, lamTy) -> registerClosure p pty body lamTy
            | TExpr.Lambda(TPat.Const(TConstValue.Unit, pty), body, lamTy) ->
                // A `fun () ->` unit binder has no name to reference, but the
                // closure's `Invoke` still allocates `ldarg.1` for the unit
                // value the caller pushes; mint a synthetic placeholder so the
                // `args` map (and `freeVars`'s bound set) still has a key.
                registerClosure (mintUnitParamKey ()) pty body lamTy
            | TExpr.Lambda(p, _, _) -> failwithf "Emit: closure parameter destructuring is out of scope: %A" p
            | _ -> ()

        let typarsForStaticFn (k: NodeKey) : TypeVar list =
            match staticFnTypars.TryGetValue k with
            | true, tps -> tps
            | false, _ -> []

        for d in decls do
            match d with
            // A static-method function's lambda is not a closure, but its body
            // may still construct inner closures — walk only the body. The
            // closures inherit the method's typars.
            | TDecl.Let(TPat.NamedSimple(k, _), value, _, _) when staticFnKeys.Contains k ->
                let _, body = peelLambda value
                go (typarsForStaticFn k) ValueNone body
            | TDecl.Let(TPat.NamedSimple(k, _), value, _, _) -> go [] (ValueSome k) value
            | TDecl.Let(_, value, _, _) -> go [] ValueNone value
            | TDecl.Expression(e, _) -> go [] ValueNone e
            | TDecl.Type _ -> ()

        [ for n in order -> lookup.[n] ], lookup
