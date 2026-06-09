namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open XParsec.FSharp.SemanticAnalysis
open EmitTypes
open EmitLower

module EmitClosures =
    let private patKeys (p: Frozen.TPat) : NodeKey list =
        let acc = ResizeArray<NodeKey>()

        let rec go p =
            match p with
            | TPatG.NamedSimple(k, _) -> acc.Add k
            | TPatG.Wildcard _
            | TPatG.Const _ -> ()
            | TPatG.Tuple(items, _) ->
                for sub in items do
                    go sub
            | TPatG.Record(fields, _) ->
                for (_, sub) in fields do
                    go sub
            | TPatG.Union(_, fields, _) ->
                for sub in fields do
                    go sub

        go p
        List.ofSeq acc

    /// The free variables of a closure body, in first-occurrence order — drives
    /// capture field order. `staticFnKeys` are excluded: a reference to a
    /// static-method function is a direct `call`, not a captured value.
    let private freeVars
        (staticFnKeys: HashSet<NodeKey>)
        (paramKeys: NodeKey list)
        (selfKey: NodeKey voption)
        (body: Frozen.TExpr)
        : (NodeKey * FrozenType) list =
        let bound = HashSet<NodeKey>()
        // Every leaf the parameter pattern binds is in scope — for a tuple param
        // (`fun (a, b) -> …`) that is each element binding, not the placeholder slot.
        for k in paramKeys do
            bound.Add k |> ignore

        bound.UnionWith staticFnKeys // static-method references are calls, not captures

        match selfKey with
        | ValueSome k -> bound.Add k |> ignore // the recursive self isn't captured — it's `this`
        | ValueNone -> ()

        let acc = ResizeArray<NodeKey * FrozenType>()
        let seen = HashSet<NodeKey>()

        let scoped (keys: NodeKey list) (k: unit -> unit) =
            let added = keys |> List.filter bound.Add
            k ()

            for key in added do
                bound.Remove key |> ignore

        let rec go (e: Frozen.TExpr) =
            match e with
            | TExprG.Var(key, ty) ->
                if not (bound.Contains key) && seen.Add key then
                    acc.Add(key, ty)
            | TExprG.Lambda(p, b, _) -> scoped (patKeys p) (fun () -> go b)
            | TExprG.Let(p, v, b, _)
            | TExprG.Use(p, v, b, _, _) ->
                go v
                scoped (patKeys p) (fun () -> go b)
            | TExprG.ForTo(var, s, e2, b, _) ->
                go s
                go e2
                scoped [ var ] (fun () -> go b)
            | TExprG.ForIn(p, src, b, _, _) ->
                go src
                scoped (patKeys p) (fun () -> go b)
            | TExprG.Match(sc, arms, _) ->
                go sc

                for arm in arms do
                    scoped
                        (patKeys arm.Pat)
                        (fun () ->
                            arm.Guard |> Option.iter go
                            go arm.Body
                        )
            | TExprG.TryWith(b, arms, _) ->
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
    let private freeVarKeys (boundKeys: NodeKey seq) (body: Frozen.TExpr) : HashSet<NodeKey> =
        let bound = HashSet<NodeKey>(boundKeys)
        let acc = HashSet<NodeKey>()

        let scoped (keys: NodeKey list) (k: unit -> unit) =
            let added = keys |> List.filter bound.Add
            k ()

            for key in added do
                bound.Remove key |> ignore

        let rec go (e: Frozen.TExpr) =
            match e with
            | TExprG.Var(key, _) ->
                if not (bound.Contains key) then
                    acc.Add key |> ignore
            | TExprG.Lambda(p, b, _) -> scoped (patKeys p) (fun () -> go b)
            | TExprG.Let(p, v, b, _)
            | TExprG.Use(p, v, b, _, _) ->
                go v
                scoped (patKeys p) (fun () -> go b)
            | TExprG.ForTo(var, s, e2, b, _) ->
                go s
                go e2
                scoped [ var ] (fun () -> go b)
            | TExprG.ForIn(p, src, b, _, _) ->
                go src
                scoped (patKeys p) (fun () -> go b)
            | TExprG.Match(sc, arms, _) ->
                go sc

                for arm in arms do
                    scoped
                        (patKeys arm.Pat)
                        (fun () ->
                            arm.Guard |> Option.iter go
                            go arm.Body
                        )
            | TExprG.TryWith(b, arms, _) ->
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

    /// Classify which top-level bindings are **module values**
    /// (module-representation-plan): a non-inline `let name = <plain value>` (no
    /// lambda parameters) on a *named* module holder, whose type is fully ground —
    /// no open typar (a generic value compiles to a generic method, not a field)
    /// and no `FTUnknown` (a leaked inference metavar the front end never
    /// resolved; such a value keeps its current treatment rather than crashing
    /// contract extraction). Each becomes a `public static` field on its holder,
    /// initialised by the holder's `.cctor`, and every reference is an `ldsfld` —
    /// never a `Main` local or a closure capture. Generic values, function
    /// values (lambdas), and anonymous top-level ("Program") values are out of
    /// scope for this slice and keep their current treatment.
    let collectModuleValues
        (moduleMembers: Map<uint64, ModuleMemberInfo>)
        (decls: Frozen.TDecl list)
        : ModuleValue list =
        decls
        |> List.choose (fun d ->
            match d with
            | TDeclG.Let(TPatG.NamedSimple(k, ty), value, isInline, _) when
                not isInline
                && (
                    match value with
                    | TExprG.Lambda _ -> false
                    | _ -> true
                )
                && ftIsGround ty
                ->
                match Map.tryFind k.Raw moduleMembers with
                | Some info ->
                    Some
                        {
                            Key = k
                            Name = info.Name
                            Ty = ty
                            Init = value
                            Holder = info.Namespace, info.Holder
                        }
                | None -> None
            | _ -> None
        )

    /// A module value's initialiser runs in its holder's `.cctor`, where only
    /// other module values (`ldsfld`) and static-method functions (direct `call`)
    /// resolve — any other top-level reference (an anonymous "Program" value, a
    /// function that escaped to a closure) would need a `Main` local no `.cctor`
    /// can see. Fail here, with the offending value and reference named, instead
    /// of deep in `buildVarLoad`'s generic "no binding" crash.
    let validateModuleValueInits
        (moduleValueKeys: HashSet<NodeKey>)
        (staticFnKeys: HashSet<NodeKey>)
        (moduleValues: ModuleValue list)
        : unit =
        for mv in moduleValues do
            for free in freeVarKeys [] mv.Init do
                if not (moduleValueKeys.Contains free || staticFnKeys.Contains free) then
                    failwithf
                        "Emit: module value '%s' references top-level binding %O, which is neither a module value nor a static method, so its initialiser cannot run in the holder's .cctor (module-representation-plan)"
                        mv.Name
                        free

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
        (moduleValueKeys: HashSet<NodeKey>)
        (decls: Frozen.TDecl list)
        : StaticFn list * HashSet<NodeKey> =
        let candidates = Dictionary<NodeKey, (NodeKey * FrozenType) list * Frozen.TExpr>()
        let order = ResizeArray<NodeKey>()

        for d in decls do
            match d with
            | TDeclG.Let(TPatG.NamedSimple(k, _), value, _, _) ->
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

        let rec walkUses (e: Frozen.TExpr) =
            match e with
            | TExprG.Var(k, _) when candidates.ContainsKey k -> escapes.Add k |> ignore
            | TExprG.App _ ->
                let head, args = TastWalk.collectSpine [] e

                match head with
                | TExprG.Var(k, _) when candidates.ContainsKey k ->
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
            | TDeclG.Let(_, value, _, _) -> walkUses value
            | TDeclG.Expression(e, _) -> walkUses e
            | TDeclG.Type _ -> ()

        // Each candidate's capture set (free vars minus its own params), tested by rule 2.
        let bodyFree =
            Dictionary<NodeKey, HashSet<NodeKey>>(
                seq {
                    for k in order do
                        let ps, body = candidates.[k]
                        // A reference to a module value is an `ldsfld`, not a captured
                        // module-level local — treat those keys as bound so a function
                        // over them stays static-method eligible (rule 2,
                        // module-representation-plan §4).
                        KeyValuePair(k, freeVarKeys (Seq.append moduleValueKeys (ps |> List.map fst)) body)
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

    /// A generic static method's type-parameter *count* (R3):
    /// `freeze` quantified the module-`let`'s free typars to `FTTypar(Method, i)`
    /// (Edge A order: params left-to-right, then return), so the count is `max i + 1`
    /// over the method's parameter + result types — those positions reconstruct the
    /// declared type freeze indexed, so every index `0..n-1` appears. `0` ⇒ a
    /// monomorphic method, emitted unchanged. The backend's `FTTypar(Method, i)`
    /// encoder maps these to `!!i` directly (no ambient window). A closure walked
    /// from this fn's body inherits the count on its `Closure.Typars`.
    let staticFnTypars (fn: StaticFn) : int =
        let mutable maxIx = -1

        let rec go (t: FrozenType) =
            match t with
            | FTTypar(TyparAxis.Method, i) ->
                if i > maxIx then
                    maxIx <- i
            | FTFun(a, b) ->
                go a
                go b
            | FTConst(_, xs)
            | FTTuple xs
            | FTRecord(_, xs)
            | FTUnion(_, xs)
            | FTClass(_, xs) ->
                for x in xs do
                    go x
            // A `Declaring`-axis typar can't occur in a module-level static fn, and
            // an unresolved nominal head (`FTUnknown`) carries no typars. Neither
            // contributes a method-axis index.
            | FTUnknown _
            | FTTypar(TyparAxis.Declaring, _) -> ()

        for (_, pty) in fn.Params do
            go pty

        go fn.ResultTy
        maxIx + 1

    /// Enumerate every `Lambda` in the lowered tree leaves-first (a closure before
    /// any closure that constructs it), with its capture set; returns a dictionary
    /// mapping each lambda node (by reference) to its `Closure`. `staticFnKeys`'
    /// outer lambdas are *not* closures (only their bodies are walked for inner
    /// closures), since a reference to one is a direct call. A closure walked from
    /// a generic static fn's body inherits that fn's `staticFnTypars` on its
    /// `Closure.Typars`; an inner closure inherits the enclosing closure's set.
    let discoverClosures
        (staticFnKeys: HashSet<NodeKey>)
        (moduleValueKeys: HashSet<NodeKey>)
        (staticFnTypars: IReadOnlyDictionary<NodeKey, int>)
        (decls: Frozen.TDecl list)
        : Closure list * Dictionary<Frozen.TExpr, Closure> =
        let order = ResizeArray<Frozen.TExpr>()
        let lookup = Dictionary<Frozen.TExpr, Closure>(HashIdentity.Reference)
        let mutable counter = 0

        // A module-level value is a `public static` field (`ldsfld`), so — like a
        // static-method reference — it is resolved without a capture
        // (module-representation-plan §4). Fold both into the non-captured set.
        let nonCaptured = HashSet<NodeKey>(staticFnKeys)
        nonCaptured.UnionWith moduleValueKeys

        // `selfKey` is the binding key when this node is the immediate value of a
        // `let f = …` lambda — a recursive self-reference resolves to `this`.
        // `currentTypars` is the typar *count* inherited from the enclosing static
        // method (or, for inner closures, the enclosing closure verbatim).
        let rec go (currentTypars: int) (selfKey: NodeKey voption) (e: Frozen.TExpr) =
            (match e with
             | TExprG.Let(TPatG.NamedSimple(k, _), (TExprG.Lambda _ as v), body, _) ->
                 go currentTypars (ValueSome k) v
                 go currentTypars ValueNone body
             | _ -> iterChildren (go currentTypars ValueNone) e) // children (and inner lambdas) first → leaves-first

            let registerClosure
                (p: NodeKey)
                (pty: FrozenType)
                (paramPat: Frozen.TPat)
                (body: Frozen.TExpr)
                (lamTy: FrozenType)
                =
                let resultTy =
                    match lamTy with
                    | FTFun(_, r) -> r
                    | _ -> failwithf "Emit: closure type is not a function: %A" lamTy

                let c =
                    {
                        Node = e
                        Name = sprintf "<closure>$%d" counter
                        ParamKey = p
                        ParamTy = pty
                        ParamPat = paramPat
                        ResultTy = resultTy
                        Body = body
                        // Bind every leaf the param pattern introduces (a tuple's
                        // element bindings), not the placeholder `ParamKey` — those
                        // leaves are parameters, never captures.
                        Captures = freeVars nonCaptured (patKeys paramPat) selfKey body
                        SelfKey = selfKey
                        Typars = currentTypars
                    }

                counter <- counter + 1
                lookup.[e] <- c
                order.Add e

            match e with
            | TExprG.Lambda((TPatG.NamedSimple(p, pty) as pat), body, lamTy) -> registerClosure p pty pat body lamTy
            | TExprG.Lambda((TPatG.Const(TConstValue.Unit, pty) as pat), body, lamTy) ->
                // A `fun () ->` unit binder has no name to reference, but the
                // closure's `Invoke` still allocates `ldarg.1` for the unit
                // value the caller pushes; mint a synthetic placeholder so the
                // `args` map (and `freeVars`'s bound set) still has a key.
                registerClosure (mintUnitParamKey ()) pty pat body lamTy
            | TExprG.Lambda((TPatG.Tuple(_, pty) as pat), body, lamTy) ->
                // A tuple-param lambda (`fun (a, b) -> …`). The single `ldarg.1`
                // carries the `ValueTuple`n` value; mint a synthetic placeholder
                // for that slot — `buildClosureInvoke` `bindPattern`s the leaf
                // element bindings out of it (Step 5). `pty` is the param's
                // `FTTuple`, which the closure's `Invoke` signature encodes.
                registerClosure (mintTupleParamKey ()) pty pat body lamTy
            | TExprG.Lambda(p, _, _) -> failwithf "Emit: closure parameter destructuring is out of scope: %A" p
            | _ -> ()

        let typarsForStaticFn (k: NodeKey) : int =
            match staticFnTypars.TryGetValue k with
            | true, n -> n
            | false, _ -> 0

        for d in decls do
            match d with
            // A static-method function's lambda is not a closure, but its body
            // may still construct inner closures — walk only the body. The
            // closures inherit the method's typars.
            | TDeclG.Let(TPatG.NamedSimple(k, _), value, _, _) when staticFnKeys.Contains k ->
                let _, body = peelLambda value
                go (typarsForStaticFn k) ValueNone body
            | TDeclG.Let(TPatG.NamedSimple(k, _), value, _, _) -> go 0 (ValueSome k) value
            | TDeclG.Let(_, value, _, _) -> go 0 ValueNone value
            | TDeclG.Expression(e, _) -> go 0 ValueNone e
            | TDeclG.Type _ -> ()

        [ for n in order -> lookup.[n] ], lookup
