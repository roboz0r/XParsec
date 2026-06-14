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
            | TPatG.NamedSimple(k, _, _) -> acc.Add k
            | TPatG.Wildcard _
            | TPatG.Const _ -> ()
            | TPatG.Tuple(items, _, _) ->
                for sub in items do
                    go sub
            | TPatG.Record(fields, _, _) ->
                for (_, sub) in fields do
                    go sub
            | TPatG.Union(_, fields, _, _) ->
                for sub in fields do
                    go sub
            | TPatG.TypeTestAs(_, inner, _, _) -> go inner

        go p
        List.ofSeq acc

    /// Walk `body`, invoking `onFree key ty` once per `Var` reference not shadowed
    /// by `bound` — the single source of truth for closure free-variable scoping.
    /// `freeVars` and `freeVarKeys` differ only in how they seed `bound` and what
    /// they record; the scoping skeleton (every binder that introduces names —
    /// lambdas, lets, `for`/`match`/`try` arms) lives here so a new binder form is
    /// handled in one place. Mutates `bound` in place across the walk (push on
    /// entering a binder's scope, pop on exit); pass a private set.
    ///
    /// A `let rec f = <lambda>` binds `f` in its own value: the recursive
    /// self-reference resolves to the closure's `this` (`discoverClosures`'
    /// `selfKey`), never a free variable — so the name is scoped across both the
    /// value and the body. Otherwise an enclosing closure gains a phantom capture
    /// and an enclosing module function is wrongly dropped from the
    /// static-method-eligible set.
    let private walkFreeRefs
        (bound: HashSet<NodeKey>)
        (onFree: NodeKey -> FrozenType -> unit)
        (body: Frozen.TExpr)
        : unit =
        let scoped (keys: NodeKey list) (k: unit -> unit) =
            let added = keys |> List.filter bound.Add
            k ()

            for key in added do
                bound.Remove key |> ignore

        let rec go (e: Frozen.TExpr) =
            match e with
            | TExprG.Var(key, ty, _) ->
                if not (bound.Contains key) then
                    onFree key ty
            | TExprG.Lambda(p, b, _, _) -> scoped (patKeys p) (fun () -> go b)
            | TExprG.Let((TPatG.NamedSimple _ as p), (TExprG.Lambda _ as v), b, _, _) ->
                scoped (patKeys p) (fun () -> go v)
                scoped (patKeys p) (fun () -> go b)
            | TExprG.Let(p, v, b, _, _)
            | TExprG.Use(p, v, b, _, _, _) ->
                go v
                scoped (patKeys p) (fun () -> go b)
            | TExprG.ForTo(var, s, e2, b, _, _) ->
                go s
                go e2
                scoped [ var ] (fun () -> go b)
            | TExprG.ForIn(p, src, b, _, _, _) ->
                go src
                scoped (patKeys p) (fun () -> go b)
            | TExprG.Match(sc, arms, _, _) ->
                go sc

                for arm in arms do
                    scoped
                        (patKeys arm.Pat)
                        (fun () ->
                            arm.Guard |> Option.iter go
                            go arm.Body
                        )
            | TExprG.TryWith(b, arms, _, _) ->
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

        walkFreeRefs
            bound
            (fun key ty ->
                if seen.Add key then
                    acc.Add(key, ty)
            )
            body

        List.ofSeq acc

    /// Like `freeVars` but keeps only keys (no types, no static-method exclusion):
    /// the capture test in `collectStaticFns` must *see* every referenced binding.
    let private freeVarKeys (boundKeys: NodeKey seq) (body: Frozen.TExpr) : HashSet<NodeKey> =
        let bound = HashSet<NodeKey>(boundKeys)
        let acc = HashSet<NodeKey>()
        walkFreeRefs bound (fun key _ -> acc.Add key |> ignore) body
        acc

    /// The shared classification shell behind `collectModuleValues` /
    /// `collectGenericModuleValues`: a non-`inline`, non-`Lambda` `let name = value`
    /// on a *named* module holder. `tyOk` selects which type shapes qualify (fully
    /// ground vs. open-but-encodable); `project` builds the caller's row from the
    /// resolved binding key, type, init value, and holder info.
    let private classifyModuleValues
        (moduleMembers: Map<uint64, ModuleMemberInfo>)
        (tyOk: FrozenType -> bool)
        (project: NodeKey -> FrozenType -> Frozen.TExpr -> ModuleMemberInfo -> 'a)
        (decls: Frozen.TDecl list)
        : 'a list =
        decls
        |> List.choose (fun d ->
            match d with
            | TDeclG.Let(TPatG.NamedSimple(k, ty, _), value, isInline, _) when
                not isInline
                && (
                    match value with
                    | TExprG.Lambda _ -> false
                    | _ -> true
                )
                && tyOk ty
                ->
                match Map.tryFind k.Raw moduleMembers with
                | Some info -> Some(project k ty value info)
                | None -> None
            | _ -> None
        )

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
        |> classifyModuleValues
            moduleMembers
            ftIsGround
            (fun k ty value info ->
                {
                    Key = k
                    Name = info.Name
                    Ty = ty
                    Init = value
                    Holder = info.Namespace, info.Holder
                }
            )

    /// True when `t` is free of leaked inference metavars (`FTUnknown`) — the
    /// front end never grounded such a type, so it cannot be encoded into a
    /// signature. A generic module value with an `FTUnknown` keeps its current
    /// (skipped) treatment rather than crashing the encoder, exactly as
    /// `collectModuleValues` already excludes them from the ground-field path.
    let rec private ftNoUnknown (t: FrozenType) : bool =
        match t with
        | FTUnknown _ -> false
        | FTTypar _ -> true
        | FTConst(_, xs)
        | FTRecord(_, xs)
        | FTUnion(_, xs)
        | FTClass(_, xs)
        | FTOr xs -> xs |> EqArray.forall ftNoUnknown
        | FTFun(a, b) -> ftNoUnknown a && ftNoUnknown b
        | FTTuple xs -> xs |> EqArray.forall ftNoUnknown

    /// Classify the *generic* module-level values (`let empty : SetTree<'T> = …`)
    /// — a non-`inline`, non-`Lambda` `let` on a *named* holder whose type carries
    /// an open typar (`FTTypar`, freeze-quantified to the method axis) and no
    /// leaked `FTUnknown`. A non-generic module holder has no type parameter to
    /// type a `SetTree<'T>` *field*, so — like real F#'s representation of a
    /// generic value — each lowers to a **zero-arg generic static method** on its
    /// holder, returning the initialiser; every reference `call`s its `MethodSpec`
    /// (the instantiation recovered from the reference's own type). They are
    /// returned as ordinary `StaticFn`s (0 params) so the layout / registry /
    /// holder-method machinery picks them up uniformly; the only bespoke handling
    /// is the value-position `call` at the reference site (`EmitExpr.buildExpr`).
    /// A *function*-typed generic value (a stored closure) is still deferred.
    let collectGenericModuleValues
        (moduleMembers: Map<uint64, ModuleMemberInfo>)
        (decls: Frozen.TDecl list)
        : StaticFn list =
        // Open (`not ftIsGround`) but encodable (`ftNoUnknown`) and not itself a
        // function type — a function-typed generic value (a stored closure, which
        // a non-lambda `let f : 'T -> 'T = id` can still produce) is still deferred.
        let tyOk ty =
            not (ftIsGround ty)
            && ftNoUnknown ty
            && (
                match ty with
                | FTFun _ -> false
                | _ -> true
            )

        decls
        |> classifyModuleValues
            moduleMembers
            tyOk
            (fun k ty value info ->
                {
                    Key = k
                    Name = info.Name
                    Holder = Some(info.Namespace, info.Holder)
                    Params = []
                    Body = value
                    ResultTy = ty
                }
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
        let candidates = Dictionary<NodeKey, StaticParam list * Frozen.TExpr>()
        let order = ResizeArray<NodeKey>()

        for d in decls do
            match d with
            | TDeclG.Let(TPatG.NamedSimple(k, _, _), value, _, _) ->
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
            | TExprG.Var(k, _, _) when candidates.ContainsKey k -> escapes.Add k |> ignore
            | TExprG.App _ ->
                let head, args = TastWalk.collectSpine [] e

                match head with
                | TExprG.Var(k, _, _) when candidates.ContainsKey k ->
                    if List.length args < arity k then
                        escapes.Add k |> ignore

                    for (a, _, _) in args do
                        walkUses a
                | _ ->
                    walkUses head

                    for (a, _, _) in args do
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
                        // The keys a parameter binds: a simple/unit param binds its
                        // own `Slot`; a tuple param binds each leaf the pattern names
                        // (not the placeholder slot), so the body's references to those
                        // leaves count as bound, not as captures.
                        let paramBound =
                            ps
                            |> List.collect (fun p ->
                                match p.Pat with
                                | Some pat -> patKeys pat
                                | None -> [ p.Slot ]
                            )

                        KeyValuePair(k, freeVarKeys (Seq.append moduleValueKeys paramBound) body)
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
            | FTClass(_, xs)
            | FTOr xs ->
                for x in xs do
                    go x
            // A `Declaring`-axis typar can't occur in a module-level static fn, and
            // an unresolved nominal head (`FTUnknown`) carries no typars. Neither
            // contributes a method-axis index.
            | FTUnknown _
            | FTTypar(TyparAxis.Declaring, _) -> ()

        for p in fn.Params do
            go p.Ty

        go fn.ResultTy
        maxIx + 1

    /// Enumerate every `Lambda` in the lowered tree leaves-first (a closure before
    /// any closure that constructs it), with its capture set; returns a dictionary
    /// mapping each lambda node (by reference) to its `Closure`. `staticFnKeys`'
    /// outer lambdas are *not* closures (only their bodies are walked for inner
    /// closures), since a reference to one is a direct call. A closure walked from
    /// a generic static fn's body inherits that fn's `staticFnTypars` on its
    /// `Closure.Typars`; an inner closure inherits the enclosing closure's set.
    /// A closure-discovery root from a *type member body*: the member's
    /// already-`expandBuiltinOps`-expanded body paired with the
    /// number of typars in scope at its construction site — the declaring type's
    /// typar count (the closure re-projects those onto its own class typars, the
    /// same `Typars` count a static-fn closure inherits). `0` for a monomorphic
    /// type, so its member closures stay the plain monomorphic-closure path.
    type MemberClosureRoot =
        {
            /// The declaring type's typar count — the closure's declaring-typar
            /// offset (its first slots are the class typars).
            DeclaringTypars: int
            /// The member's own method-typar count — these follow the class
            /// typars in the closure's typar list (offset `DeclaringTypars`).
            MethodTypars: int
            Body: Frozen.TExpr
        }

    let discoverClosures
        (staticFnKeys: HashSet<NodeKey>)
        (moduleValueKeys: HashSet<NodeKey>)
        (staticFnTypars: IReadOnlyDictionary<NodeKey, int>)
        (closureReprs: Map<uint64, ClosureRepr>)
        (decls: Frozen.TDecl list)
        (memberRoots: MemberClosureRoot list)
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
        // `currentTypars` is the *total* typar count inherited from the enclosing
        // static method / member (or, for inner closures, the enclosing closure
        // verbatim); `declaringOffset` is how many of those are the enclosing
        // class's typars (the leading slots) — `0` for a static-fn closure.
        let rec go (currentTypars: int) (declaringOffset: int) (selfKey: NodeKey voption) (e: Frozen.TExpr) =
            (match e with
             | TExprG.Let(TPatG.NamedSimple(k, _, _), (TExprG.Lambda _ as v), body, _, _) ->
                 go currentTypars declaringOffset (ValueSome k) v
                 go currentTypars declaringOffset ValueNone body
             | _ -> iterChildren (go currentTypars declaringOffset ValueNone) e) // children (and inner lambdas) first → leaves-first

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

                // RS3 verdict, keyed by the closure's binder (`let f = …`). An
                // anonymous lambda (no `SelfKey`) or a binder the snapshot didn't
                // reach defaults to `Heap` — the only shape emitted today.
                let repr =
                    match selfKey with
                    | ValueSome k ->
                        match Map.tryFind k.Raw closureReprs with
                        | Some r -> r
                        | None -> ClosureRepr.Heap
                    | ValueNone -> ClosureRepr.Heap

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
                        DeclaringTypars = declaringOffset
                        Repr = repr
                    }

                counter <- counter + 1
                lookup.[e] <- c
                order.Add e

            match e with
            | TExprG.Lambda((TPatG.NamedSimple(p, pty, _) as pat), body, lamTy, _) ->
                registerClosure p pty pat body lamTy
            | TExprG.Lambda((TPatG.Const(TConstValue.Unit, pty, _) as pat), body, lamTy, _) ->
                // A `fun () ->` unit binder has no name to reference, but the
                // closure's `Invoke` still allocates `ldarg.1` for the unit
                // value the caller pushes; mint a synthetic placeholder so the
                // `args` map (and `freeVars`'s bound set) still has a key.
                registerClosure (mintUnitParamKey ()) pty pat body lamTy
            | TExprG.Lambda((TPatG.Tuple(_, pty, _) as pat), body, lamTy, _) ->
                // A tuple-param lambda (`fun (a, b) -> …`). The single `ldarg.1`
                // carries the `ValueTuple`n` value; mint a synthetic placeholder
                // for that slot — `buildClosureInvoke` `bindPattern`s the leaf
                // element bindings out of it (Step 5). `pty` is the param's
                // `FTTuple`, which the closure's `Invoke` signature encodes.
                registerClosure (mintTupleParamKey ()) pty pat body lamTy
            | TExprG.Lambda(p, _, _, _) -> failwithf "Emit: closure parameter destructuring is out of scope: %A" p
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
            | TDeclG.Let(TPatG.NamedSimple(k, _, _), value, _, _) when staticFnKeys.Contains k ->
                let _, body = peelLambda value
                go (typarsForStaticFn k) 0 ValueNone body
            | TDeclG.Let(TPatG.NamedSimple(k, _, _), value, _, _) -> go 0 0 (ValueSome k) value
            | TDeclG.Let(_, value, _, _) -> go 0 0 ValueNone value
            | TDeclG.Expression(e, _) -> go 0 0 ValueNone e
            | TDeclG.Type _ -> ()

        // Type member bodies: a lambda inside a member body is a closure too —
        // `buildMember` walks the same expanded body, so the
        // node-identity keys in `lookup` match its `buildExpr`. A member body sees
        // no static-fn `selfKey` (a recursive `let rec` inside it would, but the
        // member itself dispatches as a call, not a captured value). The closure's
        // typar list is the declaring class typars (offset 0) followed by the
        // member's own method typars.
        for root in memberRoots do
            go (root.DeclaringTypars + root.MethodTypars) root.DeclaringTypars ValueNone root.Body

        [ for n in order -> lookup.[n] ], lookup
