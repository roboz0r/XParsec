namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open EmitTypes
open EmitLower

module EmitClosures =
    let private patKeys (p: Frozen.TPat) : NodeKey list =
        let acc = ResizeArray<NodeKey>()

        let rec go p =
            match p with
            | TPatG.NamedSimple(k, _, _) -> acc.Add k
            | TPatG.Wildcard _
            | TPatG.Null _
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
    /// `collectGenericModuleValues`: a non-`inline`, non-`Lambda` `let name = value`.
    /// `tyOk` selects which type shapes qualify (fully ground vs. open-but-encodable);
    /// `project` builds the caller's row from the resolved binding key, type, init
    /// value, and the holder info — `Some` for a value on a *named* module holder,
    /// `None` for a *top-level* (implicit-"Program"-module) value. A caller that only
    /// wants named-holder values returns `None` on the holderless case.
    let private classifyModuleValues
        (moduleMembers: Map<uint64, ModuleMemberInfo>)
        (tyOk: FrozenType -> bool)
        (project: NodeKey -> FrozenType -> Frozen.TExpr -> ModuleMemberInfo option -> 'a option)
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
                project k ty value (Map.tryFind k.Raw moduleMembers)
            | _ -> None
        )

    /// Classify which top-level bindings are **module values**: a non-inline
    /// `let name = <plain value>` (no lambda parameters) on a *named* module
    /// holder, whose type is fully ground — no open typar (a generic value
    /// compiles to a generic method, not a field) and no `FTUnknown` (a leaked
    /// inference metavar the front end never resolved; such a value keeps its
    /// current treatment rather than crashing contract extraction). Each becomes
    /// a `public static` field on its holder, initialised by the holder's
    /// `.cctor`, and every reference is an `ldsfld` — never a `Main` local or a
    /// closure capture. Generic values, function values (lambdas), and anonymous
    /// top-level ("Program") values are out of scope and keep their current
    /// treatment.
    let collectModuleValues
        (moduleMembers: Map<uint64, ModuleMemberInfo>)
        (decls: Frozen.TDecl list)
        : ModuleValue list =
        decls
        |> classifyModuleValues
            moduleMembers
            ftIsGround
            (fun k ty value info ->
                // Only a *named*-holder ground value is a field here; a top-level
                // (holderless) ground value is `collectProgramValues`' job.
                info
                |> Option.map (fun info ->
                    {
                        Key = k
                        Name = info.Name
                        Ty = ty
                        Init = value
                        Holder = info.Namespace, info.Holder
                    }
                )
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

    /// The source name of a *top-level* (holderless) binding for its Program-holder
    /// field/method. A *leading* standalone `ModuleElem.Let` had its name recorded by
    /// `Elaborate` (`TopLevelNames`); a value *after* a top-level statement folds into
    /// the preceding sequential (a nested `let`, not its own module element) and has no
    /// recorded name — synthesise one fsc-style (`value@<offset>`), the field being
    /// assembly-internal and resolved by `NodeKey`, never by name.
    let private topLevelName (topLevelNames: Map<uint64, string>) (k: NodeKey) : string =
        match Map.tryFind k.Raw topLevelNames with
        | Some n -> n
        | None -> sprintf "value@%d" k.Offset

    /// `(ns, name)` of a `TypeKey` — used to match a value's type against the
    /// ref-struct set, keyed on `(ns, name)` because the use-site `FTClass` key and
    /// the decl key can carry different `asm` qualification.
    let typeKeyNsName (k: SymbolKey) : (string * string) option =
        match k with
        | SymbolKey.TypeKey(_, ns, name) -> Some(ns, name)
        | _ -> None

    /// Classify the *generic* module-level values (`let empty : SetTree<'T> = …`)
    /// — a non-`inline`, non-`Lambda` `let` whose type carries an open typar
    /// (`FTTypar`, freeze-quantified to the method axis) and no leaked `FTUnknown`.
    /// A non-generic module holder has no type parameter to type a `SetTree<'T>`
    /// *field*, so — like real F#'s representation of a generic value — each lowers
    /// to a zero-arg generic static method on its holder (a "generic property" on
    /// the module's static class), returning the initialiser; every reference
    /// `call`s its `MethodSpec` (the instantiation recovered from the reference's
    /// own type). They are returned as ordinary `StaticFn`s (0 params) so the
    /// layout / registry / holder-method machinery picks them up uniformly; the only
    /// bespoke handling is the value-position `call` at the reference site
    /// (`EmitExpr.buildExpr`). A function-typed generic value (a stored closure) is
    /// still deferred.
    ///
    /// Both a value on a named holder and a top-level (implicit-"Program"-module)
    /// generic value classify: the latter records no `ModuleMemberInfo`, so it gets
    /// `Holder = None` (the Program holder) and a name from `TopLevelNames`
    /// (synthetic `value@<offset>` for a flattened nested trailing value).
    /// Position-independent — a method is computed on demand. A non-generalisable
    /// generic value never reaches here: the front end's value restriction
    /// (`InferGeneralize.shouldGeneralise`) keeps an expansive parameterless binding
    /// monomorphic (and `Validation.checkValueRestriction` errors a mutable one), so
    /// its type is either ground or an `FTUnknown` the `tyOk` gate rejects.
    let collectGenericModuleValues
        (moduleMembers: Map<uint64, ModuleMemberInfo>)
        (topLevelNames: Map<uint64, string>)
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
                let name, holder =
                    match info with
                    | Some info -> info.Name, Some(info.Namespace, info.Holder)
                    // A top-level generic value: `None` holder ⇒ the Program holder.
                    | None -> topLevelName topLevelNames k, None

                Some
                    {
                        Key = k
                        Name = name
                        Holder = holder
                        Params = []
                        // A generic module VALUE is never applied (it reaches codegen
                        // as a bare `Var`, see `EmitExpr`): no source groups, and it
                        // returns a value (never `void`).
                        Groups = []
                        Body = value
                        ResultTy = ty
                        ReturnsVoid = false
                    }
            )

    /// Classify the *top-level* (implicit-"Program"-module) ground values: a
    /// non-`inline`, non-`Lambda`, non-function `let name = <value>` with no
    /// enclosing named module (it records a `TopLevelNames` entry but no
    /// `ModuleMemberInfo`) whose type is fully ground. Each becomes a `public
    /// static` field on the anonymous "Program" holder; the leading/trailing
    /// `.cctor`-vs-`Main` placement is decided later in `HolderPlan.create`.
    /// Generic top-level values are handled by `collectGenericModuleValues`'
    /// holderless fallback; function-typed values (a stored closure) are deferred,
    /// as for a named holder.
    let collectProgramValues
        (moduleMembers: Map<uint64, ModuleMemberInfo>)
        (programHolder: HolderKey)
        (topLevelNames: Map<uint64, string>)
        // `(ns, name)` of every `[<Struct; IsByRefLike>]` type declared in this
        // assembly. `EmitLower.lower` strips type decls, so the caller computes this
        // from `tast.Decls`.
        (refStructNsNames: HashSet<string * string>)
        (decls: Frozen.TDecl list)
        : ModuleValue list =
        // A `[<Struct; IsByRefLike>]` value cannot be a static field (the CLR confines
        // a byref-like type to the stack) — and never needs to be (a ref struct can't
        // be read from a member / cctor anyway). Such a top-level value stays a `Main`
        // local; a byref (`FTConst("&", _)`) likewise.
        let isFieldEmittable (ty: FrozenType) =
            match ty with
            | FTClass(key, _) ->
                match typeKeyNsName key with
                | Some nsName -> not (refStructNsNames.Contains nsName)
                | None -> true
            | FTConst(n, _) when n = RuntimeNames.byrefName -> false
            | _ -> true

        // Ground (a field, not a generic method), not a stored closure (`FTFun`), and
        // storable as a static field.
        let tyOk ty =
            ftIsGround ty
            && (
                match ty with
                | FTFun _ -> false
                | _ -> true
            )
            && isFieldEmittable ty

        decls
        |> classifyModuleValues
            moduleMembers
            tyOk
            (fun k ty value info ->
                // A named-holder value (`module Foo`) takes the named-holder path;
                // only a top-level (`holder = None`) value — recording no
                // `ModuleMemberInfo` — becomes a Program-holder field here.
                match info with
                | Some _ -> None
                | None ->
                    Some
                        {
                            Key = k
                            Name = topLevelName topLevelNames k
                            Ty = ty
                            Init = value
                            Holder = programHolder
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
                        "Emit: module value '%s' references top-level binding %O, which is neither a module value nor a static method, so its initialiser cannot run in the holder's .cctor"
                        mv.Name
                        free

    /// Eta-expand every NON-saturated reference to a static-method-`eligible` module
    /// function so the function stays a flat static method even though it is *also*
    /// used as a value / under-applied. This is F#'s one model — a module `let f … = …`
    /// ALWAYS compiles to a flat static method; a value-use compiles to a closure that
    /// `call`s it — and the model the JS backend (`curryAdapter`) already mirrors. The
    /// escape is an ADDITIVE bridge: it never removes the flat method.
    ///
    /// The flat method is a function's ABI: a publicly reachable function's `.fsi`
    /// advertises it independently of how the function is used inside its producing
    /// assembly, so a cross-assembly consumer decurries it into a flat member-ref and
    /// `call`s it; the bridge keeps that method present. Each value-use / partial
    /// application becomes a curried closure that `call`s the method — realised by
    /// eta-expanding `f` to its full SOURCE arity at every non-saturated occurrence:
    ///   * a bare value-use  `f`     → `fun a0 … a(n-1) -> f a0 … a(n-1)`
    ///   * an under-application `f x` → `(fun a0 … a(n-1) -> f a0 … a(n-1)) x`
    /// After the rewrite every surviving `Var f` heads a saturated (≥ arity) spine, so
    /// `collectStaticFns` emits `f` as a static method; the synthesised eta-lambdas are
    /// ordinary closures whose body is a saturated direct `call` to it (the "wrapper
    /// that calls it"). Exported and holderless escapers are treated alike — both keep
    /// their flat method, matching F# (which emits the static method for non-exported
    /// module functions too).
    ///
    /// Bridging is gated on the `eligible` set (`staticEligible`), NOT on escape: a
    /// function demoted by the *capture* axis is absent from `eligible`, so its
    /// value-uses are left as ordinary closure-object references — eta-expanding them
    /// would wrap a closure in a closure. The pass is a no-op when no eligible function
    /// has a non-saturated reference (the corpus before any escaper lands).
    let bridgeStaticFnEscapes
        (eligible: HashSet<NodeKey>)
        (fns: CompiledFns.CompiledFn list)
        (decls: Frozen.TDecl list)
        : Frozen.TDecl list =
        // Each eligible function's source arity (its curried group count) — the number
        // of arrows the eta-expansion peels, and the spine length at or above which a
        // reference is a saturated direct `call`. `fns` is the SAME pre-bridge
        // `gather` `staticEligible` ran on, threaded in so the two cannot disagree.
        let arity = Dictionary<NodeKey, int>()

        for f in fns do
            if eligible.Contains f.Key then
                arity.[f.Key] <- List.length f.Groups

        if arity.Count = 0 then
            decls
        else
            // `fun a0 … a(n-1) -> f a0 … a(n-1)`, typed from the reference's own
            // curried type: peel `n` arrows for the param domains + each `App` node's
            // result type. A tuple / unit source group needs no special case — the
            // single fresh param carries the group's (possibly tuple / unit) domain
            // and is passed as one argument, exactly as the saturated-call site
            // (`EmitCall`) re-flattens it from `StaticFn.Groups`.
            let buildEta (fVar: Frozen.TExpr) (n: int) : Frozen.TExpr =
                let tok = TastWalk.exprTok fVar

                // Each peeled `->` as a `(domain, codomain)` pair: the fresh param's
                // type and the intermediate `App` result type.
                let levels = TastLower.peelArrows n (typeOfExpr fVar)

                if List.length levels <> n then
                    failwithf "bridgeStaticFnEscapes: function type has fewer than %d arrows: %A" n (typeOfExpr fVar)

                let keys = levels |> List.map (fun _ -> mintUnitParamKey ())

                let argTriples =
                    List.map2 (fun k (dom, cod) -> TExprG.Var(k, dom, tok), cod, tok) keys levels

                let body = TastWalk.rebuildApp fVar argTriples

                List.foldBack2
                    (fun k (dom, cod) acc -> TExprG.Lambda(TPatG.NamedSimple(k, dom, tok), acc, FTFun(dom, cod), tok))
                    keys
                    levels
                    body

            let rec rw (e: Frozen.TExpr) : Frozen.TExpr =
                match e with
                | TExprG.Var(k, _, _) when arity.ContainsKey k -> buildEta e arity.[k]
                | TExprG.App _ ->
                    let head, args = TastWalk.collectSpine [] e
                    let args = args |> List.map (fun (a, t, tk) -> rw a, t, tk)

                    match head with
                    | TExprG.Var(k, _, _) when arity.ContainsKey k ->
                        if List.length args >= arity.[k] then
                            // Saturated (or over-applied): the head stays a direct
                            // `call`; the residual spine over-applies `f`'s result.
                            TastWalk.rebuildApp head args
                        else
                            // Under-application: partially apply the eta closure.
                            TastWalk.rebuildApp (buildEta head arity.[k]) args
                    | _ -> TastWalk.rebuildApp (rw head) args
                | _ -> TastLower.mapChildren rw e

            decls
            |> List.map (fun d ->
                match d with
                | TDeclG.Let(p, value, isInline, tk) -> TDeclG.Let(p, rw value, isInline, tk)
                | TDeclG.Expression(e, tk) -> TDeclG.Expression(rw e, tk)
                | TDeclG.Type _ -> d
            )

    /// The static-method-eligible top-level functions — the ONE genuinely
    /// CLR-intrinsic demotion axis (capture), computed on its own so the bridge pass
    /// (`bridgeStaticFnEscapes`) can run *before* `collectStaticFns` knows the answer.
    /// A candidate is `let [rec] f p0 … = body` whose value peels to ≥ 1 source group;
    /// it is eligible unless it captures a module-level *local*: its free variables
    /// (minus its parameters, self, and the module-value / static-fn keys, which are
    /// `ldsfld` / direct `call`) must all themselves be eligible. A value-local
    /// reference would need a capture field, which a static method has no `this` to
    /// hold. This is a fixpoint, resolved by removing offenders until stable.
    ///
    /// There is **no escape axis** — an escaping function keeps its flat static method
    /// and the escape becomes a curried bridge (F#/JS model). The set is invariant
    /// under eta-expansion: a candidate's capture set is its OWN body's free vars, and
    /// bridging only rewrites references in *other* bodies, leaving the referent free
    /// through the synthesised lambda (the free-var walk sees through it). So the same
    /// set drives bridging on the un-bridged decls and `collectStaticFns` on the
    /// bridged decls — there is no second derivation that could disagree.
    let staticEligible (moduleValueKeys: HashSet<NodeKey>) (fns: CompiledFns.CompiledFn list) : HashSet<NodeKey> =
        let candidates = Dictionary<NodeKey, CompiledFns.CompiledFn>()
        let order = ResizeArray<NodeKey>()

        for f in fns do
            candidates.[f.Key] <- f
            order.Add f.Key

        // Each candidate's capture set (free vars minus its own params + module values).
        let bodyFree =
            Dictionary<NodeKey, HashSet<NodeKey>>(
                seq {
                    for k in order do
                        let c = candidates.[k]
                        // A reference to a module value is an `ldsfld`, not a captured
                        // module-level local — treat those keys as bound so a function
                        // over them stays static-method eligible. The keys a parameter
                        // binds: a simple/unit param binds its own `Slot`; a tuple
                        // param binds each leaf the pattern names (the flat compiled
                        // params expose each leaf binder, so references to those leaves
                        // count as bound, not as captures).
                        let paramBound =
                            c.Params
                            |> List.collect (fun p ->
                                match p.Pat with
                                | Some pat -> patKeys pat
                                | None -> [ p.Slot ]
                            )

                        KeyValuePair(k, freeVarKeys (Seq.append moduleValueKeys paramBound) c.Body)
                }
            )

        // Escape no longer demotes: every gathered function starts eligible; only the
        // capture fixpoint below removes one (its free vars reach outside the eligible
        // set, own self-reference allowed).
        let eligible = HashSet<NodeKey>(order)
        let mutable changed = true

        while changed do
            changed <- false

            for k in List.ofSeq eligible do
                let free = bodyFree.[k]

                let captures = free |> Seq.exists (fun v -> v <> k && not (eligible.Contains v))

                if captures && eligible.Remove k then
                    changed <- true

        eligible

    /// Build the **static-method** `StaticFn`s from the precomputed `eligible` set
    /// (`staticEligible`): each gathered function whose key is eligible, with its
    /// holder / name resolved from `moduleMembers` (a named-holder source name, or the
    /// anonymous `fn$<offset>` on the "Program" holder). Taking `eligible` as input —
    /// rather than recomputing it — guarantees the set bridging assumed and the set
    /// emitted as static methods are the same. A gathered function NOT in `eligible`
    /// (capture-demoted, or a binding `bridgeStaticFnEscapes` newly turned into a
    /// lambda whose key was never eligible) is left for closure discovery.
    let collectStaticFns
        (moduleMembers: Map<uint64, ModuleMemberInfo>)
        (eligible: HashSet<NodeKey>)
        (fns: CompiledFns.CompiledFn list)
        : StaticFn list =
        [
            for c in fns do
                if eligible.Contains c.Key then
                    // A binding inside a named module emits with its source name on its
                    // holder type; a top-level function keeps the anonymous
                    // `fn$<offset>` name on the "Program" holder (`Holder = None`).
                    let name, holder =
                        match Map.tryFind c.Key.Raw moduleMembers with
                        | Some info -> info.Name, Some(info.Namespace, info.Holder)
                        | None -> sprintf "fn$%d" c.Key.Offset, None

                    yield
                        {
                            Key = c.Key
                            Name = name
                            Holder = holder
                            Params = c.Params
                            Groups = c.Groups
                            Body = c.Body
                            ResultTy = c.ResultTy
                            ReturnsVoid = c.ReturnsVoid
                        }
        ]

    /// A generic static method's type-parameter count: `freeze` quantified the
    /// module-`let`'s free typars to `FTTypar(Method, i)` (params left-to-right,
    /// then return), so the count is `max i + 1` over the method's parameter +
    /// result types — those positions reconstruct the declared type freeze indexed,
    /// so every index `0..n-1` appears. `0` ⇒ a monomorphic method, emitted
    /// unchanged. The backend's `FTTypar(Method, i)` encoder maps these to `!!i`
    /// directly (no ambient window). A closure walked from this fn's body inherits
    /// the count on its `Closure.Typars`.
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

    /// rung-4 Step C (M1): the captureless source-lambda argument nodes that may
    /// lower onto a zero-alloc value-struct. An anonymous lambda passed to a
    /// project-local static fn whose corresponding parameter is a bare method
    /// typar `FTTypar(Method, i)` is — by Step A's `subsumes(TyFun, Fun\`2)` rule —
    /// being threaded through a `'TF :> Fun<_,_>` constrained slot, the only shape
    /// the call site can devirtualise by instantiating `!TF` with the closure's own
    /// struct `TypeDef`. Gated to all-`GSimple` callees (flat == curried, so the
    /// spine-arg index maps one-to-one onto the parameter index) — the narrow
    /// blast radius the design doc §1.2 recommends. Captures (M2) stay heap.
    let private collectStackLambdaArgs
        (staticFnKeys: HashSet<NodeKey>)
        (staticFnParamTys: IReadOnlyDictionary<NodeKey, FrozenType list>)
        (staticFnGroups: IReadOnlyDictionary<NodeKey, Frozen.ArgGroup list>)
        (decls: Frozen.TDecl list)
        (memberRoots: MemberClosureRoot list)
        : HashSet<Frozen.TExpr> =
        let stackNodes = HashSet<Frozen.TExpr>(HashIdentity.Reference)

        let rec walk (e: Frozen.TExpr) =
            (match e with
             | TExprG.App _ ->
                 let head, spineArgs = TastWalk.collectSpine [] e

                 match head with
                 | TExprG.Var(k, _, _) when staticFnKeys.Contains k ->
                     match staticFnParamTys.TryGetValue k, staticFnGroups.TryGetValue k with
                     | (true, paramTys), (true, groups) when TastLower.allSimpleGroups groups ->
                         // All-`GSimple`: one flat param per source group, so spine
                         // arg `i` lands on parameter `i`. A bare-typar parameter
                         // fed a literal `fun … -> …` is a constrained `Fun` slot.
                         spineArgs
                         |> List.iteri (fun i (arg, _, _) ->
                             if i < List.length paramTys then
                                 match paramTys.[i], arg with
                                 | FTTypar(TyparAxis.Method, _), (TExprG.Lambda _ as lam) ->
                                     stackNodes.Add lam |> ignore
                                 | _ -> ()
                         )
                     | _ -> ()
                 | _ -> ()
             | _ -> ())

            iterChildren walk e

        for d in decls do
            match d with
            | TDeclG.Expression(e, _) -> walk e
            | TDeclG.Let(_, v, _, _) -> walk v
            | TDeclG.Type _ -> ()

        for r in memberRoots do
            walk r.Body

        stackNodes

    let discoverClosures
        (staticFnKeys: HashSet<NodeKey>)
        (moduleValueKeys: HashSet<NodeKey>)
        (staticFnTypars: IReadOnlyDictionary<NodeKey, int>)
        (staticFnParamTys: IReadOnlyDictionary<NodeKey, FrozenType list>)
        (staticFnGroups: IReadOnlyDictionary<NodeKey, Frozen.ArgGroup list>)
        (closureReprs: Map<uint64, ClosureRepr>)
        (decls: Frozen.TDecl list)
        (memberRoots: MemberClosureRoot list)
        : Closure list * Dictionary<Frozen.TExpr, Closure> =
        let order = ResizeArray<Frozen.TExpr>()
        let lookup = Dictionary<Frozen.TExpr, Closure>(HashIdentity.Reference)
        let mutable counter = 0

        // rung-4 Step C (M1): captureless source lambdas threaded through a
        // constrained `Fun` slot — eligible for the value-struct closure shape.
        let stackLambdaArgs =
            collectStackLambdaArgs staticFnKeys staticFnParamTys staticFnGroups decls memberRoots

        // A module-level value is a `public static` field (`ldsfld`), so — like a
        // static-method reference — it is resolved without a capture. Fold both
        // into the non-captured set.
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

                // Keyed by the closure's binder (`let f = …`). An anonymous lambda
                // (no `SelfKey`) or a binder the snapshot didn't reach defaults to
                // `Heap` — the only shape the v1 heap path emits.
                //
                // `Repr` is the front-end Regions SNAPSHOT (inert on its own).
                // Keyed by the closure's binder; an anonymous lambda or an
                // unreached binder defaults to `Heap`.
                let repr =
                    match selfKey with
                    | ValueSome k ->
                        match Map.tryFind k.Raw closureReprs with
                        | Some r -> r
                        | None -> ClosureRepr.Heap
                    | ValueNone -> ClosureRepr.Heap

                // Bind every leaf the param pattern introduces (a tuple's element
                // bindings), not the placeholder `ParamKey` — those leaves are
                // parameters, never captures.
                let captures = freeVars nonCaptured (patKeys paramPat) selfKey body

                // rung-4 Step C (M1/M2): the CODEGEN value-struct trigger — the
                // stricter gate (necessary-not-sufficient `Repr` is NOT consulted).
                // An *anonymous* lambda (`ValueNone` selfKey — a `let`-bound closure
                // keeps its heap shape) threaded through a constrained `Fun` slot,
                // monomorphic. M2 dropped M1's `List.isEmpty captures` restriction: a
                // CAPTURING such lambda is now also a value-struct, its captures stored
                // by value into struct fields and constructed via the value-type ctor
                // (NOT `initobj`). A plain value struct copies by value, so passing it
                // by value into the combinator stays escape-free (no `ref struct`); a
                // mutable capture is promoted to a heap ref-cell captured by value, so
                // sharing is preserved. The narrow constrained-slot gate is unchanged —
                // the blast radius does not widen beyond M1. Everything else is heap.
                let isValueStruct =
                    currentTypars = 0 && ValueOption.isNone selfKey && stackLambdaArgs.Contains e

                let c =
                    {
                        Node = e
                        Name = sprintf "<closure>$%d" counter
                        ParamKey = p
                        ParamTy = pty
                        ParamPat = paramPat
                        ResultTy = resultTy
                        Body = body
                        Captures = captures
                        SelfKey = selfKey
                        Typars = currentTypars
                        DeclaringTypars = declaringOffset
                        Repr = repr
                        IsValueStruct = isValueStruct
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
                // element bindings out of it. `pty` is the param's `FTTuple`,
                // which the closure's `Invoke` signature encodes.
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
