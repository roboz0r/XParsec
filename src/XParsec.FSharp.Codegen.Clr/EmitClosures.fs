namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open EmitTypes
open EmitLower

module EmitClosures =
    let private patKeys (p: TastAccessor.PatId) : BinderId list =
        let acc = ResizeArray<BinderId>()

        let rec go p =
            match TastAccessor.patKind p with
            | PatShape.NamedSimple ->
                match TastAccessor.patBinder p with
                | ValueSome k -> acc.Add k
                | ValueNone -> ()
            // An or-pattern binds nothing (name resolution drops its binders), so it is
            // NOT walked for the binders in its alternatives; every other composite is.
            // Leaves (`Wildcard`/`Null`/`EnumCase`/`Const`) have no `patChildren`, so the
            // recursion bottoms out there without an arm of their own.
            | PatShape.Or -> ()
            | _ ->
                for sub in TastAccessor.patChildren p do
                    go sub

        go p
        List.ofSeq acc

    /// A `let name = <lambda> in body` — a simple-named binder whose value is a lambda.
    /// This is the shape that anchors an inner closure to its binder name (the binder is
    /// scoped across its own value, not only the body). → the binder key, the lambda
    /// value, and the body. Declines any other `Let` (a tuple/record binder, or a
    /// non-lambda value) and any non-`Let`. Single-sources the binder-vs-value shape test
    /// the two closure walks below share (was spelled once via `patKind`, once via
    /// `patBinder`).
    [<return: Struct>]
    let private (|LetBoundLambda|_|)
        (e: TastAccessor.ExprId)
        : struct (BinderId * TastAccessor.ExprId * TastAccessor.ExprId) voption =
        match e with
        | TastAccessor.ELet letv ->
            match TastAccessor.patBinder letv.Binding, TastAccessor.exprKind letv.Value with
            | ValueSome k, ExprShape.Lambda -> ValueSome(struct (k, letv.Value, letv.Body))
            | _ -> ValueNone
        | _ -> ValueNone

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
        (bound: HashSet<BinderId>)
        (onFree: BinderId -> FrozenType -> unit)
        (body: TastAccessor.ExprId)
        : unit =
        let scoped (keys: BinderId list) (k: unit -> unit) =
            let added = keys |> List.filter bound.Add
            k ()

            for key in added do
                bound.Remove key |> ignore

        let rec go (e: TastAccessor.ExprId) =
            match e with
            | TastAccessor.EVar key ->
                if not (bound.Contains key) then
                    onFree key (TastAccessor.exprTy e)
            | TastAccessor.ELambda lam -> scoped (patKeys lam.Param) (fun () -> go lam.Body)
            | LetBoundLambda(k, value, body) ->
                scoped [ k ] (fun () -> go value)
                scoped [ k ] (fun () -> go body)
            | TastAccessor.ELet letv ->
                go letv.Value
                scoped (patKeys letv.Binding) (fun () -> go letv.Body)
            | TastAccessor.EUse usev ->
                go usev.Value
                scoped (patKeys usev.Binding) (fun () -> go usev.Body)
            | TastAccessor.EForTo ft ->
                go ft.StartExpr
                go ft.EndExpr
                scoped [ ft.Var ] (fun () -> go ft.Body)
            | TastAccessor.EForIn fi ->
                go fi.Source
                scoped (patKeys fi.Pat) (fun () -> go fi.Body)
            | TastAccessor.EMatch m ->
                go m.Scrutinee

                for arm in m.Arms do
                    scoped
                        (patKeys arm.Pat)
                        (fun () ->
                            arm.Guard |> ValueOption.iter go
                            go arm.Body
                        )
            | TastAccessor.ETryWith tw ->
                go tw.Body

                for arm in tw.Arms do
                    scoped
                        (patKeys arm.Pat)
                        (fun () ->
                            arm.Guard |> ValueOption.iter go
                            go arm.Body
                        )
            | _ -> iterChildren go e

        go body

    /// The free variables of a closure body, in first-occurrence order — drives
    /// capture field order. `staticFnKeys` are excluded: a reference to a
    /// static-method function is a direct `call`, not a captured value.
    let private freeVars
        (staticFnKeys: HashSet<BinderId>)
        (paramKeys: BinderId list)
        (selfKey: BinderId voption)
        (body: TastAccessor.ExprId)
        : (BinderId * FrozenType) list =
        let bound = HashSet<BinderId>()
        // Every leaf the parameter pattern binds is in scope — for a tuple param
        // (`fun (a, b) -> …`) that is each element binding, not the placeholder slot.
        for k in paramKeys do
            bound.Add k |> ignore

        bound.UnionWith staticFnKeys // static-method references are calls, not captures

        match selfKey with
        | ValueSome k -> bound.Add k |> ignore // the recursive self isn't captured — it's `this`
        | ValueNone -> ()

        let acc = ResizeArray<BinderId * FrozenType>()
        let seen = HashSet<BinderId>()

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
    let private freeVarKeys (boundKeys: BinderId seq) (body: TastAccessor.ExprId) : HashSet<BinderId> =
        let bound = HashSet<BinderId>(boundKeys)
        let acc = HashSet<BinderId>()
        walkFreeRefs bound (fun key _ -> acc.Add key |> ignore) body
        acc

    /// A top-level binding's stable handle key: its `SymbolKey`, from the declaring
    /// holder (a named module, or the Program holder for a holderless binding) and its
    /// emitted `name`. Minted from the same (holder, name) the metadata row uses, so the
    /// combined `MethodKey.StaticFn` / `FieldKey.ModuleValue` handle map keys on an
    /// identity that is unique across compilation units (distinct qualified names) and,
    /// with the offset the shadowable names carry, across shadowed entry-file rows.
    let private bindingSymbolKey (holder: HolderKey) (name: string) : SymbolKey =
        SymbolKeyOps.valueKey (ModuleHolder.InModule holder) name

    /// The shared classification shell behind `collectModuleValues` /
    /// `collectGenericModuleValues`: a non-`inline`, non-`Lambda` `let name = value`.
    /// `tyOk` selects which type shapes qualify (fully ground vs. open-but-encodable);
    /// `project` builds the caller's row from the resolved binding key, type, init
    /// value, and the holder info — `Some` for a value on a *named* module holder,
    /// `None` for a *top-level* (implicit-"Program"-module) value. A caller that only
    /// wants named-holder values returns `None` on the holderless case.
    let private classifyModuleValues
        (moduleMembers: Map<BinderId, ModuleBindingInfo>)
        (tyOk: FrozenType -> bool)
        (project: BinderId -> FrozenType -> TastAccessor.ExprId -> ModuleBindingInfo option -> 'a option)
        (decls: TastAccessor.DeclId list)
        : 'a list =
        decls
        |> List.choose (fun d ->
            match d with
            | TastAccessor.DLet letd ->
                match letd.Binding with
                | TastAccessor.PNamed k when
                    not letd.IsInline
                    && TastAccessor.exprKind letd.Value <> ExprShape.Lambda
                    && tyOk (TastAccessor.patTy letd.Binding)
                    ->
                    project k (TastAccessor.patTy letd.Binding) letd.Value (Map.tryFind k moduleMembers)
                | _ -> None
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
        (moduleMembers: Map<BinderId, ModuleBindingInfo>)
        (decls: TastAccessor.DeclId list)
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
                        SymbolKey = bindingSymbolKey info.Holder info.Name
                        Name = info.Name
                        Ty = ty
                        Init = value
                        Holder = info.Holder
                    }
                )
            )

    /// True when `t` is free of leaked inference metavars (`FTUnknown`) and of
    /// body-local typars (`FTLocalTypar` — a typar bound by a local `let`'s own
    /// scheme, which this backend has no axis for until generic closures exist).
    /// Neither is a type the front end grounded, so neither can be encoded into a
    /// signature. A generic module value carrying one keeps its current (skipped)
    /// treatment rather than crashing the encoder, exactly as `collectModuleValues`
    /// already excludes them from the ground-field path. `FTLocalTypar` rides here
    /// rather than at a hard error because it is REACHABLE on a legal program
    /// (`let f () = let g = fun x -> x in (g, g)` compiles today, boxing the phantom
    /// typar) — the gate is "this site needs a representation", not "this leaf
    /// reached the backend".
    let rec private ftNoUnknown (t: FrozenType) : bool =
        match t with
        | FTUnknown _
        | FTLocalTypar _ -> false
        | t -> FrozenType.forallChildren ftNoUnknown t

    /// The emitted metadata name of a *top-level* (holderless) binding on the
    /// Program holder: the source name the frozen binder column spells it with (a binder
    /// no source names — one the freeze minted — takes `value`), suffixed with the
    /// binder's own SLOT (`x` → `x$<slot>`). A top-level binding is SHADOWABLE
    /// (`let x = 1 … let x = 2` is two Program-holder rows sharing the source name), so
    /// the slot makes each row's field/method name unique — and, since its `SymbolKey`
    /// handle key is minted from this same name, makes that key injective too.
    let private topLevelName (pool: PoolBuilder) (k: BinderId) : string =
        let (BinderId slot) = k

        let name =
            match TastPoolBuilder.binderNaming pool k with
            | BinderNaming.Source n -> n
            | BinderNaming.Minted _ -> "value"

        sprintf "%s$%d" name slot

    /// `(ns, name)` of a `TypeSlotKey` — used to match a value's type against the
    /// ref-struct set, keyed on `(ns, name)` because the use-site `FTClass` key and
    /// the decl key can carry different `asm` qualification.
    let typeKeyNsName (t: TypeKey) : string * string =
        t.Namespace.Dotted, SymbolKeyOps.typeNestedName t

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
    /// generic value classify: the latter records no `ModuleBindingInfo`, so it gets
    /// `Holder = None` (the Program holder) and its slot-suffixed source name
    /// (`topLevelName`).
    /// Position-independent — a method is computed on demand. A non-generalisable
    /// generic value never reaches here: the front end's value restriction
    /// (`InferGeneralize.shouldGeneralise`) keeps an expansive parameterless binding
    /// monomorphic (and `Validation.checkValueRestriction` errors a mutable one), so
    /// its type is either ground or an `FTUnknown` the `tyOk` gate rejects.
    let collectGenericModuleValues
        (moduleMembers: Map<BinderId, ModuleBindingInfo>)
        (programHolder: HolderKey)

        (decls: TastAccessor.DeclId list)
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
                    | Some info -> info.Name, Some info.Holder
                    // A top-level generic value: `None` holder ⇒ the Program holder.
                    | None -> topLevelName value.Pool k, None

                Some
                    {
                        Key = k
                        // Holderless ⇒ the Program holder is this key's declaring module.
                        SymbolKey = bindingSymbolKey (Option.defaultValue programHolder holder) name
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
                        // A generic module VALUE carries no front-end
                        // function scheme bounds; `staticFnTypars` still derives its
                        // emitted typar count.
                        Constraints = []
                    }
            )

    /// Classify the *top-level* (implicit-"Program"-module) ground values: a
    /// non-`inline`, non-`Lambda`, non-function `let name = <value>` with no
    /// enclosing named module (it records no
    /// `ModuleBindingInfo`) whose type is fully ground. Each becomes a `public
    /// static` field on the anonymous "Program" holder; the leading/trailing
    /// `.cctor`-vs-`Main` placement is decided later in `HolderPlan.create`.
    /// Generic top-level values are handled by `collectGenericModuleValues`'
    /// holderless fallback; function-typed values (a stored closure) are deferred,
    /// as for a named holder.
    let collectProgramValues
        (moduleMembers: Map<BinderId, ModuleBindingInfo>)
        (programHolder: HolderKey)

        // `(ns, name)` of every `[<Struct; IsByRefLike>]` type declared in this
        // assembly. `EmitLower.lower` strips type decls, so the caller computes this
        // from `tast.Decls`.
        (refStructNsNames: HashSet<string * string>)
        (decls: TastAccessor.DeclId list)
        : ModuleValue list =
        // A `[<Struct; IsByRefLike>]` value cannot be a static field (the CLR confines
        // a byref-like type to the stack) — and never needs to be (a ref struct can't
        // be read from a member / cctor anyway). Such a top-level value stays a `Main`
        // local; a byref (`FTConst("byref", _)`) likewise.
        let isFieldEmittable (ty: FrozenType) =
            match ty with
            | FTClass(key, _) -> not (refStructNsNames.Contains(typeKeyNsName key))
            | FTByref _ -> false
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
                // `ModuleBindingInfo` — becomes a Program-holder field here.
                match info with
                | Some _ -> None
                | None ->
                    let name = topLevelName value.Pool k

                    Some
                        {
                            Key = k
                            SymbolKey = bindingSymbolKey programHolder name
                            Name = name
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
        (moduleValueKeys: HashSet<BinderId>)
        (staticFnKeys: HashSet<BinderId>)
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
        (eligible: HashSet<BinderId>)
        (fns: CompiledFns.CompiledFn list)
        (decls: TastAccessor.DeclId list)
        : TastAccessor.DeclId list =
        // Each eligible function's source arity (its curried group count) — the number
        // of arrows the eta-expansion peels, and the spine length at or above which a
        // reference is a saturated direct `call`. `fns` is the SAME pre-bridge
        // `gather` `staticEligible` ran on, threaded in so the two cannot disagree.
        let arity = Dictionary<BinderId, int>()

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
            let buildEta (fVar: TastAccessor.ExprId) (n: int) : TastAccessor.ExprId =
                let tok = TastAccessor.exprTok fVar

                // Each peeled `->` as a `(domain, codomain)` pair: the fresh param's
                // type and the intermediate `App` result type.
                let levels = TastLower.peelArrows n (typeOfExpr fVar)

                if List.length levels <> n then
                    failwithf "bridgeStaticFnEscapes: function type has fewer than %d arrows: %A" n (typeOfExpr fVar)

                let keys = levels |> List.map (fun _ -> TastPoolBuilder.mintBinder fVar.Pool)

                // The bridge's nodes are DERIVED — they exist in no frozen tree — so they
                // are appended to the same pool `fVar` lives in, which is what lets the
                // spliced `fVar` ride into them by the id it already had.
                let pool = fVar.Pool

                let argTriples =
                    List.map2 (fun k (dom, cod) -> TastAccessor.mintVar pool k dom tok, cod, tok) keys levels

                let body = TastAccessor.mintAppSpine fVar argTriples

                List.foldBack2
                    (fun k (dom, cod) acc ->
                        TastAccessor.mintLambda (TastAccessor.mintNamedPat pool k dom tok) acc (FTFun(dom, cod)) tok
                    )
                    keys
                    levels
                    body

            let rec rw (e: TastAccessor.ExprId) : TastAccessor.ExprId =
                match e with
                | TastAccessor.EVar k when arity.ContainsKey k -> buildEta e arity.[k]
                | TastAccessor.EApp _ ->
                    let head, args = TastAccessor.collectSpine [] e

                    match head with
                    | TastAccessor.EVar k when arity.ContainsKey k && List.length args < arity.[k] ->
                        // Under-application: partially apply the eta closure. The head is
                        // a freshly minted closure, so this spine is genuinely new.
                        TastAccessor.mintAppSpine
                            (buildEta head arity.[k])
                            (args |> List.map (fun (a, t, tk) -> rw a, t, tk))
                    | _ ->
                        // The spine STANDS: a saturated (or over-applied) eligible head
                        // stays a direct `call` and everything else recurses, so only the
                        // arguments and a non-eligible head can move. Rewriting in place
                        // keeps each `App`'s own id — the rows that did not change are not
                        // re-appended.
                        TastAccessor.mapSpine
                            (fun h ->
                                match h with
                                | TastAccessor.EVar k when arity.ContainsKey k -> h
                                | _ -> rw h
                            )
                            rw
                            e
                | _ -> TastAccessor.mapChildren rw e

            // A `type` decl surfaces no `DeclExprChildren`, so the mapping is already the
            // identity on one — no arm needed to spare it.
            decls |> List.map (TastAccessor.mapDeclExpr rw)

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
    let staticEligible (moduleValueKeys: HashSet<BinderId>) (fns: CompiledFns.CompiledFn list) : HashSet<BinderId> =
        let candidates = Dictionary<BinderId, CompiledFns.CompiledFn>()
        let order = ResizeArray<BinderId>()

        for f in fns do
            candidates.[f.Key] <- f
            order.Add f.Key

        // Each candidate's capture set (free vars minus its own params + module values).
        let bodyFree =
            Dictionary<BinderId, HashSet<BinderId>>(
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
        let eligible = HashSet<BinderId>(order)
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
        (moduleMembers: Map<BinderId, ModuleBindingInfo>)
        (programHolder: HolderKey)
        // Per-binding frozen typar bounds from the front-end
        // scheme. Looked up by `c.Key`; absent ⇒ no bounds. Carried onto
        // `StaticFn.Constraints` and read by the call-site phantom-typar solve.
        (genericFnSchemes: Map<BinderId, FrozenConstraint list>)
        (eligible: HashSet<BinderId>)
        (fns: CompiledFns.CompiledFn list)
        : StaticFn list =
        [
            for c in fns do
                if eligible.Contains c.Key then
                    // A binding inside a named module emits with its source name on its
                    // holder type; a top-level function keeps the anonymous
                    // `fn$<slot>` name on the "Program" holder (`Holder = None`).
                    let name, holder =
                        match Map.tryFind c.Key moduleMembers with
                        | Some info -> info.Name, Some info.Holder
                        | None ->
                            let (BinderId slot) = c.Key
                            sprintf "fn$%d" slot, None

                    let constraints =
                        match Map.tryFind c.Key genericFnSchemes with
                        | Some cs -> cs
                        | None -> []

                    yield
                        {
                            Key = c.Key
                            // Holderless ⇒ the Program holder is this key's declaring
                            // module; its `fn$<offset>` name is already offset-unique.
                            SymbolKey = bindingSymbolKey (Option.defaultValue programHolder holder) name
                            Name = name
                            Holder = holder
                            Params = c.Params
                            Groups = c.Groups
                            Body = c.Body
                            ResultTy = c.ResultTy
                            ReturnsVoid = c.ReturnsVoid
                            Constraints = constraints
                        }
        ]

    /// A generic static method's type-parameter count: `freeze` quantified the
    /// module-`let`'s free typars to `FTTypar(Method, i)` (params left-to-right,
    /// then return, then any body-only index), so the count is `max i + 1` over the
    /// method's parameter + result types AND the body. Parameter/result positions
    /// reconstruct the declared signature; the body sweep additionally catches a
    /// PHANTOM constraint typar (`fold`'s enumerator `'E`) that appears in
    /// NO param/result but survives un-grounded as a real `FTTypar(Method, idx_E)`
    /// leaf in the `for-in` enumerator descriptor — so `fold` emits at its true
    /// arity (e.g. 5) and the call site solves `'E` from its bound. Using the body
    /// (rather than the front-end `scheme.Quantified.Length`) keeps a quantified-but-
    /// erased typar — one `instantiate`/the body grounded to a concrete type, so it
    /// occurs at no frozen index — OUT of the count: emitting a slot for it would
    /// leave an unrecoverable `MethodSpec` arg (the `SetTree.compare` over-count
    /// regression). `0` ⇒ a monomorphic method, emitted unchanged. The backend's
    /// `FTTypar(Method, i)` encoder maps these to `!!i` directly (no ambient window).
    /// A closure walked from this fn's body inherits the count on its `Closure.Typars`.
    ///
    /// This is the PRODUCER arity; it MUST STAY IN LOCKSTEP with the CONSUMER's
    /// (`Inline.openMethodSignature`' dependent-typar fixpoint) — the same method,
    /// built here then consumed across a package boundary, has to agree on its arity or
    /// the consumer's `MethodSpec` arg count mismatches this emitted IL. Nothing
    /// structural ties the two (this sweeps the frozen `TExpr` body; the consumer folds
    /// `SemType` bounds), so the graduation test guards divergence end to end.
    let staticFnTypars (fn: StaticFn) : int =
        let mutable maxIx = -1

        // The method-axis index sweep descends into EVERY child (carried type-level
        // computations included — producer/consumer arity must stay in lockstep); a
        // `Declaring`-axis typar can't occur in a module-level static fn and falls
        // through the child walk as a leaf.
        let rec go (t: FrozenType) =
            match t with
            | FTTypar(TyparAxis.Method, i) ->
                if i > maxIx then
                    maxIx <- i
            | t -> FrozenType.iterChildren go t

        for p in fn.Params do
            go p.Ty

        go fn.ResultTy

        // Sweep the body for the only method indices that param/result cannot see: a
        // phantom constraint typar lives in a `for-in` enumerator descriptor's
        // `ConstrainedInterface` ifaceArgs (and the enumerator type itself). Walk
        // every subexpression's own type plus those descriptor types.
        let goEnum (en: Frozen.ForInEnumerator) =
            match en with
            | ForInEnumeratorG.Interface -> ()
            | ForInEnumeratorG.Pattern(enumeratorTy, getEnum, members, _, _) ->
                go enumeratorTy

                match getEnum with
                | ForInGetEnumG.ConstrainedInterface(_, args) -> EqArray.iter go args
                | _ -> ()

                match members with
                | ForInEnumMembersG.ConstrainedInterface(_, args) -> EqArray.iter go args
                | _ -> ()

        let rec goExpr (e: TastAccessor.ExprId) =
            go (TastAccessor.exprTy e)

            match e with
            | TastAccessor.EForIn fi -> goEnum fi.Enumerator
            | _ -> ()

            TastAccessor.iterChildren goExpr e

        goExpr fn.Body
        maxIx + 1

    /// Enumerate every `Lambda` in the lowered tree leaves-first (a closure before
    /// any closure that constructs it), with its capture set; returns a dictionary
    /// mapping each lambda NODE to its `Closure`. `staticFnKeys`'
    /// outer lambdas are *not* closures (only their bodies are walked for inner
    /// closures), since a reference to one is a direct call. A closure walked from
    /// a generic static fn's body inherits that fn's `staticFnTypars` on its
    /// `Closure.Typars`; an inner closure inherits the enclosing closure's set.
    /// A closure-discovery root from a *type member body*: the member's body paired with
    /// the number of typars in scope at its construction site — the declaring type's
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
            Body: TastAccessor.ExprId
        }

    /// The source-lambda argument nodes that lower onto a zero-alloc
    /// value-struct, each mapped to the FLAT `FunN` arity its constrained slot
    /// demands (`1` for `Fun<_,_>`, `2` for `Fun<_,_,_>`). The decision is the
    /// node-keyed verdict `inferApp` recorded when the
    /// `subsumes(TyFun, Fun`2`/`Fun`3`)` arm fired — codegen no longer
    /// re-derives it structurally (the prior all-`GSimple` + bare-method-typar walk
    /// was a fragile reconstruction of what `subsumes` already knew, and could not
    /// see an external combinator head). A lambda's frozen node carries the same
    /// `fun`-keyword token its CST node keyed off, so its `NodeKey` (recomputed
    /// `ofToken … ExprLambda`) indexes the verdict map. Walking every lambda and
    /// testing membership covers project-local and external heads in one path.
    let private collectStackLambdaArgs
        (funVerdicts: IReadOnlyDictionary<TastAccessor.ExprId, FunVerdict>)
        (decls: TastAccessor.DeclId list)
        (memberRoots: MemberClosureRoot list)
        : Dictionary<TastAccessor.ExprId, int> =
        let stackNodes = Dictionary<TastAccessor.ExprId, int>()

        let rec walk (e: TastAccessor.ExprId) =
            (match TastAccessor.exprKind e with
             | ExprShape.Lambda ->
                 match funVerdicts.TryGetValue e with
                 | true, v -> stackNodes.[e] <- v.Arity
                 | false, _ -> ()
             | _ -> ())

            iterChildren walk e

        for d in decls do
            match d with
            | TastAccessor.DExpression(e, _) -> walk e
            | TastAccessor.DLet letd -> walk letd.Value
            | _ -> ()

        for r in memberRoots do
            walk r.Body

        stackNodes

    /// Mints the `TypeDef` name for each discovered closure. The name IS the
    /// closure's slot key (`TypeSlotKey.Closure name` / `closureByName`),
    /// resolved assembly-wide by name — so one `ClosureNamer` threaded across
    /// every `discoverClosures` call is what keeps those keys unique across the
    /// whole assembly.
    ///
    /// The default policy names closures `<closure>$0`, `<closure>$1`, … in
    /// discovery order and IGNORES the source context it is handed. F#'s own
    /// `<bound-name>@<line>` scheme (debuggable) is deliberately NOT the default:
    /// because the name is the global TypeDef key it must be (1) UNIQUE across
    /// files — the same `let f = fun…` source line recurs in every compilation
    /// unit — and (2) TOTAL — an anonymous lambda has no bound name at all.
    /// `<bound-name>@<line>` satisfies neither without an added disambiguator and
    /// an anonymous-lambda fallback, so the monotonic counter is the
    /// correct-by-construction default. `NextName` still receives the closure
    /// node and its enclosing binder so a richer, debuggable policy can later be
    /// slotted in here alone.
    type ClosureNamer() =
        let mutable counter = 0

        /// Return the next closure name, then advance (return-current-then-
        /// increment — the exact timing the inline counter had). `node` is the
        /// closure's source expression (its `NodeKey`/offset recoverable via
        /// `NodeKey.ofToken (TastAccessor.exprTok node) …`); `selfKey` is the
        /// enclosing `let f = fun…` binder (`ValueNone` for an anonymous lambda).
        /// Both are the context a `<bound-name>@<line>` policy would need; the
        /// counter policy ignores them.
        member _.NextName(_node: TastAccessor.ExprId, _selfKey: BinderId voption) : string =
            let name = sprintf "<closure>$%d" counter
            counter <- counter + 1
            name

    let discoverClosures
        (namer: ClosureNamer)
        (staticFnKeys: HashSet<BinderId>)
        (moduleValueKeys: HashSet<BinderId>)
        (staticFnTypars: IReadOnlyDictionary<BinderId, int>)
        (funVerdicts: IReadOnlyDictionary<TastAccessor.ExprId, FunVerdict>)
        (closureReprs: Map<BinderId, ClosureRepr>)
        (decls: TastAccessor.DeclId list)
        (memberRoots: MemberClosureRoot list)
        : Closure list * Dictionary<TastAccessor.ExprId, Closure> =
        let order = ResizeArray<TastAccessor.ExprId>()
        let lookup = Dictionary<TastAccessor.ExprId, Closure>()

        // Source lambdas threaded through a constrained `Fun`2`/`Fun`3`
        // slot — eligible for the value-struct closure shape, mapped to their flat
        // arity (1 or 2). The node-keyed verdict (`TastFile.FunVerdicts`).
        let stackLambdaArgs = collectStackLambdaArgs funVerdicts decls memberRoots

        // A module-level value is a `public static` field (`ldsfld`), so — like a
        // static-method reference — it is resolved without a capture. Fold both
        // into the non-captured set.
        let nonCaptured = HashSet<BinderId>(staticFnKeys)
        nonCaptured.UnionWith moduleValueKeys

        // `selfKey` is the binding key when this node is the immediate value of a
        // `let f = …` lambda — a recursive self-reference resolves to `this`.
        // `currentTypars` is the *total* typar count inherited from the enclosing
        // static method / member (or, for inner closures, the enclosing closure
        // verbatim); `declaringOffset` is how many of those are the enclosing
        // class's typars (the leading slots) — `0` for a static-fn closure.
        // The arity of a value-struct lambda node (1 by default; 2 for a
        // flat `Fun`3` slot). Only an anonymous monomorphic lambda the verdict reached.
        let valueStructArity (currentTypars: int) (selfKey: BinderId voption) (e: TastAccessor.ExprId) : int =
            if currentTypars = 0 && ValueOption.isNone selfKey then
                match stackLambdaArgs.TryGetValue e with
                | true, arity -> arity
                | false, _ -> 1
            else
                1

        let rec go (currentTypars: int) (declaringOffset: int) (selfKey: BinderId voption) (e: TastAccessor.ExprId) =
            // A FLAT (`Fun`(arity+1)`) value-struct lambda of arity `2..4` peels its
            // `arity - 1` inner `Lambda` levels into the SAME closure's extra
            // parameters (one flat `Invoke(a,b,…)`), so those inner lambdas are NOT
            // walked as independent closures — recurse into the DEEPEST body instead.
            // Every other node walks children first (leaves-first).
            let flatInner =
                let arity = valueStructArity currentTypars selfKey e

                if arity >= 2 then
                    // Unwrap all `arity` nested `Lambda` levels down to the DEEPEST
                    // body; if the shape isn't that saturated nesting, fall through
                    // (`ValueNone`) and this node walks its children normally.
                    let rec peel n (cur: TastAccessor.ExprId) =
                        if n = 0 then
                            ValueSome cur
                        else
                            match cur with
                            | TastAccessor.ELambda lam -> peel (n - 1) lam.Body
                            | _ -> ValueNone

                    peel arity e
                else
                    ValueNone

            (match flatInner with
             | ValueSome inner -> go currentTypars declaringOffset ValueNone inner
             | ValueNone ->
                 match e with
                 // A `let x = <lambda>` binder anchors an inner closure to its name
                 // (`selfKey`), scoped across the lambda value.
                 | LetBoundLambda(k, value, body) ->
                     go currentTypars declaringOffset (ValueSome k) value
                     go currentTypars declaringOffset ValueNone body
                 | _ -> iterChildren (go currentTypars declaringOffset ValueNone) e) // children (and inner lambdas) first → leaves-first

            let registerClosure
                (p: BinderId)
                (pty: FrozenType)
                (paramPat: TastAccessor.PatId)
                (body: TastAccessor.ExprId)
                (lamTy: FrozenType)
                =
                // A flat (`Fun`(arity+1)`) value-struct closure of arity `2..4` peels
                // its `arity - 1` inner `NamedSimple` lambdas — each contributes one
                // extra flat param, walking `body`/`resultTy` down to the innermost body
                // and its codomain (one `FTFun(_, r)` unwrapped per level). Arity-1 keeps
                // the curried `ResultTy = codomain`. A shape that isn't the expected
                // saturated nesting falls back to arity-1.
                let arity = valueStructArity currentTypars selfKey e

                let peeled =
                    let rec loop n extrasRev (curBody: TastAccessor.ExprId) (curTy: FrozenType) =
                        match curTy with
                        | FTFun(_, r) ->
                            if n = 0 then
                                ValueSome(List.rev extrasRev, curBody, r)
                            else
                                match curBody with
                                | TastAccessor.ELambda lam ->
                                    match TastAccessor.patBinder lam.Param with
                                    | ValueSome pk ->
                                        loop
                                            (n - 1)
                                            ((pk, TastAccessor.patTy lam.Param, lam.Param) :: extrasRev)
                                            lam.Body
                                            (TastAccessor.exprTy curBody)
                                    | ValueNone -> ValueNone
                                | _ -> ValueNone
                        | _ -> ValueNone

                    loop (arity - 1) [] body lamTy

                let funArity, extraParams, body, resultTy =
                    match peeled with
                    | ValueSome(extras, innerBody, r) when arity >= 2 && not (List.isEmpty extras) ->
                        arity, extras, innerBody, r
                    | _ ->
                        let resultTy =
                            match lamTy with
                            | FTFun(_, r) -> r
                            | _ -> failwithf "Emit: closure type is not a function: %A" lamTy

                        1, [], body, resultTy

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
                        match Map.tryFind k closureReprs with
                        | Some r -> r
                        | None -> ClosureRepr.Heap
                    | ValueNone -> ClosureRepr.Heap

                // Bind every leaf each param pattern introduces (a tuple's element
                // bindings), not the placeholder `ParamKey` — those leaves are
                // parameters, never captures. A flat closure binds ALL its params
                // (the peeled inner `Lambda`s' binders too), against the inner body.
                let paramBound =
                    patKeys paramPat
                    @ (extraParams |> List.collect (fun (_, _, ppat) -> patKeys ppat))

                let captures = freeVars nonCaptured paramBound selfKey body

                // The CODEGEN value-struct trigger — the stricter gate
                // (necessary-not-sufficient `Repr` is NOT consulted). An *anonymous*
                // lambda (`ValueNone` selfKey — a `let`-bound closure keeps its heap
                // shape) threaded through a constrained `Fun`2`/`Fun`3` slot, monomorphic;
                // the node-keyed verdict (`valueStructArity` ≥ 1 ⇒ in the table). A
                // CAPTURING such lambda is also a value-struct (captures stored by
                // value); the flat-2 arity is supported too. A plain value struct
                // copies by value, so passing it into
                // the combinator stays escape-free (no `ref struct`). Everything else is
                // heap.
                let isValueStruct =
                    currentTypars = 0 && ValueOption.isNone selfKey && stackLambdaArgs.ContainsKey e

                let c =
                    {
                        Node = e
                        Name = namer.NextName(e, selfKey)
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
                        FunArity = funArity
                        ExtraParams = extraParams
                    }

                lookup.[e] <- c
                order.Add e

            match e with
            | TastAccessor.ELambda lam ->
                let pat = lam.Param
                let body = lam.Body
                let lamTy = TastAccessor.exprTy e

                match TastAccessor.patKind pat with
                | PatShape.NamedSimple ->
                    match TastAccessor.patBinder pat with
                    | ValueSome p -> registerClosure p (TastAccessor.patTy pat) pat body lamTy
                    | ValueNone -> ()
                | PatShape.Const when TastAccessor.patConstValue pat = TConstValue.Unit ->
                    // A `fun () ->` unit binder has no name to reference, but the
                    // closure's `Invoke` still allocates `ldarg.1` for the unit
                    // value the caller pushes; mint a synthetic placeholder so the
                    // `args` map (and `freeVars`'s bound set) still has a key.
                    registerClosure (TastPoolBuilder.mintBinder pat.Pool) (TastAccessor.patTy pat) pat body lamTy
                | PatShape.Tuple ->
                    // A tuple-param lambda (`fun (a, b) -> …`). The single `ldarg.1`
                    // carries the `ValueTuple`n` value; mint a synthetic placeholder
                    // for that slot — `buildClosureInvoke` `bindPattern`s the leaf
                    // element bindings out of it. The param's `FTTuple` (`patTy pat`) is
                    // what the closure's `Invoke` signature encodes.
                    registerClosure (TastPoolBuilder.mintBinder pat.Pool) (TastAccessor.patTy pat) pat body lamTy
                | _ -> failwithf "Emit: closure parameter destructuring is out of scope: %A" pat
            | _ -> ()

        let typarsForStaticFn (k: BinderId) : int =
            match staticFnTypars.TryGetValue k with
            | true, n -> n
            | false, _ -> 0

        for d in decls do
            match d with
            | TastAccessor.DLet letd ->
                match TastAccessor.patBinder letd.Binding with
                | ValueSome k ->
                    if staticFnKeys.Contains k then
                        // A static-method function's lambda is not a closure, but its body
                        // may still construct inner closures — walk only the body. The
                        // closures inherit the method's typars.
                        let _, body = peelLambda letd.Value
                        go (typarsForStaticFn k) 0 ValueNone body
                    else
                        go 0 0 (ValueSome k) letd.Value
                | ValueNone -> go 0 0 ValueNone letd.Value
            | TastAccessor.DExpression(e, _) -> go 0 0 ValueNone e
            | _ -> ()

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
