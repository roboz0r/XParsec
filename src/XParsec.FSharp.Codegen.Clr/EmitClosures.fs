namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open EmitTypes
open EmitLower

module EmitClosures =
    let private patKeys (p: TastAccessor.PatId) : BoundVarId list =
        let acc = ResizeArray<BoundVarId>()

        let rec go p =
            match TastAccessor.patKind p with
            | PatShape.NamedSimple ->
                match TastAccessor.patBoundVar p with
                | ValueSome k -> acc.Add k
                | ValueNone -> ()
            // An or-pattern that binds names is rejected before lowering, so its alternatives
            // are not walked; every other composite is.
            | PatShape.Or -> ()
            | _ ->
                for sub in TastAccessor.patChildren p do
                    go sub

        go p
        List.ofSeq acc

    /// `let name = <lambda> in body` — the shape that anchors an inner closure to its bound variable
    /// name. A tuple/record bound variable or a non-lambda value declines.
    [<return: Struct>]
    let private (|LetBoundLambda|_|)
        (e: TastAccessor.ExprId)
        : struct (BoundVarId * TastAccessor.ExprId * TastAccessor.ExprId) voption =
        match e with
        | TastAccessor.ELet letv ->
            match TastAccessor.patBoundVar letv.Pattern, TastAccessor.exprKind letv.Value with
            | ValueSome k, ExprShape.Lambda -> ValueSome(struct (k, letv.Value, letv.Body))
            | _ -> ValueNone
        | _ -> ValueNone

    /// Walk `body`, invoking `onFree key ty` once per `Var` not shadowed by `bound`, which is
    /// mutated in place, so pass a private set. `let rec f = <lambda>` scopes `f` across its own
    /// VALUE too: the self-reference is the closure's `this`, not a phantom capture.
    let private walkFreeRefs
        (bound: HashSet<BoundVarId>)
        (onFree: BoundVarId -> FrozenType -> unit)
        (body: TastAccessor.ExprId)
        : unit =
        let scoped (keys: BoundVarId list) (k: unit -> unit) =
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
                scoped (patKeys letv.Pattern) (fun () -> go letv.Body)
            | TastAccessor.EUse usev ->
                go usev.Value
                scoped (patKeys usev.Pattern) (fun () -> go usev.Body)
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

    /// The free variables of a closure body, in first-occurrence order, which drives
    /// capture field order. `staticFnKeys` are excluded: a reference to a
    /// static-method function is a direct `call`, not a captured value.
    let private freeVars
        (staticFnKeys: HashSet<BoundVarId>)
        (paramKeys: BoundVarId list)
        (selfKey: BoundVarId voption)
        (body: TastAccessor.ExprId)
        : (BoundVarId * FrozenType) list =
        let bound = HashSet<BoundVarId>()
        // Every bound variable the parameter pattern introduces: for `fun (a, b) -> …` that
        // is `a` and `b`, not the placeholder slot.
        for k in paramKeys do
            bound.Add k |> ignore

        bound.UnionWith staticFnKeys // static-method references are calls, not captures

        match selfKey with
        | ValueSome k -> bound.Add k |> ignore // the recursive self isn't captured because it's `this`
        | ValueNone -> ()

        let acc = ResizeArray<BoundVarId * FrozenType>()
        let seen = HashSet<BoundVarId>()

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
    let private freeVarKeys (boundKeys: BoundVarId seq) (body: TastAccessor.ExprId) : HashSet<BoundVarId> =
        let bound = HashSet<BoundVarId>(boundKeys)
        let acc = HashSet<BoundVarId>()
        walkFreeRefs bound (fun key _ -> acc.Add key |> ignore) body
        acc

    /// What a top-level decl emits as: from a recorded identity, or minted as residue.
    /// Only the first is a symbol a consumer could resolve.
    type Emission =
        {
            Name: string
            /// `None` ⇒ the anonymous "Program" class, which the CLR needs because it has
            /// no namespace-level member.
            ModuleClass: ModuleClassKey option
            SymbolKey: SymbolKey
        }

    /// Name and key come straight off the front end's recorded identity, filed for every
    /// module-level `let` with a simple bound variable. A top-level binding's identity belongs
    /// to its file's NAMESPACE, which no CLR type corresponds to, so it emits on the Program class.
    let private declaredEmission (info: ModuleBindingInfo) : Emission =
        {
            Name = info.Name
            ModuleClass =
                match info.DeclaringModule with
                | ValueSome m -> Some m
                | ValueNone -> None
            SymbolKey = info.Key
        }

    /// Mints `<name>$<slot>` (`value$3` when no source names the variable) for a decl with no
    /// exportable identity: a `let` lowered out of the entry expression (a value written after
    /// a top-level `do`), or one a LATER binding in the same module class re-binds.
    let private residueEmission (programClass: ModuleClassKey) (pool: PoolBuilder) (k: BoundVarId) : Emission =
        let (BoundVarId slot) = k

        let source =
            match TastPoolBuilder.boundVarNaming pool k with
            | BoundVarNaming.Source n -> n
            | BoundVarNaming.Minted _ -> "value"

        let name = sprintf "%s$%d" source slot

        {
            Name = name
            ModuleClass = None
            SymbolKey = SymbolKeyOps.valueKey (ModuleContainer.InModule programClass) name
        }

    /// How EVERY top-level decl of a file emits. Decided over the whole list, because
    /// shadowing is: `let x = 1` then `let x = x + 10` carries one identity, so only the LAST
    /// takes it and the earlier ones, which still need storage, take the residue mint.
    let emissions
        (moduleMembers: Map<BoundVarId, ModuleBindingInfo>)
        (programClass: ModuleClassKey)
        (decls: TastAccessor.DeclId list)
        : Dictionary<BoundVarId, Emission> =
        let bound =
            [
                for d in decls do
                    match d with
                    | TastAccessor.DLet letd ->
                        match letd.Pattern with
                        | TastAccessor.PNamed k -> k, letd.Value.Pool
                        | _ -> ()
                    | _ -> ()
            ]

        // The bound variable each identity ends up naming: later declarations overwrite earlier
        // ones, exactly as the name environment does.
        let owner = Dictionary<SymbolKey, BoundVarId>()

        for (k, _) in bound do
            match Map.tryFind k moduleMembers with
            | Some info -> owner.[info.Key] <- k
            | None -> ()

        let result = Dictionary<BoundVarId, Emission>()

        for (k, pool) in bound do
            result.[k] <-
                match Map.tryFind k moduleMembers with
                | Some info when owner.[info.Key] = k -> declaredEmission info
                | _ -> residueEmission programClass pool k

        result

    /// Every non-`inline`, non-`Lambda` `let name = value`. `tyOk` selects which type shapes
    /// qualify; `project` builds the caller's row, returning `None` to decline (a caller that
    /// wants only named-module values declines a Program-class `Emission`, and vice versa).
    let private classifyModuleValues
        (emissions: Dictionary<BoundVarId, Emission>)
        (tyOk: FrozenType -> bool)
        (project: BoundVarId -> FrozenType -> TastAccessor.ExprId -> Emission -> 'a option)
        (decls: TastAccessor.DeclId list)
        : 'a list =
        decls
        |> List.choose (fun d ->
            match d with
            | TastAccessor.DLet letd ->
                match letd.Pattern with
                | TastAccessor.PNamed k when
                    not letd.IsInline
                    && TastAccessor.exprKind letd.Value <> ExprShape.Lambda
                    && tyOk (TastAccessor.patTy letd.Pattern)
                    ->
                    project k (TastAccessor.patTy letd.Pattern) letd.Value emissions.[k]
                | _ -> None
            | _ -> None
        )

    /// The **module values**: a non-inline `let name = <plain value>` on a NAMED module
    /// whose type is fully ground. Each becomes a `public static` field initialised by
    /// the module class's `.cctor`; every reference is an `ldsfld`, never a local or a capture.
    let collectModuleValues
        (emissions: Dictionary<BoundVarId, Emission>)
        (decls: TastAccessor.DeclId list)
        : ModuleValue list =
        decls
        |> classifyModuleValues
            emissions
            ftIsGround
            (fun k ty value em ->
                // Only a NAMED-module ground value is a field here.
                match em.ModuleClass with
                | None -> None
                | Some moduleClass ->
                    Some
                        {
                            Key = k
                            SymbolKey = em.SymbolKey
                            Name = em.Name
                            Ty = ty
                            Init = value
                            ModuleClass = moduleClass
                        }
            )

    /// No untyped position (`FTUnknown`) and no body-local typar (`FTLocalTypar`);
    /// neither is grounded, so neither encodes into a signature. A value carrying one is
    /// skipped, not an error, because `let f () = let g = fun x -> x in (g, g)` is a legal program.
    let rec private ftNoUnknown (t: FrozenType) : bool =
        match t with
        | FTUnknown _
        | FTLocalTypar _ -> false
        | t -> FrozenType.forallChildren ftNoUnknown t

    /// The GENERIC module-level values (`let empty : SetTree<'T> = …`). A module class has no
    /// type parameter to type a `SetTree<'T>` field, so each lowers to a zero-arg generic
    /// static method, an ordinary 0-param `StaticFn`. A reference `call`s its `MethodSpec`.
    let collectGenericModuleValues
        (emissions: Dictionary<BoundVarId, Emission>)
        (decls: TastAccessor.DeclId list)
        : StaticFn list =
        // Open but encodable, and not itself a function type: a non-lambda
        // `let f : 'T -> 'T = id` can still produce a stored closure, which is deferred.
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
            emissions
            tyOk
            (fun k ty value em ->
                Some
                    {
                        Key = k
                        SymbolKey = em.SymbolKey
                        Name = em.Name
                        ModuleClass = em.ModuleClass
                        // A generic module VALUE reaches codegen as a bare `Var`, so it has
                        // no source groups and never returns `void`.
                        Params = CompiledFns.FlatParams.ofSegments []
                        Body = value
                        ResultTy = ty
                        ReturnsVoid = false
                        // A generic module VALUE carries no front-end scheme bounds.
                        Constraints = []
                    }
            )

    /// The TOP-LEVEL ground values — those declaring no enclosing module. Each becomes a
    /// `public static` field on the anonymous "Program" class; whether it initialises in
    /// the `.cctor` or in `Main` is decided later.
    let collectProgramValues
        (emissions: Dictionary<BoundVarId, Emission>)
        (programClass: ModuleClassKey)
        // Every `[<Struct; IsByRefLike>]` type declared in this assembly. Lowering strips
        // type decls, so the caller computes this from the unlowered decls.
        (refStructKeys: HashSet<TypeKey>)
        (decls: TastAccessor.DeclId list)
        : ModuleValue list =
        // The CLR confines a byref-like type to the stack, so a `[<Struct; IsByRefLike>]`
        // value, or a byref, stays a `Main` local rather than becoming a static field.
        let isFieldEmittable (ty: FrozenType) =
            match ty with
            | FTClass(key, _) -> not (refStructKeys.Contains key)
            | FTByref _ -> false
            | _ -> true

        // Ground (a field, not a generic method), not a stored closure, and storable.
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
            emissions
            tyOk
            (fun k ty value em ->
                // A named-module value (`module Foo`) takes the named-module path.
                match em.ModuleClass with
                | Some _ -> None
                | None ->
                    Some
                        {
                            Key = k
                            SymbolKey = em.SymbolKey
                            Name = em.Name
                            Ty = ty
                            Init = value
                            ModuleClass = programClass
                        }
            )

    /// A module value's initialiser runs in its module class's `.cctor`, where only other module
    /// values (`ldsfld`) and static-method functions (direct `call`) resolve, because any
    /// other top-level reference would need a `Main` local no `.cctor` can see.
    let validateModuleValueInits
        (moduleValueKeys: HashSet<BoundVarId>)
        (staticFnKeys: HashSet<BoundVarId>)
        (moduleValues: ModuleValue list)
        : unit =
        for mv in moduleValues do
            for free in freeVarKeys [] mv.Init do
                if not (moduleValueKeys.Contains free || staticFnKeys.Contains free) then
                    failwithf
                        "Emit: module value '%s' references top-level binding %O, which is neither a module value nor a static method, so its initialiser cannot run in the module class's .cctor"
                        mv.Name
                        free

    /// Eta-expand every NON-saturated reference to an `eligible` function to its source arity,
    /// keeping the flat static method a cross-assembly consumer `call`s:
    ///   `f` → `fun a0 … a(n-1) -> f a0 … a(n-1)`, and `f x` → that lambda applied to `x`.
    let bridgeStaticFnEscapes
        (eligible: HashSet<BoundVarId>)
        (fns: CompiledFns.CompiledFn list)
        (decls: TastAccessor.DeclId list)
        : TastAccessor.DeclId list =
        // Each eligible function's source arity: the parameters the eta-expansion peels, and
        // the argument count at or above which a reference is a saturated direct `call`.
        let arity = Dictionary<BoundVarId, int>()

        for f in fns do
            if eligible.Contains f.Key then
                arity.[f.Key] <- f.Params.GroupCount

        if arity.Count = 0 then
            decls
        else
            // `fun a0 … a(n-1) -> f a0 … a(n-1)`, typed from the reference's own curried
            // type. A tuple / unit source group needs no special case: the single fresh
            // param carries the group's domain and is passed as one argument.
            let buildEta (fVar: TastAccessor.ExprId) (n: int) : TastAccessor.ExprId =
                let tok = TastAccessor.exprTok fVar

                // Each peeled `->` as `(domain, codomain)`: the fresh param's type and the
                // intermediate `App` result type.
                let levels = TastLower.peelFuns n (typeOfExpr fVar)

                if List.length levels <> n then
                    failwithf
                        "bridgeStaticFnEscapes: function type has fewer than %d parameters: %A"
                        n
                        (typeOfExpr fVar)

                let keys = levels |> List.map (fun _ -> TastPoolBuilder.mintBoundVar fVar.Pool)

                // The bridge's nodes are in no frozen tree, so they append to the same pool
                // `fVar` lives in, letting the spliced `fVar` keep its own id.
                let pool = fVar.Pool

                let appliedArgs: TastAccessor.AppliedArg list =
                    List.map2
                        (fun k (dom, cod) ->
                            {
                                Arg = TastAccessor.mintVar pool k dom tok
                                StepResultTy = cod
                                Tok = tok
                            }
                        )
                        keys
                        levels

                let body = TastAccessor.mintAppChain fVar appliedArgs

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
                    let fn, args = TastAccessor.collectAppChain [] e

                    match fn with
                    | TastAccessor.EVar k when arity.ContainsKey k && List.length args < arity.[k] ->
                        // Under-application: partially apply the eta closure.
                        TastAccessor.mintAppChain
                            (buildEta fn arity.[k])
                            (args |> List.map (fun a -> { a with Arg = rw a.Arg }))
                    | _ ->
                        // The application STANDS: a saturated (or over-applied) eligible
                        // function stays a direct `call`, so only the arguments and a
                        // non-eligible function can move.
                        TastAccessor.mapAppChain
                            (fun h ->
                                match h with
                                | TastAccessor.EVar k when arity.ContainsKey k -> h
                                | _ -> rw h
                            )
                            rw
                            e
                | _ -> TastAccessor.mapChildren rw e

            decls |> List.map (TastAccessor.mapDeclExpr rw)

    /// The static-method-eligible top-level functions. `let [rec] f p0 … = body` is eligible
    /// unless it captures a module-level LOCAL, because a capture field needs a `this` a static
    /// method has none of. So its free vars must all be eligible too: a fixpoint over the offenders.
    let staticEligible (moduleValueKeys: HashSet<BoundVarId>) (fns: CompiledFns.CompiledFn list) : HashSet<BoundVarId> =
        let candidates = Dictionary<BoundVarId, CompiledFns.CompiledFn>()
        let order = ResizeArray<BoundVarId>()

        for f in fns do
            candidates.[f.Key] <- f
            order.Add f.Key

        // Each candidate's capture set (free vars minus its own params + module values).
        let bodyFree =
            Dictionary<BoundVarId, HashSet<BoundVarId>>(
                seq {
                    for k in order do
                        let c = candidates.[k]
                        // A module-value reference is an `ldsfld`, not a capture, so those
                        // keys count as bound. A simple/unit param binds its own `Slot`; a
                        // tuple param binds each variable the pattern names.
                        let paramBound =
                            c.Params.Flat
                            |> List.collect (fun p ->
                                match p.Pat with
                                | Some pat -> patKeys pat
                                | None -> [ p.Slot ]
                            )

                        KeyValuePair(k, freeVarKeys (Seq.append moduleValueKeys paramBound) c.Body)
                }
            )

        // Every gathered function starts eligible; only the capture fixpoint removes one.
        let eligible = HashSet<BoundVarId>(order)
        let mutable changed = true

        while changed do
            changed <- false

            for k in List.ofSeq eligible do
                let free = bodyFree.[k]

                let captures = free |> Seq.exists (fun v -> v <> k && not (eligible.Contains v))

                if captures && eligible.Remove k then
                    changed <- true

        eligible

    /// Each gathered function whose key is in the precomputed `eligible` set, named by the
    /// file's `emissions` table. A function NOT in the set is left for closure discovery:
    /// capture-demoted, or newly turned into a lambda by the escape bridging.
    let collectStaticFns
        (emissions: Dictionary<BoundVarId, Emission>)
        // Per-binding frozen typar bounds from the front-end scheme; absent ⇒ no bounds.
        (genericFnSchemes: Map<BoundVarId, FrozenConstraint list>)
        (eligible: HashSet<BoundVarId>)
        (fns: CompiledFns.CompiledFn list)
        : StaticFn list =
        [
            for c in fns do
                if eligible.Contains c.Key then
                    let em = emissions.[c.Key]

                    let constraints =
                        match Map.tryFind c.Key genericFnSchemes with
                        | Some cs -> cs
                        | None -> []

                    yield
                        {
                            Key = c.Key
                            SymbolKey = em.SymbolKey
                            Name = em.Name
                            ModuleClass = em.ModuleClass
                            Params = c.Params
                            Body = c.Body
                            ResultTy = c.ResultTy
                            ReturnsVoid = c.ReturnsVoid
                            Constraints = constraints
                        }
        ]

    /// `max i + 1` over every `FTTypar(Method, i)` in the params, result AND body. The body
    /// is walked because `fold`'s enumerator `'E` occurs only in its `for-in` descriptor. Counting
    /// occurrences drops an ERASED typar, whose slot would leave an unrecoverable arg.
    let staticFnTypars (fn: StaticFn) : int =
        let mutable maxIx = -1

        // A `Declaring`-axis typar can't occur in a module-level static fn, so it falls
        // through the child walk as a leaf.
        let rec go (t: FrozenType) =
            match t with
            | FTTypar(TyparAxis.Method, i) ->
                if i > maxIx then
                    maxIx <- i
            | t -> FrozenType.iterChildren go t

        for p in fn.Params.Flat do
            go p.Ty

        go fn.ResultTy

        // The only method indices param/result cannot see live in a `for-in` enumerator
        // descriptor: its `ConstrainedInterface` ifaceArgs and the enumerator type itself.
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

    /// A closure-discovery root from a type MEMBER body: the body plus the typars in scope at
    /// its construction site, which the closure re-projects onto its own class typars.
    type MemberClosureRoot =
        {
            /// The declaring type's typar count, which is the closure's declaring-typar
            /// offset: its first slots are the class typars.
            DeclaringTypars: int
            /// The member's own method-typar count. These follow the class
            /// typars in the closure's typar list (offset `DeclaringTypars`).
            MethodTypars: int
            Body: TastAccessor.ExprId
        }

    /// The source-lambda nodes that lower onto a zero-alloc value-struct, each mapped to the
    /// FLAT arity its constrained slot demands (`1` for `Fun<_,_>`, `2` for `Fun<_,_,_>`). The
    /// front end recorded that against the lambda's own node, so this only tests membership.
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

    /// Mints `<closure>$0`, `<closure>$1`, … in discovery order. The name IS the closure's
    /// TypeDef slot key, resolved assembly-wide by name, so ONE namer threaded across every
    /// `discoverClosures` call is what keeps those keys unique across the whole assembly.
    type ClosureNamer() =
        let mutable counter = 0

        /// `node` and `selfKey` are the context a debuggable `<bound-name>@<line>` policy
        /// would need. A counter ignores them, but only this member would have to change.
        member _.NextName(_node: TastAccessor.ExprId, _selfKey: BoundVarId voption) : string =
            let name = sprintf "<closure>$%d" counter
            counter <- counter + 1
            name

    /// Every `Lambda` in the lowered tree with its capture set, leaves-first: a closure comes
    /// before any closure that constructs it. A `staticFnKeys` outer lambda is NOT a closure, so
    /// only its body is walked; the closures found there inherit its typars.
    let discoverClosures
        (namer: ClosureNamer)
        (staticFnKeys: HashSet<BoundVarId>)
        (moduleValueKeys: HashSet<BoundVarId>)
        (staticFnTypars: IReadOnlyDictionary<BoundVarId, int>)
        (funVerdicts: IReadOnlyDictionary<TastAccessor.ExprId, FunVerdict>)
        (closureReprs: Map<BoundVarId, ClosureRepr>)
        (decls: TastAccessor.DeclId list)
        (memberRoots: MemberClosureRoot list)
        : Closure list * Dictionary<TastAccessor.ExprId, Closure> =
        let order = ResizeArray<TastAccessor.ExprId>()
        let lookup = Dictionary<TastAccessor.ExprId, Closure>()

        // Source lambdas threaded through a constrained `Fun`2`/`Fun`3` slot, mapped to
        // their flat arity (1 or 2).
        let stackLambdaArgs = collectStackLambdaArgs funVerdicts decls memberRoots

        // A module-level value is an `ldsfld` and a static-method reference is a direct
        // `call`, so neither needs a capture.
        let nonCaptured = HashSet<BoundVarId>(staticFnKeys)
        nonCaptured.UnionWith moduleValueKeys

        // `1` by default, `2` for a flat `Fun`3` slot, and only for an ANONYMOUS
        // monomorphic lambda the verdict reached, which is what `selfKey` / `currentTypars`
        // gate on here.
        let valueStructArity (currentTypars: int) (selfKey: BoundVarId voption) (e: TastAccessor.ExprId) : int =
            if currentTypars = 0 && ValueOption.isNone selfKey then
                match stackLambdaArgs.TryGetValue e with
                | true, arity -> arity
                | false, _ -> 1
            else
                1

        // `currentTypars` is the typar count inherited from the enclosing method / closure, of
        // which `declaringOffset` leading slots are the enclosing class's (`0` for a static-fn
        // closure). `selfKey` is set on a `let f = …` value: its self-reference is `this`.
        let rec go (currentTypars: int) (declaringOffset: int) (selfKey: BoundVarId voption) (e: TastAccessor.ExprId) =
            // A flat value-struct lambda of arity `2..4` peels its inner `Lambda` levels into
            // the SAME closure's extra params (one `Invoke(a,b,…)`), so those inner lambdas
            // are not independent closures. Recurse into the DEEPEST body instead.
            let flatInner =
                let arity = valueStructArity currentTypars selfKey e

                if arity >= 2 then
                    // `ValueNone` if the shape isn't that saturated nesting, and this node
                    // walks its children normally.
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
                 // The bound variable anchors an inner closure to its name, scoped across the value.
                 | LetBoundLambda(k, value, body) ->
                     go currentTypars declaringOffset (ValueSome k) value
                     go currentTypars declaringOffset ValueNone body
                 | _ -> iterChildren (go currentTypars declaringOffset ValueNone) e) // children (and inner lambdas) first → leaves-first

            let registerClosure
                (p: BoundVarId)
                (pty: FrozenType)
                (paramPat: TastAccessor.PatId)
                (body: TastAccessor.ExprId)
                (lamTy: FrozenType)
                =
                // Each peeled inner `NamedSimple` lambda contributes one extra flat param,
                // walking `body` / `resultTy` down to the innermost body and its codomain.
                // Arity-1, and any shape that isn't the expected nesting, stays curried.
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
                                    match TastAccessor.patBoundVar lam.Param with
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

                // The front-end regions snapshot, keyed by the closure's bound variable (`let f = …`).
                // An anonymous lambda or a bound variable the snapshot didn't reach defaults to `Heap`.
                let repr =
                    match selfKey with
                    | ValueSome k ->
                        match Map.tryFind k closureReprs with
                        | Some r -> r
                        | None -> ClosureRepr.Heap
                    | ValueNone -> ClosureRepr.Heap

                // Every bound variable each param pattern introduces (a tuple's element bindings),
                // not the placeholder `ParamKey`, because those are parameters, never captures.
                // A flat closure binds the peeled inner lambdas' bound variables too.
                let paramBound =
                    patKeys paramPat
                    @ (extraParams |> List.collect (fun (_, _, ppat) -> patKeys ppat))

                let captures = freeVars nonCaptured paramBound selfKey body

                // The codegen trigger, stricter than `Repr`, which is NOT consulted: an
                // ANONYMOUS monomorphic lambda (a `let`-bound one keeps its heap shape) in a
                // constrained `Fun`2`/`Fun`3` slot. Captures are allowed, stored by value.
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
                    match TastAccessor.patBoundVar pat with
                    | ValueSome p -> registerClosure p (TastAccessor.patTy pat) pat body lamTy
                    | ValueNone -> ()
                | PatShape.Const when TastAccessor.patConstValue pat = TConstValue.Unit ->
                    // A `fun () ->` bound variable has no name, but `Invoke` still allocates
                    // `ldarg.1` for the unit value the caller pushes, so mint a placeholder
                    // to give the args map a key for it.
                    registerClosure (TastPoolBuilder.mintBoundVar pat.Pool) (TastAccessor.patTy pat) pat body lamTy
                | PatShape.Tuple ->
                    // `fun (a, b) -> …`: the single `ldarg.1` carries the `ValueTuple`n`, so
                    // mint a placeholder for that slot and let `Invoke` destructure `a` / `b`
                    // out of it. The `Invoke` signature encodes the param's `FTTuple`.
                    registerClosure (TastPoolBuilder.mintBoundVar pat.Pool) (TastAccessor.patTy pat) pat body lamTy
                | _ -> failwithf "Emit: closure parameter destructuring is out of scope: %A" pat
            | _ -> ()

        let typarsForStaticFn (k: BoundVarId) : int =
            match staticFnTypars.TryGetValue k with
            | true, n -> n
            | false, _ -> 0

        for d in decls do
            match d with
            | TastAccessor.DLet letd ->
                match TastAccessor.patBoundVar letd.Pattern with
                | ValueSome k ->
                    if staticFnKeys.Contains k then
                        // The outer lambda is not a closure, but its body may construct inner
                        // ones, which inherit the method's typars.
                        let _, body = peelLambda letd.Value
                        go (typarsForStaticFn k) 0 ValueNone body
                    else
                        go 0 0 (ValueSome k) letd.Value
                | ValueNone -> go 0 0 ValueNone letd.Value
            | TastAccessor.DExpression(e, _) -> go 0 0 ValueNone e
            | _ -> ()

        // A member body sees no `selfKey`: the member dispatches as a call, not a captured
        // value. Its closures' typar list is the declaring class typars (offset 0) followed
        // by the member's own method typars.
        for root in memberRoots do
            go (root.DeclaringTypars + root.MethodTypars) root.DeclaringTypars ValueNone root.Body

        [ for n in order -> lookup.[n] ], lookup
