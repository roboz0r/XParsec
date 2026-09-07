namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open Vesper
open XParsec.FSharp
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Passes.UnificationEngineCore

// Expanding one `inline` template at one call site: substituting the caller's types through
// the body, selecting its `StaticOptimization` clauses, dispatching its `TraitCall`s, and the
// tree surgery a reduction needs (`freshen` / `betaReduce`). Pre-freeze: types go via union-find.

module Inline =

    /// An SRTP trait call the expansion could NOT rewrite. `memberName` is the compiled
    /// member name (`op_Addition`); the types are SUBSTITUTED.
    type UnresolvedTrait =
        /// No type in the support set carries the member; `supportTys` is the searched set.
        | NoSupport of supportTys: Block<SemType> * memberName: string
        /// More than one type carries the member; `supportTys` is those SUPPORTING types.
        | Ambiguous of supportTys: Block<SemType> * memberName: string
        /// Members of the name exist but no signature admits the operands (`1 + 1L`).
        /// `candidateTy` is the first such member's type and `expectedTy` the trait
        /// shape it was searched with; both are ground.
        | SignatureMismatch of candidateTy: SemType * expectedTy: SemType

    /// A module-level `let` whose body is exactly one zero-operand intrinsic
    /// (`let undefined = (# "undefined" #)`), yielding the body to splice: such a binding is a
    /// compile-time ALIAS, so every reference splices the body rather than calling it.
    let nullaryIntrinsicValueBody (decl: TDecl) : TExpr voption =
        match decl with
        | TDecl.Let(binding = { Value = body }) when (TExprG.nullaryIntrinsicText body).IsSome -> ValueSome body
        | _ -> ValueNone

    /// Substitute typar roots present in `subst`: chase each `TyVar` to its union-find root
    /// and swap. Roots absent from `subst` stay abstract.
    let rec private substType (store: TypeStore) (subst: Dictionary<TyVarId, SemType>) (t: SemType) : SemType =
        match t with
        | TyVar tv ->
            let root = UnionFind.find store tv

            match subst.TryGetValue root.Id with
            | true, repl -> repl
            | _ -> TyVar root.Id
        // `mapChildren` rebuilds a `TyOr` through its smart constructor: substituting a
        // member can collapse or reorder the set.
        | t -> SemType.mapChildren (substType store subst) t

    /// Structural match of two (already typar-substituted) `SemType`s for a static-optimization
    /// `when ^T : Type` clause. `TyVar`s compare by union-find root, so a reflexive `when ^T : ^T`
    /// matches unconditionally; `TyConst`s by exact `SymbolKey`, both dealiased by name resolution.
    let rec private staticOptTypesMatch (store: TypeStore) (a: SemType) (b: SemType) : bool =
        match a, b with
        | TyVar x, TyVar y -> UnionFind.find store x = UnionFind.find store y
        | TyConst(k1, xs), TyConst(k2, ys) -> k1 = k2 && Block.forall2 (staticOptTypesMatch store) xs ys
        | TyFun(a1, r1), TyFun(a2, r2) -> staticOptTypesMatch store a1 a2 && staticOptTypesMatch store r1 r2
        | TyTuple xs, TyTuple ys -> Block.forall2 (staticOptTypesMatch store) xs ys
        | TyRecord(n1, xs), TyRecord(n2, ys)
        | TyUnion(n1, xs), TyUnion(n2, ys)
        | TyClass(n1, xs), TyClass(n2, ys) -> n1 = n2 && Block.forall2 (staticOptTypesMatch store) xs ys
        | _ -> false

    /// `when ^T : struct` as a clause guard: the target's layout, else what the declaration
    /// asked for. An unsettled layout selects no clause, falling to the base.
    let private isStructType (ctx: PassContext) (t: SemType) : bool =
        TypeLayout.ofSemType ctx t = TypeLayout.Value

    /// Build the typar-substituting mapper for one inline expansion. Typars are pinned by the
    /// time it runs, so a `StaticOptimization` keeps only the first clause whose constraints
    /// hold; `declined` is the sink for the trait calls that cannot be resolved.
    let rec private substMapper
        (ctx: PassContext)
        (declined: ResizeArray<UnresolvedTrait>)
        (subst: Dictionary<TyVarId, SemType>)
        : TastWalk.Mapper =
        let sub = substType ctx.Store subst

        let holds (c: TStaticOptConstraint) =
            match c with
            | TStaticOptConstraint.TyconEquals(typar, required) ->
                staticOptTypesMatch ctx.Store (sub typar) (sub required)
            | TStaticOptConstraint.IsStruct typar -> isStructType ctx (sub typar)

        // No clause body is a trait call: the arithmetic bodies carry the SRTP dispatch in the
        // BASE, with an explicit clause per supported primitive, so an operand matching no
        // clause falls to the base, where the trait call decides whether the type has it.
        let clauseSelected (cl: TStaticOptClause) = cl.Constraints |> Block.forall holds

        let resolveStaticOpt (clauses: Block<TStaticOptClause>) (defaultExpr: TExpr) : TExpr =
            let m = substMapper ctx declined subst

            match clauses |> Block.tryFind clauseSelected with
            | ValueSome cl -> TastWalk.mapExpr m cl.Body
            | ValueNone -> TastWalk.mapExpr m defaultExpr

        // Rewrite a substituted `TraitCall` to a `StaticMethodCall` on the one support-set
        // type whose static member matches; a failed search declines into `declined`. The
        // result type is `sub ty`, because `Vec2 * float -> Vec2` returns neither operand's type.
        let resolveTraitCall
            (m: TastWalk.Mapper)
            (supportTys: Block<SemType>)
            (memberName: string)
            (args: Block<TExpr>)
            (ty: SemType)
            (tok: SyntaxToken)
            : TExpr voption =
            let candidates = supportTys |> Block.map sub |> Block.distinct
            let argTys = args |> Block.map (fun a -> sub (TastWalk.exprTy a))
            let retTy = sub ty

            let mint (c: UnificationTraitMembers.TraitCandidate) : SymbolKey =
                match c.Source with
                | UnificationTraitMembers.TraitMemberSource.User nm ->
                    SymbolKey.Member(
                        UnificationInferOverload.frozenUserMemberKey
                            ctx.Store
                            nm.Decl.TypeKey
                            nm.Decl.TypeParams
                            nm.Member
                    )
                | UnificationTraitMembers.TraitMemberSource.External em -> SymbolKey.Member em.Key

            match UnificationTraitMembers.pick ctx memberName argTys retTy (Block.toArray candidates) true with
            | UnificationTraitMembers.TraitPick.Resolved c ->
                ValueSome(
                    TExpr.StaticMethodCall(
                        mint c,
                        Block.ofArray c.DeclArgs,
                        Block.map (TastWalk.mapExpr m) args,
                        retTy,
                        tok
                    )
                )
            | UnificationTraitMembers.TraitPick.Ambiguous(first, rest) ->
                declined.Add(UnresolvedTrait.Ambiguous(Block.ofList [ for c in first :: rest -> c.HostTy ], memberName))

                ValueNone
            | UnificationTraitMembers.TraitPick.NameOnly(first, unsupported) ->
                (match unsupported with
                 | [] ->
                     declined.Add(
                         UnresolvedTrait.SignatureMismatch(
                             first.Ty,
                             UnificationTraitMembers.expectedShape ctx.Store argTys retTy first.Ty
                         )
                     )
                 | tys -> declined.Add(UnresolvedTrait.NoSupport(Block.ofList tys, memberName)))

                ValueNone
            // Expansion is the last chance to pin, so an incomplete set is a decline too.
            | UnificationTraitMembers.TraitPick.NoSupport
            | UnificationTraitMembers.TraitPick.Incomplete ->
                declined.Add(UnresolvedTrait.NoSupport(candidates, memberName))
                ValueNone

        { TastWalk.identityMapper with
            MapType = sub
            OverrideExpr =
                fun m e ->
                    match e with
                    | TExpr.StaticOptimization(clauses, def, _, _) -> ValueSome(resolveStaticOpt clauses def)
                    | TExpr.TraitCall(supportTys, memberName, args, ty, tok) ->
                        resolveTraitCall m supportTys memberName args ty tok
                    | _ -> ValueNone
        }

    /// Expand an `inline` binding's retained body for one call site. `typars` are the template's
    /// quantified typar roots by type-argument position; `typeArgs` are the caller's types for
    /// them at the same positions, and supplying fewer leaves the rest abstract. NodeKeys and
    /// tokens stay the template's, because freshening is the caller's job.
    let inlineExpand
        (ctx: PassContext)
        (decl: TDecl)
        (typars: TyVarId[])
        (typeArgs: SemType[])
        : TExpr * UnresolvedTrait list =
        match decl with
        | TDecl.Let(binding = { Value = value }) ->
            let subst = Dictionary<TyVarId, SemType>()

            typars
            |> Array.iteri (fun i tv ->
                if i < typeArgs.Length then
                    subst.[tv] <- typeArgs.[i]
            )

            let declined = ResizeArray<UnresolvedTrait>()
            let expanded = TastWalk.mapExpr (substMapper ctx declined subst) value
            expanded, List.ofSeq declined
        | TDecl.LetGroup _ -> invalidArg "decl" "Inline.inlineExpand expects a TDecl.Let, got a TDecl.LetGroup"
        | TDecl.Expression _ -> invalidArg "decl" "Inline.inlineExpand expects a TDecl.Let, got a TDecl.Expression"
        | TDecl.Type _ -> invalidArg "decl" "Inline.inlineExpand expects a TDecl.Let, got a TDecl.Type"

    /// Rename every bound variable NodeKey in `body`, and the references to it, to a fresh key from
    /// `mint`. Two expansions would otherwise share a bound variable, and so a codegen local slot,
    /// making nested call sites (`succ (succ x)`) clobber each other. Free vars pass through.
    /// Returns the renamed body with every renaming, source key → fresh key.
    let freshen (mint: unit -> NodeKey) (body: TExpr) : TExpr * IReadOnlyDictionary<NodeKey, NodeKey> =
        let remap = Dictionary<NodeKey, NodeKey>()

        let bind (k: NodeKey) : NodeKey =
            let k' = mint ()
            remap.[k] <- k'
            k'

        let useKey (k: NodeKey) : NodeKey =
            match remap.TryGetValue k with
            | true, k' -> k'
            | _ -> k

        // `ForTo`'s bound variable is a bare `NodeKey`, not a `TPat`, so it needs an override of its
        // own. A pattern is mapped before its body, so a bound variable is in the remap before any
        // reference to it is rewritten.
        let mapper: TastWalk.Mapper =
            { TastWalk.identityMapper with
                OverridePat =
                    fun _ p ->
                        match p with
                        | TPat.NamedSimple(k, t, tok, isMutable) ->
                            ValueSome(TPat.NamedSimple(bind k, t, tok, isMutable))
                        | _ -> ValueNone
                OverrideExpr =
                    fun m e ->
                        match e with
                        | TExpr.Var(k, t, tok) -> ValueSome(TExpr.Var(useKey k, t, tok))
                        | TExpr.ForTo(var, identTok, s, e2, b, t, tok) ->
                            let var = bind var

                            ValueSome(
                                TExpr.ForTo(
                                    var,
                                    identTok,
                                    TastWalk.mapExpr m s,
                                    TastWalk.mapExpr m e2,
                                    TastWalk.mapExpr m b,
                                    t,
                                    tok
                                )
                            )
                        | _ -> ValueNone
            }

        TastWalk.mapExpr mapper body, remap

    /// Beta-reduce a curried lambda against its applied arguments, lowering each application to
    /// a `TExpr.Let` sited at that application, its bound variable keeping the lambda parameter's own
    /// token. A leftover lambda is a partial application and is returned as it stands.
    let rec betaReduce (fn: TExpr) (args: TastWalk.AppArg list) : TExpr =
        match fn, args with
        | _, [] -> fn
        | TExpr.Lambda(TPat.NamedSimple(k, paramTy, patTok, _), lamBody, _, _), a :: rest ->
            let reduced = betaReduce lamBody rest

            TExpr.Let(
                {
                    Pattern = TPat.NamedSimple(k, paramTy, patTok, false)
                    Value = a.Arg
                    Tok = a.AppTok
                },
                reduced,
                false,
                TastWalk.exprTy reduced
            )
        | TExpr.Lambda(param, _, _, _), _ ->
            failwithf "Inline.betaReduce: inline parameter destructuring is out of scope: %A" param
        | _, _ :: _ -> failwith "Inline.betaReduce: over-application of an inline function"

    /// Replace every `Var k` in `body` with `replacement`, for a `[<CallAtMostOnce>]` parameter.
    /// Elaboration has already errored unless `k` occurs at most once and not under a lambda or
    /// loop, so this substitutes 0-or-1 times and the argument is evaluated at most once.
    let internal substituteVar (k: NodeKey) (replacement: TExpr) (body: TExpr) : TExpr =
        let m =
            { TastWalk.identityMapper with
                OverrideExpr =
                    fun _ e ->
                        match e with
                        | TExpr.Var(vk, _, _) when vk = k -> ValueSome replacement
                        | _ -> ValueNone
            }

        TastWalk.mapExpr m body

    /// The number of leading `fun x -> …` abstractions with a simple-named parameter: the arity
    /// at which a lambda argument is fully applied. A destructuring parameter (`fun (a, b) -> …`)
    /// does not count, so a use saturating past it never matches and its closure is kept.
    let rec internal lambdaArity (e: TExpr) : int =
        match e with
        | TExpr.Lambda(TPat.NamedSimple _, body, _, _) -> 1 + lambdaArity body
        | _ -> 0

    /// Rewrite what a curried lambda COMPUTES, leaving its abstractions in place: beta reduction
    /// matches on the lambda chain, and its bound variables are consumed against the arguments of
    /// whatever body it is spliced into, so the rewrite applies below them, not around them.
    let rec internal underLambdas (f: TExpr -> TExpr) (e: TExpr) : TExpr =
        match e with
        | TExpr.Lambda(param, body, ty, tok) -> TExpr.Lambda(param, underLambdas f body, ty, tok)
        | _ -> f e

    /// Of the lambda-valued inline parameters in `candidates` (key → its bound lambda), those
    /// NOT eligible for elimination: a saturated call (`f a b` where `f`'s lambda has arity 2)
    /// reduces away, but a bare `Var`, a partial or an over-application forces a real closure.
    let internal nonInlinableLambdaParams (candidates: Dictionary<NodeKey, TExpr>) (core: TExpr) : HashSet<NodeKey> =
        let bad = HashSet<NodeKey>()

        let it =
            { TastWalk.identityIter with
                VisitExpr =
                    fun iter e ->
                        match e with
                        | TExpr.App _ ->
                            // The WHOLE application at once: letting the default recursion reach
                            // a sub-`App` would measure a partial application as the arity.
                            let fn, args = TastWalk.collectAppChain [] e

                            (match fn with
                             | TExpr.Var(k, _, _) when candidates.ContainsKey k ->
                                 if List.length args <> lambdaArity candidates.[k] then
                                     bad.Add k |> ignore
                             | _ -> TastWalk.iterExpr iter fn)

                            for a in args do
                                TastWalk.iterExpr iter a.Arg

                            false
                        | TExpr.Var(k, _, _) when candidates.ContainsKey k ->
                            // A bare reference: the lambda is stored or passed on.
                            bad.Add k |> ignore
                            false
                        | _ -> true
            }

        TastWalk.iterExpr it core
        bad

    /// Recover an inline binding's type arguments at a call site by matching its declared
    /// parameter and return types against the actual argument types. A typar the arguments do
    /// not pin is left as its own `TyVar`. Positional against `typars`.
    let internal deriveInlineTypeArgs
        (store: TypeStore)
        (typars: TyVarId[])
        (declTy: SemType)
        (args: TastWalk.AppArg list)
        : SemType[] =
        if typars.Length = 0 then
            [||]
        else
            let roots = typars |> Array.map (UnionFind.find store)
            let result = Array.create roots.Length ValueNone

            let rec go (defT: SemType) (actT: SemType) =
                // The actual reads erased: a body splices over the CARRIER of a measured
                // operand, so `float<m>` grounds `^T` as `float` and selects its clause.
                match Unification.zonk store defT, UnionFind.zonkErased store actT with
                | TyVar tv, act ->
                    let r = UnionFind.find store tv

                    match roots |> Array.tryFindIndex (fun x -> x = r) with
                    | Some i ->
                        match result.[i] with
                        | ValueNone -> result.[i] <- ValueSome act
                        // A later position mapping to the SAME typar upgrades a non-ground
                        // candidate to a ground one, so an abstract operand of `(=) : ^T -> ^T
                        // -> bool` cannot starve a clause its concrete SIBLING would select.
                        | ValueSome prev when not (SemTypeQuery.isGround store prev) && SemTypeQuery.isGround store act ->
                            result.[i] <- ValueSome act
                        | ValueSome _ -> ()
                    | None -> ()
                | TyFun(a1, r1), TyFun(a2, r2) ->
                    go a1 a2
                    go r1 r2
                // A generic intrinsic carries its args structurally: the array `'T[]` is
                // `TyConst("[]", ['T])`, whose element typar is reachable only by descending
                // here. The array's `get_Item` pins `'T` solely through its object argument.
                | TyConst(_, xs), TyConst(_, ys) when xs.Length = ys.Length ->
                    for i in 0 .. xs.Length - 1 do
                        go xs.[i] ys.[i]
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

            let rec pairGo ps acts =
                match ps, acts with
                | p :: ps', a :: acts' ->
                    go p a
                    pairGo ps' acts'
                | _ -> ()

            let nArgs = List.length args

            pairGo (SemTypeQuery.Funs.domains store nArgs declTy) [ for a in args -> TastWalk.exprTy a.Arg ]

            // Pair the result position too: `failwith`'s only typar `'T` sits in the RETURN
            // (`string -> 'T`), so the parameter walk leaves it unbound. The last level's
            // `AppResultTy` is the whole application's result.
            if nArgs > 0 then
                let declRetTy = SemTypeQuery.Funs.resultAfter store nArgs declTy
                let actualRetTy = (List.last args).AppResultTy
                go declRetTy actualRetTy

            Array.mapi
                (fun i v ->
                    match v with
                    | ValueSome t -> t
                    | ValueNone -> TyVar roots.[i].Id
                )
                result

    /// Report a trait call the expansion could not dispatch, at the user's call site. An
    /// operator is named as the user WROTE it (`+`), never by the member it compiled to
    /// (`op_Addition`). A name outside that table is not an operator at all:
    /// `(^T: (member GetAwaiter: …) x)`.
    let internal reportUnresolvedTrait (ctx: PassContext) (siteTok: SyntaxToken) (u: UnresolvedTrait) : unit =
        let render (supportTys: Block<SemType>) =
            supportTys
            |> Block.toArray
            |> Array.map (shown ctx.Store)
            |> Array.distinct
            |> Block.ofArray

        let spell (memberName: string) =
            match OperatorData.sourceSpelling memberName with
            | ValueSome symbol -> MemberNoun.Operator, symbol
            | ValueNone -> MemberNoun.Member, memberName

        match u with
        | UnresolvedTrait.NoSupport(supportTys, memberName) ->
            let noun, name = spell memberName
            ctx.Report(siteTok, Kind.TraitNotSupported(render supportTys, noun, name))
        | UnresolvedTrait.Ambiguous(supportTys, memberName) ->
            let noun, name = spell memberName
            ctx.Report(siteTok, Kind.TraitAmbiguous(render supportTys, noun, name))
        | UnresolvedTrait.SignatureMismatch(candidateTy, expectedTy) ->
            // Both sides are ground, so `unify` only renders the mismatch.
            UnificationEngine.unify ctx siteTok candidateTy expectedTy

    /// How a diagnostic spells a SERVED template: as the user WROTE it wherever the name is an
    /// operator (`|>`, never `op_PipeRight`), because a spelling the source never contains cannot
    /// be looked for in it.
    let servedName (key: SymbolKey) : string =
        let (DisplayName name) = SymbolKeyOps.simpleName key

        match OperatorData.sourceSpelling name with
        | ValueSome symbol -> symbol
        | ValueNone -> name
