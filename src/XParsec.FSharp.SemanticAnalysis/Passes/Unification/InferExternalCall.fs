namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationSubsume
open UnificationEngine
open UnificationTranslate
open UnificationInferGeneralize
open UnificationInferLiterals
open UnificationInferResolve
open UnificationInferPat
open UnificationInferOverload
open UnificationInferForwardSchemes
open UnificationInferDispatch
open UnificationInferRecordAccess

module internal UnificationInferExternalCall =

    /// `ValueNone` when the bound doesn't evaluate to a ground union of string literals.
    let private boundLiteralStrings (ctx: PassContext) (bound: SemType) : Set<string> voption =
        match tryLiteralStrings ctx.Store (evalTypeLevel ctx bound) with
        | ValueSome strings -> ValueSome(Set.ofList strings)
        | ValueNone -> ValueNone

    /// Per-tuple-position syntactic string constants of an external call's argument
    /// expression: `ValueSome s` where that position is a plain string literal.
    type private ConstArgFacts = string voption[]

    let private constArgFacts (ctx: PassContext) (argExpr: Expr<SyntaxToken>) : ConstArgFacts =
        match argExpr with
        | Expr.Tuple(exprs = xs) -> xs |> Seq.map (constStringArg ctx) |> Seq.toArray
        | single -> [| constStringArg ctx single |]

    /// Refine a tuple position of `argTy` to `TyLiteral` when its argument is a plain string
    /// literal and SOME candidate's parameter there is a method typar `<K extends keyof T>`
    /// whose `keyof T` admits the constant. `argTy` unchanged when no position qualifies.
    let private admitLiteralMethodTypars
        (ctx: PassContext)
        (candidates: ExternalMember[])
        (declArgs: SemType[])
        (facts: ConstArgFacts)
        (argTy: SemType)
        : SemType =
        let elemTys =
            match resolveStep ctx.Store argTy with
            | TyTuple ts -> EqArray.toArray ts
            | single -> [| single |]

        if facts.Length <> elemTys.Length then
            argTy // a shape we don't model (spread/rest), so leave the arg untouched
        else
            let refined = Array.copy elemTys
            let mutable changed = false

            for i in 0 .. facts.Length - 1 do
                match facts.[i] with
                | ValueSome s ->
                    let admits =
                        candidates
                        |> Array.exists (fun m ->
                            match List.tryItem i (memberParamTypes ctx.Store declArgs m) with
                            | Some(TyTypar(TyparAxis.Method, j)) ->
                                match ExternalSymbols.instantiateSignatureBounds m declArgs |> EqArray.tryItem j with
                                | ValueSome(ValueSome bound) ->
                                    match boundLiteralStrings ctx bound with
                                    | ValueSome set -> Set.contains s set
                                    | ValueNone -> false
                                | _ -> false
                            | _ -> false
                        )

                    if admits then
                        refined.[i] <- TyLiteral(LiteralConst.String s)
                        changed <- true
                | ValueNone -> ()

            if not changed then argTy
            elif refined.Length = 1 then refined.[0]
            else TyTuple(EqArray.ofSeq refined)

    /// At any structural depth of `t`, not just its outermost type constructor.
    let private referencedMethodTypars (store: TypeStore) (t: SemType) : Set<int> =
        let mutable acc = Set.empty

        let rec walk t =
            match resolveStep store t with
            | TyTypar(TyparAxis.Method, j) -> acc <- Set.add j acc
            | t -> SemType.iterChildren walk t

        walk t
        acc

    /// Pre-bind a method typar to a `TyLiteral` from a syntactic string constant when the
    /// typar occurs ONLY inside a conditional parameter type (`k: undefined extends E[K] ? K
    /// : never`), where unification can't solve it. A bare parameter position is left unseeded.
    let private methodTyparConstantSeed
        (ctx: PassContext)
        (chosen: ExternalMember)
        (declArgs: SemType[])
        (facts: ConstArgFacts)
        : (int * SemType) list =
        let paramTys = memberParamTypes ctx.Store declArgs chosen |> List.toArray

        if facts.Length <> paramTys.Length then
            []
        else
            let bareTypars =
                paramTys
                |> Array.choose (
                    function
                    | TyTypar(TyparAxis.Method, j) -> Some j
                    | _ -> None
                )
                |> Set.ofArray

            let bounds = ExternalSymbols.instantiateSignatureBounds chosen declArgs
            let seed = System.Collections.Generic.Dictionary<int, SemType>()

            for i in 0 .. facts.Length - 1 do
                match facts.[i] with
                | ValueSome s ->
                    for j in referencedMethodTypars ctx.Store paramTys.[i] do
                        if not (Set.contains j bareTypars) && not (seed.ContainsKey j) then
                            match bounds |> EqArray.tryItem j with
                            | ValueSome(ValueSome bound) ->
                                match boundLiteralStrings ctx bound with
                                | ValueSome set when Set.contains s set -> seed.[j] <- TyLiteral(LiteralConst.String s)
                                | _ -> ()
                            | _ -> ()
                | ValueNone -> ()

            [ for kv in seed -> kv.Key, kv.Value ]

    /// The object-argument type + member name of an instance call. `w.Write(arg)` parses with
    /// `fn = LongIdent [w; Write]` (the parser folds the dot after a plain identifier), so the
    /// anchor must be a local BINDING, leaving `TextWriter.Synchronized` to the static probe.
    let private objArgMemberOf
        (infer: Infer)
        (ctx: PassContext)
        (fn: Expr<SyntaxToken>)
        : struct (SemType * string) voption =
        match fn with
        | Expr.DotLookup(expr = objArg; longIdentOrOp = LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
            ValueSome(struct (infer ctx objArg, ctx.NameOf li.Idents.[0]))
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
            li.Idents.Length >= 2
            && ctx.Bindings.Binding.ContainsKey(NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent)
            ->
            let objArgTy = inferLongIdentPrefix ctx (CstKeys.siteOfExpr fn) li
            ValueSome(struct (objArgTy, ctx.NameOf li.Idents.[li.Idents.Length - 1]))
        | _ -> ValueNone

    /// Commit an applied `arg -> result` chain against a resolved member signature: each
    /// argument position coerces via `unifyArg`, the residual result unifies exactly.
    let rec private commitAppliedCoerce
        (ctx: PassContext)
        (tok: SyntaxToken)
        (actual: SemType)
        (expected: SemType)
        : unit =
        match resolveStep ctx.Store actual, resolveStep ctx.Store expected with
        | TyFun(ad, ar), TyFun(ed, er) ->
            unifyArg ctx tok ad ed
            commitAppliedCoerce ctx tok ar er
        | a, b -> unify ctx tok a b

    /// Commit a call-site-resolved external overload (static or instance): record the chosen
    /// `SymbolKey` in `ExternalAccess` keyed on the member node, freshen the member's
    /// method-owned typars (`Take<TSource>`), then unify the signature against `argTy -> result`.
    let rec commitExternalOverload
        (ctx: PassContext)
        (tok: SyntaxToken)
        (fn: Expr<SyntaxToken>)
        (chosen: ExternalMember)
        (declArgs: SemType[])
        (facts: ConstArgFacts)
        (argTy: SemType)
        : SemType =
        let fnKey = CstKeys.ofExpr fn

        let memberSig =
            ExternalSymbols.instantiateSignatureWith
                ctx.Store
                (methodTyparConstantSeed ctx chosen declArgs facts)
                chosen
                declArgs
                ctx.CurrentLevel

        ctx.Resolution.ExternalAccess.Set(fnKey, ResolvedExternalMember.OfMember(chosen, memberSig))

        ctx.Store.SetLink(UnionFind.find ctx.Store (freshTv ctx fnKey), ValueSome memberSig)
        let resultTy = TyVar(freshTyVar ctx)

        // An `obj` parameter absorbs a typar / value-type argument via the implicit box
        // without grounding the typar; a base / interface parameter accepts the concrete
        // subtype (`CultureInfo` into an `IFormatProvider` slot).
        commitAppliedCoerce ctx tok (TyFun(argTy, resultTy)) memberSig
        resultTy

    /// Application-site overload resolution for a static external method call
    /// (`String.Concat("a", "b")`). Declines unless the name has >1 mapped overload.
    and tryInferExternalStaticMethodCall
        (infer: Infer)
        (ctx: PassContext)
        (tok: SyntaxToken)
        (fn: Expr<SyntaxToken>)
        (argExpr: Expr<SyntaxToken>)
        : SemType voption =
        match tryResolveExternalStaticMemberRef ctx fn with
        | ValueNone -> ValueNone
        | ValueSome(declTypeKey, memberTok) ->
            let memberName = ctx.NameOf memberTok
            let candidates = ctx.Provider.TryLookupMembers(declTypeKey, memberName)

            // A folded LongIdent denotes a non-generic type (generics need `<>`), so
            // the declaring type has no type arguments to instantiate.
            let typeArgs: SemType[] = [||]

            if candidates.Length <= 1 then
                // 0 / 1 candidate: defer to the eager single-pick path unchanged.
                ValueNone
            else
                let argTy = infer ctx argExpr
                // With no declaring type arguments no candidate parameter can be a
                // keyof-bounded method typar, so the constants seed the commit only.
                let facts = constArgFacts ctx argExpr

                match pickBestOverload ctx typeArgs candidates (argElemsOf ctx.Store argTy) with
                | ValueSome chosen -> ValueSome(commitExternalOverload ctx tok fn chosen typeArgs facts argTy)
                | ValueNone ->
                    ValueSome(
                        errorTy
                            ctx
                            tok
                            (Kind.Message(
                                sprintf
                                    "No applicable (or no unique best) overload of '%s' on type '%s' for the given arguments"
                                    memberName
                                    (SymbolKeyOps.typeMetaName declTypeKey)
                            ))
                    )

    /// Call-site overload resolution for an external *instance* method call (`sb.Append("x")`),
    /// keyed off a call whose object argument infers to a ground external `TyClass`. Needed because
    /// the single-pick path takes an arbitrary overload: `Append(char[], int, int)` for one `string`.
    and tryInferExternalInstanceMethodCall
        (infer: Infer)
        (ctx: PassContext)
        (tok: SyntaxToken)
        (fn: Expr<SyntaxToken>)
        (argExpr: Expr<SyntaxToken>)
        : SemType voption =
        // Declines on a non-external object argument, a 0/1-overload member, or no unique best.
        let resolveOn (objArgTy: SemType) (memberName: string) : SemType voption =
            // TODO(perf): `infer` is not memoised, so on the *decline* path the object argument
            // is inferred here and then again by the fallback's `infer ctx fn`. Thread its
            // `SemType` out of the probe if a fluent chain shows it up.
            let onSurface (struct (declKey, typeArgs)) =
                match
                    ctx.Provider.TryLookupMembers(declKey, memberName)
                    |> EqArray.filter (fun m -> not m.IsStatic)
                with
                | EqEmpty -> None
                | candidates -> Some(struct (typeArgs, candidates))

            match externalSurfaceKeys ctx objArgTy |> List.tryPick onSurface with
            | None -> ValueNone
            | Some(typeArgs, candidates) ->
                if candidates.Length <= 1 then
                    // 0 / 1 instance overload: the single-pick path is unambiguous.
                    ValueNone
                else
                    let declArgs = EqArray.toArray typeArgs
                    let facts = constArgFacts ctx argExpr
                    // Refine BEFORE the pick, so a string constant both selects the
                    // keyof-bounded typar overload and solves its freshened typar at commit.
                    let argTy =
                        admitLiteralMethodTypars ctx (EqArray.toArray candidates) declArgs facts (infer ctx argExpr)

                    match pickBestOverload ctx declArgs candidates (argElemsOf ctx.Store argTy) with
                    | ValueSome chosen -> ValueSome(commitExternalOverload ctx tok fn chosen declArgs facts argTy)
                    // No unique best: decline rather than error, leaving the single-pick path.
                    | ValueNone -> ValueNone

        match objArgMemberOf infer ctx fn with
        | ValueSome(struct (objArgTy, memberName)) -> resolveOn objArgTy memberName
        | ValueNone -> ValueNone

    /// Call-site overload resolution for a project-LOCAL instance method call (`p.Show(1)`),
    /// when the object argument is a local class / union / record whose member name has >1 candidate.
    /// A winner's frozen `SymbolKey` is recorded so Elaborate resolves it by identity.
    and tryInferLocalInstanceMethodCall
        (infer: Infer)
        (ctx: PassContext)
        (node: NodeSite)
        (fn: Expr<SyntaxToken>)
        (argExpr: Expr<SyntaxToken>)
        : SemType voption =
        // The object argument's own declaration and the type args it is instantiated at;
        // `ValueNone` for anything but a project-local class / union / record.
        let localHost (objArgTy: SemType) : struct (TypeRegistry.NominalDecl * EqArray<SemType>) voption =
            match resolveStep ctx.Store objArgTy with
            | TyNominal(hostKey, args) ->
                TypeRegistry.tryNominalByKey ctx.Types hostKey
                |> ValueOption.map (fun decl -> struct (decl, args))
            | _ -> ValueNone

        let resolveOn (objArgTy: SemType) (memberName: string) : SemType voption =
            match localHost objArgTy with
            | ValueNone -> ValueNone
            | ValueSome(struct (decl, args)) ->
                let declKey = decl.TypeKey
                let typeParams = decl.TypeParams
                let members = decl.Members
                let argTy = infer ctx argExpr
                let argElems = argElemsOf ctx.Store argTy

                match resolveMember ctx typeParams args members memberName false argElems with
                | MemberPick.NotOverloaded -> ValueNone
                | MemberPick.NoneApplicable ->
                    ValueSome(
                        errorTy
                            ctx
                            node.Tok
                            (Kind.Message(
                                sprintf
                                    "No overload for method '%s' takes the given arguments (%s)"
                                    memberName
                                    (showParams ctx argElems)
                            ))
                    )
                | MemberPick.Ambiguous cands ->
                    let candidates =
                        cands
                        |> List.map (fun m ->
                            sprintf "%s(%s)" memberName (showParams ctx (userMemberParams ctx typeParams args m))
                        )
                        |> String.concat "; "

                    ValueSome(
                        errorTy
                            ctx
                            node.Tok
                            (Kind.Message(
                                sprintf "Ambiguous call to overloaded method '%s'; candidates: %s" memberName candidates
                            ))
                    )
                | MemberPick.Resolved chosen ->
                    // Commit: unify the applied `argTy -> resultTy` against the chosen
                    // member's instantiated function type (domains coerce, the result unifies).
                    let memberFunTy =
                        instantiateMemberCall ctx (typeParams, args) chosen.EffectiveMethodTypars chosen.Type

                    let resultTy = TyVar(freshTyVar ctx)
                    commitAppliedCoerce ctx node.Tok (TyFun(argTy, resultTy)) memberFunTy

                    // `localHost` looks at the object argument's OWN declaration, never up an
                    // `inherit` chain, so the declaring type is the object argument's own.
                    ctx.Resolution.LocalMemberCall.Set(
                        node.Key,
                        {
                            Key = frozenUserMemberKey ctx.Store declKey typeParams chosen
                            DeclaringTy = resolveStep ctx.Store objArgTy
                        }
                    )

                    ValueSome resultTy

        match objArgMemberOf infer ctx fn with
        | ValueSome(struct (objArgTy, memberName)) -> resolveOn objArgTy memberName
        | ValueNone -> ValueNone

    /// Permit an external method call that omits a suffix of the member's *trailing optional*
    /// parameters (`ArrayPool<'T>.Return(arr)` for `Return(arr, [<Optional>] clearArray = false)`):
    /// without it the application loop unifies one argument against the full tupled domain.
    and tryFillOptionalCall
        (ctx: PassContext)
        (tok: SyntaxToken)
        (fn: Expr<SyntaxToken>)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        (argTys: SemType[])
        : SemType voption =
        // Only a single .NET-tupled argument list can carry omitted optionals; a
        // genuinely curried application isn't a .NET method-call shape.
        if args.Length <> 1 then
            ValueNone
        else
            let fnKey = CstKeys.ofExpr fn

            match ctx.Resolution.ExternalAccess.TryGetValue fnKey with
            | ValueSome info when not info.IsValueMember && not (List.isEmpty info.OptionalDefaults) ->
                match info.Key with
                | SymbolKey.Member mk ->
                    let argSig = mk.ArgSig
                    let optDefaults = info.OptionalDefaults
                    let fullCount = argSig.Length
                    let requiredCount = fullCount - List.length optDefaults
                    let argTy = argTys.[0]
                    let suppliedCount = argArityOf ctx.Store argTy

                    // Fire only for a *partial* omission: a fully applied call
                    // (or one below the required minimum) is left to the normal path.
                    if suppliedCount < requiredCount || suppliedCount >= fullCount then
                        ValueNone
                    else
                        match resolveStep ctx.Store info.Signature with
                        | TyFun(fullParams, ret) ->
                            let leading =
                                match resolveStep ctx.Store fullParams with
                                | TyTuple elems -> elems |> EqArray.truncate suppliedCount |> EqArray.toList
                                | single -> [ single ]

                            let resultTy = TyVar(freshTyVar ctx)

                            unifyAppliedSig
                                ctx
                                tok
                                (TyFun(argTy, resultTy))
                                (TyFun(tupleOrSingle ctx.Intrinsics leading, ret))
                            // The omitted defaults are the last `fullCount - suppliedCount`
                            // of the optional suffix; Elaborate appends them.
                            let omitted = optDefaults |> List.skip (suppliedCount - requiredCount)
                            ctx.Resolution.ExternalOptionalFill.Set(fnKey, omitted)
                            ValueSome resultTy
                        | _ -> ValueNone
                | _ -> ValueNone
            | _ -> ValueNone
