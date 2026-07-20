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

    /// The set of string literals a realised keyof-bounded method typar admits (its
    /// `keyof`-fold), or `ValueNone` when the bound isn't a ground literal (union).
    let private boundLiteralStrings (ctx: PassContext) (bound: SemType) : Set<string> voption =
        match tryLiteralStrings ctx.Store (evalTypeLevel ctx bound) with
        | ValueSome strings -> ValueSome(Set.ofList strings)
        | ValueNone -> ValueNone

    /// Per-tuple-position syntactic string constants of an external call's argument
    /// expression (`ValueSome s` where the position is a plain string literal, else
    /// `ValueNone`), aligned to tuple positions. Computed ONCE per external call
    /// (`constArgFacts`) and threaded to both the pick refinement
    /// (`admitLiteralMethodTypars`) and the commit seed (`methodTyparConstantSeed`) so
    /// the two seams cannot derive divergent constant facts through `constStringArg`.
    type private ConstArgFacts = string voption[]

    let private constArgFacts (ctx: PassContext) (argExpr: Expr<SyntaxToken>) : ConstArgFacts =
        match argExpr with
        | Expr.Tuple(exprs = xs) -> xs |> Seq.map (constStringArg ctx) |> Seq.toArray
        | single -> [| constStringArg ctx single |]

    /// R4a step 3 item 2 — DIRECTIONAL constant admission of a syntactic string constant
    /// into a keyof-bounded METHOD TYPAR at an external instance-method call. Refines
    /// `argTy` so a tuple position whose argument is a plain string literal AND whose
    /// parameter (in SOME candidate overload) is a method typar `<Key extends keyof T>`
    /// with `T` ground and the constant among `keyof T` becomes `TyLiteral`. The literal
    /// then (a) selects the typar overload over a rival literal-`'*'` overload and (b)
    /// solves the freshened `Key` var at the `commitExternalOverload` seam, which grounds
    /// the `Events[Key]` handler/payload folds. Nominalism invariant: the literal enters
    /// via the external typar; the Vesper `"ping"` expression still types as `string`.
    /// Returns `argTy` unchanged when no position qualifies (a non-constant / non-key
    /// argument falls back to the documented precision limit).
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
            argTy // a shape we don't model (spread/rest) — leave the arg untouched
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
                                match ExternalSymbols.instantiateSignatureBounds m declArgs |> Array.tryItem j with
                                | Some(ValueSome bound) ->
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

    /// Method typars of `chosen` referenced in `t` (any structural depth) — the axis a
    /// carried node's grounding must seed.
    let private referencedMethodTypars (store: TypeStore) (t: SemType) : Set<int> =
        let mutable acc = Set.empty

        let rec walk t =
            match resolveStep store t with
            | TyTypar(TyparAxis.Method, j) -> acc <- Set.add j acc
            | t -> SemType.iterChildren walk t

        walk t
        acc

    /// Pre-bind a method typar to a `TyLiteral` when a syntactic string constant grounds it
    /// but the typar appears ONLY inside a non-bare parameter position (mitt's no-payload
    /// `emit(type: undefined extends Events[Key] ? Key : never)` — `Key` is never a bare
    /// param, so plain unification cannot solve it from the constant; seeding it lets the
    /// conditional fold). A typar at a BARE position is left unseeded (unification solves it
    /// there, keeping the keyed `emit`/`on`/`off` path byte-identical). R4a step 3, the
    /// conditional-overload extension of `admitLiteralMethodTypars`.
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
                            match bounds |> Array.tryItem j with
                            | Some(ValueSome bound) ->
                                match boundLiteralStrings ctx bound with
                                | ValueSome set when Set.contains s set -> seed.[j] <- TyLiteral(LiteralConst.String s)
                                | _ -> ()
                            | _ -> ()
                | ValueNone -> ()

            [ for kv in seed -> kv.Key, kv.Value ]

    /// The receiver type + member name of an instance-call head, or `ValueNone` for a shape
    /// that isn't one. The SINGLE owner of "which `fn` shapes are instance calls" — both the
    /// external and project-local instance probes route their `fn` dispatch through it, so the
    /// head grammar (and the folded-LongIdent local-binding guard) lives in one place. The
    /// receiver is inferred only on a matching arm, so the decline path pays nothing.
    ///
    /// Folded-LongIdent value receiver: `w.Write(arg)` parses with `fn = LongIdent [w; Write]`
    /// — the parser folds the dot into the long ident when the head is a plain identifier, so
    /// it never reaches the `DotLookup` arm and falls to the single-pick field walk (which
    /// grabs an arbitrary, here the widest, overload). The head must be a *local binding* (a
    /// value); a type-qualified head (`TextWriter.Synchronized`) is the static probe's job and
    /// is excluded by the binding guard. The receiver is the chain minus its last segment; the
    /// member is the last segment.
    let private receiverMemberOf
        (infer: Infer)
        (ctx: PassContext)
        (fn: Expr<SyntaxToken>)
        : struct (SemType * string) voption =
        match fn with
        | Expr.DotLookup(expr = recv; longIdentOrOp = LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
            ValueSome(struct (infer ctx recv, ctx.NameOf li.Idents.[0]))
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
            li.Idents.Length >= 2
            && ctx.Bindings.Binding.ContainsKey(NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent)
            ->
            let recvTy = inferLongIdentReceiverPrefix ctx (CstKeys.ofExpr fn) li
            ValueSome(struct (recvTy, ctx.NameOf li.Idents.[li.Idents.Length - 1]))
        | _ -> ValueNone

    /// Commit an applied `arg -> result` shape against a resolved member signature: coerce
    /// each argument position via `unifyArg`/`tryCoerceUpcast` — the richer coercion the
    /// picker's subsumption tier admits (a superset of `unifyArgCoerce`'s obj/union
    /// absorption, so the commit accepts exactly what filtering did, base/interface arguments
    /// included) — and unify the residual result exactly. The one overload-commit spine walk,
    /// shared by the external and project-local overload paths so they cannot drift.
    let rec private commitAppliedCoerce (ctx: PassContext) (key: NodeKey) (actual: SemType) (expected: SemType) : unit =
        match resolveStep ctx.Store actual, resolveStep ctx.Store expected with
        | TyFun(ad, ar), TyFun(ed, er) ->
            unifyArg ctx key ad ed
            commitAppliedCoerce ctx key ar er
        | a, b -> unify ctx key a b

    /// Commit a call-site-resolved external overload (static or instance): record
    /// the chosen `SymbolKey` to `ExternalAccess` keyed on the member node where
    /// Elaborate reads it, freshen the member's method-owned typars (`Take<TSource>`)
    /// via `ExternalSymbols.instantiateSignature` so the argument types drive their
    /// solution (a non-generic overload is unchanged), unify the signature against
    /// `argTy -> result`, and return the result type. Shared by the static and
    /// instance probes so the two cannot drift; `chosen.IsStatic` is authoritative
    /// for both (the instance probe pre-filters to non-static candidates). `facts`
    /// supplies the per-position syntactic constants that seed a conditional-only
    /// method typar (`methodTyparConstantSeed`).
    let rec commitExternalOverload
        (ctx: PassContext)
        (key: NodeKey)
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

        ctx.Resolution.ExternalAccess.Set(
            fnKey,
            {
                Key = SymbolKey.Member chosen.Key
                IsStatic = chosen.IsStatic
                Storage = chosen.Storage
                Signature = memberSig
                OptionalDefaults = chosen.OptionalDefaults
            }
        )

        ctx.Store.SetLink(UnionFind.find ctx.Store (freshTv ctx fnKey), ValueSome memberSig)
        let resultTy = TyVar(freshTyVar ctx)

        // The applied `arg -> result` spine coerces against the member signature: an `obj`
        // parameter absorbs a typar / value-type argument via the implicit box without
        // grounding the typar, and a base / interface parameter accepts the concrete subtype
        // argument the subsumption tier admitted (`CultureInfo` into an `IFormatProvider`
        // slot), its witnessed type args unified.
        commitAppliedCoerce ctx key (TyFun(argTy, resultTy)) memberSig
        resultTy

    /// Application-site overload resolution for a static external method call
    /// (`String.Concat("a", "b")`). Fires only when the member name has >1 mapped
    /// overload — single-candidate access keeps the existing single-pick path, so
    /// behaviour is unchanged everywhere it already worked. The commit (access
    /// record + method-typar freshening + unification) is shared with the instance
    /// probe via `commitExternalOverload`.
    and tryInferExternalStaticMethodCall
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
        (fn: Expr<SyntaxToken>)
        (argExpr: Expr<SyntaxToken>)
        : SemType voption =
        match tryResolveExternalStaticMemberRef ctx fn with
        | ValueNone -> ValueNone
        | ValueSome(declTypeKey, memberTok) ->
            let memberName = ctx.NameOf memberTok
            let candidates = ctx.Provider.TryLookupMembers(declTypeKey, memberName)

            // A folded LongIdent names a non-generic type (generics need `<>`), so
            // the declaring type has no type arguments to instantiate.
            let typeArgs: SemType[] = [||]

            if candidates.Length <= 1 then
                // 0 / 1 candidate: defer to the eager single-pick path unchanged.
                ValueNone
            else
                let argTy = infer ctx argExpr
                // The static probe seeds conditional-only method typars at commit but,
                // unlike the instance probe, does NOT pre-refine literals before the
                // pick: a folded type-qualified LongIdent names a non-generic type
                // (`declArgs = [||]`), so no candidate parameter is a keyof-bounded
                // method typar `<Key extends keyof T>` — the exact shape
                // `admitLiteralMethodTypars` selects on. The keyof-bounded-typar
                // selection the instance `on`/`off`/`emit` path needs cannot arise
                // here, so refinement is deliberately scoped out (commit-seed only).
                let facts = constArgFacts ctx argExpr

                match pickBestOverload ctx typeArgs candidates (argElemsOf ctx.Store argTy) with
                | ValueSome chosen -> ValueSome(commitExternalOverload ctx key fn chosen typeArgs facts argTy)
                | ValueNone ->
                    ValueSome(
                        errorTy
                            ctx
                            key
                            (sprintf
                                "No applicable (or no unique best) overload of '%s' on type '%s' for the given arguments"
                                memberName
                                (SymbolKeyOps.qualifiedName declTypeKey))
                    )

    /// Call-site overload resolution for an external *instance* method call
    /// (`sb.Append("x")`, `recv.M(args)`). The instance sibling of
    /// `tryInferExternalStaticMethodCall`: where the static probe keys off a folded
    /// type-qualified LongIdent, this one keys off a single-ident `DotLookup` whose
    /// receiver `infer`s to a *ground external* `TyClass`. Fires only when the member
    /// name has >1 instance overload — otherwise the single-pick `resolveFieldStep`
    /// path (reached via the generic fallback's `infer ctx fn`) is already correct,
    /// so this declines and behaviour is unchanged. The reason it must exist:
    /// `resolveFieldStep` resolves `.Member` through `TryLookupMember` (singular),
    /// which grabs an *arbitrary* overload without consulting the argument types —
    /// harmless while the receiver is a deferred TyVar (the dot-access parks and the
    /// chain stays generic), but once Gap A grounds the receiver eagerly that picks
    /// e.g. `Append(char[], int, int)` for a single `string` arg
    /// (`string vs TyTuple`). Resolving by the call-site argument types here makes
    /// the grounded pick match the overload a correct call intends.
    /// Declines (so the old path runs) on any shape it can't confidently resolve,
    /// so it never *introduces* an error.
    and tryInferExternalInstanceMethodCall
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
        (fn: Expr<SyntaxToken>)
        (argExpr: Expr<SyntaxToken>)
        : SemType voption =
        // Given the receiver's inferred type + the member name, resolve the call
        // against *all* instance overloads by the argument types. Declines (so the
        // single-pick path runs unchanged) on a non-external receiver, a 0/1-overload
        // member, or when no unique best matches — so it only ever *improves* a
        // confident pick. Shared by the `DotLookup` and folded-`LongIdent` heads.
        let resolveOn (recvTy: SemType) (memberName: string) : SemType voption =
            // TODO(perf): `infer` is not memoised, so on the *decline* path the
            // receiver is inferred here and then again by the fallback's
            // `infer ctx fn`. If a fluent chain shows it up, thread the receiver
            // `SemType` out of the probe instead of re-inferring.
            //
            // Resolve the receiver to an external `(SymbolKey, typeArgs)` — a
            // non-project-local `TyClass` or an intrinsic `TyConst` mapped to a BCL
            // type (`tryExternalReceiver`). A project-local class / array / byref
            // declines and keeps its own path.
            match tryExternalReceiver ctx recvTy with
            | ValueNone -> ValueNone
            | ValueSome(declKey, typeArgs) ->
                let candidates =
                    ctx.Provider.TryLookupMembers(declKey, memberName)
                    |> Array.filter (fun m -> not m.IsStatic)

                if candidates.Length <= 1 then
                    // 0 / 1 instance overload: the single-pick path is unambiguous.
                    ValueNone
                else
                    let declArgs = EqArray.toArray typeArgs
                    // Constant facts computed once, shared by the pick refinement and
                    // the commit seed (so the two cannot derive divergent facts).
                    let facts = constArgFacts ctx argExpr
                    // Refine a syntactic-string-constant position that lands on a keyof-
                    // bounded method typar to a `TyLiteral` (R4a step 3 item 2) BEFORE the
                    // pick, so the literal both selects the typar overload and solves the
                    // freshened `Key` at commit.
                    let argTy =
                        admitLiteralMethodTypars ctx candidates declArgs facts (infer ctx argExpr)

                    match pickBestOverload ctx declArgs candidates (argElemsOf ctx.Store argTy) with
                    | ValueSome chosen -> ValueSome(commitExternalOverload ctx key fn chosen declArgs facts argTy)
                    // No unique best on the argument types: decline rather than
                    // error, so the existing single-pick path keeps the prior
                    // behaviour (this probe only ever *improves* a confident pick).
                    | ValueNone -> ValueNone

        match receiverMemberOf infer ctx fn with
        | ValueSome(struct (recvTy, memberName)) -> resolveOn recvTy memberName
        | ValueNone -> ValueNone

    /// Call-site overload resolution for a project-LOCAL instance method call
    /// (`p.Show(1)`, `r.M(a, b)`). The user-declared twin of
    /// `tryInferExternalInstanceMethodCall`: where that probe keys off an external
    /// receiver, this one fires when the receiver is a project-local class / union / record
    /// whose member name has >1 instance candidate. It is the ONE place a user-member
    /// overload set is arg-resolved — the dot-access member-TYPE sites have no arguments, so
    /// they keep first-match (correct for the non-overloaded names those sites ever reach,
    /// since this probe intercepts every overloaded call before them).
    ///
    /// On a unique winner it commits (unifies the applied arguments against the chosen
    /// member's instantiated arrow) AND records the chosen member's TOTAL frozen `MemberKey`
    /// on the call node so Elaborate/Freeze resolves the identical overload by identity,
    /// never a second name-based pick. `NoneApplicable` / `Ambiguous` raise the two distinct
    /// call-site diagnostics; a non-overloaded name (`NotOverloaded`) declines so the
    /// single-pick `resolveFieldStep` path runs unchanged.
    and tryInferLocalInstanceMethodCall
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
        (fn: Expr<SyntaxToken>)
        (argExpr: Expr<SyntaxToken>)
        : SemType voption =
        // The receiver's declaring nominal, as `(declKey, typeParams, args, members)` — a
        // project-local class / union / record. `ValueNone` for any other receiver (an
        // external nominal, a typar, a primitive), which declines to the existing path.
        let localHost
            (recvTy: SemType)
            : struct (TypeKey * EqArray<string * TyVarId> * EqArray<SemType> * TypeMemberInfo[]) voption =
            match resolveStep ctx.Store recvTy with
            | TyClass(key, args) ->
                match TypeRegistry.tryClassByKey ctx.Types key with
                | ValueSome info -> ValueSome(struct (info.TypeKey, info.TypeParams, args, info.Members))
                | ValueNone -> ValueNone
            | TyUnion(key, args) ->
                match TypeRegistry.tryUnionByKey ctx.Types key with
                | ValueSome info -> ValueSome(struct (info.TypeKey, info.TypeParams, args, info.Members))
                | ValueNone -> ValueNone
            | TyRecord(key, args) ->
                match TypeRegistry.tryRecordByKey ctx.Types key with
                | ValueSome info -> ValueSome(struct (info.TypeKey, info.TypeParams, args, info.Members))
                | ValueNone -> ValueNone
            | _ -> ValueNone

        // A short parameter-shape rendering for the ambiguity diagnostic — the nominal head
        // simple name (`int`, `IA`), or `_` for a still-open position.
        let describeParams (ps: SemType list) : string =
            let one (t: SemType) =
                let keyOpt =
                    match zonk ctx.Store t with
                    | TyConst(k, _) -> ValueSome k
                    | TyClass(k, _)
                    | TyRecord(k, _)
                    | TyUnion(k, _) -> ValueSome(SymbolKey.Type k)
                    | _ -> ValueNone

                match keyOpt with
                | ValueSome k ->
                    let (DisplayName n) = SymbolKeyOps.simpleName k
                    n
                | ValueNone -> "_"

            ps |> List.map one |> String.concat ", "

        let resolveOn (recvTy: SemType) (memberName: string) : SemType voption =
            match localHost recvTy with
            | ValueNone -> ValueNone
            | ValueSome(struct (declKey, typeParams, args, members)) ->
                let argTy = infer ctx argExpr
                let argElems = argElemsOf ctx.Store argTy

                match resolveMember ctx typeParams args members memberName false argElems with
                | MemberPick.NotOverloaded -> ValueNone
                | MemberPick.NoneApplicable ->
                    ValueSome(
                        errorTy
                            ctx
                            key
                            (sprintf
                                "No overload for method '%s' takes the given arguments (%s)"
                                memberName
                                (describeParams argElems))
                    )
                | MemberPick.Ambiguous cands ->
                    let shown =
                        cands
                        |> List.map (fun m ->
                            sprintf "%s(%s)" memberName (describeParams (userMemberParams ctx typeParams args m))
                        )
                        |> String.concat "; "

                    ValueSome(
                        errorTy
                            ctx
                            key
                            (sprintf "Ambiguous call to overloaded method '%s'; candidates: %s" memberName shown)
                    )
                | MemberPick.Resolved chosen ->
                    // Commit: unify the applied `argTy -> resultTy` against the chosen
                    // member's instantiated arrow (domains coerce, the residual result
                    // unifies), exactly as the external overload commit does.
                    let memberArrow =
                        instantiateMemberCall ctx (typeParams, args) chosen.EffectiveMethodTypars chosen.Type

                    let resultTy = TyVar(freshTyVar ctx)
                    commitAppliedCoerce ctx key (TyFun(argTy, resultTy)) memberArrow

                    // The inference→Freeze handshake: record the chosen overload's TOTAL
                    // frozen `MemberKey` so Elaborate stamps the identical identity with no
                    // second pick.
                    ctx.Resolution.LocalMemberCall.Set(key, frozenUserMemberKey ctx.Store declKey typeParams chosen)

                    ValueSome resultTy

        match receiverMemberOf infer ctx fn with
        | ValueSome(struct (recvTy, memberName)) -> resolveOn recvTy memberName
        | ValueNone -> ValueNone

    /// Permit an external method call that omits a suffix of the member's *trailing
    /// optional* parameters (`ArrayPool<'T>.Return(arr)` for `Return(arr, [<Optional>]
    /// clearArray = false)`). Runs as the last fallback in `inferApp` (after `fn` is
    /// already inferred, so `resolveFieldStep` has recorded the member in
    /// `ExternalAccess`): without it, the generic application loop would unify the
    /// single supplied argument against the full tupled parameter domain and report a
    /// spurious arity mismatch. When the supplied arity sits between the member's
    /// required and full parameter counts, this unifies the supplied arguments against
    /// only the *leading* parameters and records the omitted constant defaults in
    /// `ExternalOptionalFill` for Elaborate to synthesise — leaving the head's own type
    /// (and so the member-ref the backend recovers) at the full signature. Declines
    /// (so the ordinary path runs, unchanged) on every other shape, so it can only
    /// *admit* a call the old path rejected.
    and tryFillOptionalCall
        (ctx: PassContext)
        (key: NodeKey)
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

            // The member's optional defaults were carried forward onto the resolved
            // record when `fn` was inferred (`ResolvedExternalMember.OptionalDefaults`),
            // so there is no provider re-query here.
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
                            unifyAppliedSig ctx key (TyFun(argTy, resultTy)) (TyFun(tupleOrSingle ctx leading, ret))
                            // The omitted defaults are the last `fullCount - suppliedCount`
                            // of the optional suffix; Elaborate appends them.
                            let omitted = optDefaults |> List.skip (suppliedCount - requiredCount)
                            ctx.Resolution.ExternalOptionalFill.Set(fnKey, omitted)
                            ValueSome resultTy
                        | _ -> ValueNone
                | _ -> ValueNone
            | _ -> ValueNone
