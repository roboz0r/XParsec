namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngine
open UnificationTranslate
open UnificationInferGeneralize
open UnificationInferLiterals
open UnificationInferResolve
open UnificationInferPat
open UnificationInferOverload
open UnificationInferForwardSchemes
open UnificationInferDispatch

module internal UnificationInferExternalCall =

    /// Commit a call-site-resolved external overload (static or instance): record
    /// the chosen `SymbolKey` to `ExternalAccess` keyed on the member node where
    /// Freeze reads it, freshen the member's method-owned typars (`Take<TSource>`)
    /// via `ExternalSymbols.instantiateSignature` so the argument types drive their
    /// solution (a non-generic overload is unchanged), unify the signature against
    /// `argTy -> result`, and return the result type. Shared by the static and
    /// instance probes so the two cannot drift; `chosen.IsStatic` is authoritative
    /// for both (the instance probe pre-filters to non-static candidates).
    let rec commitExternalOverload
        (ctx: PassContext)
        (key: NodeKey)
        (fn: Expr<SyntaxToken>)
        (chosen: ExternalMember)
        (declArgs: SemType[])
        (argTy: SemType)
        : SemType =
        let fnKey = CstKeys.ofExpr fn

        let memberSig =
            ExternalSymbols.instantiateSignature chosen declArgs ctx.CurrentLevel

        ctx.Resolution.ExternalAccess.Set(
            fnKey,
            {
                Key = chosen.Key
                IsStatic = chosen.IsStatic
                IsProperty = chosen.IsProperty
                Signature = memberSig
                OptionalDefaults = chosen.OptionalDefaults
            }
        )

        (freshTv ctx fnKey).Link <- ValueSome memberSig
        let resultTy = TyVar(freshTyVar ctx)
        // Coerce each argument position rather than unify the whole signature: an
        // `obj` parameter must absorb a typar / value-type argument via the implicit
        // box, not ground the typar. `unifyAppliedSig` walks the `actual` applied
        // shape (`arg -> result`) against the member signature.
        unifyAppliedSig ctx key (TyFun(argTy, resultTy)) memberSig
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
        | ValueSome(metaName, memberTok) ->
            let memberName = ctx.NameOf memberTok
            let candidates = ctx.Provider.TryLookupMembers(metaName, memberName)

            // A folded LongIdent names a non-generic type (generics need `<>`), so
            // the declaring type has no type arguments to instantiate.
            let typeArgs: SemType[] = [||]

            if candidates.Length <= 1 then
                // 0 / 1 candidate: defer to the eager single-pick path unchanged.
                ValueNone
            else
                let argTy = infer ctx argExpr

                match pickBestOverload typeArgs candidates (argElemsOf argTy) with
                | ValueSome chosen -> ValueSome(commitExternalOverload ctx key fn chosen typeArgs argTy)
                | ValueNone ->
                    ValueSome(
                        errorTy
                            ctx
                            key
                            (sprintf
                                "No applicable (or no unique best) overload of '%s' on type '%s' for the given arguments"
                                memberName
                                metaName)
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
    /// Infer-resolution-gaps-plan.md Gap C. Declines (so the old path runs) on any
    /// shape it can't confidently resolve, so it never *introduces* an error.
    and tryInferExternalInstanceMethodCall
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
        (fn: Expr<SyntaxToken>)
        (argExpr: Expr<SyntaxToken>)
        : SemType voption =
        match fn with
        | Expr.DotLookup(expr = recv; longIdentOrOp = LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
            // TODO(perf): `infer` is not memoised, so on the *decline* path the
            // receiver is inferred here and then again by the fallback's
            // `infer ctx fn` (which re-infers `recv`). On a fluent chain
            // (`sb.Append(..).Append(..)`) this re-inflates at every level. If it
            // shows up, thread the already-computed receiver `SemType` out of the
            // probe instead of re-inferring.
            //
            // Resolve the receiver to an external `(qualifiedName, typeArgs)` — a
            // non-project-local `TyClass` or an intrinsic `TyConst` mapped to a BCL
            // type (`tryExternalReceiver`). A project-local class / array / byref
            // declines and keeps its own path.
            match tryExternalReceiver ctx (infer ctx recv) with
            | ValueNone -> ValueNone
            | ValueSome(clsQual, typeArgs) ->
                let memberName = ctx.NameOf li.Idents.[0]

                let candidates =
                    ctx.Provider.TryLookupMembers(clsQual, memberName)
                    |> Array.filter (fun m -> not m.IsStatic)

                if candidates.Length <= 1 then
                    // 0 / 1 instance overload: the single-pick path is unambiguous.
                    ValueNone
                else
                    let declArgs = typeArgs |> EqArray.toList |> List.toArray
                    let argTy = infer ctx argExpr

                    match pickBestOverload declArgs candidates (argElemsOf argTy) with
                    | ValueSome chosen -> ValueSome(commitExternalOverload ctx key fn chosen declArgs argTy)
                    // No unique best on the argument types: decline rather than
                    // error, so the existing single-pick path keeps the prior
                    // behaviour (this probe only ever *improves* a confident pick).
                    | ValueNone -> ValueNone
        | _ -> ValueNone

    /// Permit an external method call that omits a suffix of the member's *trailing
    /// optional* parameters (`ArrayPool<'T>.Return(arr)` for `Return(arr, [<Optional>]
    /// clearArray = false)`). Runs as the last fallback in `inferApp` (after `fn` is
    /// already inferred, so `resolveFieldStep` has recorded the member in
    /// `ExternalAccess`): without it, the generic application loop would unify the
    /// single supplied argument against the full tupled parameter domain and report a
    /// spurious arity mismatch. When the supplied arity sits between the member's
    /// required and full parameter counts, this unifies the supplied arguments against
    /// only the *leading* parameters and records the omitted constant defaults in
    /// `ExternalOptionalFill` for Freeze to synthesise — leaving the head's own type
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
            | ValueSome info when not info.IsProperty && not (List.isEmpty info.OptionalDefaults) ->
                match info.Key with
                | SymbolKey.MemberKey(_, _, argSig, _) ->
                    let optDefaults = info.OptionalDefaults
                    let fullCount = argSig.Length
                    let requiredCount = fullCount - List.length optDefaults
                    let argTy = argTys.[0]
                    let suppliedCount = argArityOf argTy

                    // Fire only for a *partial* omission: a fully applied call
                    // (or one below the required minimum) is left to the normal path.
                    if suppliedCount < requiredCount || suppliedCount >= fullCount then
                        ValueNone
                    else
                        match resolveStep info.Signature with
                        | TyFun(fullParams, ret) ->
                            let leading =
                                match resolveStep fullParams with
                                | TyTuple elems -> elems |> EqArray.toList |> List.truncate suppliedCount
                                | single -> [ single ]

                            let resultTy = TyVar(freshTyVar ctx)
                            unifyAppliedSig ctx key (TyFun(argTy, resultTy)) (TyFun(tupleOrSingle leading, ret))
                            // The omitted defaults are the last `fullCount - suppliedCount`
                            // of the optional suffix; Freeze appends them.
                            let omitted = optDefaults |> List.skip (suppliedCount - requiredCount)
                            ctx.Resolution.ExternalOptionalFill.Set(fnKey, omitted)
                            ValueSome resultTy
                        | _ -> ValueNone
                | _ -> ValueNone
            | _ -> ValueNone
