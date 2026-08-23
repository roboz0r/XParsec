namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationEngine
open UnificationTranslate
open UnificationInferGeneralize
open UnificationInferLiterals
open UnificationInferResolve
open UnificationInferPat
open UnificationInferOverload
open UnificationInferForwardSchemes
open UnificationInferDispatch

module internal UnificationInferCtor =

    /// Overload-pick + unify a heritable primitive's CONTRACT `.ctor` set (the
    /// `IntrinsicClassSurface.Members` riding the provider shape) against `argExpr`. The chosen
    /// signature grounds the arguments; the result side stays a free var. Returns that `.ctor`.
    let inferIntrinsicClassCtorCall
        (infer: Infer)
        (ctx: PassContext)
        (typeArgs: SemType[])
        (surface: IntrinsicClassSurface)
        (noOverload: Kind)
        (argExpr: Expr<SyntaxToken>)
        : ExternalMember voption =
        let ctors = surface.Members |> EqArray.filter (fun m -> m.Name = ".ctor")
        let argTy = infer ctx argExpr

        match pickBestOverload ctx typeArgs ctors (argElemsOf ctx.Store argTy) with
        | ValueSome chosen ->
            let ctorSig = ExternalSymbols.openSignature chosen typeArgs
            let resultTy = TyVar(freshTyVar ctx)
            unify ctx (CstKeys.firstTokenOfExpr argExpr) ctorSig (TyFun(argTy, resultTy))
            ValueSome chosen
        | ValueNone ->
            ctx.Report(CstKeys.firstTokenOfExpr argExpr, noOverload)
            ValueNone

    /// `new T(args)` — unified as a single application against the ctor's signature.
    let rec inferNew
        (infer: Infer)
        (ctx: PassContext)
        (node: NodeSite)
        (t: Type<SyntaxToken>)
        (argExpr: Expr<SyntaxToken>)
        : SemType =
        let ctorTy = translateType ctx t

        // Provenance: `new T(…)` writes the constructed node's type explicitly.
        ctx.MarkTypeDeclared(node.Key, ctorTy)

        match resolveStep ctx.Store ctorTy with
        | TyClass(clsKey, args) ->
            match TypeRegistry.tryClassByKey ctx.Types clsKey with
            | ValueSome info ->
                let subst = mkNamedTypeSubst ctx.Store info.TypeParams args
                let argTy = infer ctx argExpr

                let expected =
                    match pickLocalCtor ctx (substituteWith ctx.Store subst) info (argElemsOf ctx.Store argTy) with
                    | ValueSome pick -> pick.Parameters
                    // No constructor of that arity: unify against the primary anyway, so the
                    // mismatch is reported at the arguments rather than passing silently.
                    | ValueNone -> [ for p in info.CtorParams -> substituteWith ctx.Store subst p.Type ]
                    |> tupleOrSingle ctx

                unifyArg ctx (CstKeys.firstTokenOfExpr argExpr) argTy expected
                ctorTy
            | ValueNone ->
                // Fall through to the external-class path: `new System.Exception(msg)`. The
                // symbol provider owns the ctor catalogue under `.ctor`, and `clsKey` came from
                // the already-resolved ctor `TyClass`, so construct by key directly.
                match ctx.Provider.TryLookupType clsKey with
                | ValueSome(ExternalTypeShape.Class _) -> inferExternalCtorOn infer ctx node clsKey args ctorTy argExpr
                | _ ->
                    ctx.Report(node.Tok, Kind.UnknownNominalType(NominalKind.Class, SymbolKeyOps.typeMetaName clsKey))

                    infer ctx argExpr |> ignore
                    TyVar(freshTyVar ctx)
        // A heritable primitive typed by its canon (`new exn "boom"`).
        | TyConst(canonKey, tyArgs) ->
            // A written PLATFORM spelling (`new System.Exception(msg, inner)`) canonicalizes to
            // the same `TyConst`, but the reference still denotes the metadata class, which is the
            // opt-in to the wider ctor catalogue. Read its external verdict, confirm CLASS.
            let stampedClassKey =
                match CstKeys.ofTypeRef t with
                | ValueSome typeRef ->
                    match ctx.Resolution.TypeRefVerdicts.TryGetValue typeRef.Site.Key with
                    | ValueSome(TypeRefVerdict.ExternalType symKey) ->
                        match ctx.Provider.TryLookupType symKey with
                        | ValueSome(ExternalTypeShape.Class _) -> ValueSome symKey
                        | _ -> ValueNone
                    | _ -> ValueNone
                | ValueNone -> ValueNone

            match stampedClassKey with
            | ValueSome declTypeKey -> inferExternalCtorOn infer ctx node declTypeKey tyArgs ctorTy argExpr
            | ValueNone ->
                match ExternalSymbols.tryIntrinsicClass ctx.Provider canonKey with
                | ValueSome(struct (_, surface)) ->
                    let (DisplayName shown) = SymbolKeyOps.typeSimpleName canonKey

                    match
                        inferIntrinsicClassCtorCall
                            infer
                            ctx
                            (tyArgs.AsSpan().ToArray())
                            surface
                            (Kind.Message(sprintf "No applicable constructor on '%s' for the given arguments" shown))
                            argExpr
                    with
                    | ValueSome chosen -> ctx.Resolution.ExternalCtor.Set(node.Key, SymbolKey.Member chosen.Key)
                    | ValueNone -> ()

                    ctorTy
                | ValueNone ->
                    ctx.Report(node.Tok, Kind.NewRequiresClassType)
                    infer ctx argExpr |> ignore
                    TyVar(freshTyVar ctx)
        | _ ->
            ctx.Report(node.Tok, Kind.NewRequiresClassType)
            infer ctx argExpr |> ignore
            TyVar(freshTyVar ctx)

    /// Resolve a constructor application on an external (BCL / referenced) class, shared by
    /// `new T(args)` and the *sugar* form `T args`. Overload-resolves on the argument types,
    /// then unifies the chosen ctor signature so each parameter constrains the arguments.
    and inferExternalCtorOn
        (infer: Infer)
        (ctx: PassContext)
        (node: NodeSite)
        (declTypeKey: TypeKey)
        (args: EqArray<SemType>)
        (ctorTy: SemType)
        (argExpr: Expr<SyntaxToken>)
        : SemType =
        let ctors = ctx.Provider.TryLookupMembers(declTypeKey, ".ctor")
        let name = SymbolKeyOps.typeMetaName declTypeKey

        let argTy = infer ctx argExpr
        let typeArgs = EqArray.toArray args
        let argElems = argElemsOf ctx.Store argTy

        // A 0-argument construction of an external *value type* is `default(T)`, not a real
        // ctor call (`Span<char>()`). A .NET struct's implicit parameterless ctor is not in
        // `GetConstructors`, so the overload pick finds no candidate; admit it directly here.
        let isExternalValueType () =
            match ctx.Provider.TryLookupType declTypeKey with
            | ValueSome(ExternalTypeShape.Class shape) -> shape.Flags.IsValueType
            | _ -> false

        if List.isEmpty argElems && isExternalValueType () then
            ctorTy
        elif ctors.Length = 0 then
            ctx.Report(node.Tok, Kind.Message(sprintf "External type '%s' has no accessible constructor" name))
            ctorTy
        else
            match pickBestOverload ctx typeArgs ctors argElems with
            | ValueSome chosen ->
                // Record the chosen ctor's identity so codegen's `TExpr.New` emission
                // selects this exact same-arity overload by key rather than re-picking.
                ctx.Resolution.ExternalCtor.Set(node.Key, SymbolKey.Member chosen.Key)
                let ctorSig = ExternalSymbols.openSignature chosen typeArgs
                let resultTy = TyVar(freshTyVar ctx)
                // Unify the ctor SIGNATURE (grounding each parameter) but leave `resultTy`
                // free: the ctor `TyClass` from the `new T<args>` annotation is the
                // AUTHORITY, and a no-arg overload may hardcode `any` type args that clash.
                unify ctx node.Tok ctorSig (TyFun(argTy, resultTy))
                ctorTy
            | ValueNone ->
                ctx.Report(
                    node.Tok,
                    Kind.Message(sprintf "No applicable constructor on '%s' for the given arguments" name)
                )

                ctorTy

    /// The `new`-less constructor-as-function sugar: `InvalidOperationException "x"`,
    /// `ArgumentException(message, name)`. The applied function must resolve to an external class (via
    /// through the active `open`s) and not be a local binding. Ctor args arrive as one tuple.
    and tryInferExternalCtorApp
        (infer: Infer)
        (ctx: PassContext)
        (node: NodeSite)
        (fn: Expr<SyntaxToken>)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        : SemType voption =
        if args.Length <> 1 then
            ValueNone
        else
            // NameResolution stamps `ResolvedType` only when the applied function does NOT resolve as a
            // local, so reading the stamp inherently excludes a local binding shadowing a type
            // name. A non-class function declines to the caller's fallback rather than erroring.
            match ctx.Resolution.ResolvedType.TryGetValue(CstKeys.ofExpr fn) with
            | ValueSome declTypeKey ->
                match ctx.Provider.TryLookupType declTypeKey with
                | ValueSome(ExternalTypeShape.Class _) ->
                    // `externalClassTy` mints a canon `TyConst` for a platform repr, else the
                    // external `TyClass`.
                    let ctorTy = externalClassTy ctx declTypeKey EqArray.empty

                    ValueSome(inferExternalCtorOn infer ctx node declTypeKey EqArray.empty ctorTy args.[0])
                | _ -> ValueNone
            | ValueNone -> ValueNone

    /// Construction of an external *generic* class through an explicit type application:
    /// `ResizeArray<int>()`, `List<string>(cap)`. The type args pin the element type up
    /// front, which a parameterless ctor's value args cannot. A *local* generic name declines.
    and tryInferExternalGenericCtorApp
        (infer: Infer)
        (ctx: PassContext)
        (node: NodeSite)
        (fn: Expr<SyntaxToken>)
        (argExpr: Expr<SyntaxToken>)
        : SemType voption =
        match fn with
        | Expr.TypeApp(expr = ctorFun; types = tyArgs) ->
            // The stamp is minted opens-aware from the spelling alone, so the local-bound-variable
            // guard must stay on the read side.
            let ctorUnbound =
                match ctorFun with
                | Expr.Ident ctorTok ->
                    not (ctx.Bindings.Binding.ContainsKey(NodeKey.ofToken ctorTok NodeKind.ExprIdent))
                | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) ->
                    li.Idents.Length >= 1
                    && not (ctx.Bindings.Binding.ContainsKey(NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent))
                | _ -> false

            if not ctorUnbound then
                ValueNone
            else
                // NameResolution's TypeApp visit stamped the ctor's `ResolvedType` at exact
                // arity. An abbreviation stamps its OWN key, which `tryExternalTypeOfKey`
                // then expands: `ResizeArray<int>` → `TyClass(List`1, [int])`.
                match ctx.Resolution.ResolvedType.TryGetValue(CstKeys.ofExpr ctorFun) with
                | ValueSome symKey ->
                    let explicit = EqArray.ofSeq (seq { for t in tyArgs -> translateType ctx t })

                    match tryExternalTypeOfKey ctx symKey explicit with
                    | ValueSome(TyClass(clsKey, args) as ctorTy) ->
                        ValueSome(inferExternalCtorOn infer ctx node clsKey args ctorTy argExpr)
                    | _ -> ValueNone
                | ValueNone -> ValueNone
        | _ -> ValueNone

    /// Construction of a *local* class/struct through a **secondary** constructor. The
    /// ctor-as-function path builds from the PRIMARY ctor's params only, so it can neither
    /// ground a generic class's type args nor reach a secondary that shares the primary's
    /// arity; this unifies the selected secondary's params, which carry both.
    and tryInferLocalCtorApp
        (infer: Infer)
        (ctx: PassContext)
        (node: NodeSite)
        (fn: Expr<SyntaxToken>)
        (argExpr: Expr<SyntaxToken>)
        : SemType voption =
        let ctorFun, explicitTyArgs =
            match fn with
            | Expr.TypeApp(expr = h; types = ts) -> h, ValueSome [ for t in ts -> translateType ctx t ]
            | _ -> fn, ValueNone

        // The ctor may spell the class bare (`OnceEnum(x)`) or through the module holding it
        // (`A.OnceEnum(x)`), and either form yields one written name.
        let ctorName =
            match ctorFun with
            | Expr.Ident ctorTok when not (ctx.Bindings.Binding.ContainsKey(NodeKey.ofToken ctorTok NodeKind.ExprIdent)) ->
                ValueSome(WrittenTypeName.bare (ctx.NameOf ctorTok))
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
                not (ctx.Bindings.Binding.ContainsKey(NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent))
                ->
                ValueSome(ctx.WrittenTypeNameOf li)
            | _ -> ValueNone

        match ctorName with
        | ValueNone -> ValueNone
        | ValueSome written ->
            // The name denotes a class only if one is in scope AT THE CALL: a class declared
            // below it is not constructible there.
            match TypeRegistry.tryWrittenClass ctx.Types (ctx.UseSiteAt node.Key) written with
            | ValueNone -> ValueNone
            | ValueSome info ->
                let argTy = infer ctx argExpr
                let args, subst = freshNamedInstance ctx info.TypeParams

                // Explicit type args (`Box<int>(x)`) pin the instantiation before the pick, so
                // a generic class's parameters rank at the written instantiation.
                match explicitTyArgs with
                | ValueSome ex when ex.Length = args.Length ->
                    List.iter2 (fun a e -> unify ctx node.Tok a e) (EqArray.toList args) ex
                | _ -> ()

                // The PRIMARY declines: `classCtorAsFunction` already types that spelling, and
                // taking it here would skip the function-application seam it is stamped at.
                match pickLocalCtor ctx (substituteWith ctx.Store subst) info (argElemsOf ctx.Store argTy) with
                | ValueSome(LocalCtorPick.Secondary paramTys) ->
                    unify ctx node.Tok (tupleOrSingle ctx paramTys) argTy
                    ValueSome(TyClass(info.TypeKey, args))
                | ValueSome(LocalCtorPick.Primary _)
                | ValueNone -> ValueNone
