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
    /// `IntrinsicClassSurface.Members` riding the provider shape) against `argExpr` —
    /// the SINGLE constructible surface `new exn "…"` (`inferNew`) and
    /// `inherit exn(…)` (`Unification.fillBaseCtorCall`) both check, target-agnostic
    /// by construction. The chosen signature grounds the call's arguments while the
    /// result side stays a free var (`inferExternalCtorOn`'s authority rule: the
    /// receiver/declared parent already IS the constructed type). `noOverloadMsg`
    /// keeps the two syntaxes' diagnostics distinct.
    let inferIntrinsicClassCtorCall
        (infer: Infer)
        (ctx: PassContext)
        (typeArgs: SemType[])
        (surface: IntrinsicClassSurface)
        (noOverloadMsg: string)
        (argExpr: Expr<SyntaxToken>)
        : unit =
        let ctors = surface.Members |> Array.filter (fun m -> m.Name = ".ctor")
        let argTy = infer ctx argExpr

        match pickBestOverload (capabilityCanonKey ctx) typeArgs ctors (argElemsOf argTy) with
        | ValueSome chosen ->
            let ctorSig = ExternalSymbols.openSignature chosen typeArgs
            let resultTy = TyVar(freshTyVar ctx)
            unify ctx (CstKeys.ofExpr argExpr) ctorSig (TyFun(argTy, resultTy))
        | ValueNone -> ctx.Error(CstKeys.ofExpr argExpr, noOverloadMsg)

    /// `new T(args)`. Mirrors a single application against the ctor, kept inline
    /// so a bare `Expr.New` doesn't need to fabricate an `Expr.App` first.
    let rec inferNew
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
        (t: Type<SyntaxToken>)
        (argExpr: Expr<SyntaxToken>)
        : SemType =
        let receiverTy = translateType ctx t

        // Type provenance: `new T(…)` writes the constructed node's type explicitly.
        ctx.MarkTypeDeclared(key, receiverTy)

        match resolveStep receiverTy with
        | TyClass(clsKey, args) ->
            match TypeRegistry.tryClassByKey ctx.Types clsKey with
            | ValueSome info ->
                let subst = mkNamedTypeSubst info.TypeParams args
                let argTy = infer ctx argExpr
                let argArity = argArityOf argTy

                // Prefer the primary constructor when its arity matches; otherwise
                // fall back to a secondary `new(...)` constructor of the right arity.
                // A type whose *only* constructor is an explicit `new(...)` (e.g. the
                // `[<Struct>]` `SetIterator<'T>` with `val` fields + `new(s)`) has an
                // empty `CtorParams`, so `new SetIterator<'T>(s)` must resolve through
                // `SecondaryCtors` — the `new`-keyword twin of `tryInferLocalCtorApp`'s
                // secondary-ctor path for the application form.
                let secondary =
                    if argArity = info.CtorParams.Length then
                        None
                    else
                        info.SecondaryCtors |> Array.tryFind (fun sc -> sc.Params.Length = argArity)

                let expected =
                    match secondary with
                    | Some sc -> sc.Params |> Array.map (fun p -> substituteWith subst p.Type) |> Array.toList
                    | None ->
                        info.CtorParams
                        |> Array.map (fun p -> substituteWith subst p.Type)
                        |> Array.toList
                    |> tupleOrSingle ctx

                unifyArg ctx (CstKeys.ofExpr argExpr) argTy expected
                receiverTy
            | ValueNone ->
                // Fall through to the external-class path: `new System.Exception(msg)`
                // inside an inline body (the `failwith` body, `raise (System.Exception
                // message)`) — the type was named through `tryResolveExternalType` so
                // `name` is the metadata full name, and the symbol provider already
                // owns the ctor catalogue (`MetadataSymbols.extractMembers` /
                // `computeMembers` surfaces them under `.ctor`). `clsKey` came from the
                // already-resolved receiver `TyClass`, so construct by key directly.
                match ctx.Provider.TryLookupType clsKey with
                | ValueSome(ExternalTypeShape.Class _) ->
                    inferExternalCtorOn infer ctx key clsKey args receiverTy argExpr
                | _ ->
                    ctx.Error(key, sprintf "Unknown class type '%s'" (SymbolKeyOps.qualifiedName clsKey))
                    infer ctx argExpr |> ignore
                    TyVar(freshTyVar ctx)
        // A heritable primitive typed by its canon (`new exn "boom"`). The
        // constructible surface is the CONTRACT `.ctor` set riding the shape's
        // class surface — the SAME set `inherit exn(…)` checks
        // (`fillBaseCtorCall`), so the two syntaxes cannot diverge; resolved by
        // DIRECT qualified lookup off the already-resolved canon key, never a
        // short-name re-scan. A written PLATFORM spelling
        // (`new System.Exception(msg, inner)`) canonicalizes to the same `TyConst`
        // at resolution, but the WRITTEN head still names the metadata class —
        // that spelling is the deliberate opt-in to the platform's wider ctor
        // catalogue (the app-form sugar `System.Exception msg` already routes
        // there), at the cost of platform generality; probe it first. A `TyConst`
        // that is neither (`new int(...)`) falls to the "'new' requires a class
        // type" error; a self-host compile of the contract itself has no provider
        // shape and errors the same way.
        | TyConst(canonKey, tyArgs) ->
            // The written head canonicalized to an intrinsic `TyConst`, but a written
            // PLATFORM spelling (`new System.Exception(msg, inner)`) still names the
            // metadata class — the deliberate opt-in to the platform's wider ctor
            // catalogue. NameResolution stamped every written head's identity into
            // `ResolvedTypeHead`, so read the head's stamp and confirm the CLASS shape
            // by key (the stamp is any-shape: a canon spelling like `new exn "boom"`
            // stamps its intrinsic identity, which must fall to the contract
            // constructible-surface path below, not the metadata catalogue).
            let stampedClassKey =
                match CstKeys.ofTypeHead t with
                | ValueSome head ->
                    match ctx.Resolution.ResolvedTypeHead.TryGetValue head.Key with
                    | ValueSome symKey ->
                        match ctx.Provider.TryLookupType symKey with
                        | ValueSome(ExternalTypeShape.Class _) -> ValueSome symKey
                        | _ -> ValueNone
                    | ValueNone -> ValueNone
                | ValueNone -> ValueNone

            match stampedClassKey with
            | ValueSome declTypeKey -> inferExternalCtorOn infer ctx key declTypeKey tyArgs receiverTy argExpr
            | ValueNone ->
                match ExternalSymbols.tryIntrinsicClass ctx.Provider canonKey with
                | ValueSome(struct (_, surface)) ->
                    inferIntrinsicClassCtorCall
                        infer
                        ctx
                        (tyArgs.AsSpan().ToArray())
                        surface
                        (sprintf
                            "No applicable constructor on '%s' for the given arguments"
                            (SymbolKeyOps.simpleName canonKey))
                        argExpr

                    receiverTy
                | ValueNone ->
                    ctx.Error(key, "'new' requires a class type")
                    infer ctx argExpr |> ignore
                    TyVar(freshTyVar ctx)
        | _ ->
            ctx.Error(key, "'new' requires a class type")
            infer ctx argExpr |> ignore
            TyVar(freshTyVar ctx)

    /// Resolve a constructor application on an external (BCL / referenced) class —
    /// shared by `new T(args)` (`inferNew`) and the *sugar* form `T args` (a ctor
    /// treated as a first-class function, routed here from `inferApp` via
    /// `tryInferExternalCtorApp`). `name` is the resolved metadata full name and
    /// `receiverTy` the `TyClass(name, args)` the call yields; the provider owns the
    /// `.ctor` catalogue. Overload-resolves on the argument types, then unifies the
    /// chosen ctor signature `(p1 * … * pN) → declTy` against `TyFun(argTy, result)`
    /// so each parameter constrains the call's arguments — the same shape as
    /// `tryInferExternalStaticMethodCall`.
    and inferExternalCtorOn
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
        (declTypeKey: SymbolKey)
        (args: EqArray<SemType>)
        (receiverTy: SemType)
        (argExpr: Expr<SyntaxToken>)
        : SemType =
        // `declTypeKey` is the constructed type's identity, resolved upstream (a
        // NameResolution `ResolvedType` stamp for a ctor-sugar head, or the already-
        // resolved receiver `TyClass`/`TyConst` key for `new T(…)`). The `.ctor`
        // catalogue is a key-addressed store-face lookup, not a spelling re-scan.
        let ctors = ctx.Provider.TryLookupMembers(declTypeKey, ".ctor")
        let name = SymbolKeyOps.qualifiedName declTypeKey

        let argTy = infer ctx argExpr
        let typeArgs = args |> EqArray.toList |> List.toArray
        let argElems = argElemsOf argTy

        // A 0-argument construction of an external *value type* is `default(T)`,
        // not a real ctor call — `Span<char>()`, `default(SomeStruct)`. A .NET
        // struct's implicit parameterless ctor is not in `GetConstructors`, so the
        // overload pick below finds no candidate; admit it directly here (codegen's
        // `EmitConstruct.buildNew` lowers it to `initobj`, the Gap D path). This
        // also covers a value type whose only ctors are explicit (`ctors` non-empty
        // but none 0-arg) and one with no surfaced ctors at all.
        let isExternalValueType () =
            match ctx.Provider.TryLookupType declTypeKey with
            | ValueSome(ExternalTypeShape.Class shape) -> shape.Flags.IsValueType
            | _ -> false

        if List.isEmpty argElems && isExternalValueType () then
            receiverTy
        elif ctors.Length = 0 then
            ctx.Error(key, sprintf "External type '%s' has no accessible constructor" name)
            receiverTy
        else
            match pickBestOverload (capabilityCanonKey ctx) typeArgs ctors argElems with
            | ValueSome chosen ->
                let ctorSig = ExternalSymbols.openSignature chosen typeArgs
                let resultTy = TyVar(freshTyVar ctx)
                // Unify the ctor SIGNATURE (grounding each parameter against the call's
                // arguments) but leave `resultTy` free: a constructor's declared return is
                // definitionally the class it constructs, so the receiver `TyClass` built
                // from the `new T<args>` annotation is the AUTHORITY on the result's type
                // args — unifying the declared return back onto it adds nothing when they
                // agree and actively CLASHES when a no-arg overload hardcodes `any` type
                // args (TS's `new (): Map<any, any>`, which `dynamic → FTUnknown "any"`
                // makes an absorbing/error head). `resultTy` absorbs that noise harmlessly;
                // it never meets `receiverTy`, so the explicit `new Map<string,int>()`
                // grounds cleanly. (Ctor return args are the SAME declaring typars,
                // substituted by the SAME `typeArgs` that built the receiver, so nothing
                // the receiver leaves open could have been solved only by the return.)
                unify ctx key ctorSig (TyFun(argTy, resultTy))
                receiverTy
            | ValueNone ->
                ctx.Error(key, sprintf "No applicable constructor on '%s' for the given arguments" name)
                receiverTy

    /// The `new`-less constructor-as-function sugar: `InvalidOperationException "x"`,
    /// `ArgumentException(message, name)`. `inferApp` reaches here only after the
    /// head fails to resolve as a value / static method / union case / *user* class
    /// ctor — exactly the slot that previously fell to a fresh, unconstrained TyVar
    /// (the head's type leaked when buried in an argument, e.g. `raise (Exn "x")`,
    /// surfacing as a stray unresolved TyVar). The head must name an external class
    /// (resolved through the active `open`s) and not be a local binding (a real
    /// call). Multi-argument ctors arrive as one tupled arg, matching `inferNew`.
    and tryInferExternalCtorApp
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
        (fn: Expr<SyntaxToken>)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        : SemType voption =
        if args.Length <> 1 then
            ValueNone
        else
            // NameResolution resolved the head's type identity (opens-aware) and
            // stamped it in `ResolvedType`, keyed by the head expr's `NodeKey`. A local
            // binding shadowing a type name is never stamped (NameResolution stamps
            // only when the head does not resolve as a local), so reading the stamp
            // inherently excludes locals — the old head-binding guard. Confirm the
            // resolved type is a Class (ctor-sugar constructs a class only) via the
            // key-addressed store face; a non-class head declines to the caller's
            // fallback rather than erroring.
            match ctx.Resolution.ResolvedType.TryGetValue(CstKeys.ofExpr fn) with
            | ValueSome declTypeKey ->
                match ctx.Provider.TryLookupType declTypeKey with
                | ValueSome(ExternalTypeShape.Class info) ->
                    // Mint the ctor's result with the resolved type's identity via
                    // `externalClassTy` (canon `TyConst` for a platform repr, else the
                    // external `TyClass`); the `.ctor` lookup is key-addressed.
                    let receiverTy =
                        externalClassTy ctx (SymbolKeyOps.qualifiedName declTypeKey) info 0 EqArray.empty

                    ValueSome(inferExternalCtorOn infer ctx key declTypeKey EqArray.empty receiverTy args.[0])
                | _ -> ValueNone
            | ValueNone -> ValueNone

    /// Construction of an external *generic* class through an explicit type
    /// application: `ResizeArray<int>()`, `List<string>(cap)` — the no-`new`
    /// sugar whose CST is `App`/`HighPrecedenceApp(TypeApp(head, tyArgs), valueArgs)`.
    /// The generic sibling of `tryInferExternalCtorApp`: the head's explicit type
    /// arguments pin the element type up front (`ResizeArray<int>` →
    /// `TyClass(System.Collections.Generic.List`1, [int])`, an abbreviation expanded
    /// to its underlying class) so the constructed node carries `TyClass(List, [int])`
    /// rather than a free TyVar the value-args alone can't resolve for a
    /// parameterless ctor. The pinned class then drives `inferExternalCtorOn`'s
    /// overload pick (so `List()` vs `List(IEnumerable<int>)` resolves) and gives
    /// Elaborate/codegen the `tyArgs` to emit `newobj List`1<!!T>::.ctor()`. A *local*
    /// generic class (`Box<int>(x)`) isn't an in-scope external type, so its head
    /// carries no `ResolvedType` stamp and this declines — the local path
    /// (`inferTypeApp`'s nominal-unify arm) handles it.
    and tryInferExternalGenericCtorApp
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
        (fn: Expr<SyntaxToken>)
        (argExpr: Expr<SyntaxToken>)
        : SemType voption =
        match fn with
        | Expr.TypeApp(expr = headExpr; types = tyArgs) ->
            // A head shadowed by a local binding is never external construction —
            // the stamp is minted opens-aware from the spelling alone, so the
            // local-binder guard must stay on the read side.
            let headUnbound =
                match headExpr with
                | Expr.Ident tok -> not (ctx.Bindings.Binding.ContainsKey(NodeKey.ofToken tok NodeKind.ExprIdent))
                | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) ->
                    li.Idents.Length >= 1
                    && not (ctx.Bindings.Binding.ContainsKey(NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent))
                | _ -> false

            if not headUnbound then
                ValueNone
            else
                // NameResolution's TypeApp visit resolved receiver+arity together and
                // stamped the head's `ResolvedType` (any shape, exact arity — an
                // abbreviation stamps its OWN key). `tryExternalTypeOfKey` fetches the
                // shape on the store face and expands an abbreviation to its underlying
                // class (`ResizeArray<int>` → `TyClass(System.Collections.Generic.List`1,
                // [int])`), so construction proceeds by the resolved class key.
                match ctx.Resolution.ResolvedType.TryGetValue(CstKeys.ofExpr headExpr) with
                | ValueSome symKey ->
                    let explicit = EqArray.ofSeq (seq { for t in tyArgs -> translateType ctx t })

                    match tryExternalTypeOfKey ctx symKey explicit with
                    | ValueSome(TyClass(clsKey, args) as receiverTy) ->
                        ValueSome(inferExternalCtorOn infer ctx key clsKey args receiverTy argExpr)
                    | _ -> ValueNone
                | ValueNone -> ValueNone
        | _ -> ValueNone

    /// Construction of a *local* generic class/struct through a **secondary**
    /// constructor (B-11): `SetIterator<'T>(s)` / `OnceEnum(x)`. The existing
    /// ctor-as-function path (`tryClassCtorAsFunction`) builds its function type
    /// from the *primary* ctor's params only — for a type whose primary is
    /// parameterless and whose construction goes through a `new(args)` overload,
    /// that leaves the type arguments ungrounded (the primary's `unit` arg never
    /// unifies them against the call's value arg). This selects the secondary ctor
    /// by its parameter arity and unifies *its* params — carrying the type args —
    /// against the call, so `OnceEnum(x:'T)` grounds to `OnceEnum<'T>`. Declines
    /// (`ValueNone`) when the arity matches the primary (the existing path handles
    /// it), when the head isn't a local class, or when no secondary matches —
    /// keeping the blast radius to the previously-unsupported secondary case.
    and tryInferLocalCtorApp
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
        (fn: Expr<SyntaxToken>)
        (argExpr: Expr<SyntaxToken>)
        : SemType voption =
        let headExpr, explicitTyArgs =
            match fn with
            | Expr.TypeApp(expr = h; types = ts) -> h, ValueSome [ for t in ts -> translateType ctx t ]
            | _ -> fn, ValueNone

        let headName =
            match headExpr with
            | Expr.Ident tok when not (ctx.Bindings.Binding.ContainsKey(NodeKey.ofToken tok NodeKind.ExprIdent)) ->
                ValueSome(ctx.NameOf tok)
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
                li.Idents.Length = 1
                && not (ctx.Bindings.Binding.ContainsKey(NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent))
                ->
                ValueSome(ctx.NameOf li.Idents.[0])
            | _ -> ValueNone

        match headName with
        | ValueNone -> ValueNone
        | ValueSome name ->
            match TypeRegistry.tryClass ctx.Types name with
            | ValueNone -> ValueNone
            | ValueSome info ->
                let argTy = infer ctx argExpr
                let argArity = argArityOf argTy

                if argArity = info.CtorParams.Length then
                    ValueNone
                else
                    match info.SecondaryCtors |> Array.tryFind (fun sc -> sc.Params.Length = argArity) with
                    | None -> ValueNone
                    | Some sc ->
                        let args, subst = freshNamedInstance ctx info.TypeParams
                        let receiverTy = TyClass(info.Key, args)

                        // Explicit type args (`SetIterator<'T>(s)`) pin the
                        // instantiation up front, mirroring `inferTypeApp`.
                        match explicitTyArgs with
                        | ValueSome ex when ex.Length = args.Length ->
                            List.iter2 (fun a e -> unify ctx key a e) (EqArray.toList args) ex
                        | _ -> ()

                        let paramTys =
                            sc.Params |> Array.map (fun p -> substituteWith subst p.Type) |> Array.toList

                        unify ctx key (tupleOrSingle ctx paramTys) argTy
                        ValueSome receiverTy
