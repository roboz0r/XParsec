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
                // `computeMembers` surfaces them under `.ctor`).
                let name = SymbolKeyOps.qualifiedName clsKey

                match ctx.Provider.TryLookupType name with
                | ValueSome(ExternalTypeShape.Class _) -> inferExternalCtorOn infer ctx key name args receiverTy argExpr
                | _ ->
                    ctx.Error(key, sprintf "Unknown class type '%s'" name)
                    infer ctx argExpr |> ignore
                    TyVar(freshTyVar ctx)
        // A heritable primitive (`new exn "boom"` — or `new System.Exception "boom"`,
        // canonicalized to the canon `TyConst` at resolution). Its constructible
        // surface rides the provider's `IntrinsicClass` shape; route the `.ctor`
        // lookup through the PLATFORM repr (`System.Exception`), the same per-target
        // routing an intrinsic receiver's instance members use, so the full platform
        // ctor catalogue stays reachable (e.g. the inner-exception overload the
        // contract deliberately does not declare).
        | TyConst(canonKey, tyArgs) when
            (let isIntrinsicClass (c: string) =
                match ctx.Provider.TryLookupType c with
                | ValueSome(ExternalTypeShape.IntrinsicClass _) -> ValueSome c
                | _ -> ValueNone

             (OpenScope.tryResolve ctx.Resolution.OpenScope isIntrinsicClass (SymbolKeyOps.intrinsicName canonKey))
                 .IsSome)
            ->
            let platform = intrinsicPlatformName ctx (SymbolKeyOps.intrinsicName canonKey)
            inferExternalCtorOn infer ctx key platform tyArgs receiverTy argExpr
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
        (name: string)
        (args: EqArray<SemType>)
        (receiverTy: SemType)
        (argExpr: Expr<SyntaxToken>)
        : SemType =
        let ctors = ctx.Provider.TryLookupMembers(name, ".ctor")
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
            match ctx.Provider.TryLookupType name with
            | ValueSome(ExternalTypeShape.Class shape) -> shape.Flags.IsValueType
            | _ -> false

        if List.isEmpty argElems && isExternalValueType () then
            receiverTy
        elif ctors.Length = 0 then
            ctx.Error(key, sprintf "External type '%s' has no accessible constructor" name)
            receiverTy
        else
            match pickBestOverload typeArgs ctors argElems with
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
            let headName =
                match fn with
                | Expr.Ident tok when not (ctx.Bindings.Binding.ContainsKey(NodeKey.ofToken tok NodeKind.ExprIdent)) ->
                    ValueSome(ctx.NameOf tok)
                | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
                    li.Idents.Length >= 1
                    && not (ctx.Bindings.Binding.ContainsKey(NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent))
                    ->
                    ValueSome(li.Idents |> Seq.map ctx.NameOf |> String.concat ".")
                | _ -> ValueNone

            match headName with
            | ValueNone -> ValueNone
            | ValueSome name ->
                match OpenScope.tryQualify ctx.Resolution.OpenScope (isExternalClass ctx) name with
                | ValueSome resolved ->
                    // Mint the ctor's result with the resolved type's identity: the
                    // canon `TyConst` when `resolved` is a platform repr with a
                    // harvested non-interface canon (`System.Exception "boom"` →
                    // `exn`, mirroring `tryResolveExternalType`), else the external
                    // `TyClass` keyed on the shape's home assembly so it unifies with
                    // the same type resolved elsewhere. The `.ctor` lookup stays
                    // keyed on `resolved` — the platform name owns the ctor catalogue
                    // either way.
                    let receiverTy =
                        match ctx.Provider.TryLookupType resolved with
                        | ValueSome(ExternalTypeShape.Class info) ->
                            match ctx.IntrinsicReverseCanon.Value.TryGetValue resolved with
                            | true, (canon :: _) when not info.IsInterface -> TyConst(canon, EqArray.empty)
                            | _ -> TyClass(SymbolKeyOps.externalTypeKey info.Origin resolved 0, EqArray.empty)
                        | _ -> TyClass(SymbolKeyOps.qualifiedTypeKey resolved 0, EqArray.empty)

                    ValueSome(inferExternalCtorOn infer ctx key resolved EqArray.empty receiverTy args.[0])
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
    /// Freeze/codegen the `tyArgs` to emit `newobj List`1<!!T>::.ctor()`. A *local*
    /// generic class (`Box<int>(x)`) isn't an in-scope external type, so the resolver
    /// returns `ValueNone` and this declines — the local path (`inferTypeApp`'s
    /// nominal-unify arm) handles it.
    and tryInferExternalGenericCtorApp
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
        (fn: Expr<SyntaxToken>)
        (argExpr: Expr<SyntaxToken>)
        : SemType voption =
        match fn with
        | Expr.TypeApp(expr = headExpr; types = tyArgs) ->
            let headName =
                match headExpr with
                | Expr.Ident tok when not (ctx.Bindings.Binding.ContainsKey(NodeKey.ofToken tok NodeKind.ExprIdent)) ->
                    ValueSome(ctx.NameOf tok)
                | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
                    li.Idents.Length >= 1
                    && not (ctx.Bindings.Binding.ContainsKey(NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent))
                    ->
                    ValueSome(li.Idents |> Seq.map ctx.NameOf |> String.concat ".")
                | _ -> ValueNone

            match headName with
            | ValueNone -> ValueNone
            | ValueSome name ->
                let explicit = EqArray.ofSeq (seq { for t in tyArgs -> translateType ctx t })

                match tryResolveExternalNominal ctx name explicit with
                | ValueSome(TyClass(clsKey, args) as receiverTy) ->
                    ValueSome(
                        inferExternalCtorOn infer ctx key (SymbolKeyOps.qualifiedName clsKey) args receiverTy argExpr
                    )
                | _ -> ValueNone
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
            match ctx.Types.Class.TryGetValue name with
            | false, _ -> ValueNone
            | true, info ->
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
