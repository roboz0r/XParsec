namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationEngine
open UnificationTranslate
open UnificationInfer

// Typing a class's CONSTRUCTORS: each secondary `new(…)` body, and the `inherit Base(args)`
// chain call. Both check an argument list against a constructor surface, which is the one
// thing the enclosing member walk does not do.

module internal UnificationClassCtors =

    /// Rebuild a type definition's typar scope from the registry entry's `TypeParams`, so a
    /// field type containing `'name` resolves to the same root the registry already holds.
    let scopeOfTypeParams (typeParams: EqArray<DeclaredTypar>) : Dictionary<string, TyVarId> =
        let d = Dictionary<string, TyVarId>(System.StringComparer.Ordinal)

        for tp in typeParams do
            if not (d.ContainsKey tp.Name) then
                d.[tp.Name] <- tp.TyVar

        d

    /// `expected` is the primary ctor's tupled parameter type; the chain call's
    /// arguments unify against it, and its function position is never inferred.
    let rec private inferSecondaryCtorBody
        (ctx: PassContext)
        (expected: SemType)
        (fieldTypes: Map<string, SemType>)
        (ace: AdditionalConstrExpr<SyntaxToken>)
        : unit =
        match ace with
        | AdditionalConstrExpr.LetIn(binding = b; body = body) ->
            inferBinding ctx b
            inferSecondaryCtorBody ctx expected fieldTypes body
        | AdditionalConstrExpr.SequenceAfter(stmt = s; rest = rest) ->
            infer ctx s |> ignore
            inferSecondaryCtorBody ctx expected fieldTypes rest
        | AdditionalConstrExpr.SequenceBefore(before = before; expr = e) ->
            inferSecondaryCtorBody ctx expected fieldTypes before
            infer ctx e |> ignore
        | AdditionalConstrExpr.Conditional(cond = c; thenBranch = t; elseBranch = el) ->
            infer ctx c |> ignore
            inferSecondaryCtorBody ctx expected fieldTypes t
            inferSecondaryCtorBody ctx expected fieldTypes el
        | AdditionalConstrExpr.Init initExpr ->
            match initExpr with
            | AdditionalConstrInitExpr.Expression e ->
                match e with
                | Expr.HighPrecedenceApp(argExpr = argExpr) ->
                    let argTy = infer ctx argExpr
                    // Chain call to the primary ctor, admitting an implicit class→interface
                    // upcast: `new() = Set(Comparer<'T>.Default, …)` into an `IComparer<'T>`
                    // primary-ctor param.
                    unifyArg ctx (CstKeys.firstTokenOfExpr argExpr) argTy expected
                | Expr.App(argExprs = argExprs) ->
                    let argTys = [ for a in argExprs -> infer ctx a ]
                    unifyArg ctx (CstKeys.firstTokenOfExpr e) (tupleOrSingle ctx.Intrinsics argTys) expected
                | _ -> infer ctx e |> ignore
            | AdditionalConstrInitExpr.Delegated(expr = e) -> infer ctx e |> ignore
            // Explicit field-init `{ f = e; … }`: unify each initialiser against the named
            // field's declared type, so a literal (`0`) or a generic field (`'T`) pins.
            | AdditionalConstrInitExpr.Explicit(initializers = inits) ->
                for FieldInitializer(longIdent = li; expr = e) in inits do
                    let initTy = infer ctx e

                    if not li.Idents.IsEmpty then
                        let fieldName = ctx.NameOf li.Idents.[li.Idents.Length - 1]

                        match Map.tryFind fieldName fieldTypes with
                        | Some fieldTy -> unify ctx (CstKeys.firstTokenOfExpr e) initTy fieldTy
                        | None -> ()

    /// Type every secondary ctor of a class under its typar scope: seed the param
    /// binding-site TyVars, then infer each body.
    let fillSecondaryCtors (ctx: PassContext) (info: ClassTypeInfo) : unit =
        if info.SecondaryCtors.Length > 0 then
            let savedEnclosing = ctx.Resolution.EnclosingTypars
            let classScope = scopeOfTypeParams info.TypeParams
            use _ = ctx.PushTyparScope(classScope, true)
            ctx.Resolution.EnclosingTypars <- ValueSome classScope

            try
                let expected =
                    info.CtorParams
                    |> Array.map (fun p -> p.Type)
                    |> Array.toList
                    |> tupleOrSingle ctx.Intrinsics

                // Declared field types (ctor-param backing fields + explicit `val` fields)
                // keyed by name. `val` fields win a name clash, because a positional ctor param
                // sharing a name is the backing store.
                let fieldTypes =
                    Map.ofSeq (
                        seq {
                            for p in info.CtorParams -> p.Name, p.Type
                            for f in info.InstanceFields -> f.Name, f.Type
                        }
                    )

                for sc in info.SecondaryCtors do
                    for p in sc.Params do
                        match p.Type with
                        | TyVar tv -> ctx.Bindings.TypeVar.Set(BoundVarKey.identity p.DeclSite.BoundVar, tv)
                        | _ -> ()

                    enterLevel ctx

                    try
                        inferSecondaryCtorBody ctx expected fieldTypes sc.Body
                    finally
                        exitLevel ctx
            finally
                ctx.Resolution.EnclosingTypars <- savedEnclosing

    /// The constructor surface an `inherit Base(args)` clause's arguments are checked against.
    [<RequireQualifiedAccess>]
    type private BaseCtorSurface =
        /// A project-local base, ranked over its declared constructors like any other
        /// construction, at the type args the `inherit` clause supplied.
        | Local of ClassTypeInfo * EqArray<SemType>
        /// A base a provider published with a MODELLED constructor catalogue: overload-resolved,
        /// and the pick stamped where `TBaseCtorCall.ChosenCtor` reads it.
        | Provided of TypeKey * EqArray<SemType>
        /// A heritable primitive (`inherit exn(m)`), checked against its contract `.ctor` surface.
        | Heritable of TypeKey * EqArray<SemType> * IntrinsicClassSurface
        /// A base whose constructors are not modelled, so its arguments go UNCHECKED: a
        /// metadata class's protected `.ctor` (`System.Attribute`) is absent from the
        /// catalogue, and a self-host build has no shape for a heritable primitive at all.
        | Unmodelled
        /// A heritable primitive that declares no `.ctor`, and whose platform type id does not
        /// resolve to an external type: a sentinel id (`"!Vesper.Attribute"`), or a missing
        /// dependency.
        | PlatformUnresolved of canon: TypeKey * PlatformTypeId
        /// A heritable primitive that declares no `.ctor`, and whose platform binding does not
        /// cover this target.
        | PlatformUnsupported of canon: TypeKey * target: string

    /// The surface a nominal class base offers: its project-local declaration, else the
    /// provider's.
    let private classCtorSurfaceOf (ctx: PassContext) (baseKey: TypeKey) (baseArgs: EqArray<SemType>) =
        match TypeRegistry.tryClassByKey ctx.Types baseKey with
        | ValueSome baseInfo -> BaseCtorSurface.Local(baseInfo, baseArgs)
        | ValueNone ->
            let declaredInThisAssembly =
                match ctx.Provider.TryLookupType baseKey with
                | ValueSome(ExternalTypeShape.Class shape) -> shape.Origin.Home.DeclaringFile.IsSome
                | _ -> false

            match ctx.Provider.TryLookupMembers(baseKey, ".ctor"), declaredInThisAssembly with
            | EqEmpty, false -> BaseCtorSurface.Unmodelled
            | _ -> BaseCtorSurface.Provided(baseKey, baseArgs)

    /// The surface a heritable primitive offers: its contract `.ctor`s (`exn`), else the
    /// constructors of the platform type its identity denotes.
    let private heritableCtorSurfaceOf (ctx: PassContext) (canonKey: TypeKey) (canonArgs: EqArray<SemType>) =
        let surface = ExternalSymbols.tryIntrinsicClass ctx.Provider canonKey

        match surface with
        | ValueSome(struct (_, s)) when s.Members |> EqArray.exists (fun m -> m.Name = ".ctor") ->
            BaseCtorSurface.Heritable(canonKey, canonArgs, s)
        | _ ->
            // The platform type id: this file's own `(# class … #)` binding, else the
            // provider's identity.
            let platform =
                match ctx.Types.IntrinsicBindings.TryGetValue canonKey with
                | true, binding -> ValueSome(IntrinsicPlatform.Bound binding.TypeId)
                | _ -> surface |> ValueOption.map (fun (struct (id, _)) -> id.Platform)

            match platform with
            | ValueSome(IntrinsicPlatform.Bound typeId) ->
                match ExternalSymbols.tryMetaTypeAt ctx.Provider typeId.Value canonArgs.Length with
                | ValueSome(struct (extKey, _)) -> classCtorSurfaceOf ctx extKey canonArgs
                | ValueNone -> BaseCtorSurface.PlatformUnresolved(canonKey, typeId)
            | ValueSome(IntrinsicPlatform.Unsupported target) -> BaseCtorSurface.PlatformUnsupported(canonKey, target)
            | ValueNone -> BaseCtorSurface.Unmodelled

    /// Classify what `baseTy` offers an `inherit` clause.
    let private baseCtorSurfaceOf (ctx: PassContext) (baseTy: SemType) : BaseCtorSurface =
        match baseTy with
        | TyClass(baseKey, baseArgs) -> classCtorSurfaceOf ctx baseKey baseArgs
        | TyConst(canonKey, canonArgs) -> heritableCtorSurfaceOf ctx canonKey canonArgs
        | _ -> BaseCtorSurface.Unmodelled

    /// Type the `inherit Base(args)` invocation against the constructor `args` selects on the
    /// parent, its parameter types substituted with the args `inherit Base<…>` supplied (read
    /// off `info.Base`).
    let fillBaseCtorCall (ctx: PassContext) (info: ClassTypeInfo) : unit =
        match info.Base with
        | ValueSome {
                        Parent = parent
                        CtorArgs = ValueSome argExpr
                    } ->
            let baseTy = BaseParent.ty parent
            let node = CstKeys.siteOfExpr argExpr

            enterLevel ctx

            try
                match baseCtorSurfaceOf ctx baseTy with
                | BaseCtorSurface.Local(baseInfo, baseArgs) ->
                    let subst = mkNamedTypeSubst ctx.Store baseInfo.TypeParams baseArgs
                    let argTy = infer ctx argExpr

                    let expected =
                        match
                            UnificationInferOverload.pickLocalCtor
                                ctx
                                (substituteWith ctx.Store subst)
                                baseInfo
                                (argElemsOf ctx.Store argTy)
                        with
                        | ValueSome pick -> pick.Parameters
                        // No constructor of that arity: unify against the primary anyway, so
                        // the mismatch is reported at the arguments.
                        | ValueNone -> [ for p in baseInfo.CtorParams -> substituteWith ctx.Store subst p.Type ]
                        |> tupleOrSingle ctx.Intrinsics

                    unify ctx node.Tok argTy expected

                | BaseCtorSurface.Provided(baseKey, baseArgs) ->
                    UnificationInferCtor.inferExternalCtorOn
                        infer
                        ctx
                        node
                        baseKey
                        baseArgs
                        (TyClass(baseKey, baseArgs))
                        argExpr
                    |> ignore

                | BaseCtorSurface.Heritable(canonKey, canonArgs, surface) ->
                    let (DisplayName shown) = SymbolKeyOps.typeSimpleName canonKey

                    match
                        UnificationInferCtor.inferIntrinsicClassCtorCall
                            infer
                            ctx
                            (canonArgs.AsSpan().ToArray())
                            surface
                            (Kind.Message(
                                sprintf "No applicable constructor on base '%s' for the given 'inherit' arguments" shown
                            ))
                            argExpr
                    with
                    | ValueSome chosen -> ctx.Resolution.ExternalCtor.Set(node.Key, SymbolKey.Member chosen.Key)
                    | ValueNone -> ()

                | BaseCtorSurface.Unmodelled -> infer ctx argExpr |> ignore

                | BaseCtorSurface.PlatformUnresolved(canonKey, typeId) ->
                    ctx.Report(
                        node.Tok,
                        Kind.Message(
                            sprintf
                                "Cannot inherit from external base '%s': its representation '%s' did not resolve to a known external type (is a package dependency missing?)"
                                canonKey.Name
                                typeId.Value
                        )
                    )

                    infer ctx argExpr |> ignore

                | BaseCtorSurface.PlatformUnsupported(canonKey, target) ->
                    ctx.Report(node.Tok, Kind.UnsupportedOnTarget(canonKey.Name, target))
                    infer ctx argExpr |> ignore
            finally
                exitLevel ctx
        | _ -> ()

    /// Mint the `base` TyVar pre-linked to the parent's instantiated `TyClass` and seed
    /// `ctx.Bindings.TypeVar` at `info.BaseKey`.
    let mintBaseTyVar (ctx: PassContext) (info: ClassTypeInfo) : unit =
        match info.Base with
        | ValueSome inh ->
            let baseTv = ctx.NewTypeVar()
            ctx.Store.SetLevel(UnionFind.find ctx.Store baseTv, ctx.CurrentLevel)
            ctx.Store.SetLink(UnionFind.find ctx.Store baseTv, ValueSome(BaseParent.ty inh.Parent))
            ctx.Bindings.TypeVar.Set(BoundVarKey.identity info.BaseKey, baseTv)
        | ValueNone -> ()
