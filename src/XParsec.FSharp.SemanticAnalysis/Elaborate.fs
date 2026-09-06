namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.ElaborateNominals
open XParsec.FSharp.SemanticAnalysis.ElaboratePatterns
open XParsec.FSharp.SemanticAnalysis.ElaborateExpr
open XParsec.FSharp.SemanticAnalysis.ElaborateTypars
open XParsec.FSharp.SemanticAnalysis.ElaborateMembers
open XParsec.FSharp.SemanticAnalysis.ElaborateTypeDecls

module Elaborate =
    /// `ValueNone` below `i+1` lambdas, or where the parameter is not a simple name, because
    /// a destructured parameter can't carry `[<CallAtMostOnce>]`.
    let rec private nthLambdaParam (body: TExpr) (i: int) : (NodeKey * TExpr) voption =
        match body with
        | TExpr.Lambda(p, inner, _, _) ->
            if i = 0 then
                match p with
                | TPat.NamedSimple(k, _, _, _) -> ValueSome(k, inner)
                | _ -> ValueNone
            else
                nthLambdaParam inner (i - 1)
        | _ -> ValueNone

    let private argPatBoundVarKey (p: Pat<SyntaxToken>) : NodeKey voption =
        BoundVarKey.ofCstPat p |> ValueOption.map BoundVarKey.identity

    /// The `[<CallAtMostOnce>]` contract: at most one use of `k` in `scope`, and not under a
    /// lambda or loop. A conditional only skips a use, never repeats it.
    let private paramUsedAtMostOnce (k: NodeKey) (scope: TExpr) : bool =
        match TastWalk.usesOf k scope with
        | [] -> true
        | [ depth ] -> depth = 0
        | _ -> false

    let private recordInlineParamAttrs
        (ctx: PassContext)
        (b: Binding<SyntaxToken>)
        (boundVarKey: NodeKey)
        (valT: TExpr)
        : unit =
        if not b.argumentPats.IsEmpty then
            let attrs =
                EqArray.ofSeq [ for p in b.argumentPats -> Attributes.paramAttrsOfArgPat ctx p ]

            if attrs |> EqArray.exists (fun a -> not a.IsDefault) then
                if not b.inlineToken.IsSome then
                    ctx.Report(
                        (CstKeys.siteOfBinding b).Tok,
                        Kind.Message
                            "A parameter attribute such as [<CallAtMostOnce>] is only valid on a parameter of an 'inline' function"
                    )
                else
                    attrs
                    |> EqArray.iteri (fun i a ->
                        if a.CallAtMostOnce then
                            // This array and the lambda nest must stay positionally aligned:
                            // the inliner re-derives the same `i` from the nest.
                            match nthLambdaParam valT i with
                            | ValueSome(pk, _) when ValueSome pk <> argPatBoundVarKey b.argumentPats.[i] ->
                                failwithf
                                    "Elaborate.recordInlineParamAttrs: parameter %d bound variable key %A does not match its argument pattern (alignment invariant broken)"
                                    i
                                    pk
                            | ValueSome(pk, scope) when paramUsedAtMostOnce pk scope -> ()
                            | ValueSome _ ->
                                ctx.Report(
                                    CstKeys.firstTokenOfPat b.argumentPats.[i],
                                    Kind.Message
                                        "A [<CallAtMostOnce>] parameter must be used at most once in the body, and not under a lambda or loop"
                                )
                            | ValueNone ->
                                ctx.Report(
                                    CstKeys.firstTokenOfPat b.argumentPats.[i],
                                    Kind.NotYetSupported
                                        "[<CallAtMostOnce>] on this parameter shape (it must be a single named parameter)"
                                )
                    )

                    ctx.InlineParamAttrs.[boundVarKey] <- attrs

    /// A *value* binding's free typars are method typars only where the generaliser
    /// quantified them: `let empty: SetTree<'T> = null` has a scheme, `let n = null` does not.
    let private bindingWasGeneralised (ctx: PassContext) (b: Binding<SyntaxToken>) : bool =
        match ctx.TryScheme(CstKeys.ofBinding b) with
        | ValueSome scheme -> not (List.isEmpty scheme.Quantified)
        | ValueNone -> false

    /// Records `boundVar`'s scheme: `quantEnv`'s length as the arity, and its constraints with
    /// each embedded type frozen over `quantEnv`, its typars as `FTTypar(ModuleFunction _, i)`.
    let private recordGenericFnScheme
        (ctx: PassContext)
        (boundVar: BoundVarKey)
        (quantEnv: (TyVarId * SemType) list)
        : unit =
        match quantEnv with
        | [] -> ()
        | _ ->
            let freezeTarget (t: SemType) : FrozenType =
                FrozenTypeBridge.freeze ctx.Store (remapDeclTypars ctx.Store quantEnv t)

            let constraints =
                constraintsOfEnv ctx.Store quantEnv
                |> EqSet.map (TyparConstraint.map freezeTarget)

            ctx.GenericFnSchemes.Set(boundVar, GenericFnScheme.create (List.length quantEnv) constraints)

    /// The binding's exportable identity, keyed by the name its source writes and carrying
    /// `[<CompiledName>]`'s as the name it emits under (`Set.empty` ⇒ `SetModule.Empty`).
    /// `ValueSome` only where the pattern binds exactly one variable; `ValueNone` for the rest
    /// (`let (a, b) = p`) and for an active-pattern name.
    let private exportedBindingInfo
        (ctx: PassContext)
        (container: ModuleContainer)
        (b: Binding<SyntaxToken>)
        (resolved: ResolvedAttributes)
        (attributes: TAttributes)
        : ModuleBindingInfo voption =
        MemberNames.ofBinding ctx b
        |> ValueOption.map (fun m ->
            {
                Container = container
                Name = m.Name
                CompiledName = AttributeDecode.compiledNameOf ctx.NameOf m.Name resolved
                Attributes = attributes
            }
        )

    /// Records the binding's identity and its declared accessibility, returning its key.
    /// `ValueNone` when either `info` or `boundVar` is absent, which records nothing.
    let private recordExportedBinding
        (ctx: PassContext)
        (b: Binding<SyntaxToken>)
        (info: ModuleBindingInfo voption)
        (boundVar: BoundVarKey voption)
        : SymbolKey voption =
        match info, boundVar with
        | ValueSome info, ValueSome bk ->
            ctx.Bindings.ModuleMembers.[bk] <- info
            ctx.Bindings.Accessibility.[info.Key] <- accessibilityOfToken b.access
            ValueSome info.Key
        | _ -> ValueNone

    /// A function binding always quantifies; a value binding only when generalised AND its free
    /// typars sit inside a type constructor, because a bare `ldnull : !!0` does not verify. A
    /// keyless binding (`let (a, b) = …`) has no scope, so its free typars freeze to `FTUnknown`.
    let private moduleLetQuantEnv
        (ctx: PassContext)
        (b: Binding<SyntaxToken>)
        (key: BindingKey voption)
        (declTy: SemType)
        : (TyVarId * SemType) list =
        let declaredTypars =
            match ctx.Bindings.DeclaredTypars.TryGetValue(CstKeys.ofBinding b) with
            | ValueSome ds -> ds
            | ValueNone -> []

        match key with
        | ValueNone -> []
        | ValueSome key ->
            let quantify () =
                mkMethodQuantEnv ctx.Store (TyparScope.ModuleFunction key) declaredTypars declTy

            // An inline binding is never emitted, so no emission gate applies: it is a TEMPLATE
            // whose free typars must be named under the binding's scope or freeze to `FTUnknown`.
            if b.inlineToken.IsSome then
                quantify ()
            else
                match Unification.zonk ctx.Store declTy with
                | TyFun _ -> quantify ()
                // A bare free var is value-restricted, so never a method typar.
                | TyVar _
                | TyTypar _ -> []
                | _ when bindingWasGeneralised ctx b -> quantify ()
                | _ -> []

    /// One module binding as a `let` member, paired with the typar env it freezes over.
    /// `ValueNone` for a format-literal alias, whose `New PrintfFormat` value is dead: it
    /// reaches the frozen tree as neither a declaration nor a bound variable.
    let private translateModuleBinding
        (ctx: PassContext)
        (container: ModuleContainer)
        (b: Binding<SyntaxToken>)
        : (TLetMember * (TyVarId * SemType) list) voption =
        let m = translateLetMember ctx b
        let elided = ctx.PrintfFormatLiterals.ContainsKey(CstKeys.ofPat b.pattern)

        // Read off the TRANSLATED pattern, never the CST binding: the analysis identity
        // addresses a pattern node that `translatePat` erases for `let (x: int) = …`.
        let boundVar = if elided then ValueNone else BoundVarKey.ofPat m.Pattern

        let declTy = m.Ty

        // The scope the binding's own typars quantify under; `ValueNone` for a pattern binding.
        let bindingKey =
            MemberNames.ofBinding ctx b
            |> ValueOption.map (fun named -> SymbolKeyOps.bindingKeyOf container named.Name)

        let quantEnv = moduleLetQuantEnv ctx b bindingKey declTy

        let attrElement =
            let isFunctionShaped =
                match Unification.zonk ctx.Store declTy with
                | TyFun _ -> true
                | _ -> false

            AttrTarget.ofModuleValue isFunctionShaped (not (List.isEmpty quantEnv))

        // The fold enforces `[<AttributeUsage>]` and yields the attributes a `.fsi` comparison
        // reads.
        let resolvedAttrs = ctx.ResolveAttributes b.attributes
        let attributes = AttributeFold.build ctx attrElement resolvedAttrs

        let info = exportedBindingInfo ctx container b resolvedAttrs attributes
        let emittedName = info |> ValueOption.map (fun i -> i.EmittedName)
        let exportedKey = recordExportedBinding ctx b info boundVar

        Attributes.declareGlobalBinding ctx b emittedName exportedKey m.Value
        Attributes.declareImportBinding ctx b emittedName exportedKey m.Value

        match m.Pattern with
        | TPat.NamedSimple(boundVarKey, _, _, _) -> recordInlineParamAttrs ctx b boundVarKey m.Value
        | _ -> ()

        // A bound-variable-less pattern has nowhere to file the scheme.
        match boundVar with
        | ValueSome bk -> recordGenericFnScheme ctx bk quantEnv
        | ValueNone -> ()

        if elided then ValueNone else ValueSome(m, quantEnv)

    let private moduleLetDecl (isRec: bool) (b: Binding<SyntaxToken>) (m: TLetMember) : TDecl =
        TDecl.Let(m, b.inlineToken.IsSome, isRec)

    /// A module-level `let rec` group as one `LetGroup` over its surviving members, with the
    /// recorded components restricted to them. A group left with one member is a `Let`.
    let private translateModuleLetGroup
        (ctx: PassContext)
        (container: ModuleContainer)
        (bindings: ImmutableArray<Binding<SyntaxToken>>)
        : (TDecl * DeclEnv) list =
        match translateRecGroup ctx bindings (translateModuleBinding ctx container) with
        | RecGroup.Empty -> []
        | RecGroup.Single((m, env), b) -> [ moduleLetDecl true b m, DeclEnv.One env ]
        | RecGroup.Group(members, components) ->
            [
                TDecl.LetGroup(EqArray.map fst members, components), DeclEnv.PerMember(EqArray.map snd members)
            ]

    let private translateModuleElem (ctx: PassContext) (m: ModuleElem<SyntaxToken>) : (TDecl * DeclEnv) list =
        let container = ctx.CurrentContainer

        match m with
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(isRec = ValueSome _; bindings = bindings)) ->
            translateModuleLetGroup ctx container bindings
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(isRec = ValueNone; bindings = bindings)) ->
            [
                for b in bindings do
                    match translateModuleBinding ctx container b with
                    | ValueSome(m, env) -> yield moduleLetDecl false b m, DeclEnv.One env
                    | ValueNone -> ()
            ]
        | ModuleElem.Expression e ->
            let eT = translateExpr ctx e
            [ TDecl.Expression(eT, typeOfKey ctx (CstKeys.ofExpr e)), DeclEnv.One [] ]
        | ModuleElem.Type defs ->
            [
                for td in defs do
                    match tryTypeDecl ctx td with
                    | Some((TDecl.Type tdecl, _) as result) ->
                        // So the file→file projection can drop a `type private T`.
                        ctx.Bindings.Accessibility.[SymbolKey.Type tdecl.TypeKey] <-
                            accessibilityOfToken (typeDefnAccessToken td)

                        yield fst result, DeclEnv.One(snd result)
                    | Some(decl, env) -> yield decl, DeclEnv.One env
                    | None -> ()
            ]
        | _ -> []

    /// CST → a `TExpr` tree whose `.ty` fields are zonk'd `SemType`, still `TyVar`-carrying.
    /// Each decl is paired with the typar env it quantifies.
    let elaborate (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : (TDecl * DeclEnv) list =
        // The flattened walk NameResolution and Unification take: a by-name read during
        // lowering resolves from the module and `open`s it is written under.
        CstModuleTree.walkImpl ctx.NameOf OpenScope.empty file
        |> List.collect (fun w ->
            ctx.EnterElement w
            translateModuleElem ctx w.Elem
        )

    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : TastFile =
        let elaborateDecls () =
            let elaborated = elaborate ctx file

            // Snapshotted ahead of the expansion walk, because that walk resolves static-opt
            // clauses and trait calls against the DEFINITION's types, which do not ground an
            // `^T` template.
            for (d, env) in elaborated do
                match d with
                | TastWalk.InlineTemplateDecl(k, _) -> ctx.InlineTemplates.[k] <- freezeTypars ctx.Store env d
                | _ -> ()

            let expanded = InlineExpansion.run ctx elaborated

            // No two decls generalize one root, so this union is unambiguous.
            let env = expanded.Decls |> List.collect (fun (_, env) -> env.All)

            let specializations =
                expanded.Specializations
                |> Array.map (fun (e: TSpecialization) ->
                    { e with
                        Pat = freezeTyparsPat ctx.Store env e.Pat
                        Value = freezeTyparsExpr ctx.Store env e.Value
                    }
                )

            let decls =
                expanded.Decls |> List.map (fun (d, env) -> freezeTypars ctx.Store env d)

            decls, specializations

        // Elaborate asserts with `failwith` rather than diagnosing. After a diagnosed error
        // nothing is code-generated, so degrade; unguarded otherwise, so a real bug surfaces.
        // Read at CATCH time, so an error Elaborate itself reported on the way down (an
        // unsupported construct) degrades the same way an earlier pass's does. A diagnosed
        // error masks any Elaborate crash in the same file: fix the reported errors first.
        let hasErrors () =
            ctx.Diagnostics |> Seq.exists Diagnostic.isError

        let elaborated =
            try
                ValueSome(elaborateDecls ())
            with _ when hasErrors () ->
                ValueNone

        let decls, specializations =
            match elaborated with
            | ValueSome result -> result
            | ValueNone -> [], [||]

        // A degraded tree introduces no bound variable, so every table keyed by one empties
        // with it: `TastPools.toPools` faults on an entry whose declaration is absent.
        let emptyIfDegraded
            (entries: seq<System.Collections.Generic.KeyValuePair<BoundVarKey, 'v>>)
            : Map<BoundVarKey, 'v> =
            match elaborated with
            | ValueSome _ -> entries |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq
            | ValueNone -> Map.empty

        {
            Decls = EqArray.ofList decls
            // Published later, ADDITIVELY, so an inline binding stays a decl here.
            InlineBodies = EqArray.empty
            // Slot order: the `SpecializationId`s the decls' edges carry index THIS array.
            Specializations = EqArray.ofArray specializations
            Diagnostics = List.ofSeq ctx.Diagnostics
            IntrinsicBindings = EqDict.ofSeq ctx.Types.IntrinsicBindings
            GlobalValueKeys = EqSet.ofSeq ctx.Bindings.GlobalValueKeys
            Modules = ModuleDeclarations.declaredInFile ctx.Types
            ModuleMembers = emptyIfDegraded ctx.Bindings.ModuleMembers
            // Filled by the Pipeline once escape analysis has run.
            ClosureReprs = Map.empty
            FunVerdicts = Map.empty
            GenericFnSchemes = emptyIfDegraded (ctx.GenericFnSchemes.AsDictionary())
            Accessibility = EqDict.ofSeq ctx.Bindings.Accessibility
        }
