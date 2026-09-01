namespace XParsec.FSharp.SemanticAnalysis

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
                | TPat.NamedSimple(k, _, _) -> ValueSome(k, inner)
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
        match ctx.Bindings.Scheme.TryGetValue(CstKeys.ofBinding b) with
        | ValueSome scheme -> not (List.isEmpty scheme.Quantified)
        | ValueNone -> false

    /// Each target is remapped through the SAME `quantEnv` the body freezes with, so its typar
    /// leaves carry matching method indices; a constraint outside that env is dropped.
    let private recordGenericFnScheme
        (ctx: PassContext)
        (b: Binding<SyntaxToken>)
        (boundVar: BoundVarKey)
        (quantEnv: (TyVarId * SemType) list)
        : unit =
        if not (List.isEmpty quantEnv) then
            match ctx.Bindings.Scheme.TryGetValue(CstKeys.ofBinding b) with
            | ValueNone -> ()
            | ValueSome scheme ->
                let methodIndexOf (tv: TyVarId) : int option =
                    match Unification.zonk ctx.Store (TyVar tv) with
                    | TyVar root ->
                        quantEnv
                        |> List.tryPick (fun (r, target) ->
                            match target with
                            | TyTypar(TyparAxis.Method, i) when r = root -> Some i
                            | _ -> None
                        )
                    | _ -> None

                let constraints =
                    [
                        for (tv, sc) in scheme.Constraints do
                            match sc.Kind with
                            | SemanticConstraintKind.Coercion target ->
                                match methodIndexOf tv with
                                | Some idx ->
                                    let frozenTarget = toFrozen (remapDeclTypars ctx.Store quantEnv target)
                                    FrozenConstraint.Coercion(idx, frozenTarget)
                                | None -> ()
                            | _ -> ()
                    ]

                ctx.GenericFnSchemes.Set(boundVar, constraints)

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

    /// A function binding always quantifies, a value binding only when generalised AND its
    /// free typars sit inside a type constructor: a bare free var would emit `ldnull : !!0`
    /// over an unconstrained typar, which does not verify.
    let private moduleLetQuantEnv
        (ctx: PassContext)
        (b: Binding<SyntaxToken>)
        (declTy: SemType)
        : (TyVarId * SemType) list =
        let declaredTypars =
            match ctx.Bindings.DeclaredTypars.TryGetValue(CstKeys.ofBinding b) with
            | ValueSome ds -> ds
            | ValueNone -> []

        // An inline binding is never emitted, so no emission gate applies: it is a TEMPLATE
        // whose free typars must be named on a self-describing axis or freeze to `FTUnknown`.
        if b.inlineToken.IsSome then
            mkMethodQuantEnv ctx.Store declaredTypars declTy
        else
            match Unification.zonk ctx.Store declTy with
            | TyFun _ -> mkMethodQuantEnv ctx.Store declaredTypars declTy
            // A bare free var is value-restricted, so never a method typar.
            | TyVar _
            | TyTypar _ -> []
            | _ when bindingWasGeneralised ctx b -> mkMethodQuantEnv ctx.Store declaredTypars declTy
            | _ -> []

    /// `ValueNone` for a format-literal alias, whose `New PrintfFormat` value is dead: it
    /// reaches the frozen tree as neither a declaration nor a bound variable.
    let private translateModuleLet
        (ctx: PassContext)
        (container: ModuleContainer)
        (b: Binding<SyntaxToken>)
        : (TDecl * (TyVarId * SemType) list) voption =
        let tpat = translatePat ctx b.pattern
        let elided = ctx.PrintfFormatLiterals.ContainsKey(CstKeys.ofPat b.pattern)

        // Read off the TRANSLATED pattern, never the CST binding: the analysis identity
        // addresses a pattern node that `translatePat` erases for `let (x: int) = …`.
        let boundVar = if elided then ValueNone else BoundVarKey.ofPat tpat

        let declTy = typeOfKey ctx (CstKeys.ofBinding b)
        let quantEnv = moduleLetQuantEnv ctx b declTy

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

        let valT = translateBinding ctx b

        Attributes.declareGlobalBinding ctx b emittedName exportedKey valT
        Attributes.declareImportBinding ctx b emittedName exportedKey valT

        match tpat with
        | TPat.NamedSimple(boundVarKey, _, _) -> recordInlineParamAttrs ctx b boundVarKey valT
        | _ -> ()

        // A bound-variable-less pattern has nowhere to file the typar-axis width.
        match boundVar with
        | ValueSome bk ->
            recordGenericFnScheme ctx b bk quantEnv
            ctx.Bindings.BindingTyparArities.[bk] <- List.length quantEnv
        | ValueNone -> ()

        if elided then
            ValueNone
        else
            ValueSome(TDecl.Let(tpat, valT, b.inlineToken.IsSome, declTy), quantEnv)

    let private translateModuleElem
        (ctx: PassContext)
        (m: ModuleElem<SyntaxToken>)
        : (TDecl * (TyVarId * SemType) list) list =
        let container = ctx.CurrentContainer

        match m with
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bindings)) ->
            [
                for b in bindings do
                    match translateModuleLet ctx container b with
                    | ValueSome decl -> yield decl
                    | ValueNone -> ()
            ]
        | ModuleElem.Expression e ->
            let eT = translateExpr ctx e
            [ TDecl.Expression(eT, typeOfKey ctx (CstKeys.ofExpr e)), [] ]
        | ModuleElem.Type defs ->
            [
                for td in defs do
                    match tryTypeDecl ctx td with
                    | Some((TDecl.Type tdecl, _) as result) ->
                        // So the file→file projection can drop a `type private T`.
                        ctx.Bindings.Accessibility.[SymbolKey.Type tdecl.TypeKey] <-
                            accessibilityOfToken (typeDefnAccessToken td)

                        yield result
                    | Some result -> yield result
                    | None -> ()
            ]
        | _ -> []

    /// CST → a `TExpr` tree whose `.ty` fields are zonk'd `SemType`, still `TyVar`-carrying.
    /// Each decl is paired with the typar env it quantifies.
    let elaborate (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : (TDecl * (TyVarId * SemType) list) list =
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
                | TDecl.Let(TPat.NamedSimple(k, _, _), _, true, _) ->
                    ctx.InlineTemplates.[k] <- freezeTypars ctx.Store env d
                | _ -> ()

            let expanded = InlineExpansion.run ctx elaborated

            // No two decls generalize one root, so this union is unambiguous.
            let env = expanded.Decls |> List.collect snd

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
            BindingTyparArities = emptyIfDegraded ctx.Bindings.BindingTyparArities
        }
