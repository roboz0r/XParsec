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

// The module-element walk + the `Elaborate.run` entry point: CST → `TastFileG<SemType>`,
// inline-expanded, open typars quantified to `TyTypar`. NOT the `SemType → FrozenType`
// freeze. Side tables can be discarded once this returns; the TAST alone is sharable.

module Elaborate =
    /// `ValueNone` below `i+1` lambdas, or where the parameter is not a simple name — a
    /// destructured parameter can't carry `[<CallAtMostOnce>]`.
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

    /// In the REFERENCE domain the elaborated lambda nest names its parameter by.
    let private argPatBinderKey (p: Pat<SyntaxToken>) : NodeKey voption =
        BinderKey.ofCstPat p |> ValueOption.map BinderKey.identity

    /// The `[<CallAtMostOnce>]` contract: at most one use of `k` in `scope`, and not under a
    /// lambda or loop. A conditional only skips a use, never repeats it.
    let private paramUsedAtMostOnce (k: NodeKey) (scope: TExpr) : bool =
        match TastWalk.usesOf k scope with
        | [] -> true
        | [ depth ] -> depth = 0
        | _ -> false

    /// Errors a `[<CallAtMostOnce>]` on a non-`inline` binding, a non-simple parameter, or
    /// one that violates the linearity contract.
    let private recordInlineParamAttrs
        (ctx: PassContext)
        (b: Binding<SyntaxToken>)
        (binderKey: NodeKey)
        (valT: TExpr)
        : unit =
        if not b.argumentPats.IsEmpty then
            let attrs = [| for p in b.argumentPats -> Attributes.paramAttrsOfArgPat ctx p |]

            if attrs |> Array.exists (fun a -> not a.IsDefault) then
                if not b.inlineToken.IsSome then
                    ctx.Report(
                        (CstKeys.siteOfBinding b).Tok,
                        Kind.Message
                            "A parameter attribute such as [<CallAtMostOnce>] is only valid on a parameter of an 'inline' function"
                    )
                else
                    attrs
                    |> Array.iteri (fun i a ->
                        if a.CallAtMostOnce then
                            // This array and the lambda nest must stay positionally aligned:
                            // the inliner re-derives the same `i` from the nest.
                            match nthLambdaParam valT i with
                            | ValueSome(pk, _) when ValueSome pk <> argPatBinderKey b.argumentPats.[i] ->
                                failwithf
                                    "Elaborate.recordInlineParamAttrs: parameter %d binder key %A does not match its argument pattern (alignment invariant broken)"
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

                    ctx.InlineParamAttrs.[binderKey] <- attrs

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
        (binder: BinderKey)
        (quantEnv: (TyVarId * SemType) list)
        : unit =
        if not (List.isEmpty quantEnv) then
            match ctx.Bindings.Scheme.TryGetValue(CstKeys.ofBinding b) with
            | ValueNone -> ()
            | ValueSome scheme ->
                // Index of `tv`'s zonked root in `quantEnv` (its `TyTypar(Method, i)`).
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

                ctx.GenericFnSchemes.Set(binder, constraints)

    /// `[<CompiledName>]`, else the source name. Matches the contract extractor, so a
    /// consumer resolving `Set.empty` to `SetModule.Empty` finds the method this emits.
    let private emittedNameOfBinding (ctx: PassContext) (b: Binding<SyntaxToken>) : string voption =
        memberNameOfBinding ctx b
        |> ValueOption.map (fun nm ->
            match VesperLibTypeTranslate.tryCompiledName ctx.Lexed b.attributes with
            | ValueSome cn -> cn
            | ValueNone -> nm
        )

    /// A head introducing no binder (`let (a, b) = p`) names no value, so records nothing.
    let private recordExportedBinding
        (ctx: PassContext)
        (holder: ModuleHolder)
        (b: Binding<SyntaxToken>)
        (emittedName: string voption)
        (binder: BinderKey voption)
        : SymbolKey voption =
        match emittedName, binder with
        | ValueSome compiledNm, ValueSome bk ->
            let info: ModuleBindingInfo = { Holder = holder; Name = compiledNm }
            ctx.Bindings.ModuleMembers.[bk] <- info
            // Captured honestly; each consumer applies its own threshold.
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
        // Source order; `canonical` orders these declared typars first, the F# rule.
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
            // A bare free var is value-restricted — never a method typar.
            | TyVar _
            | TyTypar _ -> []
            | _ when bindingWasGeneralised ctx b -> mkMethodQuantEnv ctx.Store declaredTypars declTy
            | _ -> []

    /// `ValueNone` for a format-literal alias, whose `New PrintfFormat` value is dead. It
    /// reaches the frozen tree as no declaration, so it introduces no binder either.
    let private translateModuleLet
        (ctx: PassContext)
        (holder: ModuleHolder)
        (b: Binding<SyntaxToken>)
        : (TDecl * (TyVarId * SemType) list) voption =
        let tpat = translatePat ctx b.headPat
        let elided = ctx.PrintfFormatLiterals.ContainsKey(CstKeys.ofPat b.headPat)

        // Read off the TRANSLATED head, never the CST binding: the analysis identity
        // addresses a head pattern node that `translatePat` erases for `let (x: int) = …`.
        let binder = if elided then ValueNone else BinderKey.ofPat tpat

        let emittedName = emittedNameOfBinding ctx b
        let exportedKey = recordExportedBinding ctx holder b emittedName binder

        let valT = translateBinding ctx b
        let declTy = typeOfKey ctx (CstKeys.ofBinding b)

        // `[<Global>]`-ness belongs to the VALUE the binder names, not to the decl node.
        Attributes.declareGlobalBinding ctx b emittedName exportedKey valT

        // Parameter attributes are keyed by the function binder.
        match tpat with
        | TPat.NamedSimple(binderKey, _, _) -> recordInlineParamAttrs ctx b binderKey valT
        | _ -> ()

        let quantEnv = moduleLetQuantEnv ctx b declTy

        // Both are filed under the binding's FROZEN identity: the typar-axis width belongs
        // to the value the binder names, so a binder-less head has nowhere to put it.
        match binder with
        | ValueSome bk ->
            recordGenericFnScheme ctx b bk quantEnv
            ctx.Bindings.BindingTyparArities.[bk] <- List.length quantEnv
        | ValueNone -> ()

        if elided then
            ValueNone
        else
            ValueSome(TDecl.Let(tpat, valT, b.inlineToken.IsSome, declTy), quantEnv)

    /// The exportable identity comes off `ctx.CurrentHolder` — the SAME chain builder the
    /// type-key mint reads, so a binding and a type in one module are held by the same key.
    let private translateModuleElem
        (ctx: PassContext)
        (c: DeclContainment<SyntaxToken>)
        (m: ModuleElem<SyntaxToken>)
        : (TDecl * (TyVarId * SemType) list) list =
        let holder = ctx.CurrentHolder

        match m with
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bindings)) ->
            [
                for b in bindings do
                    match translateModuleLet ctx holder b with
                    | ValueSome decl -> yield decl
                    | ValueNone -> ()
            ]
        | ModuleElem.Expression e ->
            let eT = translateExpr ctx e
            [ TDecl.Expression(eT, typeOfKey ctx (CstKeys.ofExpr e)), [] ]
        | ModuleElem.Type defs ->
            [
                for td in defs do
                    match tryTypeDecl ctx c td with
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
    /// Each decl is paired with the typar env it quantifies, minted at this single point.
    let elaborate (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : (TDecl * (TyVarId * SemType) list) list =
        // The SAME flattened walk NameResolution and Unification take: the by-name reads
        // lowering makes must speak from the module and `open`s they are written under.
        CstWalk.walkModuleTreeWith ctx.NameOf ctx.Resolution.AmbientOpenScope (fun _ _ -> ()) file
        |> List.collect (fun w ->
            ctx.EnterElement w
            translateModuleElem ctx w.Containment w.Elem
        )

    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : TastFile =
        // `InlineExpansion` runs before the typar cut, where `zonk` / union-find are native.
        // A cross-file body rides the provider entry frozen, thawed into this file's cells.
        let elaborateDecls () =
            let elaborated = elaborate ctx file

            // Snapshotted ahead of the expansion walk, which resolves static-opt clauses and
            // trait calls against the DEFINITION's types — nothing, for an `^T` template.
            for (d, env) in elaborated do
                match d with
                | TDecl.Let(TPat.NamedSimple(k, _, _), _, true, _) ->
                    ctx.InlineTemplates.[k] <- freezeTypars ctx.Store env d
                | _ -> ()

            let expanded = InlineExpansion.run ctx elaborated

            // An env keys on the ROOT `TyVarId` a decl generalized and no two decls
            // generalize one root, so this union is unambiguous rather than a compromise.
            let env = expanded.Decls |> List.collect snd

            let specializations =
                expanded.Specializations
                |> Array.map (fun (e: TSpecialization) ->
                    { e with
                        Decl = freezeTypars ctx.Store env e.Decl
                    }
                )

            let decls =
                expanded.Decls |> List.map (fun (d, env) -> freezeTypars ctx.Store env d)

            decls, specializations

        // Elaborate asserts with `failwith` rather than diagnosing. After a diagnosed error
        // nothing is code-generated, so degrade; unguarded otherwise, so a real bug surfaces.
        let hasErrors = ctx.Diagnostics |> Seq.exists Diagnostic.isError

        let decls, specializations =
            if hasErrors then
                try
                    elaborateDecls ()
                with _ ->
                    [], [||]
            else
                elaborateDecls ()

        {
            Decls = EqArray.ofList decls
            // Publishing is `Freeze`'s, and ADDITIVE — an inline binding stays a decl here.
            InlineBodies = EqArray.empty
            // Slot order: the `SpecializationId`s the decls' edges carry index THIS array.
            Specializations = EqArray.ofArray specializations
            Diagnostics = List.ofSeq ctx.Diagnostics
            // So the backend can key emitted IL off the repr without the `PassContext`.
            IntrinsicReprKeys = System.Collections.Generic.Dictionary(ctx.Types.IntrinsicReprKeys)
            // The JS backend emits no definition for a value that IS a target global.
            GlobalValueKeys = System.Collections.Generic.HashSet(ctx.Bindings.GlobalValueKeys)
            // The backend keys off a binding's identity to emit it on its holder type.
            ModuleMembers = ctx.Bindings.ModuleMembers |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq
            // Filled by the Pipeline after `Regions.run`; escape analysis hasn't run yet.
            ClosureReprs = Map.empty
            FunVerdicts = Map.empty
            // Snapshot here, not in the Pipeline: the indices are minted in this pass.
            GenericFnSchemes =
                ctx.GenericFnSchemes.AsDictionary()
                |> Seq.map (fun kv -> kv.Key, kv.Value)
                |> Map.ofSeq
            Accessibility =
                System.Collections.Generic.Dictionary(ctx.Bindings.Accessibility)
                :> System.Collections.Generic.IReadOnlyDictionary<_, _>
            BindingTyparArities =
                ctx.Bindings.BindingTyparArities
                |> Seq.map (fun kv -> kv.Key, kv.Value)
                |> Map.ofSeq
        }
