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

// The module-element walk + the top-level `Elaborate.run` entry point: CST →
// `TastFileG<SemType>`, inline-expanded, open typars quantified to `TyTypar` —
// all still `SemType`. This is NOT the `SemType → FrozenType` freeze (that is the
// `Freeze` module, the final pipeline step); renamed from `Freeze` to
// retire that naming bug. The expression / pattern projection lives in
// ElaborateExpr, type declarations in ElaborateTypeDecls, the typar cut in
// ElaborateTypars (all opened above).
//
// Invariant: side tables can be discarded after this returns. The TAST is
// sharable; the CST + side tables are scoped to one compilation.

module Elaborate =
    /// The `i`-th curried parameter of an elaborated `let`-body (a nest of
    /// `Lambda`s): its binder `NodeKey` and the lambda's body (the parameter's
    /// scope). `ValueNone` if the body has fewer than `i+1` lambdas, or the
    /// target parameter is not a simple name (a tuple-destructured parameter
    /// can't carry `[<CallAtMostOnce>]`).
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

    /// The binder an argument pattern introduces, in the REFERENCE domain the elaborated
    /// lambda nest names its parameter by. Used only to assert the positional alignment
    /// between an inline's `argumentPats` and that nest (`recordInlineParamAttrs`).
    let private argPatBinderKey (p: Pat<SyntaxToken>) : NodeKey voption =
        BinderKey.ofCstPat p |> ValueOption.map BinderKey.identity

    /// The `[<CallAtMostOnce>]` linearity contract: `k` is referenced AT MOST
    /// ONCE in `scope`, and (if once) that use is not under a lambda or loop — so
    /// substituting the argument at the use evaluates it at most once. Conditional
    /// branches / match arms are fine (they only *skip* the use, never repeat it),
    /// so they are not special-cased; `While`/`ForTo`/`ForIn` bodies (and a
    /// `While` condition) repeat, so a use there is rejected. `TastWalk.usesOf` is
    /// the shared depth-tracking walk: `[]` (unused) or `[0]` (one straight-line
    /// use) satisfies the contract.
    let private paramUsedAtMostOnce (k: NodeKey) (scope: TExpr) : bool =
        match TastWalk.usesOf k scope with
        | [] -> true
        | [ depth ] -> depth = 0
        | _ -> false

    /// Decode + validate the compiler attributes on an `inline` binding's
    /// parameters, recording them in `ctx.InlineParamAttrs` (keyed by the
    /// function-binder `NodeKey`) for `Passes.InlineExpansion`. Errors a
    /// `[<CallAtMostOnce>]` on a non-`inline` binding, a non-simple parameter, or
    /// one that violates the linearity contract. A no-op when no parameter carries
    /// a recognised attribute.
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
                            // The flag at position `i` (decoded from `argumentPats.[i]`)
                            // must validate against — and later be honoured at — the
                            // `i`-th curried lambda. The inliner's `peel` re-derives the
                            // same `i` from the lambda nest, so this attrs array and the
                            // nest must stay positionally aligned; assert the binder keys
                            // agree so a future reordering of either fails loudly here
                            // rather than silently mis-marking a parameter as lazy.
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

    /// Did the generaliser quantify this binding into a (non-empty) scheme? A
    /// *value* binding's free typars are only genuine method typars when the
    /// generaliser actually quantified them — i.e. an annotated generic value
    /// like `let empty: SetTree<'T> = null` (non-expansive, so generalised) — as
    /// opposed to a bare value-restricted `let n = null` (no scheme: its free var
    /// stays a metavar). Mirrors `inferBindingGroup`'s `shouldGeneralise` cut via
    /// the scheme it left in `ctx.Bindings.Scheme`.
    let private bindingWasGeneralised (ctx: PassContext) (b: Binding<SyntaxToken>) : bool =
        match ctx.Bindings.Scheme.TryGetValue(CstKeys.ofBinding b) with
        | ValueSome scheme -> not (List.isEmpty scheme.Quantified)
        | ValueNone -> false

    /// For a generalised binding, record its frozen typar
    /// BOUNDS, keyed by the binding's binder, onto `ctx.GenericFnSchemes`. Each
    /// `Coercion` bound is frozen as a `FrozenConstraint.Coercion(idx, target)`
    /// template over the METHOD typars: the target is remapped through the SAME
    /// `quantEnv` the body freezes with (so its typar leaves get the identical
    /// `FTTypar(Method, idx)` indices) and then `toFrozen`-converted. The constrained
    /// typar's `idx` is its position in `quantEnv`. A constraint whose typar or
    /// target is not (yet) a `quantEnv` method typar is dropped — only method-axis
    /// bounds are carried. Read by the codegen call-site phantom-typar solve.
    /// (A binding with no recorded scheme has no bounds and records nothing — its
    /// absence from the table is equivalent to an empty list; the emitted typar
    /// arity comes independently from `staticFnTypars`' body sweep.)
    ///
    /// The two keys in scope here are of different types on purpose: `binder` is the
    /// binding's FROZEN identity (what the scheme is filed under), while
    /// `CstKeys.ofBinding b` is the ANALYSIS key the generaliser's own scheme is looked up
    /// by — a `NodeKey`, and so unfileable here.
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
                                    // Freeze the target with the SAME typar env the body
                                    // uses, so its leaves carry matching method indices.
                                    let frozenTarget = toFrozen (remapDeclTypars ctx.Store quantEnv target)
                                    FrozenConstraint.Coercion(idx, frozenTarget)
                                | None -> ()
                            | _ -> ()
                    ]

                ctx.GenericFnSchemes.Set(binder, constraints)

    /// The name a module `let` is EMITTED under: its `[<CompiledName>]` (the IL
    /// boundary name, e.g. `Set.empty` ⇒ `SetModule.Empty`), falling back to the
    /// source name — matching the contract extractor's `compiledNameForVal`, so a
    /// separately-compiled consumer resolving `Set.empty` to `SetModule.Empty` finds the
    /// method this emits. Both the producer-internal call resolver (`SymbolProviders`)
    /// and codegen key off this same `Name`; the `[<Global>]` check reads it too, a
    /// restatement being a restatement of the EMITTED name.
    let private emittedNameOfBinding (ctx: PassContext) (b: Binding<SyntaxToken>) : string voption =
        memberNameOfBinding ctx b
        |> ValueOption.map (fun nm ->
            match VesperLibTypeTranslate.tryCompiledName ctx.Lexed b.attributes with
            | ValueSome cn -> cn
            | ValueNone -> nm
        )

    /// Record the binding's exportable identity on `holder` and its declared
    /// accessibility, returning the `SymbolKey` both are filed under. A head that
    /// introduces no binder (a destructuring `let (a, b) = p`) records nothing — there
    /// is no single value to name, so there is nothing to export and nothing downstream
    /// to look up.
    let private recordExportedBinding
        (ctx: PassContext)
        (holder: ModuleHolder)
        (b: Binding<SyntaxToken>)
        (emittedName: string voption)
        (binder: BinderKey voption)
        : SymbolKey voption =
        match emittedName, binder with
        | ValueSome compiledNm, ValueSome bk ->
            // The holder is the containment chain itself, so the binding's
            // `SymbolKey` is a direct construction downstream
            // (`ModuleBindingInfo.Key`), never a dotted-string re-parse.
            let info: ModuleBindingInfo = { Holder = holder; Name = compiledNm }
            ctx.Bindings.ModuleMembers.[bk] <- info
            // Capture the binding's declared accessibility under its own
            // `SymbolKey` (honestly — the file→file projection thresholds
            // it internal-or-better, the `.fsi` extractor public-only).
            ctx.Bindings.Accessibility.[info.Key] <- accessibilityOfToken b.access
            ValueSome info.Key
        | _ -> ValueNone

    /// The method-typar env a module `let` quantifies. A module-`let` compiled as a
    /// generic static method (or generic closure) carries its free typars as
    /// `TyTypar(Method, i)`. The index order is minted once here (Edge A order), but the
    /// cut itself is deferred to `freezeTypars` — `elaborate` leaves the head pattern,
    /// value body, and declared type in `TyVar` form and just pairs the decl with this
    /// env. A *function* binding (`TyFun` declared type) always quantifies. A *value*
    /// binding quantifies only when (a) the generaliser left it a non-empty scheme and
    /// (b) its free typars sit *inside a type constructor* (`let empty: SetTree<'T> =
    /// null` ⇒ `TyClass(SetTree, ['T])`, lowered to a generic method returning `ldnull :
    /// SetTree<!!0>`, which verifies). A *bare* free var — `let n = null` (`TyVar`) or
    /// `let x: 'T = …` — stays a value-restriction metavar: generalising it would emit
    /// `ldnull : !!0` over an unconstrained typar (no `class` constraint ⇒
    /// unverifiable), so it keeps its `TyVar` representation.
    let private moduleLetQuantEnv
        (ctx: PassContext)
        (b: Binding<SyntaxToken>)
        (declTy: SemType)
        : (TyVarId * SemType) list =
        // The binding's explicit `<'b,'a>` typars, in source order with their
        // inference-seeded roots (recorded by `inferBinding` while the transient
        // `TyparScope` was live). `canonical` orders these first, the F# rule.
        let declaredTypars =
            match ctx.Bindings.DeclaredTypars.TryGetValue(CstKeys.ofBinding b) with
            | ValueSome ds -> ds
            | ValueNone -> []

        // An INLINE binding quantifies unconditionally — every shape gate below is
        // about EMISSION, and an inline binding is never emitted. It is a TEMPLATE:
        // `Freeze` publishes it as vocabulary and a consumer thaws + substitutes it per
        // call site, so its free typars are its template parameters and must be named on
        // a self-describing axis (`TyTypar(Method, i)` ⇒ `FTTypar`), not left as roots
        // only this compilation's `UnionFind` can explain.
        //
        // The value-restriction arm is exactly where that bites:
        // `let inline defaultof<'T> : 'T = (# "ilzero" … #)` has a declTy that zonks to a
        // bare `TyVar`, so it would fall into `| TyVar _ -> []` and reach freeze with an
        // unmapped root — which is not a metavar leak (its scheme DID quantify it) but
        // has no binder freeze can honestly name, so it would degrade to `FTUnknown`.
        // Its verifiability rationale does not apply either: no `ldnull : !!0` is ever
        // emitted for a template.
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

    /// Translate one module-level `let` binding to its decl + the typar env it
    /// quantifies. `ValueNone` for an E1 format-literal alias binding (`let fmt :
    /// Format<…> = "%d"`): its value froze to a `New PrintfFormat` that is dead — every
    /// use const-propagates the literal (`PrintfFormatLiterals`), and the self-host
    /// contract has no cold runtime for a format value, so nothing reads it. (A
    /// genuinely dynamic read is E2, rejected upstream.) Eliding it here keeps `New
    /// PrintfFormat` off codegen — and it reaches the frozen tree as no declaration at
    /// all, so it introduces NO binder either: recording one would file a side-table
    /// entry against a definition site the tree does not contain, which the frozen
    /// binder pool (`TastPools.toPools`) faults on.
    let private translateModuleLet
        (ctx: PassContext)
        (holder: ModuleHolder)
        (b: Binding<SyntaxToken>)
        : (TDecl * (TyVarId * SemType) list) voption =
        let tpat = translatePat ctx b.headPat
        let elided = ctx.PrintfFormatLiterals.ContainsKey(CstKeys.ofPat b.headPat)

        // The identity this binding contributes to the FROZEN side tables
        // (`ModuleMembers`, `GenericFnSchemes`, `BindingTyparArities`): the binder its
        // already-translated head pattern introduces (`BinderKey.ofPat`, which is also
        // where the shapes that introduce none are enumerated), or `ValueNone` — in which
        // case the tables below record nothing at all, every reader of them
        // (`FrozenSignature`, `HolderPlan`) looking up a simple binder.
        //
        // Read off the TRANSLATED head, never the CST binding: the analysis identity
        // `CstKeys.ofBinding b` addresses the head PATTERN node, which for a wrapped head
        // (`let (x) = …`, `let (x: int) = …`) is a node `ElaboratePatterns.translatePat`
        // ERASES.
        let binder = if elided then ValueNone else BinderKey.ofPat tpat

        let emittedName = emittedNameOfBinding ctx b
        let exportedKey = recordExportedBinding ctx holder b emittedName binder

        let valT = translateBinding ctx b
        let declTy = typeOfKey ctx (CstKeys.ofBinding b)

        // `[<Global>]`-ness belongs to the VALUE the binder names, so it is filed
        // under that identity beside the accessibility, not on the decl node.
        Attributes.declareGlobalBinding ctx b emittedName exportedKey valT

        // Decode + validate compiler parameter attributes
        // (`[<CallAtMostOnce>]`) for an inline binding, recording them
        // for `Passes.InlineExpansion`. Keyed by the function binder.
        match tpat with
        | TPat.NamedSimple(binderKey, _, _) -> recordInlineParamAttrs ctx b binderKey valT
        | _ -> ()

        let quantEnv = moduleLetQuantEnv ctx b declTy

        // Record the binding's frozen typar bounds using THIS `quantEnv` (the same env
        // `freezeTypars` freezes the body with, so the bounds' typar indices line up).
        // Read by the call-site phantom-typar solve (`EmitCall`). Both this and the arity
        // below are filed under the binding's FROZEN identity: the binding's typar-axis
        // width is a property of the value the binder names, so a binder-less head (`let
        // (a, b) = p`, `let _ = e`) has nowhere to put it — and nothing to read it, such a
        // binding never being a callable.
        match binder with
        | ValueSome bk ->
            recordGenericFnScheme ctx b bk quantEnv

            // The binding's typar-axis WIDTH at this single index-minting point
            // (`quantEnv` IS the method-axis order), keyed the same as `ModuleMembers`.
            // The frozen→provider projection reads it for `ExternalSymbol.TyparArity`.
            ctx.Bindings.BindingTyparArities.[bk] <- List.length quantEnv
        | ValueNone -> ()

        if elided then
            ValueNone
        else
            ValueSome(TDecl.Let(tpat, valT, b.inlineToken.IsSome, declTy), quantEnv)

    /// `c` is the element's declaring containment — the `namespace` group plus the
    /// `module`s it is nested in. EVERY module-level `let` in it records its `NodeKey` →
    /// `ModuleBindingInfo`, so every one of them has an exportable identity: a binding
    /// inside a `module` is held by that module (and emits as a named public static method
    /// on its holder, `ListModule::fold`), a top-level one by the namespace the file
    /// declares. The holder is `ctx.CurrentHolder` = `ModuleRules.holderChain` — the SAME
    /// chain builder the type-key mint reads (`localTypeHolder`), so a binding and a type
    /// declared in one module are held by the same module key, nesting included.
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
                        // Capture the type's declared accessibility under its own key,
                        // so the file→file projection can drop a `type private T`.
                        ctx.Bindings.Accessibility.[SymbolKey.Type tdecl.TypeKey] <-
                            accessibilityOfToken (typeDefnAccessToken td)

                        yield result
                    | Some result -> yield result
                    | None -> ()
            ]
        | _ -> []

    /// The first half of the split Elaborate pass: translate
    /// the CST to a `TExpr` tree whose `.ty` fields are zonk'd `SemType`, still
    /// `TyVar`-carrying (no `TyTypar`). Each decl is paired with the typar `env`
    /// it quantifies — the declaring / method / static-fn typar roots, collected at
    /// this single index-minting point. `freezeTypars` consumes that `env` to make
    /// the `TyVar → TyTypar` cut. (A later change will slot the inline-expansion pass
    /// between `elaborate` and the freeze cut, where `zonk` / union-find are native;
    /// today nothing runs between them and the output is byte-identical to the old
    /// fused pass.)
    let elaborate (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : (TDecl * (TyVarId * SemType) list) list =
        // The SAME flattened walk NameResolution and Unification take — a nested
        // `module Foo = …` surfaces its body flat, in source order, with the containment
        // extended (a binding at any depth is held by the whole chain of modules it is
        // written in). Sharing the walk is what makes `EnterElement` reach here: the by-name
        // reads lowering still makes (a class reference, a union-case head, an enum case)
        // must speak from the module AND the `open`s they are written under, and only the
        // walk knows those.
        CstWalk.walkModuleTreeWith ctx.NameOf ctx.Resolution.AmbientOpenScope (fun _ _ -> ()) file
        |> List.collect (fun w ->
            ctx.EnterElement w
            translateModuleElem ctx w.Containment w.Elem
        )

    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : TastFile =
        // Split pass: `elaborate` produces the
        // `TyVar`-carrying tree + per-decl typar envs; the `InlineExpansion` pass
        // then expands module-level inline call sites *before* the cut (where
        // `zonk` / union-find are native); `freezeTypars` makes the
        // `TyVar → TyTypar` cut on each.
        //
        // Cross-file inline bodies ride `ctx.Provider` directly: a published body sits ON
        // the resolved entry (`ExternalSymbol.InlineBody` / `ExternalMember.InlineBody`),
        // reached by the same `SymbolKey` the use-site node carries. Frozen, so
        // `InlineExpansion` thaws it into this file's own cells before splicing.
        let elaborateDecls () =
            let elaborated = elaborate ctx file

            // The inline VOCABULARY is snapshotted HERE, ahead of the expansion walk and
            // under the same typar cut. `InlineExpansion` walks a template like any other
            // decl (it is emitted as an ordinary function), and that walk resolves
            // static-opt clauses and trait calls against the types in scope AT THE
            // DEFINITION — which for an `^T`-constrained template is nothing. A consumer
            // splicing such a body must resolve them against ITS operand types, so what is
            // published has to be the tree the walk never saw.
            for (d, env) in elaborated do
                match d with
                | TDecl.Let(TPat.NamedSimple(k, _, _), _, true, _) ->
                    ctx.InlineTemplates.[k] <- freezeTypars ctx.Store env d
                | _ -> ()

            let expanded = InlineExpansion.run ctx elaborated

            // The typar cut over the table's entries, under the union of every decl's env.
            // A union is unambiguous and not a compromise: an env keys on the ROOT `TyVarId`
            // a decl generalized, and no two decls generalize one root — so an entry whose
            // grounding mentions a consuming binding's typar is remapped by that binding's
            // marker and by no other. An entry ground to nominals meets no key at all.
            //
            // Entries are cut here and not in the pass because the cut is `Elaborate`'s own
            // boundary: what leaves this function is `TyTypar`-shaped in every tree it
            // carries, and an entry is one of them.
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

        // Elaborate assumes well-typed input: it asserts its invariants with `failwith`
        // (it never diagnoses). Under an already-diagnosed type error — malformed
        // source (`let fmt : Format<int -> string> = "%d %s"`, an arity/type mismatch) —
        // an invariant may not hold, and a raw `failwith` would abort the whole
        // compilation. Malformed source is expected input, not a reason to throw: when
        // inference has ALREADY recorded an error the program will not be code-generated,
        // so degrade elaboration to diagnostics-only (drop the decls, keep the errors)
        // rather than crash. With NO prior error, elaboration runs unguarded, so a
        // `failwith` on well-formed input still surfaces loudly as the compiler bug it is.
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
            // The inline vocabulary is `Freeze`'s to publish: that is where a template
            // becomes `FrozenType`. Publication is ADDITIVE and takes nothing out of
            // `Decls` (`TastFileG.InlineBodies`) — an inline binding stays a decl in both
            // domains, which is where the same-file splice (`Passes.InlineExpansion`)
            // reads it and where both backends emit it as an ordinary module function.
            InlineBodies = EqArray.empty
            // The resolved-specialization table `InlineExpansion.run` built, in slot order —
            // the `SpecializationId`s the decls' edges carry index THIS array. It has to
            // travel with the decls that name it: an edge whose entry did not survive the
            // freeze names nothing, and placement is deferred to emission.
            Specializations = EqArray.ofArray specializations
            Diagnostics = List.ofSeq ctx.Diagnostics
            // Snapshot so the backend can key the emitted IL type off the representation
            // string without the PassContext. The KEY-addressed table, not its by-name
            // twin: the backend holds a resolved canon key, and a display name cannot say
            // which type it names.
            IntrinsicReprKeys = System.Collections.Generic.Dictionary(ctx.Types.IntrinsicReprKeys)
            // The `[<Global>]` bindings, snapshotted on the same identity axis: the JS
            // backend reads it to emit no definition for a value that IS a target global.
            GlobalValueKeys = System.Collections.Generic.HashSet(ctx.Bindings.GlobalValueKeys)
            // Snapshot the named-module placements: the backend keys
            // off a binding's identity to emit it on its holder type.
            ModuleMembers = ctx.Bindings.ModuleMembers |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq
            // The closure stack/heap verdict is filled in by the Pipeline after
            // `Regions.run` — escape analysis hasn't run at elaboration time.
            ClosureReprs = Map.empty
            // The value-struct closure verdicts — snapshotted by the
            // Pipeline from `ctx.FunVerdicts` alongside `ClosureReprs`.
            FunVerdicts = Map.empty
            // The per-binding frozen typar bounds, filled by
            // `recordGenericFnScheme` during `elaborate` (above) at the index-minting
            // point — snapshot here, not in the Pipeline, because the indices are
            // minted in this pass. Read by the call-site phantom-typar solve.
            GenericFnSchemes =
                ctx.GenericFnSchemes.AsDictionary()
                |> Seq.map (fun kv -> kv.Key, kv.Value)
                |> Map.ofSeq
            // The captured accessibility fact, snapshotted like `IntrinsicReprKeys`.
            Accessibility =
                System.Collections.Generic.Dictionary(ctx.Bindings.Accessibility)
                :> System.Collections.Generic.IReadOnlyDictionary<_, _>
            // The `ValRepr` grouping is a FREEZE product (it reads the frozen lambda
            // chain) — empty here, filled by `Freeze.run`.
            BindingTyparArities =
                ctx.Bindings.BindingTyparArities
                |> Seq.map (fun kv -> kv.Key, kv.Value)
                |> Map.ofSeq
        }
