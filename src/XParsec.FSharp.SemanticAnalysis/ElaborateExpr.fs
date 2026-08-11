namespace XParsec.FSharp.SemanticAnalysis

open System
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.ElaborateLiterals
open XParsec.FSharp.SemanticAnalysis.ElaborateNominals
open XParsec.FSharp.SemanticAnalysis.ElaborateObjArgs
open XParsec.FSharp.SemanticAnalysis.ElaborateCalls
open XParsec.FSharp.SemanticAnalysis.ElaborateResolve
open XParsec.FSharp.SemanticAnalysis.ElaboratePatterns
open XParsec.FSharp.SemanticAnalysis.ElaborateExprArgs

/// Expression translation for the Elaborate pass: the recursive `CST -> TExpr`
/// projection.
module internal ElaborateExpr =

    let rec translateExpr (ctx: PassContext) (e: Expr<SyntaxToken>) : TExpr =
        let key = CstKeys.ofExpr e
        let ty = typeOfKey ctx key
        // The source anchor for every node this CST expression projects to.
        let tok = CstKeys.firstTokenOfExpr e

        match e with
        | Expr.Const c -> TExpr.Const(parseConst ctx c, ty, tok)
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
            li.Idents.Length > 1
            && ctx.Bindings.Binding.ContainsKey(NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent)
            ->
            // `r.X` / `r.X.Y` parsed as ONE multi-segment LongIdent: the anchor is a local
            // binding, the rest field accesses. A final *external* instance member
            // (`e.Current` on `IEnumerator<'T>`) is in `ExternalAccess` under this key.
            ElaborateIdents.translateLongIdentFieldChain ctx li ty (ctx.Resolution.ExternalAccess.TryGetValue key) tok
        // Static member on an *external* type reached through a folded LongIdent
        // (`System.Console.Out`, `Console.Out`), recorded in `ExternalAccess`.
        // Always static, so there is no object argument (`ValueNone`).
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) & ExternalAccess ctx info when li.Idents.Length >= 2 ->
            let memberName = ctx.NameOf li.Idents.[li.Idents.Length - 1]
            TExpr.ExternalMember(ValueNone, info.Key, memberName, info.Storage, ty, tok)
        // `E.C1` — an enum-case access (project-local or external). Enum cases are
        // static fields on the enum type, so this lowers to `StaticFieldGet`; the
        // case's underlying literal stays on the frozen `TTypeKind.Enum` case table.
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(li & EnumCaseAccess ctx ty enumKey)) ->
            TExpr.StaticFieldGet(SymbolKey.Type enumKey, ctx.NameOf li.Idents.[1], ty, tok)
        | Expr.New(typ = t; expr = argExpr) -> translateNew ctx key t argExpr ty tok
        // Class-name-as-function application: `Point(3, 4)` parses as
        // `Expr.App (Ident Point, [EnclosedBlock(Tuple)])`.
        | Expr.App(ClassRef ctx className, args) ->
            mkNew ctx className key ty (peelCtorArgs (translateExpr ctx) args) tok
        | Expr.HighPrecedenceApp(funcExpr = ClassRef ctx className; argExpr = arg) ->
            mkNew ctx className key ty (peelOneArg (translateExpr ctx) arg) tok
        // Class instance method invocation: `r.M(args)` →
        // `App(DotLookup(r, ., M), args)`.
        | Expr.App(funcExpr = InstanceMethodCall ctx (r, declKey, memberName); argExprs = args) ->
            let objArg = translateExpr ctx r
            mkMethodCall ctx key objArg declKey memberName (peelCtorArgs (translateExpr ctx) args) ty tok
        | Expr.HighPrecedenceApp(funcExpr = InstanceMethodCall ctx (r, declKey, memberName); argExpr = arg) ->
            let objArg = translateExpr ctx r
            mkMethodCall ctx key objArg declKey memberName (peelOneArg (translateExpr ctx) arg) ty tok
        // `p.M(args)` parses as `App` / `HighPrecedenceApp` whose fn is
        // `LongIdent [p; M]`, because the parser folds the dot into the long ident
        // rather than emitting `DotLookup` when the anchor is a regular identifier.
        | Expr.App(
            funcExpr = Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(ClassTailMethod ctx (bindingSite, objArgTy, memberName)))
            argExprs = args) ->
            let objArg = TExpr.Var(bindingSite, objArgTy, tok)

            mkMethodCall
                ctx
                key
                objArg
                (nominalDeclKey ctx.Store objArgTy)
                memberName
                (peelCtorArgs (translateExpr ctx) args)
                ty
                tok
        | Expr.HighPrecedenceApp(
            funcExpr = Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(ClassTailMethod ctx (bindingSite, objArgTy, memberName)))
            argExpr = arg) ->
            let objArg = TExpr.Var(bindingSite, objArgTy, tok)

            mkMethodCall
                ctx
                key
                objArg
                (nominalDeclKey ctx.Store objArgTy)
                memberName
                (peelOneArg (translateExpr ctx) arg)
                ty
                tok
        // `r.f.…M(args)` — method call on a *multi-segment* object argument (e.g.
        // `this.Source.MoveNext()`), which `ClassTailMethod` (2-segment) misses. The
        // prefix LongIdent rebuilds the field-chain; the tail is the method.
        | Expr.App(
            funcExpr = Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(ClassChainMethod ctx (prefixLi, objArgTy, memberName)))
            argExprs = args) ->
            let objArg =
                ElaborateIdents.translateLongIdentFieldChain ctx prefixLi objArgTy ValueNone tok

            mkMethodCall
                ctx
                key
                objArg
                (nominalDeclKey ctx.Store objArgTy)
                memberName
                (peelCtorArgs (translateExpr ctx) args)
                ty
                tok
        | Expr.HighPrecedenceApp(
            funcExpr = Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(ClassChainMethod ctx (prefixLi, objArgTy, memberName)))
            argExpr = arg) ->
            let objArg =
                ElaborateIdents.translateLongIdentFieldChain ctx prefixLi objArgTy ValueNone tok

            mkMethodCall
                ctx
                key
                objArg
                (nominalDeclKey ctx.Store objArgTy)
                memberName
                (peelOneArg (translateExpr ctx) arg)
                ty
                tok
        // `x.M(args)` where `x`'s type is a typar coerced to a project-local
        // interface (`'T :> IFace`); the interface key is in `TyparInterfaceCall`.
        // `CallVia.Interface` makes codegen emit `constrained. <typar> callvirt`.
        | Expr.App(
            funcExpr = Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(TyparInterfaceMethod ctx (prefixLi,
                                                                                            objArgTy,
                                                                                            ifaceKey,
                                                                                            ifaceArgs,
                                                                                            memberName)))
            argExprs = args) ->
            let objArg =
                ElaborateIdents.translateLongIdentFieldChain ctx prefixLi objArgTy ValueNone tok

            mkInterfaceMethodCall
                ctx
                objArg
                ifaceKey
                ifaceArgs
                memberName
                (peelCtorArgs (translateExpr ctx) args)
                ty
                tok
        | Expr.HighPrecedenceApp(
            funcExpr = Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(TyparInterfaceMethod ctx (prefixLi,
                                                                                            objArgTy,
                                                                                            ifaceKey,
                                                                                            ifaceArgs,
                                                                                            memberName)))
            argExpr = arg) ->
            let objArg =
                ElaborateIdents.translateLongIdentFieldChain ctx prefixLi objArgTy ValueNone tok

            mkInterfaceMethodCall ctx objArg ifaceKey ifaceArgs memberName (peelOneArg (translateExpr ctx) arg) ty tok
        // `p.X` (property) parses as `Expr.LongIdentOrOp(LongIdent[p; X])` when
        // the anchor is a regular identifier.
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(ClassTailProperty ctx (bindingSite, objArgTy, memberName))) ->
            let objArg = TExpr.Var(bindingSite, objArgTy, tok)

            let key = LocalSymbolKey.ofProperty (nominalDeclKey ctx.Store objArgTy) memberName

            TExpr.PropertyGet(objArg, key, viaOfObjArg ctx objArg, ty, tok)
        | Expr.App(
            funcExpr = Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(StaticMethod ctx (declKey, memberName)))
            argExprs = args) -> mkStaticMethodCall ctx declKey memberName (peelCtorArgs (translateExpr ctx) args) ty tok
        | Expr.HighPrecedenceApp(
            funcExpr = Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(StaticMethod ctx (declKey, memberName)))
            argExpr = arg) -> mkStaticMethodCall ctx declKey memberName (peelOneArg (translateExpr ctx) arg) ty tok
        // `ClassName<'args>.Method args` — static call on an explicitly instantiated
        // generic class (`Set<'T>.Singleton value`). The `<'args>` makes the funcExpr
        // a `DotLookup` over a `TypeApp` rather than a folded `LongIdent`.
        | Expr.App(funcExpr = TypeAppStaticMember ctx (declKey, memberName, ClassMemberKind.Method); argExprs = args) ->
            mkStaticMethodCall ctx declKey memberName (peelCtorArgs (translateExpr ctx) args) ty tok
        | Expr.HighPrecedenceApp(
            funcExpr = TypeAppStaticMember ctx (declKey, memberName, ClassMemberKind.Method); argExpr = arg) ->
            mkStaticMethodCall ctx declKey memberName (peelOneArg (translateExpr ctx) arg) ty tok
        // `ClassName.X` — static property read (or method-as-value).
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(StaticMember ctx (declKey, memberName))) ->
            let key = LocalSymbolKey.ofProperty declKey memberName
            TExpr.StaticPropertyGet(key, ty, tok)
        | CtorRef ctx caseName ->
            // Bare or qualified ctor reference outside an App. The result type
            // distinguishes a nullary ctor (→ `UnionCons`) from a ctor-as-value
            // (`let f = Circle`, typed `TyFun(_, TyUnion _)` → `External`).
            match Unification.zonk ctx.Store ty with
            | TyUnion(_, _) -> TExpr.UnionCons(caseName, EqArray.empty, ty, tok)
            | _ -> TExpr.External(caseName, ValueNone, ty, tok)
        | Expr.Ident _
        | Expr.LongIdentOrOp _ -> ElaborateIdents.translateIdent ctx e key ty tok
        | Expr.App(CtorRef ctx caseName, args) ->
            // Ctor application: `Circle 1.0` / `Rectangle(2.0, 3.0)`. F# treats DU
            // arguments as ONE tuple; the TAST flattens it back to a per-field list,
            // so consumers see the ctor's declared arity directly.
            mkUnionCons ctx caseName ty (peelCtorArgs (translateExpr ctx) args) tok
        | Expr.HighPrecedenceApp(funcExpr = CtorRef ctx caseName; argExpr = arg) ->
            mkUnionCons ctx caseName ty (peelOneArg (translateExpr ctx) arg) tok
        // Printf *partial* — a fully-unapplied lowerable literal (`printfn "%d"`),
        // marked in `PrintfPartial`. Synthesise a closure `fun h1 … hn ->
        // Format(sink, …)` instead of the FSharp.Core cold path.
        | Expr.App(_, args) when ctx.PrintfPartial.ContainsKey key ->
            ElaboratePrintf.translatePrintfPartial ctx key args ty tok
        // A printf happy-path call, marked in `PrintfApp`, lowers to a `TExpr.Format`.
        | Expr.App(fn, args) when ctx.PrintfApp.ContainsKey key ->
            match ElaboratePrintf.translatePrintfFormat translateExpr ctx key args ty tok with
            | ValueSome node -> node
            | ValueNone -> ElaborateApply.translateApp translateExpr ctx fn args tok
        | Expr.App(fn, args) -> ElaborateApply.translateApp translateExpr ctx fn args tok
        | Expr.HighPrecedenceApp(funcExpr = fn; argExpr = arg) ->
            ElaborateApply.translateHighPrecedenceApp translateExpr ctx fn arg ty tok
        | Expr.InfixApp(left, _, right) -> ElaborateApply.translateInfix translateExpr ctx key left right ty tok
        | Expr.PrefixApp(_, operand) -> ElaborateApply.translatePrefix translateExpr ctx key operand ty tok
        | Expr.Fun(argumentPats = argPats; expr = body) -> translateFun ctx argPats body
        | Expr.LetOrUse(keyword = kw; bindings = bindings; body = body) -> translateLet ctx kw bindings body
        | Expr.EnclosedBlock(lParen = ParenKind.List _; expr = inner) ->
            translateListLikeLiteral ctx ty false (listLiteralItems inner) tok
        | Expr.EnclosedBlock(lParen = ParenKind.Array _; expr = inner) ->
            translateListLikeLiteral ctx ty true (listLiteralItems inner) tok
        | Expr.EnclosedBlock(expr = inner) -> translateExpr ctx inner
        | Expr.IfThenElse(condition = cond; thenExpr = thenE; elifBranches = elifs; elseBranch = elseB) ->
            translateIfThenElse ctx cond thenE elifs elseB ty tok
        | Expr.Tuple(exprs = items) ->
            TExpr.Tuple(EqArray.ofSeq (seq { for x in items -> translateExpr ctx x }), ty, tok)
        | Expr.Sequential(exprs = items) ->
            TExpr.Sequential(EqArray.ofSeq (seq { for x in items -> translateExpr ctx x }), ty, tok)
        // The annotation has no runtime representation, because it only constrained
        // types in Unification; the TAST carries the inferred type inline.
        | Expr.TypeAnnotation(expr = inner) -> translateExpr ctx inner
        // Casts carry the resolved node type (`ty`): the target type for `:>` /
        // `:?>`, `bool` for `:?`. Unification already validated the coercion.
        | Expr.StaticUpcast(expr = inner) -> TExpr.Upcast(translateExpr ctx inner, ty, tok)
        | Expr.DynamicDowncast(expr = inner) -> TExpr.Downcast(translateExpr ctx inner, ty, tok)
        | Expr.DynamicTypeTest(expr = inner) ->
            // `ty` is the `bool` result; the tested-against type was stashed by
            // Unification in `TypeTestTargets`, keyed by this node.
            let testTy =
                match ctx.Resolution.TypeTestTargets.TryGetValue key with
                | ValueSome t -> t
                | ValueNone -> failwithf "Elaborate: no recorded type-test target for %O" key

            TExpr.TypeTest(translateExpr ctx inner, testTy, ty, tok)
        | Expr.EmptyBlock(lParen = ParenKind.List _) -> translateListLikeLiteral ctx ty false [] tok
        | Expr.EmptyBlock(lParen = ParenKind.Array _) -> translateListLikeLiteral ctx ty true [] tok
        | Expr.EmptyBlock _ -> unitConst ctx e
        | Expr.While(condition = cond; body = body) ->
            TExpr.While(translateExpr ctx cond, translateExpr ctx body, ty, tok)
        | Expr.ForTo(ident = ident; startExpr = startE; endExpr = endE; body = body) ->
            let varKey = CstKeys.ofForToVar ident

            let node =
                TExpr.ForTo(
                    varKey,
                    ident,
                    translateExpr ctx startE,
                    translateExpr ctx endE,
                    translateExpr ctx body,
                    ty,
                    tok
                )

            // A loop variable is the one bound variable with no pattern node behind it, so its
            // spelling is recorded here rather than by `namedSimple`. `ident` and not the
            // node's own token, which is the `for` keyword.
            BoundVarKey.ofExpr node
            |> ValueOption.iter (fun b -> ctx.SetBoundVarName(b, ident))

            node
        | Expr.ForIn(pat = pat; enumerableExpr = src; body = body) -> translateForIn ctx key pat src body ty tok
        | Expr.String _ -> ElaborateStrings.translateString translateExpr ctx e ty tok
        | Expr.Match(matchExpr = scrutinee; rules = Rules(rules = rules)) ->
            TExpr.Match(translateExpr ctx scrutinee, translateRules ctx rules, ty, tok)
        | Expr.Function(rules = Rules(rules = rules)) -> translateMatchLambda ctx e rules ty tok
        | Expr.TryWith(expr = body; rules = Rules(rules = rules)) ->
            TExpr.TryWith(translateExpr ctx body, translateRules ctx rules, ty, tok)
        | Expr.TryFinally(tryExpr = body; finallyExpr = finallyE) ->
            TExpr.TryFinally(translateExpr ctx body, translateExpr ctx finallyE, ty, tok)
        | Expr.Assignment(leftExpr = left; rightExpr = right) ->
            ElaborateAccess.translateAssignment translateExpr ctx key left right ty tok
        | Expr.Record(fieldInitializers = inits) -> translateRecord ctx inits ty tok
        | Expr.RecordClone(expr = src; fieldInitializers = inits) -> translateRecordClone ctx src inits ty tok
        // Member access on an *external* type (static `Type.Member` or instance
        // `value.Member`), resolved through the provider and emitted as a keyed
        // `TExpr.ExternalMember`; a static access has no object argument.
        | Expr.DotLookup(expr = r; longIdentOrOp = LongIdentOrOp.LongIdent li) & ExternalAccess ctx info when
            li.Idents.Length = 1
            ->
            let memberName = ctx.NameOf li.Idents.[0]

            let objArg =
                if info.IsStatic then
                    ValueNone
                else
                    ValueSome(translateExpr ctx r)

            TExpr.ExternalMember(objArg, info.Key, memberName, info.Storage, ty, tok)
        // `ClassName<'args>.Prop` — static property read on an explicitly
        // instantiated generic class (`Set<'T>.Empty`). The `<'args>` only pinned
        // the instantiation in inference and is carried on `ty`.
        | TypeAppStaticMember ctx (declKey, memberName, ClassMemberKind.Property) ->
            let key = LocalSymbolKey.ofProperty declKey memberName
            TExpr.StaticPropertyGet(key, ty, tok)
        | Expr.DotLookup(expr = r; longIdentOrOp = LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
            ElaborateAccess.translateDotLookup translateExpr ctx r (ctx.NameOf li.Idents.[0]) ty tok
        | Expr.DynamicLookup(expr = r; ident = idTok) ->
            ElaborateAccess.translateDynamicLookup translateExpr ctx key r idTok ty tok
        | Expr.Null _ -> TExpr.Null(ty, tok)
        // A range reaches here only when the counted-`ForTo` for-in lowering did NOT
        // consume it: value position, a stepped range, or a non-simple loop bound variable.
        // All unsupported, because a range materialises no seq value in this compiler.
        | Expr.Range(fromExpr = a; toExpr = b) ->
            ctx.Report(tok, Kind.RangeNotFirstClassValue)
            TExpr.Range(translateExpr ctx a, None, translateExpr ctx b, ty, tok)
        | Expr.SteppedRange(fromExpr = a; stepExpr = s; toExpr = b) ->
            ctx.Report(tok, Kind.RangeNotFirstClassValue)
            TExpr.Range(translateExpr ctx a, Some(translateExpr ctx s), translateExpr ctx b, ty, tok)
        | Expr.IndexedLookup(expr = r; indexExpr = idx) ->
            ElaborateAccess.translateIndexedLookup translateExpr ctx key r idx ty tok
        | Expr.ILIntrinsic(instrParts = parts; args = args) -> translateIlIntrinsic ctx parts args ty tok
        | Expr.StaticMemberInvocation(membersign = msig; expr = argExpr) ->
            ElaborateApply.translateStaticMemberInvocation translateExpr ctx argExpr msig ty tok
        | Expr.LibraryOnlyStaticOptimization(defaultExpr = defaultE; clauses = clauses) ->
            translateStaticOptimization ctx key defaultE clauses ty tok
        // `value<'T>` — explicit type application on a VALUE reference (the
        // `TypeAppStaticMember` class-qualifier forms matched above). The `<'T>` only
        // pinned the instantiation in inference; forward to the inner reference.
        | Expr.TypeApp(expr = inner) -> translateExpr ctx inner
        | _ ->
            // TODO: extend as the subset grows.
            failwithf "Elaborate.translateExpr: TODO %A" e

    /// `new T(args)` — Unification stamps `ty` with the `TyClass`. The CST-side
    /// fallback is purely defensive for error paths where Unification couldn't
    /// pin the constructed type.
    and private translateNew
        (ctx: PassContext)
        (key: NodeKey)
        (t: Type<SyntaxToken>)
        (argExpr: Expr<SyntaxToken>)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr =
        let className =
            match Unification.zonk ctx.Store ty with
            // The namespace-qualified name (`System.Exception`), which is what a
            // codegen error message about an unresolvable `.ctor` prints.
            | TyClass(n, _) -> SymbolKeyOps.typeMetaName n
            | _ ->
                let rec nameOf t =
                    match t with
                    | Type.NamedType li
                    | Type.GenericType(longIdent = li) when li.Idents.Length >= 1 ->
                        ctx.NameOf li.Idents.[li.Idents.Length - 1]
                    | Type.ParenType(typ = inner) -> nameOf inner
                    | _ -> ""

                nameOf t

        mkNew ctx className key ty (peelOneArg (translateExpr ctx) argExpr) tok

    /// An integer-range source (`for i in a..b do`) lowers to a counted `ForTo`
    /// loop, as F# itself does, because a range materialises no `seq` to walk. Only a
    /// unit-step range bound to a *simple* bound variable; the rest take the enumerator path.
    and private translateForIn
        (ctx: PassContext)
        (key: NodeKey)
        (pat: Pat<SyntaxToken>)
        (src: Expr<SyntaxToken>)
        (body: Expr<SyntaxToken>)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr =
        let rangeBounds =
            match src with
            | Expr.Range(fromExpr = a; toExpr = b)
            | Expr.EnclosedBlock(expr = Expr.Range(fromExpr = a; toExpr = b)) -> ValueSome(a, b)
            | _ -> ValueNone

        let tpat = translatePat ctx pat

        match rangeBounds, tpat with
        | ValueSome(a, b), TPat.NamedSimple(varKey, _, identTok) ->
            TExpr.ForTo(varKey, identTok, translateExpr ctx a, translateExpr ctx b, translateExpr ctx body, ty, tok)
        | _ ->
            // How the source yields its enumerator was resolved by Unification and
            // stashed under this node's key; absent ⇒ the `Interface` path.
            let enumerator =
                match ctx.Resolution.ForInShape.TryGetValue key with
                | ValueSome shape -> shape
                | ValueNone -> ForInEnumeratorG.Interface

            TExpr.ForIn(tpat, translateExpr ctx src, translateExpr ctx body, enumerator, ty, tok)

    /// `function …` ~ `fun x -> match x with …`. The synthesised parameter
    /// has no source token, so mint a synthetic key under the
    /// function-keyword's offset for the Match scrutinee to reference.
    and private translateMatchLambda
        (ctx: PassContext)
        (e: Expr<SyntaxToken>)
        (rules: ImmutableArray<Rule<SyntaxToken>>)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr =
        let funcKey = CstKeys.ofExpr e

        let paramKey = NodeKey.ofSynthetic funcKey.Offset NodeKind.SynthLambdaBody

        let paramTy, resultTy =
            match ty with
            | TyFun(p, r) -> p, r
            | _ -> failwithf "Elaborate.Function: expected function type, got %A" ty

        let scrutinee = TExpr.Var(paramKey, paramTy, tok)
        let body = TExpr.Match(scrutinee, translateRules ctx rules, resultTy, tok)
        TExpr.Lambda(TPat.NamedSimple(paramKey, paramTy, tok), body, ty, tok)

    and private translateRecord
        (ctx: PassContext)
        (inits: ImmutableArray<FieldInitializer<SyntaxToken>>)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr =
        let fields =
            EqArray.ofSeq (
                seq {
                    for FieldInitializer(longIdent = li; expr = e) in inits ->
                        let idents = li.Idents
                        let name = ctx.NameOf idents.[idents.Length - 1]
                        let argT = translateExpr ctx e

                        let wrapped =
                            match recordFieldTy ctx ty name with
                            | ValueSome ft -> wrapObjArg ctx.Store ft argT
                            | ValueNone -> argT

                        name, wrapped
                }
            )

        TExpr.RecordCons(fields, ty, tok)

    and private translateRecordClone
        (ctx: PassContext)
        (src: Expr<SyntaxToken>)
        (inits: ImmutableArray<FieldInitializer<SyntaxToken>>)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr =
        let overrides =
            EqArray.ofSeq (
                seq {
                    for FieldInitializer(longIdent = li; expr = e) in inits ->
                        let idents = li.Idents
                        ctx.NameOf idents.[idents.Length - 1], translateExpr ctx e
                }
            )

        TExpr.RecordClone(translateExpr ctx src, overrides, ty, tok)

    and private translateIlIntrinsic
        (ctx: PassContext)
        (parts: ImmutableArray<StringPart<SyntaxToken>>)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr =
        let opCode = stitchIlInstruction ctx parts
        let tArgs = EqArray.ofSeq (seq { for a in args -> translateExpr ctx a })

        // `newarr` / `ldelem.any` carry one element-type operand, but the source `!0`
        // placeholder is unparsed tokens: recover the element from the node's declared
        // types. Normalised (`ldelem.any` → `ldelem`) to the spelling codegen reads.
        if opCode.StartsWith "newarr" then
            let elem =
                match Unification.zonk ctx.Store ty with
                | TyArray elem -> elem
                | other -> failwithf "Elaborate: 'newarr' result is not a rank-1 array: %A" other

            TExpr.ILIntrinsic("newarr", ValueSome elem, tArgs, ty, tok)
        elif opCode.StartsWith "ldelem" then
            TExpr.ILIntrinsic("ldelem", ValueSome(Unification.zonk ctx.Store ty), tArgs, ty, tok)
        elif opCode.StartsWith "stelem" then
            // `arr.[i] <- v`. The store's result is `unit`, so the element type is
            // recovered from the value operand (the 3rd arg: array, index, value),
            // not the node's result type as `ldelem` does.
            let elem = Unification.zonk ctx.Store (typeOfKey ctx (CstKeys.ofExpr args.[2]))
            TExpr.ILIntrinsic("stelem", ValueSome elem, tArgs, ty, tok)
        elif opCode.StartsWith "box" then
            // `box value` — the boxed type is the *argument's* static type (the
            // result is always `obj`), so recover it from the single value operand.
            // On a reference type `box` is a runtime no-op.
            let elem = Unification.zonk ctx.Store (typeOfKey ctx (CstKeys.ofExpr args.[0]))
            TExpr.ILIntrinsic("box", ValueSome elem, tArgs, ty, tok)
        elif opCode.StartsWith "ilzero" then
            // `Unchecked.defaultof<'T>` — `ilzero`'s result IS the defaulted `'T`, so
            // the operand type is the node's result type. The source `type ('T)` clause
            // is decorative; the result type is authoritative.
            TExpr.ILIntrinsic("ilzero", ValueSome(Unification.zonk ctx.Store ty), tArgs, ty, tok)
        else
            TExpr.ILIntrinsic(opCode, ValueNone, tArgs, ty, tok)

    /// Unification filed each clause's RESOLVED constraints under this construct's
    /// key, positionally aligned with `clauses`. A missing entry leaves a clause
    /// unconstrained, and expansion takes the first clause whose constraints hold, so
    /// with no entry at all the source's first clause wins.
    and private translateStaticOptimization
        (ctx: PassContext)
        (key: NodeKey)
        (defaultE: Expr<SyntaxToken>)
        (clauses: ImmutableArray<StaticOptimizationClause<SyntaxToken>>)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr =
        let resolved =
            match ctx.StaticOpt.TryGetValue key with
            | ValueSome v -> v
            | ValueNone -> EqArray.empty

        // Source order throughout, so any diagnostic a body raises is reported where it reads.
        let defaultT = translateExpr ctx defaultE
        let translated = ResizeArray(clauses.Length)

        for i in 0 .. clauses.Length - 1 do
            translated.Add
                {
                    Constraints = if i < resolved.Length then resolved.[i] else EqArray.empty
                    Body = translateExpr ctx clauses.[i].OptimizedExpr
                }

        TExpr.StaticOptimization(EqArray.ofSeq translated, defaultT, ty, tok)

    and private translateRules (ctx: PassContext) (rules: ImmutableArray<Rule<SyntaxToken>>) : EqArray<TMatchArm> =
        EqArray.ofSeq (
            seq {
                for r in rules do
                    match r with
                    | Rule.Rule(pat = pat; guard = guard; expr = body) ->
                        let guardT =
                            match guard with
                            | ValueSome(PatternGuard(expr = g)) -> ValueSome(translateExpr ctx g)
                            | ValueNone -> ValueNone

                        yield
                            {
                                Pat = translatePat ctx pat
                                Guard = guardT
                                Body = translateExpr ctx body
                            }
                    | _ -> ()
            }
        )

    /// Project `[…]` / `[|…|]` literals into a `Cons` / `Nil` chain; an array
    /// additionally routes through `Array.ofList`, so codegen has one lowering
    /// target. A degenerate element type falls back to a free `TyVar`.
    and private translateListLikeLiteral
        (ctx: PassContext)
        (literalTy: SemType)
        (isArray: bool)
        (items: Expr<SyntaxToken> list)
        (tok: SyntaxToken)
        : TExpr =
        let zonked = Unification.zonk ctx.Store literalTy

        let elemTy =
            match zonked with
            // An array literal's zonked type is `TyConst("[]", [elem])`; a list
            // literal's is `TyRecord` / `TyUnion`. Pull the element out of whichever.
            | TyConst(_, args) when args.Length = 1 -> args.[0]
            | TyRecord(_, args) when args.Length = 1 -> args.[0]
            | TyUnion(_, args) when args.Length = 1 -> args.[0]
            | _ -> TyVar(ctx.NewTypeVar())

        // A program-declared list union (via the `'T list = List<'T>` abbrev) builds
        // `[…]` from its own cases: nullary = empty terminator, binary = cons.
        // Otherwise, and for every array literal, the FSharp.Core `Cons` / `Nil`.
        let listTy, consName, nilName =
            match zonked with
            | LocalUnion ctx info when not isArray ->
                let nilCase = info.Cases |> Array.tryFind (fun c -> c.Fields.Length = 0)
                let consCase = info.Cases |> Array.tryFind (fun c -> c.Fields.Length = 2)

                match nilCase, consCase with
                | Some n, Some c -> zonked, c.Name, n.Name
                | _ -> TyRecord(RuntimeNames.fsharpCoreListKey, EqArray.singleton elemTy), "Cons", "Nil"
            // The external Vesper cons-list, whose cases are `Cons` / `Empty`. Being
            // *external* it is absent from `ctx.Types.Union`, so the user-union arm
            // above misses it; recognition of its key is shared with codegen.
            | TyUnion(listKey, _) when not isArray && RuntimeNames.isVesperListKey listKey -> zonked, "Cons", "Empty"
            | _ -> TyRecord(RuntimeNames.fsharpCoreListKey, EqArray.singleton elemTy), "Cons", "Nil"

        let listExpr =
            let nil = TExpr.UnionCons(nilName, EqArray.empty, listTy, tok)

            items
            |> List.foldBack (fun item acc ->
                TExpr.UnionCons(consName, EqArray.ofList [ translateExpr ctx item; acc ], listTy, tok)
            )
            <| nil

        if isArray then
            let arrayTy = TyConst(RuntimeNames.arrayKey 1, EqArray.singleton elemTy)
            // Codegen resolves `Array.ofList` against its target; the BCL-only path
            // recognises this function and emits the array directly (no FSharp.Core).
            let opName = RuntimeNames.arrayOfListName
            let opTy = TyFun(listTy, arrayTy)
            TExpr.App(TExpr.External(opName, ValueNone, opTy, tok), listExpr, arrayTy, tok)
        else
            listExpr

    and private translateIfThenElse
        (ctx: PassContext)
        (cond: Expr<SyntaxToken>)
        (thenE: Expr<SyntaxToken>)
        (elifs: ImmutableArray<ElifBranch<SyntaxToken>>)
        (elseB: ElseBranch<SyntaxToken> voption)
        (resultTy: SemType)
        (tok: SyntaxToken)
        : TExpr =
        // A missing else is `else ()` (F# spec): inference has already constrained
        // the branches and the whole expression to `unit`.
        let mutable nestedElse =
            match elseB with
            | ValueSome(ElseBranch(expr = e)) -> translateExpr ctx e
            // A synthesised `else ()` has no source token, so anchor at the `if`'s token.
            | ValueNone -> TExpr.Const(TConstValue.Unit, ctx.Intrinsics.Unit, tok)

        for i = elifs.Length - 1 downto 0 do
            let elifCond, elifThen =
                match elifs.[i] with
                | ElifBranch.Elif(condition = c; expr = e)
                | ElifBranch.ElseIf(condition = c; expr = e) -> c, e

            nestedElse <-
                TExpr.IfThenElse(translateExpr ctx elifCond, translateExpr ctx elifThen, nestedElse, resultTy, tok)

        TExpr.IfThenElse(translateExpr ctx cond, translateExpr ctx thenE, nestedElse, resultTy, tok)

    and private translateFun
        (ctx: PassContext)
        (argPats: ImmutableArray<Pat<SyntaxToken>>)
        (body: Expr<SyntaxToken>)
        : TExpr =
        let mutable result = translateExpr ctx body
        let mutable resultTy = typeOfKey ctx (CstKeys.ofExpr body)

        for i = argPats.Length - 1 downto 0 do
            let p = argPats.[i]
            let tpat = translatePat ctx p
            let pTy = typeOfKey ctx (CstKeys.ofPat p)
            let lamTy = TyFun(pTy, resultTy)
            // The lambda's source anchor is its parameter pattern's first token.
            result <- TExpr.Lambda(tpat, result, lamTy, CstKeys.firstTokenOfPat p)
            resultTy <- lamTy

        result

    and private translateLet
        (ctx: PassContext)
        (keyword: LetOrUseKeyword<SyntaxToken>)
        (bindings: ImmutableArray<Binding<SyntaxToken>>)
        (body: Expr<SyntaxToken> voption)
        : TExpr =
        let bodyExpr = CstWalk.requireLetBody body
        let mutable result = translateExpr ctx bodyExpr
        let mutable resultTy = typeOfKey ctx (CstKeys.ofExpr bodyExpr)

        // `use` / `use!` bind a disposable: the binding folds to a `TExpr.Use`
        // (codegen wraps the body in `try … finally Dispose()`), not a `TExpr.Let`.
        let isUse =
            match keyword with
            | LetOrUseKeyword.Use _
            | LetOrUseKeyword.UseBang _ -> true
            | LetOrUseKeyword.Let _
            | LetOrUseKeyword.LetBang _ -> false

        for i = bindings.Length - 1 downto 0 do
            let b = bindings.[i]

            // Drop a format-literal alias binding (`let fmt : Format<…> = "%d" in …`):
            // its value froze to a dead `New PrintfFormat`, since every use
            // const-propagates the literal. Fold it out, keeping the body.
            if not isUse && ctx.PrintfFormatLiterals.ContainsKey(CstKeys.ofPat b.pattern) then
                ()
            else

                let tpat = translatePat ctx b.pattern
                let valT = translateBinding ctx b
                // The let/use node's source anchor is its bound variable pattern's first token.
                let bindTok = CstKeys.firstTokenOfPat b.pattern

                result <-
                    if isUse then
                        // Unification records the bound variable's resolved disposal path under
                        // the binding pattern's key. Absent ⇒ it reported a
                        // `use`-over-non-disposable error; no backend lowers `Unresolved`.
                        let dispose =
                            match ctx.Resolution.UseDispose.TryGetValue(CstKeys.ofPat b.pattern) with
                            | ValueSome d -> d
                            | ValueNone -> Disposal.Unresolved

                        TExpr.Use(tpat, valT, result, dispose, resultTy, bindTok)
                    else
                        TExpr.Let(tpat, valT, result, resultTy, bindTok)

        result

    and translateBinding (ctx: PassContext) (b: Binding<SyntaxToken>) : TExpr =
        if b.argumentPats.IsEmpty then
            translateExpr ctx b.expr
        else
            // `let f x y = body` is `let f = fun x y -> body`.
            translateFun ctx b.argumentPats b.expr
