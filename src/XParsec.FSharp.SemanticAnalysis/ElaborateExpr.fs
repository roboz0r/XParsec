namespace XParsec.FSharp.SemanticAnalysis

open System
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.ElaborateLiterals
open XParsec.FSharp.SemanticAnalysis.ElaborateResolve
open XParsec.FSharp.SemanticAnalysis.ElaboratePatterns
open XParsec.FSharp.SemanticAnalysis.ElaborateExprArgs

/// Expression translation for the Elaborate pass: the recursive `CST -> TExpr`
/// projection. The companion `Elaborate` module (type-declaration surfacing + `run`)
/// opens this one for the entry points it projects from.
module internal ElaborateExpr =

    /// Diagnostic shared by both `Range` lowering arms below — a range that reached
    /// elaboration was NOT consumed by the counted-`ForTo` lowering, so it is an
    /// unsupported first-class use.
    [<Literal>]
    let private rangeNotFirstClassValue =
        "a range expression is only supported as the source of a 'for i in a..b do' counted loop; it has no first-class value"

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
            // `r.X` (or chained `r.X.Y`) parsed as a single multi-segment
            // LongIdent: head resolved as a local binding, rest field accesses.
            // If the final segment is an *external* instance member (e.g.
            // `e.Current` on a BCL `IEnumerator<'T>`), Unification recorded it in
            // `ExternalAccess` on this chain's key — pass it so the last step emits
            // a keyed `TExpr.ExternalMember` rather than a project-local `FieldGet`.
            ElaborateIdents.translateLongIdentFieldChain ctx li ty (ctx.Resolution.ExternalAccess.TryGetValue key) tok
        // Static member on an *external* type reached through a folded LongIdent
        // (`System.Console.Out`, `Console.Out`) — Unification resolved the prefix
        // as a type and recorded the member in `ExternalAccess`. Emit the same
        // keyed `TExpr.ExternalMember` as the generic `DotLookup` form; always
        // static, so the type-name receiver is dropped.
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) & ExternalAccess ctx info when li.Idents.Length >= 2 ->
            let memberName = ctx.NameOf li.Idents.[li.Idents.Length - 1]
            TExpr.ExternalMember(ValueNone, info.Key, memberName, info.Storage, ty, tok)
        // `E.C1` — an enum-case access (project-local OR external TS-manifest enum).
        // Enum cases ARE static fields on the enum type (the "cases as static
        // members" decision, mirroring CLR enum field access), so this lowers to
        // `StaticFieldGet(enumKey, caseName, …)`. The case's underlying literal is
        // NOT carried on the node — it lives on the frozen `TTypeKind.Enum` case
        // table (the single source of truth), which codegen reads off the decl by
        // `enumKey`. `EnumCaseAccess` resolves the key from the node's `TyEnum` type
        // (set by Unification for both local and external heads) or the local enum
        // registry on the error path — exclusive with the local-binding / class /
        // union heads handled elsewhere.
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(li & EnumCaseAccess ctx ty enumKey)) ->
            TExpr.StaticFieldGet(enumKey, ctx.NameOf li.Idents.[1], ty, tok)
        | Expr.New(typ = t; expr = argExpr) -> translateNew ctx t argExpr ty tok
        // Class-name-as-function application: `Point(3, 4)` parses as
        // `Expr.App (Ident Point, [EnclosedBlock(Tuple)])`.
        | Expr.App(ClassRef ctx className, args) -> mkNew ctx className ty (peelCtorArgs (translateExpr ctx) args) tok
        | Expr.HighPrecedenceApp(funcExpr = ClassRef ctx className; argExpr = arg) ->
            mkNew ctx className ty (peelOneArg (translateExpr ctx) arg) tok
        // Class instance method invocation: `r.M(args)` →
        // `App(DotLookup(r, ., M), args)`.
        | Expr.App(funcExpr = InstanceMethodCall ctx (r, declKey, memberName); argExprs = args) ->
            let receiver = translateExpr ctx r
            mkMethodCall ctx receiver declKey memberName (peelCtorArgs (translateExpr ctx) args) ty tok
        | Expr.HighPrecedenceApp(funcExpr = InstanceMethodCall ctx (r, declKey, memberName); argExpr = arg) ->
            let receiver = translateExpr ctx r
            mkMethodCall ctx receiver declKey memberName (peelOneArg (translateExpr ctx) arg) ty tok
        // `p.M(args)` parses as `App` / `HighPrecedenceApp` whose fn is
        // `Expr.LongIdentOrOp(LongIdent [p; M])` — the parser folds the dot into
        // the long ident rather than emitting `DotLookup` when the head is a
        // regular identifier. Fold to MethodCall.
        | Expr.App(
            funcExpr = Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(ClassTailMethod ctx (bindingSite,
                                                                                       receiverTy,
                                                                                       memberName)))
            argExprs = args) ->
            let receiver = TExpr.Var(bindingSite, receiverTy, tok)

            mkMethodCall
                ctx
                receiver
                (nominalDeclKey receiverTy)
                memberName
                (peelCtorArgs (translateExpr ctx) args)
                ty
                tok
        | Expr.HighPrecedenceApp(
            funcExpr = Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(ClassTailMethod ctx (bindingSite,
                                                                                       receiverTy,
                                                                                       memberName)))
            argExpr = arg) ->
            let receiver = TExpr.Var(bindingSite, receiverTy, tok)
            mkMethodCall ctx receiver (nominalDeclKey receiverTy) memberName (peelOneArg (translateExpr ctx) arg) ty tok
        // `head.f.…M(args)` — method call on a *multi-segment* receiver chain (e.g.
        // `this.Source.MoveNext()`), which `ClassTailMethod` (2-segment) misses. The
        // prefix LongIdent rebuilds the receiver field-chain; the tail is the method.
        | Expr.App(
            funcExpr = Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(ClassChainMethod ctx (prefixLi, receiverTy, memberName)))
            argExprs = args) ->
            let receiver =
                ElaborateIdents.translateLongIdentFieldChain ctx prefixLi receiverTy ValueNone tok

            mkMethodCall
                ctx
                receiver
                (nominalDeclKey receiverTy)
                memberName
                (peelCtorArgs (translateExpr ctx) args)
                ty
                tok
        | Expr.HighPrecedenceApp(
            funcExpr = Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(ClassChainMethod ctx (prefixLi, receiverTy, memberName)))
            argExpr = arg) ->
            let receiver =
                ElaborateIdents.translateLongIdentFieldChain ctx prefixLi receiverTy ValueNone tok

            mkMethodCall ctx receiver (nominalDeclKey receiverTy) memberName (peelOneArg (translateExpr ctx) arg) ty tok
        // `x.M(args)` where `x`'s type is a generic typar coerced to a project-local
        // interface (`'T :> IFace`, rung-3 Wall B). Unification recorded the
        // interface key in `TyparInterfaceCall`; dispatch via `CallVia.Interface` so
        // codegen emits `constrained. <typar> callvirt`.
        | Expr.App(
            funcExpr = Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(TyparInterfaceMethod ctx (prefixLi,
                                                                                            receiverTy,
                                                                                            ifaceKey,
                                                                                            ifaceArgs,
                                                                                            memberName)))
            argExprs = args) ->
            let receiver =
                ElaborateIdents.translateLongIdentFieldChain ctx prefixLi receiverTy ValueNone tok

            mkInterfaceMethodCall
                ctx
                receiver
                ifaceKey
                ifaceArgs
                memberName
                (peelCtorArgs (translateExpr ctx) args)
                ty
                tok
        | Expr.HighPrecedenceApp(
            funcExpr = Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(TyparInterfaceMethod ctx (prefixLi,
                                                                                            receiverTy,
                                                                                            ifaceKey,
                                                                                            ifaceArgs,
                                                                                            memberName)))
            argExpr = arg) ->
            let receiver =
                ElaborateIdents.translateLongIdentFieldChain ctx prefixLi receiverTy ValueNone tok

            mkInterfaceMethodCall ctx receiver ifaceKey ifaceArgs memberName (peelOneArg (translateExpr ctx) arg) ty tok
        // `p.X` (property) parses as `Expr.LongIdentOrOp(LongIdent[p; X])` when
        // the head is a regular identifier. Anything not a class property falls
        // to the chained FieldGet path below.
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(ClassTailProperty ctx (bindingSite, receiverTy, memberName))) ->
            let receiver = TExpr.Var(bindingSite, receiverTy, tok)

            let key =
                LocalSymbolKey.ofMember (nominalDeclKey receiverTy) memberName 0 MemberKind.Property

            TExpr.PropertyGet(receiver, key, viaOfReceiver ctx receiver, ty, tok)
        | Expr.App(
            funcExpr = Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(StaticMethod ctx (declKey, memberName)))
            argExprs = args) -> mkStaticMethodCall ctx declKey memberName (peelCtorArgs (translateExpr ctx) args) ty tok
        | Expr.HighPrecedenceApp(
            funcExpr = Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(StaticMethod ctx (declKey, memberName)))
            argExpr = arg) -> mkStaticMethodCall ctx declKey memberName (peelOneArg (translateExpr ctx) arg) ty tok
        // `ClassName<'args>.Method args` — static-method call on an explicitly
        // instantiated generic class (e.g. `Set<'T>.Singleton value`). The
        // `<'args>`-bearing receiver makes the funcExpr a `DotLookup` over a
        // `TypeApp` rather than a folded `LongIdent`; same `StaticMethodCall`
        // lowering as the folded `StaticMethod` arms above.
        | Expr.App(funcExpr = TypeAppStaticMember ctx (declKey, memberName, ClassMemberKind.Method); argExprs = args) ->
            mkStaticMethodCall ctx declKey memberName (peelCtorArgs (translateExpr ctx) args) ty tok
        | Expr.HighPrecedenceApp(
            funcExpr = TypeAppStaticMember ctx (declKey, memberName, ClassMemberKind.Method); argExpr = arg) ->
            mkStaticMethodCall ctx declKey memberName (peelOneArg (translateExpr ctx) arg) ty tok
        // `ClassName.X` — static property read (or method-as-value).
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(StaticMember ctx (declKey, memberName))) ->
            let key = LocalSymbolKey.ofMember declKey memberName 0 MemberKind.Property
            TExpr.StaticPropertyGet(key, ty, tok)
        | CtorRef ctx caseName ->
            // Bare or qualified ctor reference outside an App. v1 distinguishes
            // nullary ctor (→ `UnionCons`) from ctor-as-value (`let f = Circle`,
            // typed `TyFun(_, TyUnion _)` → External) by the result type.
            match Unification.zonk ty with
            | TyUnion(_, _) -> TExpr.UnionCons(caseName, EqArray.empty, ty, tok)
            // Function-typed ctor-as-value; codegen can eta-expand to a
            // UnionCons lambda.
            | _ -> TExpr.External(caseName, ValueNone, ty, tok)
        | Expr.Ident _
        | Expr.LongIdentOrOp _ -> ElaborateIdents.translateIdent ctx e key ty tok
        | Expr.App(CtorRef ctx caseName, args) ->
            // Ctor application: `Circle 1.0` or `Rectangle(2.0, 3.0)`. F# treats
            // DU arguments as a single tuple; the TAST flattens it back to a
            // per-field list (the same peel the class-ctor arms use) so consumers
            // see the ctor's declared arity directly.
            mkUnionCons ctx caseName ty (peelCtorArgs (translateExpr ctx) args) tok
        | Expr.HighPrecedenceApp(funcExpr = CtorRef ctx caseName; argExpr = arg) ->
            mkUnionCons ctx caseName ty (peelOneArg (translateExpr ctx) arg) tok
        // Printf *partial* — a fully-unapplied lowerable literal (`printfn "%d"`),
        // marked by `Unification.tryInferPrintfApp`. Synthesise a Vesper closure
        // `fun h1 … hn -> Format(sink, …)` (4a: emitted heap, dispatched via the
        // ordinary `Fun`2`::Invoke` path) instead of the FSharp.Core cold path. Must
        // precede the generic `App` projection below, like the happy-path arm.
        | Expr.App(_, args) when ctx.PrintfPartial.ContainsKey key ->
            ElaboratePrintf.translatePrintfPartial ctx key args ty tok
        // Printf happy-path call, marked by `Unification.tryInferPrintfApp`. Must
        // lower to a `TExpr.Format` *before* the generic `App` projection below runs.
        | Expr.App(fn, args) when ctx.PrintfApp.ContainsKey key ->
            // The marker only declines when a `%A` hole's argument type is one the
            // structural engine can't author (`TyUnknown` / type-level vocabulary) —
            // shapes the front end has already rejected with a diagnostic, so the
            // resulting `App` never reaches a (successful) codegen. There is no cold
            // printf recipe to fall back to: every lowerable form is a `TExpr.Format`.
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
        // The annotation has no runtime representation — it only constrained
        // types in Unification; the TAST carries the inferred type inline.
        | Expr.TypeAnnotation(expr = inner) -> translateExpr ctx inner
        // Casts carry the resolved node type (`ty`): the target type for
        // `:>` / `:?>`, and `bool` for `:?` — Unification validated the
        // coercion via `subsumes`, codegen emits the box / castclass / isinst.
        | Expr.StaticUpcast(expr = inner) -> TExpr.Upcast(translateExpr ctx inner, ty, tok)
        | Expr.DynamicDowncast(expr = inner) -> TExpr.Downcast(translateExpr ctx inner, ty, tok)
        | Expr.DynamicTypeTest(expr = inner) ->
            // `ty` is the `bool` result; the tested-against type was stashed by
            // Unification (`inferDynamicTypeTest`) keyed by this node.
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
            TExpr.ForTo(varKey, translateExpr ctx startE, translateExpr ctx endE, translateExpr ctx body, ty, tok)
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
        // `value.Member`) that Unification resolved through the provider — emit a
        // keyed `TExpr.ExternalMember`. A static
        // access drops the type-name receiver (`info.IsStatic`).
        | Expr.DotLookup(expr = r; longIdentOrOp = LongIdentOrOp.LongIdent li) & ExternalAccess ctx info when
            li.Idents.Length = 1
            ->
            let memberName = ctx.NameOf li.Idents.[0]

            let receiver =
                if info.IsStatic then
                    ValueNone
                else
                    ValueSome(translateExpr ctx r)

            TExpr.ExternalMember(receiver, info.Key, memberName, info.Storage, ty, tok)
        // `ClassName<'args>.Prop` — local static property read on an explicitly
        // instantiated generic class (e.g. `Set<'T>.Empty`). Same lowering as the
        // folded `ClassName.Member` form; the `<'args>` only pinned the generic
        // instantiation in inference and is carried on `ty`. The method form
        // (`Set<'T>.Singleton value`) is `App`-wrapped and handled with the other
        // static-method arms.
        | TypeAppStaticMember ctx (declKey, memberName, ClassMemberKind.Property) ->
            let key = LocalSymbolKey.ofMember declKey memberName 0 MemberKind.Property
            TExpr.StaticPropertyGet(key, ty, tok)
        | Expr.DotLookup(expr = r; longIdentOrOp = LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
            ElaborateAccess.translateDotLookup translateExpr ctx key r (ctx.NameOf li.Idents.[0]) ty tok
        | Expr.DynamicLookup(expr = r; ident = idTok) ->
            ElaborateAccess.translateDynamicLookup translateExpr ctx key r idTok ty tok
        | Expr.Null _ -> TExpr.Null(ty, tok)
        // A range reaches these arms ONLY when it was NOT consumed by `translateForIn`'s
        // counted-`ForTo` lowering (the unit-step, simple-binder for-in source). That
        // leaves value position, a stepped range, and a non-simple loop binder — all
        // unsupported, because a range materialises no seq value in this compiler. This
        // is the lowering choke point where the range's POSITION is known, so the
        // unsupported use is diagnosed here rather than in inference (`inferRange` cannot
        // tell a for-in source from a value). `range-operators-plan.md` tracks making
        // `(..)` a real seq operator, which would delete these arms.
        | Expr.Range(fromExpr = a; toExpr = b) ->
            ctx.Error(key, rangeNotFirstClassValue)
            TExpr.Range(translateExpr ctx a, None, translateExpr ctx b, ty, tok)
        | Expr.SteppedRange(fromExpr = a; stepExpr = s; toExpr = b) ->
            ctx.Error(key, rangeNotFirstClassValue)
            TExpr.Range(translateExpr ctx a, Some(translateExpr ctx s), translateExpr ctx b, ty, tok)
        | Expr.IndexedLookup(expr = r; indexExpr = idx) ->
            ElaborateAccess.translateIndexedLookup translateExpr ctx key r idx ty tok
        | Expr.ILIntrinsic(instrParts = parts; args = args) -> translateIlIntrinsic ctx parts args ty tok
        | Expr.StaticMemberInvocation(membersign = msig; expr = argExpr) ->
            ElaborateApply.translateStaticMemberInvocation translateExpr ctx argExpr msig ty tok
        | Expr.LibraryOnlyStaticOptimization _ -> translateStaticOptimization ctx e ty tok
        // `value<'T>` — an explicit type application on a VALUE reference (NOT the
        // `TypeAppStaticMember` class-receiver forms, which are a `DotLookup` over the
        // `TypeApp` and matched above). The `<'T>` only pinned the instantiation in
        // inference (`inferTypeApp` returns the inner's type verbatim for a bare-typar
        // result, so the node and its inner reference share one TyVar); forward to the
        // frozen inner, leaving its leaf intact. For an `inline` binding
        // (`Unchecked.defaultof<'T>` / `defaultof<'T>`) that leaf is the `External` head
        // the bare form produces, so the reference reaches the same
        // `InlineExpansion` splice arm — `refTy` (pinned by the reference's expected
        // type) grounds the spliced `ilzero`.
        | Expr.TypeApp(expr = inner) -> translateExpr ctx inner
        | _ ->
            // TODO: extend as the subset grows; surface the unhandled case
            // loudly rather than emitting a broken TExpr.
            failwithf "Elaborate.translateExpr: TODO %A" e

    /// `new T(args)` — Unification stamps `ty` with the `TyClass`. The CST-side
    /// fallback is purely defensive for error paths where Unification couldn't
    /// pin the receiver.
    and private translateNew
        (ctx: PassContext)
        (t: Type<SyntaxToken>)
        (argExpr: Expr<SyntaxToken>)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr =
        let className =
            match Unification.zonk ty with
            // Qualified so the backend's external-ctor recipe (`new
            // System.Exception(...)`) resolves; the backend strips to the bare
            // simple name for the project-local class lookup.
            | TyClass(n, _) -> SymbolKeyOps.qualifiedName n
            | _ ->
                let rec nameOf t =
                    match t with
                    | Type.NamedType li
                    | Type.GenericType(longIdent = li) when li.Idents.Length >= 1 ->
                        ctx.NameOf li.Idents.[li.Idents.Length - 1]
                    | Type.ParenType(typ = inner) -> nameOf inner
                    | _ -> ""

                nameOf t

        mkNew ctx className ty (peelOneArg (translateExpr ctx) argExpr) tok

    /// An integer-range source (`for i in a..b do`) lowers to a counted
    /// `ForTo` loop — F#'s own lowering. There is no enumerable object to
    /// walk (the range materialises no `seq`), so the enumerator path can't
    /// emit it; the counted form is also the efficient one. Only the
    /// unit-step range bound to a *simple* binder is lowered here; a stepped
    /// range (`a..s..b`) or a non-trivial pattern falls through to the
    /// enumerator path (which diagnoses an unsupported source cleanly).
    /// Inference already pinned the binder + bounds to `int`
    /// (`InferControlFlow.inferForIn`'s range arm).
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
        | ValueSome(a, b), TPat.NamedSimple(varKey, _, _) ->
            TExpr.ForTo(varKey, translateExpr ctx a, translateExpr ctx b, translateExpr ctx body, ty, tok)
        | _ ->
            // How the source yields its enumerator was resolved by Unification
            // and stashed by this node's key; absent ⇒ the §4.2 interface path.
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
                            | ValueSome ft -> wrapObjArg ft argT
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

        // The tokenful array opcodes (`newarr`/`ldelem.any`) carry a single
        // element-type operand. The source `!0` placeholder is unparsed tokens,
        // so the element is recovered from the node's declared types — `newarr`'s
        // result is the array (`elem` = its argument), `ldelem`'s result IS the
        // element. The mnemonics ORIGINATE in per-target library source
        // (`array.fs`'s `zeroCreate`, `ops-platform.fs`'s `GetArray`), so this is
        // interpreting source IL, not inventing it. The mnemonic is normalised
        // (`ldelem.any` → `ldelem`) to the form codegen's emit arm reads.
        if opCode.StartsWith "newarr" then
            let elem =
                match Unification.zonk ty with
                | TyArray elem -> elem
                | other -> failwithf "Elaborate: 'newarr' result is not a rank-1 array: %A" other

            TExpr.ILIntrinsic("newarr", ValueSome elem, tArgs, ty, tok)
        elif opCode.StartsWith "ldelem" then
            TExpr.ILIntrinsic("ldelem", ValueSome(Unification.zonk ty), tArgs, ty, tok)
        elif opCode.StartsWith "stelem" then
            // `arr.[i] <- v` / `SetArray`. The store's result is `unit`, so the
            // element type is recovered from the value operand (the 3rd arg:
            // array, index, value), not the node's result type as `ldelem` does.
            let elem = Unification.zonk (typeOfKey ctx (CstKeys.ofExpr args.[2]))
            TExpr.ILIntrinsic("stelem", ValueSome elem, tArgs, ty, tok)
        elif opCode.StartsWith "box" then
            // `box value` — the boxed element type is the *argument's* static
            // type (the result is always `obj`), so recover it from the single
            // value operand. A value type emits `box <T>`; a reference type's
            // box is the JIT-erased identity (codegen leaves it as `box`, which
            // the runtime treats as a no-op on a ref type).
            let elem = Unification.zonk (typeOfKey ctx (CstKeys.ofExpr args.[0]))
            TExpr.ILIntrinsic("box", ValueSome elem, tArgs, ty, tok)
        elif opCode.StartsWith "ilzero" then
            // `Unchecked.defaultof<'T>` — a type's default value. `ilzero`'s result IS
            // the defaulted 'T, so the operand type is the node's result type (recovered
            // like `ldelem`'s). The source `type ('T)` clause is decorative here — the
            // result type is authoritative — but kept in source to match the F# idiom.
            TExpr.ILIntrinsic("ilzero", ValueSome(Unification.zonk ty), tArgs, ty, tok)
        else
            TExpr.ILIntrinsic(opCode, ValueNone, tArgs, ty, tok)

    /// The clause chain nests left-fold (outermost = the last `when` in
    /// source order). Peel it into a flat source-ordered clause list plus
    /// the leading default expr, reading each clause's resolved constraints
    /// from the side table Unification keyed by that clause node's key.
    /// Visiting outermost→innermost and prepending yields source order.
    and private translateStaticOptimization
        (ctx: PassContext)
        (e: Expr<SyntaxToken>)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr =
        let rec peel (node: Expr<SyntaxToken>) (acc: TStaticOptClause list) : TExpr * TStaticOptClause list =
            match node with
            | Expr.LibraryOnlyStaticOptimization(expr = inner; optimizedExpr = optE) ->
                let cs =
                    match ctx.StaticOpt.TryGetValue(CstKeys.ofExpr node) with
                    | ValueSome v -> v
                    | ValueNone -> EqArray.empty

                peel
                    inner
                    ({
                        Constraints = cs
                        Body = translateExpr ctx optE
                     }
                     :: acc)
            | other -> translateExpr ctx other, acc

        let defaultExpr, clauses = peel e []
        TExpr.StaticOptimization(EqArray.ofList clauses, defaultExpr, ty, tok)

    and private translateRules (ctx: PassContext) (rules: ImmutableArray<Rule<SyntaxToken>>) : EqArray<TMatchArm> =
        EqArray.ofSeq (
            seq {
                for r in rules do
                    match r with
                    | Rule.Rule(pat = pat; guard = guard; expr = body) ->
                        let guardT =
                            match guard with
                            | ValueSome(PatternGuard(expr = g)) -> Some(translateExpr ctx g)
                            | ValueNone -> None

                        yield
                            {
                                Pat = translatePat ctx pat
                                Guard = guardT
                                Body = translateExpr ctx body
                            }
                    | _ -> ()
            }
        )

    /// Project `[…]` / `[|…|]` literals into the shared `Cons` / `Nil` chain
    /// Unification typed them with. Arrays additionally route through `Array.ofList`
    /// so codegen sees a single lowering target — the list chain. Element type is
    /// recovered from the literal's frozen type; a degenerate type falls back to a
    /// free TyVar so downstream consumers see *some* element type, not a malformed
    /// node.
    and private translateListLikeLiteral
        (ctx: PassContext)
        (literalTy: SemType)
        (isArray: bool)
        (items: Expr<SyntaxToken> list)
        (tok: SyntaxToken)
        : TExpr =
        let zonked = Unification.zonk literalTy

        let elemTy =
            match zonked with
            // An array literal's zonked type is the generic intrinsic
            // `TyConst("[]", [elem])`; a list
            // literal's is `TyRecord`/`TyUnion`. Pull the element out of whichever.
            | TyConst(_, args) when args.Length = 1 -> args.[0]
            | TyRecord(_, args) when args.Length = 1 -> args.[0]
            | TyUnion(_, args) when args.Length = 1 -> args.[0]
            | _ -> TyVar(TypeVar())

        // A program-declared list union (resolved via the `'T list = List<'T>`
        // abbrev — see `Unification.listLiteralTy`) drives `[…]` construction
        // through that union's own case factories: nullary case = empty
        // terminator, single binary case = cons. Absent it (a normal program, or
        // any array literal), the FSharp.Core `Cons`/`Nil` nominal is the default.
        // Arrays never retarget — always the list chain + `Array.ofList` boundary.
        let listTy, consName, nilName =
            match zonked with
            | TyUnion(unionKey, _) when not isArray && (TypeRegistry.tryUnionByKey ctx.Types unionKey).IsSome ->
                let info = (TypeRegistry.tryUnionByKey ctx.Types unionKey).Value
                let nilCase = info.Cases |> Array.tryFind (fun c -> c.Fields.Length = 0)
                let consCase = info.Cases |> Array.tryFind (fun c -> c.Fields.Length = 2)

                match nilCase, consCase with
                | Some n, Some c -> zonked, c.Name, n.Name
                | _ -> TyRecord(RuntimeNames.fsharpCoreListKey, EqArray.singleton elemTy), "Cons", "Nil"
            // The external Vesper list: a bare-program literal a consumer drove
            // onto the Vesper cons-list (`Unification.listLiteralTy` /
            // `resolveListLiterals`). Its `Cons` / `Nil` factories are minted by the
            // backend's `TryEmitUnionCons` Vesper case — BCL-only, no FSharp.Core.
            // It is an *external* union, so it is absent from `ctx.Types.Union` and
            // is not caught by the user-union arm above. Recognition (bare /
            // arity-suffixed union name, or the lowercase abbreviation) is shared
            // with codegen via `RuntimeNames.isVesperListKey`, so the `` `N ``-strip isn't re-derived here.
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
            // Codegen resolves `Array.ofList` against its target; alternate
            // targets are free to swap the wrapper. The BCL-only path recognises
            // this exact head and emits the array directly (no FSharp.Core).
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
        // Fold elifs right-to-left, each nested as the else-branch of the previous.
        // A missing else is `else ()` (F# spec): inference has already constrained
        // the then/elif branches and the whole expression to `unit`, so synthesize a
        // `unit` constant as the innermost else.
        let mutable nestedElse =
            match elseB with
            | ValueSome(ElseBranch(expr = e)) -> translateExpr ctx e
            // Synthesised `else ()` (no source token) — anchor at the `if`'s token.
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

        // `use` / `use!` bind a disposable: each binding folds to a `TExpr.Use`
        // (codegen wraps the body in a `try … finally Dispose()` region, B-5)
        // rather than a plain `TExpr.Let`.
        let isUse =
            match keyword with
            | LetOrUseKeyword.Use _
            | LetOrUseKeyword.UseBang _ -> true
            | LetOrUseKeyword.Let _
            | LetOrUseKeyword.LetBang _ -> false

        for i = bindings.Length - 1 downto 0 do
            let b = bindings.[i]

            // Drop an E1 format-literal alias binding (`let fmt : Format<…> = "%d" in
            // …`): its value froze to a `New PrintfFormat` that is dead — every use
            // const-propagates the literal (`PrintfFormatLiterals`), and the self-host
            // contract has no cold runtime for a format value. Fold it out, keeping the
            // body, so no `New PrintfFormat` reaches codegen. (`use` never binds a
            // format, so it is never an alias.)
            if not isUse && ctx.PrintfFormatLiterals.ContainsKey(CstKeys.ofPat b.headPat) then
                ()
            else

                let tpat = translatePat ctx b.headPat
                let valT = translateBinding ctx b
                // The let/use node's source anchor is its binder pattern's first token.
                let bindTok = CstKeys.firstTokenOfPat b.headPat

                result <-
                    if isUse then
                        // An external (BCL) binder's keyed `Dispose` is recorded by
                        // Unification under the head-pattern's key; a project-local binder
                        // has none and codegen takes the duck-typed direct call (§4.3).
                        let dispose = ctx.Resolution.UseDispose.TryGetValue(CstKeys.ofPat b.headPat)
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
