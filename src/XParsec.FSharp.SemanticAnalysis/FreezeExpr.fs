namespace XParsec.FSharp.SemanticAnalysis

open System
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.FreezeLiterals
open XParsec.FSharp.SemanticAnalysis.FreezeResolve
open XParsec.FSharp.SemanticAnalysis.FreezePatterns
open XParsec.FSharp.SemanticAnalysis.FreezeExprArgs

// Expression translation for the Freeze pass: the recursive CST->TExpr projection.
// Its helpers live in the sibling `Freeze/` modules opened above — constant /
// string parsing (`FreezeLiterals`), name / member resolution + active patterns
// (`FreezeResolve`), pattern projection (`FreezePatterns`), and argument peeling
// (`FreezeExprArgs`). The companion ``Elaborate`` module (type-declaration
// surfacing + ``run``) opens this one for the entry points it projects from.

module internal FreezeExpr =

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
            translateLongIdentFieldChain ctx li ty (ctx.Resolution.ExternalAccess.TryGetValue key) tok
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
        // `new T(args)` — Unification stamps `ty` with the `TyClass`. The CST-side
        // fallback is purely defensive for error paths where Unification couldn't
        // pin the receiver.
        | Expr.New(typ = t; expr = argExpr) ->
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
            let receiver = translateLongIdentFieldChain ctx prefixLi receiverTy ValueNone tok

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
            let receiver = translateLongIdentFieldChain ctx prefixLi receiverTy ValueNone tok
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
            let receiver = translateLongIdentFieldChain ctx prefixLi receiverTy ValueNone tok

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
            let receiver = translateLongIdentFieldChain ctx prefixLi receiverTy ValueNone tok
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
        | Expr.LongIdentOrOp _ -> translateIdent ctx e key ty tok
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
        | Expr.App(_, args) when ctx.PrintfPartial.ContainsKey key -> translatePrintfPartial ctx key args ty tok
        // Printf happy-path call, marked by `Unification.tryInferPrintfApp`. Must
        // lower to a `TExpr.Format` *before* the `App(printfn, New PrintfFormat …)`
        // projection below ever runs.
        | Expr.App(fn, args) when ctx.PrintfApp.ContainsKey key ->
            // The marker may still decline (a `%A` of a record / DU — gated until
            // step-3 synthesis); fall back to the standard external-call path,
            // which lowers to the FSharp.Core cold printf.
            match translatePrintfFormat ctx key args ty tok with
            | ValueSome node -> node
            | ValueNone -> translateApp ctx fn args tok
        | Expr.App(fn, args) -> translateApp ctx fn args tok
        | Expr.HighPrecedenceApp(funcExpr = fn; argExpr = arg) ->
            // A residual single application (an external .NET method reached as a
            // folded LongIdent, a local function value, a top-level `let f (x: obj)`
            // emitted as a static method, …). Box a value arg flowing into an `obj`
            // parameter — the implicit upcast, made explicit.
            // An external method reads its `obj` slot off the declared signature
            // Unification recorded (`externalMethodParamTy`, its node SemType is the
            // un-grounded applied shape); everything else reads the parameter off
            // the head's function type.
            let fnT = translateExpr ctx fn
            let fnKey = CstKeys.ofExpr fn

            match tryTranslateExternalOptionalFill ctx fnT fnKey (ImmutableArray.Create arg) tok with
            // An external method call that omitted trailing optionals (the call parses
            // as a high-precedence application of the folded LongIdent).
            | ValueSome node -> node
            | ValueNone ->

                let argT = translateExpr ctx arg

                let paramTy =
                    match externalHeadDom ctx fnKey fnT with
                    | ValueSome _ as dom -> dom
                    | ValueNone ->
                        match Unification.zonk (TastWalk.exprTy fnT) with
                        | TyFun(p, _) -> ValueSome p
                        | _ -> ValueNone

                let argT =
                    match paramTy with
                    | ValueSome p -> wrapObjArg p argT
                    | ValueNone -> argT

                TExpr.App(fnT, argT, ty, tok)
        | Expr.InfixApp(left, _, right) -> translateInfix ctx key left right ty tok
        | Expr.PrefixApp(_, operand) -> translatePrefix ctx key operand ty tok
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
                | ValueNone -> failwithf "Freeze: no recorded type-test target for %O" key

            TExpr.TypeTest(translateExpr ctx inner, testTy, ty, tok)
        | Expr.EmptyBlock(lParen = ParenKind.List _) -> translateListLikeLiteral ctx ty false [] tok
        | Expr.EmptyBlock(lParen = ParenKind.Array _) -> translateListLikeLiteral ctx ty true [] tok
        | Expr.EmptyBlock _ -> unitConst ctx e
        | Expr.While(condition = cond; body = body) ->
            TExpr.While(translateExpr ctx cond, translateExpr ctx body, ty, tok)
        | Expr.ForTo(ident = ident; startExpr = startE; endExpr = endE; body = body) ->
            let varKey = CstKeys.ofForToVar ident
            TExpr.ForTo(varKey, translateExpr ctx startE, translateExpr ctx endE, translateExpr ctx body, ty, tok)
        | Expr.ForIn(pat = pat; enumerableExpr = src; body = body) ->
            // An integer-range source (`for i in a..b do`) lowers to a counted
            // `ForTo` loop — F#'s own lowering. There is no enumerable object to
            // walk (the range materialises no `seq`), so the enumerator path can't
            // emit it; the counted form is also the efficient one. Only the
            // unit-step range bound to a *simple* binder is lowered here; a stepped
            // range (`a..s..b`) or a non-trivial pattern falls through to the
            // enumerator path (which diagnoses an unsupported source cleanly).
            // Inference already pinned the binder + bounds to `int`
            // (`InferControlFlow.inferForIn`'s range arm).
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
        | Expr.String _ -> translateString ctx e ty tok
        | Expr.Match(matchExpr = scrutinee; rules = Rules(rules = rules)) ->
            TExpr.Match(translateExpr ctx scrutinee, translateRules ctx rules, ty, tok)
        | Expr.Function(rules = Rules(rules = rules)) ->
            // `function …` ~ `fun x -> match x with …`. The synthesised parameter
            // has no source token, so mint a synthetic key under the
            // function-keyword's offset for the Match scrutinee to reference.
            let funcKey = CstKeys.ofExpr e

            let paramKey = NodeKey.ofSynthetic funcKey.Offset NodeKind.SynthLambdaBody

            let paramTy, resultTy =
                match ty with
                | TyFun(p, r) -> p, r
                | _ -> failwithf "Freeze.Function: expected function type, got %A" ty

            let scrutinee = TExpr.Var(paramKey, paramTy, tok)
            let body = TExpr.Match(scrutinee, translateRules ctx rules, resultTy, tok)
            TExpr.Lambda(TPat.NamedSimple(paramKey, paramTy, tok), body, ty, tok)
        | Expr.TryWith(expr = body; rules = Rules(rules = rules)) ->
            TExpr.TryWith(translateExpr ctx body, translateRules ctx rules, ty, tok)
        | Expr.TryFinally(tryExpr = body; finallyExpr = finallyE) ->
            TExpr.TryFinally(translateExpr ctx body, translateExpr ctx finallyE, ty, tok)
        | Expr.Assignment(leftExpr = left; rightExpr = right) ->
            // `r.X <- v` folds to FieldSet; everything else to Assignment.
            let unwrapped =
                let rec unwrap e =
                    match e with
                    | Expr.EnclosedBlock(expr = inner)
                    | Expr.TypeAnnotation(expr = inner) -> unwrap inner
                    | _ -> e

                unwrap left

            match unwrapped with
            | Expr.DotLookup(expr = r; longIdentOrOp = LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
                let fieldName = ctx.NameOf li.Idents.[0]
                TExpr.FieldSet(translateExpr ctx r, fieldName, translateExpr ctx right, ty, tok)
            // `arr.[i] <- v` desugars to the core `SetArray` inline function (the
            // write mirror of the `IndexedLookup` → `GetArray` read path below):
            // the `stelem` mnemonic lives in Vesper.Core's `ops-platform.fs`,
            // spliced at this use site by `InlineExpansion` — never invented in this
            // target-agnostic pass. Emit a curried `External` call whose type is
            // rebuilt from the resolved operand types (`ty` is the assignment's
            // `unit` result).
            | Expr.IndexedLookup(expr = arrE; indexExpr = idxE) ->
                let arrTy = typeOfKey ctx (CstKeys.ofExpr arrE)
                let idxTy = typeOfKey ctx (CstKeys.ofExpr idxE)
                let valTy = typeOfKey ctx (CstKeys.ofExpr right)
                let valuePartial = TyFun(valTy, ty)
                let idxPartial = TyFun(idxTy, valuePartial)
                let setExpr = TExpr.External("SetArray", ValueNone, TyFun(arrTy, idxPartial), tok)
                let app1 = TExpr.App(setExpr, translateExpr ctx arrE, idxPartial, tok)
                let app2 = TExpr.App(app1, translateExpr ctx idxE, valuePartial, tok)
                TExpr.App(app2, translateExpr ctx right, ty, tok)
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
                li.Idents.Length > 1
                && ctx.Bindings.Binding.ContainsKey(NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent)
                ->
                // `r.X <- v` parsed as Assignment(LongIdent[r;X], <-, v). The
                // head-resolved chain peels into FieldGet for the intermediate
                // segments and a final FieldSet for the assigned slot.
                let receiverIdents = li.Idents
                let lastIdx = receiverIdents.Length - 1

                let receiverChain =
                    let head = receiverIdents.[0]
                    let headKey = NodeKey.ofToken head NodeKind.ExprIdent
                    let headBinding = ctx.Bindings.Binding.TryGetValue headKey

                    let headTy =
                        match headBinding with
                        | ValueSome rb -> typeOfKey ctx rb.BindingSite
                        | ValueNone -> typeOfKey ctx (CstKeys.ofExpr unwrapped)

                    let headExpr =
                        match headBinding with
                        | ValueSome rb -> TExpr.Var(rb.BindingSite, headTy, tok)
                        | ValueNone -> TExpr.External(ctx.NameOf head, ValueNone, headTy, tok)

                    let mutable curr = headExpr
                    let mutable currTy = headTy

                    // Field reads for the intermediate segments — the assigned slot
                    // is the final one, handled by the `FieldSet` below. Same chain
                    // walk as `translateLongIdentFieldChain`, stopping one short.
                    for i = 1 to lastIdx - 1 do
                        let segName = ctx.NameOf receiverIdents.[i]

                        let stepTy =
                            match recoverFieldStepTy ctx currTy segName with
                            | ValueSome t -> t
                            | ValueNone -> currTy

                        curr <- fieldStep ctx curr currTy segName stepTy tok
                        currTy <- stepTy

                    curr

                let lastName = ctx.NameOf receiverIdents.[lastIdx]
                TExpr.FieldSet(receiverChain, lastName, translateExpr ctx right, ty, tok)
            // `recv?name <- v` → `(?<-) recv "name" v` → the `op_DynamicAssignment`
            // inline body `$0[$1] = $2` splices to the computed-member write
            // `recv["name"] = v`. The name is a compile-time string literal (the ident
            // text), NOT a value reference. Mirrors the `SetArray` curried-External shape.
            | Expr.DynamicLookup(expr = r; ident = idTok) ->
                let recvTy = typeOfKey ctx (CstKeys.ofExpr r)
                let valTy = typeOfKey ctx (CstKeys.ofExpr right)

                let nameLit =
                    TExpr.Const(TConstValue.String(ctx.NameOf idTok), BuiltinTypes.tyString, tok)

                let valuePartial = TyFun(valTy, ty)
                let namePartial = TyFun(BuiltinTypes.tyString, valuePartial)

                let opExpr =
                    TExpr.External("op_DynamicAssignment", ValueNone, TyFun(recvTy, namePartial), tok)

                let app1 = TExpr.App(opExpr, translateExpr ctx r, namePartial, tok)
                let app2 = TExpr.App(app1, nameLit, valuePartial, tok)
                TExpr.App(app2, translateExpr ctx right, ty, tok)
            | _ -> TExpr.Assignment(translateExpr ctx left, translateExpr ctx right, ty, tok)
        | Expr.Record(fieldInitializers = inits) ->
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
        | Expr.RecordClone(expr = src; fieldInitializers = inits) ->
            let overrides =
                EqArray.ofSeq (
                    seq {
                        for FieldInitializer(longIdent = li; expr = e) in inits ->
                            let idents = li.Idents
                            ctx.NameOf idents.[idents.Length - 1], translateExpr ctx e
                    }
                )

            TExpr.RecordClone(translateExpr ctx src, overrides, ty, tok)
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
            let memberName = ctx.NameOf li.Idents.[0]
            let rTy = Unification.zonk (typeOfKey ctx (CstKeys.ofExpr r))
            let receiver = translateExpr ctx r

            // A class/union receiver's member access is a `PropertyGet` (a
            // method-as-value keeps the same shape — codegen eta-expands);
            // anything else reads a record/tuple field.
            match rTy with
            | TyClass _
            | TyUnion _ ->
                let key =
                    LocalSymbolKey.ofMember (nominalDeclKey rTy) memberName 0 MemberKind.Property

                TExpr.PropertyGet(receiver, key, viaOfReceiver ctx receiver, ty, tok)
            // `(expr).Length` on an intrinsic rank-1 array desugars to the core
            // `GetArrayLength` inline function — the `ldlen` mnemonic lives in
            // `ops-platform.fs`, spliced by `InlineExpansion`. Mirrors the
            // `fieldStep` array guard (the LongIdent-chain form).
            | TyConst(name, _) when name = RuntimeNames.arrayName 1 && memberName = "Length" ->
                TExpr.App(TExpr.External("GetArrayLength", ValueNone, TyFun(rTy, ty), tok), receiver, ty, tok)
            | _ -> TExpr.FieldGet(receiver, memberName, ty, tok)
        // `recv?name` → `(?) recv "name"` → the `op_Dynamic` inline body `$0[$1]`
        // splices to the computed-member read `recv["name"]`. The name is a compile-time
        // string literal (the ident text), NOT a value reference. `ty` is the (possibly
        // target-typed) result. Mirrors the `GetArray` curried-External shape.
        | Expr.DynamicLookup(expr = r; ident = idTok) ->
            let recvTy = typeOfKey ctx (CstKeys.ofExpr r)

            let nameLit =
                TExpr.Const(TConstValue.String(ctx.NameOf idTok), BuiltinTypes.tyString, tok)

            let partialTy = TyFun(BuiltinTypes.tyString, ty)
            let opExpr = TExpr.External("op_Dynamic", ValueNone, TyFun(recvTy, partialTy), tok)
            let app1 = TExpr.App(opExpr, translateExpr ctx r, partialTy, tok)
            TExpr.App(app1, nameLit, ty, tok)
        | Expr.Null _ -> TExpr.Null(ty, tok)
        | Expr.Range(fromExpr = a; toExpr = b) -> TExpr.Range(translateExpr ctx a, None, translateExpr ctx b, ty, tok)
        | Expr.SteppedRange(fromExpr = a; stepExpr = s; toExpr = b) ->
            TExpr.Range(translateExpr ctx a, Some(translateExpr ctx s), translateExpr ctx b, ty, tok)
        // `arr.[i]` desugars to the core `GetArray` inline function (mirroring F#'s
        // `IntrinsicFunctions.GetArray`): the `ldelem` mnemonic lives in
        // Vesper.Core's `ops-platform.fs`, spliced at this use site by
        // `InlineExpansion` — never invented in this target-agnostic pass. Mirrors
        // the operator path (`translateInfix`): emit a curried `External` call whose
        // type is rebuilt from the resolved operand types. `ty` is the element type.
        | Expr.IndexedLookup(expr = r; indexExpr = idx) ->
            match ctx.Resolution.ExternalAccess.TryGetValue key with
            | ValueSome info ->
                // `span.[i]` on an external indexer (`Span<char>.get_Item(i) : T&`):
                // Unification recorded the resolved `get_Item` in `ExternalAccess`.
                // The BCL accessor returns a managed pointer and has no by-value
                // form, so call it through the external-instance-method machinery
                // (PP2a address dispatch — the receiver is an unboxed struct) and
                // dereference the result with `ldobj <elem>`. `ty` is the
                // value-position element; the call's static type is `elem&`
                // (`TyConst(byrefName, [elem])`), which `ldobj` loads.
                let idxTy = typeOfKey ctx (CstKeys.ofExpr idx)
                let memberName = SymbolKeyOps.simpleName info.Key

                // A byref-returning accessor (`Span<char>.get_Item : T&`) needs the
                // `ldobj` deref; a by-value one (`string.get_Chars : char`) is a plain
                // call. Read the declared return off the recorded signature.
                let retIsByref =
                    match Unification.zonk info.Signature with
                    | TyFun(_, TyConst(n, _)) when n = RuntimeNames.byrefName -> true
                    | _ -> false

                if retIsByref then
                    let byrefTy = TyConst(RuntimeNames.byrefName, EqArray.singleton ty)
                    let memberFnTy = TyFun(idxTy, byrefTy)

                    let getItem =
                        TExpr.ExternalMember(
                            ValueSome(translateExpr ctx r),
                            info.Key,
                            memberName,
                            MemberStorage.Method,
                            memberFnTy,
                            tok
                        )

                    let callExpr = TExpr.App(getItem, translateExpr ctx idx, byrefTy, tok)
                    TExpr.ILIntrinsic("ldobj", ValueSome ty, EqArray.singleton callExpr, ty, tok)
                else
                    let memberFnTy = TyFun(idxTy, ty)

                    let getItem =
                        TExpr.ExternalMember(
                            ValueSome(translateExpr ctx r),
                            info.Key,
                            memberName,
                            MemberStorage.Method,
                            memberFnTy,
                            tok
                        )

                    TExpr.App(getItem, translateExpr ctx idx, ty, tok)
            | ValueNone ->
                let arrTy = typeOfKey ctx (CstKeys.ofExpr r)
                let idxTy = typeOfKey ctx (CstKeys.ofExpr idx)
                let partialTy = TyFun(idxTy, ty)
                let getTy = TyFun(arrTy, partialTy)

                // A `string` receiver lowers through `GetString` (its inline body emits
                // the native `s[i]` on JS); every other receiver through `GetArray`
                // (`ldelem`). The inference picked the matching intrinsic (`inferIndexedLookup`
                // → `getStringIndex`/`getArrayIndex`), so the names line up.
                let getName =
                    match Unification.zonk arrTy with
                    | TyConst("string", _) -> "GetString"
                    | _ -> "GetArray"

                let getExpr = TExpr.External(getName, ValueNone, getTy, tok)
                let app1 = TExpr.App(getExpr, translateExpr ctx r, partialTy, tok)
                TExpr.App(app1, translateExpr ctx idx, ty, tok)
        | Expr.ILIntrinsic(instrParts = parts; args = args) ->
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
                    | TyConst(name, eargs) when name = RuntimeNames.arrayName 1 && eargs.Length = 1 -> eargs.[0]
                    | other -> failwithf "Freeze: 'newarr' result is not a rank-1 array: %A" other

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
            else
                TExpr.ILIntrinsic(opCode, ValueNone, tArgs, ty, tok)
        | Expr.StaticMemberInvocation(membersign = msig; expr = argExpr) ->
            translateStaticMemberInvocation ctx argExpr msig ty tok
        | Expr.LibraryOnlyStaticOptimization _ ->
            // The clause chain nests left-fold (outermost = the last `when` in
            // source order). Peel it into a flat source-ordered clause list plus
            // the leading default expr, reading each clause's resolved constraints
            // from the side table Unification keyed by that clause node's key.
            // Visiting outermost→innermost and prepending yields source order.
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
        | _ ->
            // TODO: extend as the subset grows; surface the unhandled case
            // loudly rather than emitting a broken TExpr.
            failwithf "Freeze.translateExpr: TODO %A" e

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

    and private translateString (ctx: PassContext) (e: Expr<SyntaxToken>) (ty: SemType) (tok: SyntaxToken) : TExpr =
        match e with
        | Expr.String(parts = parts) ->
            match Unification.zonk ty with
            | TyClass(key, _) when RuntimeNames.isPrintfFormatKey key ->
                // Format literal at a printf call site (typed by
                // `Unification.tryInferPrintfApp`). It denotes `new
                // PrintfFormat<…>(text)` — the single `value: string` ctor.
                TExpr.New(
                    PrintfSpec.printfFormatName,
                    EqArray.singleton (
                        TExpr.Const(TConstValue.String(stitchLiteralString ctx parts), BuiltinTypes.tyString, tok)
                    ),
                    ty,
                    tok
                )
            | _ ->
                // A faithfully-renderable interpolation lowers to a `TExpr.Format`
                // (D9). Otherwise (plain string, or an unrenderable hole) stitch
                // the literal text, keeping any unrendered hole's `{<expr>}`
                // placeholder — additive over the pre-D9 behaviour.
                match tryTranslateInterpolation ctx parts ty tok with
                | Some node -> node
                | None -> TExpr.Const(TConstValue.String(stitchLiteralString ctx parts), ty, tok)
        | _ -> failwithf "Freeze.translateString: not a String expr: %A" e

    /// Interpolation holes have no rendering on this path, so they surface as
    /// `{<expr>}` placeholders. Only reached for plain strings, printf format
    /// literals, and interpolations a hole kept off the `TExpr.Format` path.
    and private stitchLiteralString (ctx: PassContext) (parts: ImmutableArray<StringPart<SyntaxToken>>) : string =
        foldStringParts ctx (fun () -> "{<expr>}") parts

    /// Classify one interpolation hole into the `HoleSpecSource` a `FormatSeg.Hole`
    /// carries, or `None` if it can't be rendered faithfully. A printf-style
    /// `%d{x}` carries the classified `HoleForm` (`Classified`), admitted only when
    /// `PrintfHoleForm.tryClassify` accepts it — exactly the specifiers the printf
    /// happy path covers. A plain `{x}` / `{x:fmt}` carries the raw format clause
    /// (`RawFormat`). Interpolation alignment (`{x,n}`) isn't representable here —
    /// the parser folds `x,n` into a tuple expression — so the plain forms carry no
    /// alignment.
    and private tryInterpHoleSpec
        (ctx: PassContext)
        (formatSpecifier: SyntaxToken voption)
        (formatClause: SyntaxToken voption)
        : HoleSpecSource option =
        match formatSpecifier with
        | ValueSome ft ->
            match Lexing.parseFormatSpecifierView (ctx.ReadableOf ft) with
            | ValueSome p ->
                // Same parity gate as the printf path: only specifiers faithfully
                // representable as a structured `Format` lower (`tryClassify` accepts
                // them) lower; the rest keep the generic printf call shape. The
                // classification is kept (not re-derived per backend).
                match PrintfHoleForm.tryClassify p with
                | ValueSome hf -> Some(HoleSpecSource.Classified hf)
                | ValueNone -> None
            | ValueNone -> None
        | ValueNone ->
            let fmt =
                match formatClause with
                | ValueSome fc ->
                    let raw = ctx.NameOf fc
                    let f = if raw.StartsWith ":" then raw.Substring 1 else raw
                    if f.Length = 0 then None else Some f
                | ValueNone -> None

            Some(HoleSpecSource.RawFormat fmt)

    /// Lower an interpolated string ($"…{x}…") to a `TExpr.Format` (D9). Returns
    /// `None` — keeping the literal-stitch fallback — when the string has no
    /// holes, or any hole isn't faithfully renderable: a free (unresolved) hole
    /// type, an orphan/standalone `%spec` or lexer-error part, or a printf-typed
    /// `%d{x}` whose specifier the happy path doesn't cover.
    and private tryTranslateInterpolation
        (ctx: PassContext)
        (parts: ImmutableArray<StringPart<SyntaxToken>>)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr option =
        let segments = ResizeArray<FormatSeg>()
        let litRun = System.Text.StringBuilder()
        let mutable hasHole = false
        let mutable lowerable = true

        let flushLit () =
            if litRun.Length > 0 then
                segments.Add(FormatSeg.Lit(litRun.ToString()))
                litRun.Clear() |> ignore

        for part in parts do
            if lowerable then
                match part with
                // `%%` collapses to `%` (an interpolated string rides the same
                // PrintfFormat machinery as printf); escape sequences stay
                // verbatim — the unescaping gap `stitchLiteralString` /
                // `translatePrintfFormat` carry.
                | StringPart.Text t
                | StringPart.EscapeSequence t
                | StringPart.VerbatimEscapeQuote t -> litRun.Append((ctx.NameOf t).Replace("%%", "%")) |> ignore
                | StringPart.EscapePercent _ -> litRun.Append('%') |> ignore
                | StringPart.Expr(formatSpecifier = fs; lBrace = lBrace; expr = holeExpr; formatClause = fc) ->
                    hasHole <- true
                    let holeTy = typeOfKey ctx (CstKeys.ofExpr holeExpr)

                    match Unification.zonk holeTy with
                    // A free hole type can't pick an `AppendFormatted<T>` — bail.
                    | TyVar _ -> lowerable <- false
                    | zHoleTy ->
                        match tryInterpHoleSpec ctx fs fc with
                        | Some source ->
                            flushLit ()

                            // Source token for source maps: the specifier (`%d`) or
                            // format clause (`:fmt`) when present, else the opening
                            // brace (`FormatPlaceholder` carries no position).
                            let specTok =
                                match fs with
                                | ValueSome t -> t
                                | ValueNone ->
                                    match fc with
                                    | ValueSome c -> c
                                    | ValueNone -> lBrace

                            segments.Add(
                                FormatSeg.Hole(
                                    {
                                        Ty = zHoleTy
                                        Source = source
                                        Tok = specTok
                                    },
                                    translateExpr ctx holeExpr
                                )
                            )
                        | None -> lowerable <- false
                // A standalone `%spec`, orphan specifier, or lexer-error part has
                // interpolation-specific semantics we don't model — keep the whole
                // string on the literal-stitch fallback.
                | StringPart.FormatSpecifier _
                | StringPart.OrphanFormatSpecifier _
                | StringPart.InvalidText _ -> lowerable <- false

        if hasHole && lowerable then
            flushLit ()
            Some(TExpr.Format(FormatSink.ToString, EqArray.ofSeq segments, ty, tok))
        else
            None

    and private translateIdent
        (ctx: PassContext)
        (e: Expr<SyntaxToken>)
        (key: NodeKey)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr =
        match ctx.Bindings.Binding.TryGetValue key with
        | ValueSome rb -> TExpr.Var(rb.BindingSite, ty, tok)
        | ValueNone ->
            // No Binding entry => NameResolution resolved through the provider.
            // Multi-segment names are joined with `.` so `External` carries the
            // same key the provider sees.
            let name =
                match e with
                | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) -> li.Idents |> Seq.map ctx.NameOf |> String.concat "."
                | Expr.LongIdentOrOp(LongIdentOrOp.Op(IdentOrOp.ParenOp(opName = OpName.SymbolicOp op))) ->
                    // `(+)`-as-a-value: carry the operator's compiled name so the
                    // External matches what the provider (and codegen) key on.
                    match Desugar.symbolicOpCompiledName op.Token with
                    | ValueSome n -> n
                    | ValueNone -> ctx.NameOf(CstKeys.firstTokenOfExpr e)
                // `A.B.(+)` — qualified operator form: carry the same `A.B.op_Addition` key NameResolution resolved and
                // the provider keys on.
                | Expr.LongIdentOrOp(LongIdentOrOp.QualifiedOp(longIdent = li; op = idOp)) ->
                    match OperatorNames.qualifiedOpName ctx.NameOf li idOp with
                    | ValueSome n -> n
                    | ValueNone -> ctx.NameOf(CstKeys.firstTokenOfExpr e)
                | _ -> ctx.NameOf(CstKeys.firstTokenOfExpr e)

            // An own-class static-operator member used by value (`Set.(+)`) resolves
            // to that member, not the built-in operator: eta-expand to a closure
            // calling it, ahead of the generic `External` value path.
            match tryOwnOperatorValue ctx key name ty with
            | ValueSome lam -> lam
            | ValueNone ->
                // Stamp the resolved `SymbolKey.ValueKey` when NameResolution recorded
                // one (provider hit). Lets codegen distinguish a canonical
                // `Vesper.Printf.printfn` from a user shadow `MyMod.printfn` by
                // identity rather than name suffix.
                let symKey = ctx.Resolution.ExternalValue.TryGetValue key
                TExpr.External(name, symKey, ty, tok)

    /// Fold a multi-segment `r.X.Y…` LongIdent into nested `FieldGet` nodes. The
    /// head segment's TAST node is a `Var` pointing back at the local binding.
    and private translateLongIdentFieldChain
        (ctx: PassContext)
        (li: LongIdent<SyntaxToken>)
        (finalTy: SemType)
        (lastExternal: ResolvedExternalMember voption)
        (tok: SyntaxToken)
        : TExpr =
        let head = li.Idents.[0]
        let headKey = NodeKey.ofToken head NodeKind.ExprIdent
        let headBinding = ctx.Bindings.Binding.TryGetValue headKey

        let headTy =
            // Unification didn't allocate a side-table entry for the synthetic
            // head key, so fall back to the binding site's TyVar.
            match headBinding with
            | ValueSome rb -> typeOfKey ctx rb.BindingSite
            | ValueNone -> finalTy

        let headExpr =
            match headBinding with
            | ValueSome rb -> TExpr.Var(rb.BindingSite, headTy, tok)
            | ValueNone -> TExpr.External(ctx.NameOf head, ValueNone, headTy, tok)

        // Wall B (rung 3): the chain's *last* segment may be a property read on a
        // typar receiver constrained to an interface (`this.Source.Current` where
        // `Source : 'E :> IStructEnumerator`). Unification resolved it through the
        // interface and recorded the interface key in `TyparInterfaceCall`, keyed by
        // the LongIdent's NodeKey (the same `CstKeys.ofExpr` identity the inference
        // step used). The receiver never grounds to a nominal, so `fieldStep` would
        // emit a bogus `FieldGet` on an `FTTypar` — route it to a `CallVia.Interface`
        // `PropertyGet` (codegen → `constrained. callvirt get_<name>`) instead.
        let liKey =
            NodeKey.ofToken (CstKeys.firstTokenOfLongIdent li) NodeKind.ExprLongIdent

        let mutable currTy = headTy
        let mutable curr = headExpr

        for i = 1 to li.Idents.Length - 1 do
            let segName = ctx.NameOf li.Idents.[i]
            // Intermediate steps recover the segment's declared type from the
            // receiver — a record/union/class field, or a union/class *instance
            // member* return type (so a chain through a member returning a union,
            // `xs.Tail.Head`, keeps `xs.Tail : Lst<_>` instead of collapsing to the
            // chain's final type). The last step uses the whole chain's `finalTy`.
            let stepTy =
                if i = li.Idents.Length - 1 then
                    finalTy
                else
                    match recoverFieldStepTy ctx currTy segName with
                    | ValueSome t -> t
                    | ValueNone -> finalTy

            // PropertyGet for a class/union member (codegen calls its `get_<name>`,
            // eta-expanding a method-as-value if needed), FieldGet otherwise. The
            // last segment of an external instance access (`e.Current`) emits a
            // keyed `TExpr.ExternalMember` against the receiver built so far — the
            // BCL interface/class member-ref path, not a project-local field.
            curr <-
                match lastExternal with
                | ValueSome info when i = li.Idents.Length - 1 && not info.IsStatic ->
                    TExpr.ExternalMember(ValueSome curr, info.Key, segName, info.Storage, stepTy, tok)
                | _ ->
                    // The `TyparInterfaceCall` entry is keyed by the chain's first
                    // token, which a method call's receiver *prefix* (`this.Source` of
                    // `this.Source.MoveNext()`) shares with the full chain — so also
                    // require the receiver `currTy` to be a typar (the entry is only
                    // ever recorded for a typar receiver), distinguishing the genuine
                    // property read `this.Source.Current` (receiver `'E`) from a nominal
                    // field step `this.Source` (receiver the enclosing class).
                    let isTyparRecv =
                        match Unification.zonk currTy with
                        | TyTypar _
                        | TyVar _ -> true
                        | _ -> false

                    match
                        (if i = li.Idents.Length - 1 && isTyparRecv then
                             ctx.Resolution.TyparInterfaceCall.TryGetValue liKey
                         else
                             ValueNone)
                    with
                    | ValueSome(ifaceKey, ifaceArgs) ->
                        let key = LocalSymbolKey.ofMember ifaceKey segName 0 MemberKind.Property
                        TExpr.PropertyGet(curr, key, CallVia.Interface ifaceArgs, stepTy, tok)
                    | ValueNone -> fieldStep ctx curr currTy segName stepTy tok

            currTy <- stepTy

        curr

    /// A trailing optional argument the call omitted, synthesised as a literal node
    /// from the constant default `Unification` recorded in `ExternalOptionalFill`.
    and private optionalDefaultNode (cv: TConstValue) (tok: SyntaxToken) : TExpr =
        let ty =
            match cv with
            | TConstValue.Int _ -> BuiltinTypes.tyInt
            | TConstValue.UInt _ -> BuiltinTypes.tyUInt32
            | TConstValue.Int64 _ -> BuiltinTypes.tyInt64
            | TConstValue.Byte _ -> BuiltinTypes.tyByte
            | TConstValue.Float _ -> BuiltinTypes.tyFloat
            | TConstValue.Float32 _ -> BuiltinTypes.tyFloat32
            | TConstValue.Bool _ -> BuiltinTypes.tyBool
            | TConstValue.Char _ -> BuiltinTypes.tyChar
            | TConstValue.Decimal _ -> BuiltinTypes.tyDecimal
            | TConstValue.String _ -> BuiltinTypes.tyString
            | TConstValue.Unit -> BuiltinTypes.tyUnit

        TExpr.Const(cv, ty, tok)

    /// Dispatch an application head through the optional-argument fill iff
    /// `Unification.tryFillOptionalCall` recorded omitted trailing optionals for it.
    /// Both application arms (`Expr.App`'s tupled list and the residual single
    /// `Expr.HighPrecedenceApp`) consult this first so the "did this call omit
    /// optionals?" decision lives in one place; `ValueNone` ⇒ the arm's ordinary
    /// lowering runs unchanged. `head` is the already lowered application head.
    and private tryTranslateExternalOptionalFill
        (ctx: PassContext)
        (head: TExpr)
        (fnKey: NodeKey)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        (tok: SyntaxToken)
        : TExpr voption =
        match ctx.Resolution.ExternalOptionalFill.TryGetValue fnKey with
        | ValueSome omitted when not (List.isEmpty omitted) ->
            ValueSome(translateExternalOptionalCall ctx head fnKey args omitted tok)
        | _ -> ValueNone

    /// Lower an external method call that omitted a suffix of the member's trailing
    /// optional parameters (`Unification.tryFillOptionalCall` recorded the omitted
    /// constant defaults in `ExternalOptionalFill`). The supplied arguments are
    /// flattened, the recorded defaults appended as literal nodes, and the result
    /// re-tupled to the member's *full* arity — so codegen sees a fully applied
    /// tupled call and needs no optional-argument awareness. `head` is the already
    /// lowered `TExpr.ExternalMember`; its own type stays the full signature, so the
    /// backend recovers the complete member-ref.
    and private translateExternalOptionalCall
        (ctx: PassContext)
        (head: TExpr)
        (fnKey: NodeKey)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        (omitted: TConstValue list)
        (tok: SyntaxToken)
        : TExpr =
        let supplied =
            if args.Length = 1 then
                peelOneArg (translateExpr ctx) args.[0]
            else
                EqArray.ofSeq (seq { for a in args -> translateExpr ctx a })

        let defaults = [ for cv in omitted -> optionalDefaultNode cv tok ]
        let filled = (EqArray.toList supplied) @ defaults

        // The full tupled parameter domain (for the synthesised tuple's type and the
        // element-wise `obj` box) and the member's return type, off the recorded
        // signature.
        let fullDom, ret =
            match ctx.Resolution.ExternalAccess.TryGetValue fnKey with
            | ValueSome info ->
                match Unification.zonk info.Signature with
                | TyFun(d, r) -> d, r
                | other -> other, other
            | ValueNone -> BuiltinTypes.tyUnit, BuiltinTypes.tyUnit

        let argNode =
            match filled with
            | [ single ] -> wrapObjArg fullDom single
            | many ->
                let tuple = TExpr.Tuple(EqArray.ofList many, fullDom, tok)
                wrapObjArg fullDom tuple

        TExpr.App(head, argNode, ret, tok)

    and private translateApp
        (ctx: PassContext)
        (fn: Expr<SyntaxToken>)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        (tok: SyntaxToken)
        : TExpr =
        let mutable result = translateExpr ctx fn
        let fnKey = CstKeys.ofExpr fn

        match tryTranslateExternalOptionalFill ctx result fnKey args tok with
        | ValueSome node -> node
        | ValueNone ->

            let mutable currTy = typeOfKey ctx (CstKeys.ofExpr fn)

            // An external .NET method head reads its obj slots off the declared
            // signature Unification recorded (`externalHeadDom`); its node SemType is
            // the un-grounded applied shape, not the function type. The method consumes
            // the first spine arg (its tupled argument list); a project-local function
            // reads each obj parameter off the head's function type (`currTy`) instead.
            let externalDom = externalHeadDom ctx (CstKeys.ofExpr fn) result
            let mutable isFirst = true

            for a in args do
                let argT = translateExpr ctx a

                let paramTy, resTy =
                    match currTy with
                    | TyFun(p, r) -> p, r
                    | _ ->
                        failwithf
                            "Freeze.translateApp: expected function type for application, got %A (Unification bug or free TypeVar)"
                            currTy

                // Box a value / open-typar argument flowing into an `obj` parameter —
                // the implicit upcast made explicit.
                let argT =
                    match externalDom with
                    | ValueSome dom when isFirst -> wrapObjArg dom argT
                    | _ -> wrapObjArg paramTy argT

                result <- TExpr.App(result, argT, resTy, tok)
                currTy <- resTy
                isFirst <- false

            result

    /// Whether `%A` of an argument of this (zonked) type may lower to the structural
    /// engine. The runtime `%A` dispatcher is *total* and reflection-free: a
    /// Vesper-compiled record / DU renders via its synthesised `IStructuralFormattable`,
    /// a list / array / tuple via the `IEnumerable` / `ITuple` arm, a BCL scalar via
    /// `IFormattable`, and *anything else* — an `FSharpOption`, an arbitrary BCL class,
    /// a runtime-boxed polymorphic value — falls to the `value.ToString()` tail. So the
    /// engine can lower **every concrete nominal**; the maintainer decision is that a
    /// non-Vesper structural type degrades down the `%A` hierarchy to `.ToString()` on
    /// the engine rather than riding the FSharp.Core cold path, even where its bytes
    /// diverge from F#'s reflective `%A`. The only holes that stay off the engine are
    /// the ones the backend can't author an `AppendStructured<T>` type argument for: an
    /// unresolved nominal (`TyUnknown`), an anonymous union / type-level computation
    /// (external vocabulary that a real `%A` hole never carries). A hole the engine
    /// can't take forces the *whole* format cold (`translatePrintfFormat` returns
    /// `ValueNone`) — additive, no regression.
    and private structuredArgFaithful (ctx: PassContext) (localAsm: string option) (t: SemType) : bool =
        match t with
        | TyConst(name, args) ->
            // The array intrinsic (`'T[]` ≡ `TyConst("[]", [elem])`) renders via
            // the `IEnumerable` arm — faithful iff its element type is.
            if name = "[]" then
                EqArray.forall (structuredArgFaithful ctx localAsm) args
            // Numeric primitives carry the F# literal suffixes the engine reproduces
            // (`5L`, `1.5M`); `string` / `char` / `bool` are special-cased atoms. All
            // are leaf scalars, so any type argument means it isn't really one.
            elif
                RuntimeNames.numericTypeNames.Contains name
                || name = "string"
                || name = "char"
                || name = "bool"
            then
                args.Length = 0
            else
                false
        | TyTuple items -> EqArray.forall (structuredArgFaithful ctx localAsm) items
        // The cons-list still renders via the `IEnumerable` arm (it carries no
        // synthesised `Format`), so it stays faithful-iff-its-element-is. It
        // surfaces as a `TyUnion` in the self-host (the Vesper cons-list DU) but as a
        // `TyRecord` against the FSharp.Core contract (`list`1`), so accept both
        // shapes of the list keys.
        | TyUnion(key, args)
        | TyRecord(key, args) when RuntimeNames.isVesperListKey key || RuntimeNames.isFsharpCoreListKey key ->
            EqArray.forall (structuredArgFaithful ctx localAsm) args
        // Every nominal record / DU / class renders on the engine — a Vesper-compiled
        // type via its synthesised `IStructuralFormattable.Format` (step-3
        // `NominalEmit`), an `FSharpOption` / arbitrary BCL type via the dispatcher's
        // `IFormattable` / `IEnumerable` / `ToString` tail. We do NOT recurse into
        // fields: the gate is a cold-vs-engine switch, not a per-field renderer, and
        // the runtime dispatcher already routes each field (a Vesper field via its own
        // `IStructuralFormattable`, a BCL field via `ToString` / `IEnumerable`). The
        // backend authors `AppendStructured<T>` for any of these — a project-local
        // nominal off its emitted `TypeDef`, an external one off its `TypeRef` — so the
        // only reason to decline is a hole type the encoder can't author, handled by
        // the final arm. (`ctx` / `localAsm` are still threaded through the recursive
        // array / tuple / cons-list arms above.)
        | TyUnion _
        | TyRecord _
        | TyClass _ -> true
        // A polymorphic hole (`let f x = printfn "%A" x`) zonks to a still-free `TyVar`
        // here; `freeze` generalises it to a method typar (`FTTypar(Method, i)`), which
        // the CLR encoder maps to `!!i` and `appendStructured` authors as the
        // `AppendStructured<!!i>` type argument (verified: `let f x = printfn "%A" x`
        // emits cleanly). The runtime dispatcher recovers the boxed runtime type, so the
        // engine renders the argument whatever it turns out to be.
        | TyVar _ -> true
        // The residual shapes (`TyUnknown`, `TyOr`, `TyKeyOf` / `TyIndexedAccess` /
        // `TyConditional`, `TyEnum`) are either unresolved-nominal errors the front end
        // rejects before the backend, or external-vocabulary type-level constructs a
        // real `%A` hole never carries — the encoder can't author a type argument for
        // them, so they stay cold.
        | _ -> false

    /// Lower a marked printf call (`Unification.tryInferPrintfApp` recorded a
    /// `PrintfApp` sink for it) into a `TExpr.Format`, pairing each specifier
    /// with the next argument in spec order (the format is arg 0). The happy
    /// path therefore never produces a `New PrintfFormat` / `App printfn`.
    ///
    /// Returns `ValueNone` to *decline* the lowering — when a `%A` (`Structured`)
    /// hole's argument type isn't faithful on the step-2 engine
    /// (`structuredArgFaithful`); the caller then falls back to the standard
    /// external-call (FSharp.Core cold) path for the whole format.
    and private translatePrintfFormat
        (ctx: PassContext)
        (key: NodeKey)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr voption =
        let sink =
            match ctx.PrintfApp.TryGetValue key with
            | ValueSome s -> s
            | ValueNone -> failwithf "Freeze.translatePrintfFormat: no PrintfApp marker at %O" key

        // The format argument's positional index, recovered from the sink kind:
        // `fprintf`/`fprintfn` (writer sink) put a `TextWriter` at arg 0 and the
        // format at arg 1; every other family has the format at arg 0. Kept in
        // lockstep with `PrintfSpec.Family.FormatArgIndex` (the gate's `idx`).
        let idx =
            match sink with
            | PrintfSpec.PrintfSink.Writer _
            | PrintfSpec.PrintfSink.Builder -> 1
            | _ -> 0

        // This compilation's target assembly as a home-assembly `option` — a
        // project-local nominal key's home (so a `%A` of a locally-declared record /
        // DU lowers on the engine; an external one stays cold). `None` (front-end /
        // contract-scrape, `AssemblyName = ""`) ⇒ no type is treated as local.
        let localAsm = SymbolKeyOps.asmOf ctx.AssemblyName

        let parts =
            match args.[idx] with
            | Expr.String(parts = parts) -> parts
            | other -> failwithf "Freeze.translatePrintfFormat: format arg is not a string literal: %A" other

        let segments = ResizeArray<FormatSeg>()
        let litRun = System.Text.StringBuilder()

        let flushLit () =
            if litRun.Length > 0 then
                segments.Add(FormatSeg.Lit(litRun.ToString()))
                litRun.Clear() |> ignore

        // Holes consume the trailing args (the format is `args.[idx]`) in spec order.
        let mutable holeIdx = idx + 1
        // Set when a `%A` hole's argument type isn't faithful on the step-2 engine
        // (a record / DU / unknown). Forces the whole format onto the cold path.
        let mutable cold = false

        // Consume the args for a `%a`/`%t` callback hole and append its segment. The
        // printer callback (a `Vesper.Fun`, often a closure) is captured positionally,
        // then — for `%a` — the value it consumes. Application order is callback FIRST,
        // then value (see `PrintfSpec.argTypes`). No width/precision (F# `%a`/`%t` carry
        // none), and never cold (capture-first lowers on every resolved sink).
        let addCallbackSeg t holeForm hasValue =
            let callbackExpr = args.[holeIdx]
            holeIdx <- holeIdx + 1
            let callbackT = translateExpr ctx callbackExpr

            let valueExpr =
                if hasValue then
                    let v = args.[holeIdx]
                    holeIdx <- holeIdx + 1
                    ValueSome v
                else
                    ValueNone

            let valueT = valueExpr |> ValueOption.map (translateExpr ctx)

            // The spec `Ty` is not load-bearing for capture-first emit (the residue is
            // spliced as a literal): the `%a` value's zonked type, `unit` for `%t`.
            let specTy =
                match valueExpr with
                | ValueSome v -> Unification.zonk (typeOfKey ctx (CstKeys.ofExpr v))
                | ValueNone -> TyConst("unit", EqArray.empty)

            let spec =
                {
                    Ty = specTy
                    Source = HoleSpecSource.Classified holeForm
                    Tok = t
                }

            segments.Add(FormatSeg.CallbackHole(spec, callbackT, valueT))

        // Consume the args for a plain value hole (optionally star-dimensioned) and
        // append its `Hole` / `DynHole` segment. A star *width* (`%*d`, `%*A`) then a
        // star *precision* (`%.*f`, `%.*e`, `%.*A`) each consume a leading `int` arg,
        // evaluated before the value in curried application order (width first, then
        // precision — the source arg order). The happy-path marker guarantees full
        // application (`totalArity`), so the indices line up. Walk args by per-hole arity.
        let addValueSeg t holeForm (placeholder: FormatPlaceholder) =
            let widthExpr =
                if placeholder.Width = FormatDim.Star then
                    let w = translateExpr ctx args.[holeIdx]
                    holeIdx <- holeIdx + 1
                    ValueSome w
                else
                    ValueNone

            let precisionExpr =
                if placeholder.Precision = FormatDim.Star then
                    let pr = translateExpr ctx args.[holeIdx]
                    holeIdx <- holeIdx + 1
                    ValueSome pr
                else
                    ValueNone

            let argExpr = args.[holeIdx]
            holeIdx <- holeIdx + 1
            let argT = translateExpr ctx argExpr
            // Zonk before the faithfulness check: a union-case application
            // (`S 3`) leaves a metavar that only resolves to `TyUnion` after
            // zonking (a record literal is concrete immediately), and an
            // unzonked `TyVar` would wrongly read as non-faithful (cold).
            let holeTy = Unification.zonk (typeOfKey ctx (CstKeys.ofExpr argExpr))

            // `%A` of a non-engine-faithful arg (a non-Vesper structural type —
            // FSharpOption / a BCL type — or an unknown) can't be rendered by the
            // structural engine, so the hole stays off the `Format` path and the
            // generic printf call stands; every Vesper-compiled record / DU (local
            // or external) is faithful now that step-3 synthesises their `Format`.
            if
                placeholder.Type = FormatType.Structured
                && not (structuredArgFaithful ctx localAsm holeTy)
            then
                cold <- true

            let spec =
                {
                    Ty = holeTy
                    Source = HoleSpecSource.Classified holeForm
                    Tok = t
                }

            match widthExpr, precisionExpr with
            | ValueNone, ValueNone -> segments.Add(FormatSeg.Hole(spec, argT))
            | _ ->
                segments.Add(
                    FormatSeg.DynHole
                        {
                            Width = widthExpr
                            Precision = precisionExpr
                            Spec = spec
                            Value = argT
                        }
                )

        for part in parts do
            match part with
            | StringPart.Text t
            | StringPart.EscapeSequence t
            | StringPart.VerbatimEscapeQuote t ->
                // Verbatim source text (escape unescaping is a pre-existing gap
                // shared with `translateString`). `%%` is the printf escape for a
                // literal `%`; the lexer folds it into a raw `Text` part, and
                // there's no runtime format pass to collapse it, so collapse here.
                // A real specifier is its own `FormatSpecifier` part, so every `%`
                // in a raw run is half of a `%%` pair.
                litRun.Append((ctx.NameOf t).Replace("%%", "%")) |> ignore
            | StringPart.EscapePercent _ ->
                // `%%` denotes a literal `%`; no runtime format pass here, so
                // collapse now (the FSharp.Core path does it at runtime).
                litRun.Append('%') |> ignore
            | StringPart.FormatSpecifier t ->
                flushLit ()

                let placeholder =
                    match Lexing.parseFormatSpecifierView (ctx.ReadableOf t) with
                    | ValueSome p -> p
                    | ValueNone ->
                        failwith "Freeze.translatePrintfFormat: unparsable specifier (marker invariant broken)"

                // Classify once here (also validating the marker invariant: the
                // specifier must be one a backend renders faithfully). The node carries
                // the classified `HoleForm`, so no consumer re-derives it.
                let holeForm =
                    match PrintfHoleForm.tryClassify placeholder with
                    | ValueSome hf -> hf
                    | ValueNone ->
                        failwith "Freeze.translatePrintfFormat: unsupported specifier (marker invariant broken)"

                match holeForm with
                | PrintfHoleForm.HoleForm.Callback hasValue -> addCallbackSeg t holeForm hasValue
                | _ -> addValueSeg t holeForm placeholder
            | StringPart.Expr _
            | StringPart.OrphanFormatSpecifier _
            | StringPart.InvalidText _ ->
                failwith "Freeze.translatePrintfFormat: non-literal format part (marker invariant broken)"

        flushLit ()

        if cold then
            ValueNone
        else
            let formatSink =
                match sink with
                | PrintfSpec.PrintfSink.StdOut nl -> FormatSink.ToStdOut nl
                | PrintfSpec.PrintfSink.StdErr nl -> FormatSink.ToStdErr nl
                | PrintfSpec.PrintfSink.StringResult -> FormatSink.ToString
                // The writer expression is the leading arg 0 (the format is arg 1);
                // `newline` threads `fprintfn`'s trailing `\n` into `EmitFormat`.
                | PrintfSpec.PrintfSink.Writer nl -> FormatSink.ToWriter(translateExpr ctx args.[0], nl)
                // `bprintf`: the `StringBuilder` is the leading arg 0 (the format is
                // arg 1). No `bprintfn`, so `ToBuilder` carries no trailing newline.
                | PrintfSpec.PrintfSink.Builder -> FormatSink.ToBuilder(translateExpr ctx args.[0])

            ValueSome(TExpr.Format(formatSink, EqArray.ofSeq segments, ty, tok))

    /// Lower a fully-unapplied lowerable printf partial (`ctx.PrintfPartial` marked
    /// it) to a synthesised Vesper closure `fun h1 … hn -> Format(sink, …)`. Each
    /// hole becomes a fresh lambda parameter that the `Format` node's segment reads
    /// as a `Var`; the format's literal runs and per-hole `HoleForm` are baked in
    /// exactly as the happy path bakes them, so the closure's `Invoke` — the same
    /// `EmitFormat` unroll — produces byte-identical output. `ty` is the App node's
    /// type: the curried printer arrow `h1 -> … -> hn -> tail`, whose domains supply
    /// the parameter types (in specifier order) and whose tail is the `Format`
    /// result. `%A`/`%O` (and `%a`/`%t`) are excluded at the gate, so every hole has
    /// a concrete argument type. Unlike `translatePrintfFormat` this path never
    /// declines: with no `%A` hole there is no faithfulness question, and the marker
    /// invariant guarantees each specifier parses and classifies.
    and private translatePrintfPartial
        (ctx: PassContext)
        (key: NodeKey)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr =
        let sink =
            match ctx.PrintfPartial.TryGetValue key with
            | ValueSome s -> s
            | ValueNone -> failwithf "Freeze.translatePrintfPartial: no PrintfPartial marker at %O" key

        let parts =
            match args.[0] with
            | Expr.String(parts = parts) -> parts
            | other -> failwithf "Freeze.translatePrintfPartial: format arg is not a string literal: %A" other

        let segments = ResizeArray<FormatSeg>()
        let litRun = System.Text.StringBuilder()
        // The synthesised lambda parameters, one per hole, in specifier order.
        let parameters = ResizeArray<NodeKey * SemType * SyntaxToken>()

        let flushLit () =
            if litRun.Length > 0 then
                segments.Add(FormatSeg.Lit(litRun.ToString()))
                litRun.Clear() |> ignore

        // Peel one printer-arrow domain per hole (specifier order matches the
        // curried arrow order — `PrintfSpec.printerType` folds the hole types onto
        // the tail left-to-right). The running codomain after the last hole is the
        // tail (the `Format` result).
        let mutable runningTy = Unification.zonk ty

        for part in parts do
            match part with
            | StringPart.Text t
            | StringPart.EscapeSequence t
            | StringPart.VerbatimEscapeQuote t -> litRun.Append((ctx.NameOf t).Replace("%%", "%")) |> ignore
            | StringPart.EscapePercent _ -> litRun.Append('%') |> ignore
            | StringPart.FormatSpecifier t ->
                flushLit ()

                let placeholder =
                    match Lexing.parseFormatSpecifierView (ctx.ReadableOf t) with
                    | ValueSome p -> p
                    | ValueNone ->
                        failwith "Freeze.translatePrintfPartial: unparsable specifier (marker invariant broken)"

                let holeForm =
                    match PrintfHoleForm.tryClassify placeholder with
                    | ValueSome hf -> hf
                    | ValueNone ->
                        failwith "Freeze.translatePrintfPartial: unsupported specifier (marker invariant broken)"

                let holeTy, restTy =
                    match runningTy with
                    | TyFun(dom, cod) -> dom, cod
                    | _ ->
                        failwithf
                            "Freeze.translatePrintfPartial: printer type has fewer arrows than holes: %A"
                            (Unification.zonk ty)

                // A fresh parameter keyed off the specifier's own token offset —
                // distinct per hole (distinct source positions) and stable, so the
                // synthesised `Var` and `NamedSimple` binder agree.
                let paramKey = NodeKey.ofSynthetic t.StartIndex NodeKind.SynthLambdaBody
                parameters.Add(paramKey, holeTy, t)

                segments.Add(
                    FormatSeg.Hole(
                        {
                            Ty = holeTy
                            Source = HoleSpecSource.Classified holeForm
                            Tok = t
                        },
                        TExpr.Var(paramKey, holeTy, t)
                    )
                )

                runningTy <- Unification.zonk restTy
            | StringPart.Expr _
            | StringPart.OrphanFormatSpecifier _
            | StringPart.InvalidText _ ->
                failwith "Freeze.translatePrintfPartial: non-literal format part (marker invariant broken)"

        flushLit ()

        let formatSink =
            match sink with
            | PrintfSpec.PrintfSink.StdOut nl -> FormatSink.ToStdOut nl
            | PrintfSpec.PrintfSink.StdErr nl -> FormatSink.ToStdErr nl
            | PrintfSpec.PrintfSink.StringResult -> FormatSink.ToString
            // The 4a partial gate is `idx = 0`, so a writer / builder sink
            // (`fprintf` / `bprintf` partial, `idx = 1`) never reaches this path —
            // those stay cold.
            | PrintfSpec.PrintfSink.Writer _
            | PrintfSpec.PrintfSink.Builder ->
                failwith
                    "Freeze.translatePrintfPartial: writer/builder sink is not a partial-lowering shape (marker invariant broken)"

        // `runningTy` is now the tail; the `Format` node returns it.
        let mutable body = TExpr.Format(formatSink, EqArray.ofSeq segments, runningTy, tok)
        let mutable resultTy = runningTy

        // Wrap innermost-last so the outermost lambda's type is the whole printer
        // arrow (equal to `ty`), exactly as `translateFun` folds a source lambda.
        for i = parameters.Count - 1 downto 0 do
            let (pk, pty, ptok) = parameters.[i]
            let lamTy = TyFun(pty, resultTy)
            body <- TExpr.Lambda(TPat.NamedSimple(pk, pty, ptok), body, lamTy, ptok)
            resultTy <- lamTy

        body

    /// `((^T): (static member (+) : ^T * ^T -> ^T) (x, y))` — an SRTP member-trait
    /// call (the body of a `let inline` operator's `when ^T : ^T` static-opt clause,
    /// `ops-platform.fs`). Lower to a `TExpr.TraitCall` carrying the operand type
    /// (the operator's `^T`, taken from the first argument), the resolved compiled
    /// member name, and the peeled arguments. `Inline.substMapper` resolves it to a
    /// `StaticMethodCall` once `^T` is substituted to a concrete nominal at expansion.
    and private translateStaticMemberInvocation
        (ctx: PassContext)
        (argExpr: Expr<SyntaxToken>)
        (msig: MemberSig<SyntaxToken>)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr =
        let ident =
            match msig with
            | MemberSig.MethodOrPropSig(ident = ident)
            | MemberSig.PropSig(ident = ident) -> ident

        let memberName =
            match Desugar.opPatCompiledName ctx.NameOf ident with
            | ValueSome n -> n
            | ValueNone -> failwithf "Freeze: unsupported static-member-trait operator %A" ident

        let args = peelOneArg (translateExpr ctx) argExpr
        // The trait receiver is the operand type — the operator's `^T` typar, carried
        // on the first argument. Substitution at expansion rewrites it to the concrete
        // nominal and this node to a `StaticMethodCall`.
        let receiverTy = if args.Length > 0 then TastWalk.exprTy args.[0] else ty

        TExpr.TraitCall(receiverTy, memberName, args, ty, tok)

    and private translateInfix
        (ctx: PassContext)
        (key: NodeKey)
        (left: Expr<SyntaxToken>)
        (right: Expr<SyntaxToken>)
        (resultTy: SemType)
        (tok: SyntaxToken)
        : TExpr =
        match ctx.Desugared.TryGetValue key with
        | ValueSome(DesugaredForm.OpName name) ->
            // Reconstruct the operator's type from the resolved arms, not by
            // re-instantiating the scheme: re-instantiation would mint fresh
            // TypeVars the existing TyVar table doesn't link, so the External's
            // carried type wouldn't match the App chain's resolved arms.
            let leftTy = typeOfKey ctx (CstKeys.ofExpr left)
            let rightTy = typeOfKey ctx (CstKeys.ofExpr right)
            let partialTy = TyFun(rightTy, resultTy)
            let opTy = TyFun(leftTy, partialTy)
            let opExpr = TExpr.External(name, ValueNone, opTy, tok)
            let app1 = TExpr.App(opExpr, translateExpr ctx left, partialTy, tok)
            TExpr.App(app1, translateExpr ctx right, resultTy, tok)
        | ValueSome DesugaredForm.ConsExpr ->
            // `h :: t` → `UnionCons("Cons", [h; t])` against the resolved list
            // union — the same shape `[…]` literals lower to (one cons cell).
            let consName, _ = listCaseNames ctx resultTy
            TExpr.UnionCons(consName, EqArray.ofList [ translateExpr ctx left; translateExpr ctx right ], resultTy, tok)
        | ValueSome _
        | ValueNone ->
            // Desugar always attaches an OpName for an InfixApp key; reaching
            // here is a bug. Surface loudly.
            failwithf "Freeze: InfixApp at %O missing DesugaredForm entry" key

    and private translatePrefix
        (ctx: PassContext)
        (key: NodeKey)
        (operand: Expr<SyntaxToken>)
        (resultTy: SemType)
        (tok: SyntaxToken)
        : TExpr =
        match ctx.Desugared.TryGetValue key with
        | ValueSome(DesugaredForm.OpName "op_AddressOf") ->
            // `&local` → push the local's *address*. The operand is an addressable
            // mutable local (a `Var` bound to a slot); lower to an `ldloca`
            // intrinsic (mirroring PP2b's `ldobj` lowering), which codegen emits by
            // inspecting the inner `Var`'s slot instead of recurring (a recur would
            // `ldloc` the value). `resultTy` is the byref `TyConst("&", [elem])`.
            TExpr.ILIntrinsic("ldloca", ValueNone, EqArray.singleton (translateExpr ctx operand), resultTy, tok)
        | ValueSome(DesugaredForm.OpName name) ->
            // See translateInfix: reconstruct from the resolved operand + result
            // rather than re-instantiating the scheme.
            let operandTy = typeOfKey ctx (CstKeys.ofExpr operand)
            let opTy = TyFun(operandTy, resultTy)
            let opExpr = TExpr.External(name, ValueNone, opTy, tok)
            TExpr.App(opExpr, translateExpr ctx operand, resultTy, tok)
        | ValueSome _
        | ValueNone -> failwithf "Freeze: PrefixApp at %O missing DesugaredForm entry" key

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
            let arrayTy = TyConst(RuntimeNames.arrayName 1, EqArray.singleton elemTy)
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
            | ValueNone -> TExpr.Const(TConstValue.Unit, BuiltinTypes.tyUnit, tok)

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
