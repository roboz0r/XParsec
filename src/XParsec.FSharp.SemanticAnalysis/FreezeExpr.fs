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

        match e with
        | Expr.Const c -> TExpr.Const(parseConst ctx c, ty)
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
            translateLongIdentFieldChain ctx li ty (ctx.Resolution.ExternalAccess.TryGetValue key)
        // Static member on an *external* type reached through a folded LongIdent
        // (`System.Console.Out`, `Console.Out`) — Unification resolved the prefix
        // as a type and recorded the member in `ExternalAccess`. Emit the same
        // keyed `TExpr.ExternalMember` as the generic `DotLookup` form; always
        // static, so the type-name receiver is dropped.
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) & ExternalAccess ctx info when li.Idents.Length >= 2 ->
            let memberName = ctx.NameOf li.Idents.[li.Idents.Length - 1]
            TExpr.ExternalMember(ValueNone, info.Key, memberName, info.IsProperty, ty)
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

            mkNew ctx className ty (peelOneArg (translateExpr ctx) argExpr)
        // Class-name-as-function application: `Point(3, 4)` parses as
        // `Expr.App (Ident Point, [EnclosedBlock(Tuple)])`.
        | Expr.App(ClassRef ctx className, args) -> mkNew ctx className ty (peelCtorArgs (translateExpr ctx) args)
        | Expr.HighPrecedenceApp(funcExpr = ClassRef ctx className; argExpr = arg) ->
            mkNew ctx className ty (peelOneArg (translateExpr ctx) arg)
        // Class instance method invocation: `r.M(args)` →
        // `App(DotLookup(r, ., M), args)`.
        | Expr.App(funcExpr = InstanceMethodCall ctx (r, declKey, memberName); argExprs = args) ->
            let receiver = translateExpr ctx r
            mkMethodCall ctx receiver declKey memberName (peelCtorArgs (translateExpr ctx) args) ty
        | Expr.HighPrecedenceApp(funcExpr = InstanceMethodCall ctx (r, declKey, memberName); argExpr = arg) ->
            let receiver = translateExpr ctx r
            mkMethodCall ctx receiver declKey memberName (peelOneArg (translateExpr ctx) arg) ty
        // `p.M(args)` parses as `App` / `HighPrecedenceApp` whose fn is
        // `Expr.LongIdentOrOp(LongIdent [p; M])` — the parser folds the dot into
        // the long ident rather than emitting `DotLookup` when the head is a
        // regular identifier. Fold to MethodCall.
        | Expr.App(
            funcExpr = Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(ClassTailMethod ctx (bindingSite,
                                                                                       receiverTy,
                                                                                       memberName)))
            argExprs = args) ->
            let receiver = TExpr.Var(bindingSite, receiverTy)
            mkMethodCall ctx receiver (nominalDeclKey receiverTy) memberName (peelCtorArgs (translateExpr ctx) args) ty
        | Expr.HighPrecedenceApp(
            funcExpr = Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(ClassTailMethod ctx (bindingSite,
                                                                                       receiverTy,
                                                                                       memberName)))
            argExpr = arg) ->
            let receiver = TExpr.Var(bindingSite, receiverTy)
            mkMethodCall ctx receiver (nominalDeclKey receiverTy) memberName (peelOneArg (translateExpr ctx) arg) ty
        // `p.X` (property) parses as `Expr.LongIdentOrOp(LongIdent[p; X])` when
        // the head is a regular identifier. Anything not a class property falls
        // to the chained FieldGet path below.
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(ClassTailProperty ctx (bindingSite, receiverTy, memberName))) ->
            let receiver = TExpr.Var(bindingSite, receiverTy)

            let key =
                LocalSymbolKey.ofMember (nominalDeclKey receiverTy) memberName 0 MemberKind.Property

            TExpr.PropertyGet(receiver, key, viaOfReceiver ctx receiver, ty)
        | Expr.App(
            funcExpr = Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(StaticMethod ctx (declKey, memberName)))
            argExprs = args) -> mkStaticMethodCall ctx declKey memberName (peelCtorArgs (translateExpr ctx) args) ty
        | Expr.HighPrecedenceApp(
            funcExpr = Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(StaticMethod ctx (declKey, memberName)))
            argExpr = arg) -> mkStaticMethodCall ctx declKey memberName (peelOneArg (translateExpr ctx) arg) ty
        // `ClassName<'args>.Method args` — static-method call on an explicitly
        // instantiated generic class (e.g. `Set<'T>.Singleton value`). The
        // `<'args>`-bearing receiver makes the funcExpr a `DotLookup` over a
        // `TypeApp` rather than a folded `LongIdent`; same `StaticMethodCall`
        // lowering as the folded `StaticMethod` arms above.
        | Expr.App(funcExpr = TypeAppStaticMember ctx (declKey, memberName, ClassMemberKind.Method); argExprs = args) ->
            mkStaticMethodCall ctx declKey memberName (peelCtorArgs (translateExpr ctx) args) ty
        | Expr.HighPrecedenceApp(
            funcExpr = TypeAppStaticMember ctx (declKey, memberName, ClassMemberKind.Method); argExpr = arg) ->
            mkStaticMethodCall ctx declKey memberName (peelOneArg (translateExpr ctx) arg) ty
        // `ClassName.X` — static property read (or method-as-value).
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent(StaticMember ctx (declKey, memberName))) ->
            let key = LocalSymbolKey.ofMember declKey memberName 0 MemberKind.Property
            TExpr.StaticPropertyGet(key, ty)
        | CtorRef ctx caseName ->
            // Bare or qualified ctor reference outside an App. v1 distinguishes
            // nullary ctor (→ `UnionCons`) from ctor-as-value (`let f = Circle`,
            // typed `TyFun(_, TyUnion _)` → External) by the result type.
            match Unification.zonk ty with
            | TyUnion(_, _) -> TExpr.UnionCons(caseName, EqArray.empty, ty)
            // Function-typed ctor-as-value; codegen can eta-expand to a
            // UnionCons lambda.
            | _ -> TExpr.External(caseName, ValueNone, ty)
        | Expr.Ident _
        | Expr.LongIdentOrOp _ -> translateIdent ctx e key ty
        | Expr.App(CtorRef ctx caseName, args) ->
            // Ctor application: `Circle 1.0` or `Rectangle(2.0, 3.0)`. F# treats
            // DU arguments as a single tuple; the TAST flattens it back to a
            // per-field list (the same peel the class-ctor arms use) so consumers
            // see the ctor's declared arity directly.
            mkUnionCons ctx caseName ty (peelCtorArgs (translateExpr ctx) args)
        | Expr.HighPrecedenceApp(funcExpr = CtorRef ctx caseName; argExpr = arg) ->
            mkUnionCons ctx caseName ty (peelOneArg (translateExpr ctx) arg)
        // Printf happy-path call, marked by `Unification.tryInferPrintfApp`. Must
        // lower to a `TExpr.Format` *before* the `App(printfn, New PrintfFormat …)`
        // projection below ever runs (vesper-printf-plan P1).
        | Expr.App(fn, args) when ctx.PrintfApp.ContainsKey key ->
            // The marker may still decline (a `%A` of a record / DU — gated until
            // step-3 synthesis); fall back to the standard external-call path,
            // which lowers to the FSharp.Core cold printf.
            match translatePrintfFormat ctx key args ty with
            | ValueSome node -> node
            | ValueNone -> translateApp ctx fn args
        | Expr.App(fn, args) -> translateApp ctx fn args
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
            let argT = translateExpr ctx arg

            let paramTy =
                match externalHeadDom ctx (CstKeys.ofExpr fn) fnT with
                | ValueSome _ as dom -> dom
                | ValueNone ->
                    match Unification.zonk (TastWalk.exprTy fnT) with
                    | TyFun(p, _) -> ValueSome p
                    | _ -> ValueNone

            let argT =
                match paramTy with
                | ValueSome p -> wrapObjArg p argT
                | ValueNone -> argT

            TExpr.App(fnT, argT, ty)
        | Expr.InfixApp(left, _, right) -> translateInfix ctx key left right ty
        | Expr.PrefixApp(_, operand) -> translatePrefix ctx key operand ty
        | Expr.Fun(argumentPats = argPats; expr = body) -> translateFun ctx argPats body
        | Expr.LetOrUse(keyword = kw; bindings = bindings; body = body) -> translateLet ctx kw bindings body
        | Expr.EnclosedBlock(lParen = ParenKind.List _; expr = inner) ->
            translateListLikeLiteral ctx ty false (listLiteralItems inner)
        | Expr.EnclosedBlock(lParen = ParenKind.Array _; expr = inner) ->
            translateListLikeLiteral ctx ty true (listLiteralItems inner)
        | Expr.EnclosedBlock(expr = inner) -> translateExpr ctx inner
        | Expr.IfThenElse(condition = cond; thenExpr = thenE; elifBranches = elifs; elseBranch = elseB) ->
            translateIfThenElse ctx cond thenE elifs elseB ty
        | Expr.Tuple(exprs = items) -> TExpr.Tuple(EqArray.ofSeq (seq { for x in items -> translateExpr ctx x }), ty)
        | Expr.Sequential(exprs = items) ->
            TExpr.Sequential(EqArray.ofSeq (seq { for x in items -> translateExpr ctx x }), ty)
        // The annotation has no runtime representation — it only constrained
        // types in Unification; the TAST carries the inferred type inline.
        | Expr.TypeAnnotation(expr = inner) -> translateExpr ctx inner
        // Casts carry the resolved node type (`ty`): the target type for
        // `:>` / `:?>`, and `bool` for `:?` — Unification validated the
        // coercion via `subsumes`, codegen emits the box / castclass / isinst.
        | Expr.StaticUpcast(expr = inner) -> TExpr.Upcast(translateExpr ctx inner, ty)
        | Expr.DynamicDowncast(expr = inner) -> TExpr.Downcast(translateExpr ctx inner, ty)
        | Expr.DynamicTypeTest(expr = inner) ->
            // `ty` is the `bool` result; the tested-against type was stashed by
            // Unification (`inferDynamicTypeTest`) keyed by this node.
            let testTy =
                match ctx.Resolution.TypeTestTargets.TryGetValue key with
                | ValueSome t -> t
                | ValueNone -> failwithf "Freeze: no recorded type-test target for %O" key

            TExpr.TypeTest(translateExpr ctx inner, testTy, ty)
        | Expr.EmptyBlock(lParen = ParenKind.List _) -> translateListLikeLiteral ctx ty false []
        | Expr.EmptyBlock(lParen = ParenKind.Array _) -> translateListLikeLiteral ctx ty true []
        | Expr.EmptyBlock _ -> unitConst ctx e
        | Expr.While(condition = cond; body = body) -> TExpr.While(translateExpr ctx cond, translateExpr ctx body, ty)
        | Expr.ForTo(ident = ident; startExpr = startE; endExpr = endE; body = body) ->
            let varKey = CstKeys.ofForToVar ident
            TExpr.ForTo(varKey, translateExpr ctx startE, translateExpr ctx endE, translateExpr ctx body, ty)
        | Expr.ForIn(pat = pat; enumerableExpr = src; body = body) ->
            // How the source yields its enumerator was resolved by Unification and
            // stashed by this node's key; absent ⇒ the §4.2 interface path (range
            // sources and IEnumerable<'T> sources alike).
            let enumerator =
                match ctx.Resolution.ForInShape.TryGetValue key with
                | ValueSome shape -> shape
                | ValueNone -> ForInEnumeratorG.Interface

            TExpr.ForIn(translatePat ctx pat, translateExpr ctx src, translateExpr ctx body, enumerator, ty)
        | Expr.String _ -> translateString ctx e ty
        | Expr.Match(matchExpr = scrutinee; rules = Rules(rules = rules)) ->
            TExpr.Match(translateExpr ctx scrutinee, translateRules ctx rules, ty)
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

            let scrutinee = TExpr.Var(paramKey, paramTy)
            let body = TExpr.Match(scrutinee, translateRules ctx rules, resultTy)
            TExpr.Lambda(TPat.NamedSimple(paramKey, paramTy), body, ty)
        | Expr.TryWith(expr = body; rules = Rules(rules = rules)) ->
            TExpr.TryWith(translateExpr ctx body, translateRules ctx rules, ty)
        | Expr.TryFinally(tryExpr = body; finallyExpr = finallyE) ->
            TExpr.TryFinally(translateExpr ctx body, translateExpr ctx finallyE, ty)
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
                TExpr.FieldSet(translateExpr ctx r, fieldName, translateExpr ctx right, ty)
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
                let setExpr = TExpr.External("SetArray", ValueNone, TyFun(arrTy, idxPartial))
                let app1 = TExpr.App(setExpr, translateExpr ctx arrE, idxPartial)
                let app2 = TExpr.App(app1, translateExpr ctx idxE, valuePartial)
                TExpr.App(app2, translateExpr ctx right, ty)
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
                        | ValueSome rb -> TExpr.Var(rb.BindingSite, headTy)
                        | ValueNone -> TExpr.External(ctx.NameOf head, ValueNone, headTy)

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

                        curr <- fieldStep ctx curr currTy segName stepTy
                        currTy <- stepTy

                    curr

                let lastName = ctx.NameOf receiverIdents.[lastIdx]
                TExpr.FieldSet(receiverChain, lastName, translateExpr ctx right, ty)
            | _ -> TExpr.Assignment(translateExpr ctx left, translateExpr ctx right, ty)
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

            TExpr.RecordCons(fields, ty)
        | Expr.RecordClone(expr = src; fieldInitializers = inits) ->
            let overrides =
                EqArray.ofSeq (
                    seq {
                        for FieldInitializer(longIdent = li; expr = e) in inits ->
                            let idents = li.Idents
                            ctx.NameOf idents.[idents.Length - 1], translateExpr ctx e
                    }
                )

            TExpr.RecordClone(translateExpr ctx src, overrides, ty)
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

            TExpr.ExternalMember(receiver, info.Key, memberName, info.IsProperty, ty)
        // `ClassName<'args>.Prop` — local static property read on an explicitly
        // instantiated generic class (e.g. `Set<'T>.Empty`). Same lowering as the
        // folded `ClassName.Member` form; the `<'args>` only pinned the generic
        // instantiation in inference and is carried on `ty`. The method form
        // (`Set<'T>.Singleton value`) is `App`-wrapped and handled with the other
        // static-method arms.
        | TypeAppStaticMember ctx (declKey, memberName, ClassMemberKind.Property) ->
            let key = LocalSymbolKey.ofMember declKey memberName 0 MemberKind.Property
            TExpr.StaticPropertyGet(key, ty)
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

                TExpr.PropertyGet(receiver, key, viaOfReceiver ctx receiver, ty)
            // `(expr).Length` on an intrinsic rank-1 array desugars to the core
            // `GetArrayLength` inline function — the `ldlen` mnemonic lives in
            // `ops-platform.fs`, spliced by `InlineExpansion`. Mirrors the
            // `fieldStep` array guard (the LongIdent-chain form).
            | TyConst(name, _) when name = RuntimeNames.arrayName 1 && memberName = "Length" ->
                TExpr.App(TExpr.External("GetArrayLength", ValueNone, TyFun(rTy, ty)), receiver, ty)
            | _ -> TExpr.FieldGet(receiver, memberName, ty)
        | Expr.Null _ -> TExpr.Null ty
        | Expr.Range(fromExpr = a; toExpr = b) -> TExpr.Range(translateExpr ctx a, None, translateExpr ctx b, ty)
        | Expr.SteppedRange(fromExpr = a; stepExpr = s; toExpr = b) ->
            TExpr.Range(translateExpr ctx a, Some(translateExpr ctx s), translateExpr ctx b, ty)
        // `arr.[i]` desugars to the core `GetArray` inline function (mirroring F#'s
        // `IntrinsicFunctions.GetArray`): the `ldelem` mnemonic lives in
        // Vesper.Core's `ops-platform.fs`, spliced at this use site by
        // `InlineExpansion` — never invented in this target-agnostic pass. Mirrors
        // the operator path (`translateInfix`): emit a curried `External` call whose
        // type is rebuilt from the resolved operand types. `ty` is the element type.
        | Expr.IndexedLookup(expr = r; indexExpr = idx) ->
            let arrTy = typeOfKey ctx (CstKeys.ofExpr r)
            let idxTy = typeOfKey ctx (CstKeys.ofExpr idx)
            let partialTy = TyFun(idxTy, ty)
            let getTy = TyFun(arrTy, partialTy)
            let getExpr = TExpr.External("GetArray", ValueNone, getTy)
            let app1 = TExpr.App(getExpr, translateExpr ctx r, partialTy)
            TExpr.App(app1, translateExpr ctx idx, ty)
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

                TExpr.ILIntrinsic("newarr", ValueSome elem, tArgs, ty)
            elif opCode.StartsWith "ldelem" then
                TExpr.ILIntrinsic("ldelem", ValueSome(Unification.zonk ty), tArgs, ty)
            elif opCode.StartsWith "stelem" then
                // `arr.[i] <- v` / `SetArray`. The store's result is `unit`, so the
                // element type is recovered from the value operand (the 3rd arg:
                // array, index, value), not the node's result type as `ldelem` does.
                let elem = Unification.zonk (typeOfKey ctx (CstKeys.ofExpr args.[2]))
                TExpr.ILIntrinsic("stelem", ValueSome elem, tArgs, ty)
            elif opCode.StartsWith "box" then
                // `box value` — the boxed element type is the *argument's* static
                // type (the result is always `obj`), so recover it from the single
                // value operand. A value type emits `box <T>`; a reference type's
                // box is the JIT-erased identity (codegen leaves it as `box`, which
                // the runtime treats as a no-op on a ref type).
                let elem = Unification.zonk (typeOfKey ctx (CstKeys.ofExpr args.[0]))
                TExpr.ILIntrinsic("box", ValueSome elem, tArgs, ty)
            else
                TExpr.ILIntrinsic(opCode, ValueNone, tArgs, ty)
        | Expr.StaticMemberInvocation(membersign = msig; expr = argExpr) ->
            translateStaticMemberInvocation ctx argExpr msig ty
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
            TExpr.StaticOptimization(EqArray.ofList clauses, defaultExpr, ty)
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

    and private translateString (ctx: PassContext) (e: Expr<SyntaxToken>) (ty: SemType) : TExpr =
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
                        TExpr.Const(TConstValue.String(stitchLiteralString ctx parts), BuiltinTypes.tyString)
                    ),
                    ty
                )
            | _ ->
                // A faithfully-renderable interpolation lowers to a `TExpr.Format`
                // (D9). Otherwise (plain string, or an unrenderable hole) stitch
                // the literal text, keeping any unrendered hole's `{<expr>}`
                // placeholder — additive over the pre-D9 behaviour.
                match tryTranslateInterpolation ctx parts ty with
                | Some node -> node
                | None -> TExpr.Const(TConstValue.String(stitchLiteralString ctx parts), ty)
        | _ -> failwithf "Freeze.translateString: not a String expr: %A" e

    /// Interpolation holes have no rendering on this path, so they surface as
    /// `{<expr>}` placeholders. Only reached for plain strings, printf format
    /// literals, and interpolations a hole kept off the `TExpr.Format` path.
    and private stitchLiteralString (ctx: PassContext) (parts: ImmutableArray<StringPart<SyntaxToken>>) : string =
        foldStringParts ctx (fun () -> "{<expr>}") parts

    /// Classify one interpolation hole into the `(HoleKind, .NET format,
    /// alignment)` triple a `FormatSeg.Hole` carries, or `None` if it can't be
    /// rendered faithfully. A printf-style `%d{x}` reuses
    /// `PrintfSpec.tryHoleFormat` (so it covers exactly the specifiers the printf
    /// happy path does); a plain `{x}` / `{x:fmt}` is a `Formatted` hole.
    /// Interpolation alignment (`{x,n}`) isn't representable here — the parser
    /// folds `x,n` into a tuple expression — so alignment is always `None` for
    /// the plain forms.
    and private tryInterpHoleSpec
        (ctx: PassContext)
        (formatSpecifier: SyntaxToken voption)
        (formatClause: SyntaxToken voption)
        : (PrintfSpec.HoleKind * string option * int option) option =
        match formatSpecifier with
        | ValueSome ft ->
            match Lexing.parseFormatSpecifierView (ctx.ReadableOf ft) with
            | ValueSome p ->
                match PrintfSpec.tryHoleFormat p with
                | ValueSome(k, f, a) -> Some(k, f, a)
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

            Some(PrintfSpec.HoleKind.Formatted, fmt, None)

    /// Lower an interpolated string ($"…{x}…") to a `TExpr.Format` (D9). Returns
    /// `None` — keeping the literal-stitch fallback — when the string has no
    /// holes, or any hole isn't faithfully renderable: a free (unresolved) hole
    /// type, an orphan/standalone `%spec` or lexer-error part, or a printf-typed
    /// `%d{x}` whose specifier the happy path doesn't cover.
    and private tryTranslateInterpolation
        (ctx: PassContext)
        (parts: ImmutableArray<StringPart<SyntaxToken>>)
        (ty: SemType)
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
                | StringPart.Expr(formatSpecifier = fs; expr = holeExpr; formatClause = fc) ->
                    hasHole <- true
                    let holeTy = typeOfKey ctx (CstKeys.ofExpr holeExpr)

                    match Unification.zonk holeTy with
                    // A free hole type can't pick an `AppendFormatted<T>` — bail.
                    | TyVar _ -> lowerable <- false
                    | zHoleTy ->
                        match tryInterpHoleSpec ctx fs fc with
                        | Some(kind, netFormat, alignment) ->
                            flushLit ()

                            segments.Add(
                                FormatSeg.Hole(
                                    {
                                        Ty = zHoleTy
                                        Kind = kind
                                        Format = netFormat
                                        Alignment = alignment
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
            Some(TExpr.Format(FormatSink.ToString, EqArray.ofSeq segments, ty))
        else
            None

    and private translateIdent (ctx: PassContext) (e: Expr<SyntaxToken>) (key: NodeKey) (ty: SemType) : TExpr =
        match ctx.Bindings.Binding.TryGetValue key with
        | ValueSome rb -> TExpr.Var(rb.BindingSite, ty)
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
                // `A.B.(+)` — qualified operator form (opens-overhaul-plan Gap 4):
                // carry the same `A.B.op_Addition` key NameResolution resolved and
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
                TExpr.External(name, symKey, ty)

    /// Fold a multi-segment `r.X.Y…` LongIdent into nested `FieldGet` nodes. The
    /// head segment's TAST node is a `Var` pointing back at the local binding.
    and private translateLongIdentFieldChain
        (ctx: PassContext)
        (li: LongIdent<SyntaxToken>)
        (finalTy: SemType)
        (lastExternal: ResolvedExternalMember voption)
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
            | ValueSome rb -> TExpr.Var(rb.BindingSite, headTy)
            | ValueNone -> TExpr.External(ctx.NameOf head, ValueNone, headTy)

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
                    TExpr.ExternalMember(ValueSome curr, info.Key, segName, info.IsProperty, stepTy)
                | _ -> fieldStep ctx curr currTy segName stepTy

            currTy <- stepTy

        curr

    and private translateApp
        (ctx: PassContext)
        (fn: Expr<SyntaxToken>)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        : TExpr =
        let mutable result = translateExpr ctx fn
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

            result <- TExpr.App(result, argT, resTy)
            currTy <- resTy
            isFirst <- false

        result

    /// Whether `%A` of an argument of this (zonked) type renders faithfully on the
    /// structural engine. Faithful shapes are: primitives, `string` / `char` /
    /// `bool` (special-cased), arrays and the cons-list (the `IEnumerable` arm),
    /// tuples (recursively over element/payload types), and every *Vesper-compiled*
    /// record / DU — local (home = the target assembly) or external (a `.Record` /
    /// `.Union` shape from a referenced Vesper package, which carries the same
    /// synthesised `Format`). A non-Vesper structural type (FSharpOption, a BCL
    /// type) is NOT faithful — it has no Vesper `Format`, so it stays on the
    /// reflective FSharp.Core cold path. A single non-faithful `%A` hole forces the
    /// *whole* format cold (`translatePrintfFormat` returns `ValueNone`) — additive,
    /// no regression.
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
        // Every Vesper-compiled record / DU — *local or external* — carries a
        // synthesised `IStructuralFormattable.Format` (step-3 `NominalEmit`), so the
        // engine renders it faithfully. We deliberately do NOT recurse into its
        // fields — the gate is only a cold-vs-engine switch, not a per-field
        // renderer, and the runtime dispatcher already routes each field correctly,
        // matching real F# `%A` in every reachable case:
        //   * a field that is another Vesper record / union (local *or* an external
        //     package) carries `IStructuralFormattable` too — every Vesper-compiled
        //     type does — so the dispatcher's interface arm renders it structurally,
        //     exactly as F#'s reflective `%A` recurses into any F#-reflectable type;
        //   * a BCL scalar / collection field hits the `ISpanFormattable` / `ToString`
        //     / `IEnumerable` arm — and F# `%A` `ToString`s / enumerates the same.
        // Recursing here would *regress* this: the BCL-leaf arms above return
        // non-faithful, so a recurse would force a record with one `System.Uri`
        // field onto the cold path even though F# and the engine render that field
        // identically (`ToString`).
        | TyUnion(key, _)
        | TyRecord(key, _) ->
            // Local types match the compilation's target assembly outright.
            if localAsm.IsSome && SymbolKeyOps.keyAsm key = localAsm then
                true
            else
                // An *external* record / DU is faithful iff it too was Vesper-compiled
                // — the engine needs no codegen change for it: the emitted
                // `AppendStructured<T>` is type-agnostic, the reflection-free runtime
                // dispatcher devirtualises on the `IStructuralFormattable` the type
                // implements, and the value's package is already a bundle dependency.
                // The discriminator is the resolved *shape*: the Vesper `.fsi`
                // extractor is the ONLY producer of `.Record` / `.Union` shapes — the
                // .NET metadata provider models every BCL nominal as `.Class` — so a
                // `.Record` / `.Union` shape uniquely marks a Vesper structural type.
                // FSharp.Core is excluded outright: even where a prim-types contract
                // models `option` as a `.Union`, the runtime `FSharpOption` carries no
                // Vesper `Format`, so it must stay on the reflective cold path.
                SymbolKeyOps.keyAsm key <> Some "FSharp.Core"
                && (
                    match ExternalSymbols.tryLookupType ctx.Provider key with
                    | ValueSome(ExternalTypeShape.Record _)
                    | ValueSome(ExternalTypeShape.Union _) -> true
                    | _ -> false
                )
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
        : TExpr voption =
        let sink =
            match ctx.PrintfApp.TryGetValue key with
            | ValueSome s -> s
            | ValueNone -> failwithf "Freeze.translatePrintfFormat: no PrintfApp marker at %O" key

        // This compilation's target assembly as a home-assembly `option` — a
        // project-local nominal key's home (so a `%A` of a locally-declared record /
        // DU lowers on the engine; an external one stays cold). `None` (front-end /
        // contract-scrape, `AssemblyName = ""`) ⇒ no type is treated as local.
        let localAsm = SymbolKeyOps.asmOf ctx.AssemblyName

        let parts =
            match args.[0] with
            | Expr.String(parts = parts) -> parts
            | other -> failwithf "Freeze.translatePrintfFormat: format arg is not a string literal: %A" other

        let segments = ResizeArray<FormatSeg>()
        let litRun = System.Text.StringBuilder()

        let flushLit () =
            if litRun.Length > 0 then
                segments.Add(FormatSeg.Lit(litRun.ToString()))
                litRun.Clear() |> ignore

        // Holes consume the trailing args (the format is arg 0) in spec order.
        let mutable holeIdx = 1
        // Set when a `%A` hole's argument type isn't faithful on the step-2 engine
        // (a record / DU / unknown). Forces the whole format onto the cold path.
        let mutable cold = false

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

                let kind, netFormat, alignment =
                    match PrintfSpec.tryHoleFormat placeholder with
                    | ValueSome(k, f, a) -> k, f, a
                    | ValueNone ->
                        failwith "Freeze.translatePrintfFormat: unsupported specifier (marker invariant broken)"

                let argExpr = args.[holeIdx]
                holeIdx <- holeIdx + 1
                let argT = translateExpr ctx argExpr
                // Zonk before the faithfulness check: a union-case application
                // (`S 3`) leaves a metavar that only resolves to `TyUnion` after
                // zonking (a record literal is concrete immediately), and an
                // unzonked `TyVar` would wrongly read as non-faithful (cold).
                let holeTy = Unification.zonk (typeOfKey ctx (CstKeys.ofExpr argExpr))

                // `%A` of a non-engine-faithful arg (a non-Vesper structural type —
                // FSharpOption / a BCL type — or an unknown) keeps the FSharp.Core
                // cold path; every Vesper-compiled record / DU (local or external) is
                // faithful now that step-3 synthesises their `Format`.
                if
                    kind = PrintfSpec.HoleKind.Structured
                    && not (structuredArgFaithful ctx localAsm holeTy)
                then
                    cold <- true

                segments.Add(
                    FormatSeg.Hole(
                        {
                            Ty = holeTy
                            Kind = kind
                            Format = netFormat
                            Alignment = alignment
                        },
                        argT
                    )
                )
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

            ValueSome(TExpr.Format(formatSink, EqArray.ofSeq segments, ty))

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

        TExpr.TraitCall(receiverTy, memberName, args, ty)

    and private translateInfix
        (ctx: PassContext)
        (key: NodeKey)
        (left: Expr<SyntaxToken>)
        (right: Expr<SyntaxToken>)
        (resultTy: SemType)
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
            let opExpr = TExpr.External(name, ValueNone, opTy)
            let app1 = TExpr.App(opExpr, translateExpr ctx left, partialTy)
            TExpr.App(app1, translateExpr ctx right, resultTy)
        | ValueSome DesugaredForm.ConsExpr ->
            // `h :: t` → `UnionCons("Cons", [h; t])` against the resolved list
            // union — the same shape `[…]` literals lower to (one cons cell).
            let consName, _ = listCaseNames ctx resultTy
            TExpr.UnionCons(consName, EqArray.ofList [ translateExpr ctx left; translateExpr ctx right ], resultTy)
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
        : TExpr =
        match ctx.Desugared.TryGetValue key with
        | ValueSome(DesugaredForm.OpName name) ->
            // See translateInfix: reconstruct from the resolved operand + result
            // rather than re-instantiating the scheme.
            let operandTy = typeOfKey ctx (CstKeys.ofExpr operand)
            let opTy = TyFun(operandTy, resultTy)
            let opExpr = TExpr.External(name, ValueNone, opTy)
            TExpr.App(opExpr, translateExpr ctx operand, resultTy)
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
            let nil = TExpr.UnionCons(nilName, EqArray.empty, listTy)

            items
            |> List.foldBack (fun item acc ->
                TExpr.UnionCons(consName, EqArray.ofList [ translateExpr ctx item; acc ], listTy)
            )
            <| nil

        if isArray then
            let arrayTy = TyConst(RuntimeNames.arrayName 1, EqArray.singleton elemTy)
            // Codegen resolves `Array.ofList` against its target; alternate
            // targets are free to swap the wrapper. The BCL-only path recognises
            // this exact head and emits the array directly (no FSharp.Core).
            let opName = RuntimeNames.arrayOfListName
            let opTy = TyFun(listTy, arrayTy)
            TExpr.App(TExpr.External(opName, ValueNone, opTy), listExpr, arrayTy)
        else
            listExpr

    and private translateIfThenElse
        (ctx: PassContext)
        (cond: Expr<SyntaxToken>)
        (thenE: Expr<SyntaxToken>)
        (elifs: ImmutableArray<ElifBranch<SyntaxToken>>)
        (elseB: ElseBranch<SyntaxToken> voption)
        (resultTy: SemType)
        : TExpr =
        // Fold elifs right-to-left, each nested as the else-branch of the previous.
        // A missing else is `else ()` (F# spec): inference has already constrained
        // the then/elif branches and the whole expression to `unit`, so synthesize a
        // `unit` constant as the innermost else.
        let mutable nestedElse =
            match elseB with
            | ValueSome(ElseBranch(expr = e)) -> translateExpr ctx e
            | ValueNone -> TExpr.Const(TConstValue.Unit, BuiltinTypes.tyUnit)

        for i = elifs.Length - 1 downto 0 do
            let elifCond, elifThen =
                match elifs.[i] with
                | ElifBranch.Elif(condition = c; expr = e)
                | ElifBranch.ElseIf(condition = c; expr = e) -> c, e

            nestedElse <- TExpr.IfThenElse(translateExpr ctx elifCond, translateExpr ctx elifThen, nestedElse, resultTy)

        TExpr.IfThenElse(translateExpr ctx cond, translateExpr ctx thenE, nestedElse, resultTy)

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
            result <- TExpr.Lambda(tpat, result, lamTy)
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

            result <-
                if isUse then
                    // An external (BCL) binder's keyed `Dispose` is recorded by
                    // Unification under the head-pattern's key; a project-local binder
                    // has none and codegen takes the duck-typed direct call (§4.3).
                    let dispose = ctx.Resolution.UseDispose.TryGetValue(CstKeys.ofPat b.headPat)
                    TExpr.Use(tpat, valT, result, dispose, resultTy)
                else
                    TExpr.Let(tpat, valT, result, resultTy)

        result

    and translateBinding (ctx: PassContext) (b: Binding<SyntaxToken>) : TExpr =
        if b.argumentPats.IsEmpty then
            translateExpr ctx b.expr
        else
            // `let f x y = body` is `let f = fun x y -> body`.
            translateFun ctx b.argumentPats b.expr
