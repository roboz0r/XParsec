namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Immutable
open XParsec.FSharp
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.ElaboratePatterns
open XParsec.FSharp.SemanticAnalysis.ElaborateNominals
open XParsec.FSharp.SemanticAnalysis.ElaborateObjArgs
open XParsec.FSharp.SemanticAnalysis.ElaborateCalls
open XParsec.FSharp.SemanticAnalysis.ElaborateExprArgs

// Application lowering for the Elaborate pass: the general `App` chain walk, the
// residual single `HighPrecedenceApp`, the external optional-argument fill, SRTP
// member-trait invocations, and the desugared infix / prefix operator forms.

module internal ElaborateApply =

    /// A trailing optional argument the call omitted, synthesised from the `ExternalOptionalFill`
    /// entry. The fill is post-inference and never re-unified, so an `Omitted` slot types as
    /// `undefined` rather than `unit`.
    let optionalDefaultNode (ctx: PassContext) (d: OptionalDefault) (tok: SyntaxToken) : TExpr =
        match d with
        // JS-only: `undefined` has no CLR contract and only the TS provider mints it.
        | OptionalDefault.Omitted ->
            TExpr.ILIntrinsic("undefined", ValueNone, EqArray.empty, ctx.Intrinsics.Undefined, tok)
        | OptionalDefault.Const cv ->
            match cv with
            | TConstValue.Unit -> TExpr.Const(cv, ctx.Intrinsics.Unit, tok)
            | TConstValue.Integral(k, _) -> TExpr.Const(cv, ctx.Intrinsics.OfIntKind k, tok)
            | TConstValue.Float _ -> TExpr.Const(cv, ctx.Intrinsics.Float, tok)
            | TConstValue.Float32 _ -> TExpr.Const(cv, ctx.Intrinsics.Float32, tok)
            | TConstValue.Bool _ -> TExpr.Const(cv, ctx.Intrinsics.Bool, tok)
            | TConstValue.Char _ -> TExpr.Const(cv, ctx.Intrinsics.Char, tok)
            | TConstValue.Decimal _ -> TExpr.Const(cv, ctx.Intrinsics.Decimal, tok)
            | TConstValue.String _ -> TExpr.Const(cv, ctx.Intrinsics.String, tok)

    /// Lower an external method call that omitted a suffix of the member's trailing
    /// optional parameters. The supplied arguments are flattened, the recorded defaults
    /// appended, and the result re-tupled to the member's *full* arity.
    let private translateExternalOptionalCall
        (translateExpr: TranslateExpr)
        (ctx: PassContext)
        (fn: TExpr)
        (fnKey: NodeKey)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        (omitted: OptionalDefault list)
        (tok: SyntaxToken)
        : TExpr =
        let supplied =
            if args.Length = 1 then
                peelOneArg (translateExpr ctx) args.[0]
            else
                EqArray.ofSeq (seq { for a in args -> translateExpr ctx a })

        let defaults = [ for d in omitted -> optionalDefaultNode ctx d tok ]
        let filled = (EqArray.toList supplied) @ defaults

        // The full tupled parameter domain (the synthesised tuple's type, and the
        // element-wise `obj` box) and the member's return type, off the signature.
        let fullDom, ret =
            match ctx.Resolution.ExternalAccess.TryGetValue fnKey with
            | ValueSome info ->
                match Unification.zonk ctx.Store info.Signature with
                | TyFun(d, r) -> d, r
                | other -> other, other
            | ValueNone -> ctx.Intrinsics.Unit, ctx.Intrinsics.Unit

        let argNode =
            match filled with
            | [ single ] -> wrapObjArg ctx.Store fullDom single
            | many ->
                let tuple = TExpr.Tuple(EqArray.ofList many, fullDom, tok)
                wrapObjArg ctx.Store fullDom tuple

        TExpr.App(fn, argNode, ret, tok)

    /// Dispatch a call through the optional-argument fill iff Unification recorded omitted
    /// trailing optionals for it; `ValueNone` ⇒ the arm's ordinary lowering runs unchanged.
    /// `fn` is the already lowered applied function.
    let private tryTranslateExternalOptionalFill
        (translateExpr: TranslateExpr)
        (ctx: PassContext)
        (fn: TExpr)
        (fnKey: NodeKey)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        (tok: SyntaxToken)
        : TExpr voption =
        match ctx.Resolution.ExternalOptionalFill.TryGetValue fnKey with
        | ValueSome omitted when not (List.isEmpty omitted) ->
            ValueSome(translateExternalOptionalCall translateExpr ctx fn fnKey args omitted tok)
        | _ -> ValueNone

    let translateApp
        (translateExpr: TranslateExpr)
        (ctx: PassContext)
        (fn: Expr<SyntaxToken>)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        (tok: SyntaxToken)
        : TExpr =
        let mutable result = translateExpr ctx fn
        let fnKey = CstKeys.ofExpr fn

        match tryTranslateExternalOptionalFill translateExpr ctx result fnKey args tok with
        | ValueSome node -> node
        | ValueNone ->

            let mutable currTy = typeOfKey ctx (CstKeys.ofExpr fn)

            // An external .NET method reads its `obj` slots off the declared
            // signature, since its node SemType is the un-grounded applied shape, not a
            // function type. It consumes the FIRST argument; a local one reads `currTy`.
            let externalDom = externalFnDom ctx (CstKeys.ofExpr fn) result
            let mutable isFirst = true
            // The member's own argument is the FIRST one; residual application applies to
            // its RESULT, so the opened `let`s wrap the whole application and keep it outermost.
            let mutable opened = []

            for a in args do
                let argT = translateExpr ctx a

                let paramTy, resTy =
                    match currTy with
                    | TyFun(p, r) -> p, r
                    | _ ->
                        failwithf
                            "Elaborate.translateApp: expected function type for application, got %A (Unification bug or free TypeVar)"
                            currTy

                // A tuple-VALUED argument at a multi-parameter member is opened to one
                // expression per declared parameter before anything reads it positionally.
                let argT =
                    if isFirst then
                        match openTupledMemberArg ctx result argT with
                        | ValueSome o ->
                            result <- o.Fn
                            opened <- o.Binds
                            o.Arg
                        | ValueNone -> argT
                    else
                        argT

                // Box a value / open-typar argument passed to an `obj` parameter.
                let argT =
                    match externalDom with
                    | ValueSome dom when isFirst -> wrapObjArg ctx.Store dom argT
                    | _ -> wrapObjArg ctx.Store paramTy argT

                result <- TExpr.App(result, argT, resTy, tok)
                currTy <- resTy
                isFirst <- false

            wrapOpenedBinds opened result

    /// A residual single application (an external .NET method reached as a folded
    /// LongIdent, a local function value, …). An external method reads its `obj` slot
    /// off the recorded signature; everything else off the applied function's own type.
    let translateHighPrecedenceApp
        (translateExpr: TranslateExpr)
        (ctx: PassContext)
        (fn: Expr<SyntaxToken>)
        (arg: Expr<SyntaxToken>)
        (ty: SemType)
        (tok: SyntaxToken)
        : TExpr =
        let fnT = translateExpr ctx fn
        let fnKey = CstKeys.ofExpr fn

        match tryTranslateExternalOptionalFill translateExpr ctx fnT fnKey (ImmutableArray.Create arg) tok with
        // An external method call that omitted trailing optionals (the call parses
        // as a high-precedence application of the folded LongIdent).
        | ValueSome node -> node
        | ValueNone ->

            let argT = translateExpr ctx arg

            let paramTy =
                match externalFnDom ctx fnKey fnT with
                | ValueSome _ as dom -> dom
                | ValueNone ->
                    match Unification.zonk ctx.Store (TastWalk.exprTy fnT) with
                    | TyFun(p, _) -> ValueSome p
                    | _ -> ValueNone

            // A tuple-VALUED argument opens to one expression per declared parameter,
            // and the object argument binds ahead of it.
            let opened, fnT, argT =
                match openTupledMemberArg ctx fnT argT with
                | ValueSome o -> o.Binds, o.Fn, o.Arg
                | ValueNone -> [], fnT, argT

            let argT =
                match paramTy with
                | ValueSome p -> wrapObjArg ctx.Store p argT
                | ValueNone -> argT

            wrapOpenedBinds opened (TExpr.App(fnT, argT, ty, tok))

    /// `((^T1 or ^T2): (static member (+) : ^T1 * ^T2 -> ^T3) (x, y))` — an SRTP
    /// member-trait call. `TExpr.TraitCall` carries ONE support type, the LEFT operand, so
    /// a right-operand-only member (`int * Vector -> Vector`) does NOT resolve.
    let translateStaticMemberInvocation
        (translateExpr: TranslateExpr)
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
            match OperatorNames.ofPatOp ctx.NameOf ident with
            | ValueSome n -> n
            | ValueNone -> failwithf "Elaborate: unsupported static-member-trait operator %A" ident

        let args = peelOneArg (translateExpr ctx) argExpr
        // Substitution at expansion rewrites `^T1` to the concrete nominal, and this
        // node to a `StaticMethodCall`.
        let supportTy = if args.Length > 0 then TastWalk.exprTy args.[0] else ty

        TExpr.TraitCall(supportTy, memberName, args, ty, tok)

    let translateInfix
        (translateExpr: TranslateExpr)
        (ctx: PassContext)
        (key: NodeKey)
        (left: Expr<SyntaxToken>)
        (right: Expr<SyntaxToken>)
        (resultTy: SemType)
        (tok: SyntaxToken)
        : TExpr =
        match ctx.Desugared.TryGetValue key with
        | ValueSome(DesugaredForm.OpName name) ->
            // Reconstruct the operator's type from the resolved arms: re-instantiating
            // the scheme would mint fresh TypeVars the TyVar table does not link, so
            // the `External`'s carried type would not match the App chain's arms.
            let leftTy = typeOfKey ctx (CstKeys.ofExpr left)
            let rightTy = typeOfKey ctx (CstKeys.ofExpr right)
            let partialTy = TyFun(rightTy, resultTy)
            let opTy = TyFun(leftTy, partialTy)
            // Unification stamped the resolved operator identity under this InfixApp
            // key; carry it so the contract's `let inline` body splices by KEY.
            // `ValueNone` ⇒ it did not resolve, which Unification already diagnosed.
            let opKey = ctx.Resolution.IntrinsicKey.TryGetValue key
            let opExpr = TExpr.External(name, opKey, opTy, tok)
            let app1 = TExpr.App(opExpr, translateExpr ctx left, partialTy, tok)
            TExpr.App(app1, translateExpr ctx right, resultTy, tok)
        | ValueSome DesugaredForm.ConsExpr ->
            // `h :: t` → `UnionCons("Cons", [h; t])` against the resolved list
            // union, the same shape `[…]` literals lower to (one cons cell).
            let consName, _ = listCaseNames ctx resultTy
            TExpr.UnionCons(consName, EqArray.ofList [ translateExpr ctx left; translateExpr ctx right ], resultTy, tok)
        | ValueSome _
        | ValueNone ->
            // Desugar always attaches an `OpName` for an InfixApp key.
            failwithf "Elaborate: InfixApp at %O missing DesugaredForm entry" key

    let translatePrefix
        (translateExpr: TranslateExpr)
        (ctx: PassContext)
        (key: NodeKey)
        (operand: Expr<SyntaxToken>)
        (resultTy: SemType)
        (tok: SyntaxToken)
        : TExpr =
        match ctx.Desugared.TryGetValue key with
        | ValueSome(DesugaredForm.OpName OperatorData.OpAddressOf) ->
            // `&local` → push the local's *address*. Codegen emits `ldloca` by
            // inspecting the inner `Var`'s slot rather than recurring, since a recur
            // would `ldloc` the value. `resultTy` is `TyConst("byref", [elem])`.
            TExpr.ILIntrinsic("ldloca", ValueNone, EqArray.singleton (translateExpr ctx operand), resultTy, tok)
        | ValueSome(DesugaredForm.OpName name) ->
            // Reconstruct from the resolved operand + result, not from the scheme.
            let operandTy = typeOfKey ctx (CstKeys.ofExpr operand)
            let opTy = TyFun(operandTy, resultTy)
            // Carry the resolved prefix-operator identity so the body splices by KEY.
            let opKey = ctx.Resolution.IntrinsicKey.TryGetValue key
            let opExpr = TExpr.External(name, opKey, opTy, tok)
            TExpr.App(opExpr, translateExpr ctx operand, resultTy, tok)
        | ValueSome _
        | ValueNone -> failwithf "Elaborate: PrefixApp at %O missing DesugaredForm entry" key
