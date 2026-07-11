namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.FreezePatterns
open XParsec.FSharp.SemanticAnalysis.FreezeResolve
open XParsec.FSharp.SemanticAnalysis.FreezeExprArgs

// Application lowering for the Freeze pass: the general `App` spine walk, the
// residual single `HighPrecedenceApp`, the external optional-argument fill both
// consult first, SRTP member-trait invocations, and the desugared infix /
// prefix operator forms.

module internal FreezeApply =

    /// A trailing optional argument the call omitted, synthesised from the constant
    /// default recorded in `ExternalOptionalFill`. A real constant default
    /// (`Int`/`String`/…) becomes its literal `Const`. The omitted-optional marker
    /// (`TConstValue.Unit`, minted only by the TS provider for a `T?` slot the call
    /// dropped) instead becomes an HONEST `undefined` value — a zero-operand
    /// `(# "undefined" #)` intrinsic the JS backend emits as bare `undefined` — NOT a
    /// `unit` `Const` exploiting the coincidental shared `unit`→`undefined` repr. The
    /// fill is post-inference (never re-unified against the parameter type), so typing
    /// the node `undefined` rather than `unit` is sound. (The CLR optional-fill path
    /// never mints `TConstValue.Unit`, so this case is JS-only.)
    let optionalDefaultNode (ctx: PassContext) (cv: TConstValue) (tok: SyntaxToken) : TExpr =
        match cv with
        // `undefined` is a JS-only intrinsic (no CLR contract). This fill is JS-only
        // (`TConstValue.Unit` is minted only by the TS provider), so the JS `undefined`
        // contract is always in scope and `ctx.Intrinsics.Undefined` resolves it.
        | TConstValue.Unit -> TExpr.ILIntrinsic("undefined", ValueNone, EqArray.empty, ctx.Intrinsics.Undefined, tok)
        | TConstValue.Int _ -> TExpr.Const(cv, ctx.Intrinsics.Int, tok)
        | TConstValue.UInt _ -> TExpr.Const(cv, ctx.Intrinsics.UInt32, tok)
        | TConstValue.Int64 _ -> TExpr.Const(cv, ctx.Intrinsics.Int64, tok)
        | TConstValue.Byte _ -> TExpr.Const(cv, ctx.Intrinsics.Byte, tok)
        | TConstValue.Float _ -> TExpr.Const(cv, ctx.Intrinsics.Float, tok)
        | TConstValue.Float32 _ -> TExpr.Const(cv, ctx.Intrinsics.Float32, tok)
        | TConstValue.Bool _ -> TExpr.Const(cv, ctx.Intrinsics.Bool, tok)
        | TConstValue.Char _ -> TExpr.Const(cv, ctx.Intrinsics.Char, tok)
        | TConstValue.Decimal _ -> TExpr.Const(cv, ctx.Intrinsics.Decimal, tok)
        | TConstValue.String _ -> TExpr.Const(cv, ctx.Intrinsics.String, tok)

    /// Lower an external method call that omitted a suffix of the member's trailing
    /// optional parameters (`Unification.tryFillOptionalCall` recorded the omitted
    /// constant defaults in `ExternalOptionalFill`). The supplied arguments are
    /// flattened, the recorded defaults appended as literal nodes, and the result
    /// re-tupled to the member's *full* arity — so codegen sees a fully applied
    /// tupled call and needs no optional-argument awareness. `head` is the already
    /// lowered `TExpr.ExternalMember`; its own type stays the full signature, so the
    /// backend recovers the complete member-ref.
    let private translateExternalOptionalCall
        (translateExpr: TranslateExpr)
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

        let defaults = [ for cv in omitted -> optionalDefaultNode ctx cv tok ]
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
            | ValueNone -> ctx.Intrinsics.Unit, ctx.Intrinsics.Unit

        let argNode =
            match filled with
            | [ single ] -> wrapObjArg fullDom single
            | many ->
                let tuple = TExpr.Tuple(EqArray.ofList many, fullDom, tok)
                wrapObjArg fullDom tuple

        TExpr.App(head, argNode, ret, tok)

    /// Dispatch an application head through the optional-argument fill iff
    /// `Unification.tryFillOptionalCall` recorded omitted trailing optionals for it.
    /// Both application arms (`Expr.App`'s tupled list and the residual single
    /// `Expr.HighPrecedenceApp`) consult this first so the "did this call omit
    /// optionals?" decision lives in one place; `ValueNone` ⇒ the arm's ordinary
    /// lowering runs unchanged. `head` is the already lowered application head.
    let private tryTranslateExternalOptionalFill
        (translateExpr: TranslateExpr)
        (ctx: PassContext)
        (head: TExpr)
        (fnKey: NodeKey)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        (tok: SyntaxToken)
        : TExpr voption =
        match ctx.Resolution.ExternalOptionalFill.TryGetValue fnKey with
        | ValueSome omitted when not (List.isEmpty omitted) ->
            ValueSome(translateExternalOptionalCall translateExpr ctx head fnKey args omitted tok)
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

    /// A residual single application (an external .NET method reached as a
    /// folded LongIdent, a local function value, a top-level `let f (x: obj)`
    /// emitted as a static method, …). Box a value arg flowing into an `obj`
    /// parameter — the implicit upcast, made explicit.
    /// An external method reads its `obj` slot off the declared signature
    /// Unification recorded (`externalMethodParamTy`, its node SemType is the
    /// un-grounded applied shape); everything else reads the parameter off
    /// the head's function type.
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

    /// `((^T1 or ^T2): (static member (+) : ^T1 * ^T2 -> ^T3) (x, y))` — an SRTP
    /// member-trait call (the body of a `let inline` operator's `when ^T1 : ^T1`
    /// static-opt clause, `ops-platform.fs`). Lower to a `TExpr.TraitCall` carrying the
    /// RECEIVER type, the resolved compiled member name, the peeled arguments, and the
    /// node's own `^T3` result type (which for a heterogeneous operator is neither
    /// operand's). `Inline.substMapper` resolves it to a `StaticMethodCall` once the
    /// typars are substituted to concrete types at expansion.
    ///
    /// The receiver is the LEFT operand: `TExpr.TraitCall` carries ONE receiver, so the
    /// `(^T1 or ^T2)` support set is searched left-only. A right-operand-only member
    /// (`int * Vector -> Vector`) therefore does not resolve — carrying a candidate SET
    /// is what would buy that.
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
            match Desugar.opPatCompiledName ctx.NameOf ident with
            | ValueSome n -> n
            | ValueNone -> failwithf "Freeze: unsupported static-member-trait operator %A" ident

        let args = peelOneArg (translateExpr ctx) argExpr
        // The trait receiver is the LEFT operand's type — the operator's `^T1` typar.
        // Substitution at expansion rewrites it to the concrete nominal and this node to
        // a `StaticMethodCall`.
        let receiverTy = if args.Length > 0 then TastWalk.exprTy args.[0] else ty

        TExpr.TraitCall(receiverTy, memberName, args, ty, tok)

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
            // Reconstruct the operator's type from the resolved arms, not by
            // re-instantiating the scheme: re-instantiation would mint fresh
            // TypeVars the existing TyVar table doesn't link, so the External's
            // carried type wouldn't match the App chain's resolved arms.
            let leftTy = typeOfKey ctx (CstKeys.ofExpr left)
            let rightTy = typeOfKey ctx (CstKeys.ofExpr right)
            let partialTy = TyFun(rightTy, resultTy)
            let opTy = TyFun(leftTy, partialTy)
            // Unification (`inferInfix`) stamped the resolved operator identity under
            // this InfixApp key; carry it so `InlineExpansion` splices the contract's
            // `let inline` body by KEY. Present for EVERY resolved operator, primitives
            // included — an operator that resolved at all carries its key. `ValueNone`
            // here means the operator did not resolve, which Unification already
            // diagnosed ("Unknown operator symbol").
            let opKey = ctx.Resolution.IntrinsicKey.TryGetValue key
            let opExpr = TExpr.External(name, opKey, opTy, tok)
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

    let translatePrefix
        (translateExpr: TranslateExpr)
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
            // `ldloc` the value). `resultTy` is the byref `TyConst("byref", [elem])`.
            TExpr.ILIntrinsic("ldloca", ValueNone, EqArray.singleton (translateExpr ctx operand), resultTy, tok)
        | ValueSome(DesugaredForm.OpName name) ->
            // See translateInfix: reconstruct from the resolved operand + result
            // rather than re-instantiating the scheme.
            let operandTy = typeOfKey ctx (CstKeys.ofExpr operand)
            let opTy = TyFun(operandTy, resultTy)
            // See translateInfix: carry the resolved prefix-operator identity stamped
            // by `inferPrefix` so the body splices by KEY.
            let opKey = ctx.Resolution.IntrinsicKey.TryGetValue key
            let opExpr = TExpr.External(name, opKey, opTy, tok)
            TExpr.App(opExpr, translateExpr ctx operand, resultTy, tok)
        | ValueSome _
        | ValueNone -> failwithf "Freeze: PrefixApp at %O missing DesugaredForm entry" key
