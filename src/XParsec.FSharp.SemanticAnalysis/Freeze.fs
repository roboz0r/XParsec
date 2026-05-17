namespace XParsec.FSharp.SemanticAnalysis

open System
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes

// The only tree-to-tree transformation in the pipeline. Every previous pass
// annotated via side tables; freeze projects those annotations into a fresh
// TAST.
//
// Side tables can be discarded after this returns. The TAST is sharable;
// the CST + side tables are scoped to one compilation.

module Freeze =

    let private typeOfKey (ctx: PassContext) (key: NodeKey) : SemType =
        match ctx.TypeVar.TryGetValue key with
        | ValueSome tv -> Unification.zonk (TyVar tv)
        | ValueNone -> TyVar(TypeVar())

    let private parseConst (ctx: PassContext) (c: Constant<SyntaxToken>) : TConstValue =
        match c with
        | Constant.Literal t ->
            match t.Token with
            | Token.KWTrue -> TConstValue.Bool true
            | Token.KWFalse -> TConstValue.Bool false
            | _ ->
                // Tiny subset: every other literal is parsed as an int. Real
                // dispatch (NumFloat/NumInt64/NumByte/…) lands when those
                // become reachable.
                TConstValue.Int(Int32.Parse(ctx.NameOf t))
        | Constant.MeasuredLiteral(value = t) -> TConstValue.Int(Int32.Parse(ctx.NameOf t))

    let rec private translateExpr (ctx: PassContext) (e: Expr<SyntaxToken>) : TExpr =
        let key = CstKeys.ofExpr e
        let ty = typeOfKey ctx key

        match e with
        | Expr.Const c -> TExpr.Const(parseConst ctx c, ty)
        | Expr.Ident _
        | Expr.LongIdentOrOp _ -> translateIdent ctx e key ty
        | Expr.App(fn, args) -> translateApp ctx fn args
        | Expr.InfixApp(left, _, right) -> translateInfix ctx key left right ty
        | Expr.PrefixApp(_, operand) -> translatePrefix ctx key operand ty
        | Expr.Fun(argumentPats = argPats; expr = body) -> translateFun ctx argPats body
        | Expr.LetOrUse(bindings = bindings; body = body) -> translateLet ctx bindings body
        | Expr.EnclosedBlock(expr = inner) -> translateExpr ctx inner
        | Expr.IfThenElse(condition = cond; thenExpr = thenE; elifBranches = elifs; elseBranch = elseB) ->
            translateIfThenElse ctx cond thenE elifs elseB ty
        | Expr.Tuple(exprs = items) -> TExpr.Tuple([ for x in items -> translateExpr ctx x ], ty)
        | _ ->
            // TODO: extend as the subset grows. Until then, surface the
            // unhandled case loudly rather than emitting a broken TExpr.
            failwithf "Freeze.translateExpr: TODO %A" e

    and private translateIdent (ctx: PassContext) (e: Expr<SyntaxToken>) (key: NodeKey) (ty: SemType) : TExpr =
        match ctx.Binding.TryGetValue key with
        | ValueSome rb -> TExpr.Var(rb.BindingSite, ty)
        | ValueNone ->
            // No Binding entry => NameResolution resolved through the
            // provider. Re-query for the name; the symbol's type is the same
            // as `ty` for monomorphic primitives in the tiny subset.
            let name = ctx.NameOf(CstKeys.firstTokenOfExpr e)
            TExpr.External(name, ty)

    and private translateApp
        (ctx: PassContext)
        (fn: Expr<SyntaxToken>)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        : TExpr =
        let mutable result = translateExpr ctx fn
        let mutable currTy = typeOfKey ctx (CstKeys.ofExpr fn)

        for a in args do
            let argT = translateExpr ctx a

            let resTy =
                match currTy with
                | TyFun(_, r) -> r
                | _ ->
                    failwithf
                        "Freeze.translateApp: expected function type for application, got %A (Unification bug or free TypeVar)"
                        currTy

            result <- TExpr.App(result, argT, resTy)
            currTy <- resTy

        result

    and private translateInfix
        (ctx: PassContext)
        (key: NodeKey)
        (left: Expr<SyntaxToken>)
        (right: Expr<SyntaxToken>)
        (resultTy: SemType)
        : TExpr =
        match ctx.Desugared.TryGetValue key with
        | ValueSome(DesugaredForm.OpName name) ->
            let opTy =
                match ctx.Provider.TryLookup name with
                | ValueSome sym -> sym.Type
                | ValueNone -> failwithf "Freeze.translateInfix: provider has no entry for %s" name

            let partialTy =
                match opTy with
                | TyFun(_, r) -> r
                | _ -> failwithf "Freeze.translateInfix: operator %s has non-function type %A" name opTy

            let opExpr = TExpr.External(name, opTy)
            let app1 = TExpr.App(opExpr, translateExpr ctx left, partialTy)
            TExpr.App(app1, translateExpr ctx right, resultTy)
        | ValueNone ->
            // Either Desugar didn't recognise the operator (bug) or the
            // InfixApp is malformed. Surface loudly.
            failwithf "Freeze: InfixApp at %O missing DesugaredForm entry" key

    and private translatePrefix
        (ctx: PassContext)
        (key: NodeKey)
        (operand: Expr<SyntaxToken>)
        (resultTy: SemType)
        : TExpr =
        match ctx.Desugared.TryGetValue key with
        | ValueSome(DesugaredForm.OpName name) ->
            let opTy =
                match ctx.Provider.TryLookup name with
                | ValueSome sym -> sym.Type
                | ValueNone -> failwithf "Freeze.translatePrefix: provider has no entry for %s" name

            match opTy with
            | TyFun _ -> ()
            | _ -> failwithf "Freeze.translatePrefix: operator %s has non-function type %A" name opTy

            let opExpr = TExpr.External(name, opTy)
            TExpr.App(opExpr, translateExpr ctx operand, resultTy)
        | ValueNone -> failwithf "Freeze: PrefixApp at %O missing DesugaredForm entry" key

    and private translateIfThenElse
        (ctx: PassContext)
        (cond: Expr<SyntaxToken>)
        (thenE: Expr<SyntaxToken>)
        (elifs: ImmutableArray<ElifBranch<SyntaxToken>>)
        (elseB: ElseBranch<SyntaxToken> voption)
        (resultTy: SemType)
        : TExpr =
        let elseExpr =
            match elseB with
            | ValueSome(ElseBranch(expr = e)) -> e
            | ValueNone -> failwith "Freeze: if-then without else not yet supported"

        // Fold elifs right-to-left, nesting each as the else-branch of the
        // previous. Result is `if cond then thenE else (if c1 then e1 else (… else elseExpr))`.
        let mutable nestedElse = translateExpr ctx elseExpr

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
            let pKey = CstKeys.ofPat p
            let pTy = typeOfKey ctx pKey
            let lamTy = TyFun(pTy, resultTy)
            result <- TExpr.Lambda(pKey, result, lamTy)
            resultTy <- lamTy

        result

    and private translateLet
        (ctx: PassContext)
        (bindings: ImmutableArray<Binding<SyntaxToken>>)
        (body: Expr<SyntaxToken> voption)
        : TExpr =
        match body with
        | ValueSome bodyExpr ->
            let mutable result = translateExpr ctx bodyExpr
            let mutable resultTy = typeOfKey ctx (CstKeys.ofExpr bodyExpr)

            for i = bindings.Length - 1 downto 0 do
                let b = bindings.[i]
                let bKey = CstKeys.ofBinding b
                let valT = translateBinding ctx b
                result <- TExpr.Let(bKey, valT, result, resultTy)

            result
        | ValueNone ->
            // `Expr.LetOrUse(body = ValueNone)` is `use fixed`, which the
            // tiny subset doesn't support. Surface loudly when it appears.
            failwith "Freeze: Expr.LetOrUse with no body (UseFixed) not supported"

    and private translateBinding (ctx: PassContext) (b: Binding<SyntaxToken>) : TExpr =
        if b.argumentPats.IsEmpty then
            translateExpr ctx b.expr
        else
            // `let f x y = body` is `let f = fun x y -> body`.
            translateFun ctx b.argumentPats b.expr

    let private translateModuleElem (ctx: PassContext) (m: ModuleElem<SyntaxToken>) : TDecl list =
        match m with
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bindings)) ->
            [
                for b in bindings ->
                    let bKey = CstKeys.ofBinding b
                    let valT = translateBinding ctx b
                    TDecl.Let(bKey, valT, typeOfKey ctx bKey)
            ]
        | ModuleElem.Expression e ->
            let eT = translateExpr ctx e
            [ TDecl.Expression(eT, typeOfKey ctx (CstKeys.ofExpr e)) ]
        | _ -> []

    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : TastFile =
        let decls =
            match file with
            | ImplementationFile.AnonymousModule elems -> elems |> Seq.collect (translateModuleElem ctx) |> List.ofSeq
            | ImplementationFile.NamedModule(NamedModule.NamedModule(elements = elems)) ->
                elems |> Seq.collect (translateModuleElem ctx) |> List.ofSeq
            | ImplementationFile.Namespaces _ -> []

        {
            Decls = decls
            Diagnostics = List.ofSeq ctx.Diagnostics
        }
