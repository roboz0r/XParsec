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

    /// Strip type/format suffixes the lexer leaves on a numeric token (e.g.
    /// `42L` -> "42", `0xFFuy` -> "0xFF", `1.5f` -> "1.5"). We only strip the
    /// suffixes that map to literal kinds Unification recognises; the
    /// remainder is fed to the corresponding BCL parser.
    let private stripSuffix (suffix: string) (text: string) =
        if text.EndsWith(suffix, System.StringComparison.OrdinalIgnoreCase) then
            text.Substring(0, text.Length - suffix.Length)
        else
            text

    let private parseConst (ctx: PassContext) (c: Constant<SyntaxToken>) : TConstValue =
        let parseLiteral (t: SyntaxToken) : TConstValue =
            let text = ctx.NameOf t

            match t.Token with
            | Token.KWTrue -> TConstValue.Bool true
            | Token.KWFalse -> TConstValue.Bool false
            | Token.NumIEEE64
            | Token.NumIEEE64Hex
            | Token.NumIEEE64Octal
            | Token.NumIEEE64Binary ->
                TConstValue.Float(System.Double.Parse(text, System.Globalization.CultureInfo.InvariantCulture))
            | Token.NumInt64
            | Token.NumInt64Hex
            | Token.NumInt64Octal
            | Token.NumInt64Binary -> TConstValue.Int64(System.Int64.Parse(stripSuffix "L" text))
            | Token.NumByte
            | Token.NumByteHex
            | Token.NumByteOctal
            | Token.NumByteBinary -> TConstValue.Byte(System.Byte.Parse(stripSuffix "uy" text))
            | _ ->
                // Falls through for NumInt32 family and anything Unification
                // hasn't classified — they're treated as plain ints.
                TConstValue.Int(Int32.Parse text)

        match c with
        | Constant.Literal t -> parseLiteral t
        | Constant.MeasuredLiteral(value = t) -> parseLiteral t

    /// Walk a CST pattern into a TPat. EnclosedBlock and Typed peel; Tuple
    /// recurses. Patterns Unification doesn't understand yet fall through
    /// loudly so the gap surfaces at translation time.
    let rec private translatePat (ctx: PassContext) (p: Pat<SyntaxToken>) : TPat =
        let key = CstKeys.ofPat p
        let ty = typeOfKey ctx key

        match p with
        | Pat.NamedSimple _ -> TPat.NamedSimple(key, ty)
        | Pat.Wildcard _ -> TPat.Wildcard ty
        | Pat.EnclosedBlock(pat = inner) -> translatePat ctx inner
        | Pat.Tuple(patterns = pats) -> TPat.Tuple([ for sub in pats -> translatePat ctx sub ], ty)
        | Pat.Const c -> TPat.Const(parseConst ctx c, ty)
        | Pat.As(pat = inner) ->
            // The `as`-name itself isn't surfaced in TPat yet — translate the
            // inner pattern and rely on downstream Var lookups to find the
            // alias via the CST + side tables.
            translatePat ctx inner
        | _ -> failwithf "Freeze.translatePat: TODO %A" p

    /// `()` literal. Distinct entry point because `Expr.EmptyBlock` carries
    /// `ParenKind` + closing token, not a `Constant`.
    let private unitConst (ctx: PassContext) (e: Expr<SyntaxToken>) : TExpr =
        let key = CstKeys.ofExpr e
        TExpr.Const(TConstValue.Unit, typeOfKey ctx key)

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
        | Expr.Sequential(exprs = items) -> TExpr.Sequential([ for x in items -> translateExpr ctx x ], ty)
        // The annotation only constrained types in Unification; the TAST
        // carries the inferred type inline, so the annotation node has no
        // runtime representation — return the (now-constrained) inner.
        | Expr.TypeAnnotation(expr = inner) -> translateExpr ctx inner
        | Expr.EmptyBlock _ -> unitConst ctx e
        | Expr.While(condition = cond; body = body) -> TExpr.While(translateExpr ctx cond, translateExpr ctx body, ty)
        | Expr.ForTo(ident = ident; startExpr = startE; endExpr = endE; body = body) ->
            let varKey = CstKeys.ofForToVar ident
            TExpr.ForTo(varKey, translateExpr ctx startE, translateExpr ctx endE, translateExpr ctx body, ty)
        | Expr.String _ -> translateString ctx e ty
        | Expr.Match(matchExpr = scrutinee; rules = Rules(rules = rules)) ->
            TExpr.Match(translateExpr ctx scrutinee, translateRules ctx rules, ty)
        | Expr.Function(rules = Rules(rules = rules)) ->
            // `function …` ~ `fun x -> match x with …`. The synthesised
            // parameter has no source token, so mint a synthetic key under
            // the function-keyword's offset and let the Match scrutinee
            // reference it.
            let funcKey = CstKeys.ofExpr e

            let paramKey = NodeKey.ofSynthetic funcKey.Offset NodeKind.SynthLambdaBody

            let paramTy, resultTy =
                match ty with
                | TyFun(p, r) -> p, r
                | _ -> failwithf "Freeze.Function: expected function type, got %A" ty

            let scrutinee = TExpr.Var(paramKey, paramTy)
            let body = TExpr.Match(scrutinee, translateRules ctx rules, resultTy)
            TExpr.Lambda(TPat.NamedSimple(paramKey, paramTy), body, ty)
        | _ ->
            // TODO: extend as the subset grows. Until then, surface the
            // unhandled case loudly rather than emitting a broken TExpr.
            failwithf "Freeze.translateExpr: TODO %A" e

    and private translateRules (ctx: PassContext) (rules: ImmutableArray<Rule<SyntaxToken>>) : TMatchArm list =
        [
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
        ]

    and private translateString (ctx: PassContext) (e: Expr<SyntaxToken>) (ty: SemType) : TExpr =
        // Tiny subset: stitch together the source text of every Text /
        // EscapeSequence / VerbatimEscapeQuote part. Interpolation holes are
        // not yet rendered — surface them as `{<expr>}` placeholders so test
        // output stays deterministic.
        match e with
        | Expr.String(parts = parts) ->
            let sb = System.Text.StringBuilder()

            for part in parts do
                match part with
                | StringPart.Text t
                | StringPart.EscapeSequence t
                | StringPart.FormatSpecifier t
                | StringPart.EscapePercent t
                | StringPart.VerbatimEscapeQuote t -> sb.Append(ctx.NameOf t) |> ignore
                | StringPart.Expr _ -> sb.Append("{<expr>}") |> ignore
                | StringPart.OrphanFormatSpecifier t -> sb.Append(ctx.NameOf t) |> ignore
                | StringPart.InvalidText t -> sb.Append(ctx.NameOf t) |> ignore

            TExpr.Const(TConstValue.String(sb.ToString()), ty)
        | _ -> failwithf "Freeze.translateString: not a String expr: %A" e

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
            let tpat = translatePat ctx p
            let pTy = typeOfKey ctx (CstKeys.ofPat p)
            let lamTy = TyFun(pTy, resultTy)
            result <- TExpr.Lambda(tpat, result, lamTy)
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
                let tpat = translatePat ctx b.headPat
                let valT = translateBinding ctx b
                result <- TExpr.Let(tpat, valT, result, resultTy)

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
                    let tpat = translatePat ctx b.headPat
                    let valT = translateBinding ctx b
                    TDecl.Let(tpat, valT, typeOfKey ctx (CstKeys.ofBinding b))
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
