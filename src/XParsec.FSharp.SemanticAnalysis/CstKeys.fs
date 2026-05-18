namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Parser

// Map CST nodes to their NodeKey. Each Expr / Pat DU case has a corresponding
// NodeKind tag; the NodeKey is (firstToken.StartIndex, kind).
//
// Cases outside the tiny subset fall through to failwith — extend the match
// arms as the constraint generator grows to handle more node kinds.

module CstKeys =

    let firstTokenOfLongIdent (li: LongIdent<SyntaxToken>) : SyntaxToken = li.Idents.[0]

    let firstTokenOfIdentOrOp (op: IdentOrOp<SyntaxToken>) : SyntaxToken =
        match op with
        | IdentOrOp.Ident t -> t
        | IdentOrOp.ParenOp(lParen, _, _) -> lParen
        | IdentOrOp.StarOp(lParen, _, _) -> lParen

    let firstTokenOfLongIdentOrOp (lio: LongIdentOrOp<SyntaxToken>) : SyntaxToken =
        match lio with
        | LongIdentOrOp.LongIdent li -> firstTokenOfLongIdent li
        | LongIdentOrOp.Op op -> firstTokenOfIdentOrOp op
        | LongIdentOrOp.QualifiedOp(li, _, _) -> firstTokenOfLongIdent li

    let firstTokenOfConstant (c: Constant<SyntaxToken>) : SyntaxToken =
        match c with
        | Constant.Literal t -> t
        | Constant.MeasuredLiteral(value = t) -> t

    let firstTokenOfParenKind (pk: ParenKind<SyntaxToken>) : SyntaxToken =
        match pk with
        | ParenKind.Paren t
        | ParenKind.BeginEnd t
        | ParenKind.List t
        | ParenKind.Array t
        | ParenKind.Brace t
        | ParenKind.BraceBar t
        | ParenKind.Quoted t
        | ParenKind.DoubleQuoted t -> t

    let firstTokenOfLetOrUseKeyword (kw: LetOrUseKeyword<SyntaxToken>) : SyntaxToken =
        match kw with
        | LetOrUseKeyword.Let t
        | LetOrUseKeyword.LetBang t
        | LetOrUseKeyword.Use t
        | LetOrUseKeyword.UseBang t -> t

    let rec firstTokenOfExpr (e: Expr<SyntaxToken>) : SyntaxToken =
        match e with
        | Expr.Const c -> firstTokenOfConstant c
        | Expr.Ident t -> t
        | Expr.LongIdentOrOp lio -> firstTokenOfLongIdentOrOp lio
        | Expr.App(funcExpr, _) -> firstTokenOfExpr funcExpr
        | Expr.HighPrecedenceApp(funcExpr = funcExpr) -> firstTokenOfExpr funcExpr
        // Use the operator token (not the left expr) so nested same-kind
        // InfixApps in left-assoc chains and operator-precedence stacks
        // don't collide on NodeKey. See docs/nodekey.md.
        | Expr.InfixApp(_, op, _) -> op
        | Expr.PrefixApp(op, _) -> op
        | Expr.Range(fromExpr = fromE) -> firstTokenOfExpr fromE
        | Expr.SteppedRange(fromExpr = fromE) -> firstTokenOfExpr fromE
        | Expr.Null(nullToken = t) -> t
        | Expr.LetOrUse(keyword = kw) -> firstTokenOfLetOrUseKeyword kw
        | Expr.Fun(funToken = t) -> t
        | Expr.EnclosedBlock(lParen = pk) -> firstTokenOfParenKind pk
        | Expr.EmptyBlock(lParen = pk) -> firstTokenOfParenKind pk
        | Expr.IfThenElse(ifToken = t) -> t
        | Expr.Tuple(exprs = exprs) when exprs.Length > 0 -> firstTokenOfExpr exprs.[0]
        | Expr.Sequential(exprs = exprs) when exprs.Length > 0 -> firstTokenOfExpr exprs.[0]
        | Expr.TypeAnnotation(expr = inner) -> firstTokenOfExpr inner
        | Expr.While(whileToken = t) -> t
        | Expr.ForTo(forToken = t) -> t
        | Expr.ForIn(forToken = t) -> t
        | Expr.Match(matchToken = t) -> t
        | Expr.Function(functionToken = t) -> t
        | Expr.TryWith(tryToken = t) -> t
        | Expr.TryFinally(tryToken = t) -> t
        | Expr.Assignment(arrow = t) -> t
        | Expr.String(kind = kind) ->
            match kind with
            | StringKind.String t
            | StringKind.VerbatimString t
            | StringKind.String3 t
            | StringKind.InterpolatedString t
            | StringKind.VerbatimInterpolatedString t
            | StringKind.Interpolated3String t -> t
        | Expr.DotLookup(expr = inner) -> firstTokenOfExpr inner
        | Expr.Record(lBrace = pk) -> firstTokenOfParenKind pk
        | Expr.RecordClone(lBrace = pk) -> firstTokenOfParenKind pk
        | Expr.New(newToken = t) -> t
        | _ -> failwithf "CstKeys.firstTokenOfExpr: TODO %A" e

    let rec firstTokenOfPat (p: Pat<SyntaxToken>) : SyntaxToken =
        match p with
        | Pat.Const c -> firstTokenOfConstant c
        | Pat.NamedSimple t -> t
        | Pat.Named(longIdent = li) -> firstTokenOfLongIdent li
        | Pat.Wildcard t -> t
        | Pat.EnclosedBlock(lParen = pk) -> firstTokenOfParenKind pk
        | Pat.EmptyBlock(lParen = pk) -> firstTokenOfParenKind pk
        | Pat.Tuple(patterns = pats) when pats.Length > 0 -> firstTokenOfPat pats.[0]
        | Pat.Typed(pat = inner) -> firstTokenOfPat inner
        | Pat.As(pat = inner) -> firstTokenOfPat inner
        | Pat.Or(left = inner) -> firstTokenOfPat inner
        | Pat.Record(lBrace = t) -> t
        | _ -> failwithf "CstKeys.firstTokenOfPat: TODO %A" p

    let ofExpr (e: Expr<SyntaxToken>) : NodeKey =
        let kind =
            match e with
            | Expr.Const _ -> NodeKind.ExprConst
            | Expr.Ident _ -> NodeKind.ExprIdent
            | Expr.LongIdentOrOp _ -> NodeKind.ExprLongIdent
            | Expr.App _ -> NodeKind.ExprApp
            | Expr.InfixApp _ -> NodeKind.ExprInfixApp
            | Expr.PrefixApp _ -> NodeKind.ExprPrefixApp
            | Expr.Fun _ -> NodeKind.ExprLambda
            | Expr.LetOrUse _ -> NodeKind.ExprLet
            | Expr.EnclosedBlock _ -> NodeKind.ExprEnclosedBlock
            | Expr.IfThenElse _ -> NodeKind.ExprIfThenElse
            | Expr.Tuple _ -> NodeKind.ExprTuple
            | Expr.Sequential _ -> NodeKind.ExprSequential
            | Expr.TypeAnnotation _ -> NodeKind.ExprTypeAnnotation
            | Expr.EmptyBlock _ -> NodeKind.ExprEmptyBlock
            | Expr.While _ -> NodeKind.ExprWhile
            | Expr.ForTo _ -> NodeKind.ExprForTo
            | Expr.ForIn _ -> NodeKind.ExprForIn
            | Expr.String _ -> NodeKind.ExprString
            | Expr.Match _ -> NodeKind.ExprMatch
            | Expr.Function _ -> NodeKind.ExprFunction
            | Expr.TryWith _ -> NodeKind.ExprTryWith
            | Expr.TryFinally _ -> NodeKind.ExprTryFinally
            | Expr.Assignment _ -> NodeKind.ExprAssignment
            | Expr.HighPrecedenceApp _ -> NodeKind.ExprHighPrecApp
            | Expr.Range _ -> NodeKind.ExprRange
            | Expr.SteppedRange _ -> NodeKind.ExprSteppedRange
            | Expr.Null _ -> NodeKind.ExprNull
            | Expr.DotLookup _ -> NodeKind.ExprDotLookup
            | Expr.Record _ -> NodeKind.ExprRecord
            | Expr.RecordClone _ -> NodeKind.ExprRecordClone
            | Expr.New _ -> NodeKind.ExprNew
            | _ -> NodeKind.Unknown

        NodeKey.ofToken (firstTokenOfExpr e) kind

    let ofPat (p: Pat<SyntaxToken>) : NodeKey =
        let kind =
            match p with
            | Pat.Const _ -> NodeKind.PatConst
            | Pat.NamedSimple _ -> NodeKind.PatIdent
            | Pat.Named _ -> NodeKind.PatLongIdent
            | Pat.Wildcard _ -> NodeKind.PatWildcard
            | Pat.EnclosedBlock _ -> NodeKind.PatEnclosedBlock
            | Pat.Tuple _ -> NodeKind.PatTuple
            | Pat.As _ -> NodeKind.PatAs
            | Pat.Typed _ -> NodeKind.PatTyped
            | Pat.Or _ -> NodeKind.PatOr
            | Pat.EmptyBlock _ -> NodeKind.PatEmptyBlock
            | Pat.Record _ -> NodeKind.PatRecord
            | _ -> NodeKind.Unknown

        NodeKey.ofToken (firstTokenOfPat p) kind

    /// A binding's identity is its headPat's NodeKey — that's the pattern
    /// that introduced the name(s) being bound.
    let ofBinding (b: Binding<SyntaxToken>) : NodeKey = ofPat b.headPat

    /// The loop variable of a `for i = …` introduces a binding whose site has
    /// no Pat wrapper in the CST — key on the ident token directly.
    let ofForToVar (ident: SyntaxToken) : NodeKey =
        NodeKey.ofToken ident NodeKind.PatForToVar
