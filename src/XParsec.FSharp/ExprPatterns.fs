namespace XParsec.FSharp.Parser

open XParsec.FSharp.Lexer

/// Active patterns that recover the legacy dedicated cases (Lazy, Assert, Fixed,
/// Upcast, Downcast, ExpressionSplice, WeaklyTypedExpressionSplice) from the
/// unified Expr.PrefixApp encoding. The prefix token is preserved on the node,
/// so semantic analysis still has full information.
module ExprPatterns =

    let inline private kwPrefix (kw: Token) (e: Expr<SyntaxToken>) =
        match e with
        | Expr.PrefixApp(t, inner) when t.Token = kw -> ValueSome(struct (t, inner))
        | _ -> ValueNone

    let (|LazyExpr|_|) (e: Expr<SyntaxToken>) = kwPrefix Token.KWLazy e
    let (|AssertExpr|_|) (e: Expr<SyntaxToken>) = kwPrefix Token.KWAssert e
    let (|FixedExpr|_|) (e: Expr<SyntaxToken>) = kwPrefix Token.KWFixed e
    let (|UpcastExpr|_|) (e: Expr<SyntaxToken>) = kwPrefix Token.KWUpcast e
    let (|DowncastExpr|_|) (e: Expr<SyntaxToken>) = kwPrefix Token.KWDowncast e

    // OpSplice / OpSpliceUntyped both share OpFamily.OpGeneric, so the enum
    // match alone catches the shared generic-operator slot. Disambiguate via
    // the token's string span — these are the only legal lex of `%` / `%%` as
    // a prefix operator.
    let (|ExpressionSplice|_|) (e: Expr<SyntaxToken>) =
        match e with
        | Expr.PrefixApp(t, inner) when t.Token = Token.OpSplice -> ValueSome(struct (t, inner))
        | _ -> ValueNone

    let (|WeaklyTypedExpressionSplice|_|) (e: Expr<SyntaxToken>) =
        match e with
        | Expr.PrefixApp(t, inner) when t.Token = Token.OpSpliceUntyped -> ValueSome(struct (t, inner))
        | _ -> ValueNone
