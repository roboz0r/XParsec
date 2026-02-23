namespace XParsec.FSharp.Parser

open XParsec
open XParsec.FSharp.Lexer

[<AutoOpen>]
module internal ParserRefs =
    // Forward reference mechanism for recursive parsing
    // Breaks reference cycles in the parser definitions, allowing us to define
    // mutually recursive parsers across different modules/files.
    type FSRefParser<'T> =
        RefParser<
            'T,
            PositionedToken,
            ParseState,
            ReadableImmutableArray<PositionedToken>,
            ReadableImmutableArraySlice<PositionedToken>
         >

    let refObjectConstruction = FSRefParser<ObjectConstruction<SyntaxToken>>()
    let refType = FSRefParser<Type<SyntaxToken>>()
    let refExpr = FSRefParser<Expr<SyntaxToken>>()

    /// Expression parser bounded to stop before Arrow-level operators (->).
    /// Used in pattern guard context (when <expr> ->) so that '->' is not consumed
    /// as part of the guard expression and remains available for Rule.parse.
    let refExprGuard = FSRefParser<Expr<SyntaxToken>>()

    /// Expression parser for parsing expressions in collection and record initializers,
    /// which should stop before consuming a semicolon.
    let refExprInCollectionOrRecords = FSRefParser<Expr<SyntaxToken>>()
