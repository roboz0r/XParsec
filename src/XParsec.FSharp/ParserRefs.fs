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
