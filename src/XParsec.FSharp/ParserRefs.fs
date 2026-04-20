namespace XParsec.FSharp.Parser

open XParsec
open XParsec.FSharp.Lexer

[<AutoOpen>]
module internal ParserRefs =
    // Forward reference mechanism for recursive parsing
    // Breaks reference cycles in the parser definitions, allowing us to define
    // mutually recursive parsers across different modules/files.
    type FSRefParser<'T> = RefParser<'T, PositionedToken, ParseState, ReadableImmutableArray<PositionedToken>>

    let refObjectConstruction = FSRefParser<ObjectConstruction<SyntaxToken>>()
    let refType = FSRefParser<Type<SyntaxToken>>()
    let refMeasure = FSRefParser<Measure<SyntaxToken>>()
    let refExpr = FSRefParser<Expr<SyntaxToken>>()
    let refExprSeqBlock = FSRefParser<Expr<SyntaxToken>>()

    /// Seq-block expression body with optional trailing type annotation (F# `typedSeqExprBlock`).
    /// Used for grammar productions like match-clause RHS, lambda body, if/then/else branches, etc.
    let refTypedSeqExprBlock = FSRefParser<Expr<SyntaxToken>>()

    /// Expression parser bounded to stop before Arrow-level operators (->).
    /// Used in pattern guard context (when <expr> ->) so that '->' is not consumed
    /// as part of the guard expression and remains available for Rule.parse.
    let refExprGuard = FSRefParser<Expr<SyntaxToken>>()

    /// Expression parser that stops before sequential composition (semicolons/virtual seps).
    /// Used for while conditions, where `do` at the same indent must not be consumed as a
    /// `do expr` keyword expression via sequential composition.
    let refExprNoSeq = FSRefParser<Expr<SyntaxToken>>()

    /// Expression parser for parsing expressions in collection and record initializers,
    /// which should stop before consuming a semicolon.
    let refExprInRecords = FSRefParser<Expr<SyntaxToken>>()

    /// Atomic expression parser for use in IL intrinsics, where each argument
    /// is a standalone atomic expression (identifier, literal, parenthesized expr, etc.)
    let refExprAtomic = FSRefParser<Expr<SyntaxToken>>()

    /// Expression parser for IL intrinsic arguments: accepts dot chains, indexers,
    /// and type applications, but stops at juxtaposition so each whitespace-separated
    /// arg in `(# "op" a b c : ty #)` is captured as a distinct arg.
    let refExprILArg = FSRefParser<Expr<SyntaxToken>>()
