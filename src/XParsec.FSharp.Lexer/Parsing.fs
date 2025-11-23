namespace rec XParsec.FSharp.Parser

open System
open System.Collections.Generic
open System.Collections.Immutable
open XParsec
open XParsec.Parsers
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser.OperatorParsing

#nowarn "40" // recursive references

[<RequireQualifiedAccess>]
[<Struct>]
type TokenIndex =
    | Regular of int<token>
    | Virtual

[<Struct>]
type SyntaxToken =
    {
        PositionedToken: PositionedToken
        Index: TokenIndex
    }

    member this.StartIndex = this.PositionedToken.StartIndex
    member this.Token = this.PositionedToken.Token

// type ParserContext =
//     | InExpression
//     | InType
//     | InPattern
//     | InImport
//     | InMember
//     | InParameter
//     | InArgument
//     | InMatchCase
//     | InTuple
//     | InArrayOrList
//     | InRecord
//     | InUnionCase
//     | InAttribute
//     | InComputationExpression
//     | InInterpolation

type DiagnosticCode =
    // TODO: Use F# error codes
    | Other of string

type Diagnostic =
    {
        Code: DiagnosticCode
        Token: PositionedToken
        TokenEnd: PositionedToken option
    }

[<RequireQualifiedAccess>]
type Syntax =
    | Light
    | Verbose

type ParseState =
    {
        Lexed: Lexed
        // Context: Stack<ParserContext>
        Diagnostics: ImmutableArray<Diagnostic>.Builder
        mutable IndentDirective: Syntax
        mutable MinimumIndentationStack: int64 list
        mutable LastLine: int<line>
    }

    member this.MinimumIndentation =
        match this.MinimumIndentationStack with
        | [] -> 0L
        | x :: _ -> x

module ParseState =
    let create (lexed: Lexed) =
        {
            Lexed = lexed
            // Context = Stack<ParserContext>()
            Diagnostics = ImmutableArray.CreateBuilder()
            IndentDirective = Syntax.Light
            MinimumIndentationStack = []
            LastLine = 0<line>
        }

    let addDiagnostic code startToken endToken (state: ParseState) =
        state.Diagnostics.Add(
            {
                Code = code
                Token = startToken
                TokenEnd = endToken
            }
        )

    let findLineNumber (lexed: Lexed) (guess: int<line>) (index: int<token>) =
        if index < 0<token> || index >= lexed.Tokens.LengthM then
            invalidArg (nameof index) "Index out of range"

        let lineStarts = lexed.LineStarts
        let lineCount = lineStarts.LengthM

        // A recursive function to perform the binary search
        let rec search (low: int<line>) (high: int<line>) =
            if low >= high then
                // If the range is invalid, return the last known valid line,
                // which is 'high'. In a search for the floor, this will be the correct result.
                high
            else
                let mid = low + (high - low + 1<line>) / 2 // Using ceiling for mid to prevent infinite loops in some cases
                let midVal = lineStarts.[mid]

                if midVal > index then
                    // The token is in the lower half
                    search low (mid - 1<line>)
                else
                    // The token is in the upper half.
                    // mid is a potential answer, so we continue searching to the right
                    // to find the highest line number that is still less than or equal to the index.
                    search (mid + 1<line>) high

        // Use the guess to narrow the initial search range
        let low, high =
            if guess >= 0<line> && guess < lineCount then
                if
                    lineStarts.[guess] <= index
                    && (guess = lineCount - 1<line> || lineStarts.[guess + 1<line>] > index)
                then
                    // The guess is correct, we can determine the result directly
                    (guess, guess) // Set low and high to the same value to end the search
                else if lineStarts.[guess] < index then
                    // The target is after the guess
                    (guess, lineCount - 1<line>)
                else
                    // The target is before the guess
                    (0<line>, guess)
            else
                // Default to a full search if the guess is out of range
                (0<line>, lineCount - 1<line>)

        // If the guess was correct, low and high will be the same,
        // and the search will correctly return that value.
        search low high

    let rec getIndent (state: ParseState) (index: int<token>) =
        let currentLineTokenIndex = state.Lexed.LineStarts[state.LastLine]

        if index = currentLineTokenIndex then
            0L
        elif index > currentLineTokenIndex then
            let nextLine = state.LastLine + 1<_>

            if nextLine < state.Lexed.LineStarts.LengthM then
                let nextLineTokenIndex = state.Lexed.LineStarts[nextLine]

                if index < nextLineTokenIndex then
                    let token = state.Lexed.Tokens[index]
                    let lineStartToken = state.Lexed.Tokens[currentLineTokenIndex]
                    token.StartIndex - lineStartToken.StartIndex
                else
                    state.LastLine <- findLineNumber state.Lexed nextLine index
                    getIndent state index
            else
                // Last line
                let token = state.Lexed.Tokens[index]
                token.StartIndex - state.Lexed.Tokens[currentLineTokenIndex].StartIndex
        else
            state.LastLine <- findLineNumber state.Lexed (state.LastLine - 1<_>) index
            getIndent state index


[<AutoOpen>]
module Parsing =
    let tokenIndex (reader: Reader<PositionedToken, ParseState, 'a, 'b>) = int reader.Index * 1<token>

    let syntaxToken token (index: int64) =
        {
            PositionedToken = token
            Index = TokenIndex.Regular(int index * 1<token>)
        }

    let isTriviaToken (state: ParseState) (token: PositionedToken) =
        if token.InBlockComment || token.InOCamlBlockComment then
            true
        else
            match token.TokenWithoutCommentFlags with
            | Token.LineComment
            | Token.Indent
            | Token.Whitespace
            | Token.BlockCommentStart
            | Token.BlockCommentEnd
            | Token.KWStartFSharpBlockComment
            | Token.KWEndFSharpBlockComment
            | Token.KWStartOCamlBlockComment
            | Token.KWEndOCamlBlockComment
            | Token.Newline -> true
            | Token.Tab -> state.IndentDirective = Syntax.Verbose
            | _ -> false

    let rec nextNonTriviaToken (reader: Reader<PositionedToken, ParseState, 'a, 'b>) =
        // TODO: We also need to consider handling preprocessor directives here
        match reader.Peek() with
        | ValueNone -> fail EndOfInput reader
        | ValueSome token when isTriviaToken reader.State token ->
            reader.Skip()
            nextNonTriviaToken reader
        | ValueSome token ->
            let t = syntaxToken token reader.Index
            reader.Skip()
            preturn t reader

    let rec nextNonTriviaTokenVirtualIfNot t (reader: Reader<PositionedToken, ParseState, 'a, 'b>) =
        // TODO: We also need to consider handling preprocessor directives here
        match reader.Peek() with
        | ValueNone -> fail EndOfInput reader
        | ValueSome token when isTriviaToken reader.State token ->
            let indent = ParseState.getIndent reader.State (tokenIndex reader)
            reader.Skip()
            nextNonTriviaToken reader
        | ValueSome token ->
            if token.Token = t then
                preturn (syntaxToken token reader.Index) reader
            else
                let pt =
                    PositionedToken.Create(
                        Token.ofUInt16 (uint16 t ||| TokenRepresentation.IsVirtual),
                        token.StartIndex
                    )

                preturn
                    {
                        PositionedToken = pt
                        Index = TokenIndex.Virtual
                    }
                    reader

    let rec nextNonTriviaTokenSatisfiesL (predicate: SyntaxToken -> bool) msg reader =
        match nextNonTriviaToken reader with
        | Error e -> Error e
        | Ok { Parsed = token } ->
            if predicate token then
                preturn token reader
            else
                fail (Message msg) reader

    let currentIndent (reader: Reader<PositionedToken, ParseState, 'a, 'b>) =
        let state = reader.State
        let index = int reader.Index * 1<token>
        let indent = ParseState.getIndent state index
        preturn indent reader

[<AutoOpen>]
module internal Helpers =
    let pushMinimumIndentationLevel (col: int64) (reader: Reader<PositionedToken, ParseState, 'a, 'b>) =
        let state = reader.State

        match state.IndentDirective with
        | Syntax.Verbose -> preturn () reader // No-op in verbose syntax
        | Syntax.Light ->
            state.MinimumIndentationStack <- col :: state.MinimumIndentationStack
            preturn () reader

    let popMinimumIndentationLevel (reader: Reader<PositionedToken, ParseState, 'a, 'b>) =
        let state = reader.State

        match state.IndentDirective with
        | Syntax.Verbose -> preturn () reader // No-op in verbose syntax
        | Syntax.Light ->
            match state.MinimumIndentationStack with
            | [] -> fail (Message "Indentation level stack underflow") reader
            | _ :: rest ->
                state.MinimumIndentationStack <- rest
                preturn () reader

[<AutoOpen>]
module internal Keywords =
    let isAccessModifier (token: SyntaxToken) =
        match token.Token with
        | Token.KWPrivate
        | Token.KWPublic
        | Token.KWInternal -> true
        | _ -> false

    let pMutable: Parser<SyntaxToken, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        nextNonTriviaTokenSatisfiesL (fun synTok -> synTok.Token = Token.KWMutable) "Expected 'mutable' keyword"

    let pAccessModifier: Parser<SyntaxToken, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        nextNonTriviaTokenSatisfiesL isAccessModifier "Expected access modifier (private, public or internal)"

    let pEquals: Parser<SyntaxToken, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        nextNonTriviaTokenSatisfiesL (fun synTok -> synTok.Token = Token.OpEquality) "Expected '=' symbol"

    let pIn: Parser<SyntaxToken, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        nextNonTriviaTokenVirtualIfNot Token.KWIn

    let pLet: Parser<SyntaxToken, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        // TODO: Where does VirtualLet get inserted?
        nextNonTriviaTokenSatisfiesL (fun synTok -> synTok.Token = Token.KWLet) "Expected 'let' keyword"

[<RequireQualifiedAccess>]
module Pat =
    let pNamedSimple: Parser<Pat<_>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        nextNonTriviaTokenSatisfiesL
            (fun synTok -> synTok.Token = Token.Identifier)
            "Expected identifier for simple pattern"
        |>> Pat.NamedSimple

    let parse: Parser<Pat<_>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        choiceL [ pNamedSimple ] "Pattern"

[<RequireQualifiedAccess>]
module ValueDefn =

    let parse: Parser<ValueDefn<_>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! mut = opt pMutable
            let! access = opt pAccessModifier
            let typarDefns = ValueNone // TODO: parse type parameter definitions
            let returnType = ValueNone // TODO: parse return type annotation
            let! pat = Pat.parse
            let! eq = pEquals
            let! expr = Expr.parse
            return ValueDefn(mut, access, pat, typarDefns, returnType, eq, expr)
        }

[<RequireQualifiedAccess>]
module Constant =
    let pLiteral: Parser<Constant<_>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        nextNonTriviaTokenSatisfiesL (fun synTok -> synTok.Token.IsNumeric) "Expected constant literal"
        |>> Constant.Literal

    let parse: Parser<Constant<_>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        choiceL [ pLiteral ] "Constant"

[<RequireQualifiedAccess>]
module Expr =

    type FSharpOperatorParser() =
        let completeInfix (l: Expr<_>) (op: SyntaxToken) (r: Expr<_>) = Expr.InfixApp(l, op, r)
        let completePrefix (op: SyntaxToken) (e: Expr<_>) = Expr.PrefixApp(op, e)

        interface Operators<SyntaxToken, unit, Expr<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<PositionedToken>, ReadableImmutableArraySlice<PositionedToken>> with
            member _.LhsParser =
                parser {
                    let! token = nextNonTriviaToken
                    match OperatorInfo.TryCreate(token.PositionedToken) with
                    | ValueSome opInfo when opInfo.CanBePrefix ->
                        let power = byte opInfo.Precedence * 2uy
                        let p = preturn token
                        let op = Prefix(token, p, power, completePrefix)
                        return op
                    | _ ->
                        return! fail (Message "Not a prefix operator")
                }

            member _.RhsParser =
                parser {
                    let! token = nextNonTriviaToken
                    match OperatorInfo.TryCreate(token.PositionedToken) with
                    | ValueSome opInfo ->
                        let power = byte opInfo.Precedence * 2uy
                        let p = preturn token
                        let op =
                            match opInfo.Associativity with
                            | Associativity.Left -> InfixLeft(token, p, power, completeInfix)
                            | Associativity.Right -> InfixRight(token, p, power + 1uy, completeInfix)
                            | Associativity.Non -> InfixNonAssociative(token, p, power, completeInfix)
                        return op
                    | _ ->
                        return! fail (Message "Not an infix operator")
                }

    let p = RefParser<_, _, _, _, _>()

    let pConst = Constant.parse |>> Expr.Const
    let pIdent = nextNonTriviaTokenSatisfiesL (fun synTok -> synTok.Token = Token.Identifier) "Expected identifier" |>> Expr.Ident

    let pLetValue =
        parser {
            let! letTok = pLet
            let! valueDefn = ValueDefn.parse
            let! inTok = pIn
            let! expr = p.Parser
            return Expr.LetValue(letTok, valueDefn, inTok, expr)
        }

    let pParen =
        parser {
            let! l = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpParenLeft) "Expected '('"
            let! e = p.Parser
            let! r = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpParenRight) "Expected ')'"
            return Expr.ParentBlock(l, e, r)
        }

    let atomExpr =
        choiceL [ pConst; pIdent; pLetValue; pParen ] "atom expression"

    let operators = FSharpOperatorParser()
    
    do p.Set(Operator.parser atomExpr operators)

    let parse = p.Parser


[<RequireQualifiedAccess>]
module Reader =
    let ofLexed (lexed: Lexed) =
        let initialState = ParseState.create lexed
        Reader.ofImmutableArray (lexed.Tokens.ToImmutableArray()) initialState
