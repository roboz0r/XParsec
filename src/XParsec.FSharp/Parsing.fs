namespace rec XParsec.FSharp.Parser

open System
open System.Collections.Generic
open System.Collections.Immutable
open XParsec
open XParsec.OperatorParsing
open XParsec.Parsers
open XParsec.FSharp.Lexer

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
        Input: string
        Lexed: Lexed
        // Context: Stack<ParserContext>
        Diagnostics: ImmutableArray<Diagnostic>.Builder
        mutable IndentDirective: Syntax
        mutable MinimumIndentationStack: int64 list
        mutable LastLine: int<line>
        mutable ReprocessOpAfterTypeDeclaration: bool
    }

    member this.MinimumIndentation =
        match this.MinimumIndentationStack with
        | [] -> 0L
        | x :: _ -> x

module ParseState =
    let create (lexed: Lexed) input =
        {
            Input = input
            Lexed = lexed
            // Context = Stack<ParserContext>()
            Diagnostics = ImmutableArray.CreateBuilder()
            IndentDirective = Syntax.Light
            MinimumIndentationStack = []
            LastLine = 0<line>
            ReprocessOpAfterTypeDeclaration = false
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
        if token.InComment then
            true
        else
            match token.TokenWithoutCommentFlags with
            | Token.LineComment
            | Token.Indent
            | Token.Whitespace
            | Token.BlockCommentStart
            | Token.BlockCommentEnd
            | Token.StartFSharpBlockComment
            | Token.EndFSharpBlockComment
            | Token.StartOCamlBlockComment
            | Token.EndOCamlBlockComment
            | Token.Newline -> true
            | Token.Tab -> state.IndentDirective = Syntax.Verbose
            | _ -> false

    let tokenStringIs (s: string) (token: SyntaxToken) (state: ParseState) =
        match token.Index with
        | TokenIndex.Virtual -> false
        | TokenIndex.Regular iT ->
            let t1 = state.Lexed.Tokens[iT + 1<_>]
            let tokenStr = state.Input.[int token.StartIndex .. int (t1.StartIndex - 1L)]
            // printfn "Comparing token string '%s' to '%s'" tokenStr s
            tokenStr = s

    let tokenStringStartsWith (s: string) (token: SyntaxToken) (state: ParseState) =
        match token.Index with
        | TokenIndex.Virtual -> false
        | TokenIndex.Regular iT ->
            let t1 = state.Lexed.Tokens[iT + 1<_>]
            let tokenStr = state.Input.[int token.StartIndex .. int (t1.StartIndex - 1L)]
            tokenStr.StartsWith(s)

    let rec nextNonTriviaToken (reader: Reader<PositionedToken, ParseState, 'a, 'b>) =
        // TODO: We also need to consider handling preprocessor directives here
        match reader.Peek() with
        | ValueNone -> fail EndOfInput reader
        | ValueSome token when isTriviaToken reader.State token ->
            reader.Skip()
            nextNonTriviaToken reader
        | ValueSome token ->
            // printfn "Next non-trivia token: %A at index %A" token.Token reader.Index
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
        | Ok token ->
            if predicate token then
                preturn token reader
            else
                fail (Message msg) reader

    let nextNonTriviaTokenIsL (t: Token) msg =
        nextNonTriviaTokenSatisfiesL (fun synTok -> synTok.Token = t) msg

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
        nextNonTriviaTokenIsL Token.KWMutable "Expected 'mutable' keyword"

    let pWith: Parser<SyntaxToken, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        nextNonTriviaTokenIsL Token.KWWith "Expected 'with' keyword"

    let pThen: Parser<SyntaxToken, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        nextNonTriviaTokenIsL Token.KWThen "Expected 'then' keyword"

    let pElse: Parser<SyntaxToken, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        nextNonTriviaTokenIsL Token.KWElse "Expected 'else' keyword"

    let pDo: Parser<SyntaxToken, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        nextNonTriviaTokenIsL Token.KWDo "Expected 'do' keyword"

    let pAccessModifier: Parser<SyntaxToken, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        nextNonTriviaTokenSatisfiesL isAccessModifier "Expected access modifier (private, public or internal)"

    let pEquals: Parser<SyntaxToken, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        nextNonTriviaTokenIsL Token.OpEquality "Expected '=' symbol"

    let pInVirt: Parser<SyntaxToken, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        nextNonTriviaTokenVirtualIfNot Token.KWIn

    let pLet: Parser<SyntaxToken, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        // TODO: Where does VirtualLet get inserted?
        nextNonTriviaTokenIsL Token.KWLet "Expected 'let' keyword"

    let pFor = nextNonTriviaTokenIsL Token.KWFor "for"

    let pArrowRight = nextNonTriviaTokenIsL Token.OpArrowRight "->"

    let pLetBang = nextNonTriviaTokenIsL Token.KWLetBang "let!"

    let pUseBang = nextNonTriviaTokenIsL Token.KWUseBang "use!"

    let pUse = nextNonTriviaTokenIsL Token.KWUse "use"
    let pDoBang = nextNonTriviaTokenIsL Token.KWDoBang "do!"

    let pYieldBang = nextNonTriviaTokenIsL Token.KWYieldBang "yield!"
    let pReturnBang = nextNonTriviaTokenIsL Token.KWReturnBang "return!"

    let pMatchBang = nextNonTriviaTokenIsL Token.KWMatchBang "match!"

    let pYield = nextNonTriviaTokenIsL Token.KWYield "yield"

    let pReturn = nextNonTriviaTokenIsL Token.KWReturn "return"

    let pMatch = nextNonTriviaTokenIsL Token.KWMatch "match"
    let pIf = nextNonTriviaTokenIsL Token.KWIf "if"
    let pTry = nextNonTriviaTokenIsL Token.KWTry "try"
    let pFinally = nextNonTriviaTokenIsL Token.KWFinally "finally"

    let pWhile = nextNonTriviaTokenIsL Token.KWWhile "while"
    let pIdent = nextNonTriviaTokenIsL Token.Identifier "identifier"

    let pToOrDownTo =
        nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWTo || t.Token = Token.KWDownto) "to/downto"

[<RequireQualifiedAccess>]
module AttributeTarget =
    let private pId s ctor =
        parser {
            let! state = getUserState

            let! t =
                nextNonTriviaTokenSatisfiesL
                    (fun t -> t.Token = Token.Identifier && tokenStringIs s t state)
                    (sprintf "Expected '%s'" s)

            return ctor t
        }

    let private pKw k ctor =
        nextNonTriviaTokenSatisfiesL (fun t -> t.Token = k) (sprintf "Expected '%A'" k)
        |>> ctor

    let parse: Parser<AttributeTarget<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        choiceL
            [
                pId "assembly" AttributeTarget.Assembly
                pKw Token.KWModule AttributeTarget.Module
                pKw Token.KWReturn AttributeTarget.Return
                pId "field" AttributeTarget.Field
                pId "property" AttributeTarget.Property
                pId "param" AttributeTarget.Param
                pKw Token.KWType AttributeTarget.Type
                pId "constructor" AttributeTarget.Constructor
                pId "event" AttributeTarget.Event
            ]
            "AttributeTarget"

[<RequireQualifiedAccess>]
module Attribute =
    let parse: Parser<Attribute<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            // Attempt to parse the target (e.g. "assembly:")
            // We need `attempt` because "assembly" could also be the start of the ObjectConstruction (the Attribute Type name)
            // if the colon is missing.
            let! target =
                opt (
                    parser {
                        let! t = AttributeTarget.parse
                        let! c = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpColon) "Expected ':'"
                        return (t, c)
                    }
                )

            let! construction = ObjectConstruction.parse
            return Attribute.Attribute(target, construction)
        }

[<RequireQualifiedAccess>]
module AttributeSet =
    let parse: Parser<AttributeSet<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! lBracket = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWLAttrBracket) "Expected '[<'"

            // Parse attributes separated by semicolons, capturing the separators
            // We use `many` loop instead of `sepBy` to capture the optional semicolons in the list structure
            let! attributes =
                let pAttributeItem =
                    parser {
                        let! attr = Attribute.parse

                        let! sep =
                            opt (nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpSemicolon) "Expected ';'")

                        return (attr, sep)
                    }
                // Stop when we hit the closing bracket
                many pAttributeItem

            let! rBracket = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWRAttrBracket) "Expected '>]'"

            return AttributeSet.AttributeSet(lBracket, List.ofSeq attributes, rBracket)
        }

[<RequireQualifiedAccess>]
module Attributes =
    let parse: Parser<Attributes<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        many AttributeSet.parse |>> List.ofSeq


[<RequireQualifiedAccess>]
module RangeOpName =
    let parse: Parser<RangeOpName<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        nextNonTriviaTokenSatisfiesL
            (fun t ->
                match t.Token with
                | Token.OpRange
                | Token.OpRangeStep -> true
                | _ -> false
            )
            "Expected '..' or '.. ..'"
        |>> fun t ->
            match t.Token with
            | Token.OpRange -> RangeOpName.DotDot t
            | Token.OpRangeStep -> RangeOpName.DotDotDotDot t
            | _ -> failwith "Unreachable"

[<RequireQualifiedAccess>]
module ActivePatternOpName =
    let parse: Parser<ActivePatternOpName<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        // Recursive helper to parse segments: ident | ...
        let rec parseSegments acc =
            parser {
                let! ident = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.Identifier) "Expected identifier"

                let! bar = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpBar) "Expected '|'"

                // Look ahead to see if we are at the end (RParen) or if there is a wildcard
                match! opt (lookAhead (nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWRParen) "RParen")) with
                | ValueSome _ ->
                    // Found ')', so 'bar' is the rBar
                    return (List.rev (ident :: acc), ValueNone, bar)
                | ValueNone ->
                    // Check for wildcard '_'
                    match!
                        opt (lookAhead (nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.Wildcard) "Wildcard"))
                    with
                    | ValueSome _ ->
                        let! underscore = nextNonTriviaToken // Consume '_'
                        let! finalBar = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpBar) "Expected '|'"
                        return (List.rev (ident :: acc), ValueSome underscore, finalBar)
                    | ValueNone ->
                        // Must be another identifier, loop
                        return! parseSegments (ident :: acc)
            }

        parser {
            // Parses the inside of (| ... |). The surrounding parens are handled by IdentOrOp.ParenOp.
            // Starts with '|'
            let! lBar = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpBar) "Expected '|'"

            let! idents, underscore, rBar = parseSegments []

            return ActivePatternOpName.ActivePatternOp(lBar, idents, underscore, rBar)
        }

[<RequireQualifiedAccess>]
module OpName =
    let private pSymbolicOp =
        nextNonTriviaTokenSatisfiesL
            (fun t -> t.Token.IsOperator || TokenInfo.isOperatorKeyword t.Token)
            "Expected symbolic operator"
        |>> OpName.SymbolicOp

    let parse: Parser<OpName<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        choiceL
            [
                RangeOpName.parse |>> OpName.RangeOp
                // Attempt active pattern first because it starts with '|', which is also a symbolic op
                ActivePatternOpName.parse |>> OpName.ActivePatternOp
                pSymbolicOp
            ]
            "OpName"

[<RequireQualifiedAccess>]
module IdentOrOp =
    let parse: Parser<IdentOrOp<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        choiceL
            [
                // Case 1: Simple Identifier
                nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.Identifier) "Expected Identifier"
                |>> IdentOrOp.Ident

                // Case 2: Star Operator (*)
                // Subcase 2a: Parsed as a single token KWOpDeclareMultiply `(*)`
                (parser {
                    let! token =
                        nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWOpDeclareMultiply) "Expected '(*)'"
                    // Synthesize virtual tokens to match the AST requirement of ( * )
                    let lParen =
                        { token with
                            Index = TokenIndex.Virtual
                            PositionedToken = PositionedToken.Create(Token.KWLParen, token.StartIndex)
                        }

                    let star =
                        { token with
                            Index = TokenIndex.Virtual
                            PositionedToken = PositionedToken.Create(Token.OpMultiply, token.StartIndex + 1L)
                        }

                    let rParen =
                        { token with
                            Index = TokenIndex.Virtual
                            PositionedToken = PositionedToken.Create(Token.KWRParen, token.StartIndex + 2L)
                        }

                    return IdentOrOp.StarOp(lParen, star, rParen)
                })
                // Subcase 2b: Parsed as separate tokens ( * )
                <|> (parser {
                    let! l = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWLParen) "Expected '('"
                    let! s = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpMultiply) "Expected '*'"
                    let! r = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWRParen) "Expected ')'"
                    return IdentOrOp.StarOp(l, s, r)
                })

                // Case 3: Parenthesized Operator (op) or Active Pattern (| ... |)
                <|> (parser {
                    let! l = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWLParen) "Expected '('"
                    let! op = OpName.parse
                    let! r = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWRParen) "Expected ')'"
                    return IdentOrOp.ParenOp(l, op, r)
                })
            ]
            "IdentOrOp"

[<RequireQualifiedAccess>]
module LongIdentOrOp =
    // Could be LongIdent or QualifiedOp
    let rec private parseRest ident acc =
        parser {
            // Look ahead for a dot
            match! opt (lookAhead (nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpDot) "Expected '.'")) with
            | ValueSome dot ->
                // Consume the dot
                let! dotConsumed = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpDot) "Expected '.'"

                // Parse the next IdentOrOp
                let! nextIdentOrOp = IdentOrOp.parse

                match nextIdentOrOp with
                | IdentOrOp.Ident identNext ->
                    // Continue parsing the LongIdent
                    return! parseRest ident (identNext :: acc)
                | _ ->
                    // Found an operator, return QualifiedOp
                    let longIdent = List.rev (ident :: acc)
                    return LongIdentOrOp.QualifiedOp(longIdent, dotConsumed, nextIdentOrOp)
            | ValueNone ->
                // No more dots, return LongIdent
                let longIdent = List.rev (ident :: acc)
                return LongIdentOrOp.LongIdent longIdent
        }

    let parse: Parser<LongIdentOrOp<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! first = IdentOrOp.parse

            match first with
            | IdentOrOp.Ident ident -> return! parseRest ident []
            | _ ->
                // Just an operator
                return LongIdentOrOp.Op first
        }


[<AutoOpen>]
module private TypeHelpers =
    // Forward reference mechanism for recursive Type parsing
    let refType =
        RefParser<Type<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _>()

[<RequireQualifiedAccess>]
module LongIdent =
    // Simple parser for A.B.C
    let parse =
        sepBy1
            (nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.Identifier) "Expected Identifier")
            (nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpDot) "Expected '.'")
        |>> fun struct (xs, dots) -> List.ofSeq xs

[<RequireQualifiedAccess>]
module Typar =
    let pAnon =
        nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.Wildcard) "Expected '_'"
        |>> Typar.Anon


    let pNamed =
        parser {
            let! quote = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWSingleQuote) "Expected quote"
            let! ident = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.Identifier) "Expected identifier"
            return Typar.Named(quote, ident)
        }

    let pStatic =
        parser {
            // Note: In Lexer, ^ might be OpConcatenate, need to check token text or handle appropriately
            // Assuming ^ comes as an operator or specific token.
            // Often ^identifier is lexed as a single token or Op + Ident.
            // Here assuming standard token stream:
            let! state = getUserState
            let! caret = nextNonTriviaTokenSatisfiesL (fun t -> tokenStringIs "^" t state) "Expected '^'"
            let! ident = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.Identifier) "Expected identifier"
            return Typar.Static(caret, ident)
        }

    let parse: Parser<Typar<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        choiceL [ pAnon; pNamed; pStatic ] "Type Parameter"

[<RequireQualifiedAccess>]
module StaticTypars =
    // ^T
    let pSingle =
        parser {
            let! state = getUserState
            let! caret = nextNonTriviaTokenSatisfiesL (fun t -> tokenStringIs "^" t state) "Expected '^'"
            let! ident = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.Identifier) "Expected identifier"
            return StaticTypars.Single(caret, ident)
        }

    // (^T or ^U)
    let pOrList =
        parser {
            let! lParen = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWLParen) "Expected '('"

            let pItem =
                parser {
                    let! tp = Typar.parse
                    let! orTok = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWOr) "Expected 'or'"
                    return (tp, orTok)
                }

            // This is slightly loose (allows trailing 'or'), strict grammar might require sepBy but AST implies tuple structure
            let! items = many1 pItem
            let! rParen = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWRParen) "Expected ')'"
            return StaticTypars.OrList(lParen, List.ofSeq items, rParen)
        }

    let parse = choiceL [ pOrList; pSingle ] "Static Type Parameters"

[<RequireQualifiedAccess>]
module Constraint =

    // MemberSig is complex, using placeholder for now to satisfy type signature
    let private pMemberSig: Parser<MemberSig<SyntaxToken>, _, _, _, _> =
        // Consumes tokens until matching paren? Placeholder implementation.
        // In real impl, this parses property/method signatures.
        nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.Identifier) "MemberSig Placeholder"
        |>> fun _ -> failwith "MemberSig parsing not implemented"

    let private pMemberTrait =
        parser {
            let! staticTypars = StaticTypars.parse
            let! colon = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpColon) "Expected ':'"
            let! lParen = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWLParen) "Expected '('"
            let! sigs = pMemberSig
            let! rParen = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWRParen) "Expected ')'"
            return Constraint.MemberTrait(staticTypars, colon, lParen, sigs, rParen)
        }

    let private pDefaultConstructor (typar: Typar<_>) colon lParen (tokenNew: SyntaxToken) =
        parser {
            let! colonUnit = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpColon) "Expected ':'"
            let! unitTok = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.Unit) "Expected '()'" // Simplified
            let! arrow = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpArrowRight) "Expected '->'"
            let! quoteT = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.Identifier) "Expected 'T" // Simplified
            let! rParen = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWRParen) "Expected ')'"
            // Re-map unitTok to ensure types align if AST expects specific tokens.
            // Note: AST asks for 'colonUnit' then 'arrow', logic adjusted to AST structure:
            return Constraint.DefaultConstructor(typar, colon, lParen, tokenNew, colonUnit, arrow, quoteT, rParen)
        }

    let private pTyparConstraints =
        parser {
            let! typar = Typar.parse

            // Check for :> (Coercion) vs : (Everything else)
            let! coercion = opt (nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpUpcast) ":>") // :>

            match coercion with
            | ValueSome op ->
                let! typ = refType.Parser
                return Constraint.Coercion(typar, op, typ)
            | ValueNone ->
                let! colon = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpColon) "Expected ':'"

                // Branch based on next token
                let! state = getUserState
                let! token = nextNonTriviaToken

                match token.Token with
                | Token.KWNull -> return Constraint.Nullness(typar, colon, token)
                | Token.KWStruct -> return Constraint.Struct(typar, colon, token)
                | Token.KWDelegate ->
                    let! lAngle = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpLessThan) "<"
                    let! t1 = refType.Parser
                    let! comma = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpComma) ","
                    let! t2 = refType.Parser
                    let! rAngle = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpGreaterThan) ">"
                    return Constraint.Delegate(typar, colon, token, lAngle, t1, comma, t2, rAngle)
                | _ when tokenStringIs "equality" token state -> return Constraint.Equality(typar, colon, token)
                | _ when tokenStringIs "comparison" token state -> return Constraint.Comparison(typar, colon, token)
                | _ when tokenStringIs "unmanaged" token state -> return Constraint.Unmanaged(typar, colon, token)
                | _ when tokenStringIs "not" token state ->
                    let! structTok = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWStruct) "struct"
                    return Constraint.ReferenceType(typar, colon, token, structTok)
                | _ when tokenStringIs "enum" token state ->
                    let! lAngle = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpLessThan) "<"
                    let! t = refType.Parser
                    let! rAngle = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpGreaterThan) ">"
                    return Constraint.Enum(typar, colon, token, lAngle, t, rAngle)
                | Token.KWLParen ->
                    // Could be (new : unit -> 'T)
                    let! next = nextNonTriviaToken

                    if next.Token = Token.KWNew then
                        return! pDefaultConstructor typar colon token next
                    else
                        return! fail (Message "Unknown parenthesized constraint")
                | _ -> return! fail (Message "Unknown constraint type")
        }

    let parse = choiceL [ pMemberTrait; pTyparConstraints ] "Constraint"

[<RequireQualifiedAccess>]
module TyparDefns =
    let parse =
        parser {
            let! lAngle = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpLessThan) "<"

            // Parse list of TyparDefn
            let! defns, _ =
                sepBy1
                    (parser {
                        let! attrs = opt Attributes.parse
                        let! tp = Typar.parse
                        return TyparDefn.TyparDefn(attrs, tp)
                    })
                    (nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpComma) ",")

            let! constraints =
                opt (
                    parser {
                        let! whenTok = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWWhen) "when"

                        let! constrs, _ =
                            sepBy1
                                Constraint.parse
                                (nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWAnd) "and")

                        return TyparConstraints.TyparConstraints(whenTok, List.ofSeq constrs)
                    }
                )

            let! rAngle = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpGreaterThan) ">"

            return TyparDefns.TyparDefns(lAngle, List.ofSeq defns, constraints, rAngle)
        }

[<RequireQualifiedAccess>]
module Type =

    let private pTypeArg =
        // Placeholder handling for TypeArg variations
        refType.Parser |>> TypeArg.Type

    let private pAtomicType =
        choiceL
            [
                // (Type)
                parser {
                    let! l = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWLParen) "("
                    let! t = refType.Parser
                    let! r = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWRParen) ")"
                    return Type.ParenType(l, t, r)
                }
                // struct (Type)
                parser {
                    let! s = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWStruct) "struct"
                    let! l = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWLParen) "("

                    let! ts, _ =
                        sepBy refType.Parser (nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpMultiply) "*")

                    let! r = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWRParen) ")"
                    return Type.StructTupleType(s, l, List.ofSeq ts, r)
                }
                // #Type
                parser {
                    let! h = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWHash) "#"
                    let! t = refType.Parser
                    return Type.AnonymousSubtype(h, t)
                }
                // 'a
                Typar.parse |>> Type.VarType
                // LongIdent or LongIdent<Types>
                parser {
                    let! lid = LongIdent.parse
                    // Check for Generic arguments <...>
                    let! genericPart =
                        opt (
                            parser {
                                let! l = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpLessThan) "<"

                                let! args, _ =
                                    sepBy pTypeArg (nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpComma) ",")

                                let! r = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpGreaterThan) ">"
                                return (l, List.ofSeq args, r)
                            }
                        )

                    match genericPart with
                    | ValueSome(l, args, r) -> return Type.GenericType(lid, l, args, r)
                    | ValueNone -> return Type.NamedType(lid)
                }
            ]
            "Atomic Type"

    // Postfix operators: [] (Array), ident (Suffixed e.g. int list)
    // Note: This needs to be left-recursive elimination or chained.
    // Using a simple loop for postfix application.
    let private pPostfixType =
        parser {
            let! atom = pAtomicType

            let rec loop acc =
                choice
                    [
                        // Array: [] or [,]
                        parser {
                            let! l =
                                nextNonTriviaTokenSatisfiesL
                                    (fun t -> t.Token = Token.KWLArrayBracket || t.Token = Token.KWLBracket)
                                    "["
                            // Parse commas for rank
                            let! commas = many (nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpComma) ",")

                            let! r =
                                nextNonTriviaTokenSatisfiesL
                                    (fun t -> t.Token = Token.KWRArrayBracket || t.Token = Token.KWRBracket)
                                    "]"

                            let newAcc = Type.ArrayType(acc, l, List.ofSeq commas, r)
                            return! loop newAcc
                        }
                        // Suffix: int list
                        // We only consume an identifier here if it's NOT a keyword,
                        // and not start of a new construct.
                        (parser {
                            // Lookahead or logic to ensure this is a type suffix and not next token
                            let! lid = LongIdent.parse
                            // If we see <, it's a GenericType which is an atom, not a suffix to an existing type in this specific position
                            // F# parsing `int list` -> Suffixed(int, list).
                            // `int list list` -> Suffixed(Suffixed(int, list), list)
                            let newAcc = Type.SuffixedType(acc, lid)
                            return! loop newAcc
                        })
                        // Done
                        preturn acc
                    ]

            return! loop atom
        }

    // Tuple: T * T * T
    let private pTupleType =
        parser {
            let! first = pPostfixType

            let! rest =
                many (
                    parser {
                        let! _ = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpMultiply) "*"
                        return! pPostfixType
                    }
                )

            let restList = List.ofSeq rest

            if restList.IsEmpty then
                return first
            else
                return Type.TupleType(first :: restList)
        }

    // Function: T -> T -> T (Right Associative)
    // Recursive implementation
    let private pFunctionType =
        let rec pFunc () =
            parser {
                let! lhs = pTupleType
                let! arrow = opt (nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpArrowRight) "->")

                match arrow with
                | ValueSome arr ->
                    let! rhs = pFunc ()
                    return Type.FunctionType(lhs, arr, rhs)
                | ValueNone -> return lhs
            }

        pFunc ()

    // Entry point for simple types
    let parse = pFunctionType

    // Initialize the recursive ref parser
    do refType.Set parse

module ElifBranch =
    let parse: Parser<ElifBranch<_>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! elifTok = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWElif) "Expected 'elif'"
            let! condition = Expr.parse
            let! thenTok = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWThen) "Expected 'then'"
            let! expr = Expr.parse
            return ElifBranch.ElifBranch(elifTok, condition, thenTok, expr)
        }

module ElseBranch =
    let parse: Parser<ElseBranch<_>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! elseTok = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWElse) "Expected 'else'"
            let! expr = Expr.parse
            return ElseBranch.ElseBranch(elseTok, expr)
        }


module ReturnType =
    let parse: Parser<ReturnType<_>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! colon = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpColon) "Expected ':'"
            let! typ = Type.parse
            return ReturnType.ReturnType(colon, typ)
        }

module FunctionDefn =
    let parse: Parser<FunctionDefn<_>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! inlineTok = opt (nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWInline) "Expected 'inline'")
            let! access = opt pAccessModifier
            let! identOrOp = IdentOrOp.parse
            let! typarDefns = opt TyparDefns.parse

            // Parse one or more argument patterns
            let! argumentPats = many1 (parser { return! Pat.parse })

            let! returnType = opt ReturnType.parse
            let! equals = pEquals
            let! expr = Expr.parse

            return
                FunctionDefn.FunctionDefn(
                    inlineTok,
                    access,
                    identOrOp,
                    typarDefns,
                    List.ofSeq argumentPats,
                    returnType,
                    equals,
                    expr
                )
        }

module ValueDefn =
    let pMutable: Parser<SyntaxToken, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWMutable) "Expected 'mutable'"

    let parse =
        parser {
            let! mut = opt pMutable
            let! access = opt pAccessModifier
            let! pat = Pat.parse
            let! typarDefns = opt TyparDefns.parse
            let! returnType = opt ReturnType.parse
            let! equals = pEquals
            let! expr = Expr.parse
            return ValueDefn.ValueDefn(mut, access, pat, typarDefns, returnType, equals, expr)
        }

module FunctionOrValueDefn =
    let parse: Parser<FunctionOrValueDefn<_>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        choiceL
            [
                FunctionDefn.parse |>> FunctionOrValueDefn.Function
                ValueDefn.parse |>> FunctionOrValueDefn.Value
            ]
            "FunctionOrValueDefn"

[<AutoOpen>]
module private MemberHelpers =
    // Forward reference for MemberDefn to avoid circular dependency issues
    // and provide a stub for ObjectMembers
    let refMemberDefn =
        RefParser<MemberDefn<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _>()

[<RequireQualifiedAccess>]
module FieldInitializer =
    let parse: Parser<FieldInitializer<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! id = LongIdent.parse
            let! equals = pEquals // Defined in previous keywords module
            let! expr = Expr.parse
            return FieldInitializer.FieldInitializer(id, equals, expr)
        }

[<RequireQualifiedAccess>]
module ObjectConstruction =
    let parse: Parser<ObjectConstruction<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! typ = Type.parse

            // Heuristic: If followed by '(', it's likely a constructor call (ObjectConstruction).
            // If not, it might be an Interface construction (inherit IMyInterface).
            // F# optional () logic is subtle, but for AST parsing:

            // Check for explicit unit '()' or paren '(' start
            let! argExpr =
                opt (
                    // Logic to detect if the next token starts an expression argument
                    // Usually looks for '(' or 'unit'
                    parser {
                        let! token = lookAhead nextNonTriviaToken

                        if token.Token = Token.KWLParen || token.Token = Token.Unit then
                            return! Expr.parse
                        else
                            return! fail (Message "No constructor arguments")
                    }
                )

            match argExpr with
            | ValueSome expr -> return ObjectConstruction.ObjectConstruction(typ, expr)
            | ValueNone -> return ObjectConstruction.InterfaceConstruction(typ)
        }

[<RequireQualifiedAccess>]
module BaseCall =
    let parse: Parser<BaseCall<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            // Note: This parser expects 'inherit' to be handled by the caller,
            // as the BaseCall AST type does not contain the 'inherit' token.

            let! construction = ObjectConstruction.parse

            let! asPart =
                opt (
                    parser {
                        let! asTok = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWAs) "Expected 'as'"

                        let! ident =
                            nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.Identifier) "Expected identifier"

                        return (asTok, ident)
                    }
                )

            match asPart with
            | ValueSome(asTok, ident) -> return BaseCall.NamedBaseCall(construction, asTok, ident)
            | ValueNone -> return BaseCall.AnonBaseCall(construction)
        }

[<RequireQualifiedAccess>]
module MemberDefn =
    // STUB: Real MemberDefn parsing is very complex (properties, methods, let bindings, etc.)
    // This simple implementation allows ObjectMembers to compile.
    let parse: Parser<MemberDefn<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! token = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWMember) "Expected 'member'"
            // In a real implementation, this would parse the full member body
            return failwith "MemberDefn parsing not implemented"
        }

    do refMemberDefn.Set parse

[<RequireQualifiedAccess>]
module ObjectMembers =
    let parse: Parser<ObjectMembers<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! withTok = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWWith) "Expected 'with'"

            // Parse list of members until 'end'
            // We use `many` combined with a check for the `end` token to terminate
            let! members, endTok =
                manyTill
                    refMemberDefn.Parser
                    (nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWEnd) "Expected 'end'")

            return ObjectMembers.ObjectMembers(withTok, List.ofSeq members, endTok)
        }

[<RequireQualifiedAccess>]
module InterfaceImpl =
    let parse: Parser<InterfaceImpl<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! interfaceTok =
                nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWInterface) "Expected 'interface'"

            let! typ = Type.parse
            let! members = opt ObjectMembers.parse

            return InterfaceImpl.InterfaceImpl(interfaceTok, typ, members)
        }


[<RequireQualifiedAccess>]
module SliceRange =

    let private pAll =
        nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpMultiply) "Expected '*'"
        |>> SliceRange.All

    let private pTo =
        parser {
            let! dd = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpRange) "Expected '..'"
            let! e = Expr.parse
            return SliceRange.To(dd, e)
        }

    let private pFromOrSingleOrFromTo =
        parser {
            let! start = Expr.parse

            // Check for '..'
            let! dd = opt (nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpRange) "..")

            match dd with
            | ValueNone -> return SliceRange.Single(start)
            | ValueSome dotdot ->
                // We have 'start ..', check if there is an end expression.
                // In slice syntax like a[1..], the end is optional.
                // We try to parse an expression. If it fails (e.g. hits ']'), it's a From.
                // Note: We use 'opt Expr.parse'. This relies on Expr.parse failing gracefully
                // if it encounters a closing delimiter immediately.
                let! endExpr = opt Expr.parse

                match endExpr with
                | ValueSome finish -> return SliceRange.FromTo(start, dotdot, finish)
                | ValueNone -> return SliceRange.From(start, dotdot)
        }

    let parse: Parser<SliceRange<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        choiceL [ pAll; pTo; pFromOrSingleOrFromTo ] "SliceRange"


[<RequireQualifiedAccess>]
module RangeExpr =
    // Parses a full range expression: start .. end OR start .. step .. end
    // Assumes the caller has NOT parsed the start expression yet.
    let parse: Parser<RangeExpr<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! start = Expr.parse
            let! dd1 = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpRange) "Expected '..'"
            let! middle = Expr.parse

            // Check for second '..'
            let! dd2 = opt (nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpRange) "..")

            match dd2 with
            | ValueSome dotdot2 ->
                // start .. step .. end
                let step = middle
                let! finish = Expr.parse
                return RangeExpr.SteppedRange(start, dd1, step, dotdot2, finish)
            | ValueNone ->
                // start .. end
                let finish = middle
                return RangeExpr.SimpleRange(start, dd1, finish)
        }

[<RequireQualifiedAccess>]
module ExprOrRange =
    // Parses either a RangeExpr or a standard Expr.
    // This is commonly used in list/array constructors e.g. [ 1..10 ] or [ 1; 2; 3 ]
    let parse: Parser<ExprOrRange<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! start = Expr.parse

            // Check for '..' indicating the start of a range
            let! dd1 = opt (nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpRange) "..")

            match dd1 with
            | ValueNone -> return ExprOrRange.Expr(start)
            | ValueSome dotdot1 ->
                // It is a range
                let! middle = Expr.parse

                let! dd2 = opt (nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpRange) "..")

                match dd2 with
                | ValueSome dotdot2 ->
                    let step = middle
                    let! finish = Expr.parse
                    return ExprOrRange.Range(RangeExpr.SteppedRange(start, dotdot1, step, dotdot2, finish))
                | ValueNone ->
                    let finish = middle
                    return ExprOrRange.Range(RangeExpr.SimpleRange(start, dotdot1, finish))
        }

[<AutoOpen>]
module private CompHelpers =
    // Helper to handle virtual tokens for CE
    let pDone = nextNonTriviaTokenVirtualIfNot Token.KWDone

    // Recursive reference for CompExpr
    let refCompExpr =
        RefParser<CompExpr<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _>()

[<RequireQualifiedAccess>]
module CompExpr =

    // --- Specific Clause Parsers ---


    let private pLetBangCE =
        parser {
            let! letBang = pLetBang
            let! pat = Pat.parse
            let! eq = pEquals
            let! expr = Expr.parse
            let! inTok = pInVirt
            let! comp = refCompExpr.Parser
            return CompExpr.LetBang(letBang, pat, eq, expr, inTok, comp)
        }

    let private pLetCE =
        parser {
            let! letTok = pLet // defined in global keywords
            let! pat = Pat.parse
            let! eq = pEquals
            let! expr = Expr.parse
            let! inTok = pInVirt
            let! comp = refCompExpr.Parser
            return CompExpr.Let(letTok, pat, eq, expr, inTok, comp)
        }

    let private pUseBangCE =
        parser {
            let! useBang = pUseBang
            let! pat = Pat.parse
            let! eq = pEquals
            let! expr = Expr.parse
            let! inTok = pInVirt
            let! comp = refCompExpr.Parser
            return CompExpr.UseBang(useBang, pat, eq, expr, inTok, comp)
        }

    let private pUseCE =
        parser {
            let! useTok = pUse
            let! pat = Pat.parse
            let! eq = pEquals
            let! expr = Expr.parse
            let! inTok = pInVirt
            let! comp = refCompExpr.Parser
            return CompExpr.Use(useTok, pat, eq, expr, inTok, comp)
        }

    let private pDoBangCE =
        parser {
            let! doBang = pDoBang
            let! expr = Expr.parse
            let! inTok = pInVirt
            let! comp = refCompExpr.Parser
            return CompExpr.DoBang(doBang, expr, inTok, comp)
        }

    let private pDoCE =
        parser {
            let! doTok = pDo
            let! expr = Expr.parse
            let! inTok = pInVirt
            let! comp = refCompExpr.Parser
            return CompExpr.Do(doTok, expr, inTok, comp)
        }

    let private pYieldBangCE =
        parser {
            let! t = pYieldBang
            let! e = Expr.parse
            return CompExpr.YieldBang(t, e)
        }

    let private pYieldCE =
        parser {
            let! t = pYield
            let! e = Expr.parse
            return CompExpr.Yield(t, e)
        }

    let private pReturnBangCE =
        parser {
            let! t = pReturnBang
            let! e = Expr.parse
            return CompExpr.ReturnBang(t, e)
        }

    let private pReturnCE =
        parser {
            let! t = pReturn
            let! e = Expr.parse
            return CompExpr.Return(t, e)
        }

    let private pMatchBangCE =
        parser {
            let! m = pMatchBang
            let! e = Expr.parse
            let! w = pWith
            let! r = Rules.parse
            return CompExpr.MatchBang(m, e, w, r)
        }

    let private pMatchCE =
        parser {
            let! m = pMatch
            let! e = Expr.parse
            let! w = pWith
            let! r = Rules.parse
            return CompExpr.Match(m, e, w, r)
        }

    let private pIfCE =
        parser {
            let! ifTok = pIf
            let! cond = Expr.parse
            let! thenTok = pThen
            // Check for else
            // Per AST: IfThenElse takes Expr for 'then' branch, CompExpr for 'else' branch
            // IfThen takes CompExpr for 'then' branch

            // We attempt to parse an else block to decide
            // This logic assumes we can look ahead or backtrack

            return!
                choice
                    [
                        (parser {
                            let! thenExpr = Expr.parse
                            let! elseTok = pElse
                            let! elseComp = refCompExpr.Parser
                            return CompExpr.IfThenElse(ifTok, cond, thenTok, thenExpr, elseTok, elseComp)
                        })
                        parser {
                            let! thenComp = refCompExpr.Parser
                            return CompExpr.IfThen(ifTok, cond, thenTok, thenComp)
                        }
                    ]
        }

    let private pTryCE =
        parser {
            let! tryTok = pTry
            let! comp = refCompExpr.Parser

            return!
                choice
                    [
                        parser {
                            let! withTok = pWith
                            let! rules = Rules.parse
                            return CompExpr.TryWith(tryTok, comp, withTok, rules)
                        }
                        parser {
                            let! finTok = pFinally
                            let! finExpr = Expr.parse
                            return CompExpr.TryFinally(tryTok, comp, finTok, finExpr)
                        }
                    ]
        }

    let private pWhileCE =
        parser {
            let! w = pWhile
            let! cond = Expr.parse
            let! d = pDo
            let! body = refCompExpr.Parser
            let! doneTok = pDone
            return CompExpr.While(w, cond, d, body, doneTok)
        }

    // Distinguish ForTo vs ForIn
    // for ident = ... to ...
    // for pat in ...
    let private pForCE =
        parser {
            let! forTok = pFor

            return!
                choice
                    [
                        // ForTo: for i = 1 to 10 do
                        (parser {
                            let! ident = pIdent
                            let! eq = pEquals
                            let! start = Expr.parse
                            let! toTok = pToOrDownTo
                            let! endExpr = Expr.parse
                            let! doTok = Keywords.pDo
                            let! comp = refCompExpr.Parser
                            let! doneTok = pDone
                            return CompExpr.ForTo(forTok, ident, eq, start, toTok, endExpr, doTok, comp, doneTok)
                        })
                        // ForIn: for x in xs do
                        parser {
                            let! pat = Pat.parse
                            let! inTok = pInVirt
                            let! exprOrRange = ExprOrRange.parse
                            let! doTok = Keywords.pDo
                            let! comp = refCompExpr.Parser
                            let! doneTok = pDone
                            return CompExpr.ForIn(forTok, pat, inTok, exprOrRange, doTok, comp, doneTok)
                        }
                    ]
        }

    let private pBaseExpr = Expr.parse |>> CompExpr.BaseExpr

    let private pAtomComp =
        choiceL
            [
                pLetBangCE
                pUseBangCE
                pDoBangCE
                pYieldBangCE
                pReturnBangCE
                pMatchBangCE
                // Check specific keywords before checking BaseExpr
                // Note: 'let', 'use', 'do' are in the keyword list but parsed specifically here for CE structure
                pLetCE
                pUseCE
                pDoCE
                pYieldCE
                pReturnCE
                pIfCE
                pMatchCE
                pTryCE
                pWhileCE
                pForCE
                // Fallback
                pBaseExpr
            ]
            "Computation Expression Atom"

    // Top Level: Handles Sequential composition
    // comp := atom ; comp | atom
    let parse =
        parser {
            let! head = pAtomComp

            // Check for semicolon
            let! semi = opt (nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpSemicolon) ";")

            match semi with
            | ValueSome s ->
                let! tail = refCompExpr.Parser // Recursive
                return CompExpr.Sequential(head, s, tail)
            | ValueNone -> return head
        }

    do refCompExpr.Set parse

[<RequireQualifiedAccess>]
module ShortCompExpr =
    // for pat in expr -> expr
    let parse =
        parser {
            let! forTok = pFor
            let! pat = Pat.parse
            let! inTok = pInVirt
            let! range = ExprOrRange.parse
            let! arrow = pArrowRight
            let! expr = Expr.parse
            return ShortCompExpr.ShortCompExpr(forTok, pat, inTok, range, arrow, expr)
        }

[<RequireQualifiedAccess>]
module CompOrRangeExpr =
    let parse: Parser<CompOrRangeExpr<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        choiceL
            [
                // ShortComp starts with 'for' ... '->'
                // Standard Comp 'ForIn' starts with 'for' ... 'do'
                // We use attempt on ShortComp because they share 'for pat in expr' prefix
                (ShortCompExpr.parse |>> CompOrRangeExpr.ShortComp)

                // Range: expr .. expr
                // CompExpr: includes BaseExpr(expr)
                // Ambiguity: [ 1 .. 10 ] (Range) vs [ 1 ] (Comp->BaseExpr)
                // RangeExpr parser expects 'start ..', it doesn't parse just 'start'.
                // However, CompExpr includes BaseExpr which parses 'start'.
                // If we use RangeExpr.parse, it internally calls Expr.parse then looks for '..'.
                // If '..' is not found, RangeExpr.parse fails (based on previous implementation).
                // So we can try RangeExpr first.
                (RangeExpr.parse |>> CompOrRangeExpr.Range)

                // Fallback to full Computation Expression
                (CompExpr.parse |>> CompOrRangeExpr.Comp)
            ]
            "CompOrRangeExpr"

[<RequireQualifiedAccess>]
module Constant =
    let isLiteralToken (synTok: SyntaxToken) =
        let t = synTok.Token

        t.IsNumeric
        || t = Token.KWTrue
        || t = Token.KWFalse
        || t = Token.Unit
        || t = Token.KWNull
        || t = Token.StringLiteral
        || t = Token.String3Literal
        || t = Token.VerbatimStringLiteral
        || t = Token.ByteArrayLiteral
        || t = Token.VerbatimByteArrayLiteral

    let pLiteral: Parser<Constant<_>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        nextNonTriviaTokenSatisfiesL isLiteralToken "Expected constant literal"
        |>> Constant.Literal

    let parse: Parser<Constant<_>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        choiceL [ pLiteral ] "Constant"

[<RequireQualifiedAccess>]
type ExprAux =
    | Ident of SyntaxToken
    | TypeApp of Type<SyntaxToken> list * SyntaxToken
    | DotIndex of SyntaxToken * Expr<SyntaxToken> * SyntaxToken // .[ expr ]
    | DotSlice of SyntaxToken * SliceRange<SyntaxToken> list * SyntaxToken // .[ 1.. ]
    | ComputationBlock of SyntaxToken * CompOrRangeExpr<SyntaxToken> * SyntaxToken // { ... }
    | PostfixDynamic of SyntaxToken * Type<SyntaxToken> // :? Type (DynamicTypeTest)

[<RequireQualifiedAccess>]
module Expr =
    let bp x =
        LanguagePrimitives.ByteWithMeasure<bp> x

    let pl x : PrecedenceLevel = LanguagePrimitives.EnumOfValue x

    type ExprOperatorParser() =
        let completeInfix (l: Expr<_>) (op: SyntaxToken) (r: Expr<_>) = Expr.InfixApp(l, op, r)
        let completePrefix (op: SyntaxToken) (e: Expr<_>) = Expr.PrefixApp(op, e)
        let completeTuple (elements: ResizeArray<Expr<_>>) ops = Expr.Tuple(List.ofSeq elements)

        let printOpInfo (op: OperatorInfo) =
            printfn
                $"Operator: {op.PositionedToken}({op.StartIndex}), Precedence: {op.Precedence}, Associativity: %A{op.Associativity}"

        let opComparer =
            // For parsing, we only care about the operator token itself for equality
            { new IEqualityComparer<SyntaxToken> with
                member _.Equals(x, y) = x.Token = y.Token
                member _.GetHashCode(obj) = hash obj.Token
            }

        let parseDotRhs: Parser<ExprAux, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
            parser {
                // For now we just parse an identifier after the dot
                let! ident =
                    nextNonTriviaTokenSatisfiesL
                        (fun synTok -> synTok.Token = Token.Identifier)
                        "Expected identifier after '.'"

                return ExprAux.Ident ident
            }

        let completeDot (expr: Expr<_>) (op: SyntaxToken) (aux: ExprAux) =
            match aux with
            | ExprAux.Ident ident ->
                match expr with
                | Expr.Ident firstIdent -> Expr.LongIdentOrOp(LongIdentOrOp.LongIdent [ firstIdent; ident ])
                | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent longIdentOrOp) ->
                    let newLongIdent =
                        match longIdentOrOp with
                        | [] -> [ ident ]
                        | _ -> longIdentOrOp @ [ ident ]

                    Expr.LongIdentOrOp(LongIdentOrOp.LongIdent newLongIdent)
                | _ -> Expr.DotLookup(expr, op, LongIdentOrOp.LongIdent [ ident ])
            | _ -> failwith "Unexpected Aux type for dot completion"

        let pTypeAppRhs =
            parser {
                let! types, _ =
                    sepBy
                        Type.parse
                        (nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpComma) "Expected ',' between types")

                let! state = getUserState

                let! rAngle =
                    nextNonTriviaTokenSatisfiesL
                        (fun t ->
                            match t.Token with
                            | Token.OpGreaterThan -> tokenStringIs ">" t state
                            | _ -> false
                        )
                        "Expected '>' for type application"
                    <|> parser {
                        let! pos = getPosition
                        let! token = nextNonTriviaToken
                        let! state = getUserState
                        // printfn "Handling special case for type application '>' token: %A" token
                        if token.Token = Token.OpGreaterThan then
                            if tokenStringStartsWith ">" token state then
                                // We have an operator like '>>' or '>>=' that needs to be reprocessed
                                // after the type application that takes the first '>' as its left operand
                                state.ReprocessOpAfterTypeDeclaration <- true
                                do! setPosition pos
                                return token
                            else
                                return! fail (Message "Expected '>' for type application") // Could be a different operator
                        else
                            return! fail (Message "Expected '>' for type application") // Could be a different operator
                    }

                return ExprAux.TypeApp(List.ofSeq types, rAngle)
            }

        let completeTypeApp (expr: Expr<_>) (op: SyntaxToken) (aux: ExprAux) =
            match aux with
            | ExprAux.TypeApp(types, rAngle) -> Expr.TypeApp(expr, op, types, rAngle)
            | _ -> failwith "Unexpected Aux type for type application completion"

        let rhsOperators
            : (SyntaxToken
                  -> RHSOperator<
                      SyntaxToken,
                      ExprAux,
                      Expr<SyntaxToken>,
                      PositionedToken,
                      ParseState,
                      ReadableImmutableArray<PositionedToken>,
                      ReadableImmutableArraySlice<PositionedToken>
                   >) array =
            Array.init
                29
                (fun i ->
                    let pl = pl i
                    let power = BindingPower.fromLevel i

                    match pl with
                    // Pattern only keywords
                    // | PrecedenceLevel.As -> (fun op -> InfixRight(op, preturn op, BindingPower.rightAssocLhs power, completeInfix))
                    // | PrecedenceLevel.When -> (fun op -> InfixRight(op, preturn op, BindingPower.rightAssocLhs power, completeInfix))
                    | PrecedenceLevel.Pipe -> (fun op -> InfixLeft(op, preturn op, power, completeInfix))
                    | PrecedenceLevel.Semicolon ->
                        (fun op -> InfixRight(op, preturn op, BindingPower.rightAssocLhs power, completeInfix))
                    // Atom keywords
                    // | PrecedenceLevel.Let -> (fun op -> InfixNonAssociative(op, preturn op, power, completeInfix))
                    // | PrecedenceLevel.Function -> (fun op -> InfixNonAssociative(op, preturn op, power, completeInfix))
                    // | PrecedenceLevel.If -> (fun op -> InfixNonAssociative(op, preturn op, power, completeInfix))
                    | PrecedenceLevel.Arrow ->
                        (fun op -> InfixRight(op, preturn op, BindingPower.rightAssocLhs power, completeInfix))
                    | PrecedenceLevel.Assignment ->
                        (fun op -> InfixRight(op, preturn op, BindingPower.rightAssocLhs power, completeInfix))
                    | PrecedenceLevel.Comma -> (fun op -> InfixNary(op, preturn op, power, completeTuple))
                    | PrecedenceLevel.LogicalOr -> (fun op -> InfixLeft(op, preturn op, power, completeInfix))
                    | PrecedenceLevel.LogicalAnd -> (fun op -> InfixLeft(op, preturn op, power, completeInfix))
                    // TODO: Implement mapped infix operator and Aux type
                    // | PrecedenceLevel.Cast -> (fun op -> InfixMapped(op, preturn op, power, completeInfix))
                    | PrecedenceLevel.LogicalAndBitwise -> (fun op -> InfixLeft(op, preturn op, power, completeInfix))
                    | PrecedenceLevel.Caret ->
                        (fun op -> InfixRight(op, preturn op, BindingPower.rightAssocLhs power, completeInfix))
                    | PrecedenceLevel.Cons ->
                        (fun op -> InfixRight(op, preturn op, BindingPower.rightAssocLhs power, completeInfix))
                    // | PrecedenceLevel.TypeTest -> Pattern only operator :?
                    | PrecedenceLevel.InfixAdd -> (fun op -> InfixLeft(op, preturn op, power, completeInfix))
                    | PrecedenceLevel.InfixMultiply -> (fun op -> InfixLeft(op, preturn op, power, completeInfix))
                    | PrecedenceLevel.Power ->
                        (fun op -> InfixRight(op, preturn op, BindingPower.rightAssocLhs power, completeInfix))
                    | PrecedenceLevel.Application -> (fun op -> InfixLeft(op, preturn op, power, completeInfix))
                    // | PrecedenceLevel.PatternMatchBar -> Pattern only operator
                    // | PrecedenceLevel.Prefix -> LHS only
                    | PrecedenceLevel.Dot -> (fun op -> InfixMapped(op, preturn op, power, parseDotRhs, completeDot))
                    | PrecedenceLevel.HighApplication -> (fun op -> InfixLeft(op, preturn op, power, completeInfix))
                    | PrecedenceLevel.HighTypeApplication ->
                        (fun op -> InfixMapped(op, preturn op, power, pTypeAppRhs, completeTypeApp))
                    // | PrecedenceLevel.Parens -> LHS only
                    | _ -> Unchecked.defaultof<_>
                )

        let pApplication =
            // Treat whitespace followed by an atom as application
            // Technically also need to handle line breaks with proper indentation here and interstitial comments
            // This parser also need to handle high-precedence application f(x) and f<x>
            parser {
                let! i = getPosition

                let! token =
                    satisfyL (fun (t: PositionedToken) -> t.Token = Token.Whitespace) "Whitespace for application"

                do!
                    followedBy (fun reader ->
                        match reader.Peek() with
                        | ValueNone -> fail EndOfInput reader
                        | ValueSome t ->
                            if t.Token.IsIdentifier then
                                preturn () reader
                            elif t.Token.IsLiteral then
                                preturn () reader
                            elif t.Token = Token.KWLParen then
                                preturn () reader
                            elif t.Token = Token.KWLBracket then
                                preturn () reader
                            else
                                fail (Message "Expected expression after application whitespace") reader
                    )

                let t = syntaxToken token i.Index
                return t
            }

        let pTypeApplication =
            // Treat '<' followed by a type and '>' as type application
            parser {
                let! state = getUserState

                let! lAngle =
                    nextNonTriviaTokenSatisfiesL
                        (fun synTok ->
                            match synTok.Token with
                            | Token.OpLessThan -> tokenStringIs "<" synTok state
                            | _ -> false
                        )
                        "Expected '<' for type application"

                do!
                    followedBy (fun reader ->
                        match reader.Peek() with
                        | ValueNone -> fail EndOfInput reader
                        | ValueSome t ->
                            if t.Token.IsIdentifier then
                                preturn () reader
                            elif t.Token.IsLiteral then
                                preturn () reader
                            else
                                fail (Message "Expected type after '<' for type application") reader
                    )

                let typeAngle =
                    { lAngle with
                        PositionedToken =
                            PositionedToken.Create(
                                Token.ofUInt16 (
                                    TokenRepresentation.KindOperator
                                    ||| TokenRepresentation.Precedence.HighTypeApplication
                                ),
                                lAngle.PositionedToken.StartIndex
                            )
                    }

                return typeAngle
            }

        let rhsParser =
            // First try to parse application operator (whitespace) or high-precedence application
            // then try to parse explicit operator
            (pTypeApplication <|> pApplication <|> nextNonTriviaToken)
            >>= fun token ->
                match OperatorInfo.TryCreate token.PositionedToken with
                | ValueNone -> fail (Message "Expected RHS operator")
                | ValueSome opInfo ->
                    parser {
                        let! state = getUserState

                        if state.ReprocessOpAfterTypeDeclaration then
                            // We have an operator like '>>' or '>>=' that needs to be reprocessed
                            // after the type application that takes the first '>' to close the type application
                            state.ReprocessOpAfterTypeDeclaration <- false

                            let reprocessedToken =
                                let newStart = token.StartIndex + 1L // Adjust start index to account for consumed '>'

                                let tokenString =
                                    match token.Index with
                                    | TokenIndex.Virtual -> failwith "Cannot re-lex virtual token"
                                    | TokenIndex.Regular iT ->
                                        let t1 = state.Lexed.Tokens[iT + 1<token>] // Next token after operator always exists as EOF is present
                                        state.Input.[int newStart .. int (t1.StartIndex - 1L)]
                                // printfn "Re-lexing operator after type declaration: '%s'" tokenString
                                let t =
                                    match tokenString with
                                    | "" ->
                                        failwith "Unexpected empty operator string in ReprocessOpAfterTypeDeclaration"
                                    | "." ->
                                        // Special case for dot operator as it is common after type declarations
                                        Token.OpDot
                                    | _ ->
                                        // Need to re-lex to discover the correct operator token (mostly for precedence)
                                        match Lexing.lexString tokenString with
                                        | Error e -> failwithf "Failed to re-lex operator after type declaration %A" e
                                        | Ok lexed ->
                                            if lexed.Tokens.Length <> 2 then
                                                // Expect exactly two tokens: the operator and EOF
                                                failwithf
                                                    "Re-lexed operator did not produce exactly one token: %A"
                                                    lexed.Tokens
                                            else
                                                let relexedToken = lexed.Tokens[0<token>]
                                                relexedToken.Token

                                { token with
                                    PositionedToken = PositionedToken.Create(t, newStart)
                                }

                            match OperatorInfo.TryCreate reprocessedToken.PositionedToken with
                            | ValueNone -> return! fail (Message "Not a valid RHS operator after type declaration")
                            | ValueSome opInfo ->
                                // printOpInfo opInfo
                                let pl = opInfo.Precedence
                                let x = rhsOperators[LanguagePrimitives.EnumToValue pl]reprocessedToken

                                if obj.ReferenceEquals(x, null) then
                                    return! fail (Message "Not a valid RHS operator after type declaration")
                                else
                                    return x
                        else
                            // printOpInfo opInfo
                            let pl = opInfo.Precedence
                            let x = rhsOperators[LanguagePrimitives.EnumToValue pl]token

                            if obj.ReferenceEquals(x, null) then
                                return! fail (Message "Not a valid RHS operator")
                            else
                                return x
                    }

        let lhsParser =
            nextNonTriviaToken
            >>= fun token ->
                match OperatorInfo.TryCreate(token.PositionedToken) with
                | ValueSome opInfo when opInfo.CanBePrefix ->
                    // printOpInfo opInfo
                    let power = BindingPower.fromLevel (int opInfo.Precedence)
                    let p = preturn token
                    let op = Prefix(token, p, power, completePrefix)
                    preturn op
                | _ -> fail (Message "Not a prefix operator")

        interface Operators<
            SyntaxToken,
            ExprAux,
            Expr<SyntaxToken>,
            PositionedToken,
            ParseState,
            ReadableImmutableArray<PositionedToken>,
            ReadableImmutableArraySlice<PositionedToken>
         > with
            member _.LhsParser = lhsParser
            member _.RhsParser = rhsParser
            member _.OpComparer = opComparer

    let private refExpr = RefParser<_, _, _, _, _>()
    let private refExprInCollection = RefParser<_, _, _, _, _>()

    let pConst = Constant.parse |>> Expr.Const

    let pIdent =
        nextNonTriviaTokenSatisfiesL (fun synTok -> synTok.Token = Token.Identifier) "Expected identifier"
        |>> Expr.Ident

    let pLetValue =
        parser {
            let! letTok = pLet
            let! valueDefn = ValueDefn.parse
            let! inTok = pInVirt
            let! expr = refExpr.Parser
            return Expr.LetValue(letTok, valueDefn, inTok, expr)
        }

    let pParen =
        parser {
            let! l = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWLParen) "Expected '('"
            let! e = refExpr.Parser
            let! r = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWRParen) "Expected ')'"
            return Expr.ParenBlock(l, e, r)
        }

    let pBeginEnd =
        parser {
            let! l = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWBegin) "Expected 'begin'"
            let! e = refExpr.Parser
            let! r = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWEnd) "Expected 'end'"
            return Expr.BeginEndBlock(l, e, r)
        }

    let pCollection openTok closeTok complete =
        parser {
            let! l = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = openTok) $"Expected '{openTok}'"

            let! elems, seps =
                sepEndBy
                    refExprInCollection.Parser
                    (nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpSemicolon) "Expected ';'")

            let! r = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = closeTok) $"Expected '{closeTok}'"
            return complete l elems r
        }

    let pList =
        pCollection Token.KWLBracket Token.KWRBracket (fun l elems r -> Expr.List(l, List.ofSeq elems, r))

    let pArray =
        pCollection Token.KWLArrayBracket Token.KWRArrayBracket (fun l elems r -> Expr.Array(l, List.ofSeq elems, r))

    let pStructTuple =
        parser {
            let! kw = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWStruct) "Expected 'struct'"
            let! l = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWLParen) "Expected '('"
            let! e = refExpr.Parser
            let! r = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWRParen) "Expected ')'"

            let e =
                match e with
                | Expr.Tuple(es) -> es
                | e -> [ e ]

            return Expr.StructTuple(kw, l, e, r)
        }

    let pIfExpr =
        parser {
            let! ifTok = pIf
            let! cond = refExpr.Parser
            let! thenTok = pThen
            let! thenExpr = refExpr.Parser
            let! elifs = many ElifBranch.parse
            let! elseBranch = opt ElseBranch.parse
            return Expr.IfThenElse(ifTok, cond, thenTok, thenExpr, List.ofSeq elifs, elseBranch)
        }

    let pMatchExpr =
        parser {
            let! m = pMatch
            let! e = refExpr.Parser
            let! w = pWith
            let! rules = Rules.parse
            return Expr.Match(m, e, w, rules)
        }

    let atomExpr =
        choiceL
            [
                pConst
                pIdent
                pLetValue
                pParen
                pList
                pArray
                pStructTuple
                pBeginEnd
                pIfExpr
                pMatchExpr
            ]
            "atom expression"

    let operators = ExprOperatorParser()

    refExpr.Set(Operator.parser atomExpr operators)
    // Semicolon has special handling in F# lists
    // so we create a separate parser for expressions in lists
    // and set the starting precedence one level higher so it will be parsed in `pList`
    refExprInCollection.Set(
        Operator.parserAt (int PrecedenceLevel.Semicolon + 1 |> BindingPower.fromLevel) atomExpr operators
    )

    let parse = refExpr.Parser


[<AutoOpen>]
module private PatHelpers =
    let refPat =
        RefParser<Pat<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _>()

    let refPatParam =
        RefParser<PatParam<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _>()

[<RequireQualifiedAccess>]
module PatParam =
    // PatParam is a restricted subset of patterns often used for active pattern arguments

    let private pAtom =
        choiceL
            [
                nextNonTriviaTokenIsL Token.KWNull "null" |>> PatParam.Null

                nextNonTriviaTokenSatisfiesL
                    (fun synTok ->
                        let t = synTok.Token
                        t.IsLiteral || t = Token.KWTrue || t = Token.KWFalse
                    )
                    "Literal"
                |>> PatParam.Const

                // Identifiers / App
                parser {
                    let! lid = LongIdent.parse
                    // Check if it's an Application (App)
                    // This is recursive, e.g. Case(x) or Case x
                    // Simplified: Just consume one param if present?
                    // For PatParam, App usually implies `Ident(Param)`.
                    let! param = opt refPatParam.Parser

                    match param with
                    | ValueSome p -> return PatParam.App(lid, p)
                    | ValueNone -> return PatParam.LongIdent(lid)
                }

                // List [ ... ]
                parser {
                    let! l = nextNonTriviaTokenIsL Token.KWLBracket "["
                    let! parms, _ = sepBy refPatParam.Parser (nextNonTriviaTokenIsL Token.OpSemicolon ";")
                    let! r = nextNonTriviaTokenIsL Token.KWRBracket "]"
                    return PatParam.List(l, List.ofSeq parms, r)
                }

                // Tuple ( ... )
                parser {
                    let! l = nextNonTriviaTokenIsL Token.KWLParen "("
                    let! parms, _ = sepBy refPatParam.Parser (nextNonTriviaTokenIsL Token.OpComma ",")
                    let! r = nextNonTriviaTokenIsL Token.KWRParen ")"
                    return PatParam.Tuple(l, List.ofSeq parms, r)
                }

                // Quoted
                parser {
                    let! l = nextNonTriviaTokenIsL Token.OpQuotationTypedLeft "<@"
                    let! e = Expr.parse
                    let! r = nextNonTriviaTokenIsL Token.OpQuotationTypedRight "@>"
                    return PatParam.Quoted(l, e, r)
                }
            ]
            "PatParam Atom"

    let parse =
        parser {
            let! atom = pAtom
            // Check for Typed: atom : Type
            let! typed = opt (nextNonTriviaTokenIsL Token.OpColon ":")

            match typed with
            | ValueSome colon ->
                let! t = Type.parse
                return PatParam.Typed(atom, colon, t)
            | ValueNone -> return atom
        }

    do refPatParam.Set parse

[<RequireQualifiedAccess>]
module FieldPat =
    let parse: Parser<FieldPat<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! lid = LongIdent.parse
            let! eq = pEquals
            let! p = refPat.Parser
            return FieldPat.FieldPat(lid, eq, p)
        }

[<RequireQualifiedAccess>]
module ListPat =
    let parse: Parser<ListPat<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! l = nextNonTriviaTokenIsL Token.KWLBracket "["
            let! pats, _ = sepBy refPat.Parser (nextNonTriviaTokenIsL Token.OpSemicolon ";")
            let! r = nextNonTriviaTokenIsL Token.KWRBracket "]"
            return ListPat.ListPat(l, List.ofSeq pats, r)
        }

[<RequireQualifiedAccess>]
module ArrayPat =
    let parse: Parser<ArrayPat<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! l = nextNonTriviaTokenIsL Token.KWLArrayBracket "[|"
            let! pats, _ = sepBy refPat.Parser (nextNonTriviaTokenIsL Token.OpSemicolon ";")
            let! r = nextNonTriviaTokenIsL Token.KWRArrayBracket "|]"
            return ArrayPat.ArrayPat(l, List.ofSeq pats, r)
        }

[<RequireQualifiedAccess>]
module RecordPat =
    let parse: Parser<RecordPat<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! l = nextNonTriviaTokenIsL Token.KWLBrace "{"
            let! fields, _ = sepBy1 FieldPat.parse (nextNonTriviaTokenIsL Token.OpSemicolon ";")
            let! r = nextNonTriviaTokenIsL Token.KWRBrace "}"
            return RecordPat.RecordPat(l, List.ofSeq fields, r)
        }

[<RequireQualifiedAccess>]
module Pat =

    // --- Precedence Levels ---
    // 1. Atom
    // 2. TypeTest / Typed (:? / :)
    // 3. Cons (::) - Right Assoc
    // 4. And (&) - Left Assoc
    // 5. Or (|) - Left Assoc
    // 6. As (as) - Right Assoc

    let private pAtom =
        choiceL
            [
                nextNonTriviaTokenIsL Token.Wildcard "_" |>> Pat.Wildcard
                nextNonTriviaTokenIsL Token.KWNull "null" |>> Pat.Null

                // Type Tests (:? type)
                parser {
                    let! q = nextNonTriviaTokenIsL Token.OpTypeTest ":?"
                    let! t = Type.parse

                    let! asOpt =
                        opt (
                            parser {
                                let! a = nextNonTriviaTokenIsL Token.KWAs "as"
                                let! i = nextNonTriviaTokenIsL Token.Identifier "ident"
                                return (a, i)
                            }
                        )

                    match asOpt with
                    | ValueSome(a, i) -> return Pat.TypeTestAs(q, t, a, i)
                    | ValueNone -> return Pat.TypeTest(q, t)
                }

                // Collections
                ListPat.parse |>> Pat.List
                ArrayPat.parse |>> Pat.Array
                RecordPat.parse |>> Pat.Record

                // Struct Tuple
                parser {
                    let! s = nextNonTriviaTokenIsL Token.KWStruct "struct"
                    let! l = nextNonTriviaTokenIsL Token.KWLParen "("
                    let! pats, _ = sepBy refPat.Parser (nextNonTriviaTokenIsL Token.OpComma ",")
                    let! r = nextNonTriviaTokenIsL Token.KWRParen ")"
                    return Pat.StructTuple(s, l, List.ofSeq pats, r)
                }

                // Paren or Tuple
                parser {
                    let! l = nextNonTriviaTokenIsL Token.KWLParen "("
                    let! pats, _ = sepBy refPat.Parser (nextNonTriviaTokenIsL Token.OpComma ",")
                    let! r = nextNonTriviaTokenIsL Token.KWRParen ")"

                    let pList = List.ofSeq pats

                    match pList with
                    | [ x ] -> return Pat.Paren(l, x, r)
                    | _ -> return Pat.Tuple(pList)
                }

                // Literals (Const) vs Identifier vs LongIdent (Named)
                parser {
                    // If it's a literal token, it's Const
                    // If Identifier, could be NamedSimple (variable) or Named (constructor)
                    // If LongIdent, it's Named (Constructor/Literal Enum)

                    // We peek/try LongIdent first.
                    // Note: Standard literals (1, "s") are single tokens.

                    return!
                        choice
                            [
                                nextNonTriviaTokenSatisfiesL (fun t -> t.Token.IsLiteral) "Literal"
                                |>> Pat.Const

                                parser {
                                    let! lid = LongIdent.parse
                                    // Check for arguments (constructor pattern)
                                    // Arguments could be an Atom (Paren, Struct, List, Array, Record, Const, Null, Wildcard, Ident)
                                    // But NOT operator based pattern without parens?
                                    // E.g. Case x is valid. Case x :: xs is (Case x) :: xs.
                                    // So we try to parse an atomic pattern as the argument.

                                    // BUT: NamedSimple is just `x`. `x` is also `LongIdent [x]`.
                                    // Distinction: NamedSimple is for bind variables. Named is for Destructuring.
                                    // Typically, if it starts with uppercase or is multi-part, it's Named.
                                    // Lowercase single is NamedSimple.
                                    // This logic is semantic, but parsers often approximate.

                                    // For this implementation:
                                    // 1. Try parse argument (recursive atomic).
                                    let! arg = opt pAtom // Recursive call to pAtom? Careful of infinite loop.
                                    // Actually, recursive call to pAtom is safe because we consumed LongIdent.
                                    let! state = getUserState

                                    match arg, lid with
                                    | ValueSome a, _ -> return Pat.Named(lid, ValueNone, Some a)
                                    | ValueNone, [ id ] when
                                        not (
                                            Char.IsUpper(
                                                tokenStringIs "" id state |> ignore
                                                'a'
                                            )
                                        )
                                        ->
                                        // Heuristic: if single ident, use NamedSimple.
                                        // Real parser checks "is constructor" context or casing convention.
                                        // Here simplified: simple ident -> NamedSimple.
                                        return Pat.NamedSimple(id)
                                    | ValueNone, _ -> return Pat.Named(lid, ValueNone, None)
                                }
                            ]
                }
            ]
            "Pattern Atom"

    let private pTyped =
        parser {
            let! pat = pAtom
            let! colon = opt (nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpColon) ":")

            match colon with
            | ValueSome c ->
                let! t = Type.parse
                return Pat.Typed(pat, c, t)
            | ValueNone -> return pat
        }

    let private pCons =
        let rec loop () =
            parser {
                let! head = pTyped
                let! cons = opt (nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWColonColon) "::")

                match cons with
                | ValueSome c ->
                    let! tail = loop () // Right associative
                    return Pat.Cons(head, c, tail)
                | ValueNone -> return head
            }

        loop ()

    let private pAnd =
        chainl1
            pCons
            (nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpBitwiseAnd) "&"
             |>> (fun op l r -> Pat.And(l, op, r)))

    let private pOr =
        chainl1
            pAnd
            (nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpBar) "|"
             |>> (fun op l r -> Pat.Or(l, op, r)))

    let private pAs =
        let rec loop () =
            parser {
                let! left = pOr
                let! asTok = opt (nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWAs) "as")

                match asTok with
                | ValueSome op ->
                    let! ident = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.Identifier) "ident"
                    return Pat.As(left, op, ident)
                | ValueNone -> return left
            }

        loop ()

    let parse =
        parser {
            let! attrs = opt Attributes.parse
            let! p = pAs

            match attrs with
            | ValueSome a when not (List.isEmpty a) -> return Pat.Attributed(a, p)
            | _ -> return p
        }

    do refPat.Set parse

[<RequireQualifiedAccess>]
module PatternGuard =
    let parse: Parser<PatternGuard<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! w = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.KWWhen) "when"
            let! e = Expr.parse
            return PatternGuard.PatternGuard(w, e)
        }

[<RequireQualifiedAccess>]
module Rule =
    let parse: Parser<Rule<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! pat = Pat.parse
            let! guard = opt PatternGuard.parse
            let! arrow = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpArrowRight) "->"
            let! expr = Expr.parse
            return Rule.Rule(pat, guard, arrow, expr)
        }

[<RequireQualifiedAccess>]
module Rules =
    let parse: Parser<Rules<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            // Optional leading bar
            let! firstBar = opt (nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpBar) "|")

            // First rule
            let! firstRule = Rule.parse

            // Subsequent rules separated by bar
            let! rest =
                many (
                    parser {
                        let! bar = nextNonTriviaTokenSatisfiesL (fun t -> t.Token = Token.OpBar) "|"
                        let! rule = Rule.parse
                        return (ValueSome bar, rule)
                    }
                )

            let rulesList =
                let first = firstBar, firstRule
                first :: List.ofSeq rest

            return Rules.Rules(rulesList)
        }


[<RequireQualifiedAccess>]
module Reader =
    let ofLexed (lexed: Lexed) input =
        let initialState = ParseState.create lexed input
        Reader.ofImmutableArray (lexed.Tokens.ToImmutableArray()) initialState
