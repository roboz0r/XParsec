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

// https://fsharp.github.io/fslang-spec/lexical-filtering/#1516-full-list-of-offside-contexts

[<RequireQualifiedAccess>]
type OffsideContext =
    | Let
    | If
    | Try
    | Lazy
    | Fun
    | Function
    /// The `with` keyword as part of a record expression or an object expression whose members use the syntax `{ new Foo with M() = 1 and N() = 2 }`
    | WithLet
    /// The `with` keyword as part of an extension, interface, or object expression whose members use the syntax `{ new Foo member x.M() = 1 member x. N() = 2 }`
    | WithAugment
    | Match
    | For
    | While
    | Then
    | Else
    | Do
    | Type
    | Namespace
    | Module
    /// The `member`, `abstract`, `default`, or `override` keyword, if the Member context is not already active, because multiple tokens may be present.
    /// - or -
    /// `(` is the next token after the `new` keyword. This distinguishes the member declaration `new(x) = ...` from the expression `new x()`
    | Member
    | Paren
    | Bracket
    | Brace
    | BracketBar
    | Begin
    | Struct
    | Sig
    | Quote
    /// The `with` keyword in a Try or Match context immediately after a `function` keyword.
    | MatchClauses
    /// An otherwise unprocessed keyword in a SeqBlock context.
    | Vanilla

type Offside =
    { Context: OffsideContext; Indent: int }


type DiagnosticCode =
    // TODO: Use F# error codes
    | Other of string
    | TyparInConstant of Typar<SyntaxToken>

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
        Context: Offside list
        Diagnostics: Diagnostic list
        DefinedSymbols: Set<string>
        IndentationMode: Syntax
        mutable LastLine: int<line> // ok to be mutable since it's only used as a guess
        ReprocessOpAfterTypeDeclaration: bool
    }

module ParseState =
    let create (lexed: Lexed) input definedSymbols =
        {
            Input = input
            Lexed = lexed
            Context = []
            Diagnostics = []
            DefinedSymbols = definedSymbols
            IndentationMode = Syntax.Light
            LastLine = 0<line>
            ReprocessOpAfterTypeDeclaration = false
        }

    let setIndentOn (state: ParseState) =
        { state with IndentationMode = Syntax.Light }

    let setIndentOff (state: ParseState) =
        { state with IndentationMode = Syntax.Verbose }

    let pushOffside offsideCtx (state: ParseState) = 
        { state with Context = offsideCtx :: state.Context }

    let popOffside (state: ParseState) = 
            match state.Context with 
            | [] -> invalidOp "Attempted to pop empty context"
            | head :: tail ->
                { state with Context = tail }

    let addDiagnostic code startToken endToken (state: ParseState) =
        let diag =
            {
                Code = code
                Token = startToken
                TokenEnd = endToken
            }
        { state with Diagnostics = diag :: state.Diagnostics}

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
            0
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

    let isDefined (state: ParseState) (symbol: SyntaxToken) =
        let symbol = tokenString symbol state
        state.DefinedSymbols.Contains(symbol)

[<AutoOpen>]
module Parsing =
    let tokenIndex (reader: Reader<PositionedToken, ParseState, 'a, 'b>) = int reader.Index * 1<token>

    let syntaxToken token (index: int) =
        {
            PositionedToken = token
            Index = TokenIndex.Regular(index * 1<token>)
        }

    let virtualToken token =
        {
            PositionedToken = token
            Index = TokenIndex.Virtual
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
            | Token.Tab -> state.IndentationMode = Syntax.Verbose
            | _ -> false

    let tokenString (token: SyntaxToken) (state: ParseState) =
        match token.Index with
        | TokenIndex.Virtual -> ""
        | TokenIndex.Regular iT ->
            let tokens = state.Lexed.Tokens

            match tokens[iT].Token with
            | Token.EOF -> ""
            | _ ->
                let t1 = tokens[iT + 1<_>] // Next token is guaranteed to exist (EOF)
                state.Input.[int token.StartIndex .. (t1.StartIndex - 1)]

    let tokenStringIs (s: string) (token: SyntaxToken) (state: ParseState) =
        match token.Index with
        | TokenIndex.Virtual -> false
        | TokenIndex.Regular iT ->
            let tokens = state.Lexed.Tokens

            match tokens[iT].Token with
            | Token.EOF -> false
            | _ ->
                let t1 = tokens[iT + 1<_>] // Next token is guaranteed to exist (EOF)
                let tokenStr = state.Input.[int token.StartIndex .. (t1.StartIndex - 1)]
                // printfn "Comparing token string '%s' to '%s'" tokenStr s
                tokenStr = s

    let tokenStringStartsWith (s: string) (token: SyntaxToken) (state: ParseState) =
        match token.Index with
        | TokenIndex.Virtual -> false
        | TokenIndex.Regular iT ->
            let tokens = state.Lexed.Tokens

            match tokens[iT].Token with
            | Token.EOF -> false
            | _ ->
                let t1 = tokens[iT + 1<_>] // Next token is guaranteed to exist (EOF)
                let tokenStr = state.Input.[token.StartIndex .. (t1.StartIndex - 1)]
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

    let opComparer =
        // For parsing, we only care about the operator token itself for equality
        { new IEqualityComparer<SyntaxToken> with
            member _.Equals(x, y) = x.Token = y.Token
            member _.GetHashCode(obj) = hash obj.Token
        }

type FSParser<'T> =
    Parser<
        'T,
        PositionedToken,
        ParseState,
        ReadableImmutableArray<PositionedToken>,
        ReadableImmutableArraySlice<PositionedToken>
     >

/// Represents #if expressions used in conditional compilation
[<RequireQualifiedAccess>]
type IfExpr<'T> =
    | Term of 'T
    | And of IfExpr<'T> * 'T * IfExpr<'T> // &&
    | Or of IfExpr<'T> * 'T * IfExpr<'T> // ||
    | Not of 'T * IfExpr<'T> // !
    | Paren of 'T * IfExpr<'T> * 'T // ( ... )

[<RequireQualifiedAccess>]
type IfOp =
    | And
    | Or
    | Not
    | LParen
    | RParen

// [<RequireQualifiedAccess; Struct>]
// type IfAtom =
//     | Term of string
//     | Invalid of c: char

// [<RequireQualifiedAccess>]
// type IfExpr2 =
//     | Term of string
//     | And of IfExpr2 * IfExpr2
//     | Or of IfExpr2 * IfExpr2
//     | Not of IfExpr2

// [<RequireQualifiedAccess>]
// module IfExpr2 =
//     open XParsec.CharParsers
//     let pSpaces = many (satisfyL Char.IsWhiteSpace "whitespace") >>% ()
//     let pTerm = pSpaces >>. Lexing.IdentifierParser<unit>.Parse |>> IfExpr2.Term
//     let pAnd = pSpaces >>. pstring "&&" >>% IfOp.And
//     let pOr = pSpaces >>. pstring "||" >>% IfOp.Or
//     let pNot = pSpaces >>. pstring "!" >>% IfOp.Not
//     let pLParen = pSpaces >>. pchar '(' >>% IfOp.LParen
//     let pRParen = pSpaces >>. pchar ')' >>% IfOp.RParen

//     let completeOr (l: IfExpr2) (_: IfOp) (r: IfExpr2) = IfExpr2.Or(l, r)
//     let completeAnd (l: IfExpr2) (_: IfOp) (r: IfExpr2) = IfExpr2.And(l, r)
//     let completeNot (_: IfOp) (r: IfExpr2) = IfExpr2.Not(r)

//     let operators: Operators<IfOp, unit, IfExpr2, char, unit, ReadableString, ReadableStringSlice> =
//         [
//             Operator.infixLeftAssoc IfOp.Or P1 pOr completeOr
//             Operator.infixLeftAssoc IfOp.And P2 pAnd completeAnd
//             Operator.prefix IfOp.Not P3 pNot completeNot
//             Operator.enclosedBy IfOp.LParen IfOp.RParen P10 pLParen pRParen (fun _ m _ -> m)
//         ]
//         |> Operator.create

//     let parse: Parser<IfExpr2, char, unit, ReadableString, _> =
//         Operator.parser pTerm operators

//     let evaluate (expr: IfExpr2) (isDefined: string -> bool) : bool =
//         let rec eval e =
//             match e with
//             | IfExpr2.Term t -> isDefined t
//             | IfExpr2.And(l, r) -> eval l && eval r
//             | IfExpr2.Or(l, r) -> eval l || eval r
//             | IfExpr2.Not r -> not (eval r)

//         eval expr

// TODO: Create #if context in lexer and only lex #if compatible tokens on that line
// Change the IfExpr parser to use those tokens

[<RequireQualifiedAccess>]
module IfExpr =
    // Operator precedence parser for #if expressions

    [<RequireQualifiedAccess>]
    type IfExprAux = unit

    type IfExprParser() =
        static let orPrecedence = BindingPower.fromLevel 1 // ||
        static let andPrecedence = BindingPower.fromLevel 2 // &&
        static let notPrecedence = BindingPower.fromLevel 3 // !
        static let parenPrecedence = BindingPower.fromLevel 4 // ( )
        // --- Completion Functions ---

        static let completeInfix (l: IfExpr<SyntaxToken>) (op: SyntaxToken) (r: IfExpr<SyntaxToken>) =
            match op.Token with
            | Token.OpBarBar -> IfExpr.Or(l, op, r)
            | Token.OpAmpAmp -> IfExpr.And(l, op, r)
            | _ -> failwithf "Unexpected infix if operator: %A" op

        static let completePrefix (op: SyntaxToken) (e: IfExpr<SyntaxToken>) =
            match op.Token with
            | Token.OpDereference -> IfExpr.Not(op, e)
            | _ -> failwithf "Unexpected prefix if operator: %A" op

        static let completeParen (l: SyntaxToken) (m: IfExpr<SyntaxToken>) (r: SyntaxToken) = IfExpr.Paren(l, m, r)

        // --- Operator Parsers ---

        static let lhsParser =
            nextNonTriviaToken
            >>= fun token ->
                match token.Token with
                | Token.OpDereference ->
                    // Parenthesized measure
                    let p = preturn token
                    let op = Prefix(token, p, notPrecedence, completePrefix)
                    preturn op
                | Token.KWLParen ->
                    // Parenthesized expression
                    let p = preturn token

                    let rParen =
                        virtualToken (PositionedToken.Create(Token.KWRParen, token.StartIndex + 1))

                    let op = Enclosed(token, p, parenPrecedence, rParen, p, completeParen)
                    preturn op
                | _ -> fail (Message "Not a valid LHS #if operator")

        static let rhsParser =
            nextNonTriviaToken
            >>= fun token ->
                match token.Token with
                | Token.OpBarBar ->
                    let p = preturn token
                    let op = InfixLeft(token, p, orPrecedence, completeInfix)
                    preturn op
                | Token.OpAmpAmp ->
                    let p = preturn token
                    let op = InfixLeft(token, p, andPrecedence, completeInfix)
                    preturn op
                | _ -> fail (Message "Not a valid RHS #if operator")

        interface Operators<
            SyntaxToken,
            IfExprAux,
            IfExpr<SyntaxToken>,
            PositionedToken,
            ParseState,
            ReadableImmutableArray<PositionedToken>,
            ReadableImmutableArraySlice<PositionedToken>
         > with
            member _.LhsParser = lhsParser
            member _.RhsParser = rhsParser
            member _.OpComparer = opComparer

    let atomParser: FSParser<IfExpr<SyntaxToken>> =
        nextNonTriviaToken
        >>= fun token ->
            match token.Token with
            | Token.Identifier -> preturn (IfExpr.Term token)
            | t when t.IsKeyword -> preturn (IfExpr.Term token)
            | _ -> fail (Message "Not a valid #if term")

    let opParser = IfExprParser()

    let parse: FSParser<IfExpr<SyntaxToken>> = Operator.parser atomParser opParser

    let evaluate (expr: IfExpr<SyntaxToken>) (isDefined: SyntaxToken -> bool) : bool =
        let rec eval e =
            match e with
            | IfExpr.Term t -> isDefined t
            | IfExpr.And(l, _, r) -> eval l && eval r
            | IfExpr.Or(l, _, r) -> eval l || eval r
            | IfExpr.Not(_, r) -> not (eval r)
            | IfExpr.Paren(_, m, _) -> eval m

        eval expr

    let evaluateStateful (expr: IfExpr<SyntaxToken>) (state: ParseState) : bool =
        let isDefined token = ParseState.isDefined state token
        evaluate expr isDefined


[<RequireQualifiedAccess>]
module IfDirective =
    let pIfDirective: Parser<_, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! _ = opt (satisfyL (fun t -> t.Token = Token.Indent) "Optional indent before #if")
            return! satisfyL (fun t -> t.Token = Token.IfDirective) "Expected '#if' directive"
        }

    let getLineStringAfter (state: ParseState) (i: int<token>) =
        let token = state.Lexed.Tokens[i]
        let startIndex = token.StartIndex
        let line = ParseState.findLineNumber state.Lexed state.LastLine i
        let nextLine = line + 1<_>

        if nextLine >= state.Lexed.LineStarts.LengthM then
            // Last line
            state.Input.[startIndex..]
        else
            let nextLineTokenIndex = state.Lexed.LineStarts[nextLine]
            let offEndToken = state.Lexed.Tokens[nextLineTokenIndex]
            state.Input.[startIndex .. offEndToken.StartIndex - 1]

    let skipToIndex i (reader: Reader<_, _, _, _>) =
        reader.Index <- i
        Ok()

    // let parse: Parser<IfExpr2 voption, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
    //     parser {
    //         let! ifDirective = pIfDirective
    //         let! pos = getPosition
    //         let state = pos.State
    //         let i = int pos.Index * 1<token>
    //         let s = getLineStringAfter state i
    //         let line = ParseState.findLineNumber state.Lexed state.LastLine i

    //         let iNext =
    //             if line + 1<_> < state.Lexed.LineStarts.LengthM then
    //                 state.Lexed.LineStarts[line + 1<_>] - 1<_>
    //             else
    //                 state.Lexed.Tokens.LengthM - 1<_>

    //         do! skipToIndex (int iNext)
    //         let reader = Reader.ofString s ()

    //         match IfExpr2.parse reader with
    //         | Error e ->
    //             // Add diagnostic
    //             // TODO: Better error message, recorrelating position
    //             ParseState.addDiagnostic (DiagnosticCode.Other $"Invalid #if expression {e}") ifDirective None state
    //             return ValueNone
    //         | Ok expr -> return ValueSome expr
    //     }

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

    let pLParen = nextNonTriviaTokenIsL Token.KWLParen "("
    let pRParen = nextNonTriviaTokenIsL Token.KWRParen ")"
    let pOpConcatenate = nextNonTriviaTokenIsL Token.OpConcatenate "^"
    let pOpMultiply = nextNonTriviaTokenIsL Token.OpMultiply "*"

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
                            PositionedToken = PositionedToken.Create(Token.OpMultiply, token.StartIndex + 1)
                        }

                    let rParen =
                        { token with
                            Index = TokenIndex.Virtual
                            PositionedToken = PositionedToken.Create(Token.KWRParen, token.StartIndex + 2)
                        }

                    return IdentOrOp.StarOp(lParen, star, rParen)
                })
                // Subcase 2b: Parsed as separate tokens ( * )
                <|> (parser {
                    let! l = pLParen
                    let! s = pOpMultiply
                    let! r = pRParen
                    return IdentOrOp.StarOp(l, s, r)
                })

                // Case 3: Parenthesized Operator (op) or Active Pattern (| ... |)
                <|> (parser {
                    let! l = pLParen
                    let! op = OpName.parse
                    let! r = pRParen
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
            let! caret = pOpConcatenate
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
            let! arrow = pArrowRight
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

                    let! ts, _ = sepBy refType.Parser pOpMultiply

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
                        let! _ = pOpMultiply
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
                let! arrow = opt pArrowRight

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

    let private pAll = pOpMultiply |>> SliceRange.All

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

    let private pLiteral: Parser<_, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        nextNonTriviaTokenSatisfiesL isLiteralToken "Expected constant literal"

    let private pMeasure =
        parser {
            let! pos = getPosition

            let! lAngle = // '<' must be immediately after literal
                satisfyL (fun (t: PositionedToken) -> t.Token = Token.OpLessThan) "Expected '<' for measure"

            let! m = Measure.parse
            let! rAngle = nextNonTriviaTokenIsL (Token.OpGreaterThan) "Expected '>' for measure"
            return struct (syntaxToken lAngle pos.Index, m, rAngle)
        }

    /// <summary>
    /// Traverses the measure AST and collects all Type Variable nodes (e.g. 'u).
    /// Used to validate that a measure literal (e.g. float<...>) does not contain type variables.
    /// </summary>
    let collectTypars (measure: Measure<'T>) : ResizeArray<Typar<'T>> =
        let results = ResizeArray()

        let rec visit m =
            match m with
            // The Target: Found a type variable
            | Measure.Typar token -> results.Add(token)

            // Base cases: Clean
            | Measure.Named _
            | Measure.One _
            | Measure.Anonymous _ -> ()

            // Unary recursive cases
            | Measure.Paren(_, inner, _)
            | Measure.Reciprocal(_, inner)
            | Measure.Power(inner, _, _) -> visit inner

            // Binary recursive cases
            | Measure.Product(l, _, r)
            | Measure.Quotient(l, _, r) ->
                visit l
                visit r

            // N-ary recursive case
            | Measure.Juxtaposition(ms, ops) ->
                for m in ms do
                    visit m

        visit measure
        results

    let validateMeasureNoTypars (measure: Measure<SyntaxToken>) (reader: Reader<_, _, _, _>) =
        let typars = collectTypars measure

        if typars.Count > 0 then
            let mutable state: ParseState = reader.State

            for t in typars do
                let code = TyparInConstant t

                let tokStart, tokEnd =
                    match t with
                    | Typar.Anon x -> x.PositionedToken, None
                    | Typar.Named(x, x1) -> x.PositionedToken, Some x1.PositionedToken
                    | Typar.Static(x, x1) -> x.PositionedToken, Some x1.PositionedToken

                state <- ParseState.addDiagnostic code tokStart tokEnd state
            reader.State <- state

        preturn () reader

    let parse: Parser<Constant<_>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! literal = pLiteral

            if literal.Token.IsNumeric then
                let! measure = opt pMeasure

                match measure with
                | ValueSome(lAngle, m, rAngle) ->
                    do! validateMeasureNoTypars m
                    return Constant.MeasuredLiteral(literal, lAngle, m, rAngle)
                | ValueNone -> return Constant.Literal literal
            else
                return Constant.Literal literal
        }

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
                                do! updateUserState (fun state -> { state with ReprocessOpAfterTypeDeclaration = true })
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
                            do! updateUserState (fun state -> { state with ReprocessOpAfterTypeDeclaration = false })

                            let reprocessedToken =
                                let newStart = token.StartIndex + 1 // Adjust start index to account for consumed '>'

                                let tokenString =
                                    match token.Index with
                                    | TokenIndex.Virtual -> failwith "Cannot re-lex virtual token"
                                    | TokenIndex.Regular iT ->
                                        let t1 = state.Lexed.Tokens[iT + 1<token>] // Next token after operator always exists as EOF is present
                                        state.Input.[newStart .. (t1.StartIndex - 1)]
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

[<RequireQualifiedAccess>]
module PatParam =
    // PatParam is a restricted subset of patterns used for active pattern arguments
    let private refPatParam =
        RefParser<PatParam<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _>()

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

module Pat =

    [<RequireQualifiedAccess>]
    type PatAux =
        | Type of Type<SyntaxToken>
        | AsIdent of SyntaxToken

    type PatOperatorParser() =
        // --- Precedence Definitions ---
        static let tuplePrecedence = BindingPower.fromLevel (int PrecedenceLevel.Comma)
        static let asPrecedence = BindingPower.fromLevel (int PrecedenceLevel.As)
        static let pipePrecedence = BindingPower.fromLevel (int PrecedenceLevel.Pipe)
        static let andPrecedence = BindingPower.fromLevel (int PrecedenceLevel.LogicalAnd)
        static let colonPrecedence = BindingPower.fromLevel (int PrecedenceLevel.TypeTest)
        static let consPrecedence = BindingPower.fromLevel (int PrecedenceLevel.Cons)
        static let parenPrecedence = BindingPower.fromLevel (int PrecedenceLevel.Parens)
        static let structPrecedence = BindingPower.fromLevel (int PrecedenceLevel.HighApplication)

        // --- Completion Functions ---

        static let completeInfix (l: Pat<SyntaxToken>) (op: SyntaxToken) (r: Pat<SyntaxToken>) =
            match op.Token with
            | Token.OpBar -> Pat.Or(l, op, r)
            | Token.OpAmp -> Pat.And(l, op, r)
            | Token.KWColonColon -> Pat.Cons(l, op, r)
            | _ -> failwithf "Unexpected infix pattern operator: %A" op

        static let completeTuple (elements: ResizeArray<Pat<SyntaxToken>>) (ops: ResizeArray<SyntaxToken>) =
            Pat.Tuple(List.ofSeq elements, List.ofSeq ops)

        static let completeTyped (l: Pat<SyntaxToken>) (op: SyntaxToken) (aux: PatAux) =
            match aux with
            | PatAux.Type t -> Pat.Typed(l, op, t)
            | _ -> failwith "Expected Type aux for Typed pattern"

        static let completeAs (l: Pat<SyntaxToken>) (op: SyntaxToken) (aux: PatAux) =
            match aux with
            | PatAux.AsIdent ident -> Pat.As(l, op, ident)
            | _ -> failwith "Expected Ident aux for As pattern"

        static let completeParen (l: SyntaxToken) (p: Pat<SyntaxToken>) (r: SyntaxToken) = Pat.Paren(l, p, r)

        static let completeStruct (op: SyntaxToken) (r: Pat<SyntaxToken>) =
            match r with
            | Pat.Paren(l, Pat.Tuple(elements, ops), r) -> Pat.StructTuple(op, l, elements, ops, r)
            | _ ->
                // TODO: Error - struct must be followed by tuple
                Pat.Struct(op, r)


        // --- Aux Parsers ---

        static let pTypeRhs = Type.parse |>> PatAux.Type

        static let pAs = nextNonTriviaTokenIsL Token.KWAs "Expected identifier for 'as' pattern"

        static let pAsRhs =
            parser {
                let! ident = pAs
                return PatAux.AsIdent ident
            }

        // --- Main Parsers ---
        static let lhsParser =
            parser {
                let! token = nextNonTriviaToken

                match token.Token with
                | Token.KWLParen ->
                    // Start of tuple pattern ( ... )
                    // This is a Prefix Operator on a pattern
                    let p = preturn token
                    let rParen = virtualToken (PositionedToken.Create(Token.KWRParen, 0))
                    let op = Enclosed(token, p, parenPrecedence, rParen, pRParen, completeParen)
                    return op
                | Token.KWStruct ->
                    let p = preturn token
                    // Create Prefix operator
                    // This will parse the immediate next pattern (e.g. Paren, or erroneously Literal/List)
                    let op = Prefix(token, p, structPrecedence, completeStruct)
                    return op
                | _ -> return! fail (Message "Not a prefix pattern operator")
            }

        static let rhsParser =
            nextNonTriviaToken
            >>= fun token ->
                match token.Token with
                // Infix Left: | (Or), & (And)
                | Token.OpBar ->
                    let op = InfixLeft(token, preturn token, pipePrecedence, completeInfix)
                    preturn op

                | Token.OpAmp ->
                    let op = InfixLeft(token, preturn token, andPrecedence, completeInfix)
                    preturn op

                // Infix Right: :: (Cons)
                | Token.KWColonColon ->
                    let op = InfixRight(token, preturn token, consPrecedence, completeInfix)
                    preturn op

                // Infix Mapped: : (Typed)
                | Token.OpColon ->
                    let op = InfixMapped(token, preturn token, colonPrecedence, pTypeRhs, completeTyped)
                    preturn op

                // Infix Mapped: as (As)
                | Token.KWAs ->
                    let op = InfixMapped(token, preturn token, asPrecedence, pAsRhs, completeAs)
                    preturn op

                // Infix N-ary: , (Tuple)
                | Token.OpComma ->
                    let op = InfixNary(token, preturn token, tuplePrecedence, completeTuple)
                    preturn op

                | _ -> fail (Message "Not a valid RHS pattern operator")

        interface Operators<
            SyntaxToken,
            PatAux,
            Pat<SyntaxToken>,
            PositionedToken,
            ParseState,
            ReadableImmutableArray<PositionedToken>,
            ReadableImmutableArraySlice<PositionedToken>
         > with
            member _.LhsParser = lhsParser
            member _.RhsParser = rhsParser
            member _.OpComparer = opComparer

    let private refPat =
        RefParser<Pat<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _>()

    let pListPat: Parser<ListPat<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! l = nextNonTriviaTokenIsL Token.KWLBracket "["
            let! pats, _ = sepEndBy refPat.Parser (nextNonTriviaTokenIsL Token.OpSemicolon ";")
            let! r = nextNonTriviaTokenIsL Token.KWRBracket "]"
            return ListPat.ListPat(l, List.ofSeq pats, r)
        }

    let pArrayPat: Parser<ArrayPat<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! l = nextNonTriviaTokenIsL Token.KWLArrayBracket "[|"
            let! pats, _ = sepEndBy refPat.Parser (nextNonTriviaTokenIsL Token.OpSemicolon ";")
            let! r = nextNonTriviaTokenIsL Token.KWRArrayBracket "|]"
            return ArrayPat.ArrayPat(l, List.ofSeq pats, r)
        }

    let pFieldPat =
        parser {
            let! lid = LongIdent.parse
            let! eq = pEquals
            let! p = refPat.Parser
            return FieldPat.FieldPat(lid, eq, p)
        }

    let pRecordPat: Parser<RecordPat<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! l = nextNonTriviaTokenIsL Token.KWLBrace "{"
            let! fields, _ = sepEndBy1 pFieldPat (nextNonTriviaTokenIsL Token.OpSemicolon ";")
            let! r = nextNonTriviaTokenIsL Token.KWRBrace "}"
            return RecordPat.RecordPat(l, List.ofSeq fields, r)
        }

    let pNamed =
        parser {
            let! lid = LongIdent.parse
            let! param = opt PatParam.parse
            let! arg = opt refPat.Parser

            match lid, param, arg with
            | [ name ], ValueNone, ValueNone ->
                // Simple named pattern (variable)
                return Pat.NamedSimple(name)
            | _ ->
                // Full named pattern
                return Pat.Named(lid, param, arg)
        }

    let pTypeTest =
        parser {
            let! op = nextNonTriviaTokenIsL Token.OpTypeTest ":?"
            let! t = Type.parse
            // Check optional 'as ident'
            let! asClause =
                opt (
                    parser {
                        let! asTok = nextNonTriviaTokenIsL Token.KWAs "as"
                        let! id = nextNonTriviaTokenSatisfiesL (fun t -> t.Token.IsIdentifier) "identifier"
                        return struct (asTok, id)
                    }
                )

            match asClause with
            | ValueSome(asTok, id) -> return Pat.TypeTestAs(op, t, asTok, id)
            | ValueNone -> return Pat.TypeTest(op, t)
        }

    let pAttributesPat =
        parser {
            let! attrs = Attributes.parse
            let! pat = refPat.Parser
            return Pat.Attributed(attrs, pat)
        }

    let pPatAtom =
        choiceL
            [
                nextNonTriviaTokenIsL Token.Wildcard "_" |>> Pat.Wildcard
                nextNonTriviaTokenIsL Token.KWNull "null" |>> Pat.Null
                Constant.parse |>> Pat.Const
                pTypeTest
                pNamed
                pListPat |>> Pat.List
                pArrayPat |>> Pat.Array
                pRecordPat |>> Pat.Record
                pAttributesPat
            ]
            "Pattern Atom"

    let parse = Operator.parser pPatAtom (PatOperatorParser())


[<RequireQualifiedAccess>]
module PatternGuard =
    let pWhen = nextNonTriviaTokenIsL Token.KWWhen "when"

    let parse: Parser<PatternGuard<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! w = pWhen
            let! e = Expr.parse
            return PatternGuard.PatternGuard(w, e)
        }

[<RequireQualifiedAccess>]
module Rule =

    let parse: Parser<Rule<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! pat = Pat.parse
            let! guard = opt PatternGuard.parse
            let! arrow = pArrowRight
            let! expr = Expr.parse
            return Rule.Rule(pat, guard, arrow, expr)
        }

[<RequireQualifiedAccess>]
module Rules =
    let pOpBar = nextNonTriviaTokenIsL Token.OpBar "|"

    let parse: Parser<Rules<SyntaxToken>, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            // Optional leading bar
            let! firstBar = opt pOpBar
            let! rules, bars = sepBy1 Rule.parse pOpBar
            return Rules(firstBar, List.ofSeq rules, List.ofSeq bars)
        }

[<AutoOpen>]
module internal TypeDefnHelpers =
    // Forward reference for MemberDefn (overriding the one in your stub if needed,
    // or we use the existing refMemberDefn from your provided MemberHelpers)
    let refAdditionalConstrExpr =
        RefParser<AdditionalConstrExpr<SyntaxToken>, _, _, _, _>()

[<RequireQualifiedAccess>]
module SimplePat =
    let parse: Parser<SimplePat<SyntaxToken>, _, _, _, _> =
        parser {
            let! ident = nextNonTriviaTokenIsL Token.Identifier "Expected identifier"
            let! typeAnnotation = opt (nextNonTriviaTokenIsL Token.OpColon ":")

            match typeAnnotation with
            | ValueSome colon ->
                let! t = Type.parse
                return SimplePat.Typed(SimplePat.Ident ident, colon, t)
            | ValueNone -> return SimplePat.Ident ident
        }

[<RequireQualifiedAccess>]
module PrimaryConstrArgs =
    let parse: Parser<PrimaryConstrArgs<SyntaxToken>, _, _, _, _> =
        parser {
            let! attrs = opt Attributes.parse
            let! access = opt pAccessModifier
            let! lParen = nextNonTriviaTokenIsL Token.KWLParen "("

            let! pats, _ = sepBy SimplePat.parse (nextNonTriviaTokenIsL Token.OpComma ",")

            let! rParen = nextNonTriviaTokenIsL Token.KWRParen ")"

            return PrimaryConstrArgs.PrimaryConstrArgs(attrs, access, lParen, List.ofSeq pats, rParen)
        }

[<RequireQualifiedAccess>]
module TypeName =
    let parse: Parser<TypeName<SyntaxToken>, _, _, _, _> =
        parser {
            let! attrs = opt Attributes.parse
            let! access = opt pAccessModifier
            let! ident = nextNonTriviaTokenIsL Token.Identifier "Type Name Identifier"
            let! typars = opt TyparDefns.parse

            return TypeName.TypeName(attrs, access, ident, typars)
        }

[<RequireQualifiedAccess>]
module AsDefn =
    let parse: Parser<AsDefn<SyntaxToken>, _, _, _, _> =
        parser {
            let! asTok = nextNonTriviaTokenIsL Token.KWAs "as"
            let! ident = nextNonTriviaTokenIsL Token.Identifier "self-identifier"
            return AsDefn.AsDefn(asTok, ident)
        }

// ----------------------------------------------------------------------------
// Signatures (MemberSig, ArgSpec)
// ----------------------------------------------------------------------------

[<RequireQualifiedAccess>]
module ArgNameSpec =
    let parse =
        parser {
            let! optional = opt (nextNonTriviaTokenIsL Token.OpDynamic "?")
            let! ident = nextNonTriviaTokenIsL Token.Identifier "Arg Name"
            let! colon = nextNonTriviaTokenIsL Token.OpColon ":"
            return ArgNameSpec.ArgNameSpec(optional, ident, colon)
        }

[<RequireQualifiedAccess>]
module ArgSpec =
    let parse =
        parser {
            let! attrs = opt Attributes.parse
            // Try parse "name :" first
            let! nameSpec = opt ArgNameSpec.parse
            let! typ = Type.parse
            return ArgSpec.ArgSpec(attrs, nameSpec, typ)
        }


[<RequireQualifiedAccess>]
module ArgsSpec =
    let parse =
        sepBy1 ArgSpec.parse (nextNonTriviaTokenIsL Token.OpMultiply "*")
        |>> fun struct (args, _) -> List.ofSeq args

[<RequireQualifiedAccess>]
module CurriedSig =

    let private pArgsSpec = ArgsSpec.parse .>>. pArrowRight

    let parse: Parser<CurriedSig<SyntaxToken>, _, _, _, _> =
        parser {
            let! (args, ret) = many1Till pArgsSpec Type.parse
            let args = List.ofSeq args
            return CurriedSig(args, ret)
        }

[<RequireQualifiedAccess>]
module UncurriedSig =

    let parse: Parser<UncurriedSig<SyntaxToken>, _, _, _, _> =
        parser {
            let! args = ArgsSpec.parse
            let! arrow = pArrowRight
            let! retType = Type.parse
            return UncurriedSig.UncurriedSig(args, arrow, retType)
        }

[<RequireQualifiedAccess>]
module MemberSig =

    (*
member-sig :=
    ident typar-defns~opt : curried-sig -- method or property signature
    ident typar-defns~opt : curried-sig with get -- property signature
    ident typar-defns~opt : curried-sig with set -- property signature
    ident typar-defns~opt : curried-sig with get,set -- property signature
    ident typar-defns~opt : curried-sig with set,get -- property signature
*)

    let private pGet =
        parser {
            let! getTok = nextNonTriviaTokenIsL Token.Identifier "get"
            let! state = getUserState

            if tokenStringIs "get" getTok state then
                return getTok
            else
                return! fail (Message "Expected 'get'")
        }

    let private pSet =
        parser {
            let! setTok = nextNonTriviaTokenIsL Token.Identifier "set"
            let! state = getUserState

            if tokenStringIs "set" setTok state then
                return setTok
            else
                return! fail (Message "Expected 'set'")
        }

    let private pGetSet =
        choiceL
            [
                parser {
                    let! getTok = pGet

                    let! maybeSet =
                        opt (
                            parser {
                                let! comma = nextNonTriviaTokenIsL Token.OpComma ","
                                let! setTok = pSet
                                return setTok
                            }
                        )

                    return getTok, maybeSet
                }
                parser {
                    let! setTok = pSet

                    let! maybeGet =
                        opt (
                            parser {
                                let! comma = nextNonTriviaTokenIsL Token.OpComma ","
                                let! getTok = pGet
                                return getTok
                            }
                        )

                    return setTok, maybeGet
                }
            ]
            ""


    let parse: Parser<MemberSig<SyntaxToken>, _, _, _, _> =
        parser {
            let! ident = nextNonTriviaTokenIsL Token.Identifier "Member Signature Identifier"
            let! typars = opt TyparDefns.parse
            let! colon = nextNonTriviaTokenIsL Token.OpColon ":"
            let! sigType = CurriedSig.parse

            // Check for optional 'with' get/set
            let! withClause =
                opt (
                    parser {
                        let! withTok = nextNonTriviaTokenIsL Token.KWWith "with"
                        let! getSet = pGetSet
                        return struct (withTok, getSet)
                    }
                )

            match withClause with
            | ValueSome(withTok, getSet) -> return MemberSig.PropSig(ident, typars, colon, sigType, withTok, getSet)
            | ValueNone -> return MemberSig.MethodOrPropSig(ident, typars, colon, sigType)
        }


// ----------------------------------------------------------------------------
// Member Definitions (Method, Property, Ctor)
// ----------------------------------------------------------------------------

[<RequireQualifiedAccess>]
module MethodOrPropDefn =

    // Distinguishes:
    // member x.P = ... (Property)
    // member x.M args = ... (Method)
    // member x.P with get ... (PropWithGetSet)

    let parse: Parser<MethodOrPropDefn<SyntaxToken>, _, _, _, _> =
        parser {
            // NOTE: The caller (MemberDefn) has consumed 'member', 'override', etc.
            // and likely the access modifier.
            // This parser focuses on the Identifier/Pattern and Body.

            // 1. Parse Identifier part (e.g. "x.Method" or "Method")
            // There might be a 'this' binding prefix: "x."
            // AST `identPrefix` captures the instance identifier.

            let! part1 = nextNonTriviaTokenIsL Token.Identifier "Member Identifier"
            let! dot = opt (nextNonTriviaTokenIsL Token.OpDot ".")

            let! identPrefix, ident =
                match dot with
                | ValueSome _ ->
                    parser {
                        let! part2 = nextNonTriviaTokenIsL Token.Identifier "Member Name"
                        return (ValueSome part1, part2)
                    }
                | ValueNone -> preturn (ValueNone, part1)

            // 2. Check for Arguments (Method) vs Immediate `=` or `with` (Property)

            // If next is arguments, it's a method. Arguments are Patterns.
            // If next is `=`, it's a Property (or Method with unit arg omitted? F# rules apply).
            // If next is `with`, it's PropertyWithGetSet.

            let! nextTok = pid

            match nextTok with
            | t when t.Token = Token.KWWith ->
                // Property with get/set
                let! withTok = nextNonTriviaTokenIsL Token.KWWith "with"
                // Parse get/set definitions (FunctionOrValueDefn list)
                // This usually requires a loop parsing `member val` or just `get() = ...`
                // Stubbing list for brevity:
                return MethodOrPropDefn.PropertyWithGetSet(identPrefix, ident, withTok, [])

            | t when t.Token = Token.OpEquality ->
                // Property (Get-only usually)
                // AST: Property of ident voption * ValueDefn
                // Note: ValueDefn parser usually starts with `let`/`mutable`.
                // Here we construct a ValueDefn-like structure from `ident = expr`.
                let! eq = pEquals
                let! expr = Expr.parse

                // Construct a synthetic ValueDefn for the AST
                let valDefn =
                    ValueDefn.ValueDefn(
                        ValueNone,
                        ValueNone,
                        Pat.NamedSimple(ident), // Simplified pattern
                        ValueNone,
                        ValueNone,
                        eq,
                        expr
                    )

                return MethodOrPropDefn.Property(identPrefix, valDefn)

            | _ ->
                // Method
                // We parse parameters until `=`
                // Reuse FunctionDefn, but we've already consumed the name.
                // We need to feed the name back or use a specialized parser.

                // Let's assume FunctionDefn.parse can handle the rest if we hadn't consumed ident.
                // Since we did, we reconstruct:

                let! args = many1 Pat.parse
                let! retType = opt ReturnType.parse
                let! eq = pEquals
                let! expr = Expr.parse

                let funcDefn =
                    FunctionDefn.FunctionDefn(
                        ValueNone,
                        ValueNone,
                        IdentOrOp.Ident ident,
                        ValueNone,
                        List.ofSeq args,
                        retType,
                        eq,
                        expr
                    )

                return MethodOrPropDefn.Method(identPrefix, funcDefn)
        }

[<RequireQualifiedAccess>]
module AdditionalConstrExpr =

    let private pInit =
        choiceL
            [
                parser {
                    let! lBrace = nextNonTriviaTokenIsL Token.KWLBrace "{"
                    // Helper for inherits: inherit Type(expr)
                    let! inherits =
                        parser {
                            let! inh = nextNonTriviaTokenIsL Token.KWInherit "inherit"
                            let! t = Type.parse
                            let! e = opt Expr.parse
                            return ClassInheritsDecl.ClassInheritsDecl(inh, t, e)
                        }

                    let! inits = many FieldInitializer.parse // Simplified loop
                    let! rBrace = nextNonTriviaTokenIsL Token.KWRBrace "}"
                    return AdditionalConstrInitExpr.Explicit(lBrace, inherits, List.ofSeq inits, rBrace)
                }
                parser {
                    let! newTok = nextNonTriviaTokenIsL Token.KWNew "new"
                    let! t = Type.parse
                    let! e = Expr.parse
                    return AdditionalConstrInitExpr.Delegated(newTok, t, e)
                }
            ]
            "Constructor Init"

    let parse =
        parser {
            // Simplified recursive parser for constructor body
            let! init = pInit
            return AdditionalConstrExpr.Init init
        }

    do refAdditionalConstrExpr.Set parse

[<RequireQualifiedAccess>]
module AdditionalConstrDefn =
    let parse: Parser<AdditionalConstrDefn<SyntaxToken>, _, _, _, _> =
        parser {
            let! attrs = opt Attributes.parse
            let! access = opt pAccessModifier
            let! newTok = nextNonTriviaTokenIsL Token.KWNew "new"
            let! pat = Pat.parse
            let! asDefn = opt AsDefn.parse
            let! equals = pEquals
            let! body = refAdditionalConstrExpr.Parser

            return AdditionalConstrDefn.AdditionalConstrDefn(attrs, access, newTok, pat, asDefn, equals, body)
        }

[<RequireQualifiedAccess>]
module MemberDefn =
    // Implementation of the forward reference stub
    let parse: Parser<MemberDefn<SyntaxToken>, _, _, _, _> =
        parser {
            let! attrs = opt Attributes.parse

            // Check for 'new' (Additional Constructor)
            let! isNew = opt (lookAhead (nextNonTriviaTokenIsL Token.KWNew "new"))

            match isNew with
            | ValueSome _ ->
                let! ctor = AdditionalConstrDefn.parse
                return MemberDefn.AdditionalConstructor ctor
            | ValueNone ->

                let! staticTok = opt (nextNonTriviaTokenIsL Token.KWStatic "static")
                let! access = opt pAccessModifier

                let! keyword =
                    choiceL
                        [
                            nextNonTriviaTokenIsL Token.KWMember "member"
                            nextNonTriviaTokenIsL Token.KWOverride "override"
                            nextNonTriviaTokenIsL Token.KWAbstract "abstract"
                            nextNonTriviaTokenIsL Token.KWDefault "default"
                            nextNonTriviaTokenIsL Token.KWVal "val"
                        ]
                        "Member Keyword"

                match keyword.Token with
                | Token.KWAbstract ->
                    let! memTok = opt (nextNonTriviaTokenIsL Token.KWMember "member")
                    let! sigDef = MemberSig.parse
                    return MemberDefn.Abstract(attrs, keyword, memTok, access, sigDef)

                | Token.KWVal ->
                    let! mut = opt (nextNonTriviaTokenIsL Token.KWMutable "mutable")
                    let! ident = nextNonTriviaTokenIsL Token.Identifier "val identifier"
                    let! colon = nextNonTriviaTokenIsL Token.OpColon ":"
                    let! t = Type.parse
                    return MemberDefn.Value(attrs, staticTok, keyword, mut, access, ident, colon, t)

                | Token.KWOverride ->
                    let! defn = MethodOrPropDefn.parse
                    return MemberDefn.Override(attrs, keyword, access, defn)

                | Token.KWDefault ->
                    let! defn = MethodOrPropDefn.parse
                    return MemberDefn.Default(attrs, keyword, access, defn)

                | _ -> // Token.KWMember
                    let! defn = MethodOrPropDefn.parse
                    return MemberDefn.Concrete(attrs, staticTok, keyword, access, defn)
        }

    // Set the forward reference in your existing framework
    do refMemberDefn.Set parse

// ----------------------------------------------------------------------------
// Type Body Elements
// ----------------------------------------------------------------------------

[<RequireQualifiedAccess>]
module TypeDefnElement =
    let parse: Parser<TypeDefnElement<SyntaxToken>, _, _, _, _> =
        choiceL
            [
                (parser {
                    let! intf = nextNonTriviaTokenIsL Token.KWInterface "interface"
                    let! t = Type.parse
                    // Distinguish between InterfaceSpec (in abstract class) and InterfaceImpl (with members)
                    let! withTok = opt (nextNonTriviaTokenIsL Token.KWWith "with")

                    match withTok with
                    | ValueSome _ ->
                        // InterfaceImpl requires members, usually `member ...`
                        // Reusing ObjectMembers parser logic roughly
                        // For precise AST `InterfaceImpl` expects `InterfaceImpl` type which has `opt ObjectMembers`
                        // Here we map roughly:
                        let! members = opt ObjectMembers.parse
                        return TypeDefnElement.InterfaceImpl(InterfaceImpl.InterfaceImpl(intf, t, members))
                    | ValueNone -> return TypeDefnElement.InterfaceSpec(InterfaceSpec.InterfaceSpec(intf, t))
                })
                (MemberDefn.parse |>> TypeDefnElement.Member)
            ]
            "Type Definition Element"

[<RequireQualifiedAccess>]
module TypeDefnElements =
    // Parses a list of elements until 'end' or other terminator
    let parseTill terminator =
        manyTill TypeDefnElement.parse terminator

// ----------------------------------------------------------------------------
// Specific Type Bodies (Class, Union, Record, etc.)
// ----------------------------------------------------------------------------

[<RequireQualifiedAccess>]
module ClassInheritsDecl =
    let parse: Parser<ClassInheritsDecl<SyntaxToken>, _, _, _, _> =
        parser {
            let! inh = nextNonTriviaTokenIsL Token.KWInherit "inherit"
            let! t = Type.parse
            // Optional constructor args
            let! e = opt Expr.parse
            return ClassInheritsDecl.ClassInheritsDecl(inh, t, e)
        }

[<RequireQualifiedAccess>]
module ClassFunctionOrValueDefn =
    let parse: Parser<ClassFunctionOrValueDefn<SyntaxToken>, _, _, _, _> =
        choiceL
            [
                parser {
                    let! attrs = opt Attributes.parse
                    let! stat = opt (nextNonTriviaTokenIsL Token.KWStatic "static")
                    let! d = nextNonTriviaTokenIsL Token.KWDo "do"
                    let! e = Expr.parse
                    return ClassFunctionOrValueDefn.Do(attrs, stat, d, e)
                }
                parser {
                    let! attrs = opt Attributes.parse
                    let! stat = opt (nextNonTriviaTokenIsL Token.KWStatic "static")
                    let! l = nextNonTriviaTokenIsL Token.KWLet "let"
                    let! r = opt (nextNonTriviaTokenIsL Token.KWRec "rec")
                    // Parsing multiple let bindings is complex in top level,
                    // assuming one for now or loop needed.
                    // FunctionOrValueDefn.parse handles one.
                    let! defn = FunctionOrValueDefn.parse
                    return ClassFunctionOrValueDefn.LetRecDefns(attrs, stat, l, r, [ defn ])
                }
            ]
            "Class Let/Do"

[<RequireQualifiedAccess>]
module ClassTypeBody =
    let parse terminator : Parser<ClassTypeBody<SyntaxToken>, _, _, _, _> =
        parser {
            // Implicit class body:
            // inherit?
            // let/do bindings*
            // member/interface elements*

            let! inh = opt ClassInheritsDecl.parse

            // Allow interleaving of let/do and members in implicit constructors?
            // Strictly F# puts let/do before members usually, but implicit classes allow mix slightly.
            // Simplified: Parse Let/Dos, then Elements.

            let! lets = many ClassFunctionOrValueDefn.parse

            let! elems =
                opt (TypeDefnElements.parseTill terminator)
                |>> function
                    | ValueSome(es, _) -> ValueSome(List.ofSeq es)
                    | ValueNone -> ValueNone

            return ClassTypeBody.ClassTypeBody(inh, List.ofSeq lets, elems)
        }

[<RequireQualifiedAccess>]
module StructTypeBody =
    let parse terminator =
        parser {
            let! elems, _ = TypeDefnElements.parseTill terminator
            return StructTypeBody.StructTypeBody(List.ofSeq elems)
        }

[<RequireQualifiedAccess>]
module InterfaceTypeBody =
    let parse terminator =
        parser {
            let! elems, _ = TypeDefnElements.parseTill terminator
            return InterfaceTypeBody.InterfaceTypeBody(List.ofSeq elems)
        }

// --- Union ---

[<RequireQualifiedAccess>]
module UnionTypeField =
    let parse: Parser<UnionTypeField<SyntaxToken>, _, _, _, _> =
        parser {
            // Try Named first: ident : Type
            let! namedField =
                opt (
                    parser {
                        let! ident = nextNonTriviaTokenIsL Token.Identifier "Field Name"
                        let! colon = nextNonTriviaTokenIsL Token.OpColon ":"
                        let! t = Type.parse
                        return UnionTypeField.Named(ident, colon, t)
                    }
                )

            match namedField with
            | ValueSome nf -> return nf
            | ValueNone ->
                // Unnamed field: just Type
                let! t = Type.parse
                return UnionTypeField.Unnamed t
        }

[<RequireQualifiedAccess>]
module UnionTypeCaseData =
    let parseFields: Parser<UnionTypeField<SyntaxToken> list, _, _, _, _> =
        sepBy1 UnionTypeField.parse (nextNonTriviaTokenIsL Token.OpMultiply "*")
        |>> fun struct (fields, _) -> List.ofSeq fields

    let parseNary: Parser<UnionTypeCaseData<SyntaxToken>, _, _, _, _> =
        parser {
            let! ident = nextNonTriviaTokenIsL Token.Identifier "Union Case Name"
            let! ofTok = nextNonTriviaTokenIsL Token.KWOf "of"

            // Check for uncurried signature (colon Type) or field list
            let! next = pid

            match next with
            | t when t.Token = Token.OpColon ->
                // UncurriedSig
                let! colon = nextNonTriviaTokenIsL Token.OpColon ":"
                let! sign = UncurriedSig.parse
                return UnionTypeCaseData.NaryUncurried(ident, colon, sign)
            | _ ->
                // Field list
                let! fields = parseFields
                return UnionTypeCaseData.Nary(ident, ofTok, fields)
        }

    let parse: Parser<UnionTypeCaseData<SyntaxToken>, _, _, _, _> =
        parser {
            // Try Nary first
            let! nary = opt (lookAhead (nextNonTriviaTokenIsL Token.KWOf "of"))

            match nary with
            | ValueSome _ -> return! parseNary
            | ValueNone ->
                // Nullary
                let! ident = nextNonTriviaTokenIsL Token.Identifier "Union Case Name"
                return UnionTypeCaseData.Nullary ident
        }

[<RequireQualifiedAccess>]
module UnionTypeCase =
    let parse: Parser<UnionTypeCase<SyntaxToken>, _, _, _, _> =
        parser {
            let! attrs = opt Attributes.parse
            let! caseData = UnionTypeCaseData.parse
            return UnionTypeCase.UnionTypeCase(attrs, caseData)
        }

[<RequireQualifiedAccess>]
module UnionTypeCases =
    let parse =
        parser {
            let! firstBar = opt (nextNonTriviaTokenIsL Token.OpBar "|")
            let! cases, _ = sepBy1 UnionTypeCase.parse (nextNonTriviaTokenIsL Token.OpBar "|")
            return List.ofSeq cases
        }

// --- Record ---

[<RequireQualifiedAccess>]
module RecordField =
    let parse: Parser<RecordField<SyntaxToken>, _, _, _, _> =
        parser {
            let! attrs = opt Attributes.parse
            let! mut = opt (nextNonTriviaTokenIsL Token.KWMutable "mutable")
            let! acc = opt pAccessModifier
            let! id = nextNonTriviaTokenIsL Token.Identifier "Field Name"
            let! col = nextNonTriviaTokenIsL Token.OpColon ":"
            let! t = Type.parse
            return RecordField.RecordField(attrs, mut, acc, id, col, t)
        }

// --- Enum ---

[<RequireQualifiedAccess>]
module EnumTypeCase =
    let parse: Parser<EnumTypeCase<SyntaxToken>, _, _, _, _> =
        parser {
            let! id = nextNonTriviaTokenIsL Token.Identifier "Enum Name"
            let! eq = pEquals
            let! c = nextNonTriviaTokenSatisfiesL (fun t -> t.Token.IsNumeric) "Enum Constant"
            return EnumTypeCase.EnumTypeCase(id, eq, c)
        }

[<RequireQualifiedAccess>]
module EnumTypeCases =
    let parse =
        parser {
            let! firstBar = opt (nextNonTriviaTokenIsL Token.OpBar "|")
            let! cases, _ = sepBy1 EnumTypeCase.parse (nextNonTriviaTokenIsL Token.OpBar "|")
            return List.ofSeq cases
        }

// --- Type Extensions ---

[<RequireQualifiedAccess>]
module TypeExtensionElements =
    let parse: Parser<TypeExtensionElements<SyntaxToken>, _, _, _, _> =
        parser {
            let! withTok = nextNonTriviaTokenIsL Token.KWWith "with"
            let endTokParser = nextNonTriviaTokenIsL Token.KWEnd "end"
            let! elems, _ = TypeDefnElements.parseTill endTokParser
            let! endTok = endTokParser
            return TypeExtensionElements.TypeExtensionElements(withTok, List.ofSeq elems, endTok)
        }

[<RequireQualifiedAccess>]
module DelegateSig =
    let parse: Parser<DelegateSig<SyntaxToken>, _, _, _, _> =
        parser {
            let! del = nextNonTriviaTokenIsL Token.KWDelegate "delegate"
            let! ofTok = nextNonTriviaTokenIsL Token.KWOf "of"
            let! t = Type.parse // Simplified mapping to UncurriedSig
            // Construct fake UncurriedSig for AST compliance
            let sigData = UncurriedSig.UncurriedSig([], Unchecked.defaultof<_>, t)
            return DelegateSig.DelegateSig(del, ofTok, sigData)
        }


// ----------------------------------------------------------------------------
// Top Level TypeDefn
// ----------------------------------------------------------------------------

[<RequireQualifiedAccess>]
module TypeDefn =

    // Helper to detect specific type bodies based on lookahead or specific tokens

    let parse: Parser<TypeDefn<SyntaxToken>, _, _, _, _> =
        parser {
            let! typeKw = nextNonTriviaTokenIsL Token.KWType "type" // Consumed but typically part of TypeName?
            // Note: AST TypeName doesn't include 'type' keyword, but TypeDefn implies it's wrapped or parsed before.
            // Assuming we are at the name:

            let! typeName = TypeName.parse

            // 1. Check for Primary Constructor (Class/Struct)
            let! primaryConstr = opt PrimaryConstrArgs.parse

            // 2. Check for '='
            let! equals = pEquals

            // 3. Branch based on what follows

            let! next = pid

            match next with
            | t when t.Token = Token.KWStruct ->
                // Explicit Struct
                let! str = nextNonTriviaTokenIsL Token.KWStruct "struct"
                let endTokParser = nextNonTriviaTokenIsL Token.KWEnd "end"
                let! body = StructTypeBody.parse endTokParser
                let! endTok = endTokParser
                return TypeDefn.Struct(typeName, primaryConstr, ValueNone, equals, str, body, endTok)

            | t when t.Token = Token.KWInterface ->
                // Interface
                let! intf = nextNonTriviaTokenIsL Token.KWInterface "interface"
                let endTokParser = nextNonTriviaTokenIsL Token.KWEnd "end"
                let! body = InterfaceTypeBody.parse endTokParser
                let! endTok = endTokParser
                return TypeDefn.Interface(typeName, equals, intf, body, endTok)

            | t when t.Token = Token.KWClass ->
                // Explicit Class
                let! cls = nextNonTriviaTokenIsL Token.KWClass "class"
                let endTokParser = nextNonTriviaTokenIsL Token.KWEnd "end"
                let! body = ClassTypeBody.parse endTokParser
                let! endTok = endTokParser
                return TypeDefn.Class(typeName, primaryConstr, ValueNone, equals, cls, body, endTok)

            | t when t.Token = Token.KWDelegate ->
                let! delSig = DelegateSig.parse
                return TypeDefn.Delegate(typeName, equals, delSig)

            | t when t.Token = Token.KWLBrace ->
                // Record
                let! lBrace = nextNonTriviaTokenIsL Token.KWLBrace "{"
                let! fields, _ = sepBy1 RecordField.parse (nextNonTriviaTokenIsL Token.OpSemicolon ";")
                let! rBrace = nextNonTriviaTokenIsL Token.KWRBrace "}"
                let! ext = opt TypeExtensionElements.parse
                return TypeDefn.Record(typeName, equals, lBrace, List.ofSeq fields, rBrace, ext)

            | t when t.Token = Token.OpBar ->
                // Union or Enum
                // Look ahead deeper to distinguish?
                // Heuristic: Parse first case. If it has '=', it's Enum.
                // Reusing UnionTypeCases but catching Enum pattern is complex without backtracking.
                // Assuming Union for now as it's more common with '|' start.
                let! cases = UnionTypeCases.parse
                let! ext = opt TypeExtensionElements.parse
                return TypeDefn.Union(typeName, equals, cases, ext)

            | _ ->
                // Abbreviation or Implicit Class?
                // If it starts with Type, it's Abbrev.
                // If it starts with 'member', 'val', 'new', 'inherit' -> Implicit Class.

                // We attempt to parse Type. If successful and consumed everything? Abbrev.
                // But 'new' is not a type start. 'member' is not.

                // Let's lookahead at tokens that start a Class Body
                let! isImplicitClass =
                    lookAhead (
                        choiceL
                            [
                                nextNonTriviaTokenIsL Token.KWMember "member"
                                nextNonTriviaTokenIsL Token.KWVal "val"
                                nextNonTriviaTokenIsL Token.KWNew "new"
                                nextNonTriviaTokenIsL Token.KWInherit "inherit"
                                nextNonTriviaTokenIsL Token.KWAbstract "abstract"
                                nextNonTriviaTokenIsL Token.KWDefault "default"
                                nextNonTriviaTokenIsL Token.KWOverride "override"
                                // If primary constructor was present, it's definitely a class/struct
                                (if primaryConstr.IsSome then
                                     preturn (Unchecked.defaultof<_>)
                                 else
                                     fail (Message "Not implicit"))
                            ]
                            "Implicit check"
                    )
                    |> opt

                match isImplicitClass with
                | ValueSome _ ->
                    // Implicit Class
                    // AST `Anon` is often used for Implicit Class definitions (begin/end inferred or explicit)
                    // Or `Class` with implicit tokens.
                    // The AST `Anon` expects `begin`/`end`. F# implicit classes don't always have them.
                    // We'll synthesize tokens or expect `begin`/`end` if the grammar strictly requires AST matching.
                    // Assuming AST requires `begin` `end`:
                    let! beginTok = nextNonTriviaTokenVirtualIfNot Token.KWBegin
                    let endTokParser = nextNonTriviaTokenVirtualIfNot Token.KWEnd
                    let! body = ClassTypeBody.parse endTokParser
                    let! endTok = endTokParser // consume the virtual/real end
                    return TypeDefn.Anon(typeName, primaryConstr, ValueNone, equals, beginTok, body, endTok)
                | ValueNone ->
                    // Abbreviation
                    let! t = Type.parse
                    return TypeDefn.Abbrev(typeName, equals, t)
        }

// ----------------------------------------------------------------------------
// Units of Measure Parsers
// ----------------------------------------------------------------------------

module Measure =
    [<RequireQualifiedAccess>]
    type MeasureAux = | PowerOperand of SyntaxToken

    type MeasureOperatorParser() =
        static let productPrecedence = BindingPower.fromLevel 1 // * and /
        static let juxtapositionPrecedence = BindingPower.fromLevel 2 // Implicit (whitespace)
        static let powerPrecedence = BindingPower.fromLevel 3 // ^
        static let reciprocalPrecedence = BindingPower.fromLevel 4 // / (Reciprocal)
        static let parenPrecedence = BindingPower.fromLevel 10 // ( ... )

        // --- Completion Functions ---

        static let completeInfix (l: Measure<SyntaxToken>) (op: SyntaxToken) (r: Measure<SyntaxToken>) =
            match op.Token with
            | Token.OpMultiply -> Measure.Product(l, op, r)
            | Token.OpDivision -> Measure.Quotient(l, op, r)
            | _ -> failwithf "Unexpected infix measure operator: %A" op

        static let completePrefix (op: SyntaxToken) (e: Measure<SyntaxToken>) =
            match op.Token with
            | Token.OpDivision -> Measure.Reciprocal(op, e)
            | _ -> failwithf "Unexpected prefix measure operator: %A" op

        static let completePower (l: Measure<SyntaxToken>) (op: SyntaxToken) (aux: MeasureAux) =
            match aux with
            | MeasureAux.PowerOperand exponentToken -> Measure.Power(l, op, exponentToken)

        static let completeJuxtaposition (elements: ResizeArray<Measure<SyntaxToken>>) (ops: ResizeArray<SyntaxToken>) =
            Measure.Juxtaposition(List.ofSeq elements, List.ofSeq ops)

        static let completeParen (l: SyntaxToken) (m: Measure<SyntaxToken>) (r: SyntaxToken) = Measure.Paren(l, m, r)

        // --- Aux Parsers ---

        static let pPowerRhs =
            parser {
                let! intToken = nextNonTriviaTokenSatisfiesL (fun t -> t.Token.IsNumeric) "Expected integer exponent"
                return MeasureAux.PowerOperand intToken
            }

        static let pJuxtapositionOp =
            parser {
                let! pos = getPosition
                let! token = satisfyL (fun (t: PositionedToken) -> t.Token = Token.Whitespace) "Whitespace"

                // Lookahead to confirm we are adjacent to a measure atom
                let! _ =
                    lookAhead (
                        nextNonTriviaTokenSatisfiesL
                            (fun t ->
                                t.Token.IsIdentifier
                                || t.Token = Token.OpLessThan
                                || t.Token = Token.KWSingleQuote
                                || t.Token = Token.Wildcard
                                || t.Token = Token.KWLParen
                                || t.Token.IsNumeric
                            )
                            "Measure Atom or Literal"
                    )

                return syntaxToken token pos.Index
            }

        // --- Parsers ---

        static let lhsParser =
            nextNonTriviaToken
            >>= fun token ->
                match token.Token with
                | Token.OpDivision ->
                    let p = preturn token
                    // Reciprocal: / s
                    let op = Prefix(token, p, reciprocalPrecedence, completePrefix)
                    preturn op
                | Token.KWLParen ->
                    // Parenthesized measure
                    let p = preturn token
                    let rParen = virtualToken (PositionedToken.Create(Token.KWRParen, 0))
                    let op = Enclosed(token, p, parenPrecedence, rParen, pRParen, completeParen)

                    preturn op
                | _ -> fail (Message "Not a prefix measure operator")

        static let rhsParser =
            // Try whitespace (juxtaposition) first, then standard tokens
            (pJuxtapositionOp <|> nextNonTriviaToken)
            >>= fun token ->
                match token.Token with
                | Token.OpMultiply
                | Token.OpDivision ->
                    // Product: * or /
                    let op = InfixLeft(token, preturn token, productPrecedence, completeInfix)
                    preturn op

                | Token.Whitespace ->
                    // Juxtaposition: implicit multiplication
                    let op =
                        InfixNary(token, preturn token, juxtapositionPrecedence, completeJuxtaposition)

                    preturn op

                | Token.OpConcatenate ->
                    // Power: ^
                    // InfixMapped handles the parsing of the integer operand (Aux)
                    let op =
                        InfixMapped(token, preturn token, powerPrecedence, pPowerRhs, completePower)

                    preturn op

                | _ -> fail (Message "Not a valid RHS measure operator")

        interface Operators<
            SyntaxToken,
            MeasureAux,
            Measure<SyntaxToken>,
            PositionedToken,
            ParseState,
            ReadableImmutableArray<PositionedToken>,
            ReadableImmutableArraySlice<PositionedToken>
         > with
            member _.LhsParser = lhsParser
            member _.RhsParser = rhsParser
            member _.OpComparer = opComparer

    let pOneLiteral =
        parser {
            let! state = getUserState

            let! t =
                nextNonTriviaTokenSatisfiesL
                    (fun t -> t.Token = Token.NumInt32 && tokenStringIs "1" t state)
                    "Expected '1'"

            return Measure.One t
        }

    let pAnonymous = nextNonTriviaTokenIsL Token.Wildcard "_" |>> Measure.Anonymous
    let pTypar = Typar.parse |>> Measure.Typar
    let pNamed = LongIdent.parse |>> Measure.Named

    let atomMeasureParser: Parser<Measure<SyntaxToken>, _, _, _, _> =
        choiceL [ pOneLiteral; pAnonymous; pTypar; pNamed ] "Measure Atom"

    let measureOperatorParser = MeasureOperatorParser()

    let parse: Parser<Measure<SyntaxToken>, _, _, _, _> =
        Operator.parser atomMeasureParser measureOperatorParser

[<RequireQualifiedAccess>]
module ExceptionDefn =
    let parse: Parser<ExceptionDefn<SyntaxToken>, _, _, _, _> =
        parser {
            let! attrs = opt Attributes.parse
            let! exTok = nextNonTriviaTokenIsL Token.KWException "exception"

            // Try Full first
            let! isFull = opt (lookAhead (nextNonTriviaTokenIsL Token.KWOf "of"))

            match isFull with
            | ValueSome _ ->
                let! caseData = UnionTypeCaseData.parse
                return ExceptionDefn.Full(attrs, exTok, caseData)
            | ValueNone ->
                let! ident = nextNonTriviaTokenIsL Token.Identifier "Exception Name"
                let! eq = nextNonTriviaTokenIsL Token.OpEquality "="
                let! lid = LongIdent.parse
                return ExceptionDefn.Abbreviation(attrs, exTok, ident, eq, lid)
        }


[<RequireQualifiedAccess>]
module Access =
    let parse
        : Reader<PositionedToken, ParseState, ReadableImmutableArray<PositionedToken>, _>
              -> ParseResult<Access<SyntaxToken>, PositionedToken, ParseState> =
        nextNonTriviaTokenSatisfiesL
            (fun t ->
                t.Token = Token.KWPrivate
                || t.Token = Token.KWInternal
                || t.Token = Token.KWPublic
            )
            "Access modifier"
        |>> function
            | t when t.Token = Token.KWPrivate -> Access.Private t
            | t when t.Token = Token.KWInternal -> Access.Internal t
            | t when t.Token = Token.KWPublic -> Access.Public t
            | _ -> failwith "Unreachable"

[<RequireQualifiedAccess>]
module ImportDecl =
    let parse =
        parser {
            let! openTok = nextNonTriviaTokenIsL Token.KWOpen "open"
            let! ident = LongIdent.parse
            return ImportDecl.ImportDecl(openTok, ident)
        }

[<RequireQualifiedAccess>]
module ModuleAbbrev =
    let parse =
        parser {
            let! modTok = nextNonTriviaTokenIsL Token.KWModule "module"
            let! ident = nextNonTriviaTokenSatisfiesL (fun t -> t.Token.IsIdentifier) "Expected identifier"
            let! eq = nextNonTriviaTokenIsL Token.OpEquality "="
            let! lid = LongIdent.parse
            return ModuleAbbrev.ModuleAbbrev(modTok, ident, eq, lid)
        }

[<RequireQualifiedAccess>]
module CompilerDirectiveDecl =
    let parse: Parser<CompilerDirectiveDecl<SyntaxToken>, _, _, ReadableImmutableArray<_>, _> =
        parser {
            let! hash = nextNonTriviaTokenIsL Token.KWHash "#"
            let! ident = nextNonTriviaTokenSatisfiesL (fun t -> t.Token.IsIdentifier) "Directive identifier"
            let! strings = many (nextNonTriviaTokenSatisfiesL (fun t -> t.Token.IsText) "String argument")
            return CompilerDirectiveDecl(hash, ident, List.ofSeq strings)
        }

[<RequireQualifiedAccess>]
module ModuleFunctionOrValueDefn =

    // Helper to distinguish LetValue vs LetFunction based on lookahead or backtracking
    // Assuming FunctionDefn/ValueDefn handle the body after 'let'
    let private pLetBinding attrs letTok =
        parser {
            let! isRec = opt (nextNonTriviaTokenIsL Token.KWRec "rec")

            match isRec with
            | ValueSome recTok ->
                // let rec ...
                // Parses a list of 'and' connected definitions
                let! defns, _ = sepBy1 FunctionOrValueDefn.parse (nextNonTriviaTokenIsL Token.KWAnd "and")
                return ModuleFunctionOrValueDefn.LetRec(attrs, letTok, ValueSome recTok, List.ofSeq defns)

            | ValueNone ->
                // let ...
                // Try parsing as a function (params present), if that fails/backtracks, try value.
                // Note: In a real parser, we might peek for parameters to decide.
                return!
                    choiceL
                        [
                            parser {
                                let! fn = FunctionDefn.parse
                                return ModuleFunctionOrValueDefn.LetFunction(attrs, letTok, fn)
                            }
                            parser {
                                let! valDef = ValueDefn.parse
                                return ModuleFunctionOrValueDefn.LetValue(attrs, letTok, valDef)
                            }
                        ]
                        ""
        }

    let parse =
        parser {
            let! attrs = opt Attributes.parse

            let! token = nextNonTriviaToken

            match token.Token with
            | Token.KWDo ->
                let! expr = Expr.parse
                return ModuleFunctionOrValueDefn.Do(attrs, token, expr)

            | Token.KWLet -> return! pLetBinding attrs token

            | _ -> return! fail (Message "Expected 'let' or 'do'")
        }

[<RequireQualifiedAccess>]
module ModuleDefn =

    let parseBody (elementParser: Parser<ModuleElems<SyntaxToken>, _, _, _, _>) =
        parser {
            let! beginTok = nextNonTriviaTokenIsL Token.KWBegin "begin"
            let! elems = opt elementParser
            let! endTok = nextNonTriviaTokenIsL Token.KWEnd "end"
            return ModuleDefnBody.ModuleDefnBody(beginTok, elems, endTok)
        }

    let parse (elementParser: Parser<ModuleElems<SyntaxToken>, _, _, _, _>) =
        parser {
            let! attrs = opt Attributes.parse
            let! modTok = nextNonTriviaTokenIsL Token.KWModule "module"
            let! access = opt Access.parse
            let! ident = nextNonTriviaTokenSatisfiesL (fun t -> t.Token.IsIdentifier) "Module identifier"
            let! eq = nextNonTriviaTokenIsL Token.OpEquality "="
            let! body = parseBody elementParser
            return ModuleDefn(attrs, modTok, access, ident, eq, body)
        }

[<RequireQualifiedAccess>]
module ModuleElem =

    // Forward reference setup to handle: ModuleElem -> ModuleDefn -> ModuleElem
    let rec private pModuleElems = many parse |>> List.ofSeq

    and parse =
        // 1. Try parsers that start with unique keywords and usually no attributes
        //    (Import, Directive)
        dispatch (fun (lookahead: PositionedToken voption) ->
            match lookahead with
            | ValueNone -> fail EndOfInput
            | ValueSome lookahead ->
                match lookahead.Token with
                | Token.KWOpen ->
                    parser {
                        let! x = ImportDecl.parse
                        return ModuleElem.Import x
                    }
                | Token.KWHash ->
                    parser {
                        let! x = CompilerDirectiveDecl.parse
                        return ModuleElem.CompilerDirective x
                    }
                | _ ->
                    // 2. Parsers that might be preceeded by Attributes
                    //    (Module, Type, Exception, Let, Do)

                    // We don't consume attributes here because the specific parsers
                    // (like ModuleFunctionOrValueDefn and TypeDefn) store them in their nodes.
                    // We look ahead past attributes to decide which parser to invoke.

                    // Note: We need a utility to skip attributes and see what's next
                    // to distinguish 'module' vs 'type' vs 'let' etc.

                    // For simplicity in this combinator style, we use choice with backtracking.
                    choice
                        [
                            // Type Definitions
                            TypeDefn.parse |>> ModuleElem.Type

                            // Exception Definitions
                            ExceptionDefn.parse |>> ModuleElem.Exception

                            // Module Definition (Recursive)
                            // Logic: starts with [attributes] module ident = begin ...
                            (ModuleDefn.parse pModuleElems) |>> ModuleElem.Module

                            // Module Abbreviation
                            // Logic: starts with module ident = LongIdent
                            // Note: If ModuleDefn fails (e.g. no 'begin'), this picks up.
                            ModuleAbbrev.parse |>> ModuleElem.ModuleAbbrev

                            // Let / Do bindings
                            ModuleFunctionOrValueDefn.parse |>> ModuleElem.FunctionOrValue
                        ]
        )

[<RequireQualifiedAccess>]
module NamespaceDeclGroup =
    let parse =
        parser {
            let! nsTok = nextNonTriviaTokenIsL Token.KWNamespace "namespace"

            // Check for 'global' keyword
            let! globalTok = opt (nextNonTriviaTokenIsL Token.KWGlobal "global")

            match globalTok with
            | ValueSome gTok ->
                // namespace global
                let! elems = many ModuleElem.parse
                return NamespaceDeclGroup.Global(nsTok, gTok, List.ofSeq elems)

            | ValueNone ->
                // namespace LongIdent
                let! ident = LongIdent.parse
                let! elems = many ModuleElem.parse
                return NamespaceDeclGroup.Named(nsTok, ident, List.ofSeq elems)
        }

[<RequireQualifiedAccess>]
module Reader =
    let ofLexed (lexed: Lexed) (input: string) (definedSymbols: Set<string>) : Reader<_, ParseState, _, _> =
        let initialState = ParseState.create lexed input definedSymbols
        Reader.ofImmutableArray (lexed.Tokens.AsImmutableArray()) initialState
