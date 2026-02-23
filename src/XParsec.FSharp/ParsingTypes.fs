namespace XParsec.FSharp.Parser

open System
open System.Collections.Generic
open System.Collections.Immutable
open XParsec
open XParsec.FSharp.Lexer


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
    /// A sequence of items that must be column-aligned (the primary offside context).
    /// Pushed after `=` in Let/Member, after Then/Else/Try/Finally/Do contexts, after `->` in MatchClauses, etc.
    | SeqBlock

type Offside =
    {
        Context: OffsideContext
        Indent: int
        Token: PositionedToken
    }

[<RequireQualifiedAccess>]
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
        ConditionalCompilationStack: PositionedToken list
    }

module SyntaxToken =

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

    let opComparer =
        // For parsing, we only care about the operator token itself for equality
        { new IEqualityComparer<SyntaxToken> with
            member _.Equals(x, y) = x.Token = y.Token
            member _.GetHashCode(obj) = hash obj.Token
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
            ConditionalCompilationStack = []
        }

    let setIndentOn (state: ParseState) =
        { state with
            IndentationMode = Syntax.Light
        }

    let setIndentOff (state: ParseState) =
        { state with
            IndentationMode = Syntax.Verbose
        }

    let pushOffside offsideCtx (state: ParseState) =
        { state with
            Context = offsideCtx :: state.Context
        }

    let popOffside (state: ParseState) =
        match state.Context with
        | [] -> invalidOp "Attempted to pop empty context"
        | head :: tail -> { state with Context = tail }

    let addDiagnostic code startToken endToken (state: ParseState) =
        let diag =
            {
                Code = code
                Token = startToken
                TokenEnd = endToken
            }

        { state with
            Diagnostics = diag :: state.Diagnostics
        }

    let private findLineNumberImpl (lexed: Lexed) (guess: int<line>) (index: int<token>) =
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
                // The target is after the guess
                else if lineStarts.[guess] < index then
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

    let findLineNumber (state: ParseState) (index: int<token>) =
        let lineNo = findLineNumberImpl state.Lexed state.LastLine index
        state.LastLine <- lineNo
        lineNo

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
                    state.LastLine <- findLineNumberImpl state.Lexed nextLine index
                    getIndent state index
            else
                // Last line
                let token = state.Lexed.Tokens[index]
                token.StartIndex - state.Lexed.Tokens[currentLineTokenIndex].StartIndex
        else
            state.LastLine <- findLineNumberImpl state.Lexed (state.LastLine - 1<_>) index
            getIndent state index

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

    let isDefined (state: ParseState) (symbolToken: SyntaxToken) =
        let symbol = tokenString symbolToken state
        state.DefinedSymbols.Contains(symbol)

[<RequireQualifiedAccess>]
module Reader =
    let ofLexed (lexed: Lexed) (input: string) (definedSymbols: Set<string>) : Reader<_, ParseState, _, _> =
        let initialState = ParseState.create lexed input definedSymbols
        Reader.ofImmutableArray (lexed.Tokens.AsImmutableArray()) initialState
