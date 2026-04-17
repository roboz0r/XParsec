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
    /// Between the `for` and `do` or `->` in a `for` expression. The pattern and iterable must be indented
    /// past the `for` keyword, but the body can be at the same indent as `for` or indented further.
    /// The body is represented by a `Do` context, which is pushed after the `do` or `->`.
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
    | BraceBar
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
type DiagnosticSeverity =
    | Error
    | Warning
    | Info
    | Hint

[<RequireQualifiedAccess>]
type DiagnosticCode =
    // TODO: Use F# error codes
    | Other of string
    | TyparInConstant of Typar<SyntaxToken>
    // Recovery-specific:
    | MissingExpression
    | MissingPattern
    | MissingType
    | MissingRule
    | MissingTypeDefn
    | MissingModuleElem
    | UnexpectedTopLevel
    | ExpectedEnd
    | ExpectedRParen
    | ExpectedRBracket
    | ExpectedRArrayBracket
    | ExpectedRBraceBar
    | ExpectedQuotationTypedRight
    | ExpectedQuotationUntypedRight
    | UnclosedDelimiter of opened: SyntaxToken * expected: Token

and Diagnostic =
    {
        Code: DiagnosticCode
        Severity: DiagnosticSeverity
        Token: PositionedToken
        TokenEnd: PositionedToken option
        Error: ParseError<PositionedToken, ParseState> option
    }

and [<RequireQualifiedAccess>] Syntax =
    | Light
    | Verbose

and [<RequireQualifiedAccess>] TraceEvent =
    | ContextPush of context: OffsideContext * indent: int * token: PositionedToken * stackDepth: int
    | ContextPop of context: OffsideContext * stackDepth: int
    | TokenConsumed of token: PositionedToken * index: int * col: int
    | TokenPeeked of token: PositionedToken * index: int * col: int
    | VirtualToken of token: Token * atStartIndex: int
    | OffsideOk of token: PositionedToken * tokenCol: int * contextIndent: int * context: OffsideContext
    | OffsideFail of token: PositionedToken * tokenCol: int * contextIndent: int * context: OffsideContext
    | PermittedUndentation of token: PositionedToken * tokenCol: int * contextIndent: int * rule: string
    | DiagnosticEmitted of code: DiagnosticCode * severity: DiagnosticSeverity * token: PositionedToken
    | SplitRAttrBracketSet of atStartIndex: int
    | SplitRAttrBracketConsumed of atStartIndex: int
    | SplitPowerMinusSet of atStartIndex: int
    | SplitPowerMinusConsumed of atStartIndex: int
    | Message of message: string

/// Holds the trace callback. A reference type so it doesn't affect ParseState equality
/// and is shared across immutable record copies.
and TraceCallback(callback: TraceEvent -> unit) =
    static let empty = TraceCallback(ignore)
    static member Empty = empty
    member _.Invoke(event) = callback event

and [<Struct>] WarnDirective =
    {
        Line: int<line>
        WarningNumber: int
        Suppress: bool
    }

and ParseState =
    // TODO: Refactor to separate the unchanged input/state from the mutable aspects like diagnostics and LastLine and frequently updated Context, to minimize the amount of data being copied on each state update.
    {
        Input: string
        Lexed: Lexed
        Context: Offside list
        Diagnostics: Diagnostic list
        DefinedSymbols: Set<string>
        IndentationMode: Syntax
        mutable LastLine: int<line> // ok to be mutable since it's only used as a guess
        // ReprocessOpAfterTypeDeclaration: bool
        /// Number of characters consumed since the last type parameter, used to
        /// allow procesing of `>.` or `>>=` as single characters to close type parameters without prematurely treating `>` as an operator.
        CharsConsumedAfterTypeParams: int
        ConditionalCompilationStack: PositionedToken list
        /// When true, the next `KWRAttrBracket` token encountered by `nextNonTriviaTokenImpl`
        /// is rewritten to `KWRBracket`. Set by the measure parser when it splits `>]` into
        /// a virtual `>` (for the measure close) and a real `]` (for the enclosing indexer).
        SplitRAttrBracket: bool
        /// When true, the next custom operator token starting with `^-` (which the lexer fuses
        /// as a single operator at Append precedence) is rewritten to `OpSubtraction` at
        /// `StartIndex + 1`. Set by the measure parser when it splits `^-N` into a virtual `^`
        /// (for the power operator) and a real `-` followed by the numeric exponent.
        SplitPowerMinus: bool
        /// Accumulated #nowarn / #warnon directives (most recent first).
        WarnDirectives: WarnDirective list
        /// Callback for structured parse tracing. Default is no-op.
        /// Shared across immutable record copies.
        Trace: TraceCallback
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
    let createWithTracing (lexed: Lexed) input definedSymbols (trace: TraceCallback) =
        {
            Input = input
            Lexed = lexed
            Context = []
            Diagnostics = []
            DefinedSymbols = definedSymbols
            IndentationMode = Syntax.Light
            LastLine = 0<line>
            // ReprocessOpAfterTypeDeclaration = false
            CharsConsumedAfterTypeParams = 0
            ConditionalCompilationStack = []
            SplitRAttrBracket = false
            SplitPowerMinus = false
            WarnDirectives = []
            Trace = trace
        }

    let create (lexed: Lexed) input definedSymbols =
        createWithTracing lexed input definedSymbols TraceCallback.Empty

    let setIndentOn (state: ParseState) =
        { state with
            IndentationMode = Syntax.Light
        }

    let setIndentOff (state: ParseState) =
        { state with
            IndentationMode = Syntax.Verbose
        }

    let pushOffside offsideCtx (state: ParseState) =
        let newState =
            { state with
                Context = offsideCtx :: state.Context
            }

        let stackDepth = newState.Context.Length

        newState.Trace.Invoke(
            TraceEvent.ContextPush(offsideCtx.Context, offsideCtx.Indent, offsideCtx.Token, stackDepth)
        )

        newState

    let popOffside current (state: ParseState) =
        match state.Context with
        | [] -> invalidOp "Attempted to pop empty context"
        | head :: tail ->
            if head <> current then
                invalidOp $"Attempted to pop context {current} but top of stack was {head}"

            state.Trace.Invoke(TraceEvent.ContextPop(head.Context, state.Context.Length))
            { state with Context = tail }

    let addDiagnostic code severity startToken endToken error (state: ParseState) =
        state.Trace.Invoke(TraceEvent.DiagnosticEmitted(code, severity, startToken))

        let diag =
            {
                Code = code
                Severity = severity
                Token = startToken
                TokenEnd = endToken
                Error = error
            }

        { state with
            Diagnostics = diag :: state.Diagnostics
        }

    /// Shortcut for the common case of emitting an Error diagnostic at a single token
    /// with no end-token range and no underlying parser error.
    let addErrorDiagnostic code startToken state =
        addDiagnostic code DiagnosticSeverity.Error startToken None None state

    /// Shortcut for the common case of emitting an Error diagnostic at a single token
    /// that wraps an underlying parser error.
    let addErrorDiagnosticWithError code startToken err state =
        addDiagnostic code DiagnosticSeverity.Error startToken None (Some err) state

    let private findLineNumberImpl (lexed: Lexed) (guess: int<line>) (index: int<token>) =
        if index < 0<token> || index >= lexed.Tokens.LengthM then
            invalidArg (nameof index) "Index out of range"

        let lineStarts = lexed.LineStarts
        let lineCount = lineStarts.LengthM

        // Precondition (maintained through recursion): lineStarts[low] <= index.
        // This holds initially because every initial range produced below either
        // already satisfies it (guess or guess+1 paths) or starts at low = 0, and
        // lineStarts[0] = 0 <= index for any non-negative token index.
        // Returns the largest i in [low, high] with lineStarts[i] <= index.
        let rec search (low: int<line>) (high: int<line>) =
            if low >= high then
                low
            else
                let mid = low + (high - low + 1<line>) / 2 // upper-biased to avoid infinite loop when low + 1 = high

                if lineStarts.[mid] <= index then
                    search mid high
                else
                    search low (mid - 1<line>)

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
        | TokenIndex.Regular iT -> state.Lexed.GetTokenString(iT, state.Input)

    let tokenStringIs (s: string) (token: SyntaxToken) (state: ParseState) =
        match token.Index with
        | TokenIndex.Virtual -> false
        | TokenIndex.Regular iT ->
            let span = state.Lexed.GetTokenSpan(iT, state.Input)
            span.SequenceEqual(s.AsSpan())

    let tokenStringStartsWith (s: string) (token: SyntaxToken) (state: ParseState) =
        match token.Index with
        | TokenIndex.Virtual -> false
        | TokenIndex.Regular iT ->
            let span = state.Lexed.GetTokenSpan(iT, state.Input)
            span.StartsWith(s.AsSpan())

    let isDefined (state: ParseState) (symbolToken: SyntaxToken) =
        let symbol = tokenString symbolToken state
        state.DefinedSymbols.Contains(symbol)

    /// Check if a warning number is suppressed at a given line.
    let isWarningSuppressed (warningNumber: int) (line: int<line>) (state: ParseState) =
        state.WarnDirectives
        |> List.tryFind (fun d -> d.WarningNumber = warningNumber && d.Line <= line)
        |> Option.map (fun d -> d.Suppress)
        |> Option.defaultValue false

module TraceEvent =
    let format (lexed: Lexed) (event: TraceEvent) =
        match event with
        | TraceEvent.ContextPush(ctx, indent, token, depth) ->
            $"PUSH {ctx} indent={indent} token={token.Token} @{token.StartIndex} depth={depth}"
        | TraceEvent.ContextPop(ctx, depth) -> $"POP {ctx} depth={depth}"
        | TraceEvent.TokenConsumed(token, index, col) ->
            let line = lexed.GetLineForToken(index * 1<token>)
            $"CONSUME {token.Token} @{token.StartIndex} index={index} col={col} line={line}"
        | TraceEvent.TokenPeeked(token, index, col) ->
            let line = lexed.GetLineForToken(index * 1<token>)
            $"PEEK {token.Token} @{token.StartIndex} index={index} col={col} line={line}"
        | TraceEvent.VirtualToken(token, startIndex) -> $"VIRTUAL {token} @{startIndex}"
        | TraceEvent.OffsideOk(token, tokenCol, contextIndent, ctx) ->
            $"OFFSIDE_OK {token.Token} col={tokenCol} >= indent={contextIndent} ctx={ctx}"
        | TraceEvent.OffsideFail(token, tokenCol, contextIndent, ctx) ->
            $"OFFSIDE_FAIL {token.Token} col={tokenCol} < indent={contextIndent} ctx={ctx}"
        | TraceEvent.PermittedUndentation(token, tokenCol, contextIndent, rule) ->
            $"UNDENT_OK {token.Token} col={tokenCol} < indent={contextIndent} rule={rule}"
        | TraceEvent.DiagnosticEmitted(code, severity, token) -> $"DIAGNOSTIC {severity} {code} @{token.StartIndex}"
        | TraceEvent.SplitRAttrBracketSet(startIndex) -> $"SPLIT_RATTR_SET @{startIndex}"
        | TraceEvent.SplitRAttrBracketConsumed(startIndex) -> $"SPLIT_RATTR_CONSUMED @{startIndex}"
        | TraceEvent.SplitPowerMinusSet(startIndex) -> $"SPLIT_POW_MINUS_SET @{startIndex}"
        | TraceEvent.SplitPowerMinusConsumed(startIndex) -> $"SPLIT_POW_MINUS_CONSUMED @{startIndex}"
        | TraceEvent.Message(msg) -> $"MSG: {msg}"

[<RequireQualifiedAccess>]
module Reader =
    let ofLexed (lexed: Lexed) (input: string) (definedSymbols: Set<string>) : Reader<_, ParseState, _, _> =
        let initialState = ParseState.create lexed input definedSymbols
        Reader.ofImmutableArray (lexed.Tokens.AsImmutableArray()) initialState

    let ofLexedWithTracing
        (lexed: Lexed)
        (input: string)
        (definedSymbols: Set<string>)
        (trace: TraceCallback)
        : Reader<_, ParseState, _, _> =
        let initialState = ParseState.createWithTracing lexed input definedSymbols trace
        Reader.ofImmutableArray (lexed.Tokens.AsImmutableArray()) initialState
