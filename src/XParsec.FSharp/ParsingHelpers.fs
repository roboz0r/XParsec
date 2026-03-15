namespace XParsec.FSharp.Parser

open System
open System.Collections.Generic
open System.Collections.Immutable
open XParsec
open XParsec.Parsers
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser.SyntaxToken
open XParsec.FSharp.Parser.ParseState

[<AutoOpen>]
module Parsing =
    let tokenIndex (reader: Reader<PositionedToken, ParseState, _, _>) = reader.Index * 1<token>

    /// Scans forward past tokens in an inactive branch until reaching #else or #endif at depth 0.
    /// Nested #if/#endif pairs are depth-tracked and correctly skipped.
    /// The stop token (#else or #endif) is consumed before returning.
    /// Returns true if stopped at #else (an else-branch follows), false if stopped at #endif.
    /// Directives inside block comments (with the InComment flag) are correctly ignored.
    let skipInactiveBranch (reader: Reader<PositionedToken, ParseState, _, _>) =
        let mutable depth = 0
        let mutable foundElse = false
        let mutable stop = false

        while not stop do
            match reader.Peek() with
            | ValueNone -> stop <- true // EOF — unclosed #if, stop gracefully
            | ValueSome t ->
                match t.Token with
                | Token.IfDirective ->
                    // Nested #if: increase depth to track nesting
                    depth <- depth + 1
                    reader.Skip()
                | Token.EndIfDirective when depth = 0 ->
                    // Matching #endif found: consume it and stop
                    reader.Skip()
                    stop <- true
                | Token.EndIfDirective ->
                    // #endif for a nested #if: decrease depth
                    depth <- depth - 1
                    reader.Skip()
                | Token.ElseDirective when depth = 0 ->
                    // Matching #else found: consume it and stop; else-branch is now active
                    reader.Skip()
                    foundElse <- true
                    stop <- true
                | _ -> reader.Skip()

        foundElse

    /// Scans forward past an active else-branch until the matching #endif at depth 0 is consumed.
    /// Must be called after the #else token itself has already been consumed.
    let skipElseBranch (reader: Reader<PositionedToken, ParseState, _, _>) =
        let mutable depth = 0
        let mutable stop = false

        while not stop do
            match reader.Peek() with
            | ValueNone -> stop <- true // EOF — unclosed #endif, stop gracefully
            | ValueSome t ->
                match t.Token with
                | Token.IfDirective ->
                    depth <- depth + 1
                    reader.Skip()
                | Token.EndIfDirective when depth = 0 ->
                    reader.Skip()
                    stop <- true
                | Token.EndIfDirective ->
                    depth <- depth - 1
                    reader.Skip()
                | _ -> reader.Skip()

    /// Processes a #if directive: parses the condition expression, evaluates it against
    /// the current defined symbols, and either continues into the active branch or skips
    /// to the matching #else/#endif.
    /// Must be called with the reader positioned at the #if token (not yet consumed).
    let processIfDirective
        (nextNonTriviaToken: Parser<_, _, _, _, _>)
        (ifToken: PositionedToken)
        (reader: Reader<PositionedToken, ParseState, _, _>)
        =
        let state = reader.State
        let lexed = state.Lexed
        let currentLine = findLineNumber state (tokenIndex reader)
        let nextLine = currentLine + 1<_>

        let nextLineTokenIndex =
            if nextLine < lexed.LineStarts.LengthM then
                lexed.LineStarts[nextLine]
            else
                lexed.Tokens.LengthM - 1<_> // EOF, will be handled gracefully by nextNonTriviaToken

        // Create a slice of just the #if directive line, carrying the absolute start index
        // so the IfExpr parser can compute absolute token indices for symbol name extraction.
        let sliceLen = (nextLineTokenIndex * 1< / token>) - reader.Index
        let sliceReader = reader.Slice(0, sliceLen, { AbsoluteStart = reader.Index })

        // Advance the main reader past the #if line before branching
        reader.Index <- nextLineTokenIndex * 1< / token>

        match IfExpr.parseSlice sliceReader with
        | Ok ifExpr ->
            if IfExpr.evaluateStateful ifExpr reader.State then
                // Condition is true: the then-branch is active.
                // #else and #endif encountered later will be handled by nextNonTriviaToken.
                nextNonTriviaToken reader
            else if
                // Condition is false: skip over the inactive then-branch.
                skipInactiveBranch reader
            then
                // Stopped at #else: the else-branch is now active
                nextNonTriviaToken reader
            else
                // Stopped at #endif: entire block skipped
                nextNonTriviaToken reader
        | Error e ->
            // Invalid #if expression: record a diagnostic and treat the whole block as inactive
            let msg = $"Invalid #if expression: {e}"
            reader.State <- addDiagnostic (DiagnosticCode.Other msg) DiagnosticSeverity.Error ifToken None reader.State
            skipInactiveBranch reader |> ignore
            nextNonTriviaToken reader

    /// Returns the text length of the token at the given index in the lexed token array.
    let private getTokenLength (state: ParseState) (index: int<token>) =
        let tokens = state.Lexed.Tokens

        if index + 1<token> < tokens.LengthM then
            let t0 = tokens[index]
            let t1 = tokens[index + 1<token>]
            t1.StartIndex - t0.StartIndex
        else
            0

    /// Returns true if the token can appear as an infix operator in expressions.
    /// Used for the SeqBlock infix undentation exception (F# spec 15.1.9).
    let private isInfixToken (token: Token) =
        match token with
        | Token.OpAddition
        | Token.OpSubtraction
        | Token.OpMultiply
        | Token.OpDivision
        | Token.OpModulus
        | Token.OpExponentiation
        | Token.OpPipeRight
        | Token.OpPipeRight2
        | Token.OpPipeRight3
        | Token.OpPipeLeft
        | Token.OpPipeLeft2
        | Token.OpPipeLeft3
        | Token.OpComposeLeft
        | Token.OpComposeRight
        | Token.OpBooleanAnd
        | Token.OpBooleanOr
        | Token.OpBitwiseAnd
        | Token.OpBitwiseOr
        | Token.OpExclusiveOr
        | Token.OpLeftShift
        | Token.OpRightShift
        | Token.OpLessThan
        | Token.OpGreaterThan
        | Token.OpLessThanOrEqual
        | Token.OpGreaterThanOrEqual
        | Token.OpEquality
        | Token.OpInequality
        | Token.OpAppend
        | Token.OpCons
        | Token.OpArrowRight
        | Token.OpColonEquals
        | Token.OpBar
        | Token.OpBarBar
        | Token.OpAmp
        | Token.OpAmpAmp
        | Token.OpConcatenate
        | Token.OpComma
        | Token.OpSemicolon
        | Token.OpDot -> true
        | _ ->
            // Custom operators are TokenKind.Operator but not in the keyword list above
            TokenInfo.isOperator token && not (TokenInfo.canBePrefix token)

    /// Checks whether a context in the stack permits the given token at the given column.
    /// `ctx` is the context to check, `tokenCol` is the column of the token.
    let private contextPermitsToken (token: Token) (tokenCol: int) (ctx: Offside) =
        ctx.Indent <= tokenCol
        && (
            match ctx.Context, token with
            // 15.1.9: then/elif/else may align with if
            | OffsideContext.If, (Token.KWThen | Token.KWElif | Token.KWElse) -> true
            // 15.1.9: with/finally may align with try
            | OffsideContext.Try, (Token.KWWith | Token.KWFinally) -> true
            // 15.1.9: done may align with for
            | OffsideContext.For, Token.KWDone -> true
            // 15.1.9: done may align with do
            | OffsideContext.Do, Token.KWDone -> true
            // 15.1.9: and may align with let
            | OffsideContext.Let, Token.KWAnd -> true
            // 15.1.9: }, end, and, | may align with type
            | OffsideContext.Type, (Token.KWRBrace | Token.KWEnd | Token.KWAnd | Token.OpBar) -> true
            // 15.1.9: end may align with interface (WithAugment)
            | OffsideContext.WithAugment, Token.KWEnd -> true
            | _ -> false
        )

    /// Returns true if the token is a closing delimiter that matches the given paren-like context.
    /// Closing delimiters are never offside from their matching opening context (F# spec 15.1.8).
    let private isMatchingClose (token: Token) (ctx: OffsideContext) =
        match ctx, token with
        | OffsideContext.Paren, Token.KWRParen -> true
        | OffsideContext.Bracket, Token.KWRBracket -> true
        | OffsideContext.BracketBar, Token.KWRArrayBracket -> true
        | OffsideContext.Brace, Token.KWRBrace -> true
        | OffsideContext.Begin, Token.KWEnd -> true
        | OffsideContext.Quote, (Token.OpQuotationTypedRight | Token.OpQuotationUntypedRight) -> true
        | _ -> false

    /// Determines whether a token at column `tokenCol` is permitted despite being
    /// strictly left of the innermost context's offside line.
    /// Implements F# spec sections 15.1.8 (Balancing), 15.1.9 (Exceptions to Offside Rules)
    /// and 15.1.10 (Permitted Undentations).
    let rec private isPermittedUndentation
        (token: Token)
        (tokenCol: int)
        (context: Offside list)
        (state: ParseState)
        (readerIndex: int64)
        =
        match context with
        | [] -> ValueNone
        | head :: rest ->

            // Closing delimiters are never offside from their matching paren-like context (15.1.8)
            if isMatchingClose token head.Context then
                ValueSome "15.1.8 MatchingClose"
            elif rest |> List.exists (fun (ctx: Offside) -> isMatchingClose token ctx.Context) then
                ValueSome "15.1.8 MatchingClose"

            // --- 15.1.9: Exceptions to Offside Rules ---

            // SeqBlock infix: an infix token may be offside by (tokenSize + 1)
            elif head.Context = OffsideContext.SeqBlock && isInfixToken token then
                let tokenLength = getTokenLength state (int readerIndex * 1<token>)

                if tokenCol >= head.Indent - (tokenLength + 1) then
                    ValueSome "15.1.9 InfixUndent"
                else if
                    // Still check deeper contexts
                    rest |> List.exists (contextPermitsToken token tokenCol)
                then
                    ValueSome "15.1.9 ContextPermits"
                else
                    ValueNone

            // Check if the token is permitted at the head context or any enclosing context
            elif contextPermitsToken token tokenCol head then
                ValueSome "15.1.9 ContextPermits"

            elif rest |> List.exists (contextPermitsToken token tokenCol) then
                ValueSome "15.1.9 ContextPermits"

            // --- 15.1.10: Permitted Undentations ---

            // 15.1.10.1: Fun/Function body undentation
            // The body may undent from fun/function but not past other offside lines.
            // "Constructs enclosed in brackets may be undented" — so we skip past
            // SeqBlock+Paren pairs to find the true enclosing offside line.
            elif head.Context = OffsideContext.Fun || head.Context = OffsideContext.Function then
                let rec findEnclosingIndent (stack: Offside list) =
                    match stack with
                    | [] -> true // No other context to violate
                    | ctx :: deeper ->
                        match ctx.Context with
                        | OffsideContext.SeqBlock
                        | OffsideContext.Paren
                        | OffsideContext.Bracket
                        | OffsideContext.BracketBar
                        | OffsideContext.Brace
                        | OffsideContext.Begin -> findEnclosingIndent deeper
                        | _ -> tokenCol >= ctx.Indent

                if findEnclosingIndent rest then
                    ValueSome "15.1.10.1 FunBody"
                else
                    ValueNone

            // 15.1.10.2: If/Then/Else + Paren/Begin undentation
            // Inside ( ) or begin/end following then/else, content may undent but not past if.
            elif head.Context = OffsideContext.Paren || head.Context = OffsideContext.Begin then
                let rec checkThenElseUndent (stack: Offside list) =
                    match stack with
                    | [] -> false
                    | ctx :: deeper ->
                        match ctx.Context with
                        | OffsideContext.Then
                        | OffsideContext.Else ->
                            // Find the enclosing If context and check we don't undent past it
                            deeper
                            |> List.tryFind (fun (c: Offside) -> c.Context = OffsideContext.If)
                            |> Option.map (fun ifCtx -> tokenCol >= ifCtx.Indent)
                            |> Option.defaultValue true
                        | OffsideContext.SeqBlock ->
                            // Skip SeqBlock contexts (they sit between paren and then/else)
                            checkThenElseUndent deeper
                        | _ -> false

                if checkThenElseUndent rest then
                    ValueSome "15.1.10.2 ThenElse"
                else if
                    // 15.1.10.3: Module/class body undentation
                    // Inside begin/end with enclosing Module or Type, content may undent to Module/Type indent.
                    head.Context = OffsideContext.Begin
                then
                    let result =
                        rest
                        |> List.tryFind (fun (c: Offside) ->
                            c.Context = OffsideContext.Module || c.Context = OffsideContext.Type
                        )
                        |> Option.map (fun (ctx: Offside) -> tokenCol >= ctx.Indent)
                        |> Option.defaultValue false

                    if result then
                        ValueSome "15.1.10.3 BeginModule"
                    else
                        ValueNone
                else if
                    // 15.1.10.4: Collection/CE undentation
                    // Inside [ ], [| |], or { }, content may undent to the enclosing expression's
                    // offside line, skipping SeqBlock/Paren pairs introduced by ( or =.
                    checkCollectionUndent tokenCol rest
                then
                    ValueSome "15.1.10.4 Collection"
                else
                    ValueNone

            // 15.1.10.4: Collection/CE undentation for Bracket, BracketBar, Brace contexts
            elif
                head.Context = OffsideContext.Bracket
                || head.Context = OffsideContext.BracketBar
                || head.Context = OffsideContext.Brace
            then
                if checkCollectionUndent tokenCol rest then
                    ValueSome "15.1.10.4 Collection"
                else
                    ValueNone

            else
                ValueNone

    /// Walk the context stack skipping SeqBlock+Paren pairs to find the enclosing
    /// expression's offside line for collection/CE undentation (F# spec 15.1.10.4).
    and private checkCollectionUndent (tokenCol: int) (stack: Offside list) : bool =
        match stack with
        | [] -> false
        | ctx :: deeper ->
            match (ctx: Offside).Context with
            | OffsideContext.SeqBlock ->
                // Skip this SeqBlock and check what's beneath it
                checkCollectionUndent tokenCol deeper
            | OffsideContext.Paren
            | OffsideContext.Bracket
            | OffsideContext.BracketBar
            | OffsideContext.Brace
            | OffsideContext.Begin ->
                // Skip paren-like context (and its associated SeqBlock) and keep looking
                checkCollectionUndent tokenCol deeper
            | _ ->
                // Found the enclosing non-paren context; check if token is within its indent
                tokenCol >= ctx.Indent

    let rec private nextNonTriviaTokenImpl isPeek (reader: Reader<PositionedToken, ParseState, _, _>) =
        match reader.Peek() with
        | ValueNone -> fail EndOfInput reader
        | ValueSome token when token.Token = Token.IfDirective ->
            // processIfDirective expects the reader to be positioned AT the #if token
            processIfDirective (nextNonTriviaTokenImpl isPeek) token reader
        | ValueSome token when token.Token = Token.ElseDirective ->
            // We are in an active then-branch that has reached its #else.
            // Skip the else-branch contents up to and including the matching #endif.
            reader.Skip() // consume #else
            skipElseBranch reader
            nextNonTriviaTokenImpl isPeek reader
        | ValueSome token when token.Token = Token.EndIfDirective ->
            // End of a conditional block whose then-branch was active (no #else encountered).
            reader.Skip() // consume #endif
            nextNonTriviaTokenImpl isPeek reader
        | ValueSome token when isTriviaToken reader.State token ->
            reader.Skip()
            nextNonTriviaTokenImpl isPeek reader
        | ValueSome token ->
            // SplitRAttrBracket: when the measure parser consumed the `>` half of `>]`,
            // it sets this flag so the remaining `]` half is presented as KWRBracket.
            let token =
                if reader.State.SplitRAttrBracket && token.Token = Token.KWRAttrBracket then
                    reader.State.Trace.Invoke(TraceEvent.SplitRAttrBracketConsumed(token.StartIndex))
                    PositionedToken.Create(Token.KWRBracket, token.StartIndex + 1)
                else
                    token

            // Offside check (Light syntax only): fail if the token is strictly left of the
            // innermost context's offside line, unless a permitted undentation applies.
            let isOffside =
                reader.State.IndentationMode = Syntax.Light
                && (
                    match reader.State.Context with
                    | {
                          Indent = contextIndent
                          Context = ctx
                      } :: _ as context ->
                        let tokenCol = ParseState.getIndent reader.State (reader.Index * 1<token>)

                        if tokenCol < contextIndent then
                            match isPermittedUndentation token.Token tokenCol context reader.State reader.Index with
                            | ValueSome rule ->
                                reader.State.Trace.Invoke(
                                    TraceEvent.PermittedUndentation(token, tokenCol, contextIndent, rule)
                                )

                                false
                            | ValueNone ->
                                reader.State.Trace.Invoke(TraceEvent.OffsideFail(token, tokenCol, contextIndent, ctx))
                                true
                        else
                            reader.State.Trace.Invoke(TraceEvent.OffsideOk(token, tokenCol, contextIndent, ctx))
                            false
                    | [] -> false
                )

            if isOffside then
                fail (Message "Offside") reader
            else
                let t = syntaxToken token reader.Index

                if isPeek then
                    let col = ParseState.getIndent reader.State (reader.Index * 1<token>)
                    reader.State.Trace.Invoke(TraceEvent.TokenPeeked(token, int reader.Index, col))
                    preturn t reader
                else
                    let col = ParseState.getIndent reader.State (reader.Index * 1<token>)
                    reader.State.Trace.Invoke(TraceEvent.TokenConsumed(token, int reader.Index, col))

                    if reader.State.SplitRAttrBracket then
                        reader.State <-
                            { reader.State with
                                SplitRAttrBracket = false
                            }

                    reader.Skip()
                    preturn t reader

    let nextNonTriviaToken (reader: Reader<PositionedToken, ParseState, _, _>) = nextNonTriviaTokenImpl false reader

    /// Advanced the reader to the next non-trivia token and returns it without consuming it.
    /// Allows parser to avoid re-skipping trivia tokens when it needs to look ahead at the next token to decide what to parse.
    let peekNextNonTriviaToken (reader: Reader<PositionedToken, ParseState, _, _>) = nextNonTriviaTokenImpl true reader

    /// Consumes the given token, which must have been previously returned by `peekNextNotTriviaToken`, and returns it.
    let consumePeeked (token: SyntaxToken) (reader: Reader<PositionedToken, ParseState, _, _>) =
        match token.Index with
        | TokenIndex.Virtual -> invalidOp "Cannot consume a virtual token"
        | TokenIndex.Regular tokenIdx ->
            assert (reader.Index = tokenIdx * 1< / token>) // Ensure the reader is still at the expected position
            reader.Index <- (tokenIdx + 1<token>) * 1< / token>
            preturn token reader

    let rec nextNonTriviaTokenVirtualIfNot t (reader: Reader<PositionedToken, ParseState, _, _>) =
        match peekNextNonTriviaToken reader with
        | Error e -> Error e
        | Ok token ->
            if token.Token = t then
                // Real token matches: consume it and return it.
                consumePeeked token reader
            else
                // Real token doesn't match: synthesise a virtual token in its place without
                // consuming the actual token (the caller's body parser will see it next).
                let pt =
                    PositionedToken.Create(
                        Token.ofUInt16 (uint16 t ||| TokenRepresentation.IsVirtual),
                        token.StartIndex
                    )

                reader.State.Trace.Invoke(TraceEvent.VirtualToken(pt.Token, pt.StartIndex))

                preturn
                    {
                        PositionedToken = pt
                        Index = TokenIndex.Virtual
                    }
                    reader

    let rec nextNonTriviaTokenSatisfiesL (predicate: SyntaxToken -> bool) msg reader =
        match peekNextNonTriviaToken reader with
        | Error e -> Error e
        | Ok token ->
            if predicate token then
                consumePeeked token reader
            else
                fail (Message msg) reader

    let nextNonTriviaTokenIsL (t: Token) msg =
        nextNonTriviaTokenSatisfiesL (fun synTok -> synTok.Token = t) msg

    let dispatchNextNonTriviaTokenFallback (routes: (Token * Parser<_, _, _, _, _>) list) pFallback =
        // Note: Routes are typically <20 items, so linear search is fine. Likely to be 5 or less in practice.
        // So an array is likely more efficient than a dictionary.
        let items = routes |> List.map fst |> Array.ofList
        let parsers = routes |> List.map snd |> Array.ofList

        parser {
            let! next = peekNextNonTriviaToken

            match Array.tryFindIndexV (fun t -> next.Token = t) items with
            | ValueSome i -> return! parsers[i]
            | ValueNone -> return! pFallback
        }

    let dispatchNextNonTriviaTokenL (routes: (Token * Parser<_, _, _, _, _>) list) fallbackMsg =
        dispatchNextNonTriviaTokenFallback routes (fail (Message fallbackMsg))

    let currentIndent (reader: Reader<PositionedToken, ParseState, 'a, 'b>) =
        let state = reader.State
        let index = int reader.Index * 1<token>
        let indent = ParseState.getIndent state index
        preturn indent reader

    /// Returns the column (0-based) of the next non-trivia token without consuming it.
    /// Returns -1 at EOF.
    let peekNonTriviaIndent: Parser<int, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        lookAhead (fun r ->
            match nextNonTriviaToken r with
            | Error _ -> preturn -1 r
            | Ok token ->
                match token.Index with
                | TokenIndex.Virtual -> preturn 0 r
                | TokenIndex.Regular tokenIdx ->
                    let indent = ParseState.getIndent r.State tokenIdx
                    preturn indent r
        )

    /// Synthesises a VirtualSep (virtual `;`) token at the current reader position
    /// without consuming any input.
    let makeVirtualSep (reader: Reader<PositionedToken, ParseState, _, _>) =
        match reader.Peek() with
        | ValueNone -> fail EndOfInput reader
        | ValueSome token ->
            reader.State.Trace.Invoke(TraceEvent.VirtualToken(Token.VirtualSep, token.StartIndex))

            preturn
                {
                    PositionedToken = PositionedToken.Create(Token.VirtualSep, token.StartIndex)
                    Index = TokenIndex.Virtual
                }
                reader

    /// Like nextNonTriviaTokenVirtualIfNot but emits a UnclosedDelimiter diagnostic
    /// when the token must be synthesised.
    let nextNonTriviaTokenVirtualWithDiagnostic (openTok: SyntaxToken voption) t reader =
        match peekNextNonTriviaToken reader with
        | Error e -> Error e
        | Ok token ->
            if token.Token = t then
                consumePeeked token reader
            else
                let code =
                    match openTok with
                    | ValueSome o -> DiagnosticCode.UnclosedDelimiter(o, t)
                    | ValueNone -> DiagnosticCode.Other $"Expected '{t}'"

                reader.State <-
                    ParseState.addDiagnostic code DiagnosticSeverity.Error token.PositionedToken None reader.State

                let pt =
                    PositionedToken.Create(
                        Token.ofUInt16 (uint16 t ||| TokenRepresentation.IsVirtual),
                        token.StartIndex
                    )

                reader.State.Trace.Invoke(TraceEvent.VirtualToken(pt.Token, pt.StartIndex))

                preturn
                    {
                        PositionedToken = pt
                        Index = TokenIndex.Virtual
                    }
                    reader

    module StoppingTokens =
        let afterType (tok: SyntaxToken) =
            match tok.Token with
            | Token.OpComma
            | Token.KWRParen
            | Token.KWRBracket
            | Token.KWRBrace
            | Token.OpEquality
            | Token.KWWith
            | Token.KWIn
            | Token.OpArrowRight
            | Token.OpBar
            | Token.OpSemicolon
            | Token.EOF -> true
            | _ -> false

        let afterPattern (tok: SyntaxToken) =
            match tok.Token with
            | Token.OpArrowRight
            | Token.KWWhen
            | Token.OpBar
            | Token.OpEquality
            | Token.KWIn
            | Token.EOF -> true
            | _ -> false

        let afterExpr (tok: SyntaxToken) =
            match tok.Token with
            | Token.OpSemicolon
            | Token.KWIn
            | Token.OpBar
            | Token.KWWith
            | Token.KWRParen
            | Token.KWRBracket
            | Token.KWRBrace
            | Token.KWThen
            | Token.KWElse
            | Token.KWElif
            | Token.KWEnd
            | Token.KWDone
            | Token.KWDo
            | Token.InterpolatedExpressionClose
            | Token.EOF -> true
            | _ -> false

        let afterParen (tok: SyntaxToken) =
            match tok.Token with
            | Token.KWWith
            | Token.KWRParen
            | Token.KWRBracket
            | Token.KWRBrace
            | Token.KWEnd
            | Token.KWRArrayBracket
            | Token.OpQuotationTypedRight
            | Token.OpQuotationUntypedRight
            | Token.EOF -> true
            | _ -> false

        let afterRule (tok: SyntaxToken) =
            match tok.Token with
            | Token.OpBar
            | Token.KWWith
            | Token.KWEnd
            | Token.EOF -> true
            | _ -> false

        let afterTypeDefn (tok: SyntaxToken) =
            match tok.Token with
            | Token.KWAnd
            | Token.KWType
            | Token.KWEnd
            | Token.EOF -> true
            | _ -> false

    /// On failure: calls makeCode with the ParseError to produce a DiagnosticCode,
    /// emits the diagnostic, skips tokens until `stopping` returns true,
    /// then succeeds with `placeholder skippedTokens`.
    let recoverWith
        (stopping: SyntaxToken -> bool)
        (severity: DiagnosticSeverity)
        (makeCode: ParseError<PositionedToken, ParseState> -> DiagnosticCode)
        (placeholder: SyntaxToken list -> 'Parsed)
        (p: Parser<'Parsed, PositionedToken, ParseState, _, _>)
        : Parser<'Parsed, PositionedToken, ParseState, _, _> =
        fun reader ->
            match peekNextNonTriviaToken reader with
            | Error e -> Error e
            | Ok startTok ->
                let pos = reader.Position

                match p reader with
                | Ok result -> Ok result
                | Error err ->
                    reader.Position <- pos // backtrack to the start of the failed parse, so we can skip the same tokens it would have seen
                    let skipped = ResizeArray<SyntaxToken>()
                    let mutable keepGoing = true

                    while keepGoing do
                        match peekNextNonTriviaToken reader with
                        | Error _ -> keepGoing <- false
                        | Ok tok ->
                            if stopping tok then
                                keepGoing <- false
                            else
                                match consumePeeked tok reader with
                                | Ok t -> skipped.Add(t)
                                | Error _ -> keepGoing <- false

                    let code = makeCode err

                    reader.State <- ParseState.addDiagnostic code severity startTok.PositionedToken None reader.State

                    Ok(placeholder (List.ofSeq skipped))

    /// Wraps a parser with an offside context. Peeks the first token the inner parser will see
    /// to establish the offside column, pushes an Offside entry onto ParseState.Context, runs
    /// the inner parser, then pops the context on success or restores the full saved state on
    /// failure (keeping the operation safe for backtracking).
    let withContext (ctx: OffsideContext) innerParser (reader: Reader<PositionedToken, ParseState, _, _>) =
        let savedState = reader.State

        match peekNextNonTriviaToken reader with
        | Error e -> Error e
        | Ok peekTok ->
            let indent =
                match peekTok.Index with
                | TokenIndex.Regular iT -> ParseState.getIndent reader.State iT
                | TokenIndex.Virtual -> 0

            let entry =
                {
                    Context = ctx
                    Indent = indent
                    Token = peekTok.PositionedToken
                }

            reader.State <- ParseState.pushOffside entry reader.State
            let stackDepth = reader.State.Context.Length

            reader.State.Trace.Invoke(TraceEvent.ContextPush(ctx, indent, peekTok.PositionedToken, stackDepth))

            match innerParser reader with
            | Ok result ->
                reader.State.Trace.Invoke(TraceEvent.ContextPop(ctx, stackDepth - 1))
                reader.State <- ParseState.popOffside reader.State
                Ok result
            | Error _ as e ->
                reader.State <- savedState
                e

    /// Like `withContext`, but uses an explicit indent and token for the offside context
    /// instead of peeking the next token. Used when the context's offside line should be
    /// at an already-known position (e.g. the `let` keyword's column per F# spec 15.1.7).
    let withContextAt
        (ctx: OffsideContext)
        (indent: int)
        (token: PositionedToken)
        innerParser
        (reader: Reader<PositionedToken, ParseState, _, _>)
        =
        let savedState = reader.State

        let entry =
            {
                Context = ctx
                Indent = indent
                Token = token
            }

        reader.State <- ParseState.pushOffside entry reader.State
        let stackDepth = reader.State.Context.Length
        reader.State.Trace.Invoke(TraceEvent.ContextPush(ctx, indent, token, stackDepth))

        match innerParser reader with
        | Ok result ->
            reader.State.Trace.Invoke(TraceEvent.ContextPop(ctx, stackDepth - 1))
            reader.State <- ParseState.popOffside reader.State
            Ok result
        | Error _ as e ->
            reader.State <- savedState
            e

    /// Record field separator: accepts a real ';' or emits a virtual separator when the next
    /// token is at the same indent as the enclosing SeqBlock context (spec §15.1.5: $sep insertion).
    let pRecordFieldSep: Parser<SyntaxToken, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        let failSep = fail (Message "Expected ';' or newline at the same indent")

        parser {
            match! peekNextNonTriviaToken with
            | t when t.Token = Token.OpSemicolon -> return! consumePeeked t
            | t when t.Token = Token.KWRBrace -> return! failSep
            | t ->
                let! indent = currentIndent
                let! state = getUserState

                let atContextIndent =
                    match state.Context with
                    | { Indent = ctxIndent } :: _ -> indent = ctxIndent
                    | [] -> false

                if atContextIndent then
                    return virtualToken (PositionedToken.Create(Token.OpSemicolon, t.StartIndex))
                else
                    return! failSep
        }

    /// Fails if the next non-trivia token is 't'. Saves and restores reader position fully.
    let notFollowedByNonTriviaToken t (reader: Reader<PositionedToken, ParseState, _, _>) =
        let pos = reader.Position

        match peekNextNonTriviaToken reader with
        | Ok tok when tok.Token = t ->
            reader.Position <- pos
            fail (Message(sprintf "Named module cannot be followed by '%A'" t)) reader
        | _ ->
            reader.Position <- pos
            preturn () reader

    let inline choiceL p msg =
        // Shadow choiceL to give the full error message in debug builds,
        // but avoid the overhead of constructing the full error message
        // in release builds where it shouldn't be needed.
#if DEBUG
        choice p
#else
        choiceL p msg
#endif

    let pCloseTypeParams: Parser<SyntaxToken, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        // 15.3 Lexical Analysis of Type Applications
        parser {
            let! state = getUserState

            match! peekNextNonTriviaToken with
            | t when t.Token = Token.OpGreaterThan ->
                // We have a '>' that can close the type application, but we need to check if it's part of a larger operator like '>>' or '>>='.
                // If it is, we need to reprocess it after consuming the type application.
                let opString = tokenString t state

                match opString, state.CharsConsumedAfterTypeParams with
                | ">", 0 ->
                    // It's a standalone '>', we can consume it as the closing token for the type application.
                    let! rAngle = consumePeeked t
                    return rAngle
                | _ ->
                    // It's part of a larger operator, we need to reprocess it
                    if opString.[state.CharsConsumedAfterTypeParams] = '>' then
                        // We can have a '>' for the type application, but we need to set the state to reprocess the remaining operator string.
                        if opString.Length = state.CharsConsumedAfterTypeParams + 1 then
                            // The operator is exactly '>' plus some consumed chars, so we can consume the '>' now.
                            let! rAngle = consumePeeked t

                            do!
                                updateUserState (fun s ->
                                    { s with
                                        CharsConsumedAfterTypeParams = 0
                                    }
                                )

                            return rAngle
                        else
                            // The operator has more chars after the '>', we consume the '>' and update the state to reprocess the rest.
                            let rAngle =
                                virtualToken (
                                    PositionedToken.Create(
                                        Token.OpGreaterThan,
                                        t.StartIndex + state.CharsConsumedAfterTypeParams
                                    )
                                )

                            do!
                                updateUserState (fun s ->
                                    { s with
                                        CharsConsumedAfterTypeParams = s.CharsConsumedAfterTypeParams + 1
                                    }
                                )

                            return rAngle
                    else
                        return! fail (Message "Expected '>' to close type application")
            | _ -> return! fail (Message "Expected '>' to close type application")
        }

    let reprocessedOperatorAfterTypeParams
        : Parser<SyntaxToken, PositionedToken, ParseState, ReadableImmutableArray<_>, _> =
        parser {
            let! state = getUserState
            let charsConsumed = state.CharsConsumedAfterTypeParams

            if charsConsumed > 0 then
                match! peekNextNonTriviaToken with
                | t when t.Token = Token.OpGreaterThan ->
                    let! op = consumePeeked t

                    do!
                        updateUserState (fun s ->
                            { s with
                                CharsConsumedAfterTypeParams = 0
                            }
                        )

                    let opString = tokenString t state
                    let opStringRest = opString.Substring(state.CharsConsumedAfterTypeParams)

                    match Lexing.lexString opStringRest with
                    | Ok lexed ->
                        let firstTok = lexed.Tokens[0<token>]

                        return
                            { op with
                                PositionedToken = PositionedToken.Create(firstTok.Token, t.StartIndex + charsConsumed)
                            }
                    | Error _ -> invalidOp "Failed to re-lex operator after type parameters"
                | _ -> return! fail (Message "Expected operator after type parameters")
            else
                return! fail (Message "No operator to reprocess after type parameters")

        }


    let pEnclosed
        completeEmpty
        completeEnclosed
        missing
        skipsTokens
        (pLeft: Parser<_, _, _, _, _>)
        (expectedRightTok: Token)
        (parenKindConstructor: SyntaxToken -> ParenKind<SyntaxToken>)
        (offsideCtx: OffsideContext)
        (diagCode: _ -> DiagnosticCode)
        (pInner: Parser<_, _, _, _, _>)
        : Parser<_, PositionedToken, ParseState, _, _> =

        fun reader ->
            match pLeft reader with
            | Error e -> Error e
            | Ok l ->

                // Push the paren-like offside context immediately after consuming the left
                // delimiter. This must happen before any inner peek/parse so that the
                // collection-undentation rule (15.1.10.4) can see the context on the stack
                // when the inner content is at a lower indentation than the outer SeqBlock.
                let savedState = reader.State

                let entry: Offside =
                    {
                        Context = offsideCtx
                        Indent = 0 // Paren-like contexts use indent 0; undentation rules inspect them as stack markers
                        Token = l.PositionedToken
                    }

                reader.State <- ParseState.pushOffside entry reader.State
                let stackDepth = reader.State.Context.Length

                reader.State.Trace.Invoke(TraceEvent.ContextPush(offsideCtx, 0, l.PositionedToken, stackDepth))

                let inline popAndReturn result =
                    reader.State.Trace.Invoke(TraceEvent.ContextPop(offsideCtx, stackDepth - 1))
                    reader.State <- ParseState.popOffside reader.State
                    result

                match peekNextNonTriviaToken reader with
                | Error e ->
                    reader.State <- savedState
                    Error e
                | Ok t when t.Token = expectedRightTok ->
                    // Fast path: Empty block
                    match consumePeeked t reader with
                    | Ok r -> popAndReturn (Ok(completeEmpty (parenKindConstructor l) r))
                    | Error e ->
                        reader.State <- savedState
                        Error e
                | _ ->
                    // Normal path with recovery
                    let innerParser =
                        recoverWith
                            StoppingTokens.afterParen
                            DiagnosticSeverity.Error
                            diagCode
                            (fun toks ->
                                match toks with
                                | [] ->
                                    let endTok =
                                        virtualToken (PositionedToken.Create(expectedRightTok, l.StartIndex + 1))

                                    completeEnclosed (parenKindConstructor l) missing endTok
                                | _ ->
                                    let endTok =
                                        let t = toks |> List.last
                                        virtualToken (PositionedToken.Create(expectedRightTok, t.StartIndex))

                                    completeEnclosed (parenKindConstructor l) (skipsTokens toks missing) endTok
                            )
                            (parser {
                                let! e = pInner
                                let! r = nextNonTriviaTokenVirtualWithDiagnostic (ValueSome l) expectedRightTok
                                return completeEnclosed (parenKindConstructor l) e r
                            })

                    match innerParser reader with
                    | Ok result -> popAndReturn (Ok result)
                    | Error _ ->
                        reader.State <- savedState
                        fail (Message "pEnclosed inner parser failed") reader
