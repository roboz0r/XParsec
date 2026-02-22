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
            reader.State <- addDiagnostic (DiagnosticCode.Other msg) ifToken None reader.State
            skipInactiveBranch reader |> ignore
            nextNonTriviaToken reader

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
            let t = syntaxToken token reader.Index

            if isPeek then
                preturn t reader
            else
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
            preturn
                {
                    PositionedToken = PositionedToken.Create(Token.VirtualSep, token.StartIndex)
                    Index = TokenIndex.Virtual
                }
                reader

    /// Parses a sequence of expressions at the same base indentation as the first element,
    /// combining them into right-associative Expr.Sequential chains with VirtualSep separators.
    /// Used inside begin/end blocks and function bodies in light syntax.
    let pSeqBlock pElem =
        parser {
            let! baseIndent = peekNonTriviaIndent
            let! first = pElem

            let! rest =
                many (
                    parser {
                        let! nextIndent = peekNonTriviaIndent

                        if nextIndent = baseIndent then
                            let! sep = makeVirtualSep
                            let! elem = pElem
                            return (sep, elem)
                        else
                            return! fail (Message "Not at base indent")
                    }
                )

            return
                (first, rest)
                ||> Seq.fold (fun acc (sep, elem) -> Expr.Sequential(acc, sep, elem))
        }
