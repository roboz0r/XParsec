namespace XParsec.FSharp.Parser

open System
open System.Collections.Generic
open System.Collections.Immutable
open XParsec
open XParsec.Parsers
open XParsec.FSharp
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser.SyntaxToken
open XParsec.FSharp.Parser.ParseState

[<AutoOpen>]
module Parsing =
    let tokenIndex (reader: Reader<PositionedToken, ParseState, _>) = reader.Index * 1<token>

    /// Creates a PositionedToken with the IsVirtual flag set, for synthesized tokens
    /// (virtual delimiters, recovery placeholders, etc.).
    let mkVirtualPT (tok: Token) (startIndex: int) =
        PositionedToken.Create(Token.ofUInt16 (uint16 tok ||| TokenRepresentation.IsVirtual), startIndex)

    /// Scans forward past #if branch content, respecting nested #if/#endif pairs.
    /// Stops (after consuming the stop token) at either:
    ///   * matching #endif at depth 0 — always stops here,
    ///   * matching #else at depth 0 — only when `stopOnElse` is true.
    /// Returns true if the stop was a matching #else. Gracefully stops on EOF.
    let private skipConditionalBranch (stopOnElse: bool) (reader: Reader<PositionedToken, ParseState, _>) =
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
                | Token.ElseDirective when stopOnElse && depth = 0 ->
                    // Matching #else found: consume it and stop; else-branch is now active
                    reader.Skip()
                    foundElse <- true
                    stop <- true
                | _ -> reader.Skip()

        foundElse

    /// Scans forward past tokens in an inactive branch until reaching #else or #endif at depth 0.
    /// Nested #if/#endif pairs are depth-tracked and correctly skipped.
    /// The stop token (#else or #endif) is consumed before returning.
    /// Returns true if stopped at #else (an else-branch follows), false if stopped at #endif.
    /// Directives inside block comments (with the InComment flag) are correctly ignored.
    let skipInactiveBranch reader = skipConditionalBranch true reader

    /// Scans forward past an active else-branch until the matching #endif at depth 0 is consumed.
    /// Must be called after the #else token itself has already been consumed.
    let skipElseBranch reader =
        skipConditionalBranch false reader |> ignore

    /// Processes a #if directive: parses the condition expression, evaluates it against
    /// the current defined symbols, and either continues into the active branch or skips
    /// to the matching #else/#endif.
    /// Must be called with the reader positioned at the #if token (not yet consumed).
    let processIfDirective
        (nextNonTriviaToken: Parser<_, _, _, _>)
        (ifToken: PositionedToken)
        (reader: Reader<PositionedToken, ParseState, _>)
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

            reader.State <- addErrorDiagnostic (DiagnosticCode.Other msg) ifToken reader.State

            skipInactiveBranch reader |> ignore
            nextNonTriviaToken reader

    let processWarnDirective
        (nextNonTriviaToken: Parser<_, _, _, _>)
        (isSuppress: bool)
        (_directiveToken: PositionedToken)
        (reader: Reader<PositionedToken, ParseState, _>)
        =
        let state = reader.State
        let lexed = state.Lexed
        let currentLine = findLineNumber state (tokenIndex reader)
        let nextLine = currentLine + 1<_>

        let nextLineTokenIndex =
            if nextLine < lexed.LineStarts.LengthM then
                lexed.LineStarts[nextLine]
            else
                lexed.Tokens.LengthM - 1<_>

        // Skip the directive token itself
        reader.Skip()

        let mutable warnDirectives = state.WarnDirectives

        // Scan remaining tokens on this line for warning codes
        while reader.Index < int nextLineTokenIndex do
            match reader.Peek() with
            | ValueNone -> reader.Index <- int nextLineTokenIndex
            | ValueSome token when isTriviaToken state token -> reader.Skip()
            | ValueSome token ->
                let tIdx = tokenIndex reader
                let text = lexed.GetTokenString(tIdx, state.Input)
                reader.Skip()

                match token.Token with
                | Token.NumInt32 ->
                    match System.Int32.TryParse(text) with
                    | true, n ->
                        warnDirectives <-
                            {
                                Line = currentLine
                                WarningNumber = n
                                Suppress = isSuppress
                            }
                            :: warnDirectives
                    | _ -> ()
                | Token.StringOpen ->
                    // String is now fragmented: StringOpen, StringFragment*, StringClose
                    // Look for a single StringFragment containing the warning number
                    if reader.Index < int nextLineTokenIndex then
                        match reader.Peek() with
                        | ValueSome fragToken when fragToken.Token = Token.StringFragment ->
                            let fragIdx = tokenIndex reader
                            let fragText = lexed.GetTokenString(fragIdx, state.Input)
                            reader.Skip()

                            match System.Int32.TryParse(fragText) with
                            | true, n ->
                                warnDirectives <-
                                    {
                                        Line = currentLine
                                        WarningNumber = n
                                        Suppress = isSuppress
                                    }
                                    :: warnDirectives
                            | _ -> ()
                        | _ -> ()
                | _ -> ()

        // Ensure we're past the directive line
        reader.Index <- int nextLineTokenIndex

        reader.State <-
            { reader.State with
                WarnDirectives = warnDirectives
            }

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
            // 15.1.9: with/finally/| may align with try
            | OffsideContext.Try, (Token.KWWith | Token.KWFinally | Token.OpBar) -> true
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
            // 15.1.9: with/| may align with match
            | OffsideContext.Match, (Token.KWWith | Token.OpBar) -> true
            // 15.1.9: | may align with function
            | OffsideContext.Function, Token.OpBar -> true
            // 15.1.9: done may align with while
            | OffsideContext.While, Token.KWDone -> true
            | _ -> false
        )

    /// Returns true if the token is a closing delimiter that matches the given paren-like context.
    /// Closing delimiters are never offside from their matching opening context (F# spec 15.1.8).
    let private isMatchingClose (token: Token) (ctx: OffsideContext) =
        match ctx, token with
        | OffsideContext.Paren, Token.KWRParen -> true
        | OffsideContext.Bracket, Token.KWRBracket -> true
        | OffsideContext.BracketBar, Token.KWRArrayBracket -> true
        | OffsideContext.BraceBar, Token.KWRBraceBar -> true
        | OffsideContext.Brace, Token.KWRBrace -> true
        | OffsideContext.Begin, Token.KWEnd -> true
        | OffsideContext.Quote, (Token.OpQuotationTypedRight | Token.OpQuotationUntypedRight) -> true
        | _ -> false

    /// Like List.exists contextPermitsToken, but for OpBar tokens, stops at
    /// MatchClauses boundaries to prevent inner matches from consuming outer bars.
    let rec private contextPermitsTokenBounded (token: Token) (tokenCol: int) (stack: Offside list) =
        match stack with
        | [] -> false
        | ctx :: rest ->
            if contextPermitsToken token tokenCol ctx then
                true
            elif token = Token.OpBar && ctx.Context = OffsideContext.MatchClauses then
                false
            else
                contextPermitsTokenBounded token tokenCol rest

    /// Paren-like contexts per F# spec §15.1.10.4 — delimiters and begin/end.
    /// These sit on the context stack as markers (Indent = 0), not as offside lines.
    let private isParenLike (ctx: OffsideContext) =
        match ctx with
        | OffsideContext.Paren
        | OffsideContext.Bracket
        | OffsideContext.BracketBar
        | OffsideContext.BraceBar
        | OffsideContext.Brace
        | OffsideContext.Begin -> true
        | _ -> false

    // Walks the stack looking for any matching closing-delimiter context.
    // Hoisted out of isPermittedUndentation so no per-call closure is allocated.
    let rec private anyMatchingClose (token: Token) (stack: Offside list) =
        match stack with
        | [] -> false
        | ctx :: rest ->
            if isMatchingClose token ctx.Context then
                true
            else
                anyMatchingClose token rest

    // Fun/Function-body undentation: walk past SeqBlock+Paren-like+Fun/Function frames
    // to find the true enclosing offside line (F# spec §15.1.10.1).
    let rec private findFunBodyEnclosingIndent (tokenCol: int) (stack: Offside list) =
        match stack with
        | [] -> true // No other context to violate
        | ctx :: deeper ->
            match ctx.Context with
            | OffsideContext.SeqBlock
            | OffsideContext.Paren
            | OffsideContext.Bracket
            | OffsideContext.BracketBar
            | OffsideContext.BraceBar
            | OffsideContext.Brace
            | OffsideContext.Begin
            | OffsideContext.Fun
            | OffsideContext.Function -> findFunBodyEnclosingIndent tokenCol deeper
            | _ -> tokenCol >= ctx.Indent

    // MatchClauses-body undentation: same skip-past-containers rule, but also skips
    // Match and MatchClauses frames (F# spec §15.1.10.1 extended).
    let rec private findMatchBodyEnclosingIndent (tokenCol: int) (stack: Offside list) =
        match stack with
        | [] -> true
        | ctx :: deeper ->
            match ctx.Context with
            | OffsideContext.SeqBlock
            | OffsideContext.Paren
            | OffsideContext.Bracket
            | OffsideContext.BracketBar
            | OffsideContext.BraceBar
            | OffsideContext.Brace
            | OffsideContext.Begin
            | OffsideContext.Fun
            | OffsideContext.Function
            | OffsideContext.Match
            | OffsideContext.MatchClauses -> findMatchBodyEnclosingIndent tokenCol deeper
            | _ -> tokenCol >= ctx.Indent

    // Used by the Match/Function/Try aligned-token rule (15.1.10 extended).
    // Walks past Match/MatchClauses/Function/Try/SeqBlock frames looking for an
    // enclosing paren-like frame.
    let rec private hasEnclosingParenAroundMatch (stack: Offside list) =
        match stack with
        | [] -> false
        | ctx :: deeper ->
            match ctx.Context with
            | OffsideContext.SeqBlock
            | OffsideContext.Match
            | OffsideContext.MatchClauses
            | OffsideContext.Function
            | OffsideContext.Try -> hasEnclosingParenAroundMatch deeper
            | c when isParenLike c -> true
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
            elif anyMatchingClose token rest then
                ValueSome "15.1.8 MatchingClose"

            // --- 15.1.9: Exceptions to Offside Rules ---

            // SeqBlock infix: an infix token may be offside by (tokenSize + 1)
            elif head.Context = OffsideContext.SeqBlock && isInfixToken token then
                let tokenLength = getTokenLength state (int readerIndex * 1<token>)

                if tokenCol >= head.Indent - (tokenLength + 1) then
                    ValueSome "15.1.9 InfixUndent"
                else if
                    // Still check deeper contexts, but don't let bars pass through MatchClauses
                    contextPermitsTokenBounded token tokenCol rest
                then
                    ValueSome "15.1.9 ContextPermits"
                else
                    // 15.1.10.4 fallback: infix undentation exceeded, but if the SeqBlock
                    // sits directly inside a paren-like context, delegate to the collection
                    // undentation rule.
                    tryCollectionUndent tokenCol context

            // Check if the token is permitted at the head context or any enclosing context
            elif contextPermitsToken token tokenCol head then
                ValueSome "15.1.9 ContextPermits"

            elif contextPermitsTokenBounded token tokenCol rest then
                ValueSome "15.1.9 ContextPermits"

            // --- 15.1.10: Permitted Undentations ---

            // 15.1.10.1: Fun/Function body undentation
            // The body may undent from fun/function but not past other offside lines.
            // "Constructs enclosed in brackets may be undented" — so we skip past
            // SeqBlock+Paren pairs to find the true enclosing offside line.
            elif head.Context = OffsideContext.Fun || head.Context = OffsideContext.Function then
                if findFunBodyEnclosingIndent tokenCol rest then
                    ValueSome "15.1.10.1 FunBody"
                else
                    ValueNone

            // 15.1.10.1 extended: MatchClauses body undentation
            // MatchClauses governs only `|` bar alignment; non-`|` tokens inside
            // a rule (guard continuations, ->, etc.) should be bounded by the
            // enclosing Function/Match context, not the pattern column. Apply the
            // same skip-past-containers rule as FunBody.
            elif head.Context = OffsideContext.MatchClauses && token <> Token.OpBar then
                if findMatchBodyEnclosingIndent tokenCol rest then
                    ValueSome "15.1.10.1 MatchBody"
                else
                    ValueNone

            // 15.1.10.2 (if/then/else + paren/begin undentation) and 15.1.10.3 (module/class
            // body undentation inside begin/end) are intentionally omitted. Both spec rules
            // exist because F#'s Lexical Filtering step retrofits offside onto a token stream
            // after lexing, requiring special cases for paren-like frames. XParsec.FSharp is
            // offside-aware by construction: Paren and Begin are pushed (by pEnclosed and
            // withContextAt) with Indent=0 as pure stack markers, so they can never be the
            // head context in an offside check (tokenCol < 0 is impossible). Content inside
            // `(...)` or `begin...end` is bounded by the SeqBlock inside pInner, which is
            // handled by the SeqBlockParen arm of tryCollectionUndent below.

            // 15.1.10.4: Collection/CE undentation for Bracket, BracketBar, Brace contexts
            elif
                head.Context = OffsideContext.Bracket
                || head.Context = OffsideContext.BracketBar
                || head.Context = OffsideContext.BraceBar
                || head.Context = OffsideContext.Brace
            then
                tryCollectionUndent tokenCol context

            // 15.1.10.4 extended: SeqBlock inside paren-like context
            // withContext pushes a SeqBlock on top of the Paren context from pEnclosed.
            // When content undents past that SeqBlock but is still within the enclosing
            // expression's offside line, delegate to the collection undentation rule.
            elif head.Context = OffsideContext.SeqBlock then
                tryCollectionUndent tokenCol context

            // 15.1.10 extended: Match/Function/Try aligned tokens (with/|/finally)
            // may undent when the expression is enclosed in brackets, to the
            // enclosing expression's offside line. Mirrors SeqBlockParen above.
            elif
                (head.Context = OffsideContext.Match
                 || head.Context = OffsideContext.Function
                 || head.Context = OffsideContext.Try)
                && (token = Token.OpBar
                    || (token = Token.KWWith
                        && (head.Context = OffsideContext.Match || head.Context = OffsideContext.Try))
                    || (token = Token.KWFinally && head.Context = OffsideContext.Try))
            then
                if hasEnclosingParenAroundMatch rest && checkCollectionUndent tokenCol rest then
                    ValueSome "15.1.10 MatchParen"
                else
                    ValueNone

            else
                ValueNone

    /// Walk the context stack skipping SeqBlock+Paren pairs to find the enclosing
    /// expression's offside line for collection/CE undentation (F# spec 15.1.10.4).
    and private checkCollectionUndent (tokenCol: int) (stack: Offside list) : bool =
        match stack with
        | [] -> true // Walked past all paren-like/SeqBlock contexts; no enclosing offside line to violate
        | ctx :: deeper ->
            match (ctx: Offside).Context with
            | OffsideContext.SeqBlock
            | OffsideContext.Fun
            | OffsideContext.Function -> checkCollectionUndent tokenCol deeper
            | c when isParenLike c -> checkCollectionUndent tokenCol deeper
            | _ ->
                // Found the enclosing non-paren context; check if token is within its indent
                tokenCol >= ctx.Indent

    /// F# spec §15.1.10.4 (collection/CE undentation) and its SeqBlockParen extension:
    /// a token may undent from the head context when it still lies within the enclosing
    /// expression's offside line. Returns the trace-rule name on success.
    /// Expects `stack` to be the full context list; inspects the head:
    ///   * `SeqBlock :: paren-like :: deeper` — SeqBlock pushed on top of a paren-like
    ///     context by `withContext` after `pEnclosed`. Rule: "15.1.10.4 SeqBlockParen".
    ///   * `paren-like :: rest` — head is itself the paren-like container. Rule:
    ///     "15.1.10.4 Collection".
    and private tryCollectionUndent (tokenCol: int) (stack: Offside list) : string voption =
        match stack with
        | { Context = OffsideContext.SeqBlock } :: { Context = ctx } :: deeper when isParenLike ctx ->
            if checkCollectionUndent tokenCol deeper then
                ValueSome "15.1.10.4 SeqBlockParen"
            else
                ValueNone
        | { Context = ctx } :: rest when isParenLike ctx ->
            if checkCollectionUndent tokenCol rest then
                ValueSome "15.1.10.4 Collection"
            else
                ValueNone
        | _ -> ValueNone

    let private errOffside: ErrorType<PositionedToken, ParseState> = Message "Offside"

    let rec private nextNonTriviaTokenImpl isPeek (reader: Reader<PositionedToken, ParseState, _>) =
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
        | ValueSome token when token.Token = Token.NoWarnDirective ->
            processWarnDirective (nextNonTriviaTokenImpl isPeek) true token reader
        | ValueSome token when token.Token = Token.WarnOnDirective ->
            processWarnDirective (nextNonTriviaTokenImpl isPeek) false token reader
        | ValueSome token when isTriviaToken reader.State token ->
            reader.Skip()
            nextNonTriviaTokenImpl isPeek reader
        | ValueSome token ->
            // SplitRAttrBracket: when the measure parser consumed the `>` half of `>]`,
            // it sets this flag so the remaining `]` half is presented as KWRBracket.
            // SplitPowerMinus: when the measure parser consumed the `^` half of a fused
            // `^-N` operator, it sets this flag so the remaining `-` half is presented
            // as OpSubtraction at StartIndex+1 (the numeric `N` follows as its own token).
            let token =
                if reader.State.SplitRAttrBracket && token.Token = Token.KWRAttrBracket then
                    reader.State.Trace.SplitRAttrBracketConsumed(token.StartIndex)
                    PositionedToken.Create(Token.KWRBracket, token.StartIndex + 1)
                elif reader.State.SplitPowerMinus then
                    let span =
                        reader.State.Lexed.GetTokenSpan(reader.Index * 1<token>, reader.State.Input)

                    if span.Length >= 2 && span.[0] = '^' && span.[1] = '-' then
                        reader.State.Trace.SplitPowerMinusConsumed(token.StartIndex)
                        PositionedToken.Create(Token.OpSubtraction, token.StartIndex + 1)
                    else
                        token
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
                                reader.State.Trace.PermittedUndentation(token, tokenCol, contextIndent, rule)
                                false
                            | ValueNone ->
                                reader.State.Trace.OffsideFail(token, tokenCol, contextIndent, ctx)
                                true
                        else
                            reader.State.Trace.OffsideOk(token, tokenCol, contextIndent, ctx)
                            false
                    | [] -> false
                )

            if isOffside then
                fail errOffside reader
            else
                let t = syntaxToken token reader.Index

                if isPeek then
                    let col = ParseState.getIndent reader.State (reader.Index * 1<token>)
                    reader.State.Trace.TokenPeeked(token, int reader.Index, col)
                    preturn t reader
                else
                    let col = ParseState.getIndent reader.State (reader.Index * 1<token>)
                    reader.State.Trace.TokenConsumed(token, int reader.Index, col)

                    if reader.State.SplitRAttrBracket then
                        reader.State <-
                            { reader.State with
                                SplitRAttrBracket = false
                            }

                    if reader.State.SplitPowerMinus then
                        reader.State <-
                            { reader.State with
                                SplitPowerMinus = false
                            }

                    reader.Skip()
                    preturn t reader

    let nextNonTriviaToken (reader: Reader<PositionedToken, ParseState, _>) = nextNonTriviaTokenImpl false reader

    /// Advanced the reader to the next non-trivia token and returns it without consuming it.
    /// Allows parser to avoid re-skipping trivia tokens when it needs to look ahead at the next token to decide what to parse.
    let peekNextNonTriviaToken (reader: Reader<PositionedToken, ParseState, _>) = nextNonTriviaTokenImpl true reader

    /// Emits a trace message. Use with `do!` inside a `parser { }` CE for debugging.
    let trace (msg: string) (reader: Reader<PositionedToken, ParseState, _>) =
        reader.State.Trace.Message msg
        preturn () reader

    /// Consumes the given token, which must have been previously returned by `peekNextNotTriviaToken`, and returns it.
    let consumePeeked (token: SyntaxToken) (reader: Reader<PositionedToken, ParseState, _>) =
        match token.Index with
        | TokenIndex.Virtual -> invalidOp "Cannot consume a virtual token"
        | TokenIndex.Regular tokenIdx ->
            assert (reader.Index = tokenIdx * 1< / token>) // Ensure the reader is still at the expected position
            reader.Index <- (tokenIdx + 1<token>) * 1< / token>
            let col = ParseState.getIndent reader.State tokenIdx
            reader.State.Trace.TokenConsumed(token.PositionedToken, int tokenIdx, col)
            preturn token reader

    /// Peeks the next non-trivia token, asserts it matches the expected token,
    /// consumes it, and returns the token together with its column indent.
    /// Used by keyword expression parsers to capture the keyword's indent for offside context.
    let assertKeywordToken (expected: Token) (reader: Reader<PositionedToken, ParseState, _>) =
        match peekNextNonTriviaToken reader with
        | Ok t when t.Token = expected ->
            (consumePeeked t
             |>> fun kwTok ->
                 let indent =
                     match kwTok.Index with
                     | TokenIndex.Regular iT -> ParseState.getIndent reader.State iT
                     | TokenIndex.Virtual -> invalidOp $"Virtual tokens should not be used for '{expected}' keyword"

                 struct (kwTok, indent))
                reader
        | Ok t -> fail (Message $"Expected '{expected}' keyword") reader
        | Error e -> Error e

    let assertKeywordTokens (expected1: Token) (expected2: Token) (reader: Reader<PositionedToken, ParseState, _>) =
        match peekNextNonTriviaToken reader with
        | Ok t when t.Token = expected1 || t.Token = expected2 ->
            (consumePeeked t
             |>> fun kwTok ->
                 let indent =
                     match kwTok.Index with
                     | TokenIndex.Regular iT -> ParseState.getIndent reader.State iT
                     | TokenIndex.Virtual ->
                         invalidOp $"Virtual tokens should not be used for '{expected1}'|'{expected2}' keyword"

                 struct (kwTok, indent))
                reader
        | Ok _ -> fail (Message $"Expected '{expected1}' or '{expected2}' keyword") reader
        | Error e -> Error e

    /// Core of the "match-token-or-synthesise-virtual" pattern. If the next
    /// non-trivia token matches `t`, consume and return it. Otherwise produce a
    /// virtual token of kind `t` without consuming, optionally emitting an error
    /// diagnostic built by `mkDiag` from the token at the failure site.
    let private nextNonTriviaTokenVirtualCore
        (mkDiag: PositionedToken -> DiagnosticCode voption)
        t
        (reader: Reader<PositionedToken, ParseState, _>)
        =
        match peekNextNonTriviaToken reader with
        | Ok token when token.Token = t ->
            // Real token matches: consume it and return it.
            consumePeeked token reader
        | result ->
            // Real token doesn't match (Ok with different token) or offside failure (Error):
            // optionally emit a diagnostic and produce a virtual substitute without consuming.
            let startIndex, diagToken =
                match result with
                | Ok token -> token.StartIndex, token.PositionedToken
                | Error _ ->
                    match reader.Peek() with
                    | ValueSome tok -> tok.StartIndex, tok
                    | ValueNone -> 0, PositionedToken.Create(Token.EOF, 0)

            match mkDiag diagToken with
            | ValueSome code -> reader.State <- ParseState.addErrorDiagnostic code diagToken reader.State
            | ValueNone -> ()

            let pt = mkVirtualPT t startIndex

            reader.State.Trace.VirtualToken(pt.Token, pt.StartIndex)

            preturn
                {
                    PositionedToken = pt
                    Index = TokenIndex.Virtual
                }
                reader

    let nextNonTriviaTokenVirtualIfNot t reader =
        nextNonTriviaTokenVirtualCore (fun _ -> ValueNone) t reader

    /// Primary API — takes a pre-built err. Callers are expected to build the `ErrorType.Message`
    /// statically at their module level so it allocates exactly once, not per failure.
    let nextNonTriviaTokenSatisfiesL (predicate: SyntaxToken -> bool) (err: ErrorType<PositionedToken, ParseState>) =
        fun reader ->
            match peekNextNonTriviaToken reader with
            | Error e -> Error e
            | Ok token ->
                if predicate token then
                    consumePeeked token reader
                else
                    fail err reader

    let nextNonTriviaTokenIsL (t: Token) (err: ErrorType<PositionedToken, ParseState>) =
        nextNonTriviaTokenSatisfiesL (fun synTok -> synTok.Token = t) err

    /// Convenience wrapper. Builds `Message msg` and delegates. Use this ONLY when the caller
    /// binding is itself at module level (so the Message is allocated once per binding, not
    /// per runtime invocation); inline uses inside a `parser { }` CE will re-allocate the
    /// Message on every outer call.
    let inline nextNonTriviaTokenSatisfiesLMsg (predicate: SyntaxToken -> bool) (msg: string) =
        nextNonTriviaTokenSatisfiesL predicate (Message msg)

    let inline nextNonTriviaTokenIsLMsg (t: Token) (msg: string) = nextNonTriviaTokenIsL t (Message msg)

    let isPlainStringOpen (tok: Token) =
        match tok with
        | Token.StringOpen
        | Token.VerbatimStringOpen
        | Token.String3Open -> true
        | _ -> false

    let isPlainStringClose (tok: Token) =
        match tok with
        | Token.StringClose
        | Token.ByteArrayClose
        | Token.VerbatimStringClose
        | Token.VerbatimByteArrayClose
        | Token.String3Close
        | Token.UnterminatedStringLiteral
        | Token.UnterminatedVerbatimStringLiteral
        | Token.UnterminatedString3Literal -> true
        | _ -> false

    let isPlainStringFragment (tok: Token) =
        match tok with
        | Token.StringFragment
        | Token.EscapeSequence
        | Token.EscapePercent
        | Token.VerbatimEscapeQuote
        | Token.FormatPlaceholder -> true
        | _ -> false

    let plainStringKindOfToken (t: SyntaxToken) =
        match t.Token with
        | Token.StringOpen -> StringKind.String t
        | Token.VerbatimStringOpen -> StringKind.VerbatimString t
        | Token.String3Open -> StringKind.String3 t
        | _ -> invalidOp $"Not a plain string open token: {t.Token}"

    let plainStringPartOfToken (t: SyntaxToken) =
        match t.Token with
        | Token.StringFragment -> StringPart.Text t
        | Token.EscapeSequence -> StringPart.EscapeSequence t
        | Token.EscapePercent -> StringPart.EscapePercent t
        | Token.VerbatimEscapeQuote -> StringPart.VerbatimEscapeQuote t
        | Token.FormatPlaceholder -> StringPart.FormatSpecifier t
        | _ -> invalidOp $"Not a string fragment token: {t.Token}"

    /// Parses a plain (non-interpolated) string literal into StringKind * StringPart list * closing token.
    let parsePlainStringLiteral msg (reader: Reader<PositionedToken, ParseState, _>) =
        match peekNextNonTriviaToken reader with
        | Error e -> Error e
        | Ok token when isPlainStringOpen token.Token ->
            match consumePeeked token reader with
            | Error e -> Error e
            | Ok opening ->
                let kind = plainStringKindOfToken opening
                let parts = ResizeArray()
                let mutable closing = Unchecked.defaultof<SyntaxToken>
                let mutable finished = false
                let mutable error = ValueNone

                while not finished do
                    match peekNextNonTriviaToken reader with
                    | Error e ->
                        error <- ValueSome e
                        finished <- true
                    | Ok t when isPlainStringFragment t.Token ->
                        match consumePeeked t reader with
                        | Error e ->
                            error <- ValueSome e
                            finished <- true
                        | Ok frag -> parts.Add(plainStringPartOfToken frag)
                    | Ok t when isPlainStringClose t.Token ->
                        match consumePeeked t reader with
                        | Error e ->
                            error <- ValueSome e
                            finished <- true
                        | Ok close ->
                            closing <- close
                            finished <- true
                    | Ok _ -> finished <- true

                match error with
                | ValueSome e -> Error e
                | ValueNone -> preturn (kind, parts.ToImmutableArray(), closing) reader
        | _ -> fail (Message msg) reader

    /// Matches Token.Identifier, Token.BacktickedIdentifier, or Token.UnterminatedBacktickedIdentifier.
    /// Emits a diagnostic for unterminated backticked identifiers.
    /// Primary API — takes a pre-built err; callers should build Message statically at module level.
    let nextNonTriviaIdentifierL (err: ErrorType<PositionedToken, ParseState>) =
        fun (reader: Reader<PositionedToken, ParseState, _>) ->
            match peekNextNonTriviaToken reader with
            | Error e -> Error e
            | Ok token ->
                match token.Token with
                | Token.Identifier
                | Token.BacktickedIdentifier -> consumePeeked token reader
                | Token.UnterminatedBacktickedIdentifier ->
                    reader.State <-
                        ParseState.addErrorDiagnostic
                            (DiagnosticCode.Other "Unterminated backticked identifier")
                            token.PositionedToken
                            reader.State

                    consumePeeked token reader
                | _ -> fail err reader

    let inline nextNonTriviaIdentifierLMsg (msg: string) = nextNonTriviaIdentifierL (Message msg)

    let dispatchNextNonTriviaTokenFallback (routes: (Token * Parser<_, _, _, _>) list) pFallback =
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

    let dispatchNextNonTriviaTokenL (routes: (Token * Parser<_, _, _, _>) list) fallbackMsg =
        dispatchNextNonTriviaTokenFallback routes (fail (Message fallbackMsg))

    /// Checks that the raw token immediately before the current reader position is not trivia.
    /// Used by adjacency-based parsers (high-precedence application, type application, measures)
    /// to confirm the upcoming token is truly adjacent to the preceding expression token,
    /// not just adjacent because peekNextNonTriviaToken consumed intervening trivia.
    let isPrevTokenNonTrivia (reader: Reader<PositionedToken, ParseState, _>) =
        let idx = reader.Index
        idx > 0 && not (ParseState.isTriviaToken reader.State reader.Input[idx - 1])

    let currentIndent (reader: Reader<PositionedToken, ParseState, 'a>) =
        let state = reader.State
        let index = int reader.Index * 1<token>
        let indent = ParseState.getIndent state index
        preturn indent reader

    /// Returns the column (0-based) of the next non-trivia token without consuming it.
    /// Returns -1 at EOF.
    let peekNonTriviaIndent: Parser<int, PositionedToken, ParseState, ReadableImmutableArray<_>> =
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
    let makeVirtualSep (reader: Reader<PositionedToken, ParseState, _>) =
        match reader.Peek() with
        | ValueNone -> fail EndOfInput reader
        | ValueSome token ->
            reader.State.Trace.VirtualToken(Token.VirtualSep, token.StartIndex)

            preturn
                {
                    PositionedToken = PositionedToken.Create(Token.VirtualSep, token.StartIndex)
                    Index = TokenIndex.Virtual
                }
                reader

    /// Like nextNonTriviaTokenVirtualIfNot but emits a UnclosedDelimiter diagnostic
    /// when the token must be synthesised.
    let nextNonTriviaTokenVirtualWithDiagnostic (openTok: SyntaxToken voption) t reader =
        let mkDiag _ =
            let code =
                match openTok with
                | ValueSome o -> DiagnosticCode.UnclosedDelimiter(o, t)
                | ValueNone -> DiagnosticCode.Other $"Expected '{t}'"

            ValueSome code

        nextNonTriviaTokenVirtualCore mkDiag t reader

    /// Wraps a parser so that on failure, it emits a diagnostic and returns a virtual token
    /// of the given type. Used for committed-keyword recovery where the parser must not fail
    /// after a keyword has been consumed.
    let recoverWithVirtualToken
        (expectedToken: Token)
        (diagMsg: string)
        (p: Parser<SyntaxToken, PositionedToken, ParseState, _>)
        : Parser<SyntaxToken, PositionedToken, ParseState, _> =
        fun reader ->
            match p reader with
            | Ok result -> Ok result
            | Error _ ->
                match peekNextNonTriviaToken reader with
                | Error e -> Error e
                | Ok token ->
                    reader.State <-
                        ParseState.addErrorDiagnostic (DiagnosticCode.Other diagMsg) token.PositionedToken reader.State

                    let pt = mkVirtualPT expectedToken token.StartIndex

                    reader.State.Trace.VirtualToken(pt.Token, pt.StartIndex)

                    preturn
                        {
                            PositionedToken = pt
                            Index = TokenIndex.Virtual
                        }
                        reader

    /// Wraps a LongIdent parser so that on failure, it emits a diagnostic and returns a
    /// single-element virtual identifier. Used after committed keywords like `open`, `namespace`, `module`.
    let recoverLongIdent
        (diagMsg: string)
        (p: Parser<LongIdent<SyntaxToken>, PositionedToken, ParseState, _>)
        : Parser<LongIdent<SyntaxToken>, PositionedToken, ParseState, _> =
        fun reader ->
            match p reader with
            | Ok result -> Ok result
            | Error _ ->
                match peekNextNonTriviaToken reader with
                | Error e -> Error e
                | Ok token ->
                    reader.State <-
                        ParseState.addErrorDiagnostic (DiagnosticCode.Other diagMsg) token.PositionedToken reader.State

                    let pt = mkVirtualPT Token.Identifier token.StartIndex

                    reader.State.Trace.VirtualToken(pt.Token, pt.StartIndex)

                    let virtualIdent: SyntaxToken =
                        {
                            PositionedToken = pt
                            Index = TokenIndex.Virtual
                        }

                    preturn (ImmutableArray.Create(virtualIdent): LongIdent<SyntaxToken>) reader

    module StoppingTokens =
        let afterType (tok: SyntaxToken) =
            match tok.Token with
            | Token.OpComma
            | Token.KWRParen
            | Token.KWRBracket
            | Token.KWRBrace
            | Token.KWRBraceBar
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
            | Token.KWRBraceBar
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
            | Token.KWRBraceBar
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

        let afterModuleElem (tok: SyntaxToken) =
            match tok.Token with
            // Tokens that can start a new module element
            | Token.KWLet
            | Token.KWDo
            | Token.KWOpen
            | Token.KWType
            | Token.KWModule
            | Token.KWNamespace
            | Token.KWException
            | Token.KWHash
            | Token.KWLAttrBracket // [< starts attributes which precede module elements
            // Closing/structural tokens that should not be consumed
            | Token.KWEnd
            | Token.KWWith
            | Token.KWIn
            | Token.EOF -> true
            | _ -> false

    /// On failure: emits a diagnostic with the given code and the underlying ParseError,
    /// skips tokens until `stopping` returns true,
    /// then succeeds with `placeholder skippedTokens`.
    let recoverWith
        (stopping: SyntaxToken -> bool)
        (severity: DiagnosticSeverity)
        (code: DiagnosticCode)
        (placeholder: ImArr<SyntaxToken> -> 'Parsed)
        (p: Parser<'Parsed, PositionedToken, ParseState, _>)
        : Parser<'Parsed, PositionedToken, ParseState, _> =
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

                    reader.State <-
                        ParseState.addDiagnostic code severity startTok.PositionedToken None (Some err) reader.State

                    Ok(placeholder (skipped.ToImmutableArray()))

    /// Wraps a parser with an offside context. Peeks the first token the inner parser will see
    /// to establish the offside column, pushes an Offside entry onto ParseState.Context, runs
    /// the inner parser, then pops the context on success or restores the full saved state on
    /// failure (keeping the operation safe for backtracking).
    let withContext (ctx: OffsideContext) innerParser (reader: Reader<PositionedToken, ParseState, _>) =
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

            match innerParser reader with
            | Ok result ->
                reader.State <- ParseState.popOffside entry reader.State
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
        (reader: Reader<PositionedToken, ParseState, _>)
        =
        let savedState = reader.State

        let entry =
            {
                Context = ctx
                Indent = indent
                Token = token
            }

        reader.State <- ParseState.pushOffside entry reader.State

        match innerParser reader with
        | Ok result ->
            reader.State <- ParseState.popOffside entry reader.State
            Ok result
        | Error _ as e ->
            reader.State <- savedState
            e

    /// Record field separator: accepts a real ';' or emits a virtual separator when the next
    /// token is at the same indent as the enclosing SeqBlock context (spec §15.1.5: $sep insertion).
    let pRecordFieldSep: Parser<SyntaxToken, PositionedToken, ParseState, ReadableImmutableArray<_>> =
        let failSep = fail (Message "Expected ';' or newline at the same indent")

        parser {
            match! peekNextNonTriviaToken with
            | t when t.Token = Token.OpSemicolon -> return! consumePeeked t
            | t when t.Token = Token.KWRBrace || t.Token = Token.KWRBraceBar -> return! failSep
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
    let notFollowedByNonTriviaToken t (reader: Reader<PositionedToken, ParseState, _>) =
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

    let private errExpectedGtCloseTypeApp: ErrorType<PositionedToken, ParseState> =
        Message "Expected '>' to close type application"

    let pCloseTypeParams: Parser<SyntaxToken, PositionedToken, ParseState, ReadableImmutableArray<_>> =
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
                        return! fail errExpectedGtCloseTypeApp
            | _ -> return! fail errExpectedGtCloseTypeApp
        }

    let private errExpectedOperatorAfterTypeParams: ErrorType<PositionedToken, ParseState> =
        Message "Expected operator after type parameters"

    let private errNoOperatorToReprocess: ErrorType<PositionedToken, ParseState> =
        Message "No operator to reprocess after type parameters"

    let reprocessedOperatorAfterTypeParams: Parser<SyntaxToken, PositionedToken, ParseState, ReadableImmutableArray<_>> =
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
                    | Error _ -> return invalidOp "Failed to re-lex operator after type parameters"
                | _ -> return! fail errExpectedOperatorAfterTypeParams
            else
                return! fail errNoOperatorToReprocess

        }


    let private errPEnclosedInnerFailed: ErrorType<PositionedToken, ParseState> =
        Message "pEnclosed inner parser failed"

    let pEnclosed
        completeEmpty
        completeEnclosed
        missing
        skipsTokens
        (pLeft: Parser<_, _, _, _>)
        (expectedRightTok: Token)
        (parenKindConstructor: SyntaxToken -> ParenKind<SyntaxToken>)
        (offsideCtx: OffsideContext)
        (diagCode: DiagnosticCode)
        (pInner: Parser<_, _, _, _>)
        : Parser<_, PositionedToken, ParseState, _> =

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

                let inline popAndReturn result =
                    reader.State <- ParseState.popOffside entry reader.State
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
                                if toks.IsEmpty then
                                    let endTok =
                                        virtualToken (PositionedToken.Create(expectedRightTok, l.StartIndex + 1))

                                    completeEnclosed (parenKindConstructor l) missing endTok
                                else
                                    let endTok =
                                        let t = toks[toks.Length - 1]
                                        virtualToken (PositionedToken.Create(expectedRightTok, t.StartIndex))

                                    completeEnclosed (parenKindConstructor l) (skipsTokens toks) endTok
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
                        fail errPEnclosedInnerFailed reader
