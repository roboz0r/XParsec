namespace XParsec.FSharp.Lexer

open System
open System.Globalization
open System.Collections.Generic
open System.Collections.Immutable
open XParsec
open XParsec.FSharp

[<Measure>]
type token

[<Measure>]
type line

type Lexed =
    {
        Tokens: ReadableArrayM<PositionedToken, token>
        LineStarts: ReadableArrayM<int<token>, line>
    }

    member this.FirstTokenOnLine(lineIndex: int<line>) =
        if lineIndex < 0<_> || int lineIndex >= this.LineStarts.Length then
            invalidArg (nameof lineIndex) "Index out of range"

        let tokenIndex = this.LineStarts[lineIndex]
        this.Tokens[tokenIndex]

    member this.GetLineForToken(i: int<token>) =
        if i < 0<_> || int i >= this.Tokens.Length then
            invalidArg (nameof i) "Index out of range"

        // Binary search for the line containing the token

        let rec loop (low: int<line>) (high: int<line>) =
            if low > high then
                invalidOp "Token index not found in any line"

            let mid = (low + high) / 2
            let lineStart = this.LineStarts[mid]

            if lineStart = i then
                mid
            elif lineStart < i then
                if mid + 1<_> < this.LineStarts.LengthM then
                    let nextLineStart = this.LineStarts[mid + 1<_>]

                    if nextLineStart > i then
                        mid
                    else
                        let low = mid + 1<_>
                        loop low high
                else
                    mid
            else
                let high = mid - 1<_>
                loop low high

        loop 0<_> (this.LineStarts.LengthM - 1<_>)

    member this.GetTokenRangeOnLine(lineIndex: int<line>) =
        if lineIndex < 0<_> || int lineIndex >= this.LineStarts.Length then
            invalidArg (nameof lineIndex) "Index out of range"

        let startTokenIndex = this.LineStarts[lineIndex]

        let endTokenIndex =
            if lineIndex + 1<_> < this.LineStarts.LengthM then
                this.LineStarts[lineIndex + 1<_>] - 1<_>
            else
                this.Tokens.LengthM - 1<_>

        startTokenIndex, endTokenIndex

    member this.GetTokenString(i: int<token>, input: string) =
        let tokens = this.Tokens

        if i < 0<_> || int i >= tokens.Length then
            invalidArg (nameof i) "Index out of range"

        let token = tokens[i]

        match token.Token with
        | Token.EOF -> ""
        | _ ->
            let t1 = tokens[i + 1<_>] // Next token is guaranteed to exist (EOF)
            input.[int token.StartIndex .. (t1.StartIndex - 1)]

    member this.GetTokenSpan(i: int<token>, input: string) =
        let tokens = this.Tokens

        if i < 0<_> || int i >= tokens.Length then
            invalidArg (nameof i) "Index out of range"

        let token = tokens[i]

        match token.Token with
        | Token.EOF -> ReadOnlySpan<char>()
        | _ ->
            let t1 = tokens[i + 1<_>] // Next token is guaranteed to exist (EOF)
            input.AsSpan().Slice(token.StartIndex, (t1.StartIndex - token.StartIndex))

// Format specifications for printf formats are strings with % markers
// that indicate format. Format placeholders consist of %[flags][width][.precision][type]

type InterpolationFlags =
    {
        // '0' '+' '-' ' '
        // TODO: It's an error to have duplicate flags but not really a problem for lexing
        Flags: string
        PadZeros: bool
        AlignLeft: bool
        PositiveChar: char option
    }

[<RequireQualifiedAccess>]
type FormatType =
    | Bool
    | String
    | Char
    | DecimalInt
    | UnsignedDecimalInt
    | UnsignedHex
    | UnsignedOctal
    | UnsignedBinary
    | FloatExponential
    | FloatDecimal
    | FloatCompact
    | Decimal
    | Object
    | Structured
    | FormatFunction
    | Text

type FormatPlaceholder =
    {
        Flags: string
        Width: bigint voption
        Precision: bigint voption
        Type: FormatType
    }

[<RequireQualifiedAccess; Struct>]
type LexContext =
    | Normal
    | InterpolatedString
    | VerbatimInterpolatedString
    | Interpolated3String of level: int
    | InterpolatedExpression
    | BracedExpression // used for computation expressions
    | BraceBarExpression // used for anonymous records {| ... |}
    | ParenthesExpression
    | BracketedExpression
    | QuotedExpression
    | TypedQuotedExpression
    /// #if context has different rules for identifiers and operators, so we track it with a separate context
    | IfDirective
    // Plain string contexts (fragment-based lexing)
    | PlainString
    | VerbatimString
    | TripleQuotedString

type LexBuilder =
    {
        Source: string
        Tokens: ReadableArrayBuilder<PositionedToken>
        mutable AtStartOfLine: bool
        Context: Stack<LexContext>
        // Track whether we are inside a block comment
        // These flags are used to mark tokens as being inside a block comment
        // The lexer doesn't care if we are in a block comment or not
        // but flagging tokens allows the parser or later stages to handle them easily
        mutable IsInBlockComment: bool
        mutable IsInOCamlBlockComment: bool
        mutable LastTokenWasNewLine: int<token> voption
        LineStarts: ReadableArrayBuilder<int<token>> // indices of tokens that start lines
    }

open XParsec
open XParsec.Parsers
open XParsec.CharParsers
open MoreParsers


[<RequireQualifiedAccess>]
[<Struct>]
type CtxOp =
    | Push of LexContext
    | Pop of LexContext
    | NoOp


module LexBuilder =

    // Empirical ratios across 340 test .fs files: ~3.8 chars per token and
    // ~27 chars per line. We pick slightly denser divisors (3 and 32) so the
    // initial builder arrays cover most inputs without a growth copy, then
    // round up to the next power of two for GC-friendly sizing. Both are pure
    // arithmetic — no O(n) scan of the source.
    let private roundUpToPowerOf2 (n: int) =
        if n <= 1 then
            1
        else
            int (System.Numerics.BitOperations.RoundUpToPowerOf2(uint n))

    let private estimateTokenCapacity (sourceLength: int) =
        // Minimum of 16 so tiny inputs still get a reasonable starting array.
        roundUpToPowerOf2 (max 16 (sourceLength / 3))

    let private estimateLineCapacity (sourceLength: int) =
        roundUpToPowerOf2 (max 16 (sourceLength / 32))

    let private emitUnterminatedStrings idx (state: LexBuilder) =
        // TODO: Handle other unclosed contexts (e.g. unterminated #if) — currently we just ignore them and let the parser handle any resulting errors
        // Stack enumerates top-to-bottom, matching the old head-first list traversal.
        for ctx in state.Context do
            match ctx with
            | LexContext.PlainString -> state.Tokens.Add(PositionedToken.Create(Token.UnterminatedStringLiteral, idx))
            | LexContext.VerbatimString ->
                state.Tokens.Add(PositionedToken.Create(Token.UnterminatedVerbatimStringLiteral, idx))
            | LexContext.TripleQuotedString ->
                state.Tokens.Add(PositionedToken.Create(Token.UnterminatedString3Literal, idx))
            | LexContext.InterpolatedString
            | LexContext.VerbatimInterpolatedString
            | LexContext.Interpolated3String _ ->
                state.Tokens.Add(PositionedToken.Create(Token.UnterminatedInterpolatedString, idx))
            | _ ->
                // InterpolatedExpression, BracedExpression, ParenthesExpression, etc.
                // are expression contexts nested inside an interpolated string — skip past them
                ()

    let complete idx (state: LexBuilder) =
        emitUnterminatedStrings idx state
        state.Tokens.Add(PositionedToken.Create(Token.EOF, idx))
        let tokens = ReadableArrayM(state.Tokens.ToReadableArray())

        let lineStarts =
            match state.LastTokenWasNewLine with
            | ValueSome newlineIdx ->
                state.LineStarts.Add(newlineIdx + 1<_>)
                state.LastTokenWasNewLine <- ValueNone
            | ValueNone -> ()

            ReadableArrayM(state.LineStarts.ToReadableArray())

        {
            Tokens = tokens
            LineStarts = lineStarts
        }

    let init (input: string) =
        let tokenCapacity = estimateTokenCapacity input.Length
        let lineCapacity = estimateLineCapacity input.Length

        let x =
            {
                Source = input
                Tokens = ReadableArrayBuilder(tokenCapacity)
                AtStartOfLine = true
                Context = Stack<LexContext>()
                IsInBlockComment = false
                IsInOCamlBlockComment = false
                LastTokenWasNewLine = ValueNone
                LineStarts = ReadableArrayBuilder(lineCapacity)
            }

        x.LineStarts.Add(0<_>) // The first line starts at the beginning of the file
        x

    let currentContext (x: LexBuilder) =
        if x.Context.Count = 0 then
            LexContext.Normal
        else
            x.Context.Peek()

    let pushContext (ctx: LexContext) (x: LexBuilder) =
        x.Context.Push(ctx)
        x

    let popExactContext expected (x: LexBuilder) =
        // Only pop if the expected context is on top of the stack
        // In other cases, leave the context unchanged
        // In lexing, we may encounter unmatched closing braces etc.
        if x.Context.Count > 0 && x.Context.Peek() = expected then
            x.Context.Pop() |> ignore

        x

    let level (x: LexBuilder) =
        let mutable result = ValueNone
        let mutable e = x.Context.GetEnumerator()

        while result.IsNone && e.MoveNext() do
            match e.Current with
            | LexContext.Interpolated3String level -> result <- ValueSome level
            | LexContext.InterpolatedString
            | LexContext.VerbatimInterpolatedString
            | LexContext.PlainString
            | LexContext.VerbatimString
            | LexContext.TripleQuotedString -> result <- ValueSome 1
            | _ -> ()

        match result with
        | ValueSome v -> v
        | ValueNone -> invalidOp "Cannot get level from empty context stack"

    let (|CoalescableToken|_|) (token: Token) =
        // Tokens that can be coalesced if adjacent
        // This occurs with string fragments in interpolated strings due to the way braces are handled
        match token.WithoutCommentFlags with
        // | Token.OtherUnlexed
        | Token.Interpolated3StringFragment
        | Token.VerbatimInterpolatedStringFragment
        | Token.InterpolatedStringFragment
        | Token.StringFragment -> true
        | _ -> false

    let private isLexTrivia (token: PositionedToken) =
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
            | Token.Newline
            | Token.Tab -> true
            | _ -> false

    /// Returns true if the token before `-` means `-` cannot be binary subtraction,
    /// and thus `-<numeric>` should be merged into a single negative literal token.
    let private allowsNegativeLiteral (token: PositionedToken) =
        if isLexTrivia token then
            true
        else
            match token.TokenWithoutCommentFlags with
            | Token.KWLParen
            | Token.KWLBracket
            | Token.KWLArrayBracket
            | Token.KWLBrace
            | Token.KWLAttrBracket
            | Token.KWBegin -> true
            | _ -> false

    /// Check if the current numeric token should be merged with a preceding `-` token
    /// to form a negative literal. Returns true if the merge was performed.
    let private tryMergeNegativeLiteral (token: Token) (idx: int) (state: LexBuilder) =
        let tokenCount = state.Tokens.Count

        if tokenCount < 1 then
            false
        else
            let prevIdx = tokenCount - 1
            let prev = state.Tokens[prevIdx]

            if
                prev.Token <> Token.OpSubtraction
                || idx - prev.StartIndex <> 1
                || state.Source[prev.StartIndex] <> '-'
            then
                false
            elif prevIdx = 0 || allowsNegativeLiteral state.Tokens[prevIdx - 1] then
                state.Tokens[prevIdx] <- PositionedToken.Create(token, prev.StartIndex)
                true
            else
                false

    let appendI (token: Token) (idx: int) ctxOp (state: LexBuilder) =
        // Coalesce adjacent string fragments
        // ADJACENT_PREFIX_OP handling is done in the parser (isAdjacentPrefixOp in ExpressionParsing.fs)
        // https://fsharp.github.io/fslang-spec/lexical-analysis/#381-post-filtering-of-adjacent-prefix-tokens
        let tokenCount = state.Tokens.Count
        let tokenIdx = tokenCount * 1<token>

        let addToken =
            if token.IsNumeric && tryMergeNegativeLiteral token idx state then
                false
            elif tokenCount > 0 then
                match token, state.Tokens[tokenCount - 1] with
                | CoalescableToken, t when t.Token = token -> false
                | _ -> true
            else
                true


        match token with
        | Token.EndOCamlBlockComment -> state.IsInOCamlBlockComment <- false
        | Token.BlockCommentEnd -> state.IsInBlockComment <- false
        | _ -> ()

        if addToken then
            let token =
                if state.IsInBlockComment || state.IsInOCamlBlockComment then
                    uint16 token ||| TokenRepresentation.InComment |> Token.ofUInt16
                else
                    token

            state.Tokens.Add(PositionedToken.Create(token, idx))

        match token with
        | Token.StartOCamlBlockComment -> state.IsInOCamlBlockComment <- true
        | Token.BlockCommentStart -> state.IsInBlockComment <- true
        | _ -> ()

        match state.LastTokenWasNewLine with
        | ValueSome newlineIdx ->
            state.LineStarts.Add(newlineIdx + 1<_>)
            state.LastTokenWasNewLine <- ValueNone
        | ValueNone -> ()

        match token with
        | Token.Newline ->
            state.AtStartOfLine <- true
            state.LastTokenWasNewLine <- ValueSome tokenIdx
        | _ -> state.AtStartOfLine <- false

        match ctxOp with
        | CtxOp.Push ctx -> pushContext ctx state
        | CtxOp.Pop ctx -> popExactContext ctx state
        | CtxOp.NoOp -> state

    let inline append token pos ctxOp (state: LexBuilder) =
        appendI token (int pos.Index) ctxOp state


[<AutoOpen>]
module internal Errors =
    // Centralized error messages. Explicit type annotations are mandatory: without
    // them the F# value restriction compiles each binding as a thunk that re-allocates
    // the `Message` every time it's accessed.
    let expectedEndOfIdent = Message "Expected end of identifier"

    let expectedNewline = Message "Expected newline"

    let expectedStringLiteral = Message "Expected string literal"

    let expectedIdentStartChar: ErrorType<char, LexBuilder> =
        Message "Expected identifier start character"

    let expectedBacktickedIdent: ErrorType<char, LexBuilder> =
        Message "Expected backticked identifier"

    let expectedIdentCharAfterQuote: ErrorType<char, LexBuilder> =
        Message "Expected identifier character after '"

    let expectedQuote: ErrorType<char, LexBuilder> = Message "Expected '"

    let expectedInterpolatedFragmentChar: ErrorType<char, LexBuilder> =
        Message "Expected interpolated string fragment character"

    let expectedOperator: ErrorType<char, LexBuilder> = Message "Expected operator"

    let expectedDirectiveAtStart: ErrorType<char, LexBuilder> =
        Message "Directives must be at start of line or immediately after indentation"

module Lexing =

    let anyString (xs: string seq) =
        // Sort by length (greedy first), then by lexicographic order
        // TODO: Consider a Trie (or compact DAWG) rather than linear search
        // This could improve performance for large sets of strings, likely worse for small sets
        let sorted =
            xs
            |> Seq.sortWith (fun s1 s2 ->
                let lenComp = String.length s2 - String.length s1

                if lenComp = 0 then
                    String.CompareOrdinal(s1, s2)
                else
                    sign lenComp
            )
            |> Array.ofSeq

        if Array.isEmpty sorted then
            invalidArg (nameof xs) "The input sequence must not be empty"

        if String.IsNullOrEmpty(Array.last sorted) then
            invalidArg (nameof xs) "The input sequence must not contain null or empty strings"

        let maxLen = sorted[0] |> String.length

        fun (reader: Reader<char, _, ReadableString>) ->
            let span = reader.PeekN maxLen

            if span.IsEmpty then
                fail EndOfInput reader
            else
                let mutable found = ValueNone
                let mutable i = 0

                while i < sorted.Length && found.IsNone do
                    let candidate = sorted.[i]

                    if span.StartsWith(candidate.AsSpan()) then
                        found <- ValueSome candidate

                    i <- i + 1

                match found with
                | ValueSome s ->
                    reader.SkipN s.Length
                    preturn s reader
                | ValueNone -> fail expectedStringLiteral reader

    let anyStringReturn (xs: (string * 'T) seq) =
        // Sort by length (greedy first), then by lexicographic order
        // TODO: Consider a Trie (or compact DAWG) rather than linear search
        // This could improve performance for large sets of strings, likely worse for small sets
        let sorted =
            xs
            |> Seq.sortWith (fun (s1, _) (s2, _) ->
                let lenComp = String.length s2 - String.length s1

                if lenComp = 0 then
                    String.CompareOrdinal(s1, s2)
                else
                    sign lenComp
            )
            |> Array.ofSeq

        if Array.isEmpty sorted then
            invalidArg (nameof xs) "The input sequence must not be empty"

        if String.IsNullOrEmpty(sorted |> Array.last |> fst) then
            invalidArg (nameof xs) "The input sequence must not contain null or empty strings"

        let maxLen = sorted[0] |> fst |> String.length

        fun (reader: Reader<char, _, ReadableString>) ->
            let span = reader.PeekN maxLen

            if span.IsEmpty then
                fail EndOfInput reader
            else
                let mutable found = ValueNone
                let mutable i = 0

                while i < sorted.Length && found.IsNone do
                    let (candidate, result) = sorted.[i]

                    if span.StartsWith(candidate.AsSpan()) then
                        found <- ValueSome(candidate, result)

                    i <- i + 1

                match found with
                | ValueSome(s, result) ->
                    reader.SkipN s.Length
                    preturn result reader
                | ValueNone -> fail expectedStringLiteral reader

    let identifierKeywords =
        [|
            "_", Token.Wildcard
            // 3.4 Identifiers and Keywords
            // ident-keyword
            "abstract", Token.KWAbstract
            "and", Token.KWAnd
            "as", Token.KWAs
            "assert", Token.KWAssert
            "base", Token.KWBase
            "begin", Token.KWBegin
            "break", Token.KWReservedBreak
            "checked", Token.KWReservedChecked
            "class", Token.KWClass
            "component", Token.KWReservedComponent
            "const", Token.KWConst
            "constraint", Token.KWReservedConstraint
            "continue", Token.KWReservedContinue
            "default", Token.KWDefault
            "delegate", Token.KWDelegate
            "do", Token.KWDo
            "done", Token.KWDone
            "downcast", Token.KWDowncast
            "downto", Token.KWDownto
            "elif", Token.KWElif
            "else", Token.KWElse
            "end", Token.KWEnd
            "exception", Token.KWException
            "extern", Token.KWExtern
            "false", Token.KWFalse
            "finally", Token.KWFinally
            "fixed", Token.KWFixed
            "for", Token.KWFor
            "fun", Token.KWFun
            "function", Token.KWFunction
            "global", Token.KWGlobal
            "if", Token.KWIf
            "in", Token.KWIn
            "include", Token.KWReservedInclude
            "inherit", Token.KWInherit
            "inline", Token.KWInline
            "interface", Token.KWInterface
            "internal", Token.KWInternal
            "lazy", Token.KWLazy
            "let", Token.KWLet
            "match", Token.KWMatch
            "member", Token.KWMember
            "mixin", Token.KWReservedMixin
            "mod", Token.KWMod
            "module", Token.KWModule
            "mutable", Token.KWMutable
            "namespace", Token.KWNamespace
            "new", Token.KWNew
            "null", Token.KWNull
            "of", Token.KWOf
            "open", Token.KWOpen
            "or", Token.KWOr
            "override", Token.KWOverride
            "parallel", Token.KWReservedParallel
            "private", Token.KWPrivate
            "process", Token.KWReservedProcess
            "protected", Token.KWReservedProtected
            "public", Token.KWPublic
            "pure", Token.KWReservedPure
            "rec", Token.KWRec
            "return", Token.KWReturn
            "sealed", Token.KWReservedSealed
            "sig", Token.KWSig
            "static", Token.KWStatic
            "struct", Token.KWStruct
            "tailcall", Token.KWReservedTailcall
            "then", Token.KWThen
            "to", Token.KWTo
            "trait", Token.KWReservedTrait
            "true", Token.KWTrue
            "try", Token.KWTry
            "type", Token.KWType
            "upcast", Token.KWUpcast
            "use", Token.KWUse
            "val", Token.KWVal
            "virtual", Token.KWReservedVirtual
            "void", Token.KWVoid
            "when", Token.KWWhen
            "while", Token.KWWhile
            "with", Token.KWWith
            "yield", Token.KWYield

            // 3.6 Symbolic Keywords
            "let!", Token.KWLetBang
            "use!", Token.KWUseBang
            "do!", Token.KWDoBang
            "yield!", Token.KWYieldBang
            "return!", Token.KWReturnBang
            "and!", Token.KWAndBang
            "match!", Token.KWMatchBang

            // 19.2 Extra Syntactic Forms for ML Compatibility
            // ocaml-ident-keyword
            // Deprecated but still recognized
            "asr", Token.KWAsr
            "land", Token.KWLand
            "lor", Token.KWLor
            "lsl", Token.KWLsl
            "lsr", Token.KWLsr
            "lxor", Token.KWLxor

            // reserved-ident-keyword
            "break", Token.KWReservedBreak
            "checked", Token.KWReservedChecked
            "component", Token.KWReservedComponent
            "constraint", Token.KWReservedConstraint
            "continue", Token.KWReservedContinue
            "fori", Token.KWReservedFori
            "include", Token.KWReservedInclude
            "mixin", Token.KWReservedMixin
            "parallel", Token.KWReservedParallel
            "params", Token.KWReservedParams
            "process", Token.KWReservedProcess
            "protected", Token.KWReservedProtected
            "pure", Token.KWReservedPure
            "sealed", Token.KWReservedSealed
            "tailcall", Token.KWReservedTailcall
            "trait", Token.KWReservedTrait
            "virtual", Token.KWReservedVirtual

            // 3.11 Identifier Replacements
            "__SOURCE_DIRECTORY__", Token.SourceDirectoryIdentifier
            "__SOURCE_FILE__", Token.SourceFileIdentifier
            "__LINE__", Token.LineIdentifier
        |]


    // https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/keyword-reference
    // https://fsharp.github.io/fslang-spec/lexical-analysis/#34-identifiers-and-keywords
    // let pKeyword =
    //     keywords
    //     |> anyStringReturn

    // https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/symbol-and-operator-reference/#operator-precedence
    // https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/operator-overloading
    // This construct is deprecated: '$' is not permitted as a character in operator names and is reserved for future use
    // Per https://github.com/dotnet/fsharp/pull/15923 `:` in operators is deprecated *unless* the operator starts with '>'
    let private customOperatorChars = "><+-*/=~$%.&|@^!?:"

    let private customOperatorSearchValues =
        Buffers.SearchValues.Create(customOperatorChars)

    // let (><-*/=~%.&|@^!?:) a b = a + b

    let private parenChars = [| '('; ')'; '{'; '}'; '['; ']' |]

    // Hoisted character predicates and counting/skipping sub-parsers.
    // These are allocated once at module initialization so each invocation of
    // a `parser { }` body that references them reuses the same closure instead
    // of rebuilding (e.g.) `many1Chars (pchar ' ')` on every call.
    let private isSpace c = c = ' '
    let private isLBrace c = c = '{'
    let private isRBrace c = c = '}'
    let private isDoubleQuote c = c = '"'
    let private isDollar c = c = '$'
    let private isPercent c = c = '%'

    let private isPlainStringFragmentChar c = c <> '"' && c <> '\\' && c <> '%'

    let private isVerbatimStringFragmentChar c = c <> '"' && c <> '%'

    let private isInterpolated3FragmentChar c =
        c <> '"' && c <> '{' && c <> '}' && c <> '%'

    let private isVerbatimInterpolatedFragmentChar c = c <> '"' && c <> '{' && c <> '}'

    let private isNotNewline c = c <> '\n' && c <> '\r'

    let private pSkipSpaces = skipMany1Satisfies isSpace
    let private pCountLBraces = countMany1Satisfies isLBrace
    let private pCountRBraces = countMany1Satisfies isRBrace
    let private pCountDoubleQuotes = countMany1Satisfies isDoubleQuote
    let private pCountDollars = countMany1Satisfies isDollar

    let private pSkipPlainStringFragmentChars =
        skipMany1Satisfies isPlainStringFragmentChar

    let private pSkipVerbatimStringFragmentChars =
        skipMany1Satisfies isVerbatimStringFragmentChar

    let private pSkipInterpolated3FragmentChars =
        skipMany1Satisfies isInterpolated3FragmentChar

    let private pSkipVerbatimInterpolatedFragmentChars =
        skipMany1Satisfies isVerbatimInterpolatedFragmentChar

    let private pSkipUntilNewline = skipManySatisfies isNotNewline

    /// Non-consuming span-based count of consecutive chars matching `predicate`
    /// starting at the reader's current position. Used in place of
    /// `lookAhead (many1Chars (pchar c))` to avoid allocating the run string.
    let private peekCountSatisfies (predicate: char -> bool) (reader: Reader<char, LexBuilder, ReadableString>) =
        let mutable windowSize = 16
        let mutable finished = false
        let mutable count = 0

        while not finished do
            let span = reader.PeekN(windowSize)
            let mutable i = 0

            while i < span.Length && predicate span.[i] do
                i <- i + 1

            count <- i

            if i < span.Length || span.Length < windowSize then
                finished <- true
            else
                windowSize <- windowSize * 2

        preturn count reader

    let private pPeekCountDoubleQuotes = peekCountSatisfies isDoubleQuote
    let private pPeekCountRBraces = peekCountSatisfies isRBrace
    let private pPeekCountPercents = peekCountSatisfies isPercent

    let private pToken p token =
        parser {
            let! pos = getPosition
            let! _ = p
            do! updateUserState (fun state -> LexBuilder.append token pos CtxOp.NoOp state)
        }

    let private pTokenPushCtx p token ctx =
        parser {
            let! pos = getPosition
            let! _ = p
            do! updateUserState (fun state -> LexBuilder.append token pos (CtxOp.Push ctx) state)
        }

    let private pTokenPopCtx p token ctx =
        parser {
            let! pos = getPosition
            let! _ = p
            do! updateUserState (fun state -> LexBuilder.append token pos (CtxOp.Pop ctx) state)
        }

    let pIndentOrWhitespaceToken =
        parser {
            let! pos = getPosition
            do! pSkipSpaces

            do!
                updateUserState (fun state ->
                    if state.AtStartOfLine then
                        LexBuilder.append Token.Indent pos CtxOp.NoOp state
                    else
                        LexBuilder.append Token.Whitespace pos CtxOp.NoOp state
                )
        }

    // https://fsharp.github.io/fslang-spec/lexical-analysis/#34-identifiers-and-keywords

    // TODO: Special operators
    // OCaml index set let (.()<-) e1 e2 = e1 || e2
    // OCaml index get let (.()) e1 e2 = e1 || e2
    // Index set let (.[]<-) e1 e2 = e1 || e2
    // Index get let (.[]) e1 e2 = e1 || e2


    let isIdentStartChar (c: char) =
        if c = '_' then
            true
        else
            let category = Char.GetUnicodeCategory c

            match category with
            | UnicodeCategory.UppercaseLetter //Lu
            | UnicodeCategory.LowercaseLetter //Ll
            | UnicodeCategory.TitlecaseLetter //Lt
            | UnicodeCategory.ModifierLetter //Lm
            | UnicodeCategory.OtherLetter //Lo
            | UnicodeCategory.LetterNumber -> //Nl
                true
            | _ -> false


    let isIdentChar c =
        if c = '_' || c = '\'' then
            true
        elif c >= '0' && c <= '9' then
            true
        else
            let category = Char.GetUnicodeCategory c

            match category with
            | UnicodeCategory.UppercaseLetter //Lu
            | UnicodeCategory.LowercaseLetter //Ll
            | UnicodeCategory.TitlecaseLetter //Lt
            | UnicodeCategory.ModifierLetter //Lm
            | UnicodeCategory.OtherLetter //Lo
            | UnicodeCategory.LetterNumber //Nl
            | UnicodeCategory.ConnectorPunctuation //Pc
            | UnicodeCategory.NonSpacingMark //Mn
            | UnicodeCategory.SpacingCombiningMark //Mc
            | UnicodeCategory.Format -> //Cf
                true
            | _ -> false

    let pIdentStartChar = satisfyL isIdentStartChar "identifier start character"
    let pIdentChar = satisfyL isIdentChar "identifier character"
    let pIdentifier = many1Chars2 pIdentStartChar pIdentChar

    // Ordinal-comparer-backed dictionary for keyword lookup. Per-identifier
    // we allocate one string via `source.Substring(start, len)` for the key,
    // avoiding the double allocation of the old
    // `many1Chars2 + (id + string c)` path.
    let private keywordsDict =
        let d = Dictionary<string, Token>(System.StringComparer.Ordinal)

        for (k, v) in identifierKeywords do
            d.[k] <- v

        d

    // 19.1 Conditional Compilation for ML Compatibility — these sentinel strings
    // close an OCaml-compatibility block comment when encountered in identifier
    // position.
    let private fSharpBlockCommentEnd1 = "F#*)"
    let private fSharpBlockCommentEnd2 = "ENDIF-FSHARP*)"

    // Scan an identifier body starting at `reader.Position` (caller has verified
    // the first char is an ident-start char — we consume it and the following
    // ident chars, then look up the resulting span in `keywordsDict`.
    let private lexIdentifierBody
        (reader: Reader<char, LexBuilder, ReadableString>)
        (pos: Position<LexBuilder>)
        (source: string)
        (startIdx: int)
        =
        // Consume the first char (caller has verified isIdentStartChar), then
        // span-scan the rest in one shot: PeekN grabs all remaining input as a
        // ReadOnlySpan (clamped to input length), we count matching ident chars
        // with direct indexing, and SkipN bumps the reader once.
        reader.Skip()
        let span = reader.PeekN(Int32.MaxValue)
        let mutable i = 0

        while i < span.Length && isIdentChar span.[i] do
            i <- i + 1

        reader.SkipN(i)

        let baseLen = int (reader.Position.Index - pos.Index)

        // Peek for ! / # suffix
        let suffix =
            match reader.Peek() with
            | ValueSome '!' -> ValueSome '!'
            | ValueSome '#' -> ValueSome '#'
            | _ -> ValueNone

        let token, consumedSuffix =
            match suffix with
            | ValueSome c ->
                // Suffix is always consumed if present. The full `id+c`
                // is looked up first; on miss, fall back to the
                // suffix-based token (OpDereference / ReservedIdentifierHash).
                let key = source.Substring(startIdx, baseLen + 1)

                match keywordsDict.TryGetValue(key) with
                | true, tok -> tok, true
                | false, _ ->
                    let fallback =
                        match c with
                        | '!' -> Token.OpDereference
                        | '#' -> Token.ReservedIdentifierHash
                        | _ -> Token.Identifier

                    fallback, true
            | ValueNone ->
                let key = source.Substring(startIdx, baseLen)

                match keywordsDict.TryGetValue(key) with
                | true, tok -> tok, false
                | false, _ -> Token.Identifier, false

        if consumedSuffix then
            reader.Skip()

        reader.State <- LexBuilder.append token pos CtxOp.NoOp reader.State
        Ok()

    let pIdentifierOrKeywordToken (reader: Reader<char, LexBuilder, ReadableString>) =
        let pos = reader.Position
        let source = reader.State.Source
        let startIdx = int pos.Index

        match reader.Peek() with
        | ValueNone -> fail EndOfInput reader
        // Sentinel fast path: F#*) and ENDIF-FSHARP*) can only start with F or E.
        // Skip the 2 long PeekN calls entirely for the 99%+ of identifiers that
        // don't start with one of those letters.
        | ValueSome 'F' ->
            let peek1 = reader.PeekN(fSharpBlockCommentEnd1.Length)

            if
                peek1.Length = fSharpBlockCommentEnd1.Length
                && peek1.SequenceEqual(fSharpBlockCommentEnd1.AsSpan())
            then
                reader.SkipN(fSharpBlockCommentEnd1.Length)
                reader.State <- LexBuilder.append Token.EndFSharpBlockComment pos CtxOp.NoOp reader.State
                Ok()
            else
                lexIdentifierBody reader pos source startIdx
        | ValueSome 'E' ->
            let peek2 = reader.PeekN(fSharpBlockCommentEnd2.Length)

            if
                peek2.Length = fSharpBlockCommentEnd2.Length
                && peek2.SequenceEqual(fSharpBlockCommentEnd2.AsSpan())
            then
                reader.SkipN(fSharpBlockCommentEnd2.Length)
                reader.State <- LexBuilder.append Token.EndFSharpBlockComment pos CtxOp.NoOp reader.State
                Ok()
            else
                lexIdentifierBody reader pos source startIdx
        | ValueSome _ -> lexIdentifierBody reader pos source startIdx


    let pBacktickedIdentifierToken (reader: Reader<char, LexBuilder, ReadableString>) =
        let rec pRest (reader: Reader<char, LexBuilder, ReadableString>) =
            let span = reader.PeekN(2)

            match span.Length with
            | 0 -> preturn Token.UnterminatedBacktickedIdentifier reader
            | 1 ->
                reader.Skip()
                preturn Token.UnterminatedBacktickedIdentifier reader
            | _ ->
                match span[0], span[1] with
                | '`', '`' ->
                    // Closing ``
                    reader.SkipN(2)
                    preturn Token.BacktickedIdentifier reader
                | '`', ('\n' | '\r' | '\t') ->
                    // End of backticked identifier
                    reader.Skip()
                    preturn Token.UnterminatedBacktickedIdentifier reader
                | '`', _ ->
                    reader.SkipN(2)
                    pRest reader
                | ('\n' | '\r' | '\t'), _ -> preturn Token.UnterminatedBacktickedIdentifier reader
                | _, ('`' | '\n' | '\r' | '\t') ->
                    reader.Skip()
                    pRest reader
                | _ ->
                    // Regular character within identifier
                    reader.SkipN(2)
                    pRest reader

        let pos = reader.Position
        let span = reader.PeekN(2)

        match span.Length with
        | 0 -> fail EndOfInput reader
        | 1 when span[0] = '`' ->
            reader.Skip()
            reader.State <- LexBuilder.append Token.UnterminatedBacktickedIdentifier pos CtxOp.NoOp reader.State
            preturn () reader
        | 1 -> fail expectedBacktickedIdent reader
        | _ ->
            match span[0], span[1] with
            | '`', '`' ->
                reader.SkipN(2) // Consume opening ``

                match pRest reader with
                | Ok token ->
                    reader.State <- LexBuilder.append token pos CtxOp.NoOp reader.State
                    preturn () reader
                | Error e -> Error e
            | '`', _ ->
                reader.Skip()
                reader.State <- LexBuilder.append Token.KWReservedBacktick pos CtxOp.NoOp reader.State
                preturn () reader
            | _ -> fail expectedBacktickedIdent reader


    let peekNewLine (reader: Reader<char, LexBuilder, ReadableString>) =
        match reader.Peek() with
        | ValueSome('\n' | '\r') -> preturn () reader
        | _ -> fail expectedNewline reader

    let pLineComment = pstring "//" >>. pSkipUntilNewline
    let pLineCommentToken = pToken pLineComment Token.LineComment

    module IfDirective =
        // 3.3 Conditional Compilation
        // The #if directive allows conditional compilation based on the presence of compiler symbols.
        // The syntax doesn't allow most things that are normally allowed in F#, so we use a separate context to handle it.
        let pNewlineToken =
            parser {
                let! pos = getPosition
                do! skipNewline

                do!
                    updateUserState (fun x ->
                        x
                        |> LexBuilder.append Token.Newline pos CtxOp.NoOp
                        |> LexBuilder.popExactContext LexContext.IfDirective
                    )
            }

        let pIdentifierOrOther =
            choiceL
                [
                    pLineCommentToken
                    pToken pIdentifier Token.Identifier
                    pToken pid Token.OtherUnlexed
                ]
                "#if Identifier or keyword"

        let pAnd =
            choiceL
                [
                    pToken (pstring "&&") Token.OpAmpAmp
                    pToken (pchar '&') Token.OpAmp // & is invalid in #if but we want the lexer to always succeed
                ]
                "#if And operator"

        let pOr =
            choiceL [ pToken (pstring "||") Token.OpBarBar; pToken (pchar '|') Token.OpBar ] "#if Or operator"

        let pNot = pToken (pstring "!") Token.OpDereference

        let pLParenToken =
            choiceL
                [
                    // 3.2 Comments
                    // Block comments are not allowed in #if directives, but we want the lexer to recognize
                    // the start of a block comment so that it can be flagged as an error later
                    pToken (pstring "(*") Token.BlockCommentStart
                    //
                    pToken (pchar '(') Token.KWLParen
                ]
                "Left parenthesis or unit"

        let pWhitespaceToken =
            parser {
                let! pos = getPosition
                do! pSkipSpaces
                do! updateUserState (fun state -> LexBuilder.append Token.Whitespace pos CtxOp.NoOp state)
            }

        let pRParenToken =
            choiceL [ pToken (pchar ')') Token.KWRParen ] "Right parenthesis or unit"

    // https://fsharp.github.io/fslang-spec/lexical-analysis/#35-strings-and-characters

    [<Struct>]
    [<RequireQualifiedAccess>]
    type CharChar =
        | Simple of c: char
        | Escaped of c: char
        | UnicodeShort of c: char
        | Trigraph of c: char
        | InvalidTrigraph of i: int

    let pCharChar (reader: Reader<char, LexBuilder, ReadableString>) =
        let span = reader.PeekN(2)

        match span.Length with
        | 0 -> fail EndOfInput reader
        | 1 ->
            match span[0] with
            | '"' -> fail (Unexpected '"') reader
            | ('\n' | '\t' | '\r' | '\b' | '\a' | '\f' | '\v' | '\\') as c -> fail (Unexpected c) reader
            | c ->
                reader.Skip()
                preturn (CharChar.Simple c) reader
        | _ ->
            match span[0], span[1] with
            // Escape chars ["\'ntbrafv]
            | '\\', '"' ->
                // Escaped quote
                reader.SkipN(2)
                preturn (CharChar.Escaped '"') reader
            | '\\', '\\' ->
                // Escaped backslash
                reader.SkipN(2)
                preturn (CharChar.Escaped '\\') reader
            | '\\', '\'' ->
                // Escaped single quote
                reader.SkipN(2)
                preturn (CharChar.Escaped '\'') reader
            | '\\', 'n' ->
                reader.SkipN(2)
                preturn (CharChar.Escaped '\n') reader
            | '\\', 't' ->
                reader.SkipN(2)
                preturn (CharChar.Escaped '\t') reader
            | '\\', 'b' ->
                reader.SkipN(2)
                preturn (CharChar.Escaped '\b') reader
            | '\\', 'r' ->
                reader.SkipN(2)
                preturn (CharChar.Escaped '\r') reader
            | '\\', 'a' ->
                reader.SkipN(2)
                preturn (CharChar.Escaped '\a') reader
            | '\\', 'f' ->
                reader.SkipN(2)
                preturn (CharChar.Escaped '\f') reader
            | '\\', 'v' ->
                reader.SkipN(2)
                preturn (CharChar.Escaped '\v') reader
            | '\\', 'u' ->
                // unicodegraph-short
                let span = reader.PeekN(6)

                if span.Length = 6 then
                    let hex = span.Slice(2, 4)

                    match UInt16.TryParse(hex, NumberStyles.AllowHexSpecifier, CultureInfo.InvariantCulture) with
                    | true, code ->
                        reader.SkipN(6)
                        preturn (CharChar.UnicodeShort(char code)) reader
                    | false, _ -> fail (Unexpected '\\') reader
                else
                    fail (Unexpected '\\') reader
            | '\\', 'x' ->
                // hex escape \xHH
                let span = reader.PeekN(4)

                if span.Length = 4 then
                    let hex = span.Slice(2, 2)

                    match Byte.TryParse(hex, NumberStyles.AllowHexSpecifier, CultureInfo.InvariantCulture) with
                    | true, code ->
                        reader.SkipN(4)
                        preturn (CharChar.Escaped(char code)) reader
                    | false, _ -> fail (Unexpected '\\') reader
                else
                    fail (Unexpected '\\') reader
            | '\\', c ->
                if isDigit c then
                    // trigraph
                    let span = reader.PeekN(4)

                    if span.Length = 4 then
                        let digits = span.Slice(1, 3)

                        match Int32.TryParse(digits, NumberStyles.None, CultureInfo.InvariantCulture) with
                        | true, code when code <= 255 ->
                            reader.SkipN(4)
                            preturn (CharChar.Trigraph(char code)) reader
                        | true, code ->
                            reader.SkipN(4)
                            preturn (CharChar.InvalidTrigraph code) reader
                        | false, _ -> fail (Unexpected '\\') reader
                    else
                        fail (Unexpected '\\') reader
                else
                    fail (Unexpected '\\') reader
            | c, _ ->
                reader.Skip()
                preturn (CharChar.Simple c) reader

    let pCharToken =
        let pLiteral =
            between (pchar '\'') (pchar '\'') pCharChar
            |>> (fun x ->
                match x with
                | CharChar.InvalidTrigraph _ -> Token.InvalidCharTrigraphLiteral
                | _ -> Token.CharLiteral
            )

        parser {
            let! pos = getPosition
            let! token = pLiteral
            do! updateUserState (fun state -> LexBuilder.append token pos CtxOp.NoOp state)
        }

    // ======================================================================
    // Fragment-based plain string lexing (paralleling interpolated strings)
    // ======================================================================

    let pStringOpenToken =
        pTokenPushCtx (pchar '"') Token.StringOpen LexContext.PlainString

    let pVerbatimStringOpenToken =
        pTokenPushCtx (pstring "@\"") Token.VerbatimStringOpen LexContext.VerbatimString

    let pString3OpenToken =
        pTokenPushCtx (pstring "\"\"\"") Token.String3Open LexContext.TripleQuotedString

    // Fragment: plain text inside a regular string (stops at ", \, %)
    let pPlainStringFragmentToken =
        pToken pSkipPlainStringFragmentChars Token.StringFragment

    // Fragment: plain text inside a verbatim string (stops at ", %)
    let pVerbatimStringFragmentToken2 =
        pToken pSkipVerbatimStringFragmentChars Token.StringFragment

    // Fragment: plain text inside a triple-quoted string (stops at ", %)
    let pTripleStringFragmentToken =
        pToken pSkipVerbatimStringFragmentChars Token.StringFragment

    // Escape sequence inside a regular string: \n, \t, \xHH, \uXXXX, \UXXXXXXXX, \DDD, etc.
    let pStringEscapeToken (reader: Reader<char, LexBuilder, ReadableString>) =
        let pos = reader.Position
        let span = reader.PeekN(2)

        if span.Length < 2 then
            // Backslash at EOF — treat as a single char fragment
            reader.Skip()
            updateUserState (LexBuilder.append Token.EscapeSequence pos CtxOp.NoOp) reader
        else
            let c1 = span[1]

            match c1 with
            | '"'
            | '\\'
            | '\''
            | 'n'
            | 't'
            | 'b'
            | 'r'
            | 'a'
            | 'f'
            | 'v' ->
                reader.SkipN(2)
                updateUserState (LexBuilder.append Token.EscapeSequence pos CtxOp.NoOp) reader
            | 'u' ->
                // \uXXXX — try to consume 6 chars, fall back to 2
                let full = reader.PeekN(6)

                if full.Length = 6 then reader.SkipN(6) else reader.SkipN(2)

                updateUserState (LexBuilder.append Token.EscapeSequence pos CtxOp.NoOp) reader
            | 'x' ->
                // \xHH — try to consume 4 chars, fall back to 2
                let full = reader.PeekN(4)

                if full.Length = 4 then reader.SkipN(4) else reader.SkipN(2)

                updateUserState (LexBuilder.append Token.EscapeSequence pos CtxOp.NoOp) reader
            | 'U' ->
                // \UXXXXXXXX — try to consume 10 chars, fall back to 2
                let full = reader.PeekN(10)

                if full.Length = 10 then
                    reader.SkipN(10)
                else
                    reader.SkipN(2)

                updateUserState (LexBuilder.append Token.EscapeSequence pos CtxOp.NoOp) reader
            | c when isDigit c ->
                // \DDD trigraph — try to consume 4 chars, fall back to 2
                let full = reader.PeekN(4)

                if full.Length = 4 then reader.SkipN(4) else reader.SkipN(2)

                updateUserState (LexBuilder.append Token.EscapeSequence pos CtxOp.NoOp) reader
            | _ ->
                // Unknown escape — consume \ and the next char
                reader.SkipN(2)
                updateUserState (LexBuilder.append Token.EscapeSequence pos CtxOp.NoOp) reader

    // Close tokens for plain strings: "B (byte array) vs " (string).
    let pPlainStringCloseToken =
        fun (reader: Reader<char, LexBuilder, ReadableString>) ->
            let span = reader.PeekN(2)
            let pos = reader.Position

            if span.Length >= 2 && span.[1] = 'B' then
                reader.SkipN(2)

                reader.State <-
                    LexBuilder.append Token.ByteArrayClose pos (CtxOp.Pop LexContext.PlainString) reader.State

                Ok()
            else
                reader.Skip()

                reader.State <- LexBuilder.append Token.StringClose pos (CtxOp.Pop LexContext.PlainString) reader.State

                Ok()

    // Verbatim string: "" is an escape, "B is byte array close, " is close
    let pVerbatimStringQuoteToken2 =
        parser {
            let! pos = getPosition
            let! quoteCount = pPeekCountDoubleQuotes

            if quoteCount >= 2 then
                // "" is an escaped quote
                do! skipN 2
                do! updateUserState (LexBuilder.append Token.VerbatimEscapeQuote pos CtxOp.NoOp)
            else
                // Single " — check for "B (byte array) or plain close
                let! span = lookAhead (pstring "\"B" >>% true <|> preturn false)

                if span then
                    do! skipN 2

                    do!
                        updateUserState (
                            LexBuilder.append Token.VerbatimByteArrayClose pos (CtxOp.Pop LexContext.VerbatimString)
                        )
                else
                    do! skip

                    do!
                        updateUserState (
                            LexBuilder.append Token.VerbatimStringClose pos (CtxOp.Pop LexContext.VerbatimString)
                        )
        }

    // Triple-quoted string: """ closes, fewer quotes are fragment text
    let pTripleStringQuoteOrFragment =
        parser {
            let! pos = getPosition
            let! quoteCount = pPeekCountDoubleQuotes

            if quoteCount >= 3 then
                do! skipN 3
                do! updateUserState (LexBuilder.append Token.String3Close pos (CtxOp.Pop LexContext.TripleQuotedString))
            else
                do! skipN quoteCount
                do! updateUserState (LexBuilder.append Token.StringFragment pos CtxOp.NoOp)
        }

    let pTypeParamToken (reader: Reader<char, LexBuilder, ReadableString>) =
        let pos = reader.Position

        match reader.Peek() with
        | ValueSome '\'' ->
            reader.Skip()

            match reader.Peek() with
            | ValueSome c when isIdentChar c ->
                reader.Skip()
                let identStart = int pos.Index + 1
                let mutable more = true

                while more do
                    match reader.Peek() with
                    | ValueSome ic when isIdentChar ic -> reader.Skip()
                    | _ -> more <- false

                let identLen = int (reader.Position.Index) - identStart
                let source = reader.State.Source
                let key = source.Substring(identStart, identLen)

                match keywordsDict.TryGetValue(key) with
                | true, token ->
                    // e.g. 'let is a keyword, not a type parameter
                    let newState =
                        reader.State
                        |> LexBuilder.append Token.KWSingleQuote pos CtxOp.NoOp
                        |> LexBuilder.appendI token identStart CtxOp.NoOp

                    reader.State <- newState
                    preturn () reader
                | false, _ ->
                    reader.State <- LexBuilder.append Token.TypeParameter pos CtxOp.NoOp reader.State
                    preturn () reader
            | _ -> fail expectedIdentCharAfterQuote reader
        | _ -> fail expectedQuote reader

    let pInterpolatedStringStartToken =
        pTokenPushCtx (pstring "$\"") Token.InterpolatedStringOpen LexContext.InterpolatedString

    let pVerbatimInterpolatedStartToken =
        pTokenPushCtx
            (anyString [| "@$\""; "$@\"" |])
            Token.VerbatimInterpolatedStringOpen
            LexContext.VerbatimInterpolatedString

    let pInterpolatedStringEndToken =
        pTokenPopCtx (pchar '"') Token.InterpolatedStringClose LexContext.InterpolatedString

    // Non-verbatim interpolated strings support escape sequences like \".
    // A backslash consumes itself and the next character as an escape pair;
    // a bare backslash at EOF causes the whole fragment to fail (matches the
    // old `pchar '\\' >>. anyChar` behaviour).
    let private pSkipInterpolatedFragmentChars (reader: Reader<char, LexBuilder, ReadableString>) =
        let mutable more = true
        let mutable consumedAny = false

        while more do
            match reader.Peek() with
            | ValueSome '\\' ->
                let span = reader.PeekN(2)

                if span.Length >= 2 then
                    reader.SkipN(2)
                    consumedAny <- true
                else
                    // lone \ at EOF — stop without consuming
                    more <- false
            | ValueSome c when c <> '"' && c <> '{' && c <> '}' && c <> '%' ->
                reader.Skip()
                consumedAny <- true
            | _ -> more <- false

        if consumedAny then
            preturn () reader
        else
            fail expectedInterpolatedFragmentChar reader

    let pInterpolatedStringFragmentToken =
        pToken pSkipInterpolatedFragmentChars Token.InterpolatedStringFragment

    let pInterpolated3StringFragmentToken =
        pToken pSkipInterpolated3FragmentChars Token.Interpolated3StringFragment

    let pInterpolatedExpressionStartToken =
        parser {
            let! pos = getPosition
            let! braceCount = pCountLBraces

            do!
                updateUserState (fun state ->

                    let mutable count = int braceCount
                    let mutable idx = int pos.Index

                    while count > 1 do
                        // {{ is an escape sequence for '{'
                        LexBuilder.appendI Token.EscapeLBrace idx CtxOp.NoOp state |> ignore
                        idx <- idx + 2
                        count <- count - 2

                    match count with
                    | 0 -> state
                    | _ ->
                        // Single { starts an expression
                        LexBuilder.appendI
                            Token.InterpolatedExpressionOpen
                            idx
                            (CtxOp.Push LexContext.InterpolatedExpression)
                            state
                )
        }

    let pInterpolated3ExpressionStartToken =
        parser {
            let! pos = getPosition
            let! braceCount = pCountLBraces

            do!
                updateUserState (fun state ->
                    let level = LexBuilder.level state
                    let count = int braceCount
                    let idx = pos.Index

                    let diff = count - level

                    if diff < 0 then
                        // We have fewer { than the current level, treat the whole block as literal
                        LexBuilder.appendI Token.Interpolated3StringFragment idx CtxOp.NoOp state
                    elif diff = 0 then
                        // We have exactly the number of { to open an expression
                        LexBuilder.appendI
                            Token.InterpolatedExpressionOpen
                            idx
                            (CtxOp.Push LexContext.InterpolatedExpression)
                            state
                    elif diff >= level then
                        // We have more or equal { than 2x the current level, treat the whole block as invalid
                        LexBuilder.appendI Token.TooManyLBracesInInterpolated3String idx CtxOp.NoOp state
                    else
                        // We have some number of literal braces, then an expression open
                        state
                        |> LexBuilder.appendI Token.Interpolated3StringFragment idx CtxOp.NoOp
                        |> LexBuilder.appendI
                            Token.InterpolatedExpressionOpen
                            (idx + diff)
                            (CtxOp.Push LexContext.InterpolatedExpression)
                )
        }


    /// When inside an interpolated expression ({expr:format}), ':' starts a .NET format clause.
    /// Consumes ':' and everything up to (but not including) '}'.
    let pInterpolatedFormatClause =
        let pSkipUntilRBrace = skipManySatisfies (fun c -> c <> '}')

        parser {
            let! pos = getPosition
            do! skip // consume ':'
            do! pSkipUntilRBrace

            do! updateUserState (LexBuilder.append Token.InterpolatedFormatClause pos CtxOp.NoOp)
        }

    let pOpenBraceExpressionContext =
        pTokenPushCtx (pchar '{') Token.KWLBrace LexContext.BracedExpression

    let pCloseBraceExpressionContext =
        pTokenPopCtx (pchar '}') Token.KWRBrace LexContext.BracedExpression

    let pOpenBraceBarExpressionContext =
        pTokenPushCtx (pstring "{|") Token.KWLBraceBar LexContext.BraceBarExpression

    let pCloseBraceBarExpressionContext =
        pTokenPopCtx (pstring "|}") Token.KWRBraceBar LexContext.BraceBarExpression

    let pOpenParenExpressionContext =
        pTokenPushCtx (pchar '(') Token.KWLParen LexContext.ParenthesExpression

    let pCloseParenExpressionContext =
        pTokenPopCtx (pchar ')') Token.KWRParen LexContext.ParenthesExpression

    let pOpenBracketExpressionContext =
        pTokenPushCtx (pchar '[') Token.KWLBracket LexContext.BracketedExpression

    let pCloseBracketExpressionContext =
        pTokenPopCtx (pchar ']') Token.KWRBracket LexContext.BracketedExpression

    let pInterpolatedExpressionEndToken =
        parser {
            let! pos = getPosition
            let! braceCount = pPeekCountRBraces
            let level = LexBuilder.level pos.State

            match level with
            | 1 ->
                do! skip

                do!
                    updateUserState (
                        LexBuilder.append
                            Token.InterpolatedExpressionClose
                            pos
                            (CtxOp.Pop LexContext.InterpolatedExpression)
                    )
            | _ when braceCount >= level ->
                do! skipN level

                do!
                    updateUserState (
                        LexBuilder.append
                            Token.InterpolatedExpressionClose
                            pos
                            (CtxOp.Pop LexContext.InterpolatedExpression)
                    )
            | _ ->
                do! skipN braceCount

                do!
                    updateUserState (fun state ->
                        let mutable state = state

                        for i in 0 .. (braceCount - 1) do
                            // We have some number of braces, but not enough to close the expression
                            state <- LexBuilder.appendI Token.KWRBrace (pos.Index + i) CtxOp.NoOp state

                        state
                    )
        }


    let pInterpolatedStringFragmentRBraces =
        parser {
            let! pos = getPosition
            let! braceCount = pCountRBraces

            let mutable count = int braceCount
            let mutable idx = int pos.Index

            do!
                updateUserState (fun state ->
                    let mutable state = state

                    while count > 1 do
                        // }} is an escape sequence for '}'
                        // TODO: Investigate order of imperative operations in a while loop
                        idx <- idx + 2
                        count <- count - 2
                        state <- LexBuilder.appendI Token.EscapeRBrace idx CtxOp.NoOp state

                    state
                )

            match count with
            | 0 -> return ()
            | _ ->
                // Single } is invalid outside an expression
                do! updateUserState (LexBuilder.appendI Token.UnmatchedInterpolatedRBrace idx CtxOp.NoOp)
        }

    let pInterpolated3StringFragmentRBraces =
        parser {
            let! pos = getPosition
            let! braceCount = pCountRBraces
            let level = LexBuilder.level pos.State

            let count = int braceCount
            let idx = int pos.Index

            let diff = count - level

            if diff < 0 then
                do! updateUserState (LexBuilder.appendI Token.Interpolated3StringFragment idx CtxOp.NoOp)
                return ()
            elif diff = 0 then
                do!
                    updateUserState (
                        LexBuilder.appendI
                            Token.InterpolatedExpressionClose
                            idx
                            (CtxOp.Pop LexContext.InterpolatedExpression)
                    )

                return ()
            elif diff >= level then
                // We have more } than 2x the current level, treat the whole block as invalid
                do! updateUserState (LexBuilder.appendI Token.TooManyRBracesInInterpolated3String idx CtxOp.NoOp)
                return ()
            else
                // We have some number of escaped braces, then an expression close
                do! updateUserState (LexBuilder.appendI Token.Interpolated3StringFragment idx CtxOp.NoOp)
                let idx = idx + diff

                do!
                    updateUserState (
                        LexBuilder.appendI
                            Token.InterpolatedExpressionClose
                            idx
                            (CtxOp.Pop LexContext.InterpolatedExpression)
                    )

                return ()
        }

    let pVerbatimInterpolatedStringQuoteToken =
        parser {
            let! pos = getPosition
            let! quoteCount = pCountDoubleQuotes

            do!
                updateUserState (fun state ->
                    let mutable idx = pos.Index
                    let mutable state = state
                    let mutable count = int quoteCount

                    while count > 1 do
                        // "" is an escape sequence for '"'
                        state <- LexBuilder.appendI Token.VerbatimEscapeQuote idx CtxOp.NoOp state
                        idx <- idx + 2
                        count <- count - 2

                    match count with
                    | 0 -> state
                    | _ ->
                        // Single " ends the verbatim interpolated string
                        LexBuilder.appendI
                            Token.VerbatimInterpolatedStringClose
                            idx
                            (CtxOp.Pop LexContext.VerbatimInterpolatedString)
                            state
                )
        }

    let pVerbatimInterpolatedStringFragmentToken =
        pToken pSkipVerbatimInterpolatedFragmentChars Token.VerbatimInterpolatedStringFragment

    let pInterpolated3StartToken =
        parser {
            let! pos = getPosition
            let! dollarCount = pCountDollars
            let! _ = pstring "\"\"\""

            do!
                updateUserState (
                    LexBuilder.append
                        Token.Interpolated3StringOpen
                        pos
                        (CtxOp.Push(LexContext.Interpolated3String(int dollarCount)))
                )
        }

    let pInterpolated3EndToken =
        parser {
            let! pos = getPosition
            let! _ = pstring "\"\"\""

            do!
                updateUserState (
                    LexBuilder.append
                        Token.Interpolated3StringClose
                        pos
                        (CtxOp.Pop(LexContext.Interpolated3String(LexBuilder.level pos.State)))
                )
        }

    /// When `"` is encountered inside a triple-quoted interpolated string, try to close with `"""`.
    /// If fewer than 3 quotes, treat them as literal fragment content.
    let pInterpolated3QuoteOrFragment =
        pInterpolated3EndToken
        <|> pToken pCountDoubleQuotes Token.Interpolated3StringFragment


    let pNewlineToken =
        parser {
            let! pos = getPosition
            do! skipNewline
            do! updateUserState (fun state -> LexBuilder.append Token.Newline pos CtxOp.NoOp state)
        }

    // Well-known non-`:`-prefix operator strings, in length-major order. The `:`-starting
    // operators (`:`, `::`, `:?`, `:>`, `:=`, `:?>`) are reachable only via the first-char
    // special-case branch in `pOperatorToken`, which emits their tokens directly and never
    // touches this table. The `.. ..` arm that existed in the prior literal match was dead
    // code — the consumption phase stops at whitespace, and `OpRangeStep` is fused in the
    // parser (see memory: pattern_range_step_op_name).
    let private wellKnownOps =
        [|
            // length 1 (indices 0..9)
            "|"
            "."
            "?"
            "!"
            "="
            "&"
            "*"
            "/"
            "^"
            "~"
            // length 2 (indices 10..17) — ordered by expected frequency
            "->"
            "<-"
            ".."
            "||"
            "&&"
            "??"
            "<@"
            "@>"
            // length 3 (indices 18..21)
            "?<-"
            "~~~"
            "<@@"
            "@@>"
        |]

    let private wellKnownOpTokens =
        [|
            // length 1
            Token.OpBar
            Token.OpDot
            Token.OpDynamic
            Token.OpDereference
            Token.OpEquality
            Token.OpAmp
            Token.OpMultiply
            Token.OpDivision
            Token.OpConcatenate
            Token.KWReservedTwiddle
            // length 2
            Token.OpArrowRight
            Token.OpArrowLeft
            Token.OpRange
            Token.OpBarBar
            Token.OpAmpAmp
            Token.OpQMarkQMark
            Token.OpQuotationTypedLeft
            Token.OpQuotationTypedRight
            // length 3
            Token.OpDynamicAssignment
            Token.OpLogicalNot
            Token.OpQuotationUntypedLeft
            Token.OpQuotationUntypedRight
        |]

    [<Literal>]
    let private wkLen1Start = 0

    [<Literal>]
    let private wkLen2Start = 10

    [<Literal>]
    let private wkLen3Start = 18

    [<Literal>]
    let private wkLen3End = 22

    let inline private findWellKnownOp (s: int) (e: int) (span: ReadOnlySpan<char>) =
        let mutable i = s
        let mutable found = -1

        while found < 0 && i < e do
            if span.SequenceEqual(wellKnownOps.[i].AsSpan()) then
                found <- i

            i <- i + 1

        found

    // Fast-path parser for `:`-first-char. The six predefined colon-starting operators
    // (`:`, `::`, `:?`, `:?>`, `:>`, `:=`) are emitted as distinct KindKeyword tokens.
    // Dispatched via `exprCtxDispatchChars`/`exprCtxDispatchers` so `pOperatorToken` never
    // sees `:` as the first char; mid-operator `:` is still consumed by `pOperatorToken`'s
    // scan and the span is classified as `Token.ReservedOperator` per dotnet/fsharp#15923.
    let pColonToken =
        fun (reader: Reader<char, LexBuilder, ReadableString>) ->
            let state = reader.State
            let startIdx = reader.Position.Index
            // span[0] = ':' by dispatch contract. Examine span[1] (and span[2] when span[1]='?').
            let span = reader.PeekN(3)

            let token, len =
                if span.Length < 2 then
                    Token.OpColon, 1
                else
                    match span.[1] with
                    | ':' -> Token.OpCons, 2
                    | '=' -> Token.OpColonEquals, 2
                    | '>' -> Token.OpUpcast, 2
                    | '?' when span.Length >= 3 && span.[2] = '>' -> Token.OpDowncast, 3
                    | '?' -> Token.OpTypeTest, 2
                    | _ -> Token.OpColon, 1

            reader.SkipN(len)
            reader.State <- LexBuilder.appendI token startIdx CtxOp.NoOp state
            preturn () reader

    let pOperatorToken =
        // First char is never `:` — that case is handled by `pColonToken` via the
        // `exprCtxDispatchChars` fast-path. Mid-operator `:` IS consumed greedily by the
        // scan and the span is classified as `Token.ReservedOperator` by
        // `Token.ofCustomOperator` (per dotnet/fsharp#15923, unless the op starts with `>`).
        // Inside a block comment we additionally truncate at `*)` to avoid crossing the
        // close delimiter.
        fun (reader: Reader<char, LexBuilder, ReadableString>) ->
            let state = reader.State
            let startIdx = reader.Position.Index
            let inComment = state.IsInBlockComment || state.IsInOCamlBlockComment

            let fullSpan = reader.PeekN(Int32.MaxValue)
            let idx = fullSpan.IndexOfAnyExcept(customOperatorSearchValues)
            let rawLen = if idx < 0 then fullSpan.Length else idx

            let len =
                if inComment then
                    // Probe one char past rawLen to catch the `* | )` boundary case:
                    // operator span ends at `*`, the char that stopped the scan is `)`.
                    let probeLen = min (rawLen + 1) fullSpan.Length
                    let probe = fullSpan.Slice(0, probeLen)
                    let starIdx = probe.IndexOf("*)".AsSpan())

                    if starIdx >= 0 && starIdx <= rawLen then
                        starIdx
                    else
                        rawLen
                else
                    rawLen

            if len = 0 then
                fail expectedOperator reader
            else
                reader.SkipN(len)
                let span = state.Source.AsSpan(startIdx, len)

                let found =
                    match len with
                    | 1 -> findWellKnownOp wkLen1Start wkLen2Start span
                    | 2 -> findWellKnownOp wkLen2Start wkLen3Start span
                    | 3 -> findWellKnownOp wkLen3Start wkLen3End span
                    | _ -> -1

                let token, ctx =
                    if found >= 0 then
                        let t = wellKnownOpTokens.[found]

                        let c =
                            match t with
                            | Token.OpQuotationTypedLeft -> CtxOp.Push LexContext.QuotedExpression
                            | Token.OpQuotationTypedRight -> CtxOp.Pop LexContext.QuotedExpression
                            | Token.OpQuotationUntypedLeft -> CtxOp.Push LexContext.TypedQuotedExpression
                            | Token.OpQuotationUntypedRight -> CtxOp.Pop LexContext.TypedQuotedExpression
                            | _ -> CtxOp.NoOp

                        t, c
                    else
                        Token.ofCustomOperator span, CtxOp.NoOp

                reader.State <- LexBuilder.appendI token startIdx ctx state
                preturn () reader

    let pSpecialDotOperatorToken =
        let specialOperators =
            [|
                // F# style index operators
                // These can no longer be redefined but they are still special syntactically
                ".[]<-", (Token.OpIndexSetIdentifier, CtxOp.NoOp)
                ".[]", (Token.OpIndexGetIdentifier, CtxOp.NoOp)
                ".[,]<-", (Token.OpIndexSet2Identifier, CtxOp.NoOp)
                ".[,]", (Token.OpIndexGet2Identifier, CtxOp.NoOp)
                ".[,,]<-", (Token.OpIndexSet3Identifier, CtxOp.NoOp)
                ".[,,]", (Token.OpIndexGet3Identifier, CtxOp.NoOp)
                ".[,,,]<-", (Token.OpIndexSet4Identifier, CtxOp.NoOp)
                ".[,,,]", (Token.OpIndexGet4Identifier, CtxOp.NoOp)
                // ML style index operators
                ".()<-", (Token.OpIndexSetParenIdentifier, CtxOp.NoOp)
                ".()", (Token.OpIndexGetParenIdentifier, CtxOp.NoOp)
            |]

        let pSpecialOperatorToken = anyStringReturn specialOperators

        parser {
            let! pos = getPosition
            let! (token, ctxOp) = pSpecialOperatorToken
            do! updateUserState (fun state -> LexBuilder.append token pos ctxOp state)
        }


    // This is a fallback, we shouldn't see any Other tokens in output
    let private peekEndOfIdent (reader: Reader<char, LexBuilder, ReadableString>) =
        match reader.Peek() with
        | ValueSome c when isIdentChar c -> fail expectedEndOfIdent reader
        | _ -> preturn () reader

    // Any single char that doesn't start another token
    // Multiple OtherUnlexed tokens will be combined in the append step
    let pOtherToken = pToken anyChar Token.OtherUnlexed

    module NumericLiterals =
        open System.Text

        let isHexDigit c =
            (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

        let isBinaryDigit c = c = '0' || c = '1'

        let isOctalDigit c = c >= '0' && c <= '7'

        let isDecimalDigit c = c >= '0' && c <= '9'
        let private expectedBasePrefix = Message "Expected base prefix 0x, 0o, or 0b"
        let private expectedOneDigit = Message "Expected at least one digit"

        [<TailCall>]
        let rec private pManyIntCharsWithBaseLoop
            success
            isDigitInBase
            backtrackTo
            (reader: Reader<char, LexBuilder, ReadableString>)
            =
            match reader.Peek() with
            | ValueSome '_' ->
                // Ignore underscores
                reader.Skip()
                pManyIntCharsWithBaseLoop success isDigitInBase backtrackTo reader
            | ValueSome c when isDigitInBase c ->
                reader.Skip()
                pManyIntCharsWithBaseLoop true isDigitInBase reader.Position reader
            | _ ->
                if success then
                    // We must backtrack to the last valid position
                    // to avoid consuming trailing '_'
                    reader.Position <- backtrackTo
                    preturn () reader
                else
                    fail expectedOneDigit reader

        let private pManyIntCharsWithBase isDigitInBase (reader: Reader<char, LexBuilder, ReadableString>) =
            pManyIntCharsWithBaseLoop false isDigitInBase reader.Position reader

        let pIntBase = pManyIntCharsWithBase isDecimalDigit
        let pHexBase = pManyIntCharsWithBase isHexDigit
        let pOctalBase = pManyIntCharsWithBase isOctalDigit
        let pBinaryBase = pManyIntCharsWithBase isBinaryDigit

        let pXIntBase (reader: Reader<char, LexBuilder, ReadableString>) =
            // Parses an integer with optional base prefix
            // Returns the StringBuilder with the digits and the numeric base
            // Allows underscores in the digits
            let span = reader.PeekN(3)

            match span.Length with
            | 0 -> fail EndOfInput reader
            | 1 ->
                if isDecimalDigit span[0] then
                    reader.Skip()
                    preturn NumericBase.Decimal reader
                else
                    fail expectedBasePrefix reader
            | 2 ->
                if isDecimalDigit span[0] then
                    // We need at least 3 chars to have a base prefix
                    match pIntBase reader with
                    | Ok() -> preturn NumericBase.Decimal reader
                    | Error e -> invalidOp $"Unreachable error parsing decimal number: {e}"
                else
                    fail expectedBasePrefix reader
            | _ ->
                // Check for base prefix and at least one digit in that base
                match span[0], span[1], span[2] with
                | '0', ('x' | 'X' as c), c2 when isHexDigit c2 ->
                    reader.SkipN(2)

                    match pHexBase reader with
                    | Ok() -> preturn NumericBase.Hex reader
                    | Error e -> invalidOp $"Unreachable error parsing hexadecimal number: {e}"

                | '0', ('o' | 'O' as c), c2 when isOctalDigit c2 ->
                    reader.SkipN(2)

                    match pOctalBase reader with
                    | Ok() -> preturn NumericBase.Octal reader
                    | Error e -> invalidOp $"Unreachable error parsing octal number: {e}"

                | '0', ('b' | 'B' as c), c2 when isBinaryDigit c2 ->
                    reader.SkipN(2)

                    match pBinaryBase reader with
                    | Ok() -> preturn NumericBase.Binary reader
                    | Error e -> invalidOp $"Unreachable error parsing binary number: {e}"

                | c, _, _ when isDecimalDigit c ->
                    // Decimal base
                    match pIntBase reader with
                    | Ok() -> preturn NumericBase.Decimal reader
                    | Error e -> invalidOp $"Unreachable error parsing decimal number: {e}"

                | _ -> fail expectedBasePrefix reader

        // Span-based dispatch — avoids allocating a suffix string per numeric
        // literal. The valid suffix length is ≤2; longer runs of identifier
        // chars collapse to `ReservedNumericLiteral` regardless of content.
        let private getIntTokenFromSpan (numBase: NumericBase) (suffix: ReadOnlySpan<char>) =
            let token =
                match suffix.Length with
                | 0 -> Token.NumInt32
                | 1 ->
                    match suffix.[0] with
                    | 'y' -> Token.NumSByte
                    | 's' -> Token.NumInt16
                    | 'l' -> Token.NumInt32
                    | 'u' -> Token.NumUInt32
                    | 'n' -> Token.NumNativeInt
                    | 'L' -> Token.NumInt64
                    | 'Q' ->
                        match numBase with
                        | NumericBase.Decimal -> Token.NumBigIntegerQ
                        | _ -> Token.ReservedNumericLiteral
                    | 'R' ->
                        match numBase with
                        | NumericBase.Decimal -> Token.NumBigIntegerR
                        | _ -> Token.ReservedNumericLiteral
                    | 'Z' ->
                        match numBase with
                        | NumericBase.Decimal -> Token.NumBigIntegerZ
                        | _ -> Token.ReservedNumericLiteral
                    | 'I' ->
                        match numBase with
                        | NumericBase.Decimal -> Token.NumBigIntegerI
                        | _ -> Token.ReservedNumericLiteral
                    | 'N' ->
                        match numBase with
                        | NumericBase.Decimal -> Token.NumBigIntegerN
                        | _ -> Token.ReservedNumericLiteral
                    | 'G' ->
                        match numBase with
                        | NumericBase.Decimal -> Token.NumBigIntegerG
                        | _ -> Token.ReservedNumericLiteral
                    | 'm'
                    | 'M' ->
                        // Integer decimals only; float decimals also use m/M
                        // suffix but are handled in the float parser.
                        match numBase with
                        | NumericBase.Decimal -> Token.NumDecimal
                        | _ -> Token.ReservedNumericLiteral
                    | 'f'
                    | 'F' ->
                        // Float suffixes are only valid for decimal base.
                        match numBase with
                        | NumericBase.Decimal -> Token.NumIEEE32
                        | _ -> Token.ReservedNumericLiteral
                    | _ -> Token.ReservedNumericLiteral
                | 2 ->
                    // Nested match on the two chars avoids allocating a
                    // reference tuple for the outer `match c0, c1 with` form.
                    match suffix.[0] with
                    | 'u' ->
                        match suffix.[1] with
                        | 'y' -> Token.NumByte
                        | 's' -> Token.NumUInt16
                        | 'l' -> Token.NumUInt32
                        | 'n' -> Token.NumUNativeInt
                        | 'L' -> Token.NumUInt64
                        | _ -> Token.ReservedNumericLiteral
                    | 'U' ->
                        match suffix.[1] with
                        | 'L' -> Token.NumUInt64
                        | _ -> Token.ReservedNumericLiteral
                    | 'L' ->
                        match suffix.[1] with
                        | 'F' ->
                            match numBase with
                            | NumericBase.Decimal -> Token.ReservedNumericLiteral
                            | _ -> Token.NumIEEE64
                        | _ -> Token.ReservedNumericLiteral
                    | 'l' ->
                        match suffix.[1] with
                        | 'f' ->
                            match numBase with
                            | NumericBase.Decimal -> Token.ReservedNumericLiteral
                            | _ -> Token.NumIEEE32
                        | _ -> Token.ReservedNumericLiteral
                    | _ -> Token.ReservedNumericLiteral
                | _ -> Token.ReservedNumericLiteral

            // Combine the base into the token
            let numBase = uint16 numBase <<< TokenRepresentation.NumericBaseShift
            Token.ofUInt16 (uint16 token ||| numBase)

        let private getDecimalFloatTokenFromSpan (suffix: ReadOnlySpan<char>) =
            match suffix.Length with
            | 0 -> Token.NumIEEE64
            | 1 ->
                match suffix.[0] with
                | 'f'
                | 'F' -> Token.NumIEEE32
                | 'm'
                | 'M' -> Token.NumDecimal
                | _ -> Token.ReservedNumericLiteral
            | _ -> Token.ReservedNumericLiteral

        /// Consumes a run of identifier characters (the numeric-literal suffix)
        /// and returns its span. Zero chars is a success.
        let inline private skipSuffixAndGetSpan (reader: Reader<char, LexBuilder, ReadableString>) =
            let startIdx = int reader.Position.Index
            let mutable more = true

            while more do
                match reader.Peek() with
                | ValueSome c when isIdentChar c -> reader.Skip()
                | _ -> more <- false

            let len = int reader.Position.Index - startIdx
            reader.State.Source.AsSpan(startIdx, len)

        let private parseDecimalIntToken (reader: Reader<char, LexBuilder, ReadableString>) =
            let span = skipSuffixAndGetSpan reader
            preturn (getIntTokenFromSpan NumericBase.Decimal span) reader

        let private parseDecimalFloatSuffixToken (reader: Reader<char, LexBuilder, ReadableString>) =
            let span = skipSuffixAndGetSpan reader
            preturn (getDecimalFloatTokenFromSpan span) reader

        let private parseDecimalExpFloatToken =
            let choices =
                choiceL [ pIntBase; (anyOf "+-") >>. pIntBase ] "parseDecimalExpFloatToken"

            parser {
                let! e = anyOf "eE"
                let! expPart = choices
                let! token = parseDecimalFloatSuffixToken
                return token
            }

        let private parseDecimalFracFloatToken =
            let choices =
                choiceL [ parseDecimalExpFloatToken; parseDecimalFloatSuffixToken ] "parseDecimalFracFloatToken"

            parser {
                let! dot = pchar '.' .>> notFollowedBy (pchar '.')
                let! fracPart = opt pIntBase
                return! choices
            }

        let private parseDecimalToken =
            choiceL [ parseDecimalFracFloatToken; parseDecimalExpFloatToken; parseDecimalIntToken ] "parseDecimalToken"

        let private parseIntSuffixToken (numBase: NumericBase) (reader: Reader<char, LexBuilder, ReadableString>) =
            let span = skipSuffixAndGetSpan reader
            preturn (getIntTokenFromSpan numBase span) reader

        let parseToken =
            parser {
                let! pos = getPosition
                let! numBase = pXIntBase

                match numBase with
                | NumericBase.Decimal ->
                    let! token = parseDecimalToken
                    do! updateUserState (LexBuilder.append token pos CtxOp.NoOp)
                | _ ->
                    // Non-decimal base, must be integer (or integral float "1F", "1M", "0x1LF")
                    let! token = parseIntSuffixToken numBase
                    do! updateUserState (LexBuilder.append token pos CtxOp.NoOp)
            }

    module Operator =
        open System.Collections.Concurrent
        let generatedNameCache = ConcurrentDictionary<string, string>()

        let standardOperators =
            Map.ofList
                [
                    "[]", "op_Nil"
                    "::", "op_Cons"
                    "+", "op_Addition"
                    "-", "op_Subtraction"
                    "*", "op_Multiply"
                    "**", "op_Exponentiation"
                    "/", "op_Division"
                    "@", "op_Append"
                    "^", "op_Concatenate"
                    "%", "op_Modulus"
                    "&&&", "op_BitwiseAnd"
                    "|||", "op_BitwiseOr"
                    "^^^", "op_ExclusiveOr"
                    "<<<", "op_LeftShift"
                    "~~~", "op_LogicalNot"
                    ">>>", "op_RightShift"
                    "~+", "op_UnaryPlus"
                    "~-", "op_UnaryNegation"
                    "=", "op_Equality"
                    "<=", "op_LessThanOrEqual"
                    ">=", "op_GreaterThanOrEqual"
                    "<", "op_LessThan"
                    ">", "op_GreaterThan"
                    "?", "op_Dynamic"
                    "?<-", "op_DynamicAssignment"
                    "|>", "op_PipeRight"
                    "<|", "op_PipeLeft"
                    "!", "op_Dereference"
                    ">>", "op_ComposeRight"
                    "<<", "op_ComposeLeft"
                    // "<@ @>", "op_Quotation"
                    // "<@@ @@>", "op_QuotationUntyped"
                    "+=", "op_AdditionAssignment"
                    "-=", "op_SubtractionAssignment"
                    "*=", "op_MultiplyAssignment"
                    "/=", "op_DivisionAssignment"
                    "..", "op_Range"
                // ".. ..", "op_RangeStep"
                ]

        let generateOperatorName (t: Token) (operatorText: string) =
            if operatorText.Length = 0 then
                invalidArg "operatorText" "Operator text cannot be empty."

            if not t.IsOperator then
                invalidArg "t" (sprintf "Token %A is not an operator token." t)

            if t.IsKeyword then
                operatorText // Keywords are not renamed
            else
                let getCharName =
                    function
                    | '>' -> "Greater"
                    | '<' -> "Less"
                    | '+' -> "Plus"
                    | '-' -> "Minus"
                    | '*' -> "Multiply"
                    | '/' -> "Divide"
                    | '=' -> "Equals"
                    | '~' -> "Twiddle"
                    | '$' -> "Dollar"
                    | '%' -> "Percent"
                    | '.' -> "Dot"
                    | '&' -> "Amp"
                    | '|' -> "Bar"
                    | '@' -> "At"
                    | '^' -> "Hat"
                    | '!' -> "Bang"
                    | '?' -> "Qmark"
                    | '(' -> "LParen"
                    | ',' -> "Comma"
                    | ')' -> "RParen"
                    | '[' -> "LBrack"
                    | ']' -> "RBrack"
                    | ':' -> "Colon"
                    // Handle unsupported characters gracefully.
                    | c -> invalidArg "operator" (sprintf "Unsupported character '%c' in operator." c)

                match Map.tryFind operatorText standardOperators with
                | Some name -> name
                | None ->
                    generatedNameCache.GetOrAdd(
                        operatorText,
                        fun operator ->
                            seq {
                                "op_"

                                for c in operator do
                                    getCharName c
                            }
                            |> String.Concat
                    )

    [<AutoOpen>]
    module internal FormatStrings =

        let pFormatType (reader: Reader<char, 'State, 'Input>) =
            match reader.Peek() with
            | ValueNone -> fail EndOfInput reader
            | ValueSome 'b' ->
                reader.Skip()
                preturn FormatType.Bool reader
            | ValueSome 's' ->
                reader.Skip()
                preturn FormatType.String reader
            | ValueSome 'c' ->
                reader.Skip()
                preturn FormatType.Char reader
            | ValueSome('d' | 'i') ->
                reader.Skip()
                preturn FormatType.DecimalInt reader
            | ValueSome 'u' ->
                reader.Skip()
                preturn FormatType.UnsignedDecimalInt reader
            | ValueSome('x' | 'X') ->
                reader.Skip()
                preturn FormatType.UnsignedHex reader
            | ValueSome 'o' ->
                reader.Skip()
                preturn FormatType.UnsignedOctal reader
            | ValueSome 'B' ->
                reader.Skip()
                preturn FormatType.UnsignedBinary reader
            | ValueSome('e' | 'E') ->
                reader.Skip()
                preturn FormatType.FloatExponential reader
            | ValueSome('f' | 'F') ->
                reader.Skip()
                preturn FormatType.FloatDecimal reader
            | ValueSome('g' | 'G') ->
                reader.Skip()
                preturn FormatType.FloatCompact reader
            | ValueSome 'M' ->
                reader.Skip()
                preturn FormatType.Decimal reader
            | ValueSome 'O' ->
                reader.Skip()
                preturn FormatType.Object reader
            | ValueSome 'A' ->
                reader.Skip()
                preturn FormatType.Structured reader
            | ValueSome 'a' ->
                reader.Skip()
                preturn FormatType.FormatFunction reader
            | ValueSome 't' ->
                reader.Skip()
                preturn FormatType.Text reader
            | ValueSome c -> fail (Unexpected c) reader

        let lFormatPlaceholder: Parser<_, _, _, ReadableString> =
            parser {
                let! state = getUserState
                // let level = LexBuilder.level state
                // do! skipNOf level '%'
                let! flags = manyChars (anyOf "0+- ")
                let! width = opt pbigint
                let! precision = opt (pchar '.' >>. pbigint)
                let! typeChar = pFormatType

                return
                    {
                        Flags = flags
                        Width = width
                        Precision = precision
                        Type = typeChar
                    }
            }

        let lFormatPlaceholderToken =
            lFormatPlaceholder >>% Token.FormatPlaceholder
            <|> preturn Token.InvalidFormatPlaceholder

        // TODO: Rewrite to use updateUserState instead of directly manipulating the token list
        // This is tricky because we need to add multiple tokens in some cases
        let pFormatSpecifierTokens: Parser<unit, _, _, ReadableString> =
            parser {
                let! pos = getPosition
                let! percentCount = pPeekCountPercents
                let! state = getUserState
                let tokens = state.Tokens

                let addToken (token: Token) (idx: int) =
                    let token =
                        if state.IsInBlockComment || state.IsInOCamlBlockComment then
                            uint16 token ||| TokenRepresentation.InComment |> Token.ofUInt16
                        else
                            token

                    tokens.Add(PositionedToken.Create(token, idx))

                match LexBuilder.level state with
                | 1 ->
                    // Level 1 logic
                    let count, idx =
                        let mutable count = percentCount
                        let mutable idx = pos.Index

                        while count > 1 do
                            // %% is an escape sequence for '%'
                            addToken Token.EscapePercent idx
                            idx <- idx + 2
                            count <- count - 2

                        count, idx

                    match count with
                    | 0 ->
                        // All percents consumed as %% escape pairs
                        do! skipN percentCount
                        return ()
                    | _ ->
                        // reads flags/width/precision/type after the final '%'
                        do! skipN percentCount
                        let! t = lFormatPlaceholderToken
                        addToken t idx
                        return ()

                | level ->

                    // Level 2 or higher logic, impossible to get 0 or negative since peek count > 0
                    let count = percentCount
                    let leading = count - level
                    let idx = pos.Index

                    if leading < 0 then
                        addToken Token.InterpolatedStringFragment idx
                        do! skipN count
                        return ()
                    elif leading = 0 then
                        // Exactly enough to start a FormatPlaceholder
                        do! skipN count
                        let! t = lFormatPlaceholderToken
                        addToken t idx
                        return ()
                    elif leading >= level then
                        // Too many leading '%'
                        do! skipN count
                        addToken Token.InvalidFormatPercents idx
                        return ()
                    else
                        addToken Token.InterpolatedStringFragment idx
                        do! skipN leading
                        let! pos = getPosition
                        let! t = lFormatPlaceholderToken
                        addToken t pos.Index
                        return ()
            }

    let (|ExpressionCtx|_|) (ctx: LexContext) =
        match ctx with
        | LexContext.Normal
        | LexContext.BracedExpression
        | LexContext.BraceBarExpression
        | LexContext.ParenthesExpression
        | LexContext.BracketedExpression
        | LexContext.QuotedExpression
        | LexContext.TypedQuotedExpression
        | LexContext.InterpolatedExpression -> true
        | _ -> false

    let (|NonInterpolatedExpressionCtx|_|) (ctx: LexContext) =
        match ctx with
        | LexContext.Normal
        | LexContext.BracedExpression
        | LexContext.BraceBarExpression
        | LexContext.ParenthesExpression
        | LexContext.BracketedExpression
        | LexContext.QuotedExpression
        | LexContext.TypedQuotedExpression -> true
        | _ -> false

    let pTabToken = pToken (pchar '\t') Token.Tab
    let pCommaToken = pToken (pchar ',') Token.OpComma

    let pLParenToken =
        // Dispatch on span[1]: `*` → block-comment / ML-compat prefixes, then (*) then (*;
        //                     `#` → (# IL intrinsic; else (  expression open (hot path).
        // For the `*` branch, PeekN(15) once and use span.StartsWith on each alternative
        // rather than letting each pstring do its own PeekN.
        fun (reader: Reader<char, LexBuilder, ReadableString>) ->
            let span = reader.PeekN(15)

            if span.Length < 2 then
                pOpenParenExpressionContext reader
            else
                match span.[1] with
                | '*' ->
                    let pos = reader.Position

                    if span.StartsWith("(*ENDIF-OCAML*)".AsSpan()) then
                        reader.SkipN(15)
                        reader.State <- LexBuilder.append Token.EndOCamlBlockComment pos CtxOp.NoOp reader.State
                        Ok()
                    elif span.StartsWith("(*IF-OCAML*)".AsSpan()) then
                        reader.SkipN(12)

                        reader.State <- LexBuilder.append Token.StartOCamlBlockComment pos CtxOp.NoOp reader.State

                        Ok()
                    elif span.StartsWith("(*IF-FSHARP".AsSpan()) then
                        reader.SkipN(11)

                        reader.State <- LexBuilder.append Token.StartFSharpBlockComment pos CtxOp.NoOp reader.State

                        Ok()
                    elif span.StartsWith("(*F#".AsSpan()) then
                        reader.SkipN(4)

                        reader.State <- LexBuilder.append Token.StartFSharpBlockComment pos CtxOp.NoOp reader.State

                        Ok()
                    elif span.StartsWith("(*)".AsSpan()) then
                        // Parenthesized * operator — emits 3 tokens (LParen, *, RParen).
                        let idx = int pos.Index
                        reader.SkipN(3)

                        reader.State <-
                            reader.State
                            |> LexBuilder.appendI Token.KWLParen idx (CtxOp.Push LexContext.ParenthesExpression)
                            |> LexBuilder.appendI Token.OpMultiply (idx + 1) CtxOp.NoOp
                            |> LexBuilder.appendI Token.KWRParen (idx + 2) (CtxOp.Pop LexContext.ParenthesExpression)

                        Ok()
                    else
                        // (* block comment start (span[1] = '*' guaranteed).
                        reader.SkipN(2)
                        reader.State <- LexBuilder.append Token.BlockCommentStart pos CtxOp.NoOp reader.State
                        Ok()
                | '#' ->
                    // (#  IL intrinsic literal opening
                    let pos = reader.Position
                    reader.SkipN(2)
                    reader.State <- LexBuilder.append Token.KWLHashParen pos CtxOp.NoOp reader.State
                    Ok()
                | _ -> pOpenParenExpressionContext reader

    let pLBacketToken =
        // [< (attr), [| (array), [ (default) — dispatch on second char.
        fun (reader: Reader<char, LexBuilder, ReadableString>) ->
            let span = reader.PeekN(2)

            if span.Length >= 2 then
                match span.[1] with
                | '<' ->
                    let pos = reader.Position
                    reader.SkipN(2)
                    reader.State <- LexBuilder.append Token.KWLAttrBracket pos CtxOp.NoOp reader.State
                    Ok()
                | '|' ->
                    let pos = reader.Position
                    reader.SkipN(2)
                    reader.State <- LexBuilder.append Token.KWLArrayBracket pos CtxOp.NoOp reader.State
                    Ok()
                | _ -> pOpenBracketExpressionContext reader
            else
                pOpenBracketExpressionContext reader

    let pGreaterThanToken =
        // >] (measure/attr close) vs operator — dispatch on second char.
        fun (reader: Reader<char, LexBuilder, ReadableString>) ->
            let span = reader.PeekN(2)

            if span.Length >= 2 && span.[1] = ']' then
                let pos = reader.Position
                reader.SkipN(2)
                reader.State <- LexBuilder.append Token.KWRAttrBracket pos CtxOp.NoOp reader.State
                Ok()
            else
                pOperatorToken reader

    let pDoubleQuoteToken =
        // Peek 3 chars: """ (triple) vs " (single). Span-dispatch avoids the
        // pstring "\"\"\"" PeekN inside the choiceL's first alternative.
        fun (reader: Reader<char, LexBuilder, ReadableString>) ->
            let span = reader.PeekN(3)

            if span.Length >= 3 && span.[1] = '"' && span.[2] = '"' then
                pString3OpenToken reader
            else
                pStringOpenToken reader

    let pSingleQuoteToken =
        choiceL [ pCharToken; pTypeParamToken; pToken (pchar '\'') Token.KWSingleQuote ] "Single quote"

    let pDollarToken =
        choiceL
            [
                pInterpolated3StartToken
                pInterpolatedStringStartToken
                pVerbatimInterpolatedStartToken
                // '$' is lexed as an operator char but is not permitted as a character in operator names and is reserved for future use
                pOperatorToken
            ]
            "Interpolated string or operator"

    let pAtToken =
        // @$" → verbatim interpolated; @" → verbatim string; else operator.
        fun (reader: Reader<char, LexBuilder, ReadableString>) ->
            let span = reader.PeekN(3)

            if span.Length >= 3 && span.[1] = '$' && span.[2] = '"' then
                pVerbatimInterpolatedStartToken reader
            elif span.Length >= 2 && span.[1] = '"' then
                pVerbatimStringOpenToken reader
            else
                pOperatorToken reader

    let pSlashToken =
        // 3.2 Comments — dispatch on next char instead of try/fallback via choiceL.
        fun (reader: Reader<char, LexBuilder, ReadableString>) ->
            let state = reader.State
            let inComment = state.IsInBlockComment || state.IsInOCamlBlockComment

            if inComment then
                // Inside a block comment, '/' is just an operator char (line comments don't exist).
                pOperatorToken reader
            else
                let span = reader.PeekN(2)

                if span.Length >= 2 && span.[1] = '/' then
                    pLineCommentToken reader
                else
                    pOperatorToken reader

    let pSemicolonToken =
        // We know span[0] = ';' from the outer dispatch; check span[1] for `;;`.
        fun (reader: Reader<char, LexBuilder, ReadableString>) ->
            let pos = reader.Position
            let span = reader.PeekN(2)

            if span.Length >= 2 && span.[1] = ';' then
                reader.SkipN(2)
                reader.State <- LexBuilder.append Token.OpDoubleSemicolon pos CtxOp.NoOp reader.State
                Ok()
            else
                reader.Skip()
                reader.State <- LexBuilder.append Token.OpSemicolon pos CtxOp.NoOp reader.State
                Ok()

    let pDotToken =
        // Special dot operators (.[], .[]<-, .(), .()<-, etc.) all have `[` or `(` as the second char.
        // Skip the trial if it can't possibly match.
        fun (reader: Reader<char, LexBuilder, ReadableString>) ->
            let span = reader.PeekN(2)

            if span.Length >= 2 && (span.[1] = '[' || span.[1] = '(') then
                match pSpecialDotOperatorToken reader with
                | Ok() -> Ok()
                | Error _ -> pOperatorToken reader
            else
                pOperatorToken reader

    let pCustomOperatorToken =
        // The only prefix-specific alternatives start with `|` or `*` — dispatch on span[0..1]
        // to skip 3 trial parses on every operator char.
        fun (reader: Reader<char, LexBuilder, ReadableString>) ->
            let span = reader.PeekN(2)

            if span.Length >= 2 then
                let c0 = span.[0]
                let c1 = span.[1]

                if c0 = '|' && c1 = ']' then
                    let pos = reader.Position
                    reader.SkipN(2)
                    reader.State <- LexBuilder.append Token.KWRArrayBracket pos CtxOp.NoOp reader.State
                    Ok()
                elif c0 = '|' && c1 = '}' then
                    pCloseBraceBarExpressionContext reader
                elif c0 = '*' && c1 = ')' then
                    let pos = reader.Position
                    reader.SkipN(2)
                    reader.State <- LexBuilder.append Token.BlockCommentEnd pos CtxOp.NoOp reader.State
                    Ok()
                else
                    pOperatorToken reader
            else
                pOperatorToken reader
    // TODO: Preprocessor directives
    // 3.3 Conditional Compilation
    // | IfDirective = (9us) // #if if-expression-text
    // | ElseDirective = (10us) // #else
    // | EndIfDirective = (11us) // #endif

    // // 3.8.4 Shebang
    // | Shebang = (12us) // #!/bin/usr/env fsharpi --exec

    // // 3.9 Line Directives
    // | LineIntDirective = (13us) // # int
    // | LineStringDirective = (14us) // # int string
    // | LineVerbatimStringDirective = (15us) // # int verbatim-string
    // | LineLineIntDirective = (16us) // #line int
    // | LineLineStringDirective = (17us) // #line int string
    // | LineLineVerbatimStringDirective = (18us) // #line int verbatim-string
    // | InvalidDirective = (IsInvalid ||| 19us) // Any other invalid directive starting with #

    let private isAtStartOfLineOrIndent (reader: Reader<char, LexBuilder, ReadableString>) =
        let pos = reader.Position.Index
        let state = reader.State

        if state.AtStartOfLine then
            preturn true reader
        else
            let tokens = state.Tokens

            if tokens.Count = 0 then
                // No previous tokens, must be at start of input
                preturn (pos = 0) reader
            else
                let lastToken = tokens[tokens.Count - 1]
                preturn (lastToken.Token = Token.Indent) reader

    let private directives =
        [|
            // 3.3 Conditional Compilation
            "#if", Token.IfDirective
            "#else", Token.ElseDirective
            "#endif", Token.EndIfDirective
            // 3.8.4 Shebang
            "#!", Token.Shebang
            // 3.9 Line Directives
            "#line", Token.LineDirective
            // 12.4 Compiler Directives
            "#nowarn", Token.NoWarnDirective
            "#warnon", Token.WarnOnDirective
            "#r", Token.ReferenceDirective
            "#reference", Token.ReferenceDirective
            "#I", Token.IncludePathDirective
            "#Include", Token.IncludePathDirective
            "#load", Token.LoadDirective
            "#time", Token.TimeDirective
            "#help", Token.HelpDirective
            "#q", Token.QuitDirective
            "#quit", Token.QuitDirective
            // 19.4 File Extensions and Lexical Matters
            "#indent", Token.IndentDirective
        |]

    let pDirectiveToken =
        // Hoist allocations: the choiceL and its sub-parsers would otherwise be
        // rebuilt on every invocation of the parser CE body.
        let pSkipSpaces1 = skipMany1Satisfies isSpace
        let pSkipDecimalDigits1 = skipMany1Satisfies NumericLiterals.isDecimalDigit
        let pSkipIdentChars1 = skipMany1Satisfies isIdentChar

        let pDirectiveChoice =
            choiceL
                [
                    anyStringReturn directives
                    // # int
                    // We just peek the int to distinguish from InvalidDirective
                    // The actual int is lexed to a separate token
                    tuple3 (pchar '#') pSkipSpaces1 (followedBy pSkipDecimalDigits1)
                    >>% Token.LineIntDirective
                    pchar '#' .>> pSkipIdentChars1 >>% Token.InvalidDirective
                ]
                "Directive"

        parser {
            let! atStart = isAtStartOfLineOrIndent

            if not atStart then
                return! fail expectedDirectiveAtStart
            else
                let! pos = getPosition
                let! token = pDirectiveChoice

                do!
                    updateUserState (fun x ->
                        match token with
                        | Token.IfDirective -> x |> LexBuilder.pushContext LexContext.IfDirective
                        | _ -> x
                        |> LexBuilder.append token pos CtxOp.NoOp
                    )
        }


    let pHashToken =
        // #) (IL intrinsic close) wins outright; otherwise try directive (start-of-line
        // check happens inside pDirectiveToken), fall back to bare #.
        fun (reader: Reader<char, LexBuilder, ReadableString>) ->
            let span = reader.PeekN(2)
            let pos = reader.Position

            if span.Length >= 2 && span.[1] = ')' then
                reader.SkipN(2)
                reader.State <- LexBuilder.append Token.KWRHashParen pos CtxOp.NoOp reader.State
                Ok()
            else
                match pDirectiveToken reader with
                | Ok() -> Ok()
                | Error _ ->
                    // Directive failed (not at start of line, or not a recognized directive).
                    // pDirectiveToken's body may have consumed — reset position.
                    reader.Position <- pos
                    reader.Skip()
                    reader.State <- LexBuilder.append Token.KWHash pos CtxOp.NoOp reader.State
                    Ok()

    // {| (anonymous record) vs { (braced expression) — dispatch on second char.
    let private pLBraceExpressionToken =
        fun (reader: Reader<char, LexBuilder, ReadableString>) ->
            let span = reader.PeekN(2)

            if span.Length >= 2 && span.[1] = '|' then
                pOpenBraceBarExpressionContext reader
            else
                pOpenBraceExpressionContext reader

    // Fast-path dispatch for the dominant ExpressionCtx (non-Interpolated) bucket:
    // String.IndexOf(char) is SIMD-vectorized; a hit returns an index into the
    // parallel `exprCtxDispatchers` array. The pair list keeps the two in sync.
    //
    // Order is by expected first-char frequency in idiomatic F# source: the top 16
    // chars land in the first SIMD chunk (single vector comparison); rarer chars in
    // the tail pay one extra chunk scan. `:` moved to the first chunk (index 7) —
    // it's extremely common in type annotations (Small/Medium fixtures) and was the
    // index-21 regression after hoisting `pColonToken`.
    let private exprCtxDispatchChars, (exprCtxDispatchers: Parser<unit, char, LexBuilder, ReadableString> array) =
        [|
            // First SIMD chunk (indices 0..15)
            ' ', pIndentOrWhitespaceToken
            '\n', pNewlineToken
            '\r', pNewlineToken
            '.', pDotToken
            '(', pLParenToken
            ')', pCloseParenExpressionContext
            ',', pCommaToken
            ':', pColonToken
            ';', pSemicolonToken
            '"', pDoubleQuoteToken
            '[', pLBacketToken
            ']', pCloseBracketExpressionContext
            '{', pLBraceExpressionToken
            '}', pCloseBraceExpressionContext
            '>', pGreaterThanToken
            '/', pSlashToken
            // Tail (indices 16..21) — rarer chars, one extra SIMD chunk
            ''', pSingleQuoteToken
            '@', pAtToken
            '\t', pTabToken
            '$', pDollarToken
            '#', pHashToken
            '`', pBacktickedIdentifierToken
        |]
        |> Array.unzip
        |> fun (chars, parsers) -> String(chars), parsers

    let inline private dispatchExprCtx (c: char) =
        // ASCII letters / digits / underscore dominate expression-context chars
        // in real F# source. Short-circuit before the IndexOf + Contains + BCL chain.
        if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' then
            pIdentifierOrKeywordToken
        elif c >= '0' && c <= '9' then
            NumericLiterals.parseToken
        else
            let i = exprCtxDispatchChars.IndexOf(c)

            if i >= 0 then
                exprCtxDispatchers.[i]
            elif customOperatorChars.Contains c then
                pCustomOperatorToken
            elif isIdentStartChar c then
                // Non-ASCII ident start (rare).
                pIdentifierOrKeywordToken
            else
                pOtherToken

    [<TailCall>]
    let rec lex (reader: Reader<char, LexBuilder, ReadableString>) =
        let state = reader.State

        match reader.Peek() with
        | ValueNone -> Ok(LexBuilder.complete reader.Position.Index reader.State)

        | ValueSome c ->
            let ctx = LexBuilder.currentContext state
            // printfn "At %A, Context: %A, Next char: %A" reader.Position.Index ctx c
            let p =
                match ctx with
                // Hot path: vanilla expression contexts (no special handling for } or :).
                | LexContext.Normal
                | LexContext.BracedExpression
                | LexContext.BraceBarExpression
                | LexContext.ParenthesExpression
                | LexContext.BracketedExpression
                | LexContext.QuotedExpression
                | LexContext.TypedQuotedExpression -> dispatchExprCtx c

                | LexContext.InterpolatedExpression ->
                    // Same as vanilla ExpressionCtx except `}` ends the expression
                    // and `:` opens a format clause.
                    match c with
                    | '}' -> pInterpolatedExpressionEndToken
                    | ':' -> pInterpolatedFormatClause
                    | _ -> dispatchExprCtx c

                | LexContext.IfDirective ->
                    match c with
                    | '\r'
                    | '\n' -> IfDirective.pNewlineToken
                    | '(' -> IfDirective.pLParenToken
                    | ')' -> IfDirective.pRParenToken
                    | '&' -> IfDirective.pAnd
                    | '|' -> IfDirective.pOr
                    | '!' -> IfDirective.pNot
                    | ' ' -> IfDirective.pWhitespaceToken
                    | _ -> IfDirective.pIdentifierOrOther

                | LexContext.InterpolatedString ->
                    match c with
                    | '{' -> pInterpolatedExpressionStartToken
                    | '}' -> pInterpolatedStringFragmentRBraces
                    | '"' -> pInterpolatedStringEndToken
                    | '%' -> pFormatSpecifierTokens
                    | _ -> pInterpolatedStringFragmentToken

                | LexContext.VerbatimInterpolatedString ->
                    match c with
                    | '{' -> pInterpolatedExpressionStartToken
                    | '}' -> pInterpolatedStringFragmentRBraces
                    | '"' -> pVerbatimInterpolatedStringQuoteToken
                    | '%' -> pFormatSpecifierTokens
                    | _ -> pVerbatimInterpolatedStringFragmentToken

                | LexContext.Interpolated3String _ ->
                    match c with
                    | '{' -> pInterpolated3ExpressionStartToken
                    | '}' -> pInterpolated3StringFragmentRBraces
                    | '"' -> pInterpolated3QuoteOrFragment
                    | '%' -> pFormatSpecifierTokens
                    | _ -> pInterpolated3StringFragmentToken

                | LexContext.PlainString ->
                    match c with
                    | '"' -> pPlainStringCloseToken
                    | '\\' -> pStringEscapeToken
                    | '%' -> pFormatSpecifierTokens
                    | _ -> pPlainStringFragmentToken

                | LexContext.VerbatimString ->
                    match c with
                    | '"' -> pVerbatimStringQuoteToken2
                    | '%' -> pFormatSpecifierTokens
                    | _ -> pVerbatimStringFragmentToken2

                | LexContext.TripleQuotedString ->
                    match c with
                    | '"' -> pTripleStringQuoteOrFragment
                    | '%' -> pFormatSpecifierTokens
                    | _ -> pTripleStringFragmentToken

            match p reader with
            | Ok() -> lex reader
            | Error e -> Error e

    let lexString (input: string) =
        let reader = Reader.ofString input (LexBuilder.init input)
        lex reader
