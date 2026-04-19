namespace XParsec.FSharp.Lexer

open System
open System.Globalization
open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp

[<Measure>]
type token

[<Measure>]
type block

[<Measure>]
type line

[<Struct>]
type BlockInfo =
    {
        TokenIndex: int<token>
        IndentLevel: int
    }

type Lexed =
    {
        Tokens: ImmutableArrayM<PositionedToken, token>
        Blocks: ImmutableArrayM<BlockInfo, block>
        LineStarts: ImmutableArrayM<int<token>, line>
    }

    member this.FirstTokenOnLine(lineIndex: int<line>) =
        if lineIndex < 0<_> || int lineIndex >= this.LineStarts.Length then
            invalidArg (nameof lineIndex) "Index out of range"

        let tokenIndex = this.LineStarts[lineIndex]
        this.Tokens[tokenIndex]

    member this.FirstTokenOfBlock(blockIndex: int<block>) =
        if blockIndex < 0<_> || int blockIndex >= this.Blocks.Length then
            invalidArg (nameof blockIndex) "Index out of range"

        let tokenIndex = this.Blocks[blockIndex].TokenIndex
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

[<RequireQualifiedAccess>]
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
        Tokens: ImmutableArray<PositionedToken>.Builder
        mutable AtStartOfLine: bool
        mutable Context: LexContext list
        // Track whether we are inside a block comment
        // These flags are used to mark tokens as being inside a block comment
        // The lexer doesn't care if we are in a block comment or not
        // but flagging tokens allows the parser or later stages to handle them easily
        mutable IsInBlockComment: bool
        mutable IsInOCamlBlockComment: bool
        mutable LastTokenWasNewLine: int<token> voption
        LineStarts: ImmutableArray<int<token>>.Builder // indices of tokens that start lines
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

    let private computeBlocks
        (tokens: ImmutableArrayM<PositionedToken, token>)
        (lineStarts: ImmutableArrayM<int<token>, line>)
        =
        let blocks = ImmutableArray.CreateBuilder<BlockInfo>(lineStarts.Length / 4 + 1) // Rough estimate of number of blocks

        let rec firstNonTrivialOnLine iTok iTokEnd =
            // printfn $"    firstNonTrivialOnLine: iTok={iTok}, iTokEnd={iTokEnd}"
            if iTok = iTokEnd then
                None
            else
                // Line with only whitespace/comments doesn't change blocks
                // Empty line doesn't change blocks
                let token = tokens[iTok]

                if token.InComment then
                    firstNonTrivialOnLine (iTok + 1<_>) iTokEnd
                else
                    match token.TokenWithoutCommentFlags with
                    | Token.LineComment -> None // rest of line is comment, so no non-trivial token
                    | Token.Indent
                    | Token.Whitespace
                    | Token.EOF
                    | Token.Tab
                    | Token.BlockCommentStart
                    | Token.BlockCommentEnd
                    | Token.StartFSharpBlockComment
                    | Token.EndFSharpBlockComment
                    | Token.StartOCamlBlockComment
                    | Token.EndOCamlBlockComment -> firstNonTrivialOnLine (iTok + 1<_>) iTokEnd
                    | Token.Newline -> invalidOp "Unexpected Newline token in line"

                    | _ -> Some(iTok, token)

        let rec tryFindNonTriviaToken iLine iTok =
            // printfn $"  tryFindNonTriviaToken: iLine={iLine}, iTok={iTok}"

            // Last token on this line (inclusive)
            let iTokEnd =
                if iLine + 1<line> < lineStarts.LengthM then
                    lineStarts[iLine + 1<_>] - 1<_>
                else
                    tokens.LengthM - 1<_>

            match firstNonTrivialOnLine iTok iTokEnd with
            | Some(iTok, token) -> Some(iLine, iTok, token)
            | None ->
                let iLineNext = iLine + 1<_>

                if iLineNext < lineStarts.LengthM then
                    // try next line
                    tryFindNonTriviaToken iLineNext (lineStarts[iLineNext])
                else
                    None

        let rec findBlocks iLine currentIndent =
            // printfn $"findBlocks: iLine={iLine}, currentIndent={currentIndent}"
            if iLine = lineStarts.LengthM then
                ImmutableArrayM(blocks.ToImmutable())
            elif iLine > lineStarts.LengthM then
                invalidOp "Line index out of range"
            else
                let iTok = lineStarts[iLine]

                match tryFindNonTriviaToken iLine iTok with
                | None -> ImmutableArrayM(blocks.ToImmutable())
                | Some(iLineNext, iTokNext, token) ->
                    let thisIndent =
                        let firstTok = tokens[lineStarts[iLineNext]]
                        token.StartIndex - firstTok.StartIndex

                    if thisIndent <> currentIndent then
                        // printfn $"  New block at line {iLine}, token {iTokNext}: {token}, indent {thisIndent}"
                        // We add iTok not iTokNext here, as the block starts at
                        // the first token on the line (including leading trivia)
                        blocks.Add
                            {
                                TokenIndex = iTok
                                IndentLevel = int thisIndent
                            }

                        findBlocks (iLineNext + 1<_>) thisIndent
                    else
                        findBlocks (iLineNext + 1<_>) currentIndent


        match tryFindNonTriviaToken 0<line> 0<token> with
        | None ->
            blocks.Add { TokenIndex = 0<_>; IndentLevel = 0 }
            ImmutableArrayM(blocks.ToImmutable())
        | Some(iLineNext, iTok, token) ->
            let thisIndent = token.StartIndex - tokens[lineStarts[iLineNext]].StartIndex
            // printfn $"  New block at line 0, token {iTok}: {token}, indent {thisIndent}"
            blocks.Add
                {
                    TokenIndex = 0<_>
                    IndentLevel = int thisIndent
                }

            findBlocks (iLineNext + 1<_>) thisIndent

    let private emitUnterminatedStrings idx (state: LexBuilder) =
        let rec unwindContext (ctx: LexContext list) =
            // TODO: Handle other unclosed contexts (e.g. unterminated #if) — currently we just ignore them and let the parser handle any resulting errors
            match ctx with
            | [] -> ()
            | LexContext.PlainString :: rest ->
                state.Tokens.Add(PositionedToken.Create(Token.UnterminatedStringLiteral, idx))
                unwindContext rest
            | LexContext.VerbatimString :: rest ->
                state.Tokens.Add(PositionedToken.Create(Token.UnterminatedVerbatimStringLiteral, idx))
                unwindContext rest
            | LexContext.TripleQuotedString :: rest ->
                state.Tokens.Add(PositionedToken.Create(Token.UnterminatedString3Literal, idx))
                unwindContext rest
            | LexContext.InterpolatedString :: rest
            | LexContext.VerbatimInterpolatedString :: rest
            | LexContext.Interpolated3String _ :: rest ->
                state.Tokens.Add(PositionedToken.Create(Token.UnterminatedInterpolatedString, idx))
                unwindContext rest
            | _ :: rest ->
                // InterpolatedExpression, BracedExpression, ParenthesExpression, etc.
                // are expression contexts nested inside an interpolated string — skip past them
                unwindContext rest

        unwindContext state.Context

    let complete idx (state: LexBuilder) =
        emitUnterminatedStrings idx state
        state.Tokens.Add(PositionedToken.Create(Token.EOF, idx))
        let tokens = ImmutableArrayM(state.Tokens.ToImmutable())

        let lineStarts =
            match state.LastTokenWasNewLine with
            | ValueSome newlineIdx ->
                state.LineStarts.Add(newlineIdx + 1<_>)
                state.LastTokenWasNewLine <- ValueNone
            | ValueNone -> ()

            ImmutableArrayM(state.LineStarts.ToImmutable())

        let blocks = computeBlocks tokens lineStarts

        {
            Tokens = tokens
            LineStarts = lineStarts
            Blocks = blocks
        }

    let init (input: string) =
        let x =
            {
                Source = input
                Tokens = ImmutableArray.CreateBuilder()
                AtStartOfLine = true
                Context = []
                IsInBlockComment = false
                IsInOCamlBlockComment = false
                LastTokenWasNewLine = ValueNone
                LineStarts = ImmutableArray.CreateBuilder()
            }

        x.LineStarts.Add(0<_>) // The first line starts at the beginning of the file
        x

    let currentContext (x: LexBuilder) =
        match x.Context with
        | [] -> LexContext.Normal
        | ctx :: _ -> ctx

    let pushContext (ctx: LexContext) (x: LexBuilder) =
        x.Context <- ctx :: x.Context
        x

    let popExactContext expected (x: LexBuilder) =
        // Only pop if the expected context is on top of the stack
        // In other cases, leave the context unchanged
        // In lexing, we may encounter unmatched closing braces etc.
        match x.Context with
        | [] -> x // Cannot pop context from empty context stack
        | ctx :: ctxTail when ctx = expected ->
            x.Context <- ctxTail
            x
        | _ -> x // Cannot pop context {expected} from stack

    let level (x: LexBuilder) =
        let rec findLevel ctx =
            match ctx with
            | [] -> invalidOp "Cannot get level from empty context stack"
            | ctx :: tail ->
                match ctx with
                | LexContext.Interpolated3String level -> level
                | LexContext.InterpolatedString -> 1
                | LexContext.VerbatimInterpolatedString -> 1
                | LexContext.PlainString -> 1
                | LexContext.VerbatimString -> 1
                | LexContext.TripleQuotedString -> 1
                | _ -> findLevel tail

        findLevel x.Context

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
        if not token.IsNumeric then
            false
        else
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

    let appendI token (idx: int) ctxOp (state: LexBuilder) =
        // Coalesce adjacent string fragments
        // ADJACENT_PREFIX_OP handling is done in the parser (isAdjacentPrefixOp in ExpressionParsing.fs)
        // https://fsharp.github.io/fslang-spec/lexical-analysis/#381-post-filtering-of-adjacent-prefix-tokens
        let tokenCount = state.Tokens.Count
        let tokenIdx = tokenCount * 1<token>

        let addToken =
            if tryMergeNegativeLiteral token idx state then
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

    let append token pos ctxOp (state: LexBuilder) =
        appendI token (int pos.Index) ctxOp state


[<AutoOpen>]
module internal Errors =
    // Centralized error messages
    // Use constants where possible
    let expectedEndOfIdent = Message "Expected end of identifier"

    let expectedNewline = Message "Expected newline"

    let expectedStringLiteral = Message "Expected string literal"

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

        fun (reader: Reader<char, _, ReadableString, _>) ->
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

        fun (reader: Reader<char, _, ReadableString, _>) ->
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
    let private peekCountSatisfies (predicate: char -> bool) (reader: Reader<char, LexBuilder, ReadableString, _>) =
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
            do! updateUserState (LexBuilder.append token pos CtxOp.NoOp)
        }

    let private pTokenPushCtx p token ctx =
        parser {
            let! pos = getPosition
            let! _ = p
            do! updateUserState (LexBuilder.append token pos (CtxOp.Push ctx))
        }

    let private pTokenPopCtx p token ctx =
        parser {
            let! pos = getPosition
            let! _ = p
            do! updateUserState (LexBuilder.append token pos (CtxOp.Pop ctx))
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

    let pIdentifierOrKeywordToken (reader: Reader<char, LexBuilder, ReadableString, _>) =
        let pos = reader.Position
        let source = reader.State.Source
        let startIdx = int pos.Index

        // Check F#*) sentinel first
        let peek1 = reader.PeekN(fSharpBlockCommentEnd1.Length)

        if
            peek1.Length = fSharpBlockCommentEnd1.Length
            && peek1.SequenceEqual(fSharpBlockCommentEnd1.AsSpan())
        then
            reader.SkipN(fSharpBlockCommentEnd1.Length)
            reader.State <- LexBuilder.append Token.EndFSharpBlockComment pos CtxOp.NoOp reader.State
            preturn () reader
        else
            // Check ENDIF-FSHARP*) sentinel
            let peek2 = reader.PeekN(fSharpBlockCommentEnd2.Length)

            if
                peek2.Length = fSharpBlockCommentEnd2.Length
                && peek2.SequenceEqual(fSharpBlockCommentEnd2.AsSpan())
            then
                reader.SkipN(fSharpBlockCommentEnd2.Length)
                reader.State <- LexBuilder.append Token.EndFSharpBlockComment pos CtxOp.NoOp reader.State
                preturn () reader
            else
                // Regular identifier
                match reader.Peek() with
                | ValueNone -> fail EndOfInput reader
                | ValueSome c when not (isIdentStartChar c) ->
                    fail (Message "Expected identifier start character") reader
                | ValueSome _ ->
                    reader.Skip()
                    let mutable more = true

                    while more do
                        match reader.Peek() with
                        | ValueSome ic when isIdentChar ic -> reader.Skip()
                        | _ -> more <- false

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
                    preturn () reader


    let pBacktickedIdentifierToken (reader: Reader<char, LexBuilder, ReadableString, _>) =
        let rec pRest (reader: Reader<char, LexBuilder, ReadableString, _>) =
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
        | 1 -> fail (Message "Expected backticked identifier") reader
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
            | _ -> fail (Message "Expected backticked identifier") reader


    let peekNewLine (reader: Reader<char, LexBuilder, ReadableString, _>) =
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

                do! updateUserState (LexBuilder.append Token.Whitespace pos CtxOp.NoOp)
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

    let pCharChar (reader: Reader<char, LexBuilder, ReadableString, _>) =
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
            do! updateUserState (LexBuilder.append token pos CtxOp.NoOp)
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
    let pStringEscapeToken (reader: Reader<char, LexBuilder, ReadableString, _>) =
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

    // Close tokens for plain strings
    let pPlainStringCloseToken =
        choiceL
            [
                pTokenPopCtx (pstring "\"B") Token.ByteArrayClose LexContext.PlainString
                pTokenPopCtx (pchar '"') Token.StringClose LexContext.PlainString
            ]
            "string close"

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

    let pTypeParamToken (reader: Reader<char, LexBuilder, ReadableString, _>) =
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
            | _ -> fail (Message "Expected identifier character after '") reader
        | _ -> fail (Message "Expected '") reader

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
    let private pSkipInterpolatedFragmentChars (reader: Reader<char, LexBuilder, ReadableString, _>) =
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
            fail (Message "Expected interpolated string fragment character") reader

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
            do! updateUserState (LexBuilder.append Token.Newline pos CtxOp.NoOp)
        }

    let pOperatorToken =
        fun (reader: Reader<char, LexBuilder, ReadableString, _>) ->
            let state = reader.State
            let startIdx = reader.Position.Index
            let inComment = state.IsInBlockComment || state.IsInOCamlBlockComment

            // `:` is only permitted as the first char of the predefined colon-starting
            // operators (`:`, `::`, `:?`, `:?>`, `:>`, `:=`). It must never fuse with
            // adjacent operator chars — e.g. `(x:^T)` must lex as `:` + `^T`, not `:^`.
            // Skip this treatment inside block comments: the content there is discarded
            // anyway, and splitting `://` in a URL would expose `//` to the line-comment
            // lexer, which would then eat past the closing `*)`.
            match reader.Peek() with
            | ValueSome ':' when not inComment ->
                reader.Skip()

                match reader.Peek() with
                | ValueSome ':' -> reader.Skip()
                | ValueSome '=' -> reader.Skip()
                | ValueSome '>' -> reader.Skip()
                | ValueSome '?' ->
                    reader.Skip()

                    match reader.Peek() with
                    | ValueSome '>' -> reader.Skip()
                    | _ -> ()
                | _ -> ()
            | _ ->
                // Imperatively consume operator chars, stopping at `:` (only valid as first
                // char, handled above) except inside block comments, and before `*)` when
                // inside a block comment.
                let mutable cont = true

                while cont do
                    match reader.Peek() with
                    | ValueSome ':' when not inComment -> cont <- false
                    | ValueSome c when customOperatorChars.Contains c ->
                        if inComment && c = '*' then
                            let peek2 = reader.PeekN(2)

                            if peek2.Length >= 2 && peek2[1] = ')' then
                                cont <- false // stop before `*)`
                            else
                                reader.Skip()
                        else
                            reader.Skip()
                    | _ -> cont <- false

            let endIdx = reader.Position.Index

            if endIdx = startIdx then
                fail (Message "Expected operator") reader
            else

                let op = state.Source.Substring(startIdx, endIdx - startIdx)

                let token, ctx =
                    match op with
                    | "|" -> Token.OpBar, CtxOp.NoOp
                    | "->" -> Token.OpArrowRight, CtxOp.NoOp
                    | "<-" -> Token.OpArrowLeft, CtxOp.NoOp
                    | ":" -> Token.OpColon, CtxOp.NoOp
                    | "::" -> Token.OpCons, CtxOp.NoOp
                    | ":?" -> Token.OpTypeTest, CtxOp.NoOp
                    | ":>" -> Token.OpUpcast, CtxOp.NoOp
                    | ":?>" -> Token.OpDowncast, CtxOp.NoOp
                    | ".." -> Token.OpRange, CtxOp.NoOp
                    | ".. .." -> Token.OpRangeStep, CtxOp.NoOp
                    | ":=" -> Token.OpColonEquals, CtxOp.NoOp
                    | "~" -> Token.KWReservedTwiddle, CtxOp.NoOp
                    | "<@" -> Token.OpQuotationTypedLeft, (CtxOp.Push LexContext.QuotedExpression)
                    | "@>" -> Token.OpQuotationTypedRight, (CtxOp.Pop LexContext.QuotedExpression)
                    | "<@@" -> Token.OpQuotationUntypedLeft, (CtxOp.Push LexContext.TypedQuotedExpression)
                    | "@@>" -> Token.OpQuotationUntypedRight, (CtxOp.Pop LexContext.TypedQuotedExpression)
                    | "." -> Token.OpDot, CtxOp.NoOp
                    | "?" -> Token.OpDynamic, CtxOp.NoOp
                    | "?<-" -> Token.OpDynamicAssignment, CtxOp.NoOp
                    | "!" -> Token.OpDereference, CtxOp.NoOp
                    | "??" -> Token.OpQMarkQMark, CtxOp.NoOp
                    | "=" -> Token.OpEquality, CtxOp.NoOp
                    | "&&" -> Token.OpAmpAmp, CtxOp.NoOp
                    | "&" -> Token.OpAmp, CtxOp.NoOp
                    | "*" -> Token.OpMultiply, CtxOp.NoOp
                    | "/" -> Token.OpDivision, CtxOp.NoOp
                    | "^" -> Token.OpConcatenate, CtxOp.NoOp
                    | "~~~" -> Token.OpLogicalNot, CtxOp.NoOp
                    | "||" -> Token.OpBarBar, CtxOp.NoOp
                    | _ ->
                        let token = Token.ofCustomOperator (op.AsSpan())
                        token, CtxOp.NoOp

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
            do! updateUserState (LexBuilder.append token pos ctxOp)
        }

    let pLAttrBrack = pstring "[<"
    let pLArrayBrack = pstring "[|"
    let pRAttrBrack = pstring ">]"
    let pRArrayBrack = pstring "|]"

    // This is a fallback, we shouldn't see any Other tokens in output
    let private peekEndOfIdent (reader: Reader<char, LexBuilder, ReadableString, _>) =
        match reader.Peek() with
        | ValueSome c when isIdentChar c -> fail (Message "Expected end of identifier") reader
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
            (reader: Reader<char, LexBuilder, ReadableString, _>)
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

        let private pManyIntCharsWithBase isDigitInBase (reader: Reader<char, LexBuilder, ReadableString, _>) =
            pManyIntCharsWithBaseLoop false isDigitInBase reader.Position reader

        let pIntBase = pManyIntCharsWithBase isDecimalDigit
        let pHexBase = pManyIntCharsWithBase isHexDigit
        let pOctalBase = pManyIntCharsWithBase isOctalDigit
        let pBinaryBase = pManyIntCharsWithBase isBinaryDigit

        let pXIntBase (reader: Reader<char, LexBuilder, ReadableString, _>) =
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
        let inline private skipSuffixAndGetSpan (reader: Reader<char, LexBuilder, ReadableString, _>) =
            let startIdx = int reader.Position.Index
            let mutable more = true

            while more do
                match reader.Peek() with
                | ValueSome c when isIdentChar c -> reader.Skip()
                | _ -> more <- false

            let len = int reader.Position.Index - startIdx
            reader.State.Source.AsSpan(startIdx, len)

        let private parseDecimalIntToken (reader: Reader<char, LexBuilder, ReadableString, _>) =
            let span = skipSuffixAndGetSpan reader
            preturn (getIntTokenFromSpan NumericBase.Decimal span) reader

        let private parseDecimalFloatSuffixToken (reader: Reader<char, LexBuilder, ReadableString, _>) =
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

        let private parseIntSuffixToken (numBase: NumericBase) (reader: Reader<char, LexBuilder, ReadableString, _>) =
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

        let pFormatType (reader: Reader<char, 'State, 'Input, 'InputSlice>) =
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

        let lFormatPlaceholder: Parser<_, _, _, ReadableString, _> =
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
        let pFormatSpecifierTokens: Parser<unit, _, _, ReadableString, _> =
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

    // Parenthesized `*` operator: `(*)` is the only form where `(*` does not
    // start a block comment. Emits three tokens (KWLParen, OpMultiply, KWRParen)
    // and balances the paren context stack.
    let pParenStarOperator =
        parser {
            let! pos = getPosition
            let! _ = pstring "(*)"
            let idx = int pos.Index

            do!
                updateUserState (fun state ->
                    state
                    |> LexBuilder.appendI Token.KWLParen idx (CtxOp.Push LexContext.ParenthesExpression)
                    |> LexBuilder.appendI Token.OpMultiply (idx + 1) CtxOp.NoOp
                    |> LexBuilder.appendI Token.KWRParen (idx + 2) (CtxOp.Pop LexContext.ParenthesExpression)
                )
        }

    let pLParenToken =
        choiceL
            [
                // 19.1 Conditional Compilation for ML Compatibility
                pToken (pstring "(*ENDIF-OCAML*)") Token.EndOCamlBlockComment
                pToken (pstring "(*IF-OCAML*)") Token.StartOCamlBlockComment
                pToken (pstring "(*IF-FSHARP") Token.StartFSharpBlockComment
                pToken (pstring "(*F#") Token.StartFSharpBlockComment
                // Parenthesized star operator — must precede `(*` block-comment match
                pParenStarOperator
                // 3.2 Comments
                pToken (pstring "(*") Token.BlockCommentStart
                // IL intrinsic literal opening
                pToken (pstring "(#") Token.KWLHashParen
                //pToken (pstring "()") Token.Unit
                pOpenParenExpressionContext
            ]
            "Left parenthesis or unit"

    let pLBacketToken =
        choiceL
            [
                pToken pLAttrBrack Token.KWLAttrBracket
                pToken pLArrayBrack Token.KWLArrayBracket
                //pToken (pstring "[]") Token.OpNil
                pOpenBracketExpressionContext
            ]
            "Left bracket or empty array"

    let pGreaterThanToken =
        choiceL [ pToken pRAttrBrack Token.KWRAttrBracket; pOperatorToken ] "Right bracket or operator"

    let pDoubleQuoteToken =
        choiceL [ pString3OpenToken; pStringOpenToken ] "String literal"

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
        choiceL
            [ pVerbatimInterpolatedStartToken; pVerbatimStringOpenToken; pOperatorToken ]
            "Verbatim string or operator"

    let pSlashToken =
        // 3.2 Comments
        choiceL [ pLineCommentToken; pOperatorToken ] "Line comment or operator"

    let pSemicolonToken =
        choiceL
            [
                pToken (pstring ";;") Token.OpDoubleSemicolon
                pToken (pchar ';') Token.OpSemicolon
            ]
            "Semicolon"

    let pDotToken =
        choiceL [ pSpecialDotOperatorToken; pOperatorToken ] "Dot operator or operator"

    let pCustomOperatorToken =
        choiceL
            [
                pToken pRArrayBrack Token.KWRArrayBracket
                pCloseBraceBarExpressionContext
                // 3.2 Comments
                pToken (pstring "*)") Token.BlockCommentEnd
                pOperatorToken
            ]
            "Operator"
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

    let private isAtStartOfLineOrIndent (reader: Reader<char, LexBuilder, ReadableString, _>) =
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
                return! fail (Message "Directives must be at start of line or immediately after indentation")
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
        choiceL
            [
                pDirectiveToken
                // IL intrinsic literal closing
                pToken (pstring "#)") Token.KWRHashParen
                pToken (pchar '#') Token.KWHash
            ]
            "Hash or Directive"

    // Hoisted out of the `lex` dispatch so the list/closure is built once, not per iteration.
    let private pLBraceExpressionToken =
        choiceL [ pOpenBraceBarExpressionContext; pOpenBraceExpressionContext ] "Left brace"

    [<TailCall>]
    let rec lex (reader: Reader<char, LexBuilder, ReadableString, _>) =
        let state = reader.State

        match reader.Peek() with
        | ValueNone -> Ok(LexBuilder.complete reader.Position.Index reader.State)

        | ValueSome c ->
            let ctx = LexBuilder.currentContext state
            // printfn "At %A, Context: %A, Next char: %A" reader.Position.Index ctx c
            let p =
                match c, ctx with
                | ('\r' | '\n'), LexContext.IfDirective -> IfDirective.pNewlineToken
                | '(', LexContext.IfDirective -> IfDirective.pLParenToken
                | ')', LexContext.IfDirective -> IfDirective.pRParenToken
                | '&', LexContext.IfDirective -> IfDirective.pAnd
                | '|', LexContext.IfDirective -> IfDirective.pOr
                | '!', LexContext.IfDirective -> IfDirective.pNot
                | ' ', LexContext.IfDirective -> IfDirective.pWhitespaceToken
                | _, LexContext.IfDirective -> IfDirective.pIdentifierOrOther
                | ('\r' | '\n'), ExpressionCtx -> pNewlineToken
                | ' ', ExpressionCtx -> pIndentOrWhitespaceToken
                | '\t', ExpressionCtx -> pTabToken
                | ',', ExpressionCtx -> pCommaToken
                | '(', ExpressionCtx -> pLParenToken
                | ')', ExpressionCtx -> pCloseParenExpressionContext
                | '[', ExpressionCtx -> pLBacketToken
                | '>', ExpressionCtx -> pGreaterThanToken
                | ']', ExpressionCtx -> pCloseBracketExpressionContext
                | '{', LexContext.InterpolatedString ->
                    // In an interpolated string, { starts an expression or is escaped as {{
                    pInterpolatedExpressionStartToken
                | '{', LexContext.VerbatimInterpolatedString ->
                    // In a verbatim interpolated string, { starts an expression or is escaped as {{
                    pInterpolatedExpressionStartToken
                | '{', LexContext.Interpolated3String level ->
                    // In a triple-quoted interpolated string, { * level starts an expression or is escaped as {{
                    pInterpolated3ExpressionStartToken
                | '{', ExpressionCtx -> pLBraceExpressionToken
                | '}', LexContext.InterpolatedString ->
                    // In an interpolated string, } is escaped as }}
                    pInterpolatedStringFragmentRBraces
                | '}', LexContext.VerbatimInterpolatedString ->
                    // In a verbatim interpolated string, } is escaped as }}
                    pInterpolatedStringFragmentRBraces
                | '}', LexContext.Interpolated3String level ->
                    // In a triple-quoted interpolated string, required } depends on level
                    pInterpolated3StringFragmentRBraces
                | '}', LexContext.InterpolatedExpression ->
                    // In an interpolated expression, } ends the expression
                    pInterpolatedExpressionEndToken
                | '}', NonInterpolatedExpressionCtx -> pCloseBraceExpressionContext
                | '"', LexContext.InterpolatedString -> pInterpolatedStringEndToken
                | '"', LexContext.VerbatimInterpolatedString -> pVerbatimInterpolatedStringQuoteToken
                | '"', LexContext.Interpolated3String _ -> pInterpolated3QuoteOrFragment
                // Plain string contexts
                | '"', LexContext.PlainString -> pPlainStringCloseToken
                | '"', LexContext.VerbatimString -> pVerbatimStringQuoteToken2
                | '"', LexContext.TripleQuotedString -> pTripleStringQuoteOrFragment
                | '\\', LexContext.PlainString -> pStringEscapeToken
                | '"', _ ->
                    // String or triple-quoted string literals
                    pDoubleQuoteToken
                | ''', ExpressionCtx ->
                    // Char literals, type parameters, or a single quote
                    pSingleQuoteToken
                | '$', ExpressionCtx ->
                    // Interpolated strings
                    pDollarToken
                | '@', ExpressionCtx ->
                    // Verbatim strings
                    pAtToken
                | '/', ExpressionCtx -> pSlashToken
                | '%',
                  (LexContext.InterpolatedString | LexContext.VerbatimInterpolatedString | LexContext.Interpolated3String _) ->
                    pFormatSpecifierTokens
                | '%', (LexContext.PlainString | LexContext.VerbatimString | LexContext.TripleQuotedString) ->
                    pFormatSpecifierTokens
                | _, LexContext.InterpolatedString -> pInterpolatedStringFragmentToken
                | _, LexContext.VerbatimInterpolatedString -> pVerbatimInterpolatedStringFragmentToken
                | _, LexContext.Interpolated3String _ -> pInterpolated3StringFragmentToken
                // Plain string fragment fallthroughs
                | _, LexContext.PlainString -> pPlainStringFragmentToken
                | _, LexContext.VerbatimString -> pVerbatimStringFragmentToken2
                | _, LexContext.TripleQuotedString -> pTripleStringFragmentToken
                | ':', LexContext.InterpolatedExpression -> pInterpolatedFormatClause
                | ';', ExpressionCtx -> pSemicolonToken
                | '.', ExpressionCtx -> pDotToken
                | '#', ExpressionCtx -> pHashToken
                | c, ExpressionCtx when NumericLiterals.isDecimalDigit c -> NumericLiterals.parseToken
                | c, ExpressionCtx when customOperatorChars.Contains c -> pCustomOperatorToken
                | c, ExpressionCtx when isIdentStartChar c -> pIdentifierOrKeywordToken
                | '`', ExpressionCtx -> pBacktickedIdentifierToken
                | _, _ -> pOtherToken

            match p reader with
            | Ok() -> lex reader
            | Error e -> Error e

    let lexString (input: string) =
        let reader = Reader.ofString input (LexBuilder.init input)
        lex reader
