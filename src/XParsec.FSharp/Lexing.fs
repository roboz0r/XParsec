namespace XParsec.FSharp.Lexer

open System
open System.Globalization
open System.Collections.Generic
open System.Collections.Immutable

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
    | ParenthesExpression
    | BracketedExpression
    | QuotedExpression
    | TypedQuotedExpression

type LexBuilder =
    {
        // Resume here
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

    let complete idx (state: LexBuilder) =
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

    let init () =
        let x =
            {
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
                | _ -> findLevel tail

        findLevel x.Context

    let (|CoalescableToken|_|) (token: Token) =
        // Tokens that can be coalesced if adjacent
        // This occurs with string fragments in interpolated strings due to the way braces are handled
        match token.WithoutCommentFlags with
        // | Token.OtherUnlexed
        | Token.Interpolated3StringFragment
        | Token.VerbatimInterpolatedStringFragment
        | Token.InterpolatedStringFragment -> true
        | _ -> false

    let appendI token (idx: int) ctxOp (state: LexBuilder) =
        // Coalesce adjacent string fragments
        // TODO: Handle literal negation
        // Consider if this should be done in the `lex` function instead
        // https://fsharp.github.io/fslang-spec/lexical-analysis/#381-post-filtering-of-adjacent-prefix-tokens
        let tokenCount = state.Tokens.Count
        let tokenIdx = tokenCount * 1<token>

        let addToken =
            if tokenCount > 0 then
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
            let! spaces = many1Chars (pchar ' ')

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

    let pIdentStartChar = satisfyL isIdentStartChar "identifier start character"

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

    let pIdentChar = satisfyL isIdentChar "identifier character"

    let pIdentifier: Parser<_, char, LexBuilder, ReadableString, _> =
        many1Chars2 pIdentStartChar pIdentChar

    let pIdentifierOrKeywordToken =
        let keywords = dict identifierKeywords

        parser {
            let! pos = getPosition

            let! id =
                choiceL
                    [
                        // 19.1 Conditional Compilation for ML Compatibility
                        pstring "F#*)"
                        pstring "ENDIF-FSHARP*)"
                        // 3.4 Identifiers and Keywords
                        pIdentifier
                    ]
                    "Identifier"

            match id with
            | "F#*)"
            | "ENDIF-FSHARP*)" ->
                do! updateUserState (LexBuilder.append Token.EndFSharpBlockComment pos CtxOp.NoOp)

                return ()
            | _ ->

                let! suffix = opt (anyOf "!#")

                let id =
                    match suffix with
                    | ValueSome c -> id + string c
                    | ValueNone -> id

                let token =
                    match keywords.TryGetValue id with
                    | true, kw -> kw
                    | false, _ ->
                        match suffix with
                        | ValueNone -> Token.Identifier
                        | ValueSome '!' -> Token.OpDereference
                        | ValueSome '#' -> Token.ReservedIdentifierHash
                        | ValueSome c -> invalidOp $"Unexpected identifier suffix '{c}'"

                do! updateUserState (LexBuilder.append token pos CtxOp.NoOp)
                return ()
        }


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
                    // Escaped backtick within identifier
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
                match pRest reader with
                | Ok { Parsed = token } ->
                    reader.State <- LexBuilder.append token pos CtxOp.NoOp reader.State
                    preturn () reader
                | Error e -> Error e
            | '`', _ ->
                reader.Skip()
                reader.State <- LexBuilder.append Token.KWReservedBacktick pos CtxOp.NoOp reader.State
                preturn () reader
            | _ -> fail (Message "Expected backticked identifier") reader

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

    let pStringChar (reader: Reader<char, LexBuilder, ReadableString, _>) =
        let span = reader.PeekN(2)

        match span.Length with
        | 0 -> fail EndOfInput reader
        | 1 ->
            match span[0] with
            | '"' -> fail (Unexpected '"') reader
            // TODO: The F# spec doesn't provide a listing for "simple-string-char" but empirically it's possible to have control chars in a string
            // | ('\n' | '\t' | '\r' | '\b' | '\a' | '\f' | '\v' | '\\') as c -> fail (Unexpected c) reader
            | c ->
                reader.Skip()
                preturn c reader
        | _ ->
            match span[0], span[1] with
            // Escape chars ["\'ntbrafv]
            | '\\', '"' ->
                // Escaped quote
                reader.SkipN(2)
                preturn '"' reader
            | '\\', '\\' ->
                // Escaped backslash
                reader.SkipN(2)
                preturn '\\' reader
            | '\\', '\'' ->
                // Escaped single quote
                reader.SkipN(2)
                preturn '\'' reader
            | '\\', 'n' ->
                reader.SkipN(2)
                preturn '\n' reader
            | '\\', 't' ->
                reader.SkipN(2)
                preturn '\t' reader
            | '\\', 'b' ->
                reader.SkipN(2)
                preturn '\b' reader
            | '\\', 'r' ->
                reader.SkipN(2)
                preturn '\r' reader
            | '\\', 'a' ->
                reader.SkipN(2)
                preturn '\a' reader
            | '\\', 'f' ->
                reader.SkipN(2)
                preturn '\f' reader
            | '\\', 'v' ->
                reader.SkipN(2)
                preturn '\v' reader
            | '\\', 'u' ->
                // unicodegraph-short
                let span = reader.PeekN(6)

                if span.Length = 6 then
                    let hex = span.Slice(2, 4)

                    match UInt16.TryParse(hex, NumberStyles.AllowHexSpecifier, CultureInfo.InvariantCulture) with
                    | true, code ->
                        reader.SkipN(6)
                        preturn (char code) reader
                    | false, _ ->
                        reader.Skip()
                        preturn '\\' reader
                else
                    reader.Skip()
                    preturn '\\' reader
            | '\\', 'U' ->
                // unicodegraph-long
                let span = reader.PeekN(10)

                if span.Length = 10 then
                    let hex = span.Slice(2, 8)

                    match UInt32.TryParse(hex, NumberStyles.AllowHexSpecifier, CultureInfo.InvariantCulture) with
                    | true, code when code <= 0xFFFFu ->
                        reader.SkipN(10)
                        preturn (char code) reader
                    | true, code ->
                        reader.SkipN(10)
                        // TODO: This is the wrong code for code points > U+FFFF
                        // We'd need to return a string or 2 chars
                        // Consider, sidechannel a second char through the parser state?
                        // Or a custom, many char poarser that we can appendrange to the state?
                        preturn (char code) reader
                    | false, _ ->
                        reader.Skip()
                        preturn '\\' reader
                else
                    reader.Skip()
                    preturn '\\' reader
            | '\\', c ->
                if isDigit c then
                    // trigraph
                    let span = reader.PeekN(4)

                    if span.Length = 4 then
                        let digits = span.Slice(1, 3)

                        match Int32.TryParse(digits, NumberStyles.None, CultureInfo.InvariantCulture) with
                        | true, code when code <= 255 ->
                            reader.SkipN(4)
                            preturn (char code) reader
                        | true, code ->
                            // TODO: This is currently a warning in F#
                            // We should probably have a way to report warnings from the lexer
                            reader.SkipN(4)
                            preturn (char (code % 256)) reader
                        | false, _ ->
                            reader.Skip()
                            preturn '\\' reader
                    else
                        reader.Skip()
                        preturn '\\' reader
                else
                    // non-escape-chars
                    // Just return the backslash and the next char as-is
                    reader.Skip()
                    preturn '\\' reader

            | '"', _ -> fail (Unexpected '"') reader
            | c, ('\\' | '"') ->
                // simple-char-char
                reader.Skip()
                preturn c reader
            | _, _ ->
                // Regular character
                // TODO: We could go faster by skipping two at a time until we hit a special char
                reader.Skip()
                preturn span[0] reader


    let pVerbatimStringChar (reader: Reader<char, LexBuilder, ReadableString, _>) =
        let span = reader.PeekN(2)

        match span.Length with
        | 0 -> fail EndOfInput reader
        | 1 ->
            match span[0] with
            | '"' -> fail (Unexpected '"') reader
            | c ->
                reader.Skip()
                preturn c reader
        | _ ->
            match span[0], span[1] with
            | '"', '"' ->
                // Escaped quote
                reader.SkipN(2)
                preturn '"' reader
            | '"', _ -> fail (Unexpected '"') reader
            | c, _ ->
                // simple-char-char
                reader.Skip()
                preturn c reader

    let pString3Char = anyChar

    let pString3Literal =
        pstring "\"\"\""
        >>. manyCharsTill
                pString3Char
                (pstring "\"\"\"" >>% Token.String3Literal
                 <|> (eof >>% Token.UnterminatedString3Literal))

    let pVerbatimStringLiteral =
        pstring "@\""
        >>. manyCharsTill
                pVerbatimStringChar
                (choiceL
                    [
                        pstring "\"B" >>% Token.VerbatimByteArrayLiteral
                        pchar '"' >>% Token.VerbatimStringLiteral
                        eof >>% Token.UnterminatedVerbatimStringLiteral
                    ]
                    "verbatim string literal")

    let pStringLiteral =
        (pchar '"')
        >>. manyCharsTill
                pStringChar
                (choiceL
                    [
                        pstring "\"B" >>% Token.ByteArrayLiteral
                        pchar '"' >>% Token.StringLiteral
                        eof >>% Token.UnterminatedStringLiteral
                    ]
                    "string literal")


    let pStringToken pLiteral =
        parser {
            let! pos = getPosition
            let! (s, token) = pLiteral
            do! updateUserState (LexBuilder.append token pos CtxOp.NoOp)
        }

    let pCharToken =
        between (pchar '\'') (pchar '\'') pCharChar
        |>> (fun x ->
            match x with
            | CharChar.Simple c -> x, Token.CharLiteral
            | CharChar.Escaped c -> x, Token.CharLiteral
            | CharChar.UnicodeShort c -> x, Token.CharLiteral
            | CharChar.Trigraph c -> x, Token.CharLiteral
            | CharChar.InvalidTrigraph i -> x, Token.InvalidCharTrigraphLiteral
        )
        |> pStringToken


    let pStringLiteralToken = pStringToken pStringLiteral
    let pString3LiteralToken = pStringToken pString3Literal
    let pVerbatimStringLiteralToken = pStringToken pVerbatimStringLiteral

    let pTypeParamToken =
        let keywords = dict identifierKeywords

        parser {
            let! pos = getPosition
            let! _ = pchar '\''
            let! chars = many1Chars pIdentChar
            // Type parameter
            match keywords.TryGetValue chars with
            | true, token ->
                // e.g. 'let is a keyword, not a type parameter
                do!
                    updateUserState (fun state ->
                        state
                        |> LexBuilder.append Token.KWSingleQuote pos CtxOp.NoOp
                        |> LexBuilder.appendI token (pos.Index + 1) CtxOp.NoOp

                    )
            | false, _ -> do! updateUserState (LexBuilder.append Token.TypeParameter pos CtxOp.NoOp)
        }

    let pInterpolatedStringStartToken =
        pTokenPushCtx (pstring "$\"") Token.InterpolatedStringOpen LexContext.InterpolatedString

    let pVerbatimInterpolatedStartToken =
        pTokenPushCtx
            (anyString [| "@$\""; "$@\"" |])
            Token.VerbatimInterpolatedStringOpen
            LexContext.VerbatimInterpolatedString

    let pInterpolatedStringEndToken =
        pTokenPopCtx (pchar '"') Token.InterpolatedStringClose LexContext.InterpolatedString

    let pInterpolatedStringFragmentToken =
        pToken
            (many1Chars (satisfy (fun c -> c <> '"' && c <> '{' && c <> '}' && c <> '%')))
            Token.InterpolatedStringFragment

    let pInterpolated3StringFragmentToken =
        pToken
            (many1Chars (satisfy (fun c -> c <> '"' && c <> '{' && c <> '}' && c <> '%')))
            Token.Interpolated3StringFragment

    let pInterpolatedExpressionStartToken =
        parser {
            let! pos = getPosition
            let! braces = many1Chars (pchar '{')

            do!
                updateUserState (fun state ->

                    let mutable count = braces.Length
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
            let! braces = many1Chars (pchar '{')

            do!
                updateUserState (fun state ->
                    let level = LexBuilder.level state
                    let count = braces.Length
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


    let pOpenBraceExpressionContext =
        pTokenPushCtx (pchar '{') Token.KWLBrace LexContext.BracedExpression

    let pCloseBraceExpressionContext =
        pTokenPopCtx (pchar '}') Token.KWRBrace LexContext.BracedExpression

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
            let! braces = lookAhead (many1Chars (pchar '}'))
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
            | _ when braces.Length >= level ->
                do! skipN level

                do!
                    updateUserState (
                        LexBuilder.append
                            Token.InterpolatedExpressionClose
                            pos
                            (CtxOp.Pop LexContext.InterpolatedExpression)
                    )
            | _ ->
                do! skipN braces.Length

                do!
                    updateUserState (fun state ->
                        let mutable state = state

                        for i in 0 .. (braces.Length - 1) do
                            // We have some number of braces, but not enough to close the expression
                            state <- LexBuilder.appendI Token.KWRBrace (pos.Index + i) CtxOp.NoOp state

                        state
                    )
        }


    let pInterpolatedStringFragmentRBraces =
        parser {
            let! pos = getPosition
            let! braces = many1Chars (pchar '}')

            let mutable count = braces.Length
            let mutable idx = int pos.Index

            while count > 1 do
                // }} is an escape sequence for '}'
                // TODO: Investigate order of imperative operations in a while loop
                idx <- idx + 2
                count <- count - 2
                do! updateUserState (LexBuilder.appendI Token.EscapeRBrace idx CtxOp.NoOp)


            match count with
            | 0 -> return ()
            | _ ->
                // Single } is invalid outside an expression
                do! updateUserState (LexBuilder.appendI Token.UnmatchedInterpolatedRBrace idx CtxOp.NoOp)
        }

    let pInterpolated3StringFragmentRBraces =
        parser {
            let! pos = getPosition
            let! braces = many1Chars (pchar '}')
            let level = LexBuilder.level pos.State

            let count = braces.Length
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
            let! quotes = many1Chars (pchar '"')

            do!
                updateUserState (fun state ->
                    let mutable idx = pos.Index
                    let mutable state = state
                    let mutable count = quotes.Length

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
        pToken
            (many1Chars (satisfy (fun c -> c <> '"' && c <> '{' && c <> '}')))
            Token.VerbatimInterpolatedStringFragment

    let pInterpolated3StartToken =
        parser {
            let! pos = getPosition
            let! (dollars, _) = many1Chars (pchar '$') .>>. pstring "\"\"\""

            do!
                updateUserState (
                    LexBuilder.append
                        Token.Interpolated3StringOpen
                        pos
                        (CtxOp.Push(LexContext.Interpolated3String dollars.Length))
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


    let pNewlineToken =
        parser {
            let! pos = getPosition
            do! skipNewline
            do! updateUserState (LexBuilder.append Token.Newline pos CtxOp.NoOp)
        }

    let peekNewLine (reader: Reader<char, LexBuilder, ReadableString, _>) =
        match reader.Peek() with
        | ValueSome('\n' | '\r') -> preturn () reader
        | _ -> fail expectedNewline reader

    let pLineComment = pstring "//" >>. manyCharsTill anyChar (peekNewLine <|> eof)

    let pCombiningOperator = many1Chars (anyOf customOperatorChars)

    let pOperatorToken =
        parser {
            let! pos = getPosition
            let! op = pCombiningOperator

            do!
                updateUserState (fun state ->
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
                        | ":=" -> Token.OpAssignment, CtxOp.NoOp
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
                        | _ ->
                            let token = Token.ofCustomOperator (op.AsSpan())
                            token, CtxOp.NoOp

                    LexBuilder.append token pos ctx state
                )
        }

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
                    | Ok { Parsed = () } -> preturn NumericBase.Decimal reader
                    | Error e -> invalidOp $"Unreachable error parsing decimal number: {e}"
                else
                    fail expectedBasePrefix reader
            | _ ->
                // Check for base prefix and at least one digit in that base
                match span[0], span[1], span[2] with
                | '0', ('x' | 'X' as c), c2 when isHexDigit c2 ->
                    reader.SkipN(2)

                    match pHexBase reader with
                    | Ok { Parsed = () } -> preturn NumericBase.Hex reader
                    | Error e -> invalidOp $"Unreachable error parsing hexadecimal number: {e}"

                | '0', ('o' | 'O' as c), c2 when isOctalDigit c2 ->
                    reader.SkipN(2)

                    match pOctalBase reader with
                    | Ok { Parsed = () } -> preturn NumericBase.Octal reader
                    | Error e -> invalidOp $"Unreachable error parsing octal number: {e}"

                | '0', ('b' | 'B' as c), c2 when isBinaryDigit c2 ->
                    reader.SkipN(2)

                    match pBinaryBase reader with
                    | Ok { Parsed = () } -> preturn NumericBase.Binary reader
                    | Error e -> invalidOp $"Unreachable error parsing binary number: {e}"

                | c, _, _ when isDecimalDigit c ->
                    // Decimal base
                    match pIntBase reader with
                    | Ok { Parsed = () } -> preturn NumericBase.Decimal reader
                    | Error e -> invalidOp $"Unreachable error parsing decimal number: {e}"

                | _ -> fail expectedBasePrefix reader

        let private getIntToken (numBase: NumericBase) (suffix: string) =
            let token =
                if suffix.Length > 2 then
                    Token.ReservedNumericLiteral
                else
                    match suffix with
                    | "y" -> Token.NumSByte
                    | "uy" -> Token.NumByte
                    | "s" -> Token.NumInt16
                    | "us" -> Token.NumUInt16
                    | ""
                    | "l" -> Token.NumInt32
                    | "u"
                    | "ul" -> Token.NumUInt32
                    | "n" -> Token.NumNativeInt
                    | "un" -> Token.NumUNativeInt
                    | "L" -> Token.NumInt64
                    | "uL"
                    | "UL" -> Token.NumUInt64
                    | "Q" ->
                        match numBase with
                        | NumericBase.Decimal -> Token.NumBigIntegerQ
                        | _ -> Token.ReservedNumericLiteral
                    | "R" ->
                        match numBase with
                        | NumericBase.Decimal -> Token.NumBigIntegerR
                        | _ -> Token.ReservedNumericLiteral
                    | "Z" ->
                        match numBase with
                        | NumericBase.Decimal -> Token.NumBigIntegerZ
                        | _ -> Token.ReservedNumericLiteral
                    | "I" ->
                        match numBase with
                        | NumericBase.Decimal -> Token.NumBigIntegerI
                        | _ -> Token.ReservedNumericLiteral
                    | "N" ->
                        match numBase with
                        | NumericBase.Decimal -> Token.NumBigIntegerN
                        | _ -> Token.ReservedNumericLiteral
                    | "G" ->
                        match numBase with
                        | NumericBase.Decimal -> Token.NumBigIntegerG
                        | _ -> Token.ReservedNumericLiteral
                    | "m"
                    | "M" ->
                        // Integer decimals only, float decimals also use m/M suffix
                        // but are handled in the float parser
                        match numBase with
                        | NumericBase.Decimal -> Token.NumDecimal
                        | _ -> Token.ReservedNumericLiteral
                    | "f"
                    | "F" ->
                        // Float suffixes are only valid for decimal base
                        match numBase with
                        | NumericBase.Decimal -> Token.NumIEEE32
                        | _ -> Token.ReservedNumericLiteral
                    | "LF" ->
                        match numBase with
                        | NumericBase.Decimal -> Token.ReservedNumericLiteral
                        | _ -> Token.NumIEEE64
                    | "lf" ->
                        match numBase with
                        | NumericBase.Decimal -> Token.ReservedNumericLiteral
                        | _ -> Token.NumIEEE32
                    | _ -> Token.ReservedNumericLiteral

            // Combine the base into the token
            let numBase = uint16 numBase <<< TokenRepresentation.NumericBaseShift
            Token.ofUInt16 (uint16 token ||| numBase)

        let getDecimalFloatToken (suffix: string) =
            // Float suffixes are only valid for decimal base
            match suffix with
            | "" -> Token.NumIEEE64
            | "f"
            | "F" -> Token.NumIEEE32
            | "m"
            | "M" -> Token.NumDecimal
            | _ -> Token.ReservedNumericLiteral

        let private parseDecimalIntToken =
            parser {
                let! suffix = manyChars pIdentChar
                return getIntToken NumericBase.Decimal suffix
            }

        let private parseDecimalExpFloatToken =
            let choices =
                choiceL [ pIntBase; (anyOf "+-") >>. pIntBase ] "parseDecimalExpFloatToken"

            parser {
                let! e = anyOf "eE"
                let! expPart = choices
                let! suffix = manyChars pIdentChar
                return getDecimalFloatToken suffix
            }

        let private parseDecimalFracFloatToken =
            let choices =
                choiceL
                    [ parseDecimalExpFloatToken; manyChars pIdentChar |>> getDecimalFloatToken ]
                    "parseDecimalFracFloatToken"

            parser {
                let! dot = pchar '.' .>> notFollowedBy (pchar '.')
                let! fracPart = opt pIntBase
                return! choices
            }

        let private parseDecimalToken =
            choiceL [ parseDecimalFracFloatToken; parseDecimalExpFloatToken; parseDecimalIntToken ] "parseDecimalToken"

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
                    let! suffix = manyChars pIdentChar
                    let token = getIntToken numBase suffix
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
                let! percents = many1Chars (pchar '%')
                let! state = getUserState
                let tokens = state.Tokens

                match LexBuilder.level state with
                | 1 ->
                    // Level 1 logic
                    let mutable count = percents.Length
                    let mutable idx = pos.Index

                    while count > 1 do
                        if count >= 2 then // %% is an escape sequence for '%'
                            tokens.Add(PositionedToken.Create(Token.EscapePercent, idx))
                            idx <- idx + 2
                            count <- count - 2

                    match count with
                    | 0 ->
                        do! skipN percents.Length
                        return ()
                    | _ ->
                        do! skipN (percents.Length - 1)
                        let! t = lFormatPlaceholderToken
                        tokens.Add(PositionedToken.Create(t, idx))
                        return ()

                | level ->

                    // Level 2 or higher logic, impossible to get 0 or negative from many1Chars
                    let count = percents.Length
                    let leading = count - level
                    let idx = pos.Index

                    if leading < 0 then
                        tokens.Add(PositionedToken.Create(Token.InterpolatedStringFragment, idx))
                        // do! skipN count
                        return ()
                    elif leading = 0 then
                        // Exactly enough to start a FormatPlaceholder
                        let! t = lFormatPlaceholderToken
                        tokens.Add(PositionedToken.Create(t, idx))
                        return ()
                    elif leading >= level then
                        // Too many leading '%'
                        tokens.Add(PositionedToken.Create(Token.InvalidFormatPercents, idx))
                        return ()
                    else
                        tokens.Add(PositionedToken.Create(Token.InterpolatedStringFragment, idx))
                        // do! skipN leading
                        let! pos = getPosition
                        let! t = lFormatPlaceholderToken
                        tokens.Add(PositionedToken.Create(t, pos.Index))
                        return ()
            }

    let (|ExpressionCtx|_|) (ctx: LexContext) =
        match ctx with
        | LexContext.Normal
        | LexContext.BracedExpression
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
        | LexContext.ParenthesExpression
        | LexContext.BracketedExpression
        | LexContext.QuotedExpression
        | LexContext.TypedQuotedExpression -> true
        | _ -> false

    let pTabToken = pToken (pchar '\t') Token.Tab
    let pCommaToken = pToken (pchar ',') Token.OpComma

    let pLParenToken =
        choiceL
            [
                // 19.1 Conditional Compilation for ML Compatibility
                pToken (pstring "(*ENDIF-OCAML*)") Token.EndOCamlBlockComment
                pToken (pstring "(*IF-OCAML*)") Token.StartOCamlBlockComment
                pToken (pstring "(*IF-FSHARP") Token.StartFSharpBlockComment
                pToken (pstring "(*F#") Token.StartFSharpBlockComment
                // 3.2 Comments
                pToken (pstring "(*") Token.BlockCommentStart
                pToken (pstring "()") Token.Unit
                pOpenParenExpressionContext
            ]
            "Left parenthesis or unit"

    let pLBacketToken =
        choiceL
            [
                pToken pLAttrBrack Token.KWLAttrBracket
                pToken pLArrayBrack Token.KWLArrayBracket
                pToken (pstring "[]") Token.OpNil
                pOpenBracketExpressionContext
            ]
            "Left bracket or empty array"

    let pGreaterThanToken =
        choiceL [ pToken pRAttrBrack Token.KWRAttrBracket; pOperatorToken ] "Right bracket or operator"

    let pDoubleQuoteToken =
        choiceL [ pString3LiteralToken; pStringLiteralToken ] "String literal"

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
            [ pVerbatimInterpolatedStartToken; pVerbatimStringLiteralToken; pOperatorToken ]
            "Verbatim string or operator"

    let pSlashToken =
        // 3.2 Comments
        choiceL [ pToken pLineComment Token.LineComment; pOperatorToken ] "Line comment or operator"

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
        parser {
            let! atStart = isAtStartOfLineOrIndent

            if not atStart then
                return! fail (Message "Directives must be at start of line or immediately after indentation")

            let! pos = getPosition

            let! token =
                choiceL
                    [
                        anyStringReturn directives
                        // # int
                        // We just peek the int to distinguish from InvalidDirective
                        // The actual int is lexed to a separate token
                        tuple3
                            (pchar '#')
                            (many1Chars (pchar ' '))
                            (followedBy (many1Chars (satisfy NumericLiterals.isDecimalDigit)))
                        >>% Token.LineIntDirective
                        pchar '#' .>> manyChars pIdentChar >>% Token.InvalidDirective
                    ]
                    "Directive"

            do! updateUserState (LexBuilder.append token pos CtxOp.NoOp)
        }


    let pHashToken =
        choiceL [ pDirectiveToken; pToken (pchar '#') Token.ReservedIdentifierHash ] "Hash or Directive"

    [<TailCall>]
    let rec lex () (reader: Reader<char, LexBuilder, ReadableString, _>) =
        let ctx = LexBuilder.currentContext reader.State
        let c = reader.Peek()
        // printfn "At %A, Context: %A, Next char: %A" reader.Position.Index ctx c
        match c, ctx with
        | ValueNone, _ -> preturn (LexBuilder.complete reader.Position.Index reader.State) reader
        | ValueSome('\r' | '\n'), ExpressionCtx -> (pNewlineToken >>= lex) reader
        | ValueSome ' ', ExpressionCtx -> (pIndentOrWhitespaceToken >>= lex) reader
        | ValueSome '\t', ExpressionCtx -> (pTabToken >>= lex) reader
        | ValueSome ',', ExpressionCtx -> (pCommaToken >>= lex) reader
        | ValueSome '(', ExpressionCtx -> (pLParenToken >>= lex) reader
        | ValueSome ')', ExpressionCtx -> (pCloseParenExpressionContext >>= lex) reader
        | ValueSome '[', ExpressionCtx -> (pLBacketToken >>= lex) reader
        | ValueSome '>', ExpressionCtx -> (pGreaterThanToken >>= lex) reader
        | ValueSome ']', ExpressionCtx -> (pCloseBracketExpressionContext >>= lex) reader
        | ValueSome '{', LexContext.InterpolatedString ->
            // In an interpolated string, { starts an expression or is escaped as {{
            (pInterpolatedExpressionStartToken >>= lex) reader
        | ValueSome '{', LexContext.VerbatimInterpolatedString ->
            // In a verbatim interpolated string, { starts an expression or is escaped as {{
            (pInterpolatedExpressionStartToken >>= lex) reader
        | ValueSome '{', LexContext.Interpolated3String level ->
            // In a triple-quoted interpolated string, { * level starts an expression or is escaped as {{
            (pInterpolated3ExpressionStartToken >>= lex) reader
        | ValueSome '{', _ -> (pOpenBraceExpressionContext >>= lex) reader
        | ValueSome '}', LexContext.InterpolatedString ->
            // In an interpolated string, } is escaped as }}
            (pInterpolatedStringFragmentRBraces >>= lex) reader
        | ValueSome '}', LexContext.VerbatimInterpolatedString ->
            // In a verbatim interpolated string, } is escaped as }}
            (pInterpolatedStringFragmentRBraces >>= lex) reader
        | ValueSome '}', LexContext.Interpolated3String level ->
            // In a triple-quoted interpolated string, required } depends on level
            (pInterpolated3StringFragmentRBraces >>= lex) reader
        | ValueSome '}', LexContext.InterpolatedExpression ->
            // In an interpolated expression, } ends the expression
            (pInterpolatedExpressionEndToken >>= lex) reader
        | ValueSome '}', NonInterpolatedExpressionCtx -> (pCloseBraceExpressionContext >>= lex) reader
        | ValueSome '"', LexContext.InterpolatedString -> (pInterpolatedStringEndToken >>= lex) reader
        | ValueSome '"', LexContext.VerbatimInterpolatedString -> (pVerbatimInterpolatedStringQuoteToken >>= lex) reader
        | ValueSome '"', LexContext.Interpolated3String _ -> (pInterpolated3EndToken >>= lex) reader
        | ValueSome '"', _ ->
            // String or triple-quoted string literals
            (pDoubleQuoteToken >>= lex) reader
        | ValueSome ''', ExpressionCtx ->
            // Char literals, type parameters, or a single quote
            (pSingleQuoteToken >>= lex) reader
        | ValueSome '$', ExpressionCtx ->
            // Interpolated strings
            (pDollarToken >>= lex) reader
        | ValueSome '@', ExpressionCtx ->
            // Verbatim strings
            (pAtToken >>= lex) reader
        | ValueSome '/', ExpressionCtx -> (pSlashToken >>= lex) reader
        | ValueSome '%',
          (LexContext.InterpolatedString | LexContext.VerbatimInterpolatedString | LexContext.Interpolated3String _) ->
            (pFormatSpecifierTokens >>= lex) reader
        | ValueSome _, LexContext.InterpolatedString -> (pInterpolatedStringFragmentToken >>= lex) reader
        | ValueSome _, LexContext.VerbatimInterpolatedString ->
            (pVerbatimInterpolatedStringFragmentToken >>= lex) reader
        | ValueSome _, LexContext.Interpolated3String _ -> (pInterpolated3StringFragmentToken >>= lex) reader
        | ValueSome ';', ExpressionCtx -> (pSemicolonToken >>= lex) reader
        | ValueSome '.', ExpressionCtx -> (pDotToken >>= lex) reader
        | ValueSome '#', ExpressionCtx -> (pHashToken >>= lex) reader
        | ValueSome c, ExpressionCtx when NumericLiterals.isDecimalDigit c ->
            (NumericLiterals.parseToken >>= lex) reader
        | ValueSome c, ExpressionCtx when customOperatorChars.Contains c ->
            // TODO: Consider computing names like https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/operator-overloading#overloaded-operator-names
            (pCustomOperatorToken >>= lex) reader
        | ValueSome c, ExpressionCtx when isIdentStartChar c -> (pIdentifierOrKeywordToken >>= lex) reader
        | ValueSome _, _ -> (pOtherToken >>= lex) reader


    let lexString (input: string) =
        let reader = Reader.ofString input (LexBuilder.init ())
        lex () reader


    let f xs =
        match xs with
        | [] -> 0
        | _ -> 1
