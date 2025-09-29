namespace XParsec.FSharp.Lexer

open System
open System.Globalization
open System.Collections.Generic
open System.Collections.Immutable


type Lexed =
    {
        Tokens: ImmutableArray<PositionedToken>
    }

    member this.Length = this.Tokens.Length
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
        Tokens: ResizeArray<PositionedToken>
        mutable AtStartOfLine: bool
        mutable Context: LexContext list
    }

open System
open XParsec
open XParsec.Parsers
open XParsec.CharParsers
open MoreParsers

module Lexed =
    let asSeq (x: Lexed) = x.Tokens

[<RequireQualifiedAccess>]
[<Struct>]
type CtxOp =
    | Push of LexContext
    | Pop of LexContext
    | NoOp


module LexBuilder =


    let complete (pos: Position<LexBuilder>) =
        pos.State.Tokens.Add(PositionedToken.Create(Token.EOF, pos.Index))

        {
            Tokens = pos.State.Tokens.ToImmutableArray()
        }

    let init () =
        {
            Tokens = ResizeArray<PositionedToken>()
            AtStartOfLine = true
            Context = []
        }

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
        | Token.Interpolated3StringFragment
        | Token.VerbatimInterpolatedStringFragment
        | Token.InterpolatedStringFragment -> true
        | _ -> false

    let appendI token idx ctxOp (state: LexBuilder) =
        // Coalesce adjacent string fragments
        // TODO: Handle literal negation
        // Consider if this should be done in the `lex` function instead
        // https://fsharp.github.io/fslang-spec/lexical-analysis/#381-post-filtering-of-adjacent-prefix-tokens
        let addToken =
            let count = state.Tokens.Count

            if count > 0 then
                match token, state.Tokens[count - 1] with
                | CoalescableToken, t when t.Token = token -> false
                | _ -> true
            else
                true

        if addToken then
            state.Tokens.Add(PositionedToken.Create(token, idx))

        state.AtStartOfLine <-
            match token with
            | Token.Newline -> true
            | _ -> false

        match ctxOp with
        | (CtxOp.Push ctx) -> pushContext ctx state
        | (CtxOp.Pop ctx) -> popExactContext ctx state
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

    let keywords =
        [|
            "_", Token.Wildcard
            // ident-keyword
            "abstract", Token.KWAbstract
            "and", Token.KWAnd
            "as", Token.KWAs
            "assert", Token.KWAssert
            "base", Token.KWBase
            "begin", Token.KWBegin
            "break", Token.KWBreak
            "checked", Token.KWChecked
            "class", Token.KWClass
            "component", Token.KWComponent
            "const", Token.KWConst
            "constraint", Token.KWConstraint
            "continue", Token.KWContinue
            "default", Token.KWDefault
            "delegate", Token.KWDelegate
            "do", Token.KWDo
            "done", Token.KWDone
            "downcast", Token.KWDowncast
            "downto", Token.KWDownto
            "elif", Token.KWElif
            "else", Token.KWElse
            "end", Token.KWEnd
            "event", Token.KWEvent
            "exception", Token.KWException
            "extern", Token.KWExtern
            "external", Token.KWExternal
            "false", Token.KWFalse
            "finally", Token.KWFinally
            "fixed", Token.KWFixed
            "for", Token.KWFor
            "fun", Token.KWFun
            "function", Token.KWFunction
            "global", Token.KWGlobal
            "if", Token.KWIf
            "in", Token.KWIn
            "include", Token.KWInclude
            "inherit", Token.KWInherit
            "inline", Token.KWInline
            "interface", Token.KWInterface
            "internal", Token.KWInternal
            "lazy", Token.KWLazy
            "let", Token.KWLet
            "match", Token.KWMatch
            "member", Token.KWMember
            "mixin", Token.KWMixin
            "mod", Token.KWMod
            "module", Token.KWModule
            "mutable", Token.KWMutable
            "namespace", Token.KWNamespace
            "new", Token.KWNew
            "not", Token.KWNot
            "null", Token.KWNull
            "of", Token.KWOf
            "open", Token.KWOpen
            "or", Token.KWOr
            "override", Token.KWOverride
            "parallel", Token.KWParallel
            "private", Token.KWPrivate
            "process", Token.KWProcess
            "protected", Token.KWProtected
            "public", Token.KWPublic
            "pure", Token.KWPure
            "rec", Token.KWRec
            "return", Token.KWReturn
            "sealed", Token.KWSealed
            "select", Token.KWSelect
            "sig", Token.KWSig
            "static", Token.KWStatic
            "struct", Token.KWStruct
            "tailcall", Token.KWTailcall
            "then", Token.KWThen
            "to", Token.KWTo
            "trait", Token.KWTrait
            "true", Token.KWTrue
            "try", Token.KWTry
            "type", Token.KWType
            "upcast", Token.KWUpcast
            "use", Token.KWUse
            "val", Token.KWVal
            "virtual", Token.KWVirtual
            "void", Token.KWVoid
            "when", Token.KWWhen
            "while", Token.KWWhile
            "with", Token.KWWith
            "yield", Token.KWYield

            // symbolic keywords
            "let!", Token.KWLetBang
            "use!", Token.KWUseBang
            "do!", Token.KWDoBang
            "yield!", Token.KWYieldBang
            "return!", Token.KWReturnBang
            "and!", Token.KWAndBang
            "match!", Token.KWMatchBang

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

            // Identifier replacements
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
    let private combiningOperatorChars =
        [|
            '>'
            '<'
            '+'
            '-'
            '*'
            '/'
            '='
            '~'
            '$' // This construct is deprecated: '$' is not permitted as a character in operator names and is reserved for future use
            '%'
            '.'
            '&'
            '|'
            '@'
            '^'
            '!'
            '?'
            ':' // Per https://github.com/dotnet/fsharp/pull/15923 `:` in operators is deprecated *unless* the operator starts with '>'
        |] // ':' isn't listed but it works

    let (><-*/=~%.&|@^!?:) a b = a + b

    let private parenChars = [| '('; ')'; '{'; '}'; '['; ']' |]

    // let private endOfIdentChars =
    //     [|
    //         yield! parenChars
    //         yield! combiningOperatorChars
    //         ' '
    //         '\r'
    //         '\n'
    //         ','
    //         '\t'
    //         '"'
    //     |]
    //     |> String

    // let peekEndOfIdent (reader: Reader<char, LexBuilder, ReadableString, _>) =
    //     match reader.Peek() with
    //     | ValueNone -> preturn () reader
    //     | ValueSome c when endOfIdentChars.Contains c -> preturn () reader
    //     | ValueSome _ -> fail expectedEndOfIdent reader


    // let private pKeywordToken =
    //     parser {
    //         let! pos = getPosition
    //         let! token = pKeyword .>> followedBy peekEndOfIdent
    //         do! updateUserState (LexBuilder.append token pos CtxOp.NoOp)
    //     }

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
            | UnicodeCategory.LetterNumber ->  //Nl
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
            | UnicodeCategory.Format ->  //Cf
                true
            | _ -> false

    let pIdentChar = satisfyL isIdentChar "identifier character"
    let peekEndOfIdent = notFollowedBy pIdentChar >>% ()

    let pIdentifier: Parser<_, char, LexBuilder, ReadableString, _> =
        many1Chars2 pIdentStartChar pIdentChar

    let pIdentifierOrKeywordToken =
        let keywords = dict keywords

        parser {
            let! pos = getPosition
            let! id = pIdentifier .>> followedBy peekEndOfIdent

            let token =
                match keywords.TryGetValue id with
                | true, kw -> kw
                | false, _ -> Token.Identifier

            do! updateUserState (LexBuilder.append token pos CtxOp.NoOp)
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
        let keywords = dict keywords

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
                        |> LexBuilder.append Token.SingleQuote pos CtxOp.NoOp
                        |> LexBuilder.appendI token (pos.Index + 1L) CtxOp.NoOp

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
                            (idx + int64 diff)
                            (CtxOp.Push LexContext.InterpolatedExpression)
                )
        }


    let pOpenBraceExpressionContext =
        pTokenPushCtx (pchar '{') Token.OpBraceLeft LexContext.BracedExpression

    let pCloseBraceExpressionContext =
        pTokenPopCtx (pchar '}') Token.OpBraceRight LexContext.BracedExpression

    let pOpenParenExpressionContext =
        pTokenPushCtx (pchar '(') Token.OpParenLeft LexContext.ParenthesExpression

    let pCloseParenExpressionContext =
        pTokenPopCtx (pchar ')') Token.OpParenRight LexContext.ParenthesExpression

    let pOpenBracketExpressionContext =
        pTokenPushCtx (pchar '[') Token.OpBracketLeft LexContext.BracketedExpression

    let pCloseBracketExpressionContext =
        pTokenPopCtx (pchar ']') Token.OpBracketRight LexContext.BracketedExpression

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
                            state <- LexBuilder.appendI Token.OpBraceRight (pos.Index + int64 i) CtxOp.NoOp state

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
                do! updateUserState (LexBuilder.appendI Token.EscapeRBrace idx CtxOp.NoOp)
                idx <- idx + 2
                count <- count - 2

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
            elif diff = 0 then
                do!
                    updateUserState (
                        LexBuilder.appendI
                            Token.InterpolatedExpressionClose
                            idx
                            (CtxOp.Pop LexContext.InterpolatedExpression)
                    )
            elif diff >= level then
                // We have more } than 2x the current level, treat the whole block as invalid
                do! updateUserState (LexBuilder.appendI Token.TooManyRBracesInInterpolated3String idx CtxOp.NoOp)
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
        }

    let pVerbatimInterpolatedStringEndToken =
        pTokenPopCtx
            (pchar '"' .>> notFollowedBy (pchar '"'))
            Token.VerbatimInterpolatedStringClose
            LexContext.VerbatimInterpolatedString

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

    let pComment = pstring "//" >>. manyCharsTill anyChar (peekNewLine <|> eof)

    let pCombiningOperator = many1Chars (anyOf combiningOperatorChars)

    let pOperatorToken =
        parser {
            let! pos = getPosition
            let! op = pCombiningOperator

            do!
                updateUserState (fun state ->

                    match op with
                    | "<@" ->
                        LexBuilder.append Token.OpQuotationTypedLeft pos (CtxOp.Push LexContext.QuotedExpression) state
                    | "@>" ->
                        LexBuilder.append Token.OpQuotationTypedRight pos (CtxOp.Pop LexContext.QuotedExpression) state
                    | "<@@" ->
                        LexBuilder.append
                            Token.OpQuotationUntypedLeft
                            pos
                            (CtxOp.Push LexContext.TypedQuotedExpression)
                            state
                    | "@@>" ->
                        LexBuilder.append
                            Token.OpQuotationUntypedRight
                            pos
                            (CtxOp.Pop LexContext.TypedQuotedExpression)
                            state
                    | "." -> LexBuilder.append Token.OpDot pos CtxOp.NoOp state
                    | "?" -> LexBuilder.append Token.OpDynamic pos CtxOp.NoOp state
                    | "?<-" -> LexBuilder.append Token.OpDynamicAssignment pos CtxOp.NoOp state
                    | _ ->
                        let ignored_op_char = ".$?"

                        let token =
                            if
                                op.Contains '$'
                                || (not (op.AsSpan().TrimStart(ignored_op_char).StartsWith(">")) && op.Contains ':')
                            then
                                // '$' is not permitted as a character in operator names and is reserved for future use
                                // ':' is not permitted as a character in operator names and is reserved for future use
                                // Except when it starts with '>' after trimming ignored chars
                                // https://github.com/dotnet/fsharp/pull/15923
                                Token.ReservedOperator
                            else
                                Token.ofCustomOperator (op.AsSpan())

                        LexBuilder.append token pos CtxOp.NoOp state
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
                ".[", (Token.OpIndexLeft, CtxOp.Push LexContext.BracketedExpression)
                // ML style index operators
                ".()<-", (Token.OpIndexSetParenIdentifier, CtxOp.NoOp)
                ".()", (Token.OpIndexGetParenIdentifier, CtxOp.NoOp)
                ".(", (Token.OpIndexLeftParen, CtxOp.Push LexContext.ParenthesExpression)
            |]

        let pSpecialOperatorToken = anyStringReturn specialOperators

        parser {
            let! pos = getPosition
            let! (token, ctxOp) = pSpecialOperatorToken
            do! updateUserState (LexBuilder.append token pos ctxOp)
        }

    let pLAttrBrack = pstring "[<"
    let pRAttrBrack = pstring ">]"

    // TODO: Block comments still mean all internal tokens get lexed as closing block comment in a string "*)"
    // doesn't close a block comment
    // let pBlockComment =
    //     pstring "(*" >>. manyCharsTill anyChar (pstring "*)" >>% () <|> eof) |>> fst
    // let pOCamlBlockComment = pstring "(*IF-OCAML*)" >>. manyCharsTill anyChar (pstring "(*ENDIF-OCAML*)" >>% () <|> eof) |>> fst
    // (*F# .... F#*) and (*IF-FSHARP ... ENDIF-FSHARP*)
    // This is a fallback, we shouldn't see any Other tokens in output
    let pOther = manyCharsTill anyChar peekEndOfIdent

    module NumericLiterals =
        let isHexDigit c =
            (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

        let pHexDigit = satisfyL isHexDigit "hexadecimal digit"

        let isBinaryDigit c = c = '0' || c = '1'
        let pBinaryDigit = satisfyL isBinaryDigit "binary digit"

        let isOctalDigit c = c >= '0' && c <= '7'
        let pOctalDigit = satisfyL isOctalDigit "octal digit"

        let isDecimalDigit c = c >= '0' && c <= '9'
        let pDecimalDigit = satisfyL isDecimalDigit "decimal digit"

        let pIntBase = sepBy1 (many1Chars pDecimalDigit) (many1Chars (pchar '_'))
        let pHexBase = sepBy1 (many1Chars pHexDigit) (many1Chars (pchar '_'))
        let pOctalBase = sepBy1 (many1Chars pOctalDigit) (many1Chars (pchar '_'))
        let pBinaryBase = sepBy1 (many1Chars pBinaryDigit) (many1Chars (pchar '_'))

        let pXIntBase =
            choiceL
                [
                    tuple3 (pstring "0x") pHexBase (preturn NumericBase.Hex)
                    tuple3 (pstring "0X") pHexBase (preturn NumericBase.Hex)
                    tuple3 (pstring "0o") pOctalBase (preturn NumericBase.Octal)
                    tuple3 (pstring "0O") pOctalBase (preturn NumericBase.Octal)
                    tuple3 (pstring "0b") pBinaryBase (preturn NumericBase.Binary)
                    tuple3 (pstring "0B") pBinaryBase (preturn NumericBase.Binary)
                ]
                "base 16, 8, 2 integer"
            |>> fun struct (strBase, struct (parts, _), numBase) -> struct ($"""{strBase}{String.concat "" parts}""", numBase)

        let pIntToken =
            parser {
                let! pos = getPosition

                let! (digits, numBase) =
                    choice
                        [
                            pIntBase .>>. preturn NumericBase.Decimal
                            pHexBase .>>. preturn NumericBase.Hex
                            pOctalBase .>>. preturn NumericBase.Octal
                            pBinaryBase .>>. preturn NumericBase.Binary
                        ]

                let! suffix = many1Chars pIdentChar

                let token =
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
                    | "Q"
                    | "R"
                    | "Z"
                    | "I"
                    | "N"
                    | "G" -> Token.NumBigInteger
                    | "m"
                    | "M" -> Token.NumDecimal
                    | _ -> Token.ReservedNumericLiteral

                // Combine the base into the token using the 5,4 bits of a 16 bit int
                let numBase = uint16 numBase <<< 4
                let token = Token.ofUInt16 (uint16 token ||| numBase)

                do! updateUserState (LexBuilder.append token pos CtxOp.NoOp)
            }

        let pFloatBase =
            pipe3
                pIntBase
                (pchar '.' .>> notFollowedBy (pchar '.'))
                (opt pIntBase)
                (fun struct (intParts, _) _ frac ->
                    match frac with
                    | ValueSome(fracParts, _) -> $"""{String.concat "" intParts}.{String.concat "" fracParts}"""
                    | _ -> $"""{String.concat "" intParts}."""
                )
            <|> pipe5
                pIntBase
                (opt (pchar '.' .>>. opt pIntBase))
                (anyOf "eE")
                (opt (anyOf "+-"))
                pIntBase
                (fun struct (intParts, _) fracOpt e signOpt struct (expParts, _) ->
                    let fracStr =
                        match fracOpt with
                        | ValueSome(dot, ValueSome(fracParts, fracSeps)) -> $"""{dot}{String.concat "" fracParts}"""
                        | ValueSome(dot, _) -> "."
                        | _ -> ""

                    let signStr =
                        match signOpt with
                        | ValueSome sign -> string sign
                        | _ -> ""

                    $"""{String.concat "" intParts}{fracStr}{e}{signStr}{String.concat "" expParts}"""
                )

        let pFloatToken =
            parser {
                let! pos = getPosition
                let! (number, numBase) = (pFloatBase .>>. preturn NumericBase.Decimal) <|> pXIntBase
                let! suffix = many1Chars pIdentChar
                let token =
                    match numBase, suffix with
                    | NumericBase.Decimal, "" -> Token.NumIEEE64
                    | NumericBase.Decimal, ("f" | "F") -> Token.NumIEEE32
                    | NumericBase.Decimal, ("m" | "M") -> Token.NumDecimal
                    | _, "LF" -> Token.NumIEEE64
                    | _, "lf" -> Token.NumIEEE32
                    | _ -> Token.ReservedNumericLiteral

                let numBase = uint16 numBase <<< 4
                let token = Token.ofUInt16 (uint16 token ||| numBase)
                do! updateUserState (LexBuilder.append token pos CtxOp.NoOp)
            }

    module Operator =
        open System.Collections.Concurrent
        let generatedNameCache = ConcurrentDictionary<string, string>()

        let generateOperatorName (operator: string) =
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

            match Map.tryFind operator standardOperators with
            | Some name -> name
            | None ->
                generatedNameCache.GetOrAdd(operator, fun operator ->
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
                let level = LexBuilder.level state
                do! skipNOf level '%'
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
            lFormatPlaceholder >>% Token.FormatPlaceholder <|> preturn Token.InvalidFormatPlaceholder

        // TODO: Rewrite to use updateUserState instead of directly manipulating the token list
        // This is tricky because we need to add multiple tokens in some cases
        let pFormatSpecifierTokens: Parser<unit, _, _, ReadableString, _> =
            parser {
                let! percents = many1Chars (pchar '%')
                let! state = getUserState
                let! pos = getPosition
                let tokens = state.Tokens

                match LexBuilder.level state with
                | 1 ->
                    // Level 1 logic
                    let mutable count = percents.Length
                    let mutable idx = pos.Index

                    while count > 1 do
                        if count >= 2 then // %% is an escape sequence for '%'
                            tokens.Add(PositionedToken.Create(Token.EscapePercent, idx))
                            idx <- idx + 2L
                            count <- count - 2

                    match count with
                    | 0 ->
                        do! skipN percents.Length
                        return ()
                    | _ ->
                        do! skipN (percents.Length - 1)
                        let! t = lFormatPlaceholderToken
                        tokens.Add (PositionedToken.Create(t, idx))

                | level ->

                    // Level 2 or higher logic, impossible to get 0 or negative from many1Chars
                    let count = percents.Length
                    let leading = count - level
                    let idx = int pos.Index

                    if leading < 0 then
                            tokens.Add (PositionedToken.Create(Token.InterpolatedStringFragment, idx))
                            do! skipN count
                    elif leading = 0 then
                        // Exactly enough to start a FormatPlaceholder
                        let! t = lFormatPlaceholderToken
                        tokens.Add (PositionedToken.Create(t, idx)) 
                    elif leading >= level then
                        // Too many leading '%'
                        tokens.Add (PositionedToken.Create(Token.InvalidFormatPercents, idx))
                    else
                        tokens.Add (PositionedToken.Create(Token.InterpolatedStringFragment, idx))
                        do! skipN leading
                        let! pos = getPosition
                        let! t = lFormatPlaceholderToken
                        tokens.Add (PositionedToken.Create(t, pos.Index))
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

    [<TailCall>]
    let rec lex () (reader: Reader<char, LexBuilder, ReadableString, _>) =
        let ctx = LexBuilder.currentContext reader.State
        let c = reader.Peek()
        // printfn "At %A, Context: %A, Next char: %A" reader.Position.Index ctx c
        match c, ctx with
        | ValueNone, _ -> preturn (LexBuilder.complete reader.Position) reader
        | ValueSome('\r' | '\n'), ExpressionCtx -> (pNewlineToken >>= lex) reader
        | ValueSome ' ', ExpressionCtx -> (pIndentOrWhitespaceToken >>= lex) reader
        | ValueSome '\t', ExpressionCtx -> (pToken (pchar '\t') Token.Tab >>= lex) reader
        | ValueSome ',', ExpressionCtx -> (pToken (pchar ',') Token.OpComma >>= lex) reader
        | ValueSome '(', ExpressionCtx ->

            (choice [  pOpenParenExpressionContext ]
             >>= lex)
                reader
        | ValueSome ')', ExpressionCtx -> (pCloseParenExpressionContext >>= lex) reader
        | ValueSome '[', ExpressionCtx ->
            (choice [ pToken pLAttrBrack Token.OpAttributeBracketLeft; pOpenBracketExpressionContext ] >>= lex) reader
        | ValueSome '>', ExpressionCtx -> (choice [ pToken pRAttrBrack Token.OpAttributeBracketRight; pOperatorToken ] >>= lex) reader
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
        | ValueSome '"', LexContext.VerbatimInterpolatedString -> (pVerbatimInterpolatedStringEndToken >>= lex) reader
        | ValueSome '"', LexContext.Interpolated3String _ -> (pInterpolated3EndToken >>= lex) reader
        | ValueSome '"', _ -> (choice [ pString3LiteralToken; pStringLiteralToken ] >>= lex) reader
        | ValueSome ''', ExpressionCtx ->
            (choice [ pCharToken; pTypeParamToken; pToken (pchar '\'') Token.SingleQuote ]
             >>= lex)
                reader
        | ValueSome '$', ExpressionCtx ->
            // Interpolated strings
            (choice
                [
                    pInterpolated3StartToken
                    pInterpolatedStringStartToken
                    pVerbatimInterpolatedStartToken
                    // This construct is deprecated: '$' is not permitted as a character in operator names and is reserved for future use
                    pOperatorToken
                ]
             >>= lex)
                reader
        | ValueSome '@', ExpressionCtx ->
            // Verbatim strings
            (choice [ pVerbatimInterpolatedStartToken; pVerbatimStringLiteralToken; pOperatorToken ]
             >>= lex)
                reader
        | ValueSome '/', ExpressionCtx -> (choice [ pToken pComment Token.LineComment; pOperatorToken ] >>= lex) reader
        | ValueSome '%',
          (LexContext.InterpolatedString | LexContext.VerbatimInterpolatedString | LexContext.Interpolated3String _) ->
            (pFormatSpecifierTokens >>= lex) reader
        | ValueSome _, LexContext.InterpolatedString -> (pInterpolatedStringFragmentToken >>= lex) reader
        | ValueSome _, LexContext.VerbatimInterpolatedString ->
            (pVerbatimInterpolatedStringFragmentToken >>= lex) reader
        | ValueSome _, LexContext.Interpolated3String _ -> (pInterpolated3StringFragmentToken >>= lex) reader
        | ValueSome '.', ExpressionCtx -> ((pSpecialDotOperatorToken <|> pOperatorToken) >>= lex) reader
        | ValueSome c, ExpressionCtx when NumericLiterals.isDecimalDigit c ->
            (choiceL [ NumericLiterals.pIntToken; NumericLiterals.pFloatToken ] "Numeric literal"
             >>= lex)
                reader
        | ValueSome c, ExpressionCtx when Array.contains c combiningOperatorChars ->
            // TODO: Consider computing names like https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/operator-overloading#overloaded-operator-names
            (pOperatorToken >>= lex) reader
        | ValueSome '_', ExpressionCtx -> (pToken pIdentifier Token.Identifier >>= lex) reader
        | ValueSome c, ExpressionCtx when isIdentStartChar c -> (pIdentifierOrKeywordToken >>= lex) reader
        | ValueSome _, _ -> (pToken pOther Token.OtherUnlexed >>= lex) reader


    let lexString (input: string) =
        let reader = Reader.ofString input (LexBuilder.init ())
        lex () reader


    let f xs =
        match xs with
        | [] -> 0
        | _ -> 1
