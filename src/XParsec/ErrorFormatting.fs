namespace XParsec

open System
open System.Collections.Immutable
open System.Text
open XParsec.Parsers
open XParsec.CharParsers

[<AbstractClass; Sealed>]
type LineEndings<'Input, 'InputSlice
    when 'Input :> IReadable<char, 'InputSlice> and 'InputSlice :> IReadable<char, 'InputSlice>>() =

    static let lineEndingParser: Parser<_, _, _, 'Input, 'InputSlice> =
        parser {
            let! _ = skipManyTill pid newline
            let! pos = getPosition
            return pos.Index - 1L
        }

    static let lineEndings: Parser<_, _, _, 'Input, 'InputSlice> = many lineEndingParser

    static member Parser = lineEndings

type LineIndex(endings: ImmutableArray<int64>, maxIndex) =

    member _.Indices = endings

    /// Returns the 1-based line number and column number of the given 0-based index
    member _.GetLineCol(index: int64) =
        if index < 0 then
            invalidArg "index" "Index must be non-negative"

        if index > maxIndex then
            raise (IndexOutOfRangeException $"Index must be less than or equal to {maxIndex}")
        // Line and column are 1-based
        if endings.IsEmpty then
            struct (1, index + 1L)
        else
            let minV = endings.[0]

            if index <= minV then
                1, index + 1L
            else
                // Binary search for the line number
                let rec findIndex low high =
                    if low = high then
                        low
                    else
                        match high - low with
                        | 1 ->
                            let highV = endings.[high]
                            if highV <= index then high else low
                        | diff ->
                            let mid = low + diff / 2
                            let midV = endings.[mid]

                            if midV = index then mid
                            elif midV < index then findIndex mid high
                            else findIndex low mid

                let iLine = findIndex 0 (endings.Length - 1)

                match index - endings.[iLine] with
                | 0L -> iLine + 1, index - endings.[iLine - 1]
                | col -> iLine + 2, col

    member _.GetIndex(line: int, col: int64) =
        if line < 1 then
            invalidArg "line" "Line must be greater than 0"

        if col < 1 then
            invalidArg "col" "Column must be greater than 0"

        match line with
        | 1 -> col - 1L
        | _ ->
            let iLine = endings.[line - 2]
            let i = iLine + int64 col

            if i > maxIndex then
                raise (IndexOutOfRangeException $"Index must be less than or equal to {maxIndex}")

            i

    static member OfString(input: string) =
        match LineEndings.Parser(Reader.ofString input ()) with
        | Ok result -> LineIndex(result.Parsed, int64 (input.Length - 1))
        | Error _ -> invalidOp "LineIndex: Failed to parse line endings"

    static member OfString(input: string, maxLength: int) =
        if maxLength < 0 then
            invalidArg "maxLength" "maxLength must be non-negative"

        if maxLength > input.Length then
            raise (ArgumentOutOfRangeException $"maxLength must be less than or equal to {input.Length}")

        let maxLength = int64 maxLength
        let reader = Reader.ofString input ()
        let reader = reader.Slice(0L, int64 maxLength)

        match LineEndings.Parser reader with
        | Ok result -> LineIndex(result.Parsed, maxLength - 1L)
        | Error _ -> invalidOp "LineIndex: Failed to parse line endings"


module ErrorFormatting =
    type StringBuilder with
        member this.Append(input: #IReadable<char, 'InputSlice>, start: int64, count: int) =
            let span = input.SpanSlice(start, count)
#if !FABLE_COMPILER && NET8_0_OR_GREATER
            this.Append(span)
#else
            for c in span do
                this.Append c |> ignore

            this
#endif

        member this.Append(input: #IReadable<char, 'InputSlice>) =
            if input.Length > Int32.MaxValue then
                invalidOp "StringBuilder.Append: input is too long"

            let span = input.SpanSlice(0L, int input.Length)
#if !FABLE_COMPILER && NET8_0_OR_GREATER
            this.Append(span)
#else
            for c in span do
                this.Append c |> ignore

            this
#endif

    [<Literal>]
    let private UpRight = '\u2514'

    [<Literal>]
    let private Horizontal = '\u2500'

    [<Literal>]
    let private Vertical = '\u2502'

    [<Literal>]
    let private VerticalRight = '\u251C'
    // TODO: Generalizing to IReadable int -> int64 conversion issues
    let private findIndexBack (input: #IReadable<char, 'InputSlice>) (index: int64) (backLimit: int) =
        let backLimit = int64 backLimit

        let rec loop i =
            if i <= 0L then
                0L
            else
                let i = min (input.Length - 1L) i

                match input.[i] with
                | '\r'
                | '\n' -> min (i + 1L) index
                | _ -> if index - i > backLimit then i else loop (i - 1L)

        loop index

    let private findIndexForward (input: #IReadable<char, 'InputSlice>) (index: int64) (forwardLimit: int) =
        let forwardLimit = int64 forwardLimit

        let rec loop i =
            if i >= input.Length then
                input.Length
            else
                match input.[i] with
                | '\r'
                | '\n' -> max (i - 1L) index
                | _ -> if i - index > forwardLimit then i else loop (i + 1L)

        loop index

    let private terminalSuberror = $"{UpRight}{Horizontal}{Horizontal}{Horizontal}"

    let private nonTerminalSuberror =
        $"{VerticalRight}{Horizontal}{Horizontal}{Horizontal}"

    let private terminalIndent = "    "
    let private nonTerminalIndent = $"{Vertical}   "

    type private Prefix =
        | T
        | NT

    let formatErrorsLine
        (lineIndex: LineIndex)
        (input: #IReadable<char, 'InputSlice>)
        (index: int64)
        (sb: StringBuilder)
        : StringBuilder =
        let iBack = findIndexBack input index 25
        let iForward = findIndexForward input index 40

        let sb = sb.Append(input, iBack, int (iForward - iBack)).AppendLine()

        let struct (ln, col) = lineIndex.GetLineCol index
#if FABLE_COMPILER
        // TODO: Fable doesn't support Append(char, int) overload
        // Was added in 5.0.0-alpha.6
        let spaces = String.replicate (int (index - iBack)) " "
        sb.Append(spaces).Append('^').AppendLine($" At index {index} (Ln {ln}, Col {col})")
#else
        sb.Append(' ', int (index - iBack)).Append('^').AppendLine($" At index {index} (Ln {ln}, Col {col})")
#endif

    let formatParseError<'T, 'State>
        (formatOne: 'T -> StringBuilder -> StringBuilder)
        (formatSeq: 'T seq -> StringBuilder -> StringBuilder)
        (error: ParseError<'T, 'State>)
        (sb: StringBuilder)
        =
        let inline appendString (s: string) (sb: StringBuilder) = sb.Append s
        let inline appendNewline (sb: StringBuilder) = sb.AppendLine()

        let appendPrefixes prefixes (sb: StringBuilder) =
            let rec appendIndent prefixes (sb: StringBuilder) =
                match prefixes with
                | [] -> sb
                | T :: prefixes -> sb |> appendIndent prefixes |> appendString terminalIndent
                | NT :: prefixes -> sb |> appendIndent prefixes |> appendString nonTerminalIndent

            match prefixes with
            | [] -> sb
            | [ T ] -> sb |> appendString terminalSuberror
            | [ NT ] -> sb |> appendString nonTerminalSuberror
            | T :: prefixes -> sb |> appendIndent prefixes |> appendString terminalSuberror
            | NT :: prefixes -> sb |> appendIndent prefixes |> appendString nonTerminalSuberror

        let rec f prefixes pos errors sb =
            match errors with
            | EndOfInput -> sb |> appendPrefixes prefixes |> appendString "Unexpected end of input"
            | Expected e -> sb |> appendPrefixes prefixes |> appendString "Expected " |> formatOne e

            | ExpectedSeq es -> sb |> appendPrefixes prefixes |> appendString "Expected " |> formatSeq es
            | Unexpected e -> sb |> appendPrefixes prefixes |> appendString "Unexpected " |> formatOne e
            | UnexpectedSeq es -> sb |> appendPrefixes prefixes |> appendString "Unexpected " |> formatSeq es
            | Nested(e, es) ->
                sb |> f prefixes pos e |> appendNewline |> ignore

                let rec g es =
                    match es with
                    | [] -> sb
                    | [ { Errors = e; Position = pos } ] -> sb |> f (T :: prefixes) pos e
                    | { Errors = e; Position = pos } :: es ->
                        sb |> f (NT :: prefixes) pos e |> appendNewline |> ignore
                        g es

                g es

            | Message m -> sb |> appendPrefixes prefixes |> appendString m

        let { Errors = errors; Position = pos } = error
        f [] pos errors sb

    let formatStringError (input: string) (error: ParseError<char, 'T>) =
        let index = LineIndex.OfString input
        let readable = ReadableString input

        let formatOne (x: char) (sb: StringBuilder) = sb.Append(''').Append(x).Append(''')

        let formatSeq (xs: char seq) (sb: StringBuilder) =
            sb.Append('"') |> ignore
            (sb, xs) ||> Seq.fold (fun sb x -> sb.Append x) |> ignore
            sb.Append('"')


        StringBuilder()
        |> formatErrorsLine index readable error.Position.Index
        |> formatParseError formatOne formatSeq error
        |> _.ToString()
