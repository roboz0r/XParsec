namespace XParsec

open System
open System.Collections.Immutable

type IErrorHandler<'T, 'State, 'Input> =
    abstract member ReportErrors: input: 'Input * errors: ParseError<'T, 'State> -> unit

module ParseErrors =
    open System.Text
    open XParsec.Parsers
    open XParsec.CharParsers

    [<Literal>]
    let UpRight = '\u2514'

    [<Literal>]
    let Horizontal = '\u2500'

    let lineEndingParser () : Parser<_, _, _, _, _> =
        parser {
            let! _ = skipManyTill pid newline
            let! pos = getPosition
            return int pos.Index
        }

    let lineEndings () : Parser<_, _, _, _, _> = many (lineEndingParser ())

    let rec formatError (formatOne: 'T -> string) (formatSeq: 'T seq -> string) (error: ErrorType<'T, 'State>) =
        match error with
        | EndOfInput -> "Unexpected end of input"
        | Expected(e) -> $"Expected {formatOne e}"
        | ExpectedSeq(es) -> $"Expected {formatSeq es}"
        | Unexpected(e) -> $"Unexpected {formatOne e}"
        | UnexpectedSeq(es) -> $"Unexpected {formatSeq es}"
        | Nested(e, _) -> formatError formatOne formatSeq e
        | Message(m) -> m


    let summarize (errors: ParseError<'T, 'State> list) =
        let sb = StringBuilder()
        let formatError = formatError string (sprintf "%A")

        let rec f i (errors: ParseError<'T, 'State> list) =

            errors
            |> List.iter (fun x ->
                if i > 0 then
#if FABLE_COMPILER
                    // TODO: Fable doesn't support Append(char, int) overload
                    let spaces = String.replicate (i * 2) " "
                    sb.Append(spaces).Append(UpRight).Append(Horizontal) |> ignore
#else
                    sb.Append(' ', (i - 1) * 2).Append(UpRight).Append(Horizontal) |> ignore
#endif
                match x.Errors with
                | ErrorType.Nested(e, es) ->
                    sb.AppendLine(formatError e) |> ignore
                    f (i + 1) es
                | m -> sb.AppendLine(formatError m) |> ignore)

        f 0 errors

#if FABLE_COMPILER
        sb.ToString().TrimEnd()
#else
        // TODO: Fable doesn't support set_Length
        let rec trimEnd (sb: StringBuilder) =
            if sb.Length > 0 && Char.IsWhiteSpace(sb.[sb.Length - 1]) then
                sb.Length <- sb.Length - 1
                trimEnd sb

        trimEnd sb
        sb.ToString()
#endif

type ConsoleErrorHandler() =

    let findIndexBack (input: string) (index: int) (backLimit) =
        let rec loop i =
            if i < 0 then
                0
            else
                match input.[i] with
                | '\r'
                | '\n' -> i + 1
                | _ -> if index - i > backLimit then i else loop (i - 1)

        loop index

    let findIndexForward (input: string) (index: int) (forwardLimit) =
        let rec loop i =
            if i >= input.Length then
                input.Length
            else
                match input.[i] with
                | '\r'
                | '\n' -> i - 1
                | _ -> if i - index > forwardLimit then i else loop (i + 1)

        loop index

    let formatErrorsAt (allEndings: ImmutableArray<int>) (input: string) (index, errors) =
        let iBack = findIndexBack input index 25
        let iForward = findIndexForward input index 40

        let inputLine = input.Substring(iBack, iForward - iBack)

        let (ln, col) =
            allEndings
            |> Seq.tryFindIndex (fun i -> i > index)
            |> Option.map (fun iLine -> (iLine + 1, index - allEndings.[iLine - 1] + 1))
            |> Option.defaultValue (allEndings.Length + 1, 1)

        let errorLine =
            $"""{String.replicate (index - iBack) " "}^ At index {index} (Ln {ln}, Col {col})"""

        printfn "%s\n%s" inputLine errorLine

        ParseErrors.summarize errors |> printfn "%s"

    member _.ReportErrors(input, errors) =
        let allEndings =
            match ParseErrors.lineEndings () (Reader.ofString input ()) with
            | Ok es -> es.Parsed
            | Error _ -> ImmutableArray.Empty //imm { } TODO: Fable doesn't support empty CEs?

        errors
        |> List.groupBy (fun e -> int e.Position.Index)
        |> List.sortBy (fun (i, _) -> i)
        |> List.iter (formatErrorsAt allEndings input)

    interface IErrorHandler<char, unit, string> with
        member this.ReportErrors(input, errors) = this.ReportErrors(input, [ errors ])
