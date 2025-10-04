[<AutoOpen>]
module XParsec.FSharp.Lexer.Tests.TestHelpers

open System
open System.IO

open Expecto

open XParsec
open XParsec.CharParsers
open XParsec.FSharp.Lexer
open XParsec.Parsers
open XParsec.FSharp.Lexer.Lexing

let truncateMiddle (maxLength: int) (s: string) =
    if s.Length <= maxLength then
        s
    else
        let partLength = (maxLength - 3) / 2
        let firstPart = s.Substring(0, partLength)
        let secondPart = s.Substring(s.Length - partLength, partLength)
        firstPart + "..." + secondPart

let escapeSpecialCharacters (s: string) =
    s.Replace("\n", "\\n").Replace("\r", "\\r").Replace("\t", "\\t")

let printLexed (input: string) (x: Lexed) =
    let rec f i =
        if i < x.Length then
            let pt = x.Tokens[i]
            let p = int pt.StartIndex

            let p1 =
                x.Tokens
                |> Seq.tryItem (i + 1)
                |> Option.map (fun pt1 -> int pt1.StartIndex)
                |> Option.defaultValue input.Length

            let isBlockComment =
                if pt.Token.InBlockComment then
                    " (in block comment)"
                else
                    ""

            let isInOCamlBlockComment =
                if pt.Token.InOCamlBlockComment then
                    " (in OCaml block comment)"
                else
                    ""

            match pt.TokenWithoutCommentFlags with
            | t when t.IsOperator ->
                let id = input.AsSpan(p, p1 - p).ToString()

                printfn
                    "%d, %s (%s) %s %s"
                    p
                    (Operator.generateOperatorName t id)
                    id
                    isBlockComment
                    isInOCamlBlockComment
            | Token.InvalidOperator
            | Token.Identifier
            | Token.OtherUnlexed
            | Token.EscapePercent
            | Token.EscapeLBrace
            | Token.EscapeRBrace
            | Token.FormatPlaceholder
            | Token.InvalidFormatPlaceholder
            | Token.InvalidFormatPercents
            | Token.InterpolatedExpressionOpen
            | Token.InterpolatedExpressionClose
            | Token.Interpolated3StringOpen
            | Token.Interpolated3StringClose
            | Token.InterpolatedStringOpen
            | Token.InterpolatedStringClose
            | Token.VerbatimInterpolatedStringOpen
            | Token.VerbatimInterpolatedStringClose as t ->
                let id = input.AsSpan(p, p1 - p).ToString()
                printfn "%d, %A (%s) %s %s" p t id isBlockComment isInOCamlBlockComment
            | t when t.IsNumeric ->
                let id = input.AsSpan(p, p1 - p).ToString()
                printfn "%d, %A (%s) %s %s" p t id isBlockComment isInOCamlBlockComment
            | Token.StringLiteral
            | Token.UnterminatedStringLiteral
            | Token.VerbatimStringLiteral
            | Token.UnterminatedVerbatimStringLiteral
            | Token.String3Literal
            | Token.UnterminatedString3Literal
            | Token.UnterminatedInterpolatedString
            | Token.InterpolatedStringFragment
            | Token.Interpolated3StringFragment
            | Token.VerbatimInterpolatedStringFragment as t ->
                let id = input.AsSpan(p, p1 - p).ToString()
                let id = escapeSpecialCharacters id
                let id = truncateMiddle 40 id
                printfn "%d, %A (%s) %s %s" p t id isBlockComment isInOCamlBlockComment

            | t -> printfn "%d, %A %s %s" p t isBlockComment isInOCamlBlockComment

            f (i + 1)

    f 0

let writeLexed (path: string) (x: Lexed) =
    use writer = new StreamWriter(path)

    let rec f i =
        if i < x.Length then
            writer.WriteLine($"%d{int x.Tokens[i].StartIndex}, %A{x.Tokens[i].Token}")
            f (i + 1)

    f 0

let pToken =
    // Get enum cases
    typeof<Token>.GetEnumValues()
    |> Seq.cast<Token>
    |> Seq.toArray
    |> Array.map (fun case ->
        // printfn "Creating parser for %O" case
        pstring (case.ToString()) >>% case
    )
    |> fun ps ->
        choiceL
            (seq {
                yield! ps
                yield puint16 |>> LanguagePrimitives.EnumOfValue // Fallback for any missing cases e.g. flagged with block comments
            })
            "token"


let readLexed (path: string) =
    let parser =
        many (pipe4 pint32 (pstring ", ") pToken newline (fun pos _ tok _ -> PositionedToken.Create(tok, pos)))
        .>> eof

    let text = File.ReadAllText path

    match parser (Reader.ofString text ()) with
    | Ok { Parsed = result } -> result |> List.ofSeq
    | Error err ->
        ErrorFormatting.formatStringError text err |> printfn "%s"
        failwithf "Failed to parse lexed file at %s" path

let testLexed (input: string) (expected: _ list) =
    // for i in 0 .. 100000 do
    // Run multiple times to catch any state issues
    match lexString input with
    | Ok { Parsed = lexed } ->
        try
            "" |> Expect.equal (lexed |> Lexed.asSeq |> List.ofSeq) expected
        with ex ->
            printfn "An error occurred: %s" (ex.Message)
            printLexed input lexed
            reraise ()
    | Error err ->
        printfn "Lexing failed: %A" err
        failwith "Lexing failed"

let testDataDir =
    lazy DirectoryInfo(IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "data")).FullName

let testData =
    lazy
        let dir = testDataDir.Value
        IO.Directory.GetFiles(dir, "*.fs")

let testLexFile (filePath: string) =
    let input = File.ReadAllText filePath
    let expectedPath = filePath + ".lexed"

    if not (File.Exists expectedPath) then
        // Doesn't exist so create it
        match lexString input with
        | Ok { Parsed = lexed } ->
            printfn "Expected lexed file does not exist at %s" expectedPath
            printfn "-------------\nInput was:\n%s" input
            printfn "-------------\nLexed output is:\n"
            printLexed input lexed
            printfn "-------------"
            writeLexed expectedPath lexed
            failtestf "Created expected lexed file at %s, please verify it is correct" expectedPath
        | Error err ->
            let pos = err.Position
            printLexed input (LexBuilder.complete pos)
            ErrorFormatting.formatStringError input err |> printfn "%s"
            // printfn "Lexing failed: %A" err
            failwith "Lexing failed"
    else
        let expected = readLexed expectedPath
        testLexed input expected
