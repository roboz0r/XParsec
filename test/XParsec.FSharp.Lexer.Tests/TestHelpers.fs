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

let private sprintToken (input: string) (pt: PositionedToken) (p1: int) =
    let p = int pt.StartIndex

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

        sprintf "%d, %s (%s) %s %s" p (Operator.generateOperatorName t id) id isBlockComment isInOCamlBlockComment
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
        sprintf "%d, %A (%s) %s %s" p t id isBlockComment isInOCamlBlockComment
    | t when t.IsNumeric ->
        let id = input.AsSpan(p, p1 - p).ToString()
        sprintf "%d, %A (%s) %s %s" p t id isBlockComment isInOCamlBlockComment
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
        sprintf "%d, %A (%s) %s %s" p t id isBlockComment isInOCamlBlockComment

    | t -> sprintf "%d, %A %s %s" p t isBlockComment isInOCamlBlockComment

let printLexed (input: string) (x: Lexed) =
    let rec f i =
        if i < x.Tokens.Length then
            let pt = x.Tokens[i * 1<token>]

            let p1 =
                x.Tokens
                |> Seq.tryItem (i + 1)
                |> Option.map (fun pt1 -> int pt1.StartIndex)
                |> Option.defaultValue input.Length

            let s = sprintToken input pt p1
            printfn "%s" s

            f (i + 1)

    f 0

let printLexedBlocks (input: string) (x: Lexed) =
    let rec f i =
        if i < x.Blocks.Length then
            let iT = x.Blocks[i * 1<block>].TokenIndex
            let pt = x.Tokens[iT]
            // For debugging, print the first line of the block
            let charINextLine =
                let i =
                    x.LineStarts
                    |> Seq.findIndex (fun pos -> pos >= iT && x.Tokens[pos].TokenWithoutCommentFlags <> Token.Newline)

                if i + 1 < x.LineStarts.Length then
                    int x.Tokens.[x.LineStarts[(i + 1) * 1<line>]].StartIndex
                else
                    input.Length

            let p1 = min (charINextLine - 1) (int pt.StartIndex + 40)
            let s = input.AsSpan(int pt.StartIndex, p1 - int pt.StartIndex).ToString()
            let s = escapeSpecialCharacters s
            let s = sprintf "%d, %A (%s)" (int pt.StartIndex) pt.Token s
            printfn "%s" s

            f (i + 1)

    // printfn "%A" (List.ofSeq (x.LineStarts |> Seq.map (fun iT -> x.Tokens[iT])))
    // printfn "%A" (List.ofSeq (x.Blocks |> Seq.map (fun iT -> x.Tokens[iT])))

    f 0

let private getTokenString (t: PositionedToken) = $"%d{int t.StartIndex}, %A{t.Token}"

let writeLexed (path: string) (x: Lexed) =
    use writer = new StreamWriter(path)

    let rec f i =
        if i < x.Tokens.Length then
            let t = x.Tokens[i * 1<token>]
            writer.WriteLine(getTokenString t)
            f (i + 1)

    f 0

let writeLexedBlocks (path: string) (x: Lexed) =
    use writer = new StreamWriter(path)

    let rec f i =
        if i < x.Blocks.Length then
            let ti = x.Blocks[i * 1<block>].TokenIndex
            let token = x.Tokens[ti]
            writer.WriteLine(getTokenString token)
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
            "" |> Expect.equal (lexed.Tokens |> List.ofSeq) expected
        with ex ->
            printfn "An error occurred: %s" (ex.Message)
            printLexed input lexed
            reraise ()
    | Error err ->
        printfn "Lexing failed: %A" err
        failwith "Lexing failed"

let testLexedBlocks (input: string) (expected: _ list) =
    // for i in 0 .. 100000 do
    // Run multiple times to catch any state issues
    match lexString input with
    | Ok { Parsed = lexed } ->
        try
            ""
            |> Expect.equal (lexed.Blocks |> Seq.map (fun b -> lexed.Tokens[b.TokenIndex]) |> List.ofSeq) expected
        with ex ->
            printfn "An error occurred: %s" (ex.Message)
            printLexed input lexed
            reraise ()
    | Error err ->
        printfn "Lexing failed: %A" err
        failwith "Lexing failed"

let testDataDir =
    lazy DirectoryInfo(IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "data")).FullName

let blocksTestDataDir = lazy IO.Path.Combine(testDataDir.Value, "blocks")

let testData =
    lazy
        let dir = testDataDir.Value
        IO.Directory.GetFiles(dir, "*.fs")

let blocksTestData =
    lazy
        let dir = blocksTestDataDir.Value
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
            printLexed input (LexBuilder.complete pos.Index pos.State)
            ErrorFormatting.formatStringError input err |> printfn "%s"
            // printfn "Lexing failed: %A" err
            failwith "Lexing failed"
    else
        let expected = readLexed expectedPath
        testLexed input expected


let testLexFileBlocks (filePath: string) =
    let input = File.ReadAllText filePath
    let expectedPath = filePath + ".lexedblocks"

    if not (File.Exists expectedPath) then
        // Doesn't exist so create it
        match lexString input with
        | Ok { Parsed = lexed } ->
            printfn "Expected blocks file does not exist at %s" expectedPath
            printfn "-------------\nInput was:\n%s" input
            printfn "-------------\nLexed output is:\n"
            printLexedBlocks input lexed
            printfn "-------------"
            writeLexedBlocks expectedPath lexed
            failtestf "Created expected blocks file at %s, please verify it is correct" expectedPath
        | Error err ->
            let pos = err.Position
            printLexedBlocks input (LexBuilder.complete pos.Index pos.State)
            ErrorFormatting.formatStringError input err |> printfn "%s"
            // printfn "Lexing failed: %A" err
            failwith "Lexing failed"
    else
        let expected = readLexed expectedPath
        testLexedBlocks input expected
