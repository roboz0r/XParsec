module LexerTests

open System
open System.IO

open Expecto

open XParsec
open XParsec.Parsers
open XParsec.CharParsers
open XParsec.FSharp.Lexer
open XParsec.FSharp.Lexer.Lexing

let truncateMiddle (maxLength: int) (s: string) =
    if s.Length <= maxLength then s
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
            let p1 = x.Tokens |> Seq.tryItem (i + 1) |> Option.map (fun pt1 -> int pt1.StartIndex) |> Option.defaultValue input.Length
            match pt.TokenWithoutCommentFlags with
            | t when t.IsOperator ->
                let id = input.AsSpan(p, p1 - p).ToString()
                printfn "%d, %A (%s)" p t id
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
                printfn "%d, %A (%s)" p t id
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
                printfn "%d, %A (%s)" p t id
            
            | t ->
                printfn "%d, %A" p t
            f (i + 1)

    f 0

let writeLexed (path: string) (x: Lexed) =
    use writer = new StreamWriter(path)
    let rec f i =
        if i < x.Length then
            writer.WriteLine($"%d{int x.Tokens[i].StartIndex}, %A{x.Tokens[i].Token}"  )
            f (i + 1)

    f 0

let pToken =
    // Get enum cases
    typeof<Token>.GetEnumValues()
    |> Seq.cast<Token>
    |> Seq.toArray
    |> Array.map (fun case -> 
        printfn "Creating parser for %O" case
        pstring (case.ToString()) >>% case)
    |> fun ps -> choiceL ps "token"


let readLexed (path: string) =
    let parser = many (pipe4 pint32 (pstring ", ") pToken newline (fun pos _ tok _ -> PositionedToken.Create(tok, pos))) .>> eof
    let text = File.ReadAllText path
    match parser (Reader.ofString text ()) with
    | Ok { Parsed = result } ->
        result |> List.ofSeq
    | Error err ->
        failwithf "Error reading lexed file: %A" err

let testLexed (input: string) (expected: _ list) =
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
                printfn "Lexing failed: %A" err
                failwith "Lexing failed"
    else
        let expected = readLexed expectedPath
        testLexed input expected

[<Tests>]
let tests =
    testList
        "DocsTests"
        [
            test "Index" {

                let snippet =
                    [
                        "module Test"
                        "let html s = s"
                        "html $\"<p>Hello, World!</p>\""
                    ]
                    |> String.concat "\n"

                let expected =
                    [
                        0, Token.KWModule
                        6, Token.Whitespace
                        7, Token.Identifier
                        11, Token.Newline
                        12, Token.KWLet
                        15, Token.Whitespace
                        16, Token.Identifier
                        20, Token.Whitespace
                        21, Token.Identifier
                        22, Token.Whitespace
                        23, Token.OpEquality
                        24, Token.Whitespace
                        25, Token.Identifier
                        26, Token.Newline
                        27, Token.Identifier
                        31, Token.Whitespace
                        32, Token.InterpolatedStringOpen
                        34, Token.InterpolatedStringFragment
                        54, Token.InterpolatedStringClose
                        55, Token.EOF
                    ]
                    |> List.map (fun (pos, tok) -> PositionedToken.Create(tok, pos))

                testLexed snippet expected
            }

            for file in testData.Value do
                let name = IO.Path.GetFileName file

                test $"Lexing {name}" {
                    testLexFile file
                }

            ftest "Temp test" {
                let fileName = "04_triple_dollar_with_curlies.fs"
                let file = IO.Path.Combine(testDataDir.Value, fileName)
                let snippet = File.ReadAllText file
                printfn "Lexing\n%s\n-----" fileName
                match lexString snippet with
                | Ok { Parsed = lexed } ->
                    printLexed snippet lexed
                | Error err ->
                    let lexed = LexBuilder.complete err.Position
                    printLexed snippet lexed
                    printfn "-----"
                    let s = XParsec.ErrorFormatting.formatStringError snippet err

                    printfn "%s" s
                    failwith "Lexing failed"
            }
        ]
    |> testSequenced