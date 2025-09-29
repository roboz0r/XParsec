module LexerTests

open System
open System.IO

#if FABLE_COMPILER
open Fable.Pyxpecto
#else
open Expecto
#endif

open XParsec
open XParsec.Parsers
open XParsec.CharParsers
open XParsec.FSharp.Lexer

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
            let t = x.Tokens[i]
            let p = x.Positions[i]
            match t with
            | Operator
            | InvalidOperator
            | Identifier
            | OtherUnlexed
            | EscapePercent
            | EscapeLBrace
            | EscapeRBrace
            | FormatPlaceholder
            | InvalidFormatPlaceholder
            | InvalidFormatPercents
            | InterpolatedExpressionOpen
            | InterpolatedExpressionClose
            | Interpolated3StringOpen
            | Interpolated3StringClose
            | InterpolatedStringOpen
            | InterpolatedStringClose
            | VerbatimInterpolatedStringOpen
            | VerbatimInterpolatedStringClose ->
                let p1 = x.Positions |> Seq.tryItem (i + 1) |> Option.defaultValue input.Length
                let id = input.AsSpan(p, p1 - p).ToString()
                printfn "%d, %A (%s)" p t id
            | StringLiteral
            | UnterminatedStringLiteral
            | VerbatimStringLiteral
            | UnterminatedVerbatimStringLiteral
            | String3Literal
            | UnterminatedString3Literal
            | UnterminatedInterpolatedString 
            | InterpolatedStringFragment
            | Interpolated3StringFragment
            | VerbatimInterpolatedStringFragment
                ->
                let p1 = x.Positions[i + 1]
                let id = input.AsSpan(p, p1 - p).ToString()
                let id = escapeSpecialCharacters id
                let id = truncateMiddle 40 id
                printfn "%d, %A (%s)" p t id
            
            | _ ->
                printfn "%d, %A" p t
            f (i + 1)

    f 0

let writeLexed (path: string) (x: Lexed) =
    use writer = new StreamWriter(path)
    let rec f i =
        if i < x.Length then
            writer.WriteLine($"%d{x.Positions[i]}, %A{x.Tokens[i]}"  )
            f (i + 1)

    f 0

let pToken =
    FSharp.Reflection.FSharpType.GetUnionCases(typeof<Token>)
    |> Array.map (fun case -> pstring case.Name >>% (case.DeclaringType.GetConstructors(Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Instance).[0].Invoke([|case.Tag|]) :?> Token))
    |> fun ps -> choiceL ps "token"


let readLexed (path: string) =
    let parser = many (pipe4 pint32 (pstring ", ") pToken newline (fun pos _ tok _ -> (pos, tok))) .>> eof
    let text = File.ReadAllText path
    match parser (Reader.ofString text ()) with
    | Ok { Parsed = result } ->
        result |> List.ofSeq
    | Error err ->
        failwithf "Error reading lexed file: %A" err

let testLexed (input: string) (expected: (int * Token) list) =
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

#if !FABLE_COMPILER
[<Tests>]
#endif
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
                        0, KWModule
                        6, Whitespace
                        7, Identifier
                        11, Newline
                        12, KWLet
                        15, Whitespace
                        16, Identifier
                        20, Whitespace
                        21, Identifier
                        22, Whitespace
                        23, Operator
                        24, Whitespace
                        25, Identifier
                        26, Newline
                        27, Identifier
                        31, Whitespace
                        32, InterpolatedStringOpen
                        34, InterpolatedStringFragment
                        54, InterpolatedStringClose
                        55, EOF
                    ]

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