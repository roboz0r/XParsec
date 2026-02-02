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

    let isInComment = if pt.Token.InComment then " (in comment)" else ""

    match pt.TokenWithoutCommentFlags with
    | t when t.IsOperator ->
        let id = input.AsSpan(p, p1 - p).ToString()

        sprintf "%d, %s (%s) %s" p (Operator.generateOperatorName t id) id isInComment
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
        sprintf "%d, %A (%s) %s" p t id isInComment
    | t when t.IsNumeric ->
        let id = input.AsSpan(p, p1 - p).ToString()
        sprintf "%d, %A (%s) %s" p t id isInComment
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
        sprintf "%d, %A (%s) %s" p t id isInComment

    | t -> sprintf "%d, %A %s" p t isInComment

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

let writeLexed (path: string) (x: Lexed) =
    use writer = new StreamWriter(path)

    let rec f i =
        if i < x.Tokens.Length then
            let t = x.Tokens[i * 1<token>]
            writer.WriteLine(t.ToString())
            f (i + 1)

    f 0

let writeLexedBlocks (path: string) (x: Lexed) =
    use writer = new StreamWriter(path)

    let rec f i =
        if i < x.Blocks.Length then
            let ti = x.Blocks[i * 1<block>].TokenIndex
            let token = x.Tokens[ti]
            writer.WriteLine(token.ToString())
            f (i + 1)

    f 0

let pOperatorToken =
    let pPrecedence =
        typeof<PrecedenceLevel>.GetEnumValues()
        |> Seq.cast<PrecedenceLevel>
        |> Seq.map (fun level -> level.ToString(), level)
        |> anyStringReturn

    parser {
        let! _ = pstring "Operator "
        let! tokNoFlags = puint16
        let! precedence = pchar ' ' >>. pPrecedence
        let! canBePrefix = (pstring " (can be prefix)" >>% true) <|>% false
        let! inComment = (pstring " (in comment)" >>% TokenRepresentation.InComment) <|>% 0us
        let! isVirtual = (pstring " (virtual)" >>% TokenRepresentation.IsVirtual) <|>% 0us

        let tok = tokNoFlags ||| inComment ||| isVirtual
        return LanguagePrimitives.EnumOfValue<_, Token> tok
    }

let pToken =
    // Get enum cases
    typeof<Token>.GetEnumValues()
    |> Seq.cast<Token>
    |> Seq.toArray
    |> Array.map (fun case ->
        // printfn "Creating parser for %O" case
        parser {
            let! tokNoFlags = pstring (case.ToString()) >>% (uint16 case)
            let! inComment = (pstring " (in comment)" >>% TokenRepresentation.InComment) <|>% 0us
            let! isVirtual = (pstring " (virtual)" >>% TokenRepresentation.IsVirtual) <|>% 0us

            let tok = tokNoFlags ||| inComment ||| isVirtual
            return LanguagePrimitives.EnumOfValue<_, Token> tok
        }
    )
    |> fun ps ->
        choiceL
            (seq {
                yield! ps
                yield pOperatorToken // Fallback for operators which encode associativity and precedence in the enum value
                yield puint16 |>> LanguagePrimitives.EnumOfValue // Fallback for any missing cases e.g. flagged with block comments
            })
            "token"


let readLexed (path: string) =
    let pLine =
        parser {
            let! pos = pint64
            let! _ = pstring ", "
            let! tok = pToken
            let! _ = newline
            return PositionedToken.Create(tok, pos)
        }

    let parser = many pLine .>> eof

    let text = File.ReadAllText path

    match parser (Reader.ofString text ()) with
    | Ok result -> result |> List.ofSeq
    | Error err ->
        ErrorFormatting.formatStringError text err |> printfn "%s"
        failwithf "Failed to parse lexed file at %s" path

let testLexed (input: string) (expected: _ list) =
    // for i in 0 .. 100000 do
    // Run multiple times to catch any state issues
    let input = input.Replace("\r\n", "\n")

    match lexString input with
    | Ok lexed ->
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
    let input = input.Replace("\r\n", "\n")

    match lexString input with
    | Ok lexed ->
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
    let input = input.Replace("\r\n", "\n")
    let expectedPath = filePath + ".lexed"

    if not (File.Exists expectedPath) then
        // Doesn't exist so create it
        match lexString input with
        | Ok lexed ->
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
    let input = input.Replace("\r\n", "\n")
    let expectedPath = filePath + ".lexedblocks"

    if not (File.Exists expectedPath) then
        // Doesn't exist so create it
        match lexString input with
        | Ok lexed ->
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

let testParseFile (filePath: string) =
    let input = File.ReadAllText filePath
    let input = input.Replace("\r\n", "\n")
    let expectedPath = filePath + ".parsed"

    let actual =
        match Lexing.lexString input with
        | Error e -> failwithf "Lexing failed: %A" e
        | Ok lexed ->
            let reader = XParsec.FSharp.Parser.Reader.ofLexed lexed input

            match XParsec.FSharp.Parser.Expr.parse reader with
            | Error e -> failwithf "Parsing failed: %A" e
            | Ok expr ->
                use sw = new StringWriter()
                use tw = new System.CodeDom.Compiler.IndentedTextWriter(sw, "  ")
                XParsec.FSharp.Debug.printExpr tw input lexed expr
                sw.ToString()

    if not (File.Exists expectedPath) then
        File.WriteAllText(expectedPath, actual)
        failtestf "---\n%s\n---\nCreated expected parsed file at %s, please verify it is correct" actual expectedPath
    else
        let expected = File.ReadAllText expectedPath
        Expect.equal actual expected "Parsed output does not match expected output."
