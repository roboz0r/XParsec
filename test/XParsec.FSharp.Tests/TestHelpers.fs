[<AutoOpen>]
module XParsec.FSharp.Tests.TestHelpers

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
    | Token.UnterminatedStringLiteral
    | Token.UnterminatedVerbatimStringLiteral
    | Token.UnterminatedString3Literal
    | Token.UnterminatedInterpolatedString
    | Token.InterpolatedStringFragment
    | Token.Interpolated3StringFragment
    | Token.VerbatimInterpolatedStringFragment
    | Token.StringOpen
    | Token.StringClose
    | Token.ByteArrayClose
    | Token.VerbatimStringOpen
    | Token.VerbatimStringClose
    | Token.VerbatimByteArrayClose
    | Token.String3Open
    | Token.String3Close
    | Token.StringFragment
    | Token.EscapeSequence as t ->
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

let writeLexed (path: string) (x: Lexed) =
    use writer = new StreamWriter(path)

    let rec f i =
        if i < x.Tokens.Length then
            let t = x.Tokens[i * 1<token>]
            writer.WriteLine(t.ToString())
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
    |> Array.sortByDescending (fun t -> t.ToString().Length) // Sort by length to ensure longer matches are tried first (e.g. OpBarBar before OpBar)
    |> Array.map (fun case ->
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
            let! pos = pint32
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

/// When true, golden files are overwritten with fresh output rather than compared.
/// Activate by setting the UPDATE_SNAPSHOTS environment variable to any non-empty value,
/// e.g. `UPDATE_SNAPSHOTS=1 dotnet test`. Useful after deliberate debug-output format changes.
let private updateSnapshots =
    Environment.GetEnvironmentVariable("UPDATE_SNAPSHOTS") |> isNull |> not

/// When true, a missing golden file causes the test to be skipped rather than failed.
/// Automatically activated when the CI environment variable is set (standard on GitHub Actions
/// and most CI platforms).  Keeps CI green on the first push of a new test file — a developer
/// must run the suite locally to create the golden file and commit it.
let private isCi = Environment.GetEnvironmentVariable("CI") |> isNull |> not

let testDataDir =
    lazy DirectoryInfo(IO.Path.Combine(__SOURCE_DIRECTORY__, "data")).FullName

let testData =
    lazy
        let dir = testDataDir.Value
        IO.Directory.GetFiles(dir, "*.fs")

let lexOnlyTestData =
    lazy
        let dir = Path.Combine(testDataDir.Value, "lex-only")
        IO.Directory.GetFiles(dir, "*.fs")

let corpusTestData subDir =
    let dir = Path.Combine(testDataDir.Value, subDir)
    IO.Directory.GetFiles(dir, "*.fs") |> Array.sort

let testLexFile (filePath: string) =
    let input = File.ReadAllText filePath
    let input = input.Replace("\r\n", "\n")
    let expectedPath = filePath + ".lexed"

    match lexString input with
    | Error err ->
        let pos = err.Position
        printLexed input (LexBuilder.complete pos.Index pos.State)
        ErrorFormatting.formatStringError input err |> printfn "%s"
        failwith "Lexing failed"
    | Ok lexed ->
        if updateSnapshots || not (File.Exists expectedPath) then
            writeLexed expectedPath lexed

            if not updateSnapshots then
                if isCi then
                    skiptest
                        $"Golden file created at {Path.GetFileName expectedPath}; commit it to enable this test in CI"
                else
                    failtestf "Created expected lexed file at %s, please verify it is correct" expectedPath
        else
            let expected = readLexed expectedPath
            testLexed input expected


/// Parses a source file using the given set of defined preprocessor symbols and compares
/// the result against a golden `.parsed` file.
///
/// The golden file path is derived from the source path:
///   - no symbols  → `<filePath>.parsed`
///   - with symbols → `<filePath>.<sym1>_<sym2>….parsed`  (symbols sorted for determinism)
///
/// If the golden file does not yet exist it is created and the test fails with a prompt
/// to verify the generated output.
let testParseFileWith (definedSymbols: Set<string>) (filePath: string) =
    let input = File.ReadAllText filePath
    let input = input.Replace("\r\n", "\n")

    let symbolSuffix =
        if definedSymbols.IsEmpty then
            ""
        else
            "." + (definedSymbols |> String.concat "_")

    let expectedPath = filePath + symbolSuffix + ".parsed"

    let actual =
        match Lexing.lexString input with
        | Error e -> failwithf "Lexing failed: %A" e
        | Ok lexed ->
            let reader = XParsec.FSharp.Parser.Reader.ofLexed lexed input definedSymbols

            match XParsec.FSharp.Parser.FSharpAst.parse reader with
            | Error e ->
                // failwithf "Parsing failed: %A" e
                failwithf "Parsing failed:\n%s" (XParsec.FSharp.Parser.ErrorFormatting.splitAndFormatTokenErrors e)
            | Ok ast ->
                let ctx = XParsec.FSharp.Debug.PrintContext(2)
                XParsec.FSharp.Debug.printFSharpAst ctx input lexed ast
                XParsec.FSharp.Debug.printDiagnostics ctx input reader.State.Diagnostics
                XParsec.FSharp.Debug.printWarnDirectives ctx reader.State.WarnDirectives
                ctx.FlushToString()

    if updateSnapshots || not (File.Exists expectedPath) then
        File.WriteAllText(expectedPath, actual)

        if not updateSnapshots then
            if isCi then
                skiptest $"Golden file created at {Path.GetFileName expectedPath}; commit it to enable this test in CI"
            else
                failtestf
                    "---\n%s\n---\nCreated expected parsed file at %s, please verify it is correct"
                    actual
                    expectedPath
    else
        let expected = File.ReadAllText expectedPath
        Expect.equal actual expected "Parsed output does not match expected output."

let testParseFile (filePath: string) = testParseFileWith Set.empty filePath

let testParseFileWithSymbols (symbols: string list) (filePath: string) =
    testParseFileWith (Set.ofList symbols) filePath


open FSharp.NativeInterop

#nowarn "9" // NativePtr
#nowarn "51" // Native pointer address-of

/// Result of a stack-probing parse run. Contains the deepest stack trace,
/// SP deltas at context boundaries, and the parse result.
type StackProbeResult =
    {
        MaxDepth: int
        DeepestTrace: System.Diagnostics.StackTrace option
        StackProbes: struct (string * nativeint) array
    }

/// Writes stack probe results (deepest stack trace + SP deltas) to the given path.
let writeStackProbe (path: string) (result: StackProbeResult) =
    use sw = new StreamWriter(path)
    sw.WriteLine($"Deepest context push depth: {result.MaxDepth}")

    match result.DeepestTrace with
    | Some trace ->
        sw.WriteLine($"Frame count: {trace.FrameCount}")
        sw.WriteLine()

        for i in 0 .. trace.FrameCount - 1 do
            let frame = trace.GetFrame(i)
            let m = frame.GetMethod()

            let typeName =
                if m <> null && m.DeclaringType <> null then
                    m.DeclaringType.Name
                else
                    "?"

            let methodName = if m <> null then m.Name else "?"
            let ilOffset = frame.GetILOffset()
            sw.WriteLine($"  [{i, 3}] {typeName}.{methodName} (IL offset={ilOffset})")
    | None -> ()

    sw.WriteLine()
    sw.WriteLine("=== Stack Pointer Probes (PUSH/POP at context boundaries) ===")
    sw.WriteLine()
    let mutable prev = 0n

    for struct (label, sp) in result.StackProbes do
        let delta = if prev = 0n then 0n else prev - sp
        sw.WriteLine($"SP=0x{sp:X12}  delta={delta, 8}  {label}")
        prev <- sp

/// Parses a source file on a dedicated thread with stack probing enabled.
/// Captures SP at each context push/pop and records the deepest stack trace.
/// `stackSize` is the thread stack size in bytes.
/// `timeout` is the maximum time to wait for the parse to complete.
let parseWithStackProbe (stackSize: int) (timeout: System.TimeSpan) (filePath: string) =
    let input = File.ReadAllText filePath
    let input = input.Replace("\r\n", "\n")

    // Eagerly clean up old debug files
    for ext in [| ".lexed"; ".parsed"; ".stack"; ".trace" |] do
        let p = filePath + ext

        if File.Exists p then
            File.Delete p

    match Lexing.lexString input with
    | Error e -> failwithf "Lexing failed: %A" e
    | Ok lexed ->
        writeLexed (filePath + ".lexed") lexed

        let mutable maxDepth = 0
        let mutable deepestTrace: System.Diagnostics.StackTrace option = None
        let stackProbes = ResizeArray<struct (string * nativeint)>()
        let events = ResizeArray<string>()
        let traceWriter = new StreamWriter(filePath + ".trace", false)
        traceWriter.AutoFlush <- true

        let traceCallback =
            { new XParsec.FSharp.Parser.WriterTraceCallback(lexed, traceWriter) with
                override this.Write(line) =
                    traceWriter.WriteLine(line)
                    events.Add(line)

                override this.ContextPush(ctx, indent, token, depth) =
                    let mutable marker = 0
                    let sp = NativePtr.toNativeInt &&marker
                    stackProbes.Add(struct ($"PUSH {ctx} indent={indent} depth={depth}", sp))

                    if depth > maxDepth then
                        maxDepth <- depth
                        deepestTrace <- Some(System.Diagnostics.StackTrace(true))

                        writeStackProbe
                            (filePath + ".stack")
                            {
                                MaxDepth = maxDepth
                                DeepestTrace = deepestTrace
                                StackProbes = stackProbes.ToArray()
                            }

                    base.ContextPush(ctx, indent, token, depth)

                override this.ContextPop(ctx, depth) =
                    let mutable marker = 0
                    let sp = NativePtr.toNativeInt &&marker
                    stackProbes.Add(struct ($"POP {ctx} depth={depth}", sp))
                    base.ContextPop(ctx, depth)
            }

        let reader =
            XParsec.FSharp.Parser.Reader.ofLexedWithTracing lexed input Set.empty traceCallback

        let mutable taskResult = Unchecked.defaultof<_>

        let thread =
            System.Threading.Thread(
                System.Threading.ThreadStart(fun () ->
                    match XParsec.FSharp.Parser.FSharpAst.parse reader with
                    | Error e -> taskResult <- Error e
                    | Ok ast -> taskResult <- Ok ast
                ),
                stackSize
            )

        thread.Start()

        if not (thread.Join(timeout)) then
            let stuckIdx = reader.Index
            let tok = lexed.Tokens.[stuckIdx * 1<token>]
            let pos = tok.StartIndex
            let lines = input.Substring(0, min (int pos) input.Length).Split('\n')
            let line = lines.Length

            let context =
                if int pos + 40 < input.Length then
                    input.Substring(int pos, 40)
                else
                    input.Substring(int pos)

            let lastEvents =
                lock events (fun () -> events.ToArray())
                |> Array.rev
                |> Array.truncate 80
                |> Array.rev
                |> String.concat "\n  "

            failwithf
                "Parsing stuck at reader index %d, token %A at line %d: '%s'\n\nLast trace events:\n  %s"
                stuckIdx
                tok
                line
                (context.Replace("\n", "\\n"))
                lastEvents

        traceWriter.Dispose()

        match taskResult with
        | Error e -> ()
        | Ok ast ->
            let parseOutput =
                let ctx = XParsec.FSharp.Debug.PrintContext(2)
                XParsec.FSharp.Debug.printFSharpAst ctx input lexed ast
                XParsec.FSharp.Debug.printDiagnostics ctx input reader.State.Diagnostics
                XParsec.FSharp.Debug.printWarnDirectives ctx reader.State.WarnDirectives
                ctx.FlushToString()

            let expectedPath = filePath + ".parsed"
            File.WriteAllText(expectedPath, parseOutput)

        taskResult

/// Returns paths of golden files (`.parsed`, `.lexed`, `.lexedblocks`) in `dataDir` that have
/// no corresponding `.fs` source file — i.e. orphans left behind after a source file was renamed
/// or deleted.
let findOrphanedGoldenFiles (dataDir: string) : string array =
    [| "*.parsed"; "*.lexed"; "*.lexedblocks" |]
    |> Array.collect (fun ext -> Directory.GetFiles(dataDir, ext))
    |> Array.filter (fun goldenPath ->
        let fileName = Path.GetFileName goldenPath
        let dotFsIdx = fileName.IndexOf(".fs.")

        if dotFsIdx < 0 then
            false // Cannot determine source name; not treated as orphaned
        else
            let sourceName = fileName.[.. dotFsIdx + 2]
            not (File.Exists(Path.Combine(dataDir, sourceName)))
    )
    |> Array.sort

type CorpusParseResult =
    | LexError of string
    | ParseError of string
    | ParseException of exn
    | Timeout
    | Success of diagnosticCount: int

/// Attempts to lex and parse a corpus file, returning a structured result.
/// Runs on a separate thread with a timeout to guard against infinite loops / stack overflows.
let tryParseCorpusFile (filePath: string) : CorpusParseResult =
    let input = File.ReadAllText filePath
    let input = input.Replace("\r\n", "\n")

    match Lexing.lexString input with
    | Error e -> LexError(ErrorFormatting.formatStringError input e)
    | Ok lexed ->
        let mutable result = Timeout

        let thread =
            System.Threading.Thread(
                System.Threading.ThreadStart(fun () ->
                    try
                        let reader = XParsec.FSharp.Parser.Reader.ofLexed lexed input Set.empty

                        match XParsec.FSharp.Parser.FSharpAst.parse reader with
                        | Error e ->
                            result <- ParseError(XParsec.FSharp.Parser.ErrorFormatting.splitAndFormatTokenErrors e)
                        | Ok _ ->
                            let diagCount = reader.State.Diagnostics.Length
                            result <- Success diagCount
                    with ex ->
                        result <- ParseException ex
                ),
                0x200000 // 2MB stack
            )

        thread.Start()

        if not (thread.Join(System.TimeSpan.FromSeconds 30.0)) then
            Timeout
        else
            result
