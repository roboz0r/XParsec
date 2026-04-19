module XParsec.FSharp.Tests.ParserTests

open System
open System.IO

open Expecto

let specialFiles =
    [
        // These files require special handling because they contain `#if` directives
        // that cause the parser to produce different outputs based on which symbols are defined.
        "24_if_directive.fs", [ "A" ]
    ]

/// Parser tests are discovered automatically from `.fs` source files.
/// To add a new test: create the `.fs` source file, run once locally (it will create the golden
/// file and fail), verify the output is correct, then commit both files.
/// To regenerate all golden files after a format change: set UPDATE_SNAPSHOTS=1 and re-run.
[<Tests>]
let tests =
    testList
        "ParserTests"
        [
            for path in testData.Value do
                let fileName = Path.GetFileName path
                let name = $"Parsing {fileName}"
                test name { testParseFile path }

            for (fileName, symbols) in specialFiles do
                let path = Path.Combine(testDataDir.Value, fileName)
                let name = $"""Parsing {fileName} ({String.concat ", " symbols})"""
                test name { testParseFileWithSymbols symbols path }

            ptest "Step Debugging Test" {
                // This test is intended for manual use with a debugger to verify that stepping
                // through the parser works correctly.
                // Change the file path to point to a specific test file you want to debug, change to `ftest`, then set a breakpoint in the parser.
                let path = Path.Combine(testDataDir.Value, "manual", "file.fs")
                testParseFile path
            }

            ptest "Trace Debugging Test" {
                // This test will print detailed trace events from the parser into file.fs.trace and file.fs.stack files
                // which can be useful for debugging complex parsing issues.
                let path = Path.Combine(testDataDir.Value, "manual", "file.fs")
                // 1MB is default stack size for .NET, which should be sufficient for most parsing tasks.
                // Increase if necessary for debugging stack overflows.
                // 10 second timeout to prevent hanging indefinitely if there is an infinite loop or other issue.
                let stackSize = 0x100000
                let result = parseWithStackProbe stackSize (System.TimeSpan.FromSeconds 10.0) path

                match result with
                | Error e ->
                    failtestf "Parsing failed:\n%s" (XParsec.FSharp.Parser.ErrorFormatting.splitAndFormatTokenErrors e)
                | Ok _ -> ()
            }
        ]

[<Tests>]
let integrityTests =
    testList
        "GoldenFileIntegrity"
        [
            test "No orphaned golden files in data/" {
                let orphans = findOrphanedGoldenFiles testDataDir.Value

                if orphans.Length > 0 then
                    let fileList = orphans |> String.concat "\n  "

                    failtestf
                        "Found %d orphaned golden file(s) with no corresponding .fs source:\n  %s"
                        orphans.Length
                        fileList
            }
        ]

[<Tests>]
let tracingTests =
    testList
        "TracingTests"
        [
            test "Tracing emits ContextPush events" {
                let input = "let x = 1"

                match XParsec.FSharp.Lexer.Lexing.lexString input with
                | Error e -> failtestf "Lexing failed: %A" e
                | Ok lexed ->
                    let events = ResizeArray<XParsec.FSharp.Parser.TraceEvent>()
                    let traceCallback = XParsec.FSharp.Parser.TraceCallback(events.Add)

                    let reader =
                        XParsec.FSharp.Parser.Reader.ofLexedWithTracing lexed input Set.empty traceCallback

                    match XParsec.FSharp.Parser.FSharpAst.parse reader with
                    | Error e ->
                        failtestf
                            "Parsing failed:\n%s"
                            (XParsec.FSharp.Parser.ErrorFormatting.splitAndFormatTokenErrors e)
                    | Ok _ ->
                        let pushEvents =
                            events
                            |> Seq.filter (fun e ->
                                match e with
                                | XParsec.FSharp.Parser.TraceEvent.ContextPush _ -> true
                                | _ -> false
                            )
                            |> Seq.length

                        Expect.isGreaterThan pushEvents 0 "Expected at least one ContextPush event"

                        let consumeEvents =
                            events
                            |> Seq.filter (fun e ->
                                match e with
                                | XParsec.FSharp.Parser.TraceEvent.TokenConsumed _ -> true
                                | _ -> false
                            )
                            |> Seq.length

                        Expect.isGreaterThan consumeEvents 0 "Expected at least one TokenConsumed event"
            }

            test "Tracing is off by default" {
                let input = "let x = 1"

                match XParsec.FSharp.Lexer.Lexing.lexString input with
                | Error e -> failtestf "Lexing failed: %A" e
                | Ok lexed ->
                    let reader = XParsec.FSharp.Parser.Reader.ofLexed lexed input Set.empty

                    // Should parse successfully with default (no-op) tracing
                    match XParsec.FSharp.Parser.FSharpAst.parse reader with
                    | Error e ->
                        failtestf
                            "Parsing failed:\n%s"
                            (XParsec.FSharp.Parser.ErrorFormatting.splitAndFormatTokenErrors e)
                    | Ok _ -> ()
            }
        ]

/// Tests that parsing every prefix of every golden file at token boundaries always returns Ok.
/// This validates the error recovery: no truncation should cause a parse Error or exception.
let testSlicedParsing (filePath: string) =
    let input = File.ReadAllText filePath
    let input = input.Replace("\r\n", "\n")

    // Get token boundaries from the full lex
    match XParsec.FSharp.Lexer.Lexing.lexString input with
    | Error _ -> () // If lexing the full file fails, skip (lexer errors are out of scope)
    | Ok lexed ->
        // Collect unique character positions at token boundaries (StartIndex of each token)
        let boundaries =
            [|
                yield 0 // empty input
                for i in 0 .. lexed.Tokens.Length - 1 do
                    let tok = lexed.Tokens.[i * 1<XParsec.FSharp.Lexer.token>]
                    let startIdx = tok.StartIndex

                    if startIdx > 0 && startIdx <= input.Length then
                        yield startIdx
            |]
            |> Array.distinct
            |> Array.sort

        let mutable failures = ResizeArray<string>()

        for boundary in boundaries do
            let slice = input.[.. boundary - 1] // Take first `boundary` characters

            match XParsec.FSharp.Lexer.Lexing.lexString slice with
            | Error _ -> () // Skip lexer failures (incomplete strings, etc.)
            | Ok slicedLexed ->
                let reader = XParsec.FSharp.Parser.Reader.ofLexed slicedLexed slice Set.empty

                try
                    match XParsec.FSharp.Parser.FSharpAst.parse reader with
                    | Error e ->
                        failures.Add(
                            $"  length={boundary}: Error - {XParsec.FSharp.Parser.ErrorFormatting.splitAndFormatTokenErrors e}"
                        )
                    | Ok _ -> ()
                with ex ->
                    failures.Add($"  length={boundary}: Exception - {ex.GetType().Name}: {ex.Message}")

        if failures.Count > 0 then
            let details = String.Join("\n", failures)

            failtestf
                "Sliced parsing failed for %d of %d boundaries in %s:\n%s"
                failures.Count
                boundaries.Length
                (Path.GetFileName filePath)
                details

[<Tests>]
let recoveryTests =
    testList
        "RecoveryTests"
        [
            for path in testData.Value do
                let fileName = Path.GetFileName path
                let name = $"Sliced recovery {fileName}"
                test name { testSlicedParsing path }
        ]
