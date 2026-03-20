module XParsec.FSharp.Lexer.Tests.ParserTests

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

            ptest "Debug Test" {
                let path = Path.Combine(testDataDir.Value, "file.fs")
                let result = parseWithStackProbe 1_000_000 (System.TimeSpan.FromSeconds 5.0) path
                writeStackProbe (path + ".stack") result

                match result.ParseResult with
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

            test "No orphaned golden files in data/blocks/" {
                let orphans = findOrphanedGoldenFiles blocksTestDataDir.Value

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

            test "TraceEvent.format produces non-empty strings" {
                let events =
                    [
                        XParsec.FSharp.Parser.TraceEvent.ContextPush(
                            XParsec.FSharp.Parser.OffsideContext.Let,
                            4,
                            XParsec.FSharp.Lexer.PositionedToken.Create(XParsec.FSharp.Lexer.Token.KWLet, 0),
                            1
                        )
                        XParsec.FSharp.Parser.TraceEvent.ContextPop(XParsec.FSharp.Parser.OffsideContext.Let, 0)
                        XParsec.FSharp.Parser.TraceEvent.VirtualToken(XParsec.FSharp.Lexer.Token.VirtualSep, 10)
                    ]

                for event in events do
                    let formatted = XParsec.FSharp.Parser.TraceEvent.format event
                    Expect.isNotEmpty formatted "TraceEvent.format should produce non-empty output"
            }
        ]
