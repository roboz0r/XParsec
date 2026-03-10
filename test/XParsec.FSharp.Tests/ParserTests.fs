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
                let fileName = "file.fs" // Change this to the specific file you want to debug
                let path = Path.Combine(testDataDir.Value, fileName)
                let input = File.ReadAllText path
                let input = input.Replace("\r\n", "\n")

                match XParsec.FSharp.Lexer.Lexing.lexString input with
                | Error e -> failtestf "Lexing failed: %A" e
                | Ok lexed ->
                    let reader = XParsec.FSharp.Parser.Reader.ofLexed lexed input Set.empty

                    let task =
                        System.Threading.Tasks.Task.Run(fun () -> XParsec.FSharp.Parser.FSharpAst.parse reader)
                    // Wait up to 5 seconds for the parser to complete. If it doesn't, dump debugging info to help identify where it's stuck.
                    if not (task.Wait(5000)) then
                        let stuckIdx = reader.Index
                        let tok = lexed.Tokens.[stuckIdx * 1<_>]
                        let pos = tok.StartIndex
                        let lines = input.Substring(0, min pos input.Length).Split('\n')
                        let line = lines.Length

                        let context =
                            if pos + 40 < input.Length then
                                input.Substring(pos, 40)
                            else
                                input.Substring(pos)

                        failtestf
                            "Parsing stuck at reader index %d, token %A at line %d: '%s'"
                            stuckIdx
                            tok
                            line
                            (context.Replace("\n", "\\n"))
                    else
                        match task.Result with
                        | Error e ->
                            failtestf
                                "Parsing failed:\n%s"
                                (XParsec.FSharp.Parser.ErrorFormatting.splitAndFormatTokenErrors e)
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
