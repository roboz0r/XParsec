[<AutoOpen>]
module Vesper.Tests.TestHelpers

// The golden-file parser-test machinery the Vesper.* contract tests need —
// the minimal subset of `XParsec.FSharp.Tests/TestHelpers.fs` (`testParseFile` /
// `testParseSignatureFile` + the snapshot flags). Kept here so this project does
// not depend on the parser's own test project. `UPDATE_SNAPSHOTS=1` (the
// `-UpdateSnapshots` wrapper flag) overwrites goldens; `CI` skips a missing
// golden instead of failing, so a new file is green on first push.

open System
open System.IO

open Expecto

open XParsec.FSharp.Lexer

/// When set (env `UPDATE_SNAPSHOTS`), golden files are overwritten rather than compared.
let private updateSnapshots =
    Environment.GetEnvironmentVariable("UPDATE_SNAPSHOTS") |> isNull |> not

/// When set (env `CI`), a missing golden file skips rather than fails the test.
let private isCi = Environment.GetEnvironmentVariable("CI") |> isNull |> not

/// Shared core for golden-file parser tests. Parses the source via `parseFn`,
/// formats the resulting AST + diagnostics via Debug, and asserts against the
/// golden file at `expectedPath`. Creates the golden file if missing.
let private testParseFileWithParser
    (parseFn:
        XParsec.FSharp.Parser.FSReader -> Result<XParsec.FSharp.Parser.FSharpAst<XParsec.FSharp.Parser.SyntaxToken>, _>)
    (definedSymbols: Set<string>)
    (filePath: string)
    (expectedPath: string)
    =
    let input = File.ReadAllText filePath
    let input = input.Replace("\r\n", "\n")

    let actual =
        match Lexing.lexString input with
        | Error e -> failwithf "Lexing failed: %A" e
        | Ok lexed ->
            let reader = XParsec.FSharp.Parser.Reader.ofLexed lexed input definedSymbols

            match parseFn reader with
            | Error e ->
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

/// Parses an implementation `.fs` file and compares against `<filePath>.parsed`.
let testParseFile (filePath: string) =
    testParseFileWithParser XParsec.FSharp.Parser.FSharpAst.parse Set.empty filePath (filePath + ".parsed")

/// Parses a `.fsi` signature file via `FSharpAst.parseSignature` and compares
/// against `<filePath>.parsed`.
let testParseSignatureFile (filePath: string) =
    testParseFileWithParser XParsec.FSharp.Parser.FSharpAst.parseSignature Set.empty filePath (filePath + ".parsed")
