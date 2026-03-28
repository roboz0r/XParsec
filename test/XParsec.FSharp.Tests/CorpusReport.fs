module XParsec.FSharp.Tests.CorpusReport

open System.IO
open System.Text

type CorpusResult =
    | Clean
    | WithDiagnostics of count: int
    | LexError
    | ParseError
    | Exception of name: string
    | Timeout
    | Crashed

/// Prints a categorized report from corpus parse results.
/// Returns the report string.
let printReport corpusDir (results: (string * CorpusResult) array) (totalFiles: int) =
    let categorize predicate =
        results |> Array.filter (snd >> predicate)

    let clean =
        categorize (
            function
            | Clean -> true
            | _ -> false
        )

    let withDiagnostics =
        categorize (
            function
            | WithDiagnostics _ -> true
            | _ -> false
        )

    let lexErrors =
        categorize (
            function
            | LexError -> true
            | _ -> false
        )

    let parseErrors =
        categorize (
            function
            | ParseError -> true
            | _ -> false
        )

    let exceptions =
        categorize (
            function
            | Exception _ -> true
            | _ -> false
        )

    let timeouts =
        categorize (
            function
            | Timeout -> true
            | _ -> false
        )

    let crashed =
        categorize (
            function
            | Crashed -> true
            | _ -> false
        )

    let sb = StringBuilder()
    let w (s: string) = sb.AppendLine(s) |> ignore

    w "========================================"
    w " Corpus Parse Report"
    w "========================================"
    w ""
    w $"Total files:       {totalFiles}"
    w $"Clean (0 diag):    {clean.Length}"
    w $"With diagnostics:  {withDiagnostics.Length}"
    w $"Lex errors:        {lexErrors.Length}"
    w $"Parse errors:      {parseErrors.Length}"
    w $"Exceptions:        {exceptions.Length}"
    w $"Timeouts:          {timeouts.Length}"
    w $"Crashed:           {crashed.Length}"
    w ""

    if lexErrors.Length > 0 then
        w "--- Lex Errors ---"

        for (name, _) in lexErrors do
            w $"  LEXERR  {name}"

        w ""

    if parseErrors.Length > 0 then
        w "--- Parse Errors ---"

        for (name, _) in parseErrors do
            w $"  PARSERR {name}"

        w ""

    if exceptions.Length > 0 then
        w "--- Exceptions ---"

        for (name, r) in exceptions do
            match r with
            | Exception exName -> w $"  EXCEPT  {name}: {exName}"
            | _ -> ()

        w ""

    if timeouts.Length > 0 then
        w "--- Timeouts ---"

        for (name, _) in timeouts do
            w $"  TIMEOUT {name}"

        w ""

    if crashed.Length > 0 then
        w "--- Crashed (likely StackOverflow) ---"

        for (name, _) in crashed do
            w $"  CRASH   {name}"

        w ""

    if withDiagnostics.Length > 0 then
        w "--- With Diagnostics ---"

        for (name, r) in withDiagnostics do
            match r with
            | WithDiagnostics n -> w $"  DIAG({n, 3}) {name}"
            | _ -> ()

        w ""

    if clean.Length > 0 then
        w "--- Clean ---"

        for (name, _) in clean do
            w $"  OK      {name}"

        w ""

    let report = sb.ToString()
    printfn "%s" report

    // Write report to file
    let reportPath = Path.Combine(corpusDir, "REPORT.txt")

    File.WriteAllText(reportPath, report)
    printfn "Report written to: %s" reportPath
    report
