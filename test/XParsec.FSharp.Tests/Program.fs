module XParsec.FSharp.Tests.Program

open System
open System.Diagnostics
open System.IO

open Expecto

/// When invoked with `--parse-file <path>`, parses a single file and exits with a code
/// indicating the result. This is used as a subprocess for process isolation (StackOverflowException
/// crashes the entire process and cannot be caught).
///
/// Exit codes:
///   0 = Success with 0 diagnostics
///   1 = Success with diagnostics (count written to stdout)
///   2 = Lex error
///   3 = Parse error
///   4 = Timeout
///   5 = Unhandled exception (message on stderr)
let private parseFileMain (path: string) =
    try
        let result = tryParseCorpusFile path

        match result with
        | Success 0 ->
            printfn "OK"
            testParseFile path
            0
        | Success n ->
            printfn "DIAG %d" n
            testParseFile path
            1
        | LexError e ->
            printfn "LEXERR"
            eprintfn "%s" e
            2
        | ParseError e ->
            printfn "PARSERR"
            eprintfn "%s" e
            3
        | Timeout ->
            printfn "TIMEOUT"
            4
        | ParseException ex ->
            printfn "EXCEPT %s" (ex.GetType().Name)
            eprintfn "%s" ex.Message
            5
    with ex ->
        printfn "EXCEPT %s" (ex.GetType().Name)
        eprintfn "%s" ex.Message
        5

/// Runs the corpus report by spawning one subprocess per file for process isolation.
let private corpusMain subDir =
    let files = corpusTestData subDir
    let corpusDir = Path.Combine(testDataDir.Value, subDir)

    if files.Length = 0 then
        eprintfn $"No .fs files found in {corpusDir}"
        1
    else
        let exePath =
            let asm = Reflection.Assembly.GetExecutingAssembly()
            let dllPath = asm.Location
            let dir = Path.GetDirectoryName dllPath
            Path.Combine(dir, "XParsec.FSharp.Tests.exe")

        printfn "Parsing %d files from corpus..." files.Length
        printfn "Using: %s" exePath

        let maxParallel = Environment.ProcessorCount

        let results =
            files
            |> Array.Parallel.map (fun path ->
                let fileName = Path.GetFileName path

                let psi = ProcessStartInfo(exePath, $"--parse-file \"{path}\"")
                psi.UseShellExecute <- false
                psi.RedirectStandardOutput <- true
                psi.RedirectStandardError <- true
                psi.CreateNoWindow <- true
                psi.Environment["UPDATE_SNAPSHOTS"] <- "1"

                let proc = new Process()
                proc.StartInfo <- psi

                let started =
                    try
                        proc.Start()
                    with _ ->
                        false

                let result =
                    if not started then
                        proc.Dispose()
                        CorpusReport.Exception "ProcessStartFailed"
                    else
                        // Read stdout/stderr async to avoid pipe buffer deadlock
                        let stdoutTask = proc.StandardOutput.ReadToEndAsync()
                        let stderrTask = proc.StandardError.ReadToEndAsync()

                        let timedOut =
                            if not (proc.WaitForExit(60_000)) then
                                try
                                    proc.Kill(true)
                                with _ ->
                                    ()

                                true
                            else
                                false

                        let stdout = stdoutTask.Result
                        stderrTask.Result |> ignore

                        let exitCode = if timedOut then -1 else proc.ExitCode
                        proc.Dispose()

                        if timedOut then
                            CorpusReport.Timeout
                        else
                            let line = stdout.Trim()

                            match exitCode with
                            | 0 -> CorpusReport.Clean
                            | 1 ->
                                match line.Split(' ') with
                                | [| "DIAG"; n |] ->
                                    match Int32.TryParse n with
                                    | true, count -> CorpusReport.WithDiagnostics count
                                    | _ -> CorpusReport.WithDiagnostics 0
                                | _ -> CorpusReport.WithDiagnostics 0
                            | 2 -> CorpusReport.LexError
                            | 3 -> CorpusReport.ParseError
                            | 4 -> CorpusReport.Timeout
                            | 5 ->
                                match line.Split(' ', 2) with
                                | [| "EXCEPT"; name |] -> CorpusReport.Exception name
                                | _ -> CorpusReport.Exception "Unknown"
                            | _ -> CorpusReport.Crashed

                fileName, result
            )

        CorpusReport.printReport corpusDir results files.Length |> ignore
        0

[<EntryPoint>]
let main argv =
    match argv with
    | [| "--parse-file"; path |] ->
        let fullPath =
            if Path.IsPathRooted path then
                path
            else
                Path.GetFullPath path

        parseFileMain fullPath
    | [| "--corpus"; subDir |] -> corpusMain subDir
    | _ -> Tests.runTestsInAssemblyWithCLIArgs [] argv
