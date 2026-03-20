module XParsec.FSharp.Lexer.Tests.ParserTests

open System.IO
open System.Collections.Generic

open Expecto
open FSharp.NativeInterop

#nowarn "9" // NativePtr
#nowarn "51" // Native pointer address-of

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

            test "Stack Probe" {
                let fileName = "file.fs" // Change this to the specific file you want to debug
                let path = Path.Combine(testDataDir.Value, fileName)
                let input = File.ReadAllText path
                let input = input.Replace("\r\n", "\n")

                match XParsec.FSharp.Lexer.Lexing.lexString input with
                | Error e -> failtestf "Lexing failed: %A" e
                | Ok lexed ->
                    // Always write .lexed file
                    writeLexed (path + ".lexed") lexed

                    let events = ResizeArray()
                    let eventsPath = path + ".trace"
                    use eventsWriter = new StreamWriter(eventsPath, false)
                    let mutable maxDepth = 0
                    let mutable deepestTrace: System.Diagnostics.StackTrace = null
                    let stackProbes = ResizeArray<struct (string * nativeint)>()

                    let traceCallback =
                        XParsec.FSharp.Parser.TraceCallback(fun x ->
                            eventsWriter.WriteLine(XParsec.FSharp.Parser.TraceEvent.format x)

                            match x with
                            | XParsec.FSharp.Parser.TraceEvent.ContextPush(ctx, indent, _, depth) ->
                                let mutable marker = 0
                                let sp = NativePtr.toNativeInt &&marker
                                stackProbes.Add(struct ($"PUSH {ctx} indent={indent} depth={depth}", sp))

                                if depth > maxDepth then
                                    maxDepth <- depth
                                    deepestTrace <- System.Diagnostics.StackTrace(true)
                            | XParsec.FSharp.Parser.TraceEvent.ContextPop(ctx, depth) ->
                                let mutable marker = 0
                                let sp = NativePtr.toNativeInt &&marker
                                stackProbes.Add(struct ($"POP {ctx} depth={depth}", sp))
                            | _ -> ()

                            lock events (fun () -> events.Add x)
                        )

                    let reader =
                        XParsec.FSharp.Parser.Reader.ofLexedWithTracing lexed input Set.empty traceCallback

                    // let task =
                    //     System.Threading.Tasks.Task.Run(fun () -> XParsec.FSharp.Parser.FSharpAst.parse reader)
                    let mutable taskResult = Unchecked.defaultof<_>

                    let thread =
                        System.Threading.Thread(
                            System.Threading.ThreadStart(fun () ->
                                match XParsec.FSharp.Parser.FSharpAst.parse reader with
                                | Error e -> taskResult <- Error e
                                | Ok ast -> taskResult <- Ok ast
                            ),
                            1_000_000
                        )

                    thread.Start()
                    // Wait up to 5 seconds for the parser to complete. If it doesn't, dump debugging info to help identify where it's stuck.
                    // if not (task.Wait(5000)) then
                    if not (thread.Join(5000)) then
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

                        let lastEvents =
                            lock events (fun () -> events.ToArray())
                            |> Array.rev
                            |> Array.truncate 80
                            |> Array.rev
                            |> Array.map XParsec.FSharp.Parser.TraceEvent.format
                            |> String.concat "\n  "

                        failtestf
                            "Parsing stuck at reader index %d, token %A at line %d: '%s'\n\nLast trace events:\n  %s"
                            stuckIdx
                            tok
                            line
                            (context.Replace("\n", "\\n"))
                            lastEvents
                    else
                        // match task.Result with
                        match taskResult with
                        | Error e ->
                            let lastEvents =
                                events
                                |> Seq.toArray
                                |> Array.rev
                                |> Array.truncate 80
                                |> Array.rev
                                |> Array.map XParsec.FSharp.Parser.TraceEvent.format
                                |> String.concat "\n  "

                            failtestf
                                "Parsing failed:\n%s\n\nLast trace events:\n  %s"
                                (XParsec.FSharp.Parser.ErrorFormatting.splitAndFormatTokenErrors e)
                                lastEvents
                        | Ok ast ->
                            // Always write .parsed file
                            let ctx = XParsec.FSharp.Debug.PrintContext(2)
                            XParsec.FSharp.Debug.printFSharpAst ctx input lexed ast
                            XParsec.FSharp.Debug.printDiagnostics ctx reader.State.Diagnostics
                            let output = ctx.FlushToString()
                            File.WriteAllText(path + ".parsed", output)

                    // Dump deepest stack trace
                    let stackPath = path + ".stack"
                    use sw = new StreamWriter(stackPath, false)
                    sw.WriteLine($"Deepest context push depth: {maxDepth}")

                    if deepestTrace <> null then
                        sw.WriteLine($"Frame count: {deepestTrace.FrameCount}")
                        sw.WriteLine()

                        for i in 0 .. deepestTrace.FrameCount - 1 do
                            let frame = deepestTrace.GetFrame(i)
                            let m = frame.GetMethod()

                            let typeName =
                                if m <> null && m.DeclaringType <> null then
                                    m.DeclaringType.Name
                                else
                                    "?"

                            let methodName = if m <> null then m.Name else "?"
                            let ilOffset = frame.GetILOffset()
                            sw.WriteLine($"  [{i, 3}] {typeName}.{methodName} (IL offset={ilOffset})")

                    sw.WriteLine()
                    sw.WriteLine("=== Stack Pointer Probes (PUSH/POP at context boundaries) ===")
                    sw.WriteLine()
                    let mutable prev = 0n

                    for struct (label, sp) in stackProbes do
                        let delta = if prev = 0n then 0n else prev - sp // stack grows downward on x86/x64
                        sw.WriteLine($"SP=0x{sp:X12}  delta={delta, 8}  {label}")
                        prev <- sp
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
                    let events = List<XParsec.FSharp.Parser.TraceEvent>()
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
