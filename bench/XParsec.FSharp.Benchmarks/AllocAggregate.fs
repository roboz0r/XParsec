/// Aggregates GC allocation events from a .nettrace by resolved call stack.
///
/// Reads GCAllocationTick_V4 events (fire every ~100KB per heap; each event carries
/// AllocationAmount64 + a call stack when the session captured stacks — gc-verbose does).
/// Walks each stack and attributes bytes both as self (leaf frame) and total (every
/// frame on the stack), printing the top frames by either.
///
/// Filter defaults to "XParsec"; pass "*" for everything (e.g. to see which BCL / JIT
/// frames dominate allocations outside our code).
module XParsec.FSharp.Benchmarks.AllocAggregate

open System
open System.Collections.Generic
open Microsoft.Diagnostics.Tracing.Etlx
open Microsoft.Diagnostics.Tracing.Parsers.Clr

let private add (dict: Dictionary<string, int64>) key bytes =
    match dict.TryGetValue key with
    | true, v -> dict.[key] <- v + bytes
    | _ -> dict.[key] <- bytes

let private isAllocationEvent (name: string) =
    name.Contains "AllocationTick" || name.Contains "SampledObjectAllocation"

/// Opens the .nettrace, walks GC allocation events, and prints top-N-by-self and
/// top-N-by-total attribution tables to stdout. `filter` scopes the printed tables
/// to frames whose name contains the substring (pass "*" to disable filtering).
let aggregate (tracePath: string) (filter: string) =
    printfn "Opening %s ..." tracePath
    let etlxPath = TraceLog.CreateFromEventPipeDataFile(tracePath)
    use traceLog = new TraceLog(etlxPath)
    printfn "Loaded. %d events, %d processes." traceLog.EventCount traceLog.Processes.Count

    let selfBytes = Dictionary<string, int64>()
    let totalBytes = Dictionary<string, int64>()
    let mutable totalAlloc = 0L
    let mutable eventCount = 0
    let mutable eventsWithStack = 0

    for ev in traceLog.Events do
        let name = ev.EventName

        if isAllocationEvent name then
            eventCount <- eventCount + 1

            let bytes =
                match ev with
                | :? GCAllocationTickTraceData as a -> int64 a.AllocationAmount64
                | _ -> 0L

            if bytes > 0L then
                totalAlloc <- totalAlloc + bytes
                let cs = ev.CallStack()

                if cs <> null then
                    eventsWithStack <- eventsWithStack + 1
                    let seen = HashSet<string>()
                    let mutable frame = cs
                    let mutable isLeaf = true

                    while frame <> null do
                        let methodName = frame.CodeAddress.FullMethodName
                        let modName = frame.CodeAddress.ModuleName

                        let display =
                            if String.IsNullOrEmpty methodName then
                                sprintf "%s!?" modName
                            else
                                sprintf "%s!%s" modName methodName

                        if isLeaf then
                            add selfBytes display bytes
                            isLeaf <- false

                        if seen.Add display then
                            add totalBytes display bytes

                        frame <- frame.Caller

    printfn ""

    printfn
        "Allocation events: %d   with stack: %d   bytes attributed: %s"
        eventCount
        eventsWithStack
        (totalAlloc.ToString("N0"))

    printfn "Filter: %s" filter
    printfn ""

    let matches (name: string) = filter = "*" || name.Contains filter

    let pct v =
        if totalAlloc = 0L then
            "n/a"
        else
            sprintf "%5.2f%%" (float v / float totalAlloc * 100.0)

    let fmtBytes (b: int64) = sprintf "%12s" (b.ToString("N0"))

    let short (s: string) =
        if s.Length > 140 then s.Substring(0, 137) + "..." else s

    let topBy (d: Dictionary<string, int64>) n =
        d
        |> Seq.filter (fun kv -> matches kv.Key)
        |> Seq.sortByDescending (fun kv -> kv.Value)
        |> Seq.truncate n
        |> Seq.toArray

    printfn "## Top 30 frames by SELF allocation bytes (leaf = where the alloc happens)"

    for kv in topBy selfBytes 30 do
        printfn "%s  %s  %s" (pct kv.Value) (fmtBytes kv.Value) (short kv.Key)

    printfn ""
    printfn "## Top 30 frames by TOTAL (inclusive) allocation bytes"

    for kv in topBy totalBytes 30 do
        printfn "%s  %s  %s" (pct kv.Value) (fmtBytes kv.Value) (short kv.Key)
