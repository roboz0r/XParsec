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
open Microsoft.Diagnostics.Tracing.Parsers.Kernel

let private add (dict: Dictionary<string, int64>) key bytes =
    match dict.TryGetValue key with
    | true, v -> dict.[key] <- v + bytes
    | _ -> dict.[key] <- bytes

/// Opens a .nettrace (EventPipe) or .etl/.etlx (ETW) trace and returns a TraceLog
/// that the caller must dispose.
let private openTraceLog (tracePath: string) : TraceLog =
    let ext = IO.Path.GetExtension(tracePath).ToLowerInvariant()

    match ext with
    | ".nettrace" ->
        let etlxPath = TraceLog.CreateFromEventPipeDataFile(tracePath)
        new TraceLog(etlxPath)
    | ".etl"
    | ".etlx" -> TraceLog.OpenOrConvert(tracePath)
    | _ -> failwithf "Unknown trace extension %s (expected .nettrace, .etl, .etlx)" ext

let private isAllocationEvent (name: string) =
    name.Contains "AllocationTick" || name.Contains "SampledObjectAllocation"

/// Aggregate by allocated type name for samples whose leaf frame contains
/// `targetSubstring`. Reveals *what* is being allocated at that leaf (often the
/// most useful diagnostic — leaf stack attribution says where, this says what).
let aggregateTypesAt (tracePath: string) (targetSubstring: string) (topN: int) =
    printfn "Opening %s ..." tracePath
    let etlxPath = TraceLog.CreateFromEventPipeDataFile(tracePath)
    use traceLog = new TraceLog(etlxPath)
    let byType = Dictionary<string, int64>()
    let mutable matched = 0L

    for ev in traceLog.Events do
        if isAllocationEvent ev.EventName then
            match ev with
            | :? GCAllocationTickTraceData as a ->
                let cs = ev.CallStack()

                if cs <> null then
                    let leaf = cs.CodeAddress.FullMethodName

                    if not (String.IsNullOrEmpty leaf) && leaf.Contains targetSubstring then
                        let bytes = int64 a.AllocationAmount64
                        matched <- matched + bytes
                        add byType a.TypeName bytes
            | _ -> ()

    printfn ""
    printfn "Matched %s bytes at leaf frames containing '%s'" (matched.ToString("N0")) targetSubstring
    printfn ""

    let pct v =
        if matched = 0L then
            "n/a"
        else
            sprintf "%5.2f%%" (float v / float matched * 100.0)

    let fmt (b: int64) = sprintf "%12s" (b.ToString("N0"))
    printfn "## Top %d allocated types" topN

    for kv in byType |> Seq.sortByDescending (fun kv -> kv.Value) |> Seq.truncate topN do
        printfn "%s  %s  %s" (pct kv.Value) (fmt kv.Value) kv.Key

/// Dump the first `maxStacks` full call stacks for allocation samples whose leaf
/// frame contains `targetSubstring`. Shows frame names from leaf to root one per line.
let dumpStacks (tracePath: string) (targetSubstring: string) (maxStacks: int) =
    printfn "Opening %s ..." tracePath
    let etlxPath = TraceLog.CreateFromEventPipeDataFile(tracePath)
    use traceLog = new TraceLog(etlxPath)
    let mutable printed = 0

    for ev in traceLog.Events do
        if printed < maxStacks && isAllocationEvent ev.EventName then
            let cs = ev.CallStack()

            if cs <> null then
                let leaf = cs.CodeAddress.FullMethodName

                if not (String.IsNullOrEmpty leaf) && leaf.Contains targetSubstring then
                    printed <- printed + 1
                    printfn ""
                    printfn "---- stack %d ----" printed
                    let mutable f = cs
                    let mutable depth = 0

                    while f <> null && depth < 20 do
                        let m = f.CodeAddress.FullMethodName

                        let show =
                            if String.IsNullOrEmpty m then
                                sprintf "%s!?" f.CodeAddress.ModuleName
                            else
                                sprintf "%s!%s" f.CodeAddress.ModuleName m

                        printfn "  [%2d] %s" depth show
                        depth <- depth + 1
                        f <- f.Caller

/// Print the top-N callers of any frame whose name contains `targetSubstring`.
/// Useful for pinpointing which source location is constructing a particular
/// compiler-generated closure (e.g. `clo@1023-6`). For every allocation sample
/// whose leaf frame matches, bucket by the immediate caller and report totals.
let aggregateCallers (tracePath: string) (targetSubstring: string) (topN: int) =
    printfn "Opening %s ..." tracePath
    let etlxPath = TraceLog.CreateFromEventPipeDataFile(tracePath)
    use traceLog = new TraceLog(etlxPath)
    printfn "Loaded. %d events, %d processes." traceLog.EventCount traceLog.Processes.Count

    let callerBytes = Dictionary<string, int64>()
    let mutable matchedBytes = 0L
    let mutable matchedSamples = 0

    for ev in traceLog.Events do
        let name = ev.EventName

        if isAllocationEvent name then
            let bytes =
                match ev with
                | :? GCAllocationTickTraceData as a -> int64 a.AllocationAmount64
                | _ -> 0L

            if bytes > 0L then
                let cs = ev.CallStack()

                if cs <> null then
                    let leafMethod = cs.CodeAddress.FullMethodName

                    if not (String.IsNullOrEmpty leafMethod) && leafMethod.Contains targetSubstring then
                        matchedBytes <- matchedBytes + bytes
                        matchedSamples <- matchedSamples + 1
                        let caller = cs.Caller

                        let callerName =
                            if caller = null then
                                "<root>"
                            else
                                let m = caller.CodeAddress.FullMethodName
                                let mo = caller.CodeAddress.ModuleName

                                if String.IsNullOrEmpty m then
                                    sprintf "%s!?" mo
                                else
                                    sprintf "%s!%s" mo m

                        add callerBytes callerName bytes

    printfn ""

    printfn
        "Matched %d samples / %s bytes at leaf frames containing '%s'"
        matchedSamples
        (matchedBytes.ToString("N0"))
        targetSubstring

    printfn ""

    let pct v =
        if matchedBytes = 0L then
            "n/a"
        else
            sprintf "%5.2f%%" (float v / float matchedBytes * 100.0)

    let fmt (b: int64) = sprintf "%12s" (b.ToString("N0"))
    printfn "## Top %d immediate callers" topN

    for kv in callerBytes |> Seq.sortByDescending (fun kv -> kv.Value) |> Seq.truncate topN do
        printfn "%s  %s  %s" (pct kv.Value) (fmt kv.Value) kv.Key

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

/// Opens a CPU trace (ETW .etl, or EventPipe .nettrace with the sampler enabled)
/// and prints top-N-by-self / top-N-by-total tables counting CPU samples.
/// Each sample is ≈1ms of on-CPU time, so the counts proxy wall-time attribution.
let aggregateCpu (tracePath: string) (filter: string) =
    printfn "Opening %s ..." tracePath
    use traceLog = openTraceLog tracePath
    printfn "Loaded. %d events, %d processes." traceLog.EventCount traceLog.Processes.Count

    let selfSamples = Dictionary<string, int64>()
    let totalSamples = Dictionary<string, int64>()
    let mutable totalCount = 0L
    let mutable samplesWithStack = 0

    for ev in traceLog.Events do
        match ev with
        | :? SampledProfileTraceData ->
            totalCount <- totalCount + 1L
            let cs = ev.CallStack()

            if cs <> null then
                samplesWithStack <- samplesWithStack + 1
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
                        add selfSamples display 1L
                        isLeaf <- false

                    if seen.Add display then
                        add totalSamples display 1L

                    frame <- frame.Caller
        | _ -> ()

    printfn ""

    printfn "CPU samples: %d   with stack: %d   (~%d ms of on-CPU time at 1 kHz)" totalCount samplesWithStack totalCount

    printfn "Filter: %s" filter
    printfn ""

    let matches (name: string) = filter = "*" || name.Contains filter

    let pct v =
        if totalCount = 0L then
            "n/a"
        else
            sprintf "%5.2f%%" (float v / float totalCount * 100.0)

    let fmtCount (b: int64) = sprintf "%12s" (b.ToString("N0"))

    let short (s: string) =
        if s.Length > 140 then s.Substring(0, 137) + "..." else s

    let topBy (d: Dictionary<string, int64>) n =
        d
        |> Seq.filter (fun kv -> matches kv.Key)
        |> Seq.sortByDescending (fun kv -> kv.Value)
        |> Seq.truncate n
        |> Seq.toArray

    printfn "## Top 30 frames by SELF CPU samples (leaf = where the CPU is spent)"

    for kv in topBy selfSamples 30 do
        printfn "%s  %s  %s" (pct kv.Value) (fmtCount kv.Value) (short kv.Key)

    printfn ""
    printfn "## Top 30 frames by TOTAL (inclusive) CPU samples"

    for kv in topBy totalSamples 30 do
        printfn "%s  %s  %s" (pct kv.Value) (fmtCount kv.Value) (short kv.Key)
