module XParsec.FSharp.Codegen.Js.Tests.ConformanceTests

open Expecto
open XParsec.FSharp.Codegen.Common.Tests.Conformance
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// The JS backend's obligations over the shared corpus (`test/Codegen.Conformance/`).
// Everything but this three-field record lives in `Codegen.Common.Tests`.

let private jsBackend: Backend =
    {
        Name = "js"
        // Node runs OUT of process, so a fault IS a non-zero exit; `runJs` has appended
        // stderr by then, carrying the thrown `Error`'s message. `None` (node absent)
        // reaches the runner as "runtime unavailable" and SKIPS the row.
        CompileAndRun =
            fun name src ->
                runJs ("conformance-" + name) src
                |> Option.map (fun (exitCode, output) ->
                    if exitCode = 0 then
                        RunOutcome.Completed(exitCode, output)
                    else
                        RunOutcome.Faulted output
                )
        Diagnostics = fun src -> analyseWith jsProvider.Value src |> List.map (fun d -> d.Message)
    }

[<Tests>]
let tests = conformanceTests jsBackend
