module XParsec.FSharp.Codegen.Clr.Tests.ConformanceTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common.Tests.Conformance
open XParsec.FSharp.Codegen.Clr.Tests
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The CLR's obligations over the shared corpus (`test/Codegen.Conformance/`). This
// file is the whole of the CLR's participation: the loader, the goldens, and every
// assertion live in `Codegen.Common.Tests`, so a corpus program added there is picked
// up here with no edit.

/// The corpus names programs with hyphens (`arith-byte`); an assembly name has to be
/// an identifier the emitted module can carry.
let private assemblyName (program: string) : string =
    "Conformance_" + program.Replace("-", "_")

let private clrBackend: Backend =
    {
        Name = "clr"
        // The CLR runtime is the host, so `CompileAndRun` is never `None` and no row
        // ever skips. `runEntryPoint` invokes the entry point IN-PROCESS by reflection:
        // an uncaught user exception never reaches a process exit code, it comes back
        // as a `failwithf` naming the inner exception's type + message. That throw IS
        // the CLR's fault surface, so it is caught here and reported as `Faulted`
        // rather than escaping as a test error.
        //
        // The compile is deliberately OUTSIDE the `try`: a backend that cannot emit a
        // program is not a conformance verdict, it is a broken backend, and it must
        // surface as such rather than masquerade as a runtime fault.
        CompileAndRun =
            fun name src ->
                let _, artifact = compileSource (assemblyName name) src
                let bytes = Codegen.toBytes artifact

                // Every corpus program's PE goes through the metadata assertions. Like
                // the compile, this sits OUTSIDE the `try`: metadata the emitter's own
                // prefix-sum prediction contradicts is a broken backend, not a
                // conformance verdict, and must surface as such.
                MetadataStructure.assertWellFormed name bytes

                try
                    Some(RunOutcome.Completed(runEntryPoint bytes))
                with ex ->
                    Some(RunOutcome.Faulted ex.Message)
        Diagnostics =
            fun src ->
                (analyse src).Diagnostics
                |> List.filter (fun d -> d.Severity = Severity.Error)
                |> List.map (fun d -> d.Message)
    }

[<Tests>]
let tests = conformanceTests clrBackend
