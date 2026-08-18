module XParsec.FSharp.Codegen.Clr.Tests.ConformanceTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Common.Tests.Conformance
open XParsec.FSharp.Codegen.Clr.Tests
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The CLR's side of the shared conformance corpus. Only this backend record is local,
// so a program added to the corpus is picked up here with no edit.

let private clrBackend: Backend =
    {
        Name = Target.Clr
        // The entry point runs IN-PROCESS by reflection, so an uncaught user exception
        // never reaches an exit code: it arrives as a throw carrying the inner exception,
        // and that throw is the fault surface reported as `Faulted`.
        CompileAndRun =
            fun name src ->
                let _, artifact = compileSource (conformanceAssemblyName name) src
                let bytes = Codegen.toBytes artifact

                // Outside the `try`, like the compile: a backend that cannot emit a
                // well-formed program is not a conformance verdict, it is broken.
                MetadataStructure.assertWellFormed name bytes

                try
                    Some(RunOutcome.Completed(runEntryPoint bytes))
                with ex ->
                    Some(RunOutcome.Faulted ex.Message)
        Diagnostics = fun src -> (analyse src).Diagnostics |> Diagnostic.errors |> List.map (fun d -> d.Message)
    }

[<Tests>]
let tests = conformanceTests clrBackend
