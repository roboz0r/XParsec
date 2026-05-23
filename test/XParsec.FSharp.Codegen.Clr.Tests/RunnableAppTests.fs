module XParsec.FSharp.Codegen.Clr.Tests.RunnableAppTests

open System
open Expecto
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The "real `dotnet my.dll`" milestone from `il-emission-roadmap.md`. The
// slice tests load the emitted PE in-process (`Assembly.Load`); this proves the
// same PE runs as a standalone framework-dependent app under the `dotnet` host.
// The only thing beyond the PE is deployment plumbing: a `runtimeconfig.json`
// beside the dll (the host won't start without one) and a copy of
// `FSharp.Core.dll` (absent from the shared framework). `materialiseApp` writes
// both; `runOnDisk` shells out to `dotnet <dll>` and captures the result.

[<Tests>]
let tests =
    testList
        "RunnableApp"
        [
            test "`materialiseApp` emits a `dotnet <dll>`-runnable bundle that prints [1; 2; 3]" {
                let outDir = tmpDir "runnable-app"
                let project = ProjectInfo.app "XParsecListApp" outDir

                let artifact = compileSourceTo project "printfn \"%A\" [1; 2; 3]"
                Codegen.materialiseApp project artifact

                let dllPath = IO.Path.Combine(outDir, "XParsecListApp.dll")
                Expect.isTrue (IO.File.Exists dllPath) "PE written"

                Expect.isTrue
                    (IO.File.Exists(IO.Path.Combine(outDir, "XParsecListApp.runtimeconfig.json")))
                    "runtimeconfig.json written"

                Expect.isTrue (IO.File.Exists(IO.Path.Combine(outDir, "FSharp.Core.dll"))) "FSharp.Core.dll copied"

                let exitCode, output = runOnDisk dllPath
                Expect.equal exitCode 0 (sprintf "dotnet exits 0 (output was: %s)" output)
                Expect.equal (output.Trim()) "[1; 2; 3]" "the standalone app prints the list"
            }
        ]
