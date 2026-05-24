module XParsec.FSharp.Codegen.Clr.Tests.RunnableAppTests

open System
open Expecto
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Proves the emitted PE runs as a standalone framework-dependent app under the
// `dotnet` host, not just in-process. Beyond the PE, the host needs deployment
// plumbing `materialiseApp` writes: a `runtimeconfig.json` (the host won't start
// without one) and a copy of `FSharp.Core.dll` (absent from the shared framework).

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
