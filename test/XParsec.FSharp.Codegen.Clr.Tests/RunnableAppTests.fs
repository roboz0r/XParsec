module XParsec.FSharp.Codegen.Clr.Tests.RunnableAppTests

open System
open Expecto
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Proves the emitted PE runs as a standalone framework-dependent app under the
// `dotnet` host, not just in-process. Beyond the PE, the host needs deployment
// plumbing `materialiseApp` writes: a `runtimeconfig.json` (the host won't start
// without one) and a copy of `FSharp.Core.dll` (absent from the shared framework).
// The on-disk-PE anchors below (former `Slice1`/`Rung2`) cover the lower-level
// `materialise` (a loadable DLL, no app bundle) and a recursive-static-method app.

[<Tests>]
let tests =
    testList
        "RunnableApp"
        [
            // `materialise` (not `materialiseApp`) writes a loadable PE to disk that
            // round-trips through the runtime loader and runs (former Slice1).
            test "`materialise` writes a loadable PE to disk that runs and prints \"hi\"" {
                let outDir = tmpDir "runnable-materialise"
                let outPath = IO.Path.Combine(outDir, "MaterialiseHi.dll")

                let project =
                    { ProjectInfo.defaults "MaterialiseHi" with
                        OutputPath = Some outPath
                    }

                let artifact = compileSourceTo project "printfn \"hi\""
                Codegen.materialise artifact

                Expect.isTrue (IO.File.Exists outPath) "PE written to disk"

                let bytes = IO.File.ReadAllBytes outPath
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "disk PE Main returns 0"
                Expect.equal (output.Trim()) "hi" "disk PE prints hi"
            }

            test "a recursive static-method program runs as a standalone `dotnet <dll>` app (prints 15)" {
                let outDir = tmpDir "runnable-static-app"
                let project = ProjectInfo.app "XParsecStaticApp" outDir

                let src =
                    "let rec sumTo n =\n    match n with\n    | 0 -> 0\n    | _ -> n + sumTo (n - 1)\nprintfn \"%d\" (sumTo 5)"

                let artifact = compileSourceTo project src
                Codegen.materialiseApp project artifact

                let dllPath = IO.Path.Combine(outDir, "XParsecStaticApp.dll")
                let exitCode, output = runOnDisk dllPath

                Expect.equal exitCode 0 (sprintf "dotnet exits 0 (output was: %s)" output)
                Expect.equal (output.Trim()) "15" "the recursive static method runs as a real assembly"
            }

            test "`materialiseApp` emits a `dotnet <dll>`-runnable bundle that prints [1; 2; 3]" {
                let outDir = tmpDir "runnable-app"
                // `withCore`: a `%A` bundle needs `Vesper.Core` (the formatter's
                // `RuntimeFormatState` implements the Core-owned `IFormatSink`), so
                // its on-disk path must be a resolvable reference source for the
                // bundle's transitive-closure copy.
                let project = withCore (ProjectInfo.app "XParsecListApp" outDir)

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
