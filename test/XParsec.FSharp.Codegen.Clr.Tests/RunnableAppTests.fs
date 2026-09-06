module XParsec.FSharp.Codegen.Clr.Tests.RunnableAppTests

open System
open Expecto
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The emitted PE run as a standalone framework-dependent app under the `dotnet` host, not
// just in-process. The host will not start without a `runtimeconfig.json`, and needs any
// referenced assembly the shared framework does not carry copied beside the PE.

[<Tests>]
let tests =
    testList
        "RunnableApp"
        [
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
                // `withCore`: `printfn "%d"` binds `Vesper.Printf` and its deps, so their
                // on-disk paths must be references for the bundle to copy them beside the PE.
                let project = withCore (ProjectInfo.app "XParsecStaticApp" outDir)

                let src =
                    "let rec sumTo n =\n    match n with\n    | 0 -> 0\n    | _ -> n + sumTo (n - 1)\nprintfn \"%d\" (sumTo 5)"

                runsOnDisk project "15" src |> ignore
            }

            test "`materialiseApp` emits a `dotnet <dll>`-runnable bundle that prints a `%A` list" {
                let outDir = tmpDir "runnable-app"
                // `withCore`: a `%A` bundle needs `Vesper.Core`, because the formatter's
                // `RuntimeFormatState` implements the Core-owned `IFormatSink`.
                let project = withCore (ProjectInfo.app "XParsecListApp" outDir)

                let dllPath = runsOnDisk project "[1; 2; 3]" "printfn \"%A\" [1; 2; 3]"

                Expect.equal dllPath (IO.Path.Combine(outDir, "XParsecListApp.dll")) "the PE lands in the app directory"

                Expect.isTrue
                    (IO.File.Exists(IO.Path.Combine(outDir, "XParsecListApp.runtimeconfig.json")))
                    "runtimeconfig.json written"
            }
        ]
