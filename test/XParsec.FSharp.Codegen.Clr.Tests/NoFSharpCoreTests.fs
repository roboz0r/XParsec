module XParsec.FSharp.Codegen.Clr.Tests.NoFSharpCoreTests

open System
open Expecto
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.PeInspection
open XParsec.FSharp.Codegen.Clr.Tests.PackageHarness

// Nothing the backend emits references an FSharp.Core construct, so the invariant is asserted on
// the ARTIFACT: no `AssemblyRef` row in the PE, no `FSharp.Core.dll` beside a materialised app.

// Each specifier below is a format that must lower on the structural engine, so no reference
// row appears for it.
let private nativeFormats =
    [
        "SpaceA", "printfn \"% A\" 42"
        "PlusZeroF", "printfn \"%+08.2f\" 1234.5"
        "StarWidth", "printfn \"%*d\" 5 42"
        "StarWidthA", "printfn \"%*A\" 1 [1; 2; 3]"
        "StarLeftA", "printfn \"%-*A\" 1 [1; 2; 3]"
        "StarPlusA", "printfn \"%+*A\" 1 [1; 2; 3]"
        "StarPrecA", "printfn \"%.*A\" 2 [1; 2; 3]"
        "StarPrecF", "printfn \"%.*f\" 2 3.5"
        "StarWidthPrecF", "printfn \"%*.*f\" 8 2 3.5"
        "StarPrecPlusF", "printfn \"%+.*f\" 3 3.14159"
        "StarPrecE", "printfn \"%.*e\" 3 31415.9"
        "StarPrecG", "printfn \"%.*g\" 4 31415.9"
        "SynthRecordA", "type R = { X: int; Y: string }\nprintfn \"%A\" { X = 1; Y = \"a\" }"
        "SynthUnionA", "type Opt = | N | S of int\nlet v = S 3\nprintfn \"%A\" v"
        "BclA", "printfn \"%A\" System.Guid.Empty"
        "PolyA", "let f x = printfn \"%A\" x\nf 42"
        "ListA", "let nums = [1; 2; 3]\nprintfn \"%A\" nums"
        "Interp", "printfn \"%s\" $\"n={42}\""
        "Fprintf", "fprintf System.Console.Out \"%d\" 42"
        "Bprintf", "bprintf (System.Text.StringBuilder()) \"%d\" 42"
        "Fold", "let inline sum xs = List.fold (+) 0 xs\nprintfn \"%d\" (sum [1; 2; 3])"
    ]

[<Tests>]
let tests =
    testList
        "NoFSharpCore"
        [
            testList
                "every format specifier lowers on the structural engine"
                [
                    for name, src in nativeFormats do
                        test src {
                            let artifact = compileSource ("Deps" + name) src

                            expectNoFSharpCore artifact src
                        }

                    // An EXTERNAL Vesper union carries the synthesised `Format`, so `%A` of one
                    // lowers on the engine too. Stdout cannot show this — the cold path renders
                    // `Ok 5` identically — but the use-set can.
                    test "printfn \"%A\" (Ok 5 : Result<int, string>)" {
                        let artifact =
                            compileResultArtifact "open Vesper\nlet r : Result<int, string> = Ok 5\nprintfn \"%A\" r"

                        expectNoFSharpCore artifact "%A of an external Vesper union"
                    }
                ]

            // The provider's refs are `lazy`, so an `AssemblyRef` row is added only when one
            // is actually forced, so an empty use-set leaves no dead reference row.
            test "an emitted executable does not carry an FSharp.Core reference row" {
                let artifact = compileSource "DepsCleanExe" "printfn \"%d\" 42"
                let asm = loadAssembly (Codegen.toBytes artifact)
                let refs = asm.GetReferencedAssemblies() |> Array.map (fun a -> a.Name)

                Expect.isFalse
                    (refs |> Array.contains "FSharp.Core")
                    (sprintf "no FSharp.Core AssemblyRef row in the executable (refs: %A)" refs)
            }

            test "`materialiseApp` does not write FSharp.Core.dll, and the app still runs" {
                let outDir = tmpDir "no-fsharpcore-app"
                // `withCore`: the happy-path `printfn` binds `Vesper.Printf` and its deps,
                // so their on-disk paths must be references for the bundle to copy them.
                let project = withCore (ProjectInfo.app "XParsecNoCoreApp" outDir)

                // Deterministic regardless of a prior run leaving the dll behind.
                let coreDst = IO.Path.Combine(outDir, "FSharp.Core.dll")

                if IO.File.Exists coreDst then
                    IO.File.Delete coreDst

                runsOnDisk project "42" "printfn \"%d\" 42" |> ignore

                Expect.isFalse (IO.File.Exists coreDst) "FSharp.Core.dll NOT copied, because there is no dependency"
            }
        ]
