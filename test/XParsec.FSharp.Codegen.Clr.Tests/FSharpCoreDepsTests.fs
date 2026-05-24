module XParsec.FSharp.Codegen.Clr.Tests.FSharpCoreDepsTests

open System
open Expecto
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// `ClrArtifact.FSharpCoreDependencies` records which FSharp.Core constructs the
// emission referenced. Empty ⇒ no `FSharp.Core.dll` dependency, so
// `materialiseApp` ships the app without it; non-empty is the §D3 cut list. The
// set is authoritative because every FSharp.Core reference is minted (and
// marked) through `ClrProvider`.

[<Tests>]
let tests =
    testList
        "FSharpCoreDeps"
        [
            test "a happy-path `printfn` references no FSharp.Core construct" {
                let _, artifact = compileSource "DepsHappyPrintf" "printfn \"%d\" 42"
                Expect.isEmpty artifact.FSharpCoreDependencies "happy-path printf has no FSharp.Core dependency"
            }

            test "interpolation lowering references no FSharp.Core construct" {
                let _, artifact = compileSource "DepsInterp" "printfn \"%s\" $\"n={42}\""
                Expect.isEmpty artifact.FSharpCoreDependencies "interpolation has no FSharp.Core dependency"
            }

            // G6 / P2: the provider's refs are `lazy`, so an `AssemblyRef` row is
            // added only when a ref is actually forced. Before that, constructing
            // `ClrProvider` added FSharp.Core's `AssemblyRef` eagerly — every
            // executable carried a dead reference row even with an empty use-set.
            test "a happy-path executable carries no FSharp.Core reference row (G6)" {
                let _, artifact = compileSource "DepsCleanExe" "printfn \"%d\" 42"
                Expect.isEmpty artifact.FSharpCoreDependencies "the use-set is empty"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let refs = asm.GetReferencedAssemblies() |> Array.map (fun a -> a.Name)

                Expect.isFalse
                    (refs |> Array.contains "FSharp.Core")
                    (sprintf "no FSharp.Core AssemblyRef row in the executable (refs: %A)" refs)
            }

            test "`printfn \"%A\"` (cold path) pins PrintfModule + PrintfFormat" {
                let _, artifact = compileSource "DepsColdPrintf" "printfn \"%A\" 42"
                let deps = artifact.FSharpCoreDependencies
                Expect.isNonEmpty deps "the %A cold path depends on FSharp.Core"

                Expect.contains
                    deps
                    "Microsoft.FSharp.Core.PrintfModule.PrintFormatLine"
                    "the cold path calls PrintFormatLine"

                Expect.contains deps "Microsoft.FSharp.Core.PrintfFormat`4 (.ctor)" "and constructs a PrintfFormat"
            }

            test "a list literal pins FSharpList" {
                let _, artifact =
                    compileSource "DepsList" "let nums = [1; 2; 3]\nprintfn \"%A\" nums"

                let deps = artifact.FSharpCoreDependencies
                Expect.contains deps "Microsoft.FSharp.Collections.FSharpList`1" "list type"
                Expect.contains deps "Microsoft.FSharp.Collections.FSharpList`1.Cons" "cons constructor"
                Expect.contains deps "Microsoft.FSharp.Collections.FSharpList`1.get_Empty" "nil getter"
            }

            test "a closure + List.fold pins FSharpFunc, ListModule, and FSharpList" {
                let src =
                    "let inline sum xs = List.fold (+) 0 xs\nlet nums = [1; 2; 3; 4; 5]\nprintfn \"%d\" (sum nums)"

                let _, artifact = compileSource "DepsFold" src
                let deps = artifact.FSharpCoreDependencies
                Expect.contains deps "Microsoft.FSharp.Collections.ListModule.Fold" "List.fold"
                Expect.contains deps "Microsoft.FSharp.Core.FSharpFunc`2 (closure base)" "synthesised closure base"
                Expect.contains deps "Microsoft.FSharp.Collections.FSharpList`1" "the list argument"
            }

            test "`materialiseApp` omits FSharp.Core.dll for a zero-dependency app, which still runs" {
                let outDir = tmpDir "no-fsharpcore-app"
                let project = ProjectInfo.app "XParsecNoCoreApp" outDir

                // Deterministic regardless of a prior run leaving the dll behind.
                let coreDst = IO.Path.Combine(outDir, "FSharp.Core.dll")

                if IO.File.Exists coreDst then
                    IO.File.Delete coreDst

                let artifact = compileSourceTo project "printfn \"%d\" 42"
                Expect.isEmpty artifact.FSharpCoreDependencies "the happy path references no FSharp.Core construct"

                Codegen.materialiseApp project artifact

                let dllPath = IO.Path.Combine(outDir, "XParsecNoCoreApp.dll")
                Expect.isTrue (IO.File.Exists dllPath) "PE written"
                Expect.isFalse (IO.File.Exists coreDst) "FSharp.Core.dll NOT copied — no dependency"

                let exitCode, output = runOnDisk dllPath
                Expect.equal exitCode 0 (sprintf "dotnet exits 0 (output was: %s)" output)
                Expect.equal (output.Trim()) "42" "the standalone app runs and prints 42 without FSharp.Core present"
            }
        ]
