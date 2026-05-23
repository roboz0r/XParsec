module XParsec.FSharp.Codegen.Clr.Tests.FSharpCoreDepsTests

open System
open Expecto
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// `ClrArtifact.FSharpCoreDependencies` records — positively — which FSharp.Core
// constructs the emission referenced. Empty ⇒ the PE has no `FSharp.Core.dll`
// dependency, so `materialiseApp` ships the app without it; non-empty is the
// list of constructs still pinning the dependency (the §D3 cut list). Every
// FSharp.Core reference is minted through `ClrProvider`, which marks each
// use-site, so the set is authoritative.

[<Tests>]
let tests =
    testList
        "FSharpCoreDeps"
        [
            // ---- The happy path pins nothing ----

            test "a happy-path `printfn` references no FSharp.Core construct" {
                // Lowered to `Vesper.Formatter` + `System.Console`; arithmetic and
                // literals touch no FSharp.Core type. So the set is empty.
                let _, artifact = compileSource "DepsHappyPrintf" "printfn \"%d\" 42"
                Expect.isEmpty artifact.FSharpCoreDependencies "happy-path printf has no FSharp.Core dependency"
            }

            test "interpolation lowering references no FSharp.Core construct" {
                let _, artifact = compileSource "DepsInterp" "printfn \"%s\" $\"n={42}\""
                Expect.isEmpty artifact.FSharpCoreDependencies "interpolation has no FSharp.Core dependency"
            }

            // ---- Cold paths name exactly what pins them ----

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

            // ---- The on-disk payoff: a zero-dependency app ships without FSharp.Core ----

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
