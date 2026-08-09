module XParsec.FSharp.Codegen.Clr.Tests.FSharpCoreDepsTests

open System
open Expecto
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// `ClrArtifact.FSharpCoreDependencies` records which FSharp.Core constructs the emission
// referenced. Empty ⇒ no `FSharp.Core.dll` dependency, so the app ships without it;
// non-empty is the cut list.

[<Tests>]
let tests =
    testList
        "FSharpCoreDeps"
        [
            test "a happy-path `printfn` references no FSharp.Core construct" {
                let _, artifact = compileSource "DepsHappyPrintf" "printfn \"%d\" 42"
                Expect.isEmpty artifact.FSharpCoreDependencies "happy-path printf has no FSharp.Core dependency"
            }

            test "a fully-applied `fprintf` to a writer references no FSharp.Core construct" {
                // A fully-applied `fprintf`/`fprintfn` lowers to a `Vesper.Formatter`
                // `ToWriter` sink.
                let _, artifact = compileSource "DepsFprintf" "fprintf System.Console.Out \"%d\" 42"

                Expect.isEmpty artifact.FSharpCoreDependencies "native fprintf has no FSharp.Core dependency"
            }

            test "a fully-applied `bprintf` to a builder references no FSharp.Core construct" {
                // A fully-applied `bprintf` lowers to a `Vesper.Formatter` `ToBuilder` sink.
                let _, artifact =
                    compileSource "DepsBprintf" "bprintf (System.Text.StringBuilder()) \"%d\" 42"

                Expect.isEmpty artifact.FSharpCoreDependencies "native bprintf has no FSharp.Core dependency"
            }

            test "interpolation lowering references no FSharp.Core construct" {
                let _, artifact = compileSource "DepsInterp" "printfn \"%s\" $\"n={42}\""
                Expect.isEmpty artifact.FSharpCoreDependencies "interpolation has no FSharp.Core dependency"
            }

            // The provider's refs are `lazy`, so an `AssemblyRef` row is added only when one
            // is actually forced, so an empty use-set leaves no dead reference row.
            test "a happy-path executable carries no FSharp.Core reference row" {
                let _, artifact = compileSource "DepsCleanExe" "printfn \"%d\" 42"
                Expect.isEmpty artifact.FSharpCoreDependencies "the use-set is empty"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let refs = asm.GetReferencedAssemblies() |> Array.map (fun a -> a.Name)

                Expect.isFalse
                    (refs |> Array.contains "FSharp.Core")
                    (sprintf "no FSharp.Core AssemblyRef row in the executable (refs: %A)" refs)
            }

            // The space flag is a pure no-op for `%A`, so `% A` lowers on the structural
            // engine exactly as plain `%A` does.
            test "`printfn \"% A\"` lowers on the engine (cold path cut)" {
                let _, artifact = compileSource "DepsSpaceA" "printfn \"% A\" 42"
                let deps = artifact.FSharpCoreDependencies

                Expect.isFalse
                    (deps |> Seq.contains "Microsoft.FSharp.Core.PrintfModule.PrintFormatLine")
                    "% A does NOT take the PrintFormatLine cold path"

                Expect.isEmpty deps (sprintf "%% A is pure Vesper — no FSharp.Core dependency (%A)" deps)
            }

            // `%+08.2f` (forced sign + zero-pad float) lowers natively: a half-to-even
            // `"F2"` body, then zero-padding applied after the sign.
            test "`printfn \"%+08.2f\"` lowers natively, so it pins no FSharp.Core (former cold pin)" {
                let _, artifact = compileSource "DepsPlusZeroF" "printfn \"%+08.2f\" 1234.5"

                Expect.isEmpty
                    artifact.FSharpCoreDependencies
                    (sprintf "native %%+08.2f pins no FSharp.Core (%A)" artifact.FSharpCoreDependencies)
            }

            test "`printfn \"%*d\"` (star width) lowers natively, so it pins no FSharp.Core" {
                // The guarded runtime width feeds the `Vesper.Formatter` signed-alignment
                // members.
                let _, artifact = compileSource "DepsStarPrintf" "printfn \"%*d\" 5 42"

                Expect.isEmpty
                    artifact.FSharpCoreDependencies
                    (sprintf "native star-width printf pins no FSharp.Core (%A)" artifact.FSharpCoreDependencies)
            }

            test "`printfn \"%*A\"` (bare star width) lowers natively, so it takes no cold path" {
                // Bare `%*A` feeds the runtime column budget to the structural engine.
                let _, artifact = compileSource "DepsStarA" "printfn \"%*A\" 1 [1; 2; 3]"

                Expect.isFalse
                    (artifact.FSharpCoreDependencies
                     |> Seq.contains "Microsoft.FSharp.Core.PrintfModule.PrintFormatLine")
                    "bare %*A does NOT take the PrintFormatLine cold path"
            }

            // Star *precision* lowers natively: the float forms build the .NET format string
            // in-handler from the runtime precision.
            test "star precision (`%.*f`, `%*.*f`, `%.*e`, `%.*g`, `%+.*f`) lowers natively, pinning no FSharp.Core" {
                let native =
                    [
                        "DepsPrecF", "printfn \"%.*f\" 2 3.5"
                        "DepsPrecWF", "printfn \"%*.*f\" 8 2 3.5"
                        "DepsPrecE", "printfn \"%.*e\" 3 31415.9"
                        "DepsPrecG", "printfn \"%.*g\" 4 31415.9"
                        "DepsPrecPlus", "printfn \"%+.*f\" 3 3.14159"
                    ]

                for name, src in native do
                    let _, artifact = compileSource name src

                    Expect.isEmpty
                        artifact.FSharpCoreDependencies
                        (sprintf "%s lowers natively — no FSharp.Core (%A)" src artifact.FSharpCoreDependencies)
            }

            // `%.*A` feeds the runtime print-size budget to the structural engine. The list
            // LITERAL argument still pins `FSharpList`, so the assertion here can only be
            // the absence of `PrintFormatLine`, not a fully-empty use-set.
            test "`%.*A` (star precision) lowers on the structural engine, so it takes no cold path" {
                let _, artifact = compileSource "DepsPrecA" "printfn \"%.*A\" 2 [1; 2; 3]"

                Expect.isFalse
                    (artifact.FSharpCoreDependencies
                     |> Seq.contains "Microsoft.FSharp.Core.PrintfModule.PrintFormatLine")
                    "star-precision %.*A does NOT take the PrintFormatLine cold path"
            }

            // `-` and `+` are pure no-ops for `%A`, so `%-*A` / `%+*A` take the same runtime
            // column budget on the structural engine as a bare `%*A`.
            test "`%-*A` / `%+*A` (flagged star-%A) lower natively, so they take no cold path" {
                let native =
                    [
                        "DepsStarLeftA", "printfn \"%-*A\" 1 [1; 2; 3]"
                        "DepsStarPlusA", "printfn \"%+*A\" 1 [1; 2; 3]"
                    ]

                for name, src in native do
                    let _, artifact = compileSource name src

                    Expect.isFalse
                        (artifact.FSharpCoreDependencies
                         |> Seq.contains "Microsoft.FSharp.Core.PrintfModule.PrintFormatLine")
                        (sprintf "%s does NOT take the PrintFormatLine cold path" src)
            }

            // A project-local record / DU carries a synthesised
            // `IStructuralFormattable.Format`, so `%A` of one lowers on the structural
            // engine, which is observable only as the absence of the FSharp.Core constructs.
            test "`%A` of a synthesised record pins no FSharp.Core (cold path cut)" {
                let _, artifact =
                    compileSource "DepsStructRec" "type R = { X: int; Y: string }\nprintfn \"%A\" { X = 1; Y = \"a\" }"

                let deps = artifact.FSharpCoreDependencies

                Expect.isFalse
                    (deps |> Seq.contains "Microsoft.FSharp.Core.PrintfModule.PrintFormatLine")
                    "%A of a record does NOT take the PrintFormatLine cold path"

                Expect.isEmpty deps (sprintf "record %%A is pure Vesper — no FSharp.Core dependency (%A)" deps)
            }

            test "`%A` of a synthesised union pins no FSharp.Core (cold path cut)" {
                let _, artifact =
                    compileSource "DepsStructDu" "type Opt = | N | S of int\nlet v = S 3\nprintfn \"%A\" v"

                let deps = artifact.FSharpCoreDependencies

                Expect.isFalse
                    (deps |> Seq.contains "Microsoft.FSharp.Core.PrintfModule.PrintFormatLine")
                    "%A of a union does NOT take the PrintFormatLine cold path"

                Expect.isEmpty deps (sprintf "union %%A is pure Vesper — no FSharp.Core dependency (%A)" deps)
            }

            // An EXTERNAL Vesper-package union resolves as Vesper-compiled, so it carries
            // the synthesised `Format` and `%A` of it lowers on the engine. Stdout cannot
            // show this, because the cold path renders `Ok 5` identically, but the use-set can.
            test "`%A` of an external Vesper union lowers on the engine (no cold-path pin)" {
                let artifact =
                    compileResultArtifact "open Vesper\nlet r : Result<int, string> = Ok 5\nprintfn \"%A\" r"

                let deps = artifact.FSharpCoreDependencies

                Expect.isFalse
                    (deps |> Seq.contains "Microsoft.FSharp.Core.PrintfModule.PrintFormatLine")
                    "external-union %A does NOT take the PrintFormatLine cold path"

                Expect.isEmpty
                    deps
                    (sprintf "the external-union %%A program is BCL-only + Vesper — no FSharp.Core dependency (%A)" deps)
            }

            // `%A` of an arbitrary BCL type renders through the `IFormattable` / `ToString`
            // arm of the dispatcher, which is BCL-only.
            test "`%A` of a BCL type is BCL-only, so it pins no FSharp.Core" {
                let _, artifact = compileSource "DepsBclA" "printfn \"%A\" System.Guid.Empty"

                Expect.isEmpty
                    artifact.FSharpCoreDependencies
                    (sprintf "BCL %%A pins no FSharp.Core (%A)" artifact.FSharpCoreDependencies)
            }

            // A polymorphic `%A` has a hole typed by the function's own method typar, so the
            // emitted call is `AppendStructured<!!i>`. This test throws outright if that
            // typar cannot be authored.
            test "polymorphic `%A` (`let f x = printfn \"%A\" x`) lowers on the engine (no cold-path pin)" {
                let _, artifact = compileSource "DepsPolyA" "let f x = printfn \"%A\" x\nf 42"
                let deps = artifact.FSharpCoreDependencies

                Expect.isFalse
                    (deps |> Seq.contains "Microsoft.FSharp.Core.PrintfModule.PrintFormatLine")
                    "polymorphic %A does NOT take the PrintFormatLine cold path"

                Expect.isFalse
                    (deps |> Seq.contains "Microsoft.FSharp.Core.PrintfFormat`4 (.ctor)")
                    "polymorphic %A constructs no PrintfFormat"
            }

            test "a list literal pins FSharpList" {
                let _, artifact =
                    compileSource "DepsList" "let nums = [1; 2; 3]\nprintfn \"%A\" nums"

                let deps = artifact.FSharpCoreDependencies
                Expect.contains deps "Microsoft.FSharp.Collections.FSharpList`1" "list type"
                Expect.contains deps "Microsoft.FSharp.Collections.FSharpList`1.Cons" "cons constructor"
                Expect.contains deps "Microsoft.FSharp.Collections.FSharpList`1.get_Empty" "nil getter"
            }

            test "List.fold over a bare-program list is BCL-only + Vesper, no FSharp.Core" {
                // Constrained by `sum`, the literal builds a Vesper `List`, and `List.fold`
                // is emitted inline over it with a `Vesper.Fun` closure for `(+)`.
                let src =
                    "let inline sum xs = List.fold (+) 0 xs\nlet nums = [1; 2; 3; 4; 5]\nprintfn \"%d\" (sum nums)"

                let _, artifact = compileSource "DepsFold" src

                Expect.isEmpty
                    artifact.FSharpCoreDependencies
                    (sprintf "the canonical sample pins no FSharp.Core construct (%A)" artifact.FSharpCoreDependencies)
            }

            test "`materialiseApp` omits FSharp.Core.dll for a zero-dependency app, which still runs" {
                let outDir = tmpDir "no-fsharpcore-app"
                // `withCore`: the happy-path `printfn` binds `Vesper.Printf` and its deps,
                // so their on-disk paths must be references for the bundle to copy them.
                let project = withCore (ProjectInfo.app "XParsecNoCoreApp" outDir)

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
