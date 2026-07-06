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

            test "a fully-applied `fprintf` to a writer references no FSharp.Core construct" {
                // `fprintf`/`fprintfn` now lower natively to a `ToWriter` sink, so a
                // fully-applied writer call rides the Vesper.Formatter path instead of
                // FSharp.Core's cold printf.
                let _, artifact = compileSource "DepsFprintf" "fprintf System.Console.Out \"%d\" 42"

                Expect.isEmpty artifact.FSharpCoreDependencies "native fprintf has no FSharp.Core dependency"
            }

            test "a fully-applied `bprintf` to a builder references no FSharp.Core construct" {
                // `bprintf` now lowers natively to a `ToBuilder` sink, so a
                // fully-applied builder call rides the Vesper.Formatter path instead of
                // FSharp.Core's cold printf.
                let _, artifact =
                    compileSource "DepsBprintf" "bprintf (System.Text.StringBuilder()) \"%d\" 42"

                Expect.isEmpty artifact.FSharpCoreDependencies "native bprintf has no FSharp.Core dependency"
            }

            test "interpolation lowering references no FSharp.Core construct" {
                let _, artifact = compileSource "DepsInterp" "printfn \"%s\" $\"n={42}\""
                Expect.isEmpty artifact.FSharpCoreDependencies "interpolation has no FSharp.Core dependency"
            }

            // The provider's refs are `lazy`, so an `AssemblyRef` row is added only
            // when a ref is actually forced — no dead reference row for an empty
            // use-set.
            test "a happy-path executable carries no FSharp.Core reference row" {
                let _, artifact = compileSource "DepsCleanExe" "printfn \"%d\" 42"
                Expect.isEmpty artifact.FSharpCoreDependencies "the use-set is empty"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let refs = asm.GetReferencedAssemblies() |> Array.map (fun a -> a.Name)

                Expect.isFalse
                    (refs |> Array.contains "FSharp.Core")
                    (sprintf "no FSharp.Core AssemblyRef row in the executable (refs: %A)" refs)
            }

            // `% A` (the space flag on `%A`) used to ride the cold path; it now lowers
            // on the structural engine like plain `%A` (the space flag is a pure no-op
            // for `%A`), so it pins no FSharp.Core construct.
            test "`printfn \"% A\"` lowers on the engine (cold path cut)" {
                let _, artifact = compileSource "DepsSpaceA" "printfn \"% A\" 42"
                let deps = artifact.FSharpCoreDependencies

                Expect.isFalse
                    (deps |> Seq.contains "Microsoft.FSharp.Core.PrintfModule.PrintFormatLine")
                    "% A does NOT take the PrintFormatLine cold path"

                Expect.isEmpty deps (sprintf "%% A is pure Vesper — no FSharp.Core dependency (%A)" deps)
            }

            // `%+08.2f` (forced sign + zero-pad float) used to be THE cold pin; it now
            // lowers natively — the forced sign rides a half-to-even `"F2"` body then
            // zero-pads after the sign (`AppendForcedSignZeroPaddedFloat`), so it pins no
            // FSharp.Core construct.
            test "`printfn \"%+08.2f\"` lowers natively — no FSharp.Core (former cold pin)" {
                let _, artifact = compileSource "DepsPlusZeroF" "printfn \"%+08.2f\" 1234.5"

                Expect.isEmpty
                    artifact.FSharpCoreDependencies
                    (sprintf "native %%+08.2f pins no FSharp.Core (%A)" artifact.FSharpCoreDependencies)
            }

            test "`printfn \"%*d\"` (star width) lowers natively — no FSharp.Core" {
                // Star *width* now lowers to the `Vesper.Formatter` handler (the guarded
                // runtime width feeds the signed-alignment members), so the whole program
                // references no FSharp.Core construct.
                let _, artifact = compileSource "DepsStarPrintf" "printfn \"%*d\" 5 42"

                Expect.isEmpty
                    artifact.FSharpCoreDependencies
                    (sprintf "native star-width printf pins no FSharp.Core (%A)" artifact.FSharpCoreDependencies)
            }

            test "`printfn \"%*A\"` (bare star width) lowers natively — no FSharp.Core" {
                // Bare `%*A` takes the runtime column budget on the structural engine
                // (`AppendStructured`), so no FSharp.Core cold path.
                let _, artifact = compileSource "DepsStarA" "printfn \"%*A\" 1 [1; 2; 3]"

                Expect.isFalse
                    (artifact.FSharpCoreDependencies
                     |> Seq.contains "Microsoft.FSharp.Core.PrintfModule.PrintFormatLine")
                    "bare %*A does NOT take the PrintFormatLine cold path"
            }

            // Star *precision* now lowers natively: the float forms build the .NET format
            // string in-handler from the runtime precision — so the whole program pins no
            // FSharp.Core construct.
            test "star precision (`%.*f`, `%*.*f`, `%.*e`, `%.*g`, `%+.*f`) lowers natively — no FSharp.Core" {
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

            // `%.*A` feeds the runtime `PrintSize` budget to the structural engine
            // (`AppendStructured`), not the cold path. The list *literal* argument still
            // pins `FSharpList`, so assert the discriminator — no `PrintFormatLine` — as
            // the bare `%*A` test does, rather than a fully-empty use-set.
            test "`%.*A` (star precision) lowers on the structural engine — no cold path" {
                let _, artifact = compileSource "DepsPrecA" "printfn \"%.*A\" 2 [1; 2; 3]"

                Expect.isFalse
                    (artifact.FSharpCoreDependencies
                     |> Seq.contains "Microsoft.FSharp.Core.PrintfModule.PrintFormatLine")
                    "star-precision %.*A does NOT take the PrintFormatLine cold path"
            }

            // `%-*A` / `%+*A` (flagged star-`%A`) now lower on the structural engine like
            // a bare `%*A`: the `-`/`+` flags are pure no-ops for `%A`, so they take the
            // same runtime column budget (`AppendStructured`), off the cold path. (The
            // runtime-width zero-pad residuals `%0*d` / `%0*A` are diagnosed at the gate —
            // see the front-end PrintfTests — so no valid program routes them cold.)
            test "`%-*A` / `%+*A` (flagged star-%A) lower natively — no cold path" {
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
            // engine instead of the FSharp.Core cold path. The observable proof:
            // `PrintfModule.PrintFormatLine` / `PrintfFormat` no longer appear in the
            // use-set — the whole program pins *no* FSharp.Core construct.
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

            // A `%A` of an EXTERNAL Vesper-package union lowers on the structural
            // engine (its `.Union` resolved shape marks it Vesper-compiled, so it
            // carries the synthesised `Format`), NOT the cold path. `Vesper.Result.dll`
            // is BCL-only, so an engine lowering leaves the whole program free of
            // FSharp.Core. This is the discriminator the stdout-only `runsResult` test
            // can't make: the cold path renders `Ok 5` identically.
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

            // `%A` of an arbitrary BCL type (a `System.Guid`) renders via the
            // dispatcher's `IFormattable` / `ToString` arm — BCL-only, so the whole
            // program pins *no* FSharp.Core construct.
            test "`%A` of a BCL type is BCL-only — no FSharp.Core" {
                let _, artifact = compileSource "DepsBclA" "printfn \"%A\" System.Guid.Empty"

                Expect.isEmpty
                    artifact.FSharpCoreDependencies
                    (sprintf "BCL %%A pins no FSharp.Core (%A)" artifact.FSharpCoreDependencies)
            }

            // A polymorphic `%A` (`let f x = printfn "%A" x`) has a hole whose type is
            // the function's own method typar. `freeze` generalises it to
            // `FTTypar(Method, i)`, the encoder maps it to `!!i`, and `appendStructured`
            // authors `AppendStructured<!!i>` — so codegen emits cleanly (this test
            // throws if the typar can't be authored) and the call rides the engine, not
            // the FSharp.Core cold path.
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
                // The bare `[1;…]` literal builds a Vesper `List`, `List.fold` is
                // emitted inline over it with a `Vesper.Fun` folder, and the `(+)`
                // folder is a `Vesper.Fun` closure. Nothing pins FSharp.Core.
                let src =
                    "let inline sum xs = List.fold (+) 0 xs\nlet nums = [1; 2; 3; 4; 5]\nprintfn \"%d\" (sum nums)"

                let _, artifact = compileSource "DepsFold" src

                Expect.isEmpty
                    artifact.FSharpCoreDependencies
                    (sprintf "the canonical sample pins no FSharp.Core construct (%A)" artifact.FSharpCoreDependencies)
            }

            test "`materialiseApp` omits FSharp.Core.dll for a zero-dependency app, which still runs" {
                let outDir = tmpDir "no-fsharpcore-app"
                // `withCore`: the happy-path `printfn` binds `Vesper.Printf` (and its
                // `Vesper.Core` / `Vesper.List` deps), so their on-disk paths must be
                // resolvable reference sources for the bundle's transitive-closure copy.
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
