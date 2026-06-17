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

            test "`printfn \"% A\"` (cold path) pins PrintfModule + PrintfFormat" {
                // A faithful `%A` (int / list / …) now lowers to the structural engine,
                // as do the `%.NA` / `%+A` / `%-A` flag forms. The one still-deferred
                // `%A` flag — the space flag `% A` — rides the FSharp.Core cold path,
                // so it remains the pin that proves the cold recipes still work.
                let _, artifact = compileSource "DepsColdPrintf" "printfn \"% A\" 42"
                let deps = artifact.FSharpCoreDependencies
                Expect.isNonEmpty deps "the % A cold path depends on FSharp.Core"

                Expect.contains
                    deps
                    "Microsoft.FSharp.Core.PrintfModule.PrintFormatLine"
                    "the cold path calls PrintFormatLine"

                Expect.contains deps "Microsoft.FSharp.Core.PrintfFormat`4 (.ctor)" "and constructs a PrintfFormat"
            }

            // P3 step 4 — the cut. A project-local record / DU now carries a
            // synthesised `IStructuralFormattable.Format`, so `%A` of one lowers on
            // the structural engine instead of the FSharp.Core cold path. The
            // observable proof of the cut: `PrintfModule.PrintFormatLine` /
            // `PrintfFormat` no longer appear in the use-set — and, the record / DU
            // being pure Vesper, the whole program pins *no* FSharp.Core construct.
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

            // The gate widening: a `%A` of an EXTERNAL Vesper-package union lowers on
            // the structural engine (its `.Union` resolved shape marks it Vesper-
            // compiled, so it carries the synthesised `Format`), NOT the cold path.
            // `Vesper.Result.dll` is BCL-only, so an engine lowering leaves the whole
            // program free of FSharp.Core — in particular `PrintFormatLine` is absent
            // (it would be pinned had `%A` fallen back to the reflective cold path).
            // This is the discriminator the stdout-only `runsResult` test can't make:
            // the cold path renders `Ok 5` identically.
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

            test "a list literal pins FSharpList" {
                let _, artifact =
                    compileSource "DepsList" "let nums = [1; 2; 3]\nprintfn \"%A\" nums"

                let deps = artifact.FSharpCoreDependencies
                Expect.contains deps "Microsoft.FSharp.Collections.FSharpList`1" "list type"
                Expect.contains deps "Microsoft.FSharp.Collections.FSharpList`1.Cons" "cons constructor"
                Expect.contains deps "Microsoft.FSharp.Collections.FSharpList`1.get_Empty" "nil getter"
            }

            test "List.fold over a bare-program list is BCL-only + Vesper, no FSharp.Core (R3)" {
                // The canonical sample: the bare `[1;…]` literal flips onto the Vesper
                // `List` (driven by `List.fold`'s Vesper-list parameter), `List.fold`
                // is emitted inline over it with a `Vesper.Fun` folder, and the `(+)`
                // folder is a `Vesper.Fun` closure (R1). Nothing pins FSharp.Core — R3
                // cut the last three pins (`ListModule.Fold`, its `FSharpFunc` folder,
                // the `FSharpList` argument).
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
                // resolvable reference sources for the bundle's transitive-closure copy
                // (printf-port-steps.md step 3 — no host fallback ships the handler now).
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
