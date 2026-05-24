module XParsec.FSharp.Codegen.Clr.Tests.Slice5Tests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

[<Tests>]
let tests =
    testList
        "Slice5"
        [
            test "`let f = fun x -> x + 1` / `printfn \"%d\" (f 41)` prints 42 (one closure type, no captures)" {
                let _, artifact =
                    compileSource "Slice5NonCapturing" "let f = fun x -> x + 1\nprintfn \"%d\" (f 41)"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "42" "closure constructed, stored, reloaded, and Invoked"
            }

            test "`let g = fun x -> x + n` captures n and prints 51 (a FieldDefinition + ldfld)" {
                let _, artifact =
                    compileSource "Slice5Capturing" "let n = 10\nlet g = fun x -> x + n\nprintfn \"%d\" (g 41)"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "51" "the captured n is read back via ldfld in Invoke"
            }

            test "`let add = (+)` analyses clean to an External op_Addition value" {
                let tast = analyse "let add = (+)\nprintfn \"%d\" (add 40 2)"
                Expect.isEmpty tast.Diagnostics "no diagnostics — (+) resolves as a value"

                match tast.Decls with
                | [ TDecl.Let(TPat.NamedSimple(kAdd, _), TExpr.External("op_Addition", _), false, _)
                    TDecl.Expression(TExpr.Format(FormatSink.ToStdOut true, segs, _), _) ] ->
                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(_,
                                       TExpr.App(TExpr.App(TExpr.Var(kUse, _), TExpr.Const(TConstValue.Int 40, _), _),
                                                 TExpr.Const(TConstValue.Int 2, _),
                                                 _)) ] ->
                        Expect.equal kUse kAdd "the call site references the (+) binding"
                    | other -> failtestf "unexpected slice-5 M3 segments: %A" other
                | other -> failtestf "unexpected slice-5 M3 TAST: %A" other
            }

            test "`let add = (+)` / `printfn \"%d\" (add 40 2)` prints 42 (nested closures + eta-reification)" {
                let _, artifact =
                    compileSource "Slice5Eta" "let add = (+)\nprintfn \"%d\" (add 40 2)"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "42" "outer closure newobjs the inner; Invoke().Invoke() runs op_Addition"
            }

            let fullSample =
                "let inline sum xs = List.fold (+) 0 xs\nlet nums = [1; 2; 3; 4; 5]\nprintfn \"%d\" (sum nums)"

            test "the full sample analyses clean (inline sum + List.fold (+) over a list literal)" {
                let tast = analyse fullSample
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            // R3 (docs/selfhost-handoff.md): the bare-program list literal +
            // `List.fold` now retarget onto the Vesper `List` — the literal builds a
            // `Vesper.Collections.List` and `List.fold` is emitted inline over it
            // (`Vesper.Fun` folder, `IsEmpty`/`Head`/`Tail`), so the canonical sample
            // is BCL-only + `Vesper.Core`, no FSharp.Core.
            test "the full sample compiles, runs in-process, prints 15" {
                let _, artifact = compileSource "Slice5FullSample" fullSample

                Expect.isEmpty
                    artifact.FSharpCoreDependencies
                    (sprintf "the canonical sample pins no FSharp.Core (%A)" artifact.FSharpCoreDependencies)

                // Acceptance (minimal-core-lib-plan + package-split-plan PS2): the
                // emitted PE references Vesper.Core (Fun) AND Vesper.List (the cons-
                // list, its own package now) and carries no Microsoft.FSharp.*
                // AssemblyRef.
                let asm = loadAssembly (Codegen.toBytes artifact)
                let refs = asm.GetReferencedAssemblies() |> Array.map (fun a -> a.Name)
                Expect.contains refs "Vesper.Core" "references Vesper.Core (Fun)"
                Expect.contains refs "Vesper.List" "references Vesper.List (the cons-list)"

                Expect.isFalse
                    (refs |> Array.exists (fun n -> n = "FSharp.Core"))
                    (sprintf "no FSharp.Core AssemblyRef (refs: %A)" refs)

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "15" "List.fold (+) 0 [1..5] = 15"
            }

            test "the full sample runs as a standalone `dotnet <dll>` app and prints 15" {
                let outDir = tmpDir "slice5-full-sample"
                // `withCore` here too, so the *same* project (carrying `VesperCorePath`
                // + `VesperListPath`) drives both the compile and `materialiseApp`'s
                // copy of Vesper.Core.dll + Vesper.List.dll beside the app — the bundle
                // the out-of-process loader needs.
                let project = withCore (ProjectInfo.app "XParsecFoldApp" outDir)

                let artifact = compileSourceTo project fullSample
                Codegen.materialiseApp project artifact

                let dllPath = System.IO.Path.Combine(outDir, "XParsecFoldApp.dll")
                let exitCode, output = runOnDisk dllPath

                Expect.equal exitCode 0 (sprintf "dotnet exits 0 (output was: %s)" output)
                Expect.equal (output.Trim()) "15" "the canonical sample runs as a real assembly"
            }
        ]
