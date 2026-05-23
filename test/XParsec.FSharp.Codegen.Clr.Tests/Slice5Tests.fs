module XParsec.FSharp.Codegen.Clr.Tests.Slice5Tests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Thin-slice #5 — closure synthesis, culminating in the roadmap's canonical
// sample:
//
//   let inline sum xs = List.fold (+) 0 xs
//   let nums = [1; 2; 3; 4; 5]
//   printfn "%d" (sum nums)
//
// A function value at runtime is an `FSharpFunc\`2` instance, so this is the
// first slice that *emits new types*: one synthesised closure class per lambda
// (and per eta-reified operator-as-value), each a subclass of the instantiated
// `FSharpFunc\`2<a,b>` with a virtual `Invoke` override carrying the lowered
// body, free variables captured as instance fields. The milestones isolate the
// mechanics — non-capturing, capturing, curried/eta — before composing them
// with the non-inline generic `List.fold` call.

[<Tests>]
let tests =
    testList
        "Slice5"
        [
            // ---- Milestone 1: non-capturing closure ----

            test "`let f = fun x -> x + 1` / `printfn \"%d\" (f 41)` prints 42 (one closure type, no captures)" {
                let _, artifact =
                    compileSource "Slice5NonCapturing" "let f = fun x -> x + 1\nprintfn \"%d\" (f 41)"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "42" "closure constructed, stored, reloaded, and Invoked"
            }

            // ---- Milestone 2: capturing closure ----

            test "`let g = fun x -> x + n` captures n and prints 51 (a FieldDefinition + ldfld)" {
                let _, artifact =
                    compileSource "Slice5Capturing" "let n = 10\nlet g = fun x -> x + n\nprintfn \"%d\" (g 41)"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "51" "the captured n is read back via ldfld in Invoke"
            }

            // ---- Milestone 3: curried / eta-reified operator-as-value ----

            test "`let add = (+)` analyses clean to an External op_Addition value" {
                let tast = analyse "let add = (+)\nprintfn \"%d\" (add 40 2)"
                Expect.isEmpty tast.Diagnostics "no diagnostics — (+) resolves as a value"

                match tast.Decls with
                | [ TDecl.Let(TPat.NamedSimple(kAdd, _), TExpr.External("op_Addition", _), false, _)
                    TDecl.Expression(TExpr.Format(FormatSink.ToStdOut true, segs, _), _) ] ->
                    // `printfn "%d" (add 40 2)` lowers to a `%d` hole whose arg is
                    // the curried `add 40 2` (eta-reified later, in `Emit.lower`).
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

            // ---- Milestone 4: the full canonical sample ----

            let fullSample =
                "let inline sum xs = List.fold (+) 0 xs\nlet nums = [1; 2; 3; 4; 5]\nprintfn \"%d\" (sum nums)"

            test "the full sample analyses clean (inline sum + List.fold (+) over a list literal)" {
                let tast = analyse fullSample
                Expect.isEmpty tast.Diagnostics "no diagnostics"
            }

            test "the full sample compiles, runs in-process, prints 15" {
                let _, artifact = compileSource "Slice5FullSample" fullSample
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "15" "List.fold (+) 0 [1..5] = 15"
            }

            test "the full sample runs as a standalone `dotnet <dll>` app and prints 15" {
                let outDir = tmpDir "slice5-full-sample"
                let project = ProjectInfo.app "XParsecFoldApp" outDir

                let artifact = compileSourceTo project fullSample
                Codegen.materialiseApp project artifact

                let dllPath = System.IO.Path.Combine(outDir, "XParsecFoldApp.dll")
                let exitCode, output = runOnDisk dllPath

                Expect.equal exitCode 0 (sprintf "dotnet exits 0 (output was: %s)" output)
                Expect.equal (output.Trim()) "15" "the canonical sample runs as a real assembly"
            }
        ]
