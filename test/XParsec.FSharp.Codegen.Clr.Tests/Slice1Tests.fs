module XParsec.FSharp.Codegen.Clr.Tests.Slice1Tests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Thin-slice #1: `printfn "hi"`. The first end-to-end milestone — it proves the
// assembly writer, FSharp.Core resolution, the entry point, the metadata layer,
// the body DSL, and the simplest provider call, with no closure / list / inline
// / arithmetic machinery.

[<Tests>]
let tests =
    testList
        "Slice1"
        [
            test "`printfn \"hi\"` analyses clean and freezes to a Format node" {
                // No-hole literal printf now lowers to the `Vesper.Formatter`
                // happy path (vesper-printf-plan P1): a `TExpr.Format` with a
                // single literal segment and a stdout+newline sink — not the old
                // `App(printfn, New PrintfFormat …)` FSharp.Core shape.
                let tast = analyse "printfn \"hi\""
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match tast.Decls with
                | [ TDecl.Expression(TExpr.Format(sink, segs, _), _) ] ->
                    Expect.equal sink (FormatSink.ToStdOut true) "printfn → stdout with newline"

                    match EqArray.toList segs with
                    | [ FormatSeg.Lit "hi" ] -> ()
                    | other -> failtestf "unexpected Format segments: %A" other
                | other -> failtestf "unexpected slice-1 TAST: %A" other
            }

            test "`printfn \"hi\"` compiles, runs, prints \"hi\", exits 0" {
                let _, artifact = compileSource "Slice1Hi" "printfn \"hi\""
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.stringContains output "hi" "stdout carries the printed text"
                Expect.equal (output.Trim()) "hi" "output is exactly \"hi\" (plus newline)"
            }

            test "`materialise` writes a loadable PE to disk" {
                let tast = analyse "printfn \"hi\""

                let outDir =
                    System.IO.Path.Combine(System.IO.Path.GetTempPath(), "xparsec-codegen-tests")

                System.IO.Directory.CreateDirectory outDir |> ignore
                let outPath = System.IO.Path.Combine(outDir, "Slice1Disk.dll")

                let project =
                    { ProjectInfo.defaults "Slice1Disk" with
                        OutputPath = Some outPath
                    }

                let artifact = Codegen.compile MockBuiltins.provider project tast
                Codegen.materialise artifact

                Expect.isTrue (System.IO.File.Exists outPath) "PE written to disk"

                let bytes = System.IO.File.ReadAllBytes outPath
                let exitCode, output = runEntryPoint bytes
                Expect.equal exitCode 0 "disk PE Main returns 0"
                Expect.equal (output.Trim()) "hi" "disk PE prints hi"
            }
        ]
