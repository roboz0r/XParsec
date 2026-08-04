module XParsec.FSharp.Codegen.Clr.Tests.PrintfPartialTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Printf partial application, sub-step 4a: a *fully-unapplied* lowerable literal
// partial (`printfn "%d"`) is synthesised into a Vesper closure
// `fun h -> Format(sink, …)` — emitted on the ordinary (heap) closure path and
// dispatched via `Fun`2`::Invoke` — instead of falling back to the FSharp.Core
// `PrintfFormat` cold path. Not yet the zero-alloc value struct (4c); the win here
// is correctness + dropping the FSharp.Core dependency. Any shape 4a does not mark
// (an `%A`/`%O` hole, a within-chunk partial, `fprintf`) keeps the existing path.

let private runPrints (src: string) (expected: string) =
    let exitCode, output = withPrintfAlc (fun alc -> runDriverInAlc alc src)
    Expect.equal exitCode 0 (sprintf "Main returns 0 for: %s" src)
    Expect.equal (output.TrimEnd('\r', '\n')) expected (sprintf "%s prints %s" src expected)

[<Tests>]
let tests =
    testList
        "PrintfPartial"
        [
            // The synthesis shape: a fully-unapplied `printfn "%d"` freezes to a
            // closure `fun h -> Format(stdout+nl, [Hole h])` — a `Lambda` whose body
            // is a `Format` node, NOT an `App` of the FSharp.Core printf.
            test "`printfn \"%d\"` lowers to a synthesised closure over a Format node" {
                let tast = analyse "let p = printfn \"%d\""
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match tast.Decls with
                | EqList [ TDecl.Let(_, TExpr.Lambda(_, TExpr.Format(sink, segs, _, _), _, _), _, _) ] ->
                    Expect.equal sink (FormatSink.ToStdOut true) "printfn → stdout with newline"

                    match EqArray.toList segs with
                    | [ FormatSeg.Hole(_, TExpr.Var _) ] -> ()
                    | other -> failtestf "expected a single Hole reading a Var param, got: %A" other
                | other -> failtestf "expected a let bound to a Lambda-over-Format, got: %A" other
            }

            // Acceptance: bind the partial to a name, then apply it — the escape
            // caveat says there is no single-use-let inlining, so `p` really is the
            // synthesised closure and `p 3` invokes it. Prints `3`.
            test "`let p = printfn \"%d\" in p 3` prints 3" { runPrints "let p = printfn \"%d\"\np 3" "3" }

            // Multi-hole partial (arity 2), applied in the same scope. `printf` (no
            // trailing newline) so the parity check pins the exact bytes.
            test "`let p = printf \"%d %s\" in p 3 \"x\"` prints \"3 x\"" {
                runPrints "let p = printf \"%d %s\"\np 3 \"x\"" "3 x"
            }

            // `sprintf` partial: the string-result sink flows through the same
            // synthesis; applying it yields the formatted string, which we print.
            test "`let f = sprintf \"n=%d\" in printfn \"%s\" (f 7)` prints \"n=7\"" {
                runPrints "let f = sprintf \"n=%d\"\nprintfn \"%s\" (f 7)" "n=7"
            }

            // The headline 4a win: the partial no longer pins FSharp.Core — its
            // `Invoke` is the Vesper `EmitFormat` unroll and dispatch is `Vesper.Fun`,
            // so nothing references `PrintfModule`/`PrintfFormat`.
            test "a lowered `printfn \"%d\"` partial references no FSharp.Core construct" {
                let _, artifact = compileSource "DepsPartialPrintf" "let p = printfn \"%d\"\np 3"

                Expect.isEmpty
                    artifact.FSharpCoreDependencies
                    (sprintf
                        "the printf partial is pure Vesper — no FSharp.Core dependency (%A)"
                        artifact.FSharpCoreDependencies)
            }
        ]
