module XParsec.FSharp.Codegen.Clr.Tests.PrintfPartialTests

open Vesper
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// A fully-unapplied literal partial (`printfn "%d"`) becomes a synthesised heap closure
// `fun h -> Format(sink, [Hole h])`.

[<Tests>]
let tests =
    testList
        "PrintfPartial"
        [
            test "`printfn \"%d\"` lowers to a synthesised closure over a Format node" {
                let tast = analyse "let p = printfn \"%d\""
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match tast.Decls with
                | EqList [ TDecl.Let({
                                         Value = TExpr.Lambda(_, TExpr.Format(sink, segs, _, _), _, _)
                                     },
                                     _,
                                     _) ] ->
                    Expect.equal sink (FormatSink.ToStdOut true) "printfn → stdout with newline"

                    match Block.toList segs with
                    | [ FormatSeg.Hole(_, TExpr.Var _) ] -> ()
                    | other -> failtestf "expected a single Hole reading a Var param, got: %A" other
                | other -> failtestf "expected a let bound to a Lambda-over-Format, got: %A" other
            }

            // Nothing inlines a single-use `let`, so `p` really is the synthesised
            // closure and `p 3` invokes it.
            test "`let p = printfn \"%d\" in p 3` prints 3" {
                printsUnder TrailingNewlines "PPartialPrintfn" "3" "let p = printfn \"%d\"\np 3"
            }

            // `printf` (no trailing newline) so the assertion pins the exact bytes.
            test "`let p = printf \"%d %s\" in p 3 \"x\"` prints \"3 x\"" {
                printsUnder TrailingNewlines "PPartialPrintf" "3 x" "let p = printf \"%d %s\"\np 3 \"x\""
            }

            test "`let f = sprintf \"n=%d\" in printfn \"%s\" (f 7)` prints \"n=7\"" {
                printsUnder TrailingNewlines "PPartialSprintf" "n=7" "let f = sprintf \"n=%d\"\nprintfn \"%s\" (f 7)"
            }

            // The closure's `Invoke` is the Vesper format unroll dispatched through
            // `Vesper.Fun`, so nothing references `PrintfModule` / `PrintfFormat`.
            test "a lowered `printfn \"%d\"` partial references no FSharp.Core construct" {
                let artifact = compileSource "DepsPartialPrintf" "let p = printfn \"%d\"\np 3"

                expectNoFSharpCore artifact "the printf partial is pure Vesper"
            }
        ]
