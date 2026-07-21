module XParsec.FSharp.Codegen.Js.Tests.TryFinallyTests

open System
open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// JS-backend `try body finally cleanup`. In statement position it maps straight onto JS
// `try { body } finally { cleanup }`; in expression position it wraps in a zero-arg IIFE
// that `return`s the body's value from the `try` (the same shape the `use` desugaring
// uses). `cleanup` runs on every exit — normal completion or a `throw` unwind. Asserting
// on captured stdout proves the ordering and the exception path.

[<Tests>]
let tryFinallyTests =
    testList
        "Codegen.Js TryFinally"
        [
            test "`try`/`finally` lowers to a JS try/finally" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let run () ="
                            "    try"
                            "        printfn \"body\""
                            "    finally"
                            "        printfn \"cleanup\""
                            "run ()"
                        ]

                let js = emitJs src
                Expect.stringContains js "try " "wraps the body in a try"
                Expect.stringContains js "finally " "runs the cleanup in a finally"
            }

            test "the finally runs after the body on normal completion" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let run () ="
                            "    try"
                            "        printfn \"body\""
                            "    finally"
                            "        printfn \"cleanup\""
                            "run ()"
                        ]

                match runJs "js-tryfinally-order" src with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "body\ncleanup" "the body runs, then the cleanup in the finally"
            }

            test "the body's result survives the finally and is the expression's value" {
                // `compute ()` returns the body value (42) from inside the `try`; the finally
                // still runs before the value is consumed — cleanup prints first, then the
                // caller prints 42. The expression-position IIFE `return`s the parked result
                // past the `finally`.
                let src =
                    String.concat
                        "\n"
                        [
                            "let compute () ="
                            "    try"
                            "        42"
                            "    finally"
                            "        printfn \"cleanup\""
                            "printfn \"%d\" (compute ())"
                        ]

                match runJs "js-tryfinally-result" src with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "cleanup\n42" "the finally runs, then the body's 42 is the result"
            }

            test "the finally runs when the body throws, and the throw still propagates" {
                // The finally must run on the exception path too. `failwith` lowers to a
                // throwing IIFE; the finally's console.log flushes to stdout during unwind,
                // then the uncaught throw exits non-zero. Stdout therefore carries both the
                // body and the cleanup lines even though the program crashes.
                let src =
                    String.concat
                        "\n"
                        [
                            "let run () ="
                            "    try"
                            "        printfn \"body\""
                            "        (failwith \"boom\" : unit)"
                            "    finally"
                            "        printfn \"cleanup\""
                            "run ()"
                        ]

                match runJs "js-tryfinally-throw" src with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.notEqual code 0 "an uncaught throw exits non-zero"
                    Expect.stringContains out "body" "the body ran"
                    Expect.stringContains out "cleanup" "the finally ran during the throw unwind"
            }
        ]
