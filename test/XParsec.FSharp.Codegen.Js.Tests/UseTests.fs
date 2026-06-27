module XParsec.FSharp.Codegen.Js.Tests.UseTests

open System
open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// JS-backend `use` lowering. `use x = e in body` lowers to `const x = e; try { body }
// finally { if (x != null) x.Dispose(); }` — the JS analogue of the IL exception
// region the CLR backend emits. A *project-local* binder disposes via a direct
// `x.Dispose()` (the duck-typed path — no `IDisposable` upcast), so the disposable
// here is a plain user class with a `Dispose` member that records the call by
// printing. Asserting on captured stdout proves both that `Dispose` ran and that it
// ran *after* the body. (The external/keyed `Symbol.dispose` path is a later slice;
// this arm calls the named `Dispose` member directly.)

// A project-local disposable: a class with a `Dispose` member that prints when run.
let private disposable = "type Res() =\n    member this.Dispose () = printfn \"disposed\""

[<Tests>]
let useTests =
    testList
        "Codegen.Js Use"
        [
            test "`use` lowers to a `try`/`finally` that calls `.Dispose()`" {
                let src =
                    String.concat
                        "\n"
                        [
                            disposable
                            "let run () ="
                            "    use r = Res()"
                            "    printfn \"body\""
                            "run ()"
                        ]

                let js = emitJs src
                Expect.stringContains js "try " "wraps the body in a try"
                Expect.stringContains js "finally " "disposes in a finally"
                // A project-local member is a free receiver-first fn, so disposal is
                // `Res__Dispose(r)`, not an attached `r.Dispose()`.
                Expect.stringContains js "Dispose(r)" "calls the binder's mangled Dispose free fn"
            }

            test "`use` disposes the binder after the body runs" {
                let src =
                    String.concat
                        "\n"
                        [
                            disposable
                            "let run () ="
                            "    use r = Res()"
                            "    printfn \"body\""
                            "run ()"
                        ]

                match runJs "js-use-dispose" src with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "body\ndisposed" "body runs, then Dispose() in the finally"
            }

            test "`use _ = e` disposes the binder even though the body can't name it" {
                // A wildcard `use` binder (`use _ = …`, the RAII-guard form): the value is
                // still parked in a fresh `_use<tok>` local and disposed in the finally, but
                // the body has no name for it. A regression that crashed the emitter on the
                // nameless binder (or skipped disposal) would fail here.
                let src =
                    String.concat
                        "\n"
                        [
                            disposable
                            "let run () ="
                            "    use _ = Res()"
                            "    printfn \"body\""
                            "run ()"
                        ]

                let js = emitJs src
                Expect.stringContains js "finally " "the wildcard binder still gets a finally"
                Expect.stringContains js "Dispose(_use" "the wildcard `_use<tok>` binder is still disposed"

                match runJs "js-use-wildcard" src with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "body\ndisposed" "body runs, then Dispose() — the `_` binder is still disposed"
            }

            test "the body's result survives the finally and is the `use` expression's value" {
                // `compute ()` returns the body value (42); `Dispose` still runs in the
                // finally before the value is returned — disposal prints first, then the
                // caller prints 42. The IIFE arm `return`s the body value from inside the
                // `try`, so the parked result survives the `finally`.
                let src =
                    String.concat
                        "\n"
                        [
                            disposable
                            "let compute () ="
                            "    use r = Res()"
                            "    42"
                            "printfn \"%d\" (compute ())"
                        ]

                match runJs "js-use-result" src with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "disposed\n42" "Dispose() runs in the finally; the body's 42 is the result"
            }
        ]
