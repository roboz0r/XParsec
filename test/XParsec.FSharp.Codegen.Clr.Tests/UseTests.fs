module XParsec.FSharp.Codegen.Clr.Tests.UseTests

open System
open Expecto
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// vesper-set-sprint-phase-4 Step 4.1 / B-5 backend tests. `use x = e in body`
// lowers to `let x = e in try body finally if x <> null then x.Dispose()`: the
// IL-IR exception region (H5) wraps the body, and the binder is disposed on
// every exit. v1 disposes via a direct `Dispose()` call on the binder (the
// looser type-check — no `IDisposable` upcast, Phase 5 not required), so the
// mock here is a plain user class with a `Dispose` member that records the call
// by printing. Asserting on captured stdout proves both that `Dispose` ran and
// that it ran *after* the body.

[<Tests>]
let useTests =
    testList
        "Use"
        [
            test "`use` disposes the binder after the body runs" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type Res() ="
                            "    member this.Dispose () = printfn \"disposed\""
                            "let run () ="
                            "    use r = Res()"
                            "    printfn \"body\""
                            "run ()"
                        ]

                let _, artifact = compileSource "UseDispose" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "body\ndisposed"
                    "body runs, then Dispose() in the finally"
            }

            test "the body's result survives the finally and is the `use` expression's value" {
                // `compute ()` returns the body value (42); `Dispose` still runs in
                // the finally before the return, so the parked result is reloaded
                // after the region. Disposal prints first, then the caller prints 42.
                let src =
                    String.concat
                        "\n"
                        [
                            "type Res() ="
                            "    member this.Dispose () = printfn \"disposed\""
                            "let compute () ="
                            "    use r = Res()"
                            "    42"
                            "printfn \"%d\" (compute ())"
                        ]

                let _, artifact = compileSource "UseResult" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "disposed\n42"
                    "Dispose() runs in the finally; the body's 42 is reloaded as the result"
            }
        ]
