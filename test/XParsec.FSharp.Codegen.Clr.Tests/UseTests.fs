module XParsec.FSharp.Codegen.Clr.Tests.UseTests

open System
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

let private errors (tast: TastFile) =
    tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

// B-5 backend tests. `use x = e in body` lowers to `let x = e in try body
// finally if x <> null then x.Dispose()`: the IL-IR exception region (H5) wraps
// the body, and the binder is disposed on every exit. Under the §3b disposal-model
// flip, a `use` binder must implement `disposable` (`System.IDisposable`) — matching
// real F# — so the project-local mock here implements the interface; its `Dispose`
// records the call by printing. The front end records nothing (`dispose = ValueNone`)
// and codegen disposes through the binder's nominal `Dispose` slot (which resolves the
// interface impl method). Asserting on captured stdout proves both that `Dispose` ran
// and that it ran *after* the body. The external (BCL) binder path resolves a keyed
// `Dispose` (`System.IDisposable`'s, or an own ref-struct `Dispose`) and codegen
// disposes it through an `ExternalMemberRef` `callvirt` — the `MemoryStream` test
// below is the gate.

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
                            "    interface System.IDisposable with"
                            "        member this.Dispose () = printfn \"disposed\""
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

            test "canonical BCL-free `interface disposable` resolves, emits System.IDisposable, and `use` disposes it" {
                // Platform-independence slice 5: authoring the capability by its canonical
                // BCL-free name (`interface disposable`, not `interface System.IDisposable`).
                // Resolves, emits a real `System.IDisposable` interface row (the canon→platform
                // reconciliation in `ClrEnv.externalClassRef`), and `use` recognises + disposes
                // it — identical observable behaviour to the BCL-spelled form.
                let src =
                    String.concat
                        "\n"
                        [
                            "type Res() ="
                            "    interface disposable with"
                            "        member this.Dispose () = printfn \"disposed\""
                            "let run () ="
                            "    use r = Res()"
                            "    printfn \"body\""
                            "run ()"
                        ]

                let tast, artifact = compileSource "UseDisposeCanonical" src
                Expect.isEmpty (errors tast) "no analysis errors: `use` accepts the canonical disposable"
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "body\ndisposed"
                    "body runs, then Dispose() in the finally"
            }

            test "`use _ = e` disposes the binder even though the body can't name it" {
                // A wildcard `use` binder (`use _ = …`) is the RAII-guard form: the
                // value is still parked in a local and disposed in the finally, but
                // the body has no name for it. Codegen keys the slot off a synthetic
                // placeholder (`mintUseBinderKey`); Validation permits `_` as a simple
                // pattern. A regression that rejected it (or crashed the emitter) would
                // fail here.
                let src =
                    String.concat
                        "\n"
                        [
                            "type Res() ="
                            "    interface System.IDisposable with"
                            "        member this.Dispose () = printfn \"disposed\""
                            "let run () ="
                            "    use _ = Res()"
                            "    printfn \"body\""
                            "run ()"
                        ]

                let _, artifact = compileSource "UseWildcard" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "body\ndisposed"
                    "body runs, then Dispose() in the finally — the `_` binder is still disposed"
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
                            "    interface System.IDisposable with"
                            "        member this.Dispose () = printfn \"disposed\""
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

            test "`use` over an external BCL disposable compiles, runs, and disposes it (Step 4.3)" {
                // `System.IO.MemoryStream` declares no `Dispose` of its own — it
                // inherits `Stream.Dispose()` and implements `IDisposable`. The front
                // end therefore resolves the *interface* `Dispose` (the provider's
                // `DeclaredOnly` member walk misses the inherited one), and codegen
                // disposes through an `ExternalMemberRef` `callvirt`. A false
                // "non-disposable" diagnostic or a `MissingMethodException` at the
                // disposal site would fail this; running to a clean exit proves the
                // external disposal path binds and emits.
                let src =
                    String.concat
                        "\n"
                        [
                            "let run () ="
                            "    use s = new System.IO.MemoryStream()"
                            "    printfn \"body\""
                            "run ()"
                        ]

                let _, artifact = compileSource "UseExternalDispose" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "body"
                    "the body runs and the BCL stream is disposed in the finally without faulting"
            }
        ]
