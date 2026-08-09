module XParsec.FSharp.Codegen.Clr.Tests.UseTests

open System
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

let private errors (tast: TastFile) = tast.Diagnostics |> Diagnostic.errors

// `use x = e in body` lowers to `let x = e in try body finally x.Dispose()`, the finally
// being an IL-IR exception region around the body. Each test asserts on captured stdout:
// "body" then "disposed" proves `Dispose` ran, and that it ran after the body.

[<Tests>]
let useTests =
    testList
        "Use"
        [
            test "`use` disposes the bound variable after the body runs" {
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
                // `interface disposable` is the canonical BCL-free spelling; the canon→platform
                // reconciliation emits a real `System.IDisposable` interface row.
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

            test "`use _ = e` disposes the bound variable even though the body can't name it" {
                // `use _ = …` is the RAII-guard form: the value is still parked in a local
                // and disposed in the finally, keyed off a synthetic placeholder.
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
                    "body runs, then Dispose() in the finally — the `_` bound variable is still disposed"
            }

            test "the body's result survives the finally and is the `use` expression's value" {
                // `Dispose` runs in the finally before the return and the parked result is
                // reloaded after the region, so "disposed" precedes the caller's 42.
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

            test "`use` over an external BCL disposable compiles, runs, and disposes it" {
                // `System.IO.MemoryStream` declares no `Dispose` of its own but inherits
                // `Stream.Dispose()`, so the front end resolves the *interface* `Dispose`
                // and codegen disposes through an `ExternalMemberRef` `callvirt`.
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
