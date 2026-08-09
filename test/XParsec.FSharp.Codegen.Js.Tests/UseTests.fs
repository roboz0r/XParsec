module XParsec.FSharp.Codegen.Js.Tests.UseTests

open System
open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// `use x = e in body` lowers to `const x = e; try { body } finally { if (x != null)
// x[Symbol.dispose](); }`. A `use` bound variable must implement `disposable`
// (`System.IDisposable`), whose `Dispose` emits as a native `[Symbol.dispose]()` method.

let private disposable =
    "type Res() =\n    interface System.IDisposable with\n        member this.Dispose () = printfn \"disposed\""

[<Tests>]
let useTests =
    testList
        "Codegen.Js Use"
        [
            test "`use` lowers to a `try`/`finally` that calls `[Symbol.dispose]()`" {
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
                Expect.stringContains js "[Symbol.dispose]() {" "emits the disposer as a native [Symbol.dispose] method"
                Expect.stringContains js "r[Symbol.dispose]()" "disposes via the native Symbol.dispose member call"
                Expect.isFalse (js.Contains "Res__Dispose") "no mangled <Type>__Dispose free fn is emitted"
            }

            test "`use` disposes the bound variable after the body runs" {
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
                    Expect.equal out "body\ndisposed" "body runs, then [Symbol.dispose]() in the finally"
            }

            test "canonical BCL-free `interface disposable` emits [Symbol.dispose] and `use` disposes it under Node" {
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

                let js = emitJs src

                Expect.stringContains
                    js
                    "[Symbol.dispose]() {"
                    "canonical disposable emits a native [Symbol.dispose] method"

                Expect.stringContains js "r[Symbol.dispose]()" "use disposes via Symbol.dispose"

                match runJs "js-use-dispose-canonical" src with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "body\ndisposed" "canonical interface disposable runs under Node"
            }

            test "`use _ = e` disposes the bound variable even though the body can't name it" {
                // `use _ = …`, the RAII-guard form: the value is still parked in a fresh
                // `_use<tok>` local and disposed in the finally, but the body has no name
                // for it.
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
                Expect.stringContains js "finally " "the wildcard bound variable still gets a finally"

                Expect.stringContains
                    js
                    "[Symbol.dispose]()"
                    "the wildcard `_use<tok>` bound variable is still disposed via Symbol.dispose"

                match runJs "js-use-wildcard" src with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)

                    Expect.equal
                        out
                        "body\ndisposed"
                        "body runs, then disposal — the `_` bound variable is still disposed"
            }

            test "the body's result survives the finally and is the `use` expression's value" {
                // The IIFE arm `return`s the body value from inside the `try`, so 42
                // survives the `finally`. Disposal prints first, then the caller prints 42.
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
                    Expect.equal out "disposed\n42" "disposal runs in the finally; the body's 42 is the result"
            }
        ]
