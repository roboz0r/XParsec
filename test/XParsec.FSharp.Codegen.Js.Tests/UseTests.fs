module XParsec.FSharp.Codegen.Js.Tests.UseTests

open System
open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// JS-backend `use` lowering. `use x = e in body` lowers to `const x = e; try { body }
// finally { if (x != null) x[Symbol.dispose](); }` — the JS analogue of the IL exception
// region the CLR backend emits. Under the §3b disposal-model flip, a `use` binder must
// implement `disposable` (`System.IDisposable`) — matching real F# — so the disposable
// here implements the interface. Its `Dispose` emits as a NATIVE `[Symbol.dispose]()`
// method (the JS analogue of the CLR `IDisposable::Dispose` slot), and `use` disposes
// through `x[Symbol.dispose]()`, NOT a mangled `<Type>__Dispose` free fn. Asserting on
// captured stdout proves both that disposal ran and that it ran *after* the body.

// A project-local disposable: a class implementing `System.IDisposable` whose `Dispose`
// prints when run.
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
                // The disposable's `Dispose` impl emits as a native `[Symbol.dispose]()`
                // method on the class.
                Expect.stringContains js "[Symbol.dispose]() {" "emits the disposer as a native [Symbol.dispose] method"
                // `use` disposes through `binder[Symbol.dispose]()` — the native well-known
                // symbol slot — not a mangled `<Type>__Dispose` free fn.
                Expect.stringContains js "r[Symbol.dispose]()" "disposes via the native Symbol.dispose member call"
                Expect.isFalse (js.Contains "Res__Dispose") "no mangled <Type>__Dispose free fn is emitted"
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

                Expect.stringContains
                    js
                    "[Symbol.dispose]()"
                    "the wildcard `_use<tok>` binder is still disposed via Symbol.dispose"

                match runJs "js-use-wildcard" src with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "body\ndisposed" "body runs, then disposal — the `_` binder is still disposed"
            }

            test "the body's result survives the finally and is the `use` expression's value" {
                // `compute ()` returns the body value (42); disposal still runs in the
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
                    Expect.equal out "disposed\n42" "disposal runs in the finally; the body's 42 is the result"
            }
        ]
