module XParsec.FSharp.Codegen.Js.Tests.Step8Tests

open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

[<Tests>]
let tests =
    testList
        "Codegen.Js Step8"
        [
            test "a constructed exception lowers to `new Error(message)` (exn repr from contract)" {
                let src =
                    emitJs "let f (b: bool) = if b then 7 else raise (InvalidOperationException \"boom\")"

                Expect.stringContains src "new Error(" "the exception lowers to `new Error` (exn → Error)"
            }

            test "a different exception type also resolves through the contract chain" {
                // Every exception in the contract erases to the one `exn` root — not a per-name special case.
                let src =
                    emitJs "let f (b: bool) = if b then 7 else raise (ArgumentException \"bad\")"

                Expect.stringContains src "new Error(" "ArgumentException erases to the `exn` root too"
                Expect.isFalse (src.Contains "ArgumentException") "the BCL name does not leak into the output"
            }

            test "a constructed exception throws with its message under Node (uncaught → non-zero)" {
                match
                    runJs
                        "step8-raise-notsupported"
                        ("let f (b: bool) = if b then 7 else raise (System.NotSupportedException \"nope\")\n"
                         + "printfn \"%d\" (f false)")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.notEqual code 0 "an uncaught throw exits non-zero"
                    Expect.stringContains out "nope" "the native Error carries the exception's message"
            }

            test "a caught exception is a JS Error carrying the message" {
                // The thrown value is a genuine `Error`, so `catch (e)` reads `e.message`.
                match
                    runJs
                        "step8-catch-error"
                        ("let f (b: bool) = if b then 7 else raise (InvalidOperationException \"kaboom\")\n"
                         + "let g () = (# \"(() => { try { return $0; } catch (e) { console.log(e.message); return 0; } })()\" (f false) : int #)\n"
                         + "printfn \"%d\" (g ())")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 "the caught throw lets the program exit zero"
                    Expect.stringContains out "kaboom" "the caught Error carries the message"
            }
        ]
