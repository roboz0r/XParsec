module XParsec.FSharp.Codegen.Js.Tests.Step8Tests

open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// codegen-js Step 8 — `exn` as a JS-native root + the common BCL exceptions as Vesper
// *contract* types. The whole JS suite now resolves against a BCL-free provider
// (`JsNativeSymbols.buildJsNativeContractFor`, TestHelpers), so these tests double as the
// "without host metadata" proof: `System.InvalidOperationException` & co. resolve through
// the `Vesper.Exceptions` contract's `inherit exn` chain, not host reflection. `exn` binds
// to the native `Error` (`prim-types-exn.js.fs`, harvested in preference to the base
// `prim-types-exn.fs`'s `System.Exception`), and a constructed exception lowers to
// `new Error(message)` — its repr resolved through the provider to the `Error` class
// (`exnReprOf`), no `EndsWith "Exception"` name heuristic, no hardcoded `"Error"`.

[<Tests>]
let tests =
    testList
        "Codegen.Js Step8"
        [
            test "a constructed exception lowers to `new Error(message)` (exn repr from contract)" {
                // `InvalidOperationException` is a `Vesper.Exceptions` contract type
                // inheriting `exn`; with no host metadata, its `:> exn` argument to
                // `raise` reconciles through that contract chain, and `exn`'s JS repr
                // (`Error`) is what the construction emits.
                let src =
                    emitJs "let f (b: bool) = if b then 7 else raise (InvalidOperationException \"boom\")"

                Expect.stringContains src "new Error(" "the exception lowers to `new Error` (exn → Error)"
            }

            test "a different exception type also resolves through the contract chain" {
                // Not just `InvalidOperationException`: every exception in the contract
                // set erases to the one `exn` root. Proves the lowering is base-chain
                // driven, not a per-name special case.
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
                // The thrown value is a genuine `Error`, so a JS `catch (e)` reads
                // `e.message` — the structural-interop the runtime modules rely on.
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
