module XParsec.FSharp.Codegen.Js.Tests.Step1Tests

open System
open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// codegen-js Step 1 — scalars + control flow + the `ILIntrinsic` `$N`-template
// path, all going through the relocated `TastLower.lower` (JS `finishOps`). The
// operator templates are spliced pre-freeze from `ops-platform.js.fs` (Step F1):
// a ground `2 + 2` freezes to `ILIntrinsic("($0 + $1) | 0", …)`, which the
// **(a\*)** `JsRaw` path emits with universal parenthesization. Golden-text plus
// execution under Node (the latter skips when `node` is absent).

[<Tests>]
let tests =
    testList
        "Codegen.Js Step1"
        [
            // ---- golden text: the template path's universal parenthesization ----

            test "int32 `+` emits the masked `| 0` template, operands parenthesised" {
                // `($0 + $1) | 0` with operands `2`,`2`; (a*) wraps the whole node
                // and each operand in parens.
                Expect.equal
                    (emitJs "printfn \"%d\" (2 + 2)")
                    "console.log((((2) + (2)) | 0));\n"
                    "the emitted ESM source"
            }

            test "a top-level `let` becomes a `const`; its use resolves to the same name" {
                Expect.equal
                    (emitJs "let x = 2 + 3\nprintfn \"%d\" x")
                    "const x = (((2) + (3)) | 0);\nconsole.log(x);\n"
                    "const binding + reference"
            }

            test "`if … then … else` emits a parenthesised ternary" {
                Expect.equal
                    (emitJs "printfn \"%d\" (if true then 1 else 2)")
                    "console.log((true ? 1 : 2));\n"
                    "conditional expression"
            }

            // ---- execution under Node ----

            test "int32 arithmetic executes (2 + 2 = 4)" {
                match runJs "step1-add" "printfn \"%d\" (2 + 2)" with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "4" "prints 4"
            }

            test "int32 multiply wraps (Math.imul: 100000 * 100000 = 1410065408)" {
                match runJs "step1-imul" "printfn \"%d\" (100000 * 100000)" with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "1410065408" "int32 wrapping via Math.imul"
            }

            test "float arithmetic executes (1.5 + 2.0 = 3.5, no truncation)" {
                match runJs "step1-float" "printfn \"%f\" (1.5 + 2.0)" with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "3.5" "float add (printf width/precision deferred)"
            }

            test "int equality executes (2 = 2 → true)" {
                match runJs "step1-eq" "printfn \"%b\" (2 = 2)" with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "true" "strict === over int32"
            }

            test "conditional executes (if true then 1 else 2 → 1)" {
                match runJs "step1-if" "printfn \"%d\" (if true then 1 else 2)" with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "1" "ternary picks the consequent"
            }

            test "a `let` binding + reference executes (x = 2 + 3 → 5)" {
                match runJs "step1-let" "let x = 2 + 3\nprintfn \"%d\" x" with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "5" "const binding round-trips"
            }

            test "int64 add emits the BigInt-wrap template over BigInt literals" {
                // int64 `Const`s are `bigint` literals (`2n`); the `+` selects the
                // `BigInt.asIntN(64, …)` clause. Observing the *value* under Node
                // needs print/conversion infrastructure not in Step 1 (`%d` is
                // typed int32; `=` on int64 falls to the un-bootstrapped structural
                // `equals`), so Step 1 checks the emitted template.
                Expect.equal
                    (emitJs "let x = 2L + 3L")
                    "const x = (BigInt.asIntN(64, (2n) + (3n)));\n"
                    "int64 BigInt-wrapped add"
            }
        ]
