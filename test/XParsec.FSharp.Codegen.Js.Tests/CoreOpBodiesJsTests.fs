module XParsec.FSharp.Codegen.Js.Tests.CoreOpBodiesJsTests

open System
open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// The Vesper.Core bodies whose JS form diverges from the CLR one, run under Node rather
// than inspected: `ignore` yields `void`, `box` is the identity, and `invalidArg` keeps
// the argument name only inside the message, every exception erasing to `Error`.

let private lines xs = String.concat "\n" xs

[<Tests>]
let tests =
    testList
        "Codegen.Js CoreOpBodies"
        [
            test "ignore discards the result but still evaluates the operand" {
                let src =
                    lines
                        [
                            "let echo (x: int) : int ="
                            "    printfn \"%d\" x"
                            "    x"
                            ""
                            "ignore (echo 7)"
                            "printfn \"done\""
                        ]

                match runJs "core-ignore" src with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "7\ndone" "the operand's effect happens, its value does not survive"
            }

            test "isNull tests against `null`, and tests it with `===`" {
                let src =
                    lines
                        [
                            "let absent: string | null = Unchecked.defaultof<string | null>"
                            "let present: string | null = \"hi\""
                            "printfn \"%b\" (isNull absent)"
                            "printfn \"%b\" (isNull present)"
                        ]

                // `== null` would also be true for `undefined`, which is a separate
                // type here. The operator is where that choice is visible, so it is read.
                let js = emitJs src
                Expect.stringContains js "=== null" (sprintf "strict, not the nullish `==`, got:\n%s" js)

                match runJs "core-isnull" src with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "true\nfalse" "the JS default of a nullable type IS null"
            }

            test "box is the identity: nothing is emitted around the value" {
                let js = emitJs "let b = box 42\n"

                Expect.stringContains js "const b = 42" (sprintf "box erases entirely, got:\n%s" js)

                match runJs "core-box" "let b = box 42\nprintfn \"%b\" (b = box 42)" with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "true" "a boxed number is still that number"
            }

            test "invalidArg throws an Error carrying both the message and the argument name" {
                let src =
                    lines
                        [
                            "let positive (x: int) : int ="
                            "    if x > 0 then x else invalidArg \"x\" \"must be positive\""
                            ""
                            "printfn \"%d\" (positive 3)"
                            "printfn \"%d\" (positive 0)"
                        ]

                match runJs "core-invalidarg" src with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.notEqual code 0 "the second call throws"
                    Expect.stringContains out "3" "the valid call returned first"

                    Expect.stringContains
                        out
                        "must be positive (Parameter 'x')"
                        "the BCL's own wording, so both targets report the same text"
            }

            test "IntComparison's four operators are the native relational operators" {
                let src =
                    lines
                        [
                            "printfn \"%b\" (IntComparison.(<) 2 3)"
                            "printfn \"%b\" (IntComparison.(>) 2 3)"
                            "printfn \"%b\" (IntComparison.(<=) 3 3)"
                            "printfn \"%b\" (IntComparison.(>=) 2 3)"
                        ]

                match runJs "core-intcomparison" src with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "true\nfalse\ntrue\nfalse" "the four orderings agree with F#"
            }
        ]
