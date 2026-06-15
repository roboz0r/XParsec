module XParsec.FSharp.Codegen.Js.Tests.Step5Tests

open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// codegen-js Step 5 — tuples + Option (List is tracked separately: see
// codegen-js-steps.md Step 5). A tuple `(a, b)` is a JS array `[a, b]`; a tupled
// lambda parameter (`fun (a, b) -> …`) becomes an array-destructuring arrow param
// (`([a, b]) => …`); a tuple *pattern* indexes the array positionally
// (`scrut[0]`). `Option` is an *external* union the file references but does not
// declare — its case shapes (`None` tag 0, `Some` tag 1 with a `Value` field) are
// read off the symbol provider and emitted as honest nominal JS classes (the same
// base-class + per-case-subclass shape a local union gets), so `Some`/`None`
// construction and match ride the unchanged Step-4 union path. Golden-text plus
// execution under Node.

[<Tests>]
let tests =
    testList
        "Codegen.Js Step5"
        [
            // ---- tuples: golden text ----

            test "a tuple is a JS array; a tupled param destructures it" {
                Expect.equal
                    (emitJs "let p = (1, 2)\nlet f (a, b) = a + b\nprintfn \"%d\" (f p)")
                    ("const p = [1, 2];\n"
                     + "const f = ([a, b]) => (((a) + (b)) | 0);\n"
                     + "console.log(f(p));\n")
                    "tuple → array literal, tupled param → array destructuring"
            }

            // ---- tuples: execution under Node ----

            test "a tupled function adds its destructured elements (f (2,3) → 5)" {
                match runJs "step5-tuple-add" "let f (a, b) = a + b\nprintfn \"%d\" (f (2, 3))" with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "5" "destructured tuple elements add"
            }

            test "a tuple pattern in match binds positionally (first/second)" {
                match
                    runJs
                        "step5-tuple-match"
                        ("let f t =\n"
                         + "    match t with\n"
                         + "    | (0, y) -> y\n"
                         + "    | (x, _) -> x\n"
                         + "printfn \"%d\" (f (0, 9))\n"
                         + "printfn \"%d\" (f (7, 3))")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "9\n7" "tuple pattern indexes elements; constant element refutes"
            }

            test "a nested tuple param destructures recursively (((a,b),c) → a+b+c)" {
                match runJs "step5-tuple-nested" "let f ((a, b), c) = a + b + c\nprintfn \"%d\" (f ((1, 2), 3))" with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "6" "nested tuple destructuring [[a, b], c]"
            }

            // ---- Option: golden text ----

            test "Option emits as honest nominal classes (None tag 0, Some tag 1)" {
                Expect.equal
                    (emitJs "let x = Some 5")
                    ("class Option {\n"
                     + "  constructor(tag) {\n"
                     + "    this.tag = tag;\n"
                     + "  }\n"
                     + "  cases() {\n"
                     + "    return [\"None\", \"Some\"];\n"
                     + "  }\n"
                     + "}\n"
                     + "class Option_None extends Option {\n"
                     + "  constructor() {\n"
                     + "    super(0);\n"
                     + "  }\n"
                     + "}\n"
                     + "class Option_Some extends Option {\n"
                     + "  constructor(Value) {\n"
                     + "    super(1);\n"
                     + "    this.Value = Value;\n"
                     + "  }\n"
                     + "}\n"
                     + "const x = new Option_Some(5);\n")
                    "external Option union → base class + None/Some subclasses, then the `new` site"
            }

            // ---- Option: execution under Node ----

            test "Some binds its value in a match (Some 5 → 5)" {
                match
                    runJs
                        "step5-option-some"
                        ("let x = Some 5\n"
                         + "match x with\n"
                         + "| Some n -> printfn \"%d\" n\n"
                         + "| None -> printfn \"%d\" 0")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "5" "Some arm binds the Value field"
            }

            test "None matches its tag (None → 0)" {
                match
                    runJs
                        "step5-option-none"
                        ("let x : int option = None\n"
                         + "match x with\n"
                         + "| Some n -> printfn \"%d\" n\n"
                         + "| None -> printfn \"%d\" 0")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "0" "None matches tag 0"
            }

            test "Option threads through a function (toInt)" {
                match
                    runJs
                        "step5-option-fn"
                        ("let toInt o =\n"
                         + "    match o with\n"
                         + "    | Some n -> n\n"
                         + "    | None -> -1\n"
                         + "printfn \"%d\" (toInt (Some 42))\n"
                         + "printfn \"%d\" (toInt None)")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "42\n-1" "Some/None both route through the match"
            }
        ]
