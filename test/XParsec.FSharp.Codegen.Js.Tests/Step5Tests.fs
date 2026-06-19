module XParsec.FSharp.Codegen.Js.Tests.Step5Tests

open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

[<Tests>]
let tests =
    testList
        "Codegen.Js Step5"
        [
            test "a tuple is a JS array; a tupled module-function group flattens to flat params" {
                Expect.equal
                    (emitJs "let p = (1, 2)\nlet f (a, b) = a + b\nprintfn \"%d\" (f p)")
                    ("const p = [1, 2];\n"
                     + "const f = (a, b) => (((a) + (b)) | 0);\n"
                     + "console.log(f(p[0], p[1]));\n")
                    "tuple → array literal; a tupled group flattens to N flat params, the tuple-value call flattening to positional reads"
            }

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

            test "Some imports the case class from the Option runtime module" {
                Expect.equal
                    (emitJs "let x = Some 5")
                    ("import { Option_Some as $Vesper_Option_Option_Some } from \"./Vesper.Option.mjs\";\n"
                     + "const x = new $Vesper_Option_Option_Some(5);\n")
                    "external Option `Some` → import the case class from its home module, then `new` it (no local re-emit)"
            }

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
