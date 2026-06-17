module XParsec.FSharp.Codegen.Js.Tests.Step3Tests

open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

[<Tests>]
let tests =
    testList
        "Codegen.Js Step3"
        [
            test "a record type emits a class with a positional constructor" {
                Expect.equal
                    (emitJs "type Point = { X: int; Y: int }\nlet p = { X = 7; Y = 9 }")
                    ("class Point {\n"
                     + "  constructor(X, Y) {\n"
                     + "    this.X = X;\n"
                     + "    this.Y = Y;\n"
                     + "  }\n"
                     + "}\n"
                     + "const p = new Point(7, 9);\n")
                    "class declaration precedes its `new` site"
            }

            test "a record literal reorders fields to declaration order" {
                // The constructor is positional in declaration order regardless of source order.
                Expect.equal
                    (emitJs "type Point = { X: int; Y: int }\nlet p = { Y = 9; X = 7 }")
                    ("class Point {\n"
                     + "  constructor(X, Y) {\n"
                     + "    this.X = X;\n"
                     + "    this.Y = Y;\n"
                     + "  }\n"
                     + "}\n"
                     + "const p = new Point(7, 9);\n")
                    "out-of-order literal still constructs in declaration order"
            }

            test "a field access is a plain member expression" {
                Expect.equal
                    (emitJs "type Point = { X: int; Y: int }\nlet p = { X = 7; Y = 9 }\nlet x = p.X")
                    ("class Point {\n"
                     + "  constructor(X, Y) {\n"
                     + "    this.X = X;\n"
                     + "    this.Y = Y;\n"
                     + "  }\n"
                     + "}\n"
                     + "const p = new Point(7, 9);\n"
                     + "const x = p.X;\n")
                    "p.X → p.X"
            }

            test "a copy-update reconstructs new R with copied + overridden fields" {
                Expect.equal
                    (emitJs "type Point = { X: int; Y: int }\nlet p = { X = 1; Y = 2 }\nlet p2 = { p with Y = 99 }")
                    ("class Point {\n"
                     + "  constructor(X, Y) {\n"
                     + "    this.X = X;\n"
                     + "    this.Y = Y;\n"
                     + "  }\n"
                     + "}\n"
                     + "const p = new Point(1, 2);\n"
                     + "const p2 = new Point(p.X, 99);\n")
                    "{ p with Y = 99 } → new Point(p.X, 99)"
            }

            test "record construction + field-get executes (p.X = 7)" {
                match
                    runJs "step3-lit" "type Point = { X: int; Y: int }\nlet p = { X = 7; Y = 9 }\nprintfn \"%d\" p.X"
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "7" "p.X returns the field the literal stored"
            }

            test "field arithmetic over a record executes (p.X + p.Y = 30)" {
                match
                    runJs
                        "step3-arith"
                        "type Point = { X: int; Y: int }\nlet p = { X = 10; Y = 20 }\nprintfn \"%d\" (p.X + p.Y)"
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "30" "both fields read back and add"
            }

            test "copy-update overrides one field and copies the rest (prints 99 then 1)" {
                match
                    runJs
                        "step3-clone"
                        ("type Point = { X: int; Y: int }\n"
                         + "let p = { X = 1; Y = 2 }\n"
                         + "let p2 = { p with Y = 99 }\n"
                         + "printfn \"%d\" p2.Y\n"
                         + "printfn \"%d\" p2.X")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "99\n1" "Y overridden, X copied from source"
            }

            test "a record threaded through a function executes (mk + read = 5)" {
                match
                    runJs
                        "step3-fn"
                        ("type Point = { X: int; Y: int }\n"
                         + "let mk a b = { X = a; Y = b }\n"
                         + "let p = mk 2 3\n"
                         + "printfn \"%d\" (p.X + p.Y)")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "5" "record built inside a curried function"
            }
        ]
