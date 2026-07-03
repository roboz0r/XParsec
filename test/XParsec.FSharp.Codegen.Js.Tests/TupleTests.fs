module XParsec.FSharp.Codegen.Js.Tests.TupleTests

open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

[<Tests>]
let tests =
    testList
        "Codegen.Js Tuples"
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
                match runJs "tuple-add" "let f (a, b) = a + b\nprintfn \"%d\" (f (2, 3))" with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "5" "destructured tuple elements add"
            }

            test "a tuple pattern in match binds positionally (first/second)" {
                match
                    runJs
                        "tuple-match"
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
                match runJs "tuple-nested" "let f ((a, b), c) = a + b + c\nprintfn \"%d\" (f ((1, 2), 3))" with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "6" "nested tuple destructuring [[a, b], c]"
            }

            // Tuples are unbounded; the JS backend already represents every tuple as
            // a flat array, so an 8-element tuple needs no `TRest` nesting — just a
            // longer array literal with positional reads. Regression guard.
            test "an 8-element tuple is a flat 8-slot JS array (no nesting)" {
                Expect.equal
                    (emitJs
                        "let p = (1, 2, 3, 4, 5, 6, 7, 8)\nlet f (a, b, c, d, e, g, h, i) = a + i\nprintfn \"%d\" (f p)")
                    ("const p = [1, 2, 3, 4, 5, 6, 7, 8];\n"
                     + "const f = (a, b, c, d, e, g, h, i) => (((a) + (i)) | 0);\n"
                     + "console.log(f(p[0], p[1], p[2], p[3], p[4], p[5], p[6], p[7]));\n")
                    "8-tuple → flat 8-slot array; the tupled group flattens to 8 positional reads"
            }

            test "an 8-tuple round-trips: destructure all eight (sum)" {
                match
                    runJs
                        "tuple8"
                        "let f (a, b, c, d, e, g, h, i) = a+b+c+d+e+g+h+i\nprintfn \"%d\" (f (1, 2, 3, 4, 5, 6, 7, 8))"
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "36" "all eight array slots add (1+..+8)"
            }

            test "a 15-tuple round-trips (would be double Rest-nested on CLR; flat on JS)" {
                match
                    runJs
                        "tuple15"
                        ("let f (a,b,c,d,e,g,h,i,j,k,l,m,n,o,p) = a+b+c+d+e+g+h+i+j+k+l+m+n+o+p\n"
                         + "printfn \"%d\" (f (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "120" "all fifteen array slots add (1+..+15)"
            }
        ]
