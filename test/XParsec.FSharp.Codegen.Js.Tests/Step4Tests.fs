module XParsec.FSharp.Codegen.Js.Tests.Step4Tests

open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// codegen-js Step 4 — discriminated unions + match. A union `type` becomes a JS
// base class (an integer `tag` + a `cases()` returning the case names) plus one
// `extends`-subclass per case carrying its declaration-order fields (positional
// fields synthesised as `Item` / `Item1` / `Item2`). `UnionCons` (`Circle 5`) →
// `new Shape_Circle(5)`. A `match` lowers to an IIFE that tests each arm in order
// (`scrut.tag === N`, `&&`-conjoined with nested tests; constant arms compare the
// value; a guard gates the `return`) and `return`s the first match's body — an
// unmatched value `throw`s. Golden-text plus execution under Node.

[<Tests>]
let tests =
    testList
        "Codegen.Js Step4"
        [
            // ---- golden text ----

            test "a union type emits a base class + one subclass per case" {
                Expect.equal
                    (emitJs "type Shape = Circle of int | Rect of int * int | Dot\nlet c = Circle 5")
                    ("class Shape {\n"
                     + "  constructor(tag) {\n"
                     + "    this.tag = tag;\n"
                     + "  }\n"
                     + "  cases() {\n"
                     + "    return [\"Circle\", \"Rect\", \"Dot\"];\n"
                     + "  }\n"
                     + "}\n"
                     + "class Shape_Circle extends Shape {\n"
                     + "  constructor(Item) {\n"
                     + "    super(0);\n"
                     + "    this.Item = Item;\n"
                     + "  }\n"
                     + "}\n"
                     + "class Shape_Rect extends Shape {\n"
                     + "  constructor(Item1, Item2) {\n"
                     + "    super(1);\n"
                     + "    this.Item1 = Item1;\n"
                     + "    this.Item2 = Item2;\n"
                     + "  }\n"
                     + "}\n"
                     + "class Shape_Dot extends Shape {\n"
                     + "  constructor() {\n"
                     + "    super(2);\n"
                     + "  }\n"
                     + "}\n"
                     + "const c = new Shape_Circle(5);\n")
                    "base class + tagged subclasses, then the `new` site"
            }

            // ---- execution under Node ----

            test "construction + match on a field-carrying case (Circle 5 → 5)" {
                match
                    runJs
                        "step4-circle"
                        ("type Shape = Circle of int | Rect of int * int | Dot\n"
                         + "let area s =\n"
                         + "    match s with\n"
                         + "    | Circle r -> r\n"
                         + "    | Rect (w, h) -> w * h\n"
                         + "    | Dot -> 0\n"
                         + "printfn \"%d\" (area (Circle 5))")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "5" "Circle arm binds and returns the field"
            }

            test "a multi-field case destructures positionally (Rect 3 4 → 12)" {
                match
                    runJs
                        "step4-rect"
                        ("type Shape = Circle of int | Rect of int * int | Dot\n"
                         + "let area s =\n"
                         + "    match s with\n"
                         + "    | Circle r -> r\n"
                         + "    | Rect (w, h) -> w * h\n"
                         + "    | Dot -> 0\n"
                         + "printfn \"%d\" (area (Rect(3, 4)))")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "12" "Rect binds Item1/Item2 and multiplies"
            }

            test "a nullary case matches by tag (Dot → 0)" {
                match
                    runJs
                        "step4-dot"
                        ("type Shape = Circle of int | Rect of int * int | Dot\n"
                         + "let area s =\n"
                         + "    match s with\n"
                         + "    | Circle r -> r\n"
                         + "    | Rect (w, h) -> w * h\n"
                         + "    | Dot -> 0\n"
                         + "printfn \"%d\" (area Dot)")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "0" "nullary Dot matches its tag"
            }

            test "a guarded arm falls through on guard failure (A 0 → 100, A 7 → 7)" {
                match
                    runJs
                        "step4-guard"
                        ("type T = A of int | B\n"
                         + "let f x =\n"
                         + "    match x with\n"
                         + "    | A n when n = 0 -> 100\n"
                         + "    | A n -> n\n"
                         + "    | B -> 99\n"
                         + "printfn \"%d\" (f (A 0))\n"
                         + "printfn \"%d\" (f (A 7))\n"
                         + "printfn \"%d\" (f B)")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "100\n7\n99" "guard picks the first arm only when it holds"
            }

            test "a nested union pattern matches inner tag (Wrap Two → 2)" {
                match
                    runJs
                        "step4-nested"
                        ("type Inner = One | Two\n"
                         + "type Outer = Wrap of Inner | Empty\n"
                         + "let g x =\n"
                         + "    match x with\n"
                         + "    | Wrap One -> 1\n"
                         + "    | Wrap Two -> 2\n"
                         + "    | Empty -> 0\n"
                         + "printfn \"%d\" (g (Wrap Two))")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "2" "Wrap Two short-circuits to the inner tag test"
            }

            test "constant + wildcard arms on a scalar scrutinee (1 → 20, 5 → 99)" {
                match
                    runJs
                        "step4-const"
                        ("let h n =\n"
                         + "    match n with\n"
                         + "    | 0 -> 10\n"
                         + "    | 1 -> 20\n"
                         + "    | _ -> 99\n"
                         + "printfn \"%d\" (h 1)\n"
                         + "printfn \"%d\" (h 5)")
                with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "20\n99" "constant arms compare, wildcard catches the rest"
            }
        ]
