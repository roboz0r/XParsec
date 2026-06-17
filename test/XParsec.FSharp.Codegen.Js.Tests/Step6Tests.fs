module XParsec.FSharp.Codegen.Js.Tests.Step6Tests

open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

[<Tests>]
let tests =
    testList
        "Codegen.Js Step6"
        [
            test "aggregate `=` imports the aliased `structuralEquals` runtime value" {
                let src =
                    emitJs (
                        "type Point = { X: int; Y: int }\n"
                        + "let a = { X = 1; Y = 2 }\n"
                        + "let b = { X = 1; Y = 2 }\n"
                        + "printfn \"%b\" (a = b)"
                    )

                Expect.stringContains
                    src
                    "import { structuralEquals as $Vesper_StructuralRuntime_structuralEquals } from \"./Vesper.Core.mjs\";"
                    "the equality base imports `structuralEquals`, `$`-aliased like any external value"

                Expect.stringContains
                    src
                    "$Vesper_StructuralRuntime_structuralEquals(a, b)"
                    "the aggregate base emits a flat call to the runtime value"
            }

            test "`hash` imports the aliased `structuralHash` runtime value" {
                let src =
                    emitJs ("type Point = { X: int; Y: int }\nlet a = { X = 1; Y = 2 }\nprintfn \"%d\" (hash a)")

                Expect.stringContains
                    src
                    "import { structuralHash as $Vesper_StructuralRuntime_structuralHash } from \"./Vesper.Core.mjs\";"
                    "`hash` lowers to a call to the `structuralHash` runtime value + its import"

                Expect.stringContains
                    src
                    "$Vesper_StructuralRuntime_structuralHash(a)"
                    "the hash base emits the runtime call"
            }

            test "a primitive-only `=` pulls in NO core import (`===` stays inline)" {
                let src = emitJs "printfn \"%b\" (2 = 2)"
                Expect.isFalse (src.Contains "Vesper.Core.mjs") "int `=` lowers to `===`, never the structural base"
            }

            test "record equality is structural" {
                let prog =
                    "type Point = { X: int; Y: int }\n"
                    + "let a = { X = 1; Y = 2 }\n"
                    + "let b = { X = 1; Y = 2 }\n"
                    + "let c = { X = 1; Y = 3 }\n"
                    + "printfn \"%b\" (a = b)\n"
                    + "printfn \"%b\" (a = c)\n"
                    + "printfn \"%b\" (a <> c)"

                match runJs "step6-record-eq" prog with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "true\nfalse\ntrue" "equal fields ⇒ equal; differing ⇒ unequal; `<>` negates"
            }

            test "union equality discriminates by tag and fields" {
                let prog =
                    "type Shape = Circle of int | Rect of int * int | Dot\n"
                    + "printfn \"%b\" (Circle 5 = Circle 5)\n"
                    + "printfn \"%b\" (Circle 5 = Circle 6)\n"
                    + "printfn \"%b\" (Rect(3, 4) = Rect(3, 4))\n"
                    + "printfn \"%b\" (Circle 5 = Dot)"

                match runJs "step6-union-eq" prog with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)

                    Expect.equal
                        out
                        "true\nfalse\ntrue\nfalse"
                        "same case+fields ⇒ equal; differing field/tag ⇒ unequal"
            }

            test "tuple equality is elementwise" {
                let prog = "printfn \"%b\" ((1, 2) = (1, 2))\nprintfn \"%b\" ((1, 2) = (1, 3))"

                match runJs "step6-tuple-eq" prog with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "true\nfalse" "tuples (JS arrays) compare position-wise"
            }

            test "option equality is structural (Some/None)" {
                let prog =
                    "printfn \"%b\" (Some 5 = Some 5)\n"
                    + "printfn \"%b\" (Some 5 = Some 6)\n"
                    + "printfn \"%b\" (Some 5 = None)"

                match runJs "step6-option-eq" prog with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "true\nfalse\nfalse" "Some carries a value field; None is a bare tag"
            }

            test "list equality recurses the cons spine" {
                let prog =
                    "printfn \"%b\" ([1; 2; 3] = [1; 2; 3])\nprintfn \"%b\" ([1; 2; 3] = [1; 2])"

                match runJs "step6-list-eq" prog with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "true\nfalse" "structural over tag/Head/Tail down to the Empty terminal"
            }

            test "inline-built and runtime-built lists compare equal (Step 5b interop)" {
                // Structural `equals` dispatches on shape, not class identity.
                let prog = "printfn \"%b\" (List.map (fun x -> x) [1; 2; 3] = [1; 2; 3])"

                match runJs "step6-list-interop-eq" prog with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "true" "runtime cons cells equal inline cons instances"
            }

            test "equal records hash equal" {
                let prog =
                    "type Point = { X: int; Y: int }\n"
                    + "let a = { X = 1; Y = 2 }\n"
                    + "let b = { X = 1; Y = 2 }\n"
                    + "printfn \"%b\" (hash a = hash b)"

                match runJs "step6-record-hash" prog with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "true" "equal ⇒ equal hash (the only hash guarantee)"
            }

            test "equal unions / tuples / lists hash equal" {
                let prog =
                    "type Shape = Circle of int | Rect of int * int\n"
                    + "printfn \"%b\" (hash (Circle 5) = hash (Circle 5))\n"
                    + "printfn \"%b\" (hash (1, 2) = hash (1, 2))\n"
                    + "printfn \"%b\" (hash [1; 2; 3] = hash [1; 2; 3])"

                match runJs "step6-aggregate-hash" prog with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "true\ntrue\ntrue" "structural hash agrees across each aggregate shape"
            }

            test "inline-built and runtime-built lists hash equal (Step 5b interop)" {
                let prog =
                    "printfn \"%b\" (hash (List.map (fun x -> x) [1; 2; 3]) = hash [1; 2; 3])"

                match runJs "step6-list-interop-hash" prog with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "true" "shape-based hash ignores cons-cell vs class identity"
            }

            test "an aggregate `<` imports the aliased `structuralCompare` runtime value" {
                let src = emitJs ("printfn \"%b\" ((1, 2) < (1, 3))")

                Expect.stringContains
                    src
                    "import { structuralCompare as $Vesper_ComparisonRuntime_structuralCompare } from \"./Vesper.Comparison.mjs\";"
                    "the ordering base imports `structuralCompare`, `$`-aliased like any external value"

                Expect.stringContains
                    src
                    "$Vesper_ComparisonRuntime_structuralCompare"
                    "the aggregate base emits a curried call to the runtime comparator"
            }

            test "a primitive-only `<` pulls in NO comparison import (`<` stays inline)" {
                let src = emitJs "printfn \"%b\" (2 < 3)"

                Expect.isFalse
                    (src.Contains "Vesper.Comparison.mjs")
                    "int `<` lowers to a direct JS `<`, never the structural base"
            }

            test "primitive ordering is the direct JS relational operator" {
                let prog =
                    "printfn \"%b\" (2 < 3)\n"
                    + "printfn \"%b\" (3 < 2)\n"
                    + "printfn \"%b\" (3 > 2)\n"
                    + "printfn \"%b\" (3 <= 3)\n"
                    + "printfn \"%b\" (2 >= 3)"

                match runJs "step6-prim-cmp" prog with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "true\nfalse\ntrue\ntrue\nfalse" "each operator is its JS counterpart"
            }

            test "tuple ordering is lexicographic (structural)" {
                let prog =
                    "printfn \"%b\" ((1, 2) < (1, 3))\n"
                    + "printfn \"%b\" ((1, 3) < (1, 2))\n"
                    + "printfn \"%b\" ((2, 0) > (1, 9))\n"
                    + "printfn \"%b\" ((1, 2) <= (1, 2))"

                match runJs "step6-tuple-cmp" prog with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "true\nfalse\ntrue\ntrue" "first element decides, then the second"
            }

            test "record ordering compares fields in declaration order" {
                // `[<StructuralComparison>]` is required — ordering is not default for user types.
                let prog =
                    "[<StructuralComparison>] type Point = { X: int; Y: int }\n"
                    + "printfn \"%b\" ({ X = 1; Y = 2 } < { X = 1; Y = 3 })\n"
                    + "printfn \"%b\" ({ X = 2; Y = 0 } > { X = 1; Y = 9 })\n"
                    + "printfn \"%b\" ({ X = 1; Y = 2 } >= { X = 1; Y = 2 })"

                match runJs "step6-record-cmp" prog with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "true\ntrue\ntrue" "X dominates Y; equal fields ⇒ `>=` holds"
            }

            test "union ordering compares the case tag before fields" {
                let prog =
                    "[<StructuralComparison>] type Shape = Circle of int | Rect of int * int\n"
                    + "printfn \"%b\" (Circle 9 < Rect(0, 0))\n"
                    + "printfn \"%b\" (Circle 5 < Circle 6)\n"
                    + "printfn \"%b\" (Rect(3, 4) > Rect(3, 2))"

                match runJs "step6-union-cmp" prog with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "true\ntrue\ntrue" "lower case index orders first; then fields"
            }

            test "list ordering recurses the cons spine" {
                let prog =
                    "printfn \"%b\" ([1; 2; 3] < [1; 2; 4])\n"
                    + "printfn \"%b\" ([1; 2] < [1; 2; 3])\n"
                    + "printfn \"%b\" ([1; 2; 3] > [1; 2])"

                match runJs "step6-list-cmp" prog with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "true\ntrue\ntrue" "elementwise, then a shorter prefix orders first"
            }

            test "a `%A` hole imports the aliased `structuralFormat` runtime value" {
                let src = emitJs "printfn \"%A\" [1; 2; 3]"

                Expect.stringContains
                    src
                    "import { structuralFormat as $Vesper_StructuralPrinter_structuralFormat } from \"./Vesper.Printf.mjs\";"
                    "the `%A` hole imports `structuralFormat` from Vesper.Printf (Printf owns `%A`), `$`-aliased"

                Expect.stringContains
                    src
                    "$Vesper_StructuralPrinter_structuralFormat"
                    "the hole emits a call to the runtime formatter, curried over (value)(width)(size)"
            }

            test "`%A` renders primitives as Vesper source atoms" {
                let prog =
                    "printfn \"%A\" 5\n"
                    + "printfn \"%A\" true\n"
                    + "printfn \"%A\" \"hi\"\n"
                    + "printfn \"%A\" 5L"

                match runJs "step6-percenta-prim" prog with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "5\ntrue\n\"hi\"\n5L" "number / bool / quoted string / bigint with L suffix"
            }

            test "`%A` renders tuples and lists in Vesper source form" {
                let prog =
                    "printfn \"%A\" (1, 2)\nprintfn \"%A\" [1; 2; 3]\nprintfn \"%A\" ([]: int list)"

                match runJs "step6-percenta-collections" prog with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)

                    Expect.equal
                        out
                        "(1, 2)\n[1; 2; 3]\n[]"
                        "tuple parens; list brackets with `; ` separators; empty list"
            }

            test "`%A` renders options structurally (Some/None, nested parens)" {
                let prog =
                    "printfn \"%A\" (Some 3)\n"
                    + "printfn \"%A\" (None: int option)\n"
                    + "printfn \"%A\" (Some (Some 3))"

                match runJs "step6-percenta-option" prog with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)

                    Expect.equal
                        out
                        "Some 3\nNone\nSome (Some 3)"
                        "single-payload application; nullary bare; nested arg parens"
            }

            test "`%A` renders records flat in declaration order" {
                let prog = "type Point = { X: int; Y: int }\nprintfn \"%A\" { X = 1; Y = 2 }"

                match runJs "step6-percenta-record" prog with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "{ X = 1; Y = 2 }" "single-line `{ F = v; … }`, fields in declaration order"
            }

            test "`%A` renders unions: nullary bare, single payload, multi-field tuple" {
                let prog =
                    "type Shape = Circle of int | Rect of int * int | Dot\n"
                    + "printfn \"%A\" (Circle 5)\n"
                    + "printfn \"%A\" (Rect(3, 4))\n"
                    + "printfn \"%A\" Dot"

                match runJs "step6-percenta-union" prog with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "Circle 5\nRect (3, 4)\nDot" "`Case arg`; `Case (a, b)`; bare nullary"
            }

            test "`%A` works in a mixed format with literal text and other holes" {
                let prog = "printfn \"x = %A, n = %d\" (Some 3) 7"

                match runJs "step6-percenta-mixed" prog with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)

                    Expect.equal
                        out
                        "x = Some 3, n = 7"
                        "the structural hole concatenates with literal text and a `%d` hole"
            }

            test "structural `compare` agrees with structural `=` across every shape" {
                // `compare x y = 0` must agree with `x = y` — drift between the two walkers fails here.
                let prog =
                    "[<StructuralComparison>] type Point = { X: int; Y: int }\n"
                    + "[<StructuralComparison>] type Shape = Circle of int | Rect of int * int\n"
                    + "let inline agree x y = (x = y) = (not (x < y) && not (x > y))\n"
                    + "printfn \"%b\" (agree (1, 2) (1, 2))\n"
                    + "printfn \"%b\" (agree (1, 2) (1, 3))\n"
                    + "printfn \"%b\" (agree { X = 1; Y = 2 } { X = 1; Y = 2 })\n"
                    + "printfn \"%b\" (agree { X = 1; Y = 2 } { X = 1; Y = 3 })\n"
                    + "printfn \"%b\" (agree (Circle 5) (Circle 5))\n"
                    + "printfn \"%b\" (agree (Circle 5) (Rect(0, 0)))\n"
                    + "printfn \"%b\" (agree [1; 2] [1; 2])\n"
                    + "printfn \"%b\" (agree [1; 2] [1; 3])"

                match runJs "step6-cmp-eq-agree" prog with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "true\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue" "`cmp` = 0 iff `eq`, every shape"
            }
        ]
