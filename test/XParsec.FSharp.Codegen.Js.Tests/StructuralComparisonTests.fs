module XParsec.FSharp.Codegen.Js.Tests.StructuralComparisonTests

open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

[<Tests>]
let tests =
    testList
        "Codegen.Js Structural Comparison"
        [
            test "an aggregate `<` imports the aliased `structuralCompare` runtime value" {
                let src = emitJs ("printfn \"%b\" ((1, 2) < (1, 3))")

                Expect.stringContains
                    src
                    "import { structuralCompare as $Vesper_ComparisonRuntime_structuralCompare } from \"./Vesper.Comparison/index.mjs\";"
                    "the ordering base imports `structuralCompare`, `$`-aliased like any external value"

                Expect.stringContains
                    src
                    "$Vesper_ComparisonRuntime_structuralCompare"
                    "the aggregate base emits a curried call to the runtime comparator"
            }

            test "a primitive-only `<` stays inline, with NO comparison import" {
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

                match runJs "cmp-prim" prog with
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

                match runJs "cmp-tuple" prog with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "true\nfalse\ntrue\ntrue" "first element decides, then the second"
            }

            test "record ordering compares fields in declaration order" {
                // `[<StructuralComparison>]` is required because ordering is not default for user types.
                let prog =
                    "[<StructuralComparison>] type Point = { X: int; Y: int }\n"
                    + "printfn \"%b\" ({ X = 1; Y = 2 } < { X = 1; Y = 3 })\n"
                    + "printfn \"%b\" ({ X = 2; Y = 0 } > { X = 1; Y = 9 })\n"
                    + "printfn \"%b\" ({ X = 1; Y = 2 } >= { X = 1; Y = 2 })"

                match runJs "cmp-record" prog with
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

                match runJs "cmp-union" prog with
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

                match runJs "cmp-list" prog with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "true\ntrue\ntrue" "elementwise, then a shorter prefix orders first"
            }

            test "structural `compare` agrees with structural `=` across every shape" {
                // `compare x y = 0` must agree with `x = y`, so drift between the two walkers fails here.
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

                match runJs "cmp-eq-agree" prog with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "true\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue" "`cmp` = 0 iff `eq`, every shape"
            }
        ]
