module XParsec.FSharp.Codegen.Js.Tests.Step6Tests

open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// codegen-js Step 6 (equality + hashing half) — the structural runtime core.
// The `ops-platform.js.fs` `(=)` / `(<>)` aggregate base calls `structuralEquals`
// and `hash` calls `structuralHash`: non-inline `Vesper.Core` runtime values, so a
// use site lowers to an ordinary external call that rides the standard runtime
// import (`JsImports.addRef`) — `$`-aliased like any other Vesper module function,
// and curried (`f(a)(b)`). `materialise` writes `Vesper.Core.mjs` beside the
// output. Both walkers dispatch on value SHAPE — primitives, tuples (arrays),
// records / unions (objects with a numeric `tag`) — so a value built inline and one
// built by a structural runtime module (the Step 5b list interop) compare and hash
// identically.
//
// Compare / ordering (this commit): the four bare ordering operators (`< > <= >=`)
// resolve through `Vesper.Comparison` — primitives inline, aggregates through the
// imported `structuralCompare` (see `Vesper.Comparison.mjs` for the wiring).
//
// Scope: equality + hashing + compare / ordering. `toString` / `%A` rides the parallel
// Printf track. Golden text for the core-import wiring, plus execution under Node (skips
// when `node` is absent).

[<Tests>]
let tests =
    testList
        "Codegen.Js Step6"
        [
            // ---- golden text: the structural-core import wiring ----

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
                    "$Vesper_StructuralRuntime_structuralEquals(a)(b)"
                    "the aggregate base emits a curried call to the runtime value"
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

            // ---- execution under Node: equality ----

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
                // `List.map` builds plain `{ tag, Head, Tail }` cells in the runtime
                // module; the literal builds `List_Cons` class instances. Structural
                // `equals` ignores the class identity, so they are equal.
                let prog = "printfn \"%b\" (List.map (fun x -> x) [1; 2; 3] = [1; 2; 3])"

                match runJs "step6-list-interop-eq" prog with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "true" "runtime cons cells equal inline cons instances"
            }

            // ---- execution under Node: hashing (consistency with equality) ----

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

            // ---- golden text: the structural-comparator import wiring ----

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

            // ---- execution under Node: ordering ----

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
                // Comparison is opt-in for user types (`[<StructuralComparison>]`);
                // equality is default-on. With the attribute the front end admits the
                // ordering use site and the JS aggregate base routes through
                // `structuralCompare`.
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

            // The load-bearing cross-package contract: `Vesper.Comparison.mjs`'s `cmp`
            // must agree with `Vesper.Core.mjs`'s `eq` — `(x = y)` exactly when
            // `compare x y = 0`, i.e. neither `<` nor `>` holds. Exercised over every
            // emitted shape (tuple, record, union — including the `tag`-first path — and
            // list) so a drift between the two structural walkers fails here.
            test "structural `compare` agrees with structural `=` across every shape" {
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
