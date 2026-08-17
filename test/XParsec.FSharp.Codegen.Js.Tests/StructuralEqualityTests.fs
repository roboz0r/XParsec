module XParsec.FSharp.Codegen.Js.Tests.StructuralEqualityTests

open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

[<Tests>]
let tests =
    testList
        "Codegen.Js Structural Equality & Hashing"
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
                    "import { structuralEquals as $Vesper_StructuralRuntime_structuralEquals } from \"./Vesper.Core/index.mjs\";"
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
                    "import { structuralHash as $Vesper_StructuralRuntime_structuralHash } from \"./Vesper.Core/index.mjs\";"
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

                match runJs "eq-record" prog with
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

                match runJs "eq-union" prog with
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

                match runJs "eq-tuple" prog with
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

                match runJs "eq-option" prog with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "true\nfalse\nfalse" "Some carries a value field; None is a bare tag"
            }

            test "list equality recurses the cons spine" {
                let prog =
                    "printfn \"%b\" ([1; 2; 3] = [1; 2; 3])\nprintfn \"%b\" ([1; 2; 3] = [1; 2])"

                match runJs "eq-list" prog with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "true\nfalse" "structural over tag/Head/Tail down to the Empty terminal"
            }

            test "inline-built and runtime-built lists compare equal (list interop)" {
                // Structural `equals` dispatches on shape, not class identity.
                let prog = "printfn \"%b\" (List.map (fun x -> x) [1; 2; 3] = [1; 2; 3])"

                match runJs "eq-list-interop" prog with
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

                match runJs "hash-record" prog with
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

                match runJs "hash-aggregate" prog with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "true\ntrue\ntrue" "structural hash agrees across each aggregate shape"
            }

            test "inline-built and runtime-built lists hash equal (list interop)" {
                let prog =
                    "printfn \"%b\" (hash (List.map (fun x -> x) [1; 2; 3]) = hash [1; 2; 3])"

                match runJs "hash-list-interop" prog with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "true" "shape-based hash ignores cons-cell vs class identity"
            }
        ]
