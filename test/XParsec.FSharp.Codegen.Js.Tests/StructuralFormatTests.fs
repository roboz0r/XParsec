module XParsec.FSharp.Codegen.Js.Tests.StructuralFormatTests

open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// `%A` (structural formatting) of primitives, tuples, and lists.
//
// The record / union / option `%A` output forms are owned by the recipe-derived
// cross-target differential (`StructuralFormatRecipeTests`), which renders the
// expectation straight from `recordRecipe`/`unionCaseRecipe`. This file keeps only
// the non-recipe shapes (primitives, tuples, lists) and the mixed-format integration.
[<Tests>]
let tests =
    testList
        "Codegen.Js %A Structural Formatting"
        [
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

                match runJs "fmt-prim" prog with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "5\ntrue\n\"hi\"\n5L" "number / bool / quoted string / bigint with L suffix"
            }

            test "`%A` renders tuples and lists in Vesper source form" {
                let prog =
                    "printfn \"%A\" (1, 2)\nprintfn \"%A\" [1; 2; 3]\nprintfn \"%A\" ([]: int list)"

                match runJs "fmt-collections" prog with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)

                    Expect.equal
                        out
                        "(1, 2)\n[1; 2; 3]\n[]"
                        "tuple parens; list brackets with `; ` separators; empty list"
            }

            test "`%A` works in a mixed format with literal text and other holes" {
                let prog = "printfn \"x = %A, n = %d\" (Some 3) 7"

                match runJs "fmt-mixed" prog with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)

                    Expect.equal
                        out
                        "x = Some 3, n = 7"
                        "the structural hole concatenates with literal text and a `%d` hole"
            }
        ]
