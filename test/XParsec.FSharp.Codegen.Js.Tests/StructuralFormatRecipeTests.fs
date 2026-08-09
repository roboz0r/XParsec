module XParsec.FSharp.Codegen.Js.Tests.StructuralFormatRecipeTests

open Expecto
open XParsec.FSharp.Codegen.Common.StructuralFormatRecipe
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// Expected record/union strings are rendered from the `StructuralFormatRecipe` grammar;
// actual ones come from running the JS runtime, which rebuilds that grammar dynamically
// rather than reading it. No call path ties them, so this differential is what does.

// The never-break layout the JS runtime always produces, modelled here so the expected
// string can be derived from the recipe alone.
let private renderFlat (ops: SinkOp list) (child: int -> string) (arg: int -> string) : string =
    ops
    |> List.map (fun op ->
        match op with
        | Text s -> s
        | Line -> " "
        | SoftBreak -> ""
        | BeginGroup
        | EndGroup
        | BeginNest _
        | EndNest
        | BeginApplication
        | EndApplication -> ""
        | FormatChild i -> child i
        | FormatArg i -> arg i
    )
    |> String.concat ""

// Only the shapes the recipe owns. Tuple/list/atom grammar has no recipe form, so it is
// not modelled here.
type private V =
    | Prim of string
    | Record of (string * V) list
    | Union of name: string * args: V list

/// A payload-bearing union parenthesises in argument position: `Some (Some 3)`.
let rec private renderArg (v: V) : string =
    match v with
    | Union(_, _ :: _) -> "(" + render v + ")"
    | _ -> render v

/// The recipe-derived flat string for a value; a `Prim` is its own atom text.
and private render (v: V) : string =
    match v with
    | Prim s -> s
    | Record fields ->
        let names = fields |> List.map fst
        let children = fields |> List.map snd |> List.toArray
        renderFlat (recordRecipe names) (fun i -> render children.[i]) (fun i -> renderArg children.[i])
    | Union(name, args) ->
        let children = args |> List.toArray
        renderFlat (unionCaseRecipe name args.Length) (fun i -> render children.[i]) (fun i -> renderArg children.[i])

// Each case: a label, the Vesper source whose `%A` runs under Node, and the value model
// the recipe renders to derive the expectation.
let private cases: (string * string * V) list =
    [
        "record",
        "type Point = { X: int; Y: int }\nprintfn \"%A\" { X = 1; Y = 2 }",
        Record [ "X", Prim "1"; "Y", Prim "2" ]

        "union nullary", "type Shape = Circle of int | Rect of int * int | Dot\nprintfn \"%A\" Dot", Union("Dot", [])

        "union single payload",
        "type Shape = Circle of int | Rect of int * int | Dot\nprintfn \"%A\" (Circle 5)",
        Union("Circle", [ Prim "5" ])

        "union multi-field tuple",
        "type Shape = Circle of int | Rect of int * int | Dot\nprintfn \"%A\" (Rect(3, 4))",
        Union("Rect", [ Prim "3"; Prim "4" ])

        "option single payload", "printfn \"%A\" (Some 3)", Union("Some", [ Prim "3" ])

        "option nullary", "printfn \"%A\" (None: int option)", Union("None", [])

        "option nested arg parens", "printfn \"%A\" (Some (Some 3))", Union("Some", [ Union("Some", [ Prim "3" ]) ])
    ]

[<Tests>]
let tests =
    testList
        "Codegen.Js StructuralFormatRecipe"
        [
            test "recordRecipe emits `{ F = ·; G = · }`" {
                let ops = recordRecipe [ "X"; "Y" ]

                Expect.equal
                    (renderFlat ops (fun i -> sprintf "<%d>" i) (fun i -> sprintf "<%d>" i))
                    "{ X = <0>; Y = <1> }"
                    "flat record form with `;`-separated `label = child`"
            }

            test "unionCaseRecipe: nullary / single arg / multi-field tuple" {
                let flat name n =
                    renderFlat (unionCaseRecipe name n) (fun i -> sprintf "<%d>" i) (fun i -> sprintf "[%d]" i)

                Expect.equal (flat "Dot" 0) "Dot" "nullary ⇒ bare identifier"
                Expect.equal (flat "Circle" 1) "Circle [0]" "single payload ⇒ `Name arg` (arg position)"
                Expect.equal (flat "Rect" 2) "Rect (<0>, <1>)" "multi-field ⇒ parenthesised child tuple"
            }

            for label, src, value in cases do
                test (sprintf "JS `%%A` matches the recipe grammar: %s" label) {
                    match runJs ("recipe-diff-" + label.Replace(" ", "-")) src with
                    | None -> skiptest "node not found on PATH"
                    | Some(code, out) ->
                        Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                        Expect.equal out (render value) "JS `%A` output equals the recipe-derived grammar"
                }
        ]
