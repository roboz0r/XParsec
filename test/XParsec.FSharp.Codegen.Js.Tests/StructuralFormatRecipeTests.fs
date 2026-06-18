module XParsec.FSharp.Codegen.Js.Tests.StructuralFormatRecipeTests

open Expecto
open XParsec.FSharp.Codegen.Common.StructuralFormatRecipe
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// The `%A` output *grammar* (record /
// union forms) is lifted to `Codegen.Common.StructuralFormatRecipe`, the single
// source of truth. The CLR backend lowers the recipe to IL directly (the
// end-to-end `PrintfDifferentialTests` prove that path byte-identical, flat and
// broken). The JS central shape-keyed walker (`Vesper.Printf.mjs`) deliberately does NOT consume the
// recipe — it reconstructs the same grammar dynamically. Parity between the two
// is held by THIS differential test, not by a shared call path: the *expected*
// record/union strings below are rendered straight from the recipe, and the
// *actual* strings come from running the JS runtime. They can drift only if this
// test is missing — so it is load-bearing (Decisions/risks in the plan).

// A flat interpretation of a `SinkOp` sequence — the never-break layout the JS
// runtime always produces (`width` accepted but unused; CLR `%0A` mode). `Line`
// is its flat alternative (a single space); groups / nests / applications are
// layout-only and vanish flat; `FormatChild`/`FormatArg` substitute the child's
// already-rendered string. This is the layout half the recipe delegates to the
// consumer (the CLR `IFormatSink`, the JS walker) — modelled here to derive the
// grammar's flat string from the recipe alone.
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

// A minimal value model — exactly the shapes the recipe owns: a `Prim` leaf plus
// the `Record`/`Union` forms that route through `recordRecipe`/`unionCaseRecipe`.
// Tuple/list/atom grammar is NOT recipe-driven (no recipe form), so it stays in
// `Step6Tests` rather than being re-modelled here.
type private V =
    | Prim of string
    | Record of (string * V) list
    | Union of name: string * args: V list

/// Is `v` a payload-bearing union (so it parenthesises in argument position,
/// `Some (Some 3)`)? Mirrors the JS `fmtArg` rule and the recipe's
/// `BeginApplication` semantics.
let rec private renderArg (v: V) : string =
    match v with
    | Union(_, _ :: _) -> "(" + render v + ")"
    | _ -> render v

/// The recipe-derived flat string for a value. Record/union forms come straight
/// from `recordRecipe`/`unionCaseRecipe`; a `Prim` is its own atom text.
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

// Each case: a label, the Vesper source whose `%A` we run, and the value model
// the recipe renders to derive the expectation. Only recipe-owned shapes
// (records, unions, options) live here — this test is the canonical owner of
// their JS `%A` goldens; tuple/list/atom forms stay in `Step6Tests`.
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
            // The recipe IS the grammar — pin its sink-op forms directly so a form
            // change is a conscious edit here, visible to both backends.
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

            // The cross-target differential: recipe-derived expectation vs the JS
            // runtime's actual `%A`. Drift in either the recipe or the JS walker
            // (Vesper.Printf.mjs) trips the corresponding case.
            for label, src, value in cases do
                test (sprintf "JS `%%A` matches the recipe grammar: %s" label) {
                    match runJs ("recipe-diff-" + label.Replace(" ", "-")) src with
                    | None -> skiptest "node not found on PATH"
                    | Some(code, out) ->
                        Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                        Expect.equal out (render value) "JS `%A` output equals the recipe-derived grammar"
                }
        ]
