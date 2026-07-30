module XParsec.FSharp.Codegen.Js.Tests.StructuralPrinterTests

open System
open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// The `%A` structural formatter runtime module: authored as Vesper source
// (`structural-printer.js.fs`) and compiled by the JS backend in library mode into the
// committed `Vesper.Printf.mjs`. The walker is shape-keyed and FLAT-output; this file
// pins the generated module in isolation (it exports `structuralFormat`, imports nothing,
// and renders representative shapes correctly under Node) plus its Wadler width-breaking
// pretty-print layout. The end-to-end parity over real emitted unions/lists/records —
// which materialises THIS generated `.mjs` — lives in `StructuralFormatTests` (the `%A`
// hole-lowering cases) and `StructuralFormatRecipeTests` (the cross-target differential).

/// The generated `Vesper.Printf.mjs` source (deps-only provider, library mode).
let private generated: Lazy<string> =
    lazy
        compileLibrary
            coreDepsJsContract.Value
            "Vesper.Printf"
            "structural-printer.js.fs"
            (IO.File.ReadAllText(srcFile "Vesper.Printf" "structural-printer.js.fs"))

/// Normalise line endings so a CRLF checkout still matches the printer's `\n` output.
let private lf (s: string) : string = s.Replace("\r\n", "\n")

[<Tests>]
let tests =
    testList
        "Codegen.Js Structural Printer (%A runtime module)"
        [
            test "the generated module exports `structuralFormat` and imports nothing" {
                let src = generated.Value
                Expect.stringContains src "export const structuralFormat = " "exports the runtime entry"
                Expect.isFalse (src.Contains "import ") "the walker is self-contained — no runtime imports"
            }

            test "the committed Vesper.Printf.mjs matches the generated source (regenerable)" {
                let path = srcFile "Vesper.Printf" "Vesper.Printf.mjs"

                if Environment.GetEnvironmentVariable "UPDATE_SNAPSHOTS" = "1" then
                    IO.File.WriteAllText(path, generated.Value)

                Expect.equal
                    (lf generated.Value)
                    (lf (IO.File.ReadAllText path))
                    "committed asset is stale — rerun with UPDATE_SNAPSHOTS=1"
            }

            test "the generated module renders every shape under Node (flat parity)" {
                let driver =
                    String.concat
                        "\n"
                        [
                            "import { structuralFormat } from \"./Vesper.Printf.mjs\";"
                            "const fmt = (v) => structuralFormat(v, 80, 10000);"
                            // A union value carries `tag` + fields as own-keys and `cases()` on the
                            // prototype — exactly the emitted-class shape (Object.keys excludes cases).
                            "const mkUnion = (names) => { const p = { cases() { return names; } };"
                            "  return (tag, fields) => Object.assign(Object.create(p), { tag }, fields); };"
                            "const opt = mkUnion([\"None\", \"Some\"]);"
                            "const shape = mkUnion([\"Circle\", \"Rect\", \"Dot\"]);"
                            "const lst = mkUnion([\"Empty\", \"Cons\"]);"
                            "const cons = (h, t) => lst(1, { Head: h, Tail: t });"
                            "const empty = lst(0, {});"
                            // primitives
                            "console.log(fmt(5));"
                            "console.log(fmt(true));"
                            "console.log(fmt(\"a\\nb\"));"
                            "console.log(fmt(5n));"
                            "console.log(fmt(undefined));"
                            // tuple / record
                            "console.log(fmt([1, 2]));"
                            "console.log(fmt({ X: 1, Y: 2 }));"
                            // unions: nullary / single payload / multi-field / nested-arg parens
                            "console.log(fmt(opt(0, {})));"
                            "console.log(fmt(opt(1, { value: 3 })));"
                            "console.log(fmt(shape(1, { a: 3, b: 4 })));"
                            "console.log(fmt(opt(1, { value: shape(0, { a: 5 }) })));"
                            // cons-list
                            "console.log(fmt(cons(1, cons(2, cons(3, empty)))));"
                            "console.log(fmt(empty));"
                        ]

                match
                    runNodeFiles "structural-printer" [ "driver.mjs", driver; "Vesper.Printf.mjs", generated.Value ]
                with
                | None -> skiptest "node is not installed"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "driver exited non-zero: %s" out)

                    Expect.equal
                        out
                        (String.concat
                            "\n"
                            [
                                "5"
                                "true"
                                "\"a\\nb\""
                                "5L"
                                "()"
                                "(1, 2)"
                                "{ X = 1; Y = 2 }"
                                "None"
                                "Some 3"
                                "Rect (3, 4)"
                                "Some (Circle 5)"
                                "[1; 2; 3]"
                                "[]"
                            ])
                        "primitives / unit / tuple / record / union forms / cons-list, all flat"
            }

            // The shared Wadler `Doc`/`render` width-breaking kernel. A `Group` lays out
            // ALL-FLAT when its flat width fits from the current column, else ALL-BROKEN — its
            // `Line`s become a newline + the active `Nest` indent. The expectations below are
            // byte-identical to the CLR `PrintfSpecTests` goldens (record nest 2,
            // tuple nest 1, list brackets on own lines nest 2), so JS `%NA` matches CLR
            // `%NA` rather than collapsing to `%0A`.
            test "the layout breaks under a tight width budget (CLR parity)" {
                let driver =
                    String.concat
                        "\n"
                        [
                            "import { structuralFormat } from \"./Vesper.Printf.mjs\";"
                            "const fmt = (v, w) => structuralFormat(v, w, 10000);"
                            "const mkUnion = (names) => { const p = { cases() { return names; } };"
                            "  return (tag, fields) => Object.assign(Object.create(p), { tag }, fields); };"
                            "const lst = mkUnion([\"Empty\", \"Cons\"]);"
                            "const cons = (h, t) => lst(1, { Head: h, Tail: t });"
                            "const empty = lst(0, {});"
                            // A record breaks its fields at +2 indent (CLR `%5A` golden).
                            "console.log(JSON.stringify(fmt({ X: 1, Y: \"a\" }, 5)));"
                            // A tuple hangs components under the open paren at +1 indent.
                            "console.log(JSON.stringify(fmt([1, 2, 3], 5)));"
                            // A list puts the brackets on their own lines, elements nested +2.
                            "console.log(JSON.stringify(fmt(cons(1, cons(2, cons(3, empty))), 5)));"
                            // width 0 ⇒ never break (the `%0A` mode) even when it overflows.
                            "console.log(JSON.stringify(fmt(cons(1, cons(2, cons(3, empty))), 0)));"
                            // A wide budget keeps everything flat (no spurious breaks).
                            "console.log(JSON.stringify(fmt({ X: 1, Y: \"a\" }, 80)));"
                        ]

                match
                    runNodeFiles
                        "structural-printer-break"
                        [ "driver.mjs", driver; "Vesper.Printf.mjs", generated.Value ]
                with
                | None -> skiptest "node is not installed"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "driver exited non-zero: %s" out)

                    Expect.equal
                        (lf out)
                        (String.concat
                            "\n"
                            [
                                "\"{ X = 1;\\n  Y = \\\"a\\\" }\"" // record breaks, fields at +2
                                "\"(1,\\n 2,\\n 3)\"" // tuple hangs at +1 under the paren
                                "\"[\\n  1;\\n  2;\\n  3\\n]\"" // list brackets own lines, elems +2
                                "\"[1; 2; 3]\"" // width 0 ⇒ flat
                                "\"{ X = 1; Y = \\\"a\\\" }\"" // fits 80 ⇒ flat
                            ])
                        "groups break all-or-nothing at the width budget, matching the CLR layout"
            }
        ]
