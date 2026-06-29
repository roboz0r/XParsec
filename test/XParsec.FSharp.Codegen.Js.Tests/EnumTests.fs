module XParsec.FSharp.Codegen.Js.Tests.EnumTests

open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// Step 6 (JS codegen) of enum support: every enum variant (numeric / string /
// mixed) emits a module-scope FROZEN OBJECT MAP `const E = Object.freeze({ … })`,
// `E.Ci` is a property read, and an `EnumCase` pattern lowers to `scrut === E.Ci`
// (JS `===` is value equality for numbers and strings, so it is correct for all
// three variants — v1 = equality only, no reverse map).
[<Tests>]
let tests =
    testList
        "Codegen.Js Enum"
        [
            // ---- Emitted-source shape -------------------------------------------
            test "a numeric enum emits a frozen object map of number literals" {
                let src =
                    emitJs "type Color = | Red = 0 | Green = 1 | Blue = 2\nlet c = Color.Green"

                Expect.stringContains
                    src
                    "const Color = Object.freeze({ Red: 0, Green: 1, Blue: 2 });"
                    "numeric cases emit as JS number literals in declaration order, frozen"

                Expect.stringContains src "const c = Color.Green;" "`Color.Green` is a property read on the frozen map"
            }

            test "a string enum emits a frozen object map of string literals" {
                let src = emitJs "type Dir = | Up = \"up\" | Down = \"down\"\nlet d = Dir.Up"

                Expect.stringContains
                    src
                    "const Dir = Object.freeze({ Up: \"up\", Down: \"down\" });"
                    "string cases emit as JS string literals — same object-map shape as numeric"

                Expect.stringContains src "const d = Dir.Up;" "`Dir.Up` is a property read on the frozen map"
            }

            test "a mixed enum emits the same object map with number + string values" {
                let src = emitJs "type M = | A = 1 | B = \"two\"\nlet x = M.A"

                // JS is untyped: a mix of numbers and strings is the same object-map
                // shape, no special handling (no reverse map, no obj-box).
                Expect.stringContains
                    src
                    "const M = Object.freeze({ A: 1, B: \"two\" });"
                    "mixed cases coexist in one untyped frozen object map"
            }

            test "an enum-case pattern lowers to a `=== E.Ci` test against the frozen map" {
                let src =
                    emitJs (
                        "type E = | A = 1 | B = 2\n"
                        + "let describe (e: E) = match e with | E.A -> 10 | E.B -> 20 | _ -> 0\n"
                        + "let r = describe E.A"
                    )

                Expect.stringContains
                    src
                    "=== E.A"
                    "the `| E.A` arm tests the scrutinee for `=== E.A` (object map is the single source of truth)"

                Expect.stringContains src "=== E.B" "the `| E.B` arm tests `=== E.B`"
            }

            // ---- Node round-trip ------------------------------------------------
            test "a numeric enum match picks the right arm at runtime" {
                let prog =
                    "type E = | A = 1 | B = 2 | C = 3\n"
                    + "let describe (e: E) =\n"
                    + "    match e with\n"
                    + "    | E.A -> 10\n"
                    + "    | E.B -> 20\n"
                    + "    | _ -> 0\n"
                    + "printfn \"%d\" (describe E.A)\n"
                    + "printfn \"%d\" (describe E.B)\n"
                    + "printfn \"%d\" (describe E.C)"

                match runJs "enum-numeric-match" prog with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "10\n20\n0" "`=== E.Ci` value equality routes each case; the fallthrough hits `_`"
            }

            test "a string enum round-trips its case strings through a match" {
                let prog =
                    "type Dir = | Up = \"up\" | Down = \"down\"\n"
                    + "let tag (d: Dir) =\n"
                    + "    match d with\n"
                    + "    | Dir.Up -> \"U\"\n"
                    + "    | Dir.Down -> \"D\"\n"
                    + "    | _ -> \"?\"\n"
                    + "printfn \"%s\" (tag Dir.Up)\n"
                    + "printfn \"%s\" (tag Dir.Down)"

                match runJs "enum-string-match" prog with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal out "U\nD" "`===` is string value equality, so the case strings discriminate the arms"
            }
        ]
