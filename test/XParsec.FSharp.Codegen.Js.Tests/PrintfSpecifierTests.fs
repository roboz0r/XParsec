module XParsec.FSharp.Codegen.Js.Tests.PrintfSpecifierTests

open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// Per-hole format-specifier lowering: the format-handler specifiers
// (`%x`/`%o`/`%u`/`%b`/padding/alignment/`%f`/forced-sign) emit per-hole JS string
// expressions inline (no runtime import, no `.NET` format-string round trip) instead
// of the raw value. These tests pin each specifier's Node output to F#'s `printf`
// semantics (the CLR `PrintfSpecTests` goldens, extended). The genuinely subtle
// float forms (`%e`/`%E`/`%g`/`%G`) emit `toExponential`/`toPrecision` — an accepted JS
// *approximation* of .NET's byte-exact output, so their tests pin the JS behaviour
// rather than F# parity.
//
// Each hole is wrapped in `[...]` literals in the format string: `runJs` trims the
// total output, which would otherwise eat the leading spaces of a right-justified
// field — the brackets keep significant whitespace observable.

/// `printfn`-per-line program; the expected lines joined by `\n` (output is trimmed).
/// `name` is the per-test output module (distinct so concurrent runs don't share a file).
let private runsLines (name: string) (program: string) (expected: string list) =
    match runJs ("printf-specifier-" + name) program with
    | None -> skiptest "node not found on PATH"
    | Some(code, out) ->
        Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
        Expect.equal out (String.concat "\n" expected) "each specifier matches F# printf output"

[<Tests>]
let tests =
    testList
        "Codegen.Js printf format specifiers"
        [
            // Emission: a lone `%x` hole splices its operand once into the radix form
            // as real `JsExpr` nodes (inline: no runtime import, no IIFE, no `.NET`
            // format-string round trip — the operand is referenced once).
            test "`%x` emits an inline radix conversion" {
                Expect.equal
                    (emitJs "printfn \"%x\" 255")
                    "console.log((255 >>> 0).toString(16));\n"
                    "inline lowering, operand spliced once"
            }

            // A side-effecting argument under a multi-reference form (`%05d`) must be
            // evaluated exactly once — the IIFE rewrite binds it to `v`.
            test "`%05d` of a call wraps the operand in a single-eval IIFE" {
                let js = emitJs "let f () = 42\nprintfn \"%05d\" (f ())"
                Expect.stringContains js "((v) => " "multi-reference forms bind the operand once"
                Expect.stringContains js "Math.abs(v)" "the bound name is reused, not the operand expression"
            }

            test "radix + reinterpret specifiers (`%x`/`%X`/`%o`/`%u`/`%b`) match F#" {
                runsLines
                    "radix"
                    (String.concat
                        "\n"
                        [
                            "printfn \"[%x]\" 255"
                            "printfn \"[%X]\" 255"
                            "printfn \"[%o]\" 255"
                            "printfn \"[%u]\" (-1)"
                            "printfn \"[%b]\" true"
                            "printfn \"[%b]\" false"
                        ])
                    [ "[ff]"; "[FF]"; "[377]"; "[4294967295]"; "[true]"; "[false]" ]
            }

            test "width + alignment (`%5d`/`%-5d`/`%10s`/`%-10s`) match F#" {
                runsLines
                    "align"
                    (String.concat
                        "\n"
                        [
                            "printfn \"[%5d]\" 42"
                            "printfn \"[%-5d]\" 42"
                            "printfn \"[%10s]\" \"hi\""
                            "printfn \"[%-10s]\" \"hi\""
                        ])
                    [ "[   42]"; "[42   ]"; "[        hi]"; "[hi        ]" ]
            }

            test "zero-pad (`%05d`/`%08x`/`%08.2f`) — sign-aware for decimals — match F#" {
                runsLines
                    "zeropad"
                    (String.concat
                        "\n"
                        [
                            "printfn \"[%05d]\" 42"
                            "printfn \"[%05d]\" (-42)"
                            "printfn \"[%08x]\" 255"
                            "printfn \"[%08.2f]\" 3.14159"
                            "printfn \"[%08.2f]\" (-3.14159)"
                        ])
                    [ "[00042]"; "[-00042]"; "[000000ff]"; "[00003.14]"; "[-0003.14]" ]
            }

            test "fixed-point (`%f` default 6, `%.2f`) matches F#" {
                runsLines
                    "fixed"
                    (String.concat "\n" [ "printfn \"[%f]\" 3.5"; "printfn \"[%.2f]\" 3.14159" ])
                    [ "[3.500000]"; "[3.14]" ]
            }

            test "forced-sign sections (`%+d`/`% d`/`%+.2f`) match F#" {
                runsLines
                    "sign"
                    (String.concat
                        "\n"
                        [
                            "printfn \"[%+d]\" 5"
                            "printfn \"[% d]\" 5"
                            "printfn \"[%+d]\" (-5)"
                            "printfn \"[%+.2f]\" 3.14"
                        ])
                    [ "[+5]"; "[ 5]"; "[-5]"; "[+3.14]" ]
            }

            // `%e`/`%E`/`%g`/`%G` were previously cold (raw operand). They now emit
            // `toExponential` / `toPrecision` — an accepted *approximation* of .NET's
            // byte-exact output (JS uses a minimal exponent width, not .NET's 3-digit
            // zero-pad, and `toPrecision` keeps trailing zeros), so these pin the JS
            // behaviour rather than F# parity.
            test "`%e` emits an inline `toExponential` conversion" {
                Expect.equal
                    (emitJs "printfn \"%e\" 1234.5")
                    "console.log((1234.5).toExponential(6));\n"
                    "inline lowering, operand spliced once"
            }

            test "`%G` upper-cases the `toPrecision` result" {
                Expect.equal
                    (emitJs "printfn \"%G\" 1234.5")
                    "console.log((1234.5).toPrecision(6).toUpperCase());\n"
                    "compact form, upper-cased exponent letter"
            }

            test "exponential / compact forms (`%e`/`%E`/`%.2e`/`%g`/`%G`/`%.3g`) render via JS" {
                runsLines
                    "expg"
                    (String.concat
                        "\n"
                        [
                            "printfn \"[%e]\" 1234.5"
                            "printfn \"[%E]\" 1234.5"
                            "printfn \"[%.2e]\" 1234.5"
                            "printfn \"[%e]\" (-0.000789)"
                            "printfn \"[%g]\" 1234.5"
                            "printfn \"[%G]\" 0.0001234"
                            "printfn \"[%.3g]\" 1234.5"
                        ])
                    [
                        "[1.234500e+3]"
                        "[1.234500E+3]"
                        "[1.23e+3]"
                        "[-7.890000e-4]"
                        "[1234.50]"
                        "[0.000123400]"
                        "[1.23e+3]"
                    ]
            }
        ]
