module XParsec.FSharp.Codegen.Js.Tests.PrintfSpecifierTests

open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// Each specifier lowers to an inline per-hole JS expression, pinned against F#'s `printf`.
// `%e`/`%E`/`%g`/`%G` lower to `toExponential`/`toPrecision`, which pins JS output, not F# parity.
// Holes sit inside `[...]` because `runJs` trims, which would eat a right-justified field's spaces.

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
            // The curried callback is invoked `cb(unit)(value)` and its residue splices at the hole.
            test "`sprintf \"%a\"` invokes the callback and splices its residue string" {
                runsLines
                    "callback-a"
                    "printfn \"%s\" (sprintf \"%a\" (fun (s: unit) (x: int) -> sprintf \"%d\" x) 42)"
                    [ "42" ]
            }

            // No value argument, so `cb(unit)` yields the residue directly.
            test "`sprintf \"%t\"` invokes the value-less callback" {
                runsLines "callback-t" "printfn \"%s\" (sprintf \"%t\" (fun (s: unit) -> \"hi\"))" [ "hi" ]
            }

            test "`sprintf \"%a\"` callback closes over an outer local" {
                runsLines
                    "callback-closure"
                    (String.concat
                        "\n"
                        [
                            "let prefix = \"n=\""
                            "printfn \"%s\" (sprintf \"%a\" (fun (s: unit) (x: int) -> prefix + sprintf \"%d\" x) 7)"
                        ])
                    [ "n=7" ]
            }

            // The residue splices into a hole in the `+`-concatenation of the surrounding segments.
            test "`sprintf \"%a\"` splices inside a multi-segment format" {
                runsLines
                    "callback-multi"
                    "printfn \"%s\" (sprintf \"[%a]\" (fun (s: unit) (x: int) -> sprintf \"%d\" x) 9)"
                    [ "[9]" ]
            }

            // `printf`'s `%a` has `State = TextWriter`, and the JS provider declares no
            // `System.IO.TextWriter`, so the sink-type diagnostic fires on the same
            // specifier `sprintf` lowers above.
            test "`printf \"%a\"` on JS diagnoses the missing sink type" {
                let ds = analyseWith jsProvider.Value "printf \"%a\" (fun s (x: int) -> ()) 42"

                Expect.stringContains
                    (errorText ds)
                    "requires a sink type"
                    "writer-family %a diagnoses on a target whose provider lacks TextWriter"
            }

            test "`%x` emits an inline radix conversion" {
                Expect.equal
                    (emitJs "printfn \"%x\" 255")
                    "console.log((255 >>> 0).toString(16));\n"
                    "inline lowering, operand spliced once"
            }

            // A multi-reference form must evaluate a side-effecting argument once, so the
            // rewrite binds it to `v`.
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

            test "zero-pad (`%05d`/`%08x`/`%08.2f`) matches F#, sign-aware for decimals" {
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

            // `padStart` never truncates, so digits already exceeding the width print unpadded.
            test "zero-pad unsigned/octal (`%05u`/`%08o`) matches F#, overflow unpadded" {
                runsLines
                    "zeropaduo"
                    (String.concat
                        "\n"
                        [
                            "printfn \"[%05u]\" 42"
                            "printfn \"[%05u]\" (-1)"
                            "printfn \"[%08o]\" 8"
                            "printfn \"[%08o]\" (-1)"
                        ])
                    [ "[00042]"; "[4294967295]"; "[00000010]"; "[37777777777]" ]
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

            // The sign stays at the field's left edge and the zeros fill in after it.
            test "sign + zero-pad integer (`%+05d`/`% 05d`) match F#" {
                runsLines
                    "signzero"
                    (String.concat
                        "\n"
                        [
                            "printfn \"[%+05d]\" 42"
                            "printfn \"[%+05d]\" (-42)"
                            "printfn \"[% 05d]\" 42"
                            "printfn \"[%+05d]\" 123456"
                        ])
                    [ "[+0042]"; "[-0042]"; "[ 0042]"; "[+123456]" ]
            }

            // Zeros fill after the sign, to a total field of 8 (`padStart` on the post-sign
            // slice). Values avoid exact float midpoints, where JS `toFixed` diverges from .NET.
            test "sign + zero-pad float (`%+08.2f`/`% 08.2f`) match F#" {
                runsLines
                    "signzerof"
                    (String.concat
                        "\n"
                        [
                            "printfn \"[%+08.2f]\" 3.14159"
                            "printfn \"[%+08.2f]\" (-3.14159)"
                            "printfn \"[% 08.2f]\" 3.14159"
                            "printfn \"[%+08.2f]\" 12345.5"
                        ])
                    [ "[+0003.14]"; "[-0003.14]"; "[ 0003.14]"; "[+12345.50]" ]
            }

            // F# fills the RIGHT with zeros here; `padEnd` reproduces it byte-for-byte.
            test "left-align + zero-pad float (`%-05.2f`) matches F#" {
                runsLines
                    "rightzero"
                    (String.concat
                        "\n"
                        [
                            "printfn \"[%-05.2f]\" 3.14159"
                            "printfn \"[%-05.2f]\" (-3.14159)"
                            "printfn \"[%-08.2f]\" 3.14159"
                            "printfn \"[%-05.2f]\" 12345.6"
                        ])
                    [ "[3.140]"; "[-3.14]"; "[3.140000]"; "[12345.60]" ]
            }

            test "`%+e` prefixes the sign onto the `toExponential` result" {
                Expect.stringContains
                    (emitJs "printfn \"%+e\" 1234.5")
                    "toExponential(6)"
                    "forced-sign exponential lowers to toExponential"
            }

            test "`%014e` zero-pads the `toExponential` result after any sign" {
                let js = emitJs "printfn \"%014e\" 1234.5"
                Expect.stringContains js "toExponential(6)" "zero-pad exponential lowers to toExponential"
                Expect.stringContains js "padStart(14" "and zero-pads to the field width"
            }

            // JS uses a minimal exponent width, not .NET's 3-digit zero-pad, and
            // `toPrecision` keeps trailing zeros.
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

            test
                "exponential / compact forms (`%e`/`%E`/`%.2e`/`%g`/`%G`/`%.3g`) render through toExponential/toPrecision" {
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

            test "star width (`%*d`/`%-*d`/`%*s`/`%*x`) matches F# value output" {
                runsLines
                    "starwidth"
                    (String.concat
                        "\n"
                        [
                            "printfn \"[%*d]\" 5 42"
                            "printfn \"[%-*d]\" 5 42"
                            "printfn \"[%*s]\" 6 \"hi\""
                            "printfn \"[%*x]\" 6 255"
                            "printfn \"[%*d]\" 2 12345"
                        ])
                    [ "[   42]"; "[42   ]"; "[    hi]"; "[    ff]"; "[12345]" ]
            }

            test "`%*d` binds the runtime width before the value" {
                Expect.stringContains
                    (emitJs "printfn \"%*d\" 5 42")
                    "(w) =>"
                    "the star width is bound in a `w`-parameter arrow, evaluated before the value"
            }

            // `%*.*f` binds `w` (outer) then `p` (inner), so JS evaluates width, then
            // precision, then the value, matching F#'s curried order.
            test "star precision (`%.*f`/`%*.*f`) matches F# value output" {
                runsLines
                    "starprec"
                    (String.concat
                        "\n"
                        [
                            "printfn \"[%.*f]\" 2 3.14159"
                            "printfn \"[%*.*f]\" 8 2 3.14159"
                            "printfn \"[%.*f]\" 0 3.14159"
                        ])
                    [ "[3.14]"; "[    3.14]"; "[3]" ]
            }

            test "`%.*f` binds the runtime precision in a `p`-parameter arrow" {
                Expect.stringContains
                    (emitJs "printfn \"%.*f\" 2 3.14159")
                    "(p) =>"
                    "the star precision is bound in a `p`-parameter arrow"
            }

            test "`%*.*f` binds width then precision" {
                let js = emitJs "printfn \"%*.*f\" 8 2 3.14159"
                Expect.stringContains js "(w) =>" "the width is bound"
                Expect.stringContains js "(p) =>" "the precision is bound"
                Expect.stringContains js "Math.min(99" "the two-star path clamps precision to 0..99"
            }

            test "`%.*A` feeds the runtime size budget to structuralFormat" {
                Expect.stringContains
                    (emitJs "printfn \"%.*A\" 2 [1; 2; 3]")
                    "(p) =>"
                    "the star size is bound in a `p`-parameter arrow"
            }
        ]
