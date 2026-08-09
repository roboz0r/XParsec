module XParsec.FSharp.Codegen.Js.Tests.BoundVarNamingTests

open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// A bound variable's emitted name is the frozen naming column, mangled for JS. The column is
// filled by asking the LEXER what the token spells, so what these pin are the forms the lexer
// accepts and a character-class scan does not: a quoted name, and a primed one.

[<Tests>]
let tests =
    testList
        "Codegen.Js bound variable naming"
        [
            test "a quoted bound variable emits under its own name" {
                let js = emit "let ``my value`` = 41\nprintfn \"%d\" (``my value`` + 1)"

                Expect.stringContains js "my_value" "the quoted name is emitted, with the space mangled"

                Expect.isFalse
                    (js.Contains "``")
                    "the quoting is the source's, not part of the name — it must not reach the JS"
            }

            test "a quoted bound variable round-trips under Node" {
                match runJs "quoted-bound-var" "let ``my value`` = 41\nprintfn \"%d\" (``my value`` + 1)" with
                | None -> skiptest "node not on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "exit code (output: %s)" out)
                    Expect.equal (out.Trim()) "42" "the mangled name binds and reads back"
            }

            // A bound variable of an EXPANDED body is named after its slot, never after where it
            // sits: the expansion copies the body onto the call site, so the introducing node's
            // token spells the CALL, while the bound variable is minted and never written.
            test "an expanded body's bound variable is named after its slot, not the call site" {
                // `int (…)` expands a conversion whose lambda parameter is minted, onto the
                // `int` call site; the lambda survives only over the division, whose operand
                // cannot be duplicated. `emitFrozenJs` is the path `checkedDivisor` resolves on.
                let src = "printfn \"%d\" (int (200uy / 3uy))"
                let js = emitFrozenJs "Conv" src (frozenOf src)

                Expect.isFalse
                    (js.Contains "(int)")
                    (sprintf "no bound variable is named after the call site it was copied onto:\n%s" js)

                Expect.stringContains js "_s" "the expanded body's bound variable takes a slot name"
            }

            // An apostrophe is legal in an unquoted F# identifier and illegal in JS, so each
            // prime must mangle to a distinct legal name.
            test "a primed bound variable round-trips under Node" {
                match runJs "primed-bound-var" "let x' = 20\nlet x'' = x' + x'\nprintfn \"%d\" (x'' + 2)" with
                | None -> skiptest "node not on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "exit code (output: %s)" out)
                    Expect.equal (out.Trim()) "42" "primes mangle to a distinct legal name each"
            }
        ]
