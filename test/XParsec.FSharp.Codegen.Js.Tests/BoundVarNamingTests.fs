module XParsec.FSharp.Codegen.Js.Tests.BoundVarNamingTests

open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// A bound variable's emitted name is the frozen naming column, mangled for JS and nothing more.
//
// The column is filled at the freeze by asking the LEXER what the token spells
// (`Lexed.GetIdentifierAt`), which is the only thing that knows F#'s naming forms — so the
// forms the lexer accepts and a character-class scan does not are exactly what these pin.
// A quoted name is the discriminating case: it starts with a character no identifier
// starts with, and it can carry characters no JS identifier may.

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
            // sits. The emit-time expansion copies an inline body onto its call site, so the
            // introducing node's token spells the CALL — here the `byte` conversion the
            // division is written under — while the bound variable itself is minted and the source
            // writes it nowhere. Deriving the name from the node's anchor (rather than from a
            // record made where the bound variable was minted) named every one of these after the
            // call, which is how this test came to exist.
            test "an expanded body's bound variable is named after its slot, not the call site" {
                // `int (…)` expands a conversion whose lambda parameter is a freshened —
                // hence minted — bound variable, and the expansion puts it on the `int` call site.
                // The lambda survives only over a division, whose operand the expansion
                // cannot duplicate; `emitFrozenJs` is the manifest-backed path
                // `checkedDivisor` resolves through.
                let src = "printfn \"%d\" (int (200uy / 3uy))"
                let js = emitFrozenJs "Conv" src (frozenOf src)

                Expect.isFalse
                    (js.Contains "(int)")
                    (sprintf "no bound variable is named after the call site it was copied onto:\n%s" js)

                Expect.stringContains js "_s" "the expanded body's bound variable takes a slot name"
            }

            // An apostrophe is legal in an unquoted F# identifier and illegal in JS; it is
            // the case the mangle already covered, kept so widening it to every illegal
            // character cannot quietly drop it.
            test "a primed bound variable round-trips under Node" {
                match runJs "primed-bound-var" "let x' = 20\nlet x'' = x' + x'\nprintfn \"%d\" (x'' + 2)" with
                | None -> skiptest "node not on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "exit code (output: %s)" out)
                    Expect.equal (out.Trim()) "42" "primes mangle to a distinct legal name each"
            }
        ]
