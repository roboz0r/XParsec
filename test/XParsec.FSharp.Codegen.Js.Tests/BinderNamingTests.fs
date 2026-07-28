module XParsec.FSharp.Codegen.Js.Tests.BinderNamingTests

open Expecto
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// A binder's emitted name is the frozen naming column, mangled for JS and nothing more.
//
// The column is filled at the freeze by asking the LEXER what the token spells
// (`Lexed.GetIdentifierAt`), which is the only thing that knows F#'s naming forms — so the
// forms the lexer accepts and a character-class scan does not are exactly what these pin.
// A quoted name is the discriminating case: it starts with a character no identifier
// starts with, and it can carry characters no JS identifier may.

[<Tests>]
let tests =
    testList
        "Codegen.Js binder naming"
        [
            test "a quoted binder emits under its own name" {
                let js = emit "let ``my value`` = 41\nprintfn \"%d\" (``my value`` + 1)"

                Expect.stringContains js "my_value" "the quoted name is emitted, with the space mangled"

                Expect.isFalse
                    (js.Contains "``")
                    "the quoting is the source's, not part of the name — it must not reach the JS"
            }

            test "a quoted binder round-trips under Node" {
                match runJs "quoted-binder" "let ``my value`` = 41\nprintfn \"%d\" (``my value`` + 1)" with
                | None -> skiptest "node not on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "exit code (output: %s)" out)
                    Expect.equal (out.Trim()) "42" "the mangled name binds and reads back"
            }

            // A binder of a SPLICED body is named after its slot, never after where it
            // sits. `Inline.spliceAt` moves an inlined body onto its call site, so the
            // introducing node's token spells the CALL — here the `byte` conversion the
            // division is written under — while the binder itself is `freshen`-minted and
            // the source writes it nowhere. Deriving the name from the node's anchor
            // (rather than from a record made where the binder was minted) named every one
            // of these after the call, which is how this test came to exist.
            test "a spliced body's binder is named after its slot, not the call site" {
                // `int (…)` splices a conversion whose lambda parameter is a freshened —
                // hence minted — binder, and the splice puts it on the `int` call site. The
                // lambda survives only over a division, whose operand the splice cannot
                // duplicate; `emitFrozenJs` is the manifest-backed path `checkedDivisor`
                // resolves through.
                let src = "printfn \"%d\" (int (200uy / 3uy))"
                let js = emitFrozenJs "Conv" src (frozenOf src)

                Expect.isFalse
                    (js.Contains "(int)")
                    (sprintf "no binder is named after the call site it was spliced onto:\n%s" js)

                Expect.stringContains js "_s" "the spliced body's binder takes a slot name"
            }

            // An apostrophe is legal in an unquoted F# identifier and illegal in JS; it is
            // the case the mangle already covered, kept so widening it to every illegal
            // character cannot quietly drop it.
            test "a primed binder round-trips under Node" {
                match runJs "primed-binder" "let x' = 20\nlet x'' = x' + x'\nprintfn \"%d\" (x'' + 2)" with
                | None -> skiptest "node not on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "exit code (output: %s)" out)
                    Expect.equal (out.Trim()) "42" "primes mangle to a distinct legal name each"
            }
        ]
