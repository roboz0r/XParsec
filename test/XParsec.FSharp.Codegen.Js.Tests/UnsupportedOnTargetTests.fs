module XParsec.FSharp.Codegen.Js.Tests.UnsupportedOnTargetTests

// The unsupported-on-target rule, driven end to end through the REAL JS contract stack.
// `prim-types-nativeint.fsi` / `prim-types-decimal.fsi` are ABSENT from `manifest.js.toml`:
// JS has no such types. `nativeint` / `unativeint` / `decimal` are language-known keys (a
// literal token reaches each), so a mention resolves and is then refused as unsupported on
// the target; the pointer spellings are known to no js contract and go undefined instead.

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

/// The error messages a program produces through the JS contract.
let private jsErrors (src: string) : string list =
    analyseWith jsProvider.Value src |> List.map (fun d -> d.Message)

[<Tests>]
let tests =
    testList
        "UnsupportedOnTarget"
        [
            // ---- The rule fires, and names both the type and the target ----

            test "a JS program mentioning `nativeint` is rejected, naming type and target" {
                let errors = jsErrors "let f (x: nativeint) = x"

                Expect.contains errors "nativeint is not supported on the js target" "the targeted verdict"
            }

            test "the verdict is NOT a name-resolution failure: the name resolves, then is refused" {
                // No js contract declares `nativeint`; the LANGUAGE does (`10n` is a lexer
                // token), so the written name resolves to its key and the mention is refused.
                let errors = jsErrors "let f (x: nativeint) = x"

                Expect.isFalse
                    (errors |> List.exists (fun m -> m.Contains "is not defined"))
                    (sprintf "nativeint must RESOLVE on js, not go undefined; got %A" errors)
            }

            test "every language-known primitive JS lacks is refused on js" {
                let refused (src: string) (name: string) =
                    Expect.contains
                        (jsErrors src)
                        (sprintf "%s is not supported on the js target" name)
                        (sprintf "%s is refused" name)

                refused "let f (x: nativeint) = x" "nativeint"
                refused "let f (x: unativeint) = x" "unativeint"
                refused "let f (x: decimal) = x" "decimal"
                refused "let f (x: voidptr) = x" "voidptr"
            }

            test "a nativeint LITERAL is refused, not crashed on: the key mints without a contract" {
                let errors = jsErrors "let x = 10n"

                Expect.contains errors "nativeint is not supported on the js target" "the literal's width is refused"
            }

            // ---- A type no contract and no token knows is simply absent ----

            test "the keyless pointer spellings are undefined on js, not `unsupported`" {
                // `nativeptr` / `ilsigptr` come only from `prim-types-nativeint.fsi`, which
                // the js manifest omits, and neither is a built-in primitive identity.
                let undefinedOn (src: string) (name: string) =
                    Expect.contains
                        (jsErrors src)
                        (sprintf "The type '%s' is not defined" name)
                        (sprintf "%s is absent" name)

                undefinedOn "type Holder = { P: nativeptr<int> }" "nativeptr"
                undefinedOn "let f (x: ilsigptr<int>) = x" "ilsigptr"
            }

            // ---- ...and stays silent otherwise ----

            test "a JS program mentioning none of these produces no unsupported verdict" {
                let errors =
                    jsErrors "let add (x: int) (y: int) = x + y\nlet s = \"ok\"\nlet xs = [| 1.0; 2.0 |]"

                Expect.isEmpty errors (sprintf "a representable program compiles clean; got %A" errors)
            }

            test "loading the library is silent: the diagnostic belongs to the mentioning program" {
                Expect.isEmpty (jsErrors "let x = 1 + 2") "a JS compile that never names one is clean"
            }
        ]
