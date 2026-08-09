module XParsec.FSharp.Codegen.Js.Tests.UnsupportedOnTargetTests

// The unsupported-on-target rule, driven end to end through the REAL JS contract stack.
// `prim-types-nativeint.fsi` stays in the shared `[core] files` tier, so JS knows these
// types; what it lacks is a `.js.fs` binding a repr, and that absence is the statement.

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

            test "the verdict is NOT a name-resolution failure: the type resolves, then is refused" {
                // If `prim-types-nativeint.fsi` left the shared tier, JS would report an
                // undefined type instead and this rule would be unreachable.
                let errors = jsErrors "let f (x: nativeint) = x"

                Expect.isFalse
                    (errors |> List.exists (fun m -> m.Contains "is not defined"))
                    (sprintf "nativeint must RESOLVE on js, not go undefined; got %A" errors)
            }

            test "a mention in a signature that is never instantiated still errors" {
                // `nativeptr<'T>` is generic and nothing here constructs one: a mention in
                // a declared field type is enough.
                let errors = jsErrors "type Holder = { P: nativeptr<int> }"

                Expect.contains errors "nativeptr is not supported on the js target" "a declared field type"
            }

            test "every type `prim-types-nativeint.fsi` declares is refused on js" {
                let refused (src: string) (name: string) =
                    Expect.contains
                        (jsErrors src)
                        (sprintf "%s is not supported on the js target" name)
                        (sprintf "%s is refused" name)

                refused "let f (x: nativeint) = x" "nativeint"
                refused "let f (x: unativeint) = x" "unativeint"
                refused "let f (x: nativeptr<int>) = x" "nativeptr"
                refused "let f (x: voidptr) = x" "voidptr"
                refused "let f (x: ilsigptr<int>) = x" "ilsigptr"
            }

            // ---- ...and stays silent otherwise ----

            test "a JS program mentioning none of these produces no unsupported verdict" {
                let errors =
                    jsErrors "let add (x: int) (y: int) = x + y\nlet s = \"ok\"\nlet xs = [| 1.0; 2.0 |]"

                Expect.isEmpty errors (sprintf "a representable program compiles clean; got %A" errors)
            }

            test "loading the library is silent: the contract that DECLARES them is not the mention" {
                // The `.fsi` declaring these is parsed on every JS compile; the diagnostic
                // belongs to the program that writes `nativeint`, never to the library
                // that declares it.
                Expect.isEmpty (jsErrors "let x = 1 + 2") "a JS compile that never names one is clean"
            }
        ]
