module XParsec.FSharp.Codegen.Js.Tests.IntrinsicForwardReprJsTests

open Expecto

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// The forward intrinsic axis `{ canon -> platform-repr }`, extracted from the same
// `<base>.js.fs` `(# "<repr>" #)` bindings that yield the reverse axis. A JS `number`
// read as a value is `float` only because `float` reprs to `number`, so pin the mapping.

let private forwardRepr = jsProvider.Value.IntrinsicForwardRepr

/// Bridge a bare `.fsi` canon name to the axis's qualified intrinsic key (`Vesper.int`).
let private tryRepr (canon: string) : string option =
    match forwardRepr.TryGetValue(RuntimeNames.primitiveKey canon) with
    | true, repr -> Some repr
    | _ -> None

[<Tests>]
let tests =
    testList
        "IntrinsicForwardReprJs"
        [
            test "the JS number family all repr to `number`" {
                for canon in [ "int"; "float"; "float32" ] do
                    Expect.equal
                        (tryRepr canon)
                        (Some "number")
                        (sprintf "canon '%s' must extract JS repr 'number'" canon)
            }

            test "`bool` reprs to `boolean`" { Expect.equal (tryRepr "bool") (Some "boolean") "bool -> boolean" }

            // A primitive whose JS type name is spelled like its own canon. Dropping it as
            // "no repr on this target" would render a `bigint` literal without its `n`
            // suffix and hide `string` from every consumer that enumerates this map.
            test "a primitive reprs to its own spelling like any other" {
                for canon, repr in [ "string", "string"; "bigint", "bigint"; "undefined", "undefined" ] do
                    Expect.equal (tryRepr canon) (Some repr) (sprintf "canon '%s' must extract JS repr '%s'" canon repr)
            }

            test "`float` reprs to `number`, which is what licenses the covariant read" {
                Expect.equal
                    (tryRepr "float")
                    (Some "number")
                    "the covariant `number -> float` target requires `float -> number` in the forward axis"
            }
        ]
