module XParsec.FSharp.Codegen.Js.Tests.PlatformTypeIdJsTests

open Expecto

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// The `canon -> platform type id` direction, extracted from the `<base>.js.fs` `(# "…" #)`
// bindings. A JS `number` read as a value is `float` only because `float` binds to
// `number`, so pin the mapping.

let private intrinsics = jsProvider.Value.IntrinsicTypeMap

/// Bridge a bare `.fsi` canon name to the axis's qualified intrinsic key (`Vesper.int`).
let private tryPlatform (canon: string) : PlatformTypeId option =
    match IntrinsicTypeMap.tryPlatformTypeId (RuntimeNames.primitiveKey canon) intrinsics with
    | ValueSome typeId -> Some typeId
    | ValueNone -> None

[<Tests>]
let tests =
    testList
        "PlatformTypeIdJs"
        [
            test "the JS number family all bind to `number`" {
                for canon in [ "int"; "float"; "float32" ] do
                    Expect.equal
                        (tryPlatform canon)
                        (Some(PlatformTypeId "number"))
                        (sprintf "canon '%s' must extract JS type id 'number'" canon)
            }

            test "`bool` binds to `boolean`" {
                Expect.equal (tryPlatform "bool") (Some(PlatformTypeId "boolean")) "bool -> boolean"
            }

            // A primitive whose JS type name is spelled like its own canon. Dropping it as
            // "no binding on this target" would render a `bigint` literal without its `n`
            // suffix and hide `string` from every consumer that enumerates this map.
            test "a primitive binds to its own spelling like any other" {
                for canon, typeId in [ "string", "string"; "bigint", "bigint"; "undefined", "undefined" ] do
                    Expect.equal
                        (tryPlatform canon)
                        (Some(PlatformTypeId typeId))
                        (sprintf "canon '%s' must extract JS type id '%s'" canon typeId)
            }

            test "`float` binds to `number`, which is what licenses the covariant read" {
                Expect.equal
                    (tryPlatform "float")
                    (Some(PlatformTypeId "number"))
                    "the covariant `number -> float` target requires `float -> number` in the forward axis"
            }
        ]
