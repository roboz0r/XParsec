module XParsec.FSharp.Codegen.Js.Tests.IntrinsicForwardReprJsTests

open Expecto

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// The forward intrinsic axis `{ canon -> platform-repr }` must be present on the
// JS-target contract stack exactly as it is on CLR — harvested from the same
// `<base>.js.fs` `(# "<repr>" #)` bindings (`prim-types-min.js.fs` etc.) that yield
// the reverse axis. It is the datum the G1 covariant `number -> float` relocation
// asserts against (a JS `number` read as a value is `float` PRECISELY BECAUSE
// `float` reprs to `number`); if these canons ever stopped repring to `number` the
// covariant target would be a lie, so pin the mapping here.

/// The JS-native contract stack's harvested forward `{ canon -> platform-repr }` map —
/// the single source of each primitive's JS representation (the mirror of the CLR-side
/// `IntrinsicReprConformanceTests`).
let private forwardRepr = jsProvider.Value.IntrinsicForwardRepr

/// Look a bare canon name up in the forward axis, which is now keyed by the qualified
/// intrinsic `SymbolKey` (`Vesper.int`) — bridge from the bare `.fsi` name.
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
                // int / float / float32 (and the sub-32-bit integers) all project to the
                // JS `number` tag — the relation `numericFamilyOr` widens off (reverse)
                // and the covariant `number -> float` target rests on (forward).
                for canon in [ "int"; "float"; "float32" ] do
                    Expect.equal
                        (tryRepr canon)
                        (Some "number")
                        (sprintf "canon '%s' must harvest JS repr 'number'" canon)
            }

            test "`bool` reprs to `boolean`" { Expect.equal (tryRepr "bool") (Some "boolean") "bool -> boolean" }

            test "`float` reprs to `number` — the covariant target's licensing datum" {
                // The exact fact the G1 covariant relocation asserts at construction:
                // naming `float` as `number`'s covariant value-read target is only sound
                // because `float` itself reprs to `number` on this target.
                Expect.equal
                    (tryRepr "float")
                    (Some "number")
                    "the covariant `number -> float` target requires `float -> number` in the forward axis"
            }
        ]
