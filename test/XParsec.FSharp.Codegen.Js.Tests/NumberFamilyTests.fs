module XParsec.FSharp.Codegen.Js.Tests.NumberFamilyTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers
open XParsec.FSharp.Codegen.Js.Tests.SchemaDsl

// TS `number` is wider than any single Vesper numeric, so a foreign parameter typed `number`
// admits any of int / float / float32, which all carry platform repr `number`. The widening is
// contravariant and confined to the foreign-call seam: a genuine `float` parameter stays strict.

/// `cfglib`: a free function `configure(x: number): unit`, the scalar `number` parameter that
/// must absorb the whole numeric family at the argument position.
let private manifest: Schema.PackageManifest =
    {
        SchemaVersion = Schema.SchemaVersion
        Package = "cfglib"
        Version = None
        Exports =
            [
                Schema.Export.Function(
                    "configure",
                    [ sig1 "x" (named "number") (named "unit") ],
                    Schema.ImportShape.Named
                )
                // A `number`-typed PROPERTY, read covariantly: a JS `number` value IS a Vesper
                // `float`, so `b.size` must type as `float`, not as the opaque `number` token.
                Schema.Export.Interface("Box", 0, [ property' "size" (named "number") ], [], [])
                Schema.Export.Function("makeBox", [ sig0 (named "Box") ], Schema.ImportShape.Named)
            ]
        Diagnostics = []
        Refs = []
    }

let private provider: IExternalSymbolProvider = stackTs manifest

let private analyseErrors (input: string) : string list =
    analyseWith provider input |> List.map (fun d -> d.Message)

[<Tests>]
let tests =
    testList
        "NumberFamily"
        [
            test "an `int` argument satisfies a `number` parameter" {
                let errs = analyseErrors "configure 5\n"
                Expect.isEmpty errs (sprintf "int should widen into number, got: %A" errs)
            }

            test "a `float` argument satisfies a `number` parameter" {
                let errs = analyseErrors "configure 5.0\n"
                Expect.isEmpty errs (sprintf "float should widen into number, got: %A" errs)
            }

            test "a `float32` argument satisfies a `number` parameter" {
                let errs = analyseErrors "configure 5.0f\n"
                Expect.isEmpty errs (sprintf "float32 should widen into number, got: %A" errs)
            }

            test "a `string` argument is rejected by a `number` parameter" {
                let errs = analyseErrors "configure \"hi\"\n"
                Expect.isNonEmpty errs "a non-numeric argument must NOT widen into number"
            }

            test "a `number` property read binds to a `float` annotation (covariant identity)" {
                let errs = analyseErrors "let b = makeBox()\nlet s: float = b.size\n"
                Expect.isEmpty errs (sprintf "a number property read should type as float, got: %A" errs)
            }

            test "a `number` property read is usable in `float` arithmetic" {
                let errs = analyseErrors "let b = makeBox()\nlet s = b.size + 1.0\n"
                Expect.isEmpty errs (sprintf "a number return should be float-arithmetic-usable, got: %A" errs)
            }

            // A user file may bind any repr string it likes, `number` included; nothing
            // reserves the platform spelling. Shadowing is per DECLARATION, so `meters` hides
            // the provider's `meters` (there is none) and says nothing about its `int`.
            test "a user intrinsic on the `number` repr leaves the family visible" {
                let errs =
                    analyseErrors "type meters = (# \"number\" #)\nconfigure 5\nconfigure 5.0f\n"

                Expect.isEmpty errs (sprintf "a colliding local repr must not narrow the family, got: %A" errs)
            }
        ]
