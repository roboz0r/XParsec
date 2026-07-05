module XParsec.FSharp.Codegen.Js.Tests.StructuralWidenTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers
open XParsec.FSharp.Codegen.Js.Tests.SchemaDsl

// G1: a Vesper RECORD satisfies an EXTERNAL interface PARAMETER by WIDTH — the
// options/config-object call shape. At a foreign-call argument position, a record whose
// fields cover the interface's REQUIRED members (each field type coercing into the
// member's type) is admitted with no pin. Confined to the argument-coercion seams
// (`Engine.tryStructuralWiden`), gated on the provider's `IsInterface`. An OPTIONAL
// member (`verbose?`) is not required; the `number`-family widening lets an `int` field
// satisfy a `number` member. A missing required field or a mismatched field type rejects.

/// `cfglib2`: an interface `Options` with two required members (`retries: number`,
/// `label: string`) and one optional (`verbose?: bool`), plus a free function
/// `configure(opts: Options): unit` — the parameter a Vesper config record widens into.
let private manifest: Schema.PackageManifest =
    {
        SchemaVersion = Schema.SchemaVersion
        Package = "cfglib2"
        Version = None
        Exports =
            [
                Schema.Export.Interface(
                    "Options",
                    0,
                    [
                        property' "retries" (named "number")
                        property' "label" (named "string")
                        optProperty' "verbose" (named "bool")
                    ],
                    [],
                    None
                )
                Schema.Export.Function(
                    "configure",
                    [ sig1 "opts" (named "Options") (named "unit") ],
                    Schema.ImportShape.Named
                )
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
        "StructuralWiden"
        [
            test "a record covering the required members satisfies the interface parameter" {
                let errs =
                    analyseErrors
                        "type Cfg = { retries: int; label: string }\nconfigure { retries = 1; label = \"x\" }\n"

                Expect.isEmpty errs (sprintf "record should widen into interface, got: %A" errs)
            }

            test "a wider record (extra field) still satisfies the interface parameter" {
                let errs =
                    analyseErrors
                        "type Cfg = { retries: int; label: string; extra: bool }\nconfigure { retries = 1; label = \"x\"; extra = true }\n"

                Expect.isEmpty errs (sprintf "extra field should be ignored, got: %A" errs)
            }

            test "a record missing a required member is rejected" {
                let errs = analyseErrors "type Cfg = { retries: int }\nconfigure { retries = 1 }\n"
                Expect.isNonEmpty errs "a record missing the required `label` member must be rejected"
            }

            test "a record with a wrong-typed member is rejected" {
                let errs =
                    analyseErrors "type Cfg = { retries: int; label: int }\nconfigure { retries = 1; label = 2 }\n"

                Expect.isNonEmpty errs "an `int` field cannot satisfy a `string` member"
            }
        ]
