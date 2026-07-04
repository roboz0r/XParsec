module XParsec.FSharp.Codegen.Js.Tests.TypeArgNumberTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers
open XParsec.FSharp.Codegen.Js.Tests.SchemaDsl

// G1 Step 4 — a TS `number` in a generic ARGUMENT position (invariant) resolves to the
// repr-family UNION `int|float|float32|…`, NOT the covariant scalar `float`. An invariant
// slot must admit family WRITES and family READS under one type, so neither the covariant
// `float` (Step 3, a scalar read) nor the bare `number` token will do. The family is read
// off the reverse intrinsic axis — the same source `numericFamilyOr` widens a scalar param
// off — so a `Box<number>` value read is exactly the family a `number` PARAMETER admits.
//
// This is DISTINCT from the scalar covariant identity (`Box.size : number` → `float`,
// `NumberFamilyTests`): a generic `Box<number>.value` reads as the UNION, demonstrably not
// scalar `float`. Known deferred caveat (per the plan): a covariant element read yields the
// union, so downstream float arithmetic needs a narrow — not exercised here.

/// `boxlib`: a generic `interface Box<T> { value: T }`, a `makeNumBox(): Box<number>`
/// factory (the type-arg `number` under test), and a scalar `consume(x: number)` the
/// union must still flow into (family absorption at the arg seam).
let private manifest: Schema.PackageManifest =
    {
        SchemaVersion = Schema.SchemaVersion
        Package = "boxlib"
        Version = None
        Exports =
            [
                Schema.Export.Interface("Box", 1, [ property' "value" (typar 0) ], [])
                Schema.Export.Function(
                    "makeNumBox",
                    [ sig0 (namedG "Box" [ named "number" ]) ],
                    Schema.ImportShape.Named
                )
                Schema.Export.Function(
                    "consume",
                    [ sig1 "x" (named "number") (named "unit") ],
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
        "TypeArgNumber"
        [
            test "a `Box<number>` value read flows into a `number` parameter (family absorption)" {
                // The invariant type-arg resolves to the SAME family a `number` parameter
                // widens to, so the union read is admitted at the scalar arg seam.
                let errs = analyseErrors "let b = makeNumBox()\nconsume b.value\n"
                Expect.isEmpty errs (sprintf "a Box<number> read should satisfy a number parameter, got: %A" errs)
            }

            test "a `Box<number>` value read does NOT type as scalar `float`" {
                // The distinguishing pin: were the type-arg resolved to the covariant scalar
                // `float` (the Step-3 rule), this would succeed. As the invariant UNION it is
                // wider than `float` and must NOT assign to a strict `float` annotation.
                let errs = analyseErrors "let b = makeNumBox()\nlet s: float = b.value\n"
                Expect.isNonEmpty errs "a Box<number> read is the repr-family union, not scalar float"
            }

            test "a `Box<number>` value read does NOT type as scalar `int`" {
                let errs = analyseErrors "let b = makeNumBox()\nlet n: int = b.value\n"
                Expect.isNonEmpty errs "a Box<number> read is the repr-family union, not scalar int"
            }
        ]
