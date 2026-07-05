module XParsec.FSharp.Codegen.Js.Tests.NullUndefinedTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers
open XParsec.FSharp.Codegen.Js.Tests.SchemaDsl

// `null` / `undefined` as first-class intrinsic types. A TS `T | null` /
// `T | undefined` rides in as a `TyOr`/`FTOr` member; this pins that such a type
// resolves through the provider, survives JS emit (no `PlatformTypes` reject), and
// runs under Node round-tripping present / absent values.
//
// `N` is a class with FOUR static methods (the class path stamps a real assembly
// origin onto the member key, unlike the v1 free-function path):
//   pickName   : bool -> (string | null)
//   pickMiddle : bool -> (string | undefined)
//   renderN    : (string | null)      -> string
//   renderU    : (string | undefined) -> string
// The renderers' parameter type EXACTLY matches each producer's return, so no
// union-assignability is exercised — only the type's end-to-end survival.

/// `nulllib`: a class `N` with four static methods. `pickName`/`renderN` traffic in
/// `string | null`; `pickMiddle`/`renderU` in `string | undefined`. Each renderer's
/// parameter EXACTLY matches its producer's return, so no union-assignability is
/// exercised — only the types' end-to-end survival.
let private boolT = named "bool"
let private stringT = named "string"
let private nullUnion = union [ named "null"; stringT ]
let private undefUnion = union [ named "undefined"; stringT ]

let private nullManifest: Schema.PackageManifest =
    {
        SchemaVersion = Schema.SchemaVersion
        Package = "nulllib"
        Version = None
        Exports =
            [
                Schema.Export.Class(
                    "N",
                    0,
                    [
                        staticMethod' "pickName" [ sig1 "present" boolT nullUnion ]
                        staticMethod' "pickMiddle" [ sig1 "present" boolT undefUnion ]
                        staticMethod' "renderN" [ sig1 "value" nullUnion stringT ]
                        staticMethod' "renderU" [ sig1 "value" undefUnion stringT ]
                    ],
                    [],
                    Schema.ImportShape.Named,
                    []
                )
            ]
        Diagnostics = []
        Refs = []
    }

let private nullProvider: IExternalSymbolProvider = stackTs nullManifest

/// Hand-authored JS runtime backing the `nulllib` manifest. A single-argument
/// external static-member call passes its argument directly (`$N_renderN(value)`),
/// so each flat export takes one plain parameter.
let private nullRuntimeSource =
    """export function N_pickName(present) { return present ? "hi" : null; }
export function N_pickMiddle(present) { return present ? "yo" : undefined; }
export function N_renderN(value) { return value === null ? "NULL" : value; }
export function N_renderU(value) { return value === undefined ? "UNDEF" : value; }
"""

let private program =
    String.concat
        "\n"
        [
            "let r1 = N.renderN(N.pickName(true))"
            "let r2 = N.renderN(N.pickName(false))"
            "let r3 = N.renderU(N.pickMiddle(true))"
            "let r4 = N.renderU(N.pickMiddle(false))"
            "printfn \"%s %s %s %s\" r1 r2 r3 r4"
            ""
        ]

/// Emit `input` to JS through the `nulllib` provider, injecting the runtime module.
let private emitWithNull (input: string) : string =
    emitWith
        nullProvider
        (Map.ofList
            [
                "nulllib",
                {
                    FileName = "nulllib.mjs"
                    Source = nullRuntimeSource
                }
            ])
        false
        input

[<Tests>]
let tests =
    testList
        "NullUndefined"
        [
            test "`T | null` and `T | undefined` survive JS emit and round-trip under Node" {
                // The emit itself proves the types survive `PlatformTypes` (no
                // "no representation on the target" reject) and JS lowering.
                let js = emitWithNull program

                // r1 present (string), r2 absent (null), r3 present (string), r4 absent (undefined).
                expectNodeOutput
                    "null-undefined"
                    [ "null-undefined.mjs", js; "nulllib.mjs", nullRuntimeSource ]
                    "hi NULL yo UNDEF"
            }
        ]
