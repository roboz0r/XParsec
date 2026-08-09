module XParsec.FSharp.Codegen.Js.Tests.MethodAxisGenericTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers
open XParsec.FSharp.Codegen.Js.Tests.SchemaDsl

// A METHOD-AXIS generic member, resolved and emitted and run. `Id.identity<U>(x: U): U`
// is called at TWO types in one program; both type-check and run only if `U` is
// instantiated independently per call, rather than solved once for the member.

let private floatT = named "float"

/// `idlib`: a class `Id` whose static `identity` carries the generic arity-1
/// `identity<U>(x: U): U` under test, and an arity-2 `(float, float)` overload that is
/// never selected but puts the call on the multi-candidate overload-commit path.
let private idManifest: Schema.PackageManifest =
    {
        SchemaVersion = Schema.SchemaVersion
        Package = "idlib"
        Version = None
        Exports =
            [
                Schema.Export.Class(
                    "Id",
                    0,
                    [
                        staticMethod'
                            "identity"
                            [
                                sigG 1 [ param' "x" (methodTypar 0) ] (methodTypar 0)
                                sig2 "x" floatT "y" floatT floatT
                            ]
                    ],
                    [],
                    Schema.ImportShape.Named,
                    []
                )
            ]
        Diagnostics = []
        Refs = []
    }

let private idContract = contractTs idManifest

/// Hand-authored runtime backing the `idlib` manifest. A single-argument external
/// static-member call passes its argument directly (`$Id_identity(x)`).
let private idRuntimeSource = "export function Id_identity(x) { return x; }\n"

// The same generic method at int and then at string, in one program.
let private program =
    String.concat
        "\n"
        [
            "let a = Id.identity(5)"
            "let b = Id.identity(\"hi\")"
            "printfn \"%d %s\" a b"
            ""
        ]

let private emitWithId (input: string) : string =
    emitWith
        idContract
        (Map.ofList
            [
                "idlib",
                {
                    FileName = "idlib.mjs"
                    Source = idRuntimeSource
                }
            ])
        false
        input

[<Tests>]
let tests =
    testList
        "MethodAxisGeneric"
        [
            test "a method-axis generic member freshens per call and round-trips under Node" {
                let js = emitWithId program

                Expect.isTrue
                    (js.Contains "Id_identity")
                    (sprintf "expected a mangled static-member ref `Id_identity`, got:\n%s" js)

                expectNodeOutput
                    "method-axis-generic"
                    [ "method-axis-generic.mjs", js; "idlib.mjs", idRuntimeSource ]
                    "5 hi"
            }
        ]
