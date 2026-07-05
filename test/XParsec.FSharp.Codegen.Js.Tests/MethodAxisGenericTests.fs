module XParsec.FSharp.Codegen.Js.Tests.MethodAxisGenericTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers
open XParsec.FSharp.Codegen.Js.Tests.SchemaDsl

// Consumer half: a METHOD-AXIS generic member resolves, emits, and
// runs end-to-end, with its method typar FRESHENED per call site. `Id.identity<U>(x: U)
// : U` is called at TWO different types in one program; both must type-check and run,
// which is only possible if `U` (`FTTypar(TyparAxis.Method, 0)` through the provider,
// `MethodTypar 0` on the wire) is instantiated INDEPENDENTLY per call — the proof that
// the new `MethodTypar` contract bump rides the proven per-call freshening path
// (`ExternalSymbols.instantiateSignature` via `commitExternalOverload`).
//
// `identity` carries a SECOND, deliberately-non-matching overload (`identity(x, y)`, a
// 2-arity sham). It is never selected for the 1-arg calls — its only role is to push
// `identity` onto the multi-candidate overload-commit path, which is the proven site
// that freshens method typars (the single-candidate field-walk does not; staying on the
// existing path here respects the `feedback_codegen_js_owns_assignability` guardrail
// against unifier/overload edits). A class STATIC method is used deliberately: the class
// path stamps a real assembly origin onto the member key (the v1 free-function path
// leaves `asm=None`).

/// `idlib`: a class `Id` with a static `identity` carrying TWO overloads — the
/// generic arity-1 `identity<U>(x: U): U` (the method-axis typar under test) and a
/// deliberately-non-matching arity-2 `identity(x: float, y: float): float` sham that
/// forces the multi-candidate overload-commit path (see the header note).
let private floatT = named "float"

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
                    None
                )
            ]
        Diagnostics = []
        Refs = []
    }

let private idProvider: IExternalSymbolProvider = stackTs idManifest

/// Hand-authored runtime backing the `idlib` manifest. A single-argument external
/// static-member call passes its argument directly (`$Id_identity(x)`).
let private idRuntimeSource = "export function Id_identity(x) { return x; }\n"

// Call the SAME generic method at int and at string in one program — `U` must freshen
// per call (int for the first, string for the second) or the second use fails to type.
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
        idProvider
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
                // The emit itself proves `Id.identity` resolves at TWO instantiations of its
                // method typar (int, string) — a single-axis collapse would fail one of them.
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
