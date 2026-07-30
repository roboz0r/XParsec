module XParsec.FSharp.Codegen.Js.Tests.MethodAxisSingleCandidateTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers
open XParsec.FSharp.Codegen.Js.Tests.SchemaDsl

// TS provider: the SINGLE-candidate `TryLookupMember`
// field-walk must freshen a member's method typars per call site, exactly as the
// multi-candidate overload-commit path already does
// (`ExternalSymbols.instantiateSignature` via `commitExternalOverload`).
//
// The sibling `MethodAxisGenericTests` pins the MULTI-candidate path: `Id.identity`
// carries a sham second overload precisely to force `on`-style overload commit, the
// only site that historically freshened. Here `echo` has ONE signature, so
// `tryInferExternalInstanceMethodCall` DECLINES (candidates.Length <= 1) and the call
// falls to `resolveFieldStep`'s external-`TyClass` field-walk — the path that used
// `ExternalSymbols.openSignature` (method typars left as inert `TyTypar(Method,_)`
// markers a per-call solution can never touch). Calling `echo` at TWO instantiations
// (int, string) in one program is the discriminator: shared inert markers cross-
// contaminate (or fail rigid unification), independent fresh `TyVar`s type both.

/// `boxlib`: an interface `Box` with a SINGLE generic instance method
/// `echo<U>(x: U): U` (one signature, so the single-candidate field-walk path), plus a
/// `makeBox(): Box` factory.
let private echoManifest: Schema.PackageManifest =
    {
        SchemaVersion = Schema.SchemaVersion
        Package = "boxlib"
        Version = None
        Exports =
            [
                Schema.Export.Interface(
                    "Box",
                    0,
                    [ method' "echo" (sigG 1 [ param' "x" (methodTypar 0) ] (methodTypar 0)) ],
                    [],
                    []
                )
                Schema.Export.Function("makeBox", [ sig0 (named "Box") ], Schema.ImportShape.Named)
            ]
        Diagnostics = []
        Refs = []
    }

let private echoContract = contractTs echoManifest

let private echoProvider: IExternalSymbolProvider = echoContract.Provider

/// Hand-authored runtime backing `boxlib`: `makeBox()` yields an object whose
/// `echo` instance method is the identity (so the native `receiver.member(args)`
/// lowering round-trips the argument unchanged).
let private echoRuntimeSource =
    "export function makeBox() { return { echo(x) { return x; } }; }\n"

// A SINGLE generic instance method, called at int then at string on one receiver:
// `U` must freshen per call, or one of the two uses fails to type.
let private program =
    String.concat
        "\n"
        [
            "let b = makeBox()"
            "let a = b.echo(5)"
            "let c = b.echo(\"hi\")"
            "printfn \"%d %s\" a c"
            ""
        ]

let private analyseErrors (input: string) : string list =
    analyseWith echoProvider input |> List.map (fun d -> d.Message)

let private emitWithEcho (input: string) : string =
    emitWith
        echoContract
        (Map.ofList
            [
                "boxlib",
                {
                    FileName = "boxlib.mjs"
                    Source = echoRuntimeSource
                }
            ])
        false
        input

[<Tests>]
let tests =
    testList
        "MethodAxisSingleCandidate"
        [
            test "a single-candidate instance generic member freshens per call (analysis)" {
                // The discriminator: with the field-walk leaving `U` an inert
                // `TyTypar(Method,0)` marker, calling `echo` at int and string shares one
                // marker and cannot solve both — analysis errors. With per-call freshening
                // both calls type cleanly.
                let errors = analyseErrors program

                Expect.isEmpty
                    errors
                    (sprintf "single-candidate method typar must freshen per call, got errors:\n%A" errors)
            }

            test "a single-candidate instance generic member freshens per call and round-trips under Node" {
                let js = emitWithEcho program

                Expect.isTrue
                    (js.Contains "echo")
                    (sprintf "expected the instance member call `.echo(...)`, got:\n%s" js)

                expectNodeOutput
                    "method-axis-single"
                    [ "method-axis-single.mjs", js; "boxlib.mjs", echoRuntimeSource ]
                    "5 hi"
            }
        ]
