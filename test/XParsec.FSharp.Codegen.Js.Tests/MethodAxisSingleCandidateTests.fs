module XParsec.FSharp.Codegen.Js.Tests.MethodAxisSingleCandidateTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers
open XParsec.FSharp.Codegen.Js.Tests.SchemaDsl

// Overload commit declines a member with ONE signature, so its method typars are freshened
// on the single-candidate field-walk instead. `echo<U>(x: U): U` called at int and then at
// string is the discriminator: a single shared `U` cannot solve both.

/// `boxlib`: an interface `Box` with the single generic instance method
/// `echo<U>(x: U): U`, plus a `makeBox(): Box` factory.
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

/// Hand-authored runtime backing `boxlib`: `makeBox()` yields an object whose `echo`
/// instance method is the identity, so a round-trip prints the argument unchanged.
let private echoRuntimeSource =
    "export function makeBox() { return { echo(x) { return x; } }; }\n"

// One object, `echo` called at int and then at string.
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
    emitWith echoContract (Map.ofList [ "boxlib", JsRuntimeModule.ofSource "boxlib.mjs" echoRuntimeSource ]) false input

[<Tests>]
let tests =
    testList
        "MethodAxisSingleCandidate"
        [
            test "a single-candidate instance generic member freshens per call (analysis)" {
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
