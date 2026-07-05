module XParsec.FSharp.Codegen.Js.Tests.StructuralWidenE2ETests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers
open XParsec.FSharp.Codegen.Js.Tests.SchemaDsl

// G1 end-to-end: a Vesper record flows INTO a foreign `configure(opts: Options)` by
// structural width and RUNS under Node. The record emits verbatim as a POJO, the foreign
// function reads its fields, and `int` satisfies the interface's `number` member — the
// options/config-object call shape, proven through emit + execution (not just analysis).

/// `cfglib3`: `configure(opts: Options): string` where `Options { retries: number;
/// label: string }`. The runtime formats the two fields so the round-trip observes that
/// the Vesper record reached the foreign function intact.
let private manifest: Schema.PackageManifest =
    {
        SchemaVersion = Schema.SchemaVersion
        Package = "cfglib3"
        Version = None
        Exports =
            [
                Schema.Export.Interface(
                    "Options",
                    0,
                    [ property' "retries" (named "number"); property' "label" (named "string") ],
                    [],
                    []
                )
                Schema.Export.Function(
                    "configure",
                    [ sig1 "opts" (named "Options") (named "string") ],
                    Schema.ImportShape.Named
                )
            ]
        Diagnostics = []
        Refs = []
    }

let private provider: IExternalSymbolProvider = stackTs manifest

/// A POJO-consuming runtime: `configure` reads `opts.label` / `opts.retries` off the
/// object it is handed — only a verbatim record (native own props) round-trips here.
let private runtime =
    "export function configure(opts) { return opts.label + \":\" + opts.retries; }\n"

let private emitCfg (input: string) : string =
    emitWith
        provider
        (Map.ofList
            [
                "cfglib3",
                {
                    FileName = "cfglib3.mjs"
                    Source = runtime
                }
            ])
        true
        input

let private program =
    String.concat
        "\n"
        [
            "type Cfg = { retries: int; label: string }"
            "let result = configure { retries = 3; label = \"go\" }"
            ""
        ]

let private harness =
    "import { result } from \"./cfg-program.mjs\";\nconsole.log(result);\n"

[<Tests>]
let tests =
    testList
        "StructuralWidenE2E"
        [
            test "a Vesper record widens into a foreign interface parameter (analysis)" {
                let errors = analyseWith provider program |> List.map (fun d -> d.Message)
                Expect.isEmpty errors (sprintf "the config-object call must type-check, got:\n%A" errors)
            }

            test "the record emits verbatim and the foreign `configure` round-trips under Node" {
                let js = emitCfg program

                // The record is passed straight into the foreign call — no @struct home,
                // no interface class decl leaks into the output.
                Expect.stringContains js "configure(" (sprintf "expected a `configure(` call, got:\n%s" js)
                Expect.isFalse (js.Contains "@struct") (sprintf "no @struct home may appear in emit:\n%s" js)

                expectNodeOutput
                    "structural-widen-e2e"
                    [ "harness.mjs", harness; "cfg-program.mjs", js; "cfglib3.mjs", runtime ]
                    "go:3"
            }
        ]
