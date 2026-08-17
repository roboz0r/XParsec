module XParsec.FSharp.Codegen.Js.Tests.LiteralUnionTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers
open XParsec.FSharp.Codegen.Js.Tests.SchemaDsl

// A manifest member `setMode(mode: "auto" | "manual")` admits a string CONSTANT, or a
// Vesper string enum whose case values are a subset, by set membership. Admission is
// DIRECTIONAL: a plain `string`-typed variable misses. A literal-union RETURN widens out.

// ─── Hand-built manifest; builders from `SchemaDsl` ────────────────────────────

let private unitT = named "unit"

/// `"auto" | "manual"` — the mode literal union.
let private modeUnion: Schema.TypeRef = union [ strLit "auto"; strLit "manual" ]

/// `widgetlib`: an interface `Widget { setMode(mode: "auto"|"manual"): unit;
/// getMode(): "auto"|"manual" }` plus a factory `makeWidget(): Widget`.
let private widgetManifest: Schema.PackageManifest =
    {
        SchemaVersion = Schema.SchemaVersion
        Package = "widgetlib"
        Version = None
        Exports =
            [
                Schema.Export.Interface(
                    "Widget",
                    0,
                    [
                        method' "setMode" (sig1 "mode" modeUnion unitT)
                        method' "getMode" (sig0 modeUnion)
                    ],
                    [],
                    []
                )
                Schema.Export.Function("makeWidget", [ sig0 (named "Widget") ], Schema.ImportShape.Named)
            ]
        Diagnostics = []
        Refs = []
    }

let private widgetContract = contractTs widgetManifest

let private widgetProvider: IExternalSymbolProvider = widgetContract.Provider

let private analyse (input: string) : Diagnostic list = analyseWith widgetProvider input

// State lives on `this`; `setMode` stashes the received value so a Vesper-only
// program can read back what the native side observed.
let private widgetRuntime =
    String.concat
        "\n"
        [
            "export function makeWidget() {"
            "  return {"
            "    _mode: \"\","
            "    setMode(mode) { this._mode = mode; },"
            "    getMode() { return this._mode; },"
            "  };"
            "}"
            ""
        ]

let private emitWidget (input: string) : string =
    emitWith
        widgetContract
        (Map.ofList [ "widgetlib", JsPackageOutput.rootModule "widgetlib.mjs" widgetRuntime ])
        true
        input

let private resultHarness =
    "import { result } from \"./widget-program.mjs\";\nconsole.log(result);\n"

[<Tests>]
let tests =
    testList
        "LiteralUnion"
        [
            test "(1) a matching string constant admits into a literal-union parameter" {
                let errors =
                    analyse (String.concat "\n" [ "let w = makeWidget()"; "w.setMode(\"auto\")"; "" ])

                Expect.isEmpty errors (sprintf "expected no errors, got:\n%s" (errorText errors))
            }

            test "(2) a non-matching constant errors, naming the allowed set" {
                let errors =
                    analyse (String.concat "\n" [ "let w = makeWidget()"; "w.setMode(\"bogus\")"; "" ])

                Expect.isNonEmpty errors "expected a type error for the out-of-set constant"
                let msg = errorText errors
                Expect.stringContains msg "auto" "the message names the allowed value 'auto'"
                Expect.stringContains msg "manual" "the message names the allowed value 'manual'"
            }

            test "(3) a plain string-typed NON-constant does NOT admit (directional)" {
                // Vesper never mints a literal type, so `let m = "auto"` gives `m` plain
                // `string` and the constant is no longer visible at the arg site.
                let errors =
                    analyse (String.concat "\n" [ "let w = makeWidget()"; "let m = \"auto\""; "w.setMode(m)"; "" ])

                Expect.isNonEmpty errors "a plain string variable must not admit into the literal union (directional)"
            }

            test "(4) a Vesper string enum whose value set ⊆ the union admits" {
                let program =
                    String.concat
                        "\n"
                        [
                            "type Mode = | Auto = \"auto\" | Manual = \"manual\""
                            "let w = makeWidget()"
                            "w.setMode(Mode.Auto)"
                            ""
                        ]

                let errors = analyse program
                Expect.isEmpty errors (sprintf "expected no errors, got:\n%s" (errorText errors))
            }

            test "(4b) parenthesized string case values still admit (shared projection peels the paren)" {
                // `| Auto = ("auto")` is a legal string case: the shared value projection
                // peels the grouping paren, so the case-value set is populated and the
                // enum still admits.
                let program =
                    String.concat
                        "\n"
                        [
                            "type Mode = | Auto = (\"auto\") | Manual = (\"manual\")"
                            "let w = makeWidget()"
                            "w.setMode(Mode.Auto)"
                            ""
                        ]

                let errors = analyse program
                Expect.isEmpty errors (sprintf "expected no errors, got:\n%s" (errorText errors))
            }

            test "(5) a string enum with a case NOT in the union is rejected" {
                let program =
                    String.concat
                        "\n"
                        [
                            "type Bad = | Auto = \"auto\" | Extra = \"extra\""
                            "let w = makeWidget()"
                            "w.setMode(Bad.Auto)"
                            ""
                        ]

                let errors = analyse program
                Expect.isNonEmpty errors "an enum with a value outside the union must be rejected"
            }

            test "(6) a literal-union return widens OUTWARD into a string context" {
                let program =
                    String.concat "\n" [ "let w = makeWidget()"; "let s : string = w.getMode()"; "" ]

                let errors = analyse program
                Expect.isEmpty errors (sprintf "expected no errors (outward widening), got:\n%s" (errorText errors))
            }

            test "E2E: the literal arg emits as a bare JS string and reaches the runtime" {
                // The string constant must emit as the bare JS string, with no wrapper;
                // the runtime stashes it and `getMode` reads it back.
                let program =
                    String.concat
                        "\n"
                        [
                            "let w = makeWidget()"
                            "let u = w.setMode(\"auto\")"
                            "let result = w.getMode()"
                            ""
                        ]

                let js = emitWidget program

                Expect.isTrue (js.Contains "setMode(\"auto\")") (sprintf "expected a bare-string arg, got:\n%s" js)

                expectNodeOutput
                    "widget-e2e"
                    [
                        "harness.mjs", resultHarness
                        "widget-program.mjs", js
                        "widgetlib.mjs", widgetRuntime
                    ]
                    "auto"
            }
        ]
