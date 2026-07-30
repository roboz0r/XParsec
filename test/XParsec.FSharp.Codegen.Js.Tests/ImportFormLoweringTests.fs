module XParsec.FSharp.Codegen.Js.Tests.ImportFormLoweringTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers
open XParsec.FSharp.Codegen.Js.Tests.SchemaDsl

// W2 isolation fixtures (systematic-tests-first): the CommonJS (`export =`) and
// Namespace import forms + the `node/* → Node.*` namespace mount. Each is hand-built
// and asserted on the emitted JS TEXT (no Node round-trip — these pin lowering, not
// runtime).
//
// The overloaded-free-function shape (an `export =` module whose value is an overloaded
// function) is the node hard-blocker: the extractor brands it `ImportShape.CommonJsExport`
// but the provider formerly (a) collapsed it to `Named` and (b) THREW in
// `buildOverloadGroupingTypes`. Here the grouping type carries the group's import form
// on its flags, and `erasedGroupingRef` lowers it faithfully.

let private strT = named "string"
let private floatT = named "float"

/// An OVERLOADED free function `f` (`string | float`) → the provider mints a synthetic
/// erased grouping type whose static members are the overloads. `import` selects the
/// erase's import shape.
let private overloadedFnManifest (pkg: string) (fnName: string) (import: Schema.ImportShape) : Schema.PackageManifest =
    {
        SchemaVersion = Schema.SchemaVersion
        Package = pkg
        Version = None
        Exports =
            [
                Schema.Export.Function(fnName, [ sig1 "x" strT strT; sig1 "x" floatT strT ], import)
            ]
        Diagnostics = []
        Refs = []
    }

/// Emit `input` through a single-manifest provider, injecting a stub runtime module for
/// `pkg` so the erase's `addRef` import resolves (the synthetic package has no `.toml`
/// asset — the import contract is what these tests pin).
let private emitOverload (man: Schema.PackageManifest) (fileName: string) (input: string) : string =
    emitWith (contractTs man) (Map.ofList [ man.Package, { FileName = fileName; Source = "" } ]) false input

[<Tests>]
let tests =
    testList
        "ImportFormLowering"
        [
            test "an overloaded `export =` module lowers to a DEFAULT import + bare-erased call" {
                // `Package = "m"` → grouping type `M`; `export = f` → `ImportShape.CommonJsExport`.
                // Under esModuleInterop `export =` binds `module.exports` to a DEFAULT import,
                // so `M.f("hi")` erases to `import $_f from "./m.mjs"; $_f("hi")`.
                let js =
                    emitOverload (overloadedFnManifest "m" "f" Schema.ImportShape.CommonJsExport) "m.mjs" "M.f(\"hi\")"

                Expect.stringContains
                    js
                    "import $_f from \"./m.mjs\""
                    (sprintf "expected a DEFAULT import of the `export =` module, got:\n%s" js)

                Expect.stringContains
                    js
                    "$_f(\"hi\")"
                    (sprintf "expected the erased bare call `$_f(\"hi\")`, got:\n%s" js)

                Expect.isFalse
                    (js.Contains "f as ")
                    (sprintf "a CommonJS `export =` must NOT emit a named specifier, got:\n%s" js)

                Expect.isFalse
                    (js.Contains "M_f")
                    (sprintf "the grouping type must ERASE — no mangled `M_f`, got:\n%s" js)
            }

            test "an overloaded Namespace module lowers to `import * as ns` + `ns.member` call" {
                // `Package = "m2"` → grouping type `M2`; `ImportShape.Namespace` → the exports are
                // read off a namespace object: `import * as $ns_m2 from "./m2.mjs"; $ns_m2.g("hi")`.
                let js =
                    emitOverload (overloadedFnManifest "m2" "g" Schema.ImportShape.Namespace) "m2.mjs" "M2.g(\"hi\")"

                Expect.stringContains
                    js
                    "import * as $ns_m2 from \"./m2.mjs\""
                    (sprintf "expected a namespace-object import, got:\n%s" js)

                Expect.stringContains
                    js
                    "$ns_m2.g(\"hi\")"
                    (sprintf "expected the erased member call `$ns_m2.g(\"hi\")`, got:\n%s" js)
            }

            test "a node/* home MOUNTS under Node.* yet still emits a real import (split of the mount from is-global)" {
                // `Package = "node/fs"` mounts its exports under `Node.Fs` (mount axis) but is NOT
                // a global home (import axis), so `Node.Fs.readFileSync("p")` must emit a REAL
                // import — NOT the bare-name, no-import lowering a global pack (es2015) gets.
                let nodeFsManifest: Schema.PackageManifest =
                    {
                        SchemaVersion = Schema.SchemaVersion
                        Package = "node/fs"
                        Version = None
                        Exports =
                            [
                                Schema.Export.Function(
                                    "readFileSync",
                                    [ sig1 "path" strT strT ],
                                    Schema.ImportShape.Named
                                )
                            ]
                        Diagnostics = []
                        Refs = []
                    }

                let runtime =
                    Map.ofList
                        [
                            "node/fs",
                            {
                                FileName = "fs.mjs"
                                Source = "export function readFileSync(p) { return p; }\n"
                            }
                        ]

                let js =
                    emitWith (contractTs nodeFsManifest) runtime false "Node.Fs.readFileSync(\"p\")\n"

                Expect.stringContains
                    js
                    "import"
                    (sprintf "a node module mounts but is NOT global — it must emit a real import, got:\n%s" js)

                Expect.stringContains
                    js
                    "./fs.mjs"
                    (sprintf "the import must name the node module's runtime, got:\n%s" js)

                // The mount homes the export under `Node.Fs` — the alias reflects that path.
                Expect.stringContains
                    js
                    "$Node_Fs_readFileSync"
                    (sprintf "expected the `Node.Fs`-mounted alias, got:\n%s" js)
            }
        ]
