module XParsec.FSharp.Codegen.Js.Tests.ImportFormLoweringTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers
open XParsec.FSharp.Codegen.Js.Tests.SchemaDsl

// The CommonJS (`export =`) and Namespace import forms, plus the `node/* → Node.*`
// namespace mount, each carried on an OVERLOADED free function so the synthetic grouping
// type has to carry the import form. Asserted on the JS text: these pin lowering.

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

/// Emit through a single-manifest provider with a stub runtime module for `pkg`, so the
/// erase's import resolves (the synthetic package has no `.toml` asset).
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
                // `node/fs` mounts its exports under `Node.Fs` but is not a global home, so
                // `Node.Fs.readFileSync("p")` must emit a REAL import rather than the
                // bare-name, no-import lowering a global pack gets.
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

                Expect.stringContains
                    js
                    "$Node_Fs_readFileSync"
                    (sprintf "expected the `Node.Fs`-mounted alias, got:\n%s" js)
            }
        ]
