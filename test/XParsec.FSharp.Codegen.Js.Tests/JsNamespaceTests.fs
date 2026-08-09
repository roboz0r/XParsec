module XParsec.FSharp.Codegen.Js.Tests.JsNamespaceTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers
open XParsec.FSharp.Codegen.Js.Tests.SchemaDsl

// A GLOBAL ref-pack manifest (`Package = "es2015"`) mounts under its Vesper-facing `Js`
// namespace and emits with NO `import`, because the JS runtime provides its types
// intrinsically. A non-global control (`somepkg`) pins that both ride the HOME.

let private unitT = named "unit"
let private intT = named "int"

/// A parameterless `.ctor` member, so `new Js.Widget()` overload-resolves. Its return is
/// the declaring class, so `new` grounds to `Js.Widget`.
let private ctor0: Schema.Member = methodOf ".ctor" false [ sig0 (named "Widget") ]

/// `es2015`: a GLOBAL pack holding a constructible `Widget { ping(): unit }`. It mounts
/// as `Js.Widget`; `new Js.Widget()` must emit the BARE `new Widget()` with no import,
/// and `w.ping()` a native `w.ping()`.
let private es2015Manifest: Schema.PackageManifest =
    {
        SchemaVersion = Schema.SchemaVersion
        Package = "es2015"
        Version = None
        Exports =
            [
                Schema.Export.Class(
                    "Widget",
                    0,
                    [ ctor0; method' "ping" (sig0 unitT) ],
                    [],
                    Schema.ImportShape.Named,
                    []
                )
                // A free function on the global pack: the value-export sibling of the
                // class path, emitting its bare export name with no import.
                Schema.Export.Function("spin", [ sig0 unitT ], Schema.ImportShape.Named)
            ]
        Diagnostics = []
        Refs = []
    }

/// `somepkg`: a NON-global control, whose free function `poke(): unit` must emit a
/// normal `import … from './somepkg.mjs'`.
let private somepkgManifest: Schema.PackageManifest =
    {
        SchemaVersion = Schema.SchemaVersion
        Package = "somepkg"
        Version = None
        Exports = [ Schema.Export.Function("poke", [ sig0 unitT ], Schema.ImportShape.Named) ]
        Diagnostics = []
        Refs = []
    }

let private es2015Contract = contractTs es2015Manifest

let private es2015Provider: IExternalSymbolProvider = es2015Contract.Provider

/// Package `B` references `Widget` with `home = es2015`, so its homed identity must mint
/// under the `Js` namespace as `Js.Widget`: the same qualified name the mounted es2015
/// provider registers, letting the two resolve against each other.
let private manifestB: Schema.PackageManifest =
    {
        SchemaVersion = Schema.SchemaVersion
        Package = "B"
        Version = None
        Exports =
            [
                Schema.Export.Variable("theWidget", named "Widget", true, Schema.ImportShape.Named)
            ]
        Diagnostics = []
        Refs = [ refEntry "Widget" "es2015" Schema.RefKind.Class 0 ]
    }

let private bProviderRaw: IExternalSymbolProvider =
    TsManifestProvider.providerOfManifest manifestB

/// Emit with NO injected runtime modules: a global pack records no import, so it needs none.
let private emitGlobal (contract: SymbolProviders.Contract) (input: string) : string =
    emitWith contract Map.empty false input

[<Tests>]
let tests =
    testList
        "JsNamespace"
        [
            test "(a) a global-pack class mounts as Js.Widget and constructs with the bare name, no import" {
                // `new Js.Widget()` resolves the mounted annotation and emits the BARE
                // export `new Widget(`. The runtime provides `Widget` intrinsically, so no
                // `import` is written and the wire home `es2015` does not leak.
                let js = emitGlobal es2015Contract "let w = new Js.Widget()\n"

                Expect.stringContains js "new Widget(" (sprintf "expected bare `new Widget(`, got:\n%s" js)

                Expect.isFalse
                    (js.Contains "import")
                    (sprintf "a global-pack construction must emit NO import, got:\n%s" js)

                Expect.isFalse
                    (js.Contains "es2015")
                    (sprintf "the wire home `es2015` must not leak into the output, got:\n%s" js)
            }

            test "(b) member access on a mounted global type resolves and emits a native objArg.member call" {
                // `w.ping()` on the `Js.Widget` value lowers to a native `w.ping()`, still
                // with no import.
                let js = emitGlobal es2015Contract "let w = new Js.Widget()\nw.ping()\n"

                Expect.stringContains js ".ping(" (sprintf "expected the native member call `.ping(`, got:\n%s" js)

                Expect.isFalse
                    (js.Contains "import")
                    (sprintf "member access on a global type must add no import, got:\n%s" js)
            }

            test "(b') a global-pack free function emits its bare export name with no import (addRef skip)" {
                // The value-export sibling: the mounted `Js.spin()` emits the BARE `spin()`,
                // because import recording is skipped for a global home.
                let js = emitGlobal es2015Contract "Js.spin()\n"

                Expect.stringContains js "spin(" (sprintf "expected bare `spin(`, got:\n%s" js)

                Expect.isFalse
                    (js.Contains "import")
                    (sprintf "a global-pack free function must emit no import, got:\n%s" js)
            }

            test "(c) CONTROL: a non-global package's free function still emits its normal import" {
                // `somepkg` is not a global home, so `poke()` is imported normally from its
                // runtime module: the no-import behaviour fires only for a global pack.
                let runtime =
                    Map.ofList
                        [
                            "somepkg", JsRuntimeModule.ofSource "somepkg.mjs" "export function poke() {}\n"
                        ]

                let js = emitWith (contractTs somepkgManifest) runtime false "poke()\n"

                Expect.stringContains
                    js
                    "import"
                    (sprintf "a non-global package call must emit a normal import, got:\n%s" js)

                Expect.stringContains
                    js
                    "./somepkg.mjs"
                    (sprintf "the import must name the package's runtime module, got:\n%s" js)
            }

            test "(d) a refs entry homed to es2015 mints an FTClass under the Js namespace (Js.Widget)" {
                // A ref whose `home = es2015` mints under the `Js` namespace, so its
                // qualified name is `Js.Widget`, not the bare `Widget`. The home rides the
                // SHAPE, not the key, so B's key must resolve to an es2015-homed shape.
                match bProviderRaw.TryLookup "theWidget" with
                | ValueSome sym ->
                    match sym.Scheme with
                    | FTClass(key, _) ->
                        Expect.equal
                            (SymbolKeyOps.typeMetaName key)
                            "Js.Widget"
                            "the homed ref must mint under the Js namespace (globalLibHomes), not bare Widget"

                        match (es2015Provider :> IExternalSymbolStore).TryLookupType(SymbolKey.Type key) with
                        | ValueSome(ExternalTypeShape.Class info) ->
                            Expect.equal
                                info.Origin.Home.AssemblyOption
                                (ValueSome "es2015")
                                "the key B minted resolves against the mounted pack to a shape homed in es2015"
                        | other -> failtestf "the homed ref must resolve to es2015's Widget class shape, got %A" other
                    | other -> failtestf "theWidget scheme should be an FTClass, got %A" other
                | ValueNone -> failtest "theWidget did not resolve as a value symbol"
            }

            test "(d') with the es2015 pack stacked under B, member access on the homed Widget resolves" {
                // Once the es2015 pack is stacked, the homed `Js.Widget` identity has a
                // shape, so `ping` resolves through the ordinary provider stack.
                let provider = stackTsMany [ manifestB; es2015Manifest ]
                let errors = analyseWith provider "let w = theWidget\nw.ping()\n"

                Expect.isEmpty
                    errors
                    (sprintf "member access on the homed Js.Widget should type-check, got:\n%A" (errorText errors))
            }
        ]
