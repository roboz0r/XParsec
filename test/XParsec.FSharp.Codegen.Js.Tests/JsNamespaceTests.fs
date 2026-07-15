module XParsec.FSharp.Codegen.Js.Tests.JsNamespaceTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers
open XParsec.FSharp.Codegen.Js.Tests.SchemaDsl

// TS-provider isolation fixtures, systematic-tests-first: a hand-built
// GLOBAL ref-pack manifest (`Package = "es2015"`, an entry of
// `TsGlobalHomes.globalLibHomes`) is MOUNTED under its Vesper-facing `Js` namespace
// AND emits with NO `import` — the JS runtime provides its types intrinsically. A
// NON-global control (`Package = "somepkg"`) pins that `Global` rides the HOME: the
// mounting/no-import fires only for a global-pack home.
//
// These manifests are hand-built and collision-free — the REAL es2015 pack (stacked
// by the `Js.Map` / mitt gates) has a ctor-merge collision that would throw on load, so
// nothing here runs Node; every assertion is on the emitted JS TEXT.

let private unitT = named "unit"
let private intT = named "int"

/// A parameterless `.ctor` member (the seam constructor the provider's `expandCtor`
/// keys off `Name = ".ctor"`), so `new Js.Widget()` overload-resolves. Its return is
/// the declaring class (the seam ctor codomain), so `new` grounds to `Js.Widget`.
let private ctor0: Schema.Member = methodOf ".ctor" false [ sig0 (named "Widget") ]

/// `es2015`: a GLOBAL pack (its home is in `globalLibHomes`). A constructible class
/// `Widget { ping(): unit }`. Mounts as `Js.Widget`; `new Js.Widget()` must emit the
/// BARE `new Widget()` with no import, and `w.ping()` a native `w.ping()`.
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
                // A free function on the global pack, to exercise the `addRef` Global-skip
                // (bare export name, no import) — the value-export sibling of the class path.
                Schema.Export.Function("spin", [ sig0 unitT ], Schema.ImportShape.Named)
            ]
        Diagnostics = []
        Refs = []
    }

/// `somepkg`: a NON-global control (its home is ABSENT from `globalLibHomes`). A free
/// function `poke(): unit` whose call must emit a NORMAL `import … from './somepkg.mjs'`.
let private somepkgManifest: Schema.PackageManifest =
    {
        SchemaVersion = Schema.SchemaVersion
        Package = "somepkg"
        Version = None
        Exports = [ Schema.Export.Function("poke", [ sig0 unitT ], Schema.ImportShape.Named) ]
        Diagnostics = []
        Refs = []
    }

let private es2015Provider: IExternalSymbolProvider = stackTs es2015Manifest

/// Package B (refs-table shape): references `Widget` with `home = es2015`, so its
/// homed identity must mint under the `Js` namespace (`Js.Widget`) — the same qualified
/// name the mounted es2015 provider registers, letting the two resolve against each other.
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

/// Emit `input` through `provider` with NO injected runtime modules (a global pack needs
/// none — that is the whole point). Global emit records no import, so `entryFor` is never hit.
let private emitGlobal (provider: IExternalSymbolProvider) (input: string) : string =
    emitWith provider Map.empty false input

[<Tests>]
let tests =
    testList
        "JsNamespace"
        [
            test "(a) a global-pack class mounts as Js.Widget and constructs with the bare name, no import" {
                // `new Js.Widget()` resolves the mounted `Js.Widget` annotation (the external
                // dotted-name seam), type-checks, and emits `new Widget(` — the BARE export.
                // No `import` and no `es2015` home leaks into the output: the runtime provides
                // `Widget` intrinsically (the `Global` flag on the resolved shape).
                let js = emitGlobal es2015Provider "let w = new Js.Widget()\n"

                Expect.stringContains js "new Widget(" (sprintf "expected bare `new Widget(`, got:\n%s" js)

                Expect.isFalse
                    (js.Contains "import")
                    (sprintf "a global-pack construction must emit NO import, got:\n%s" js)

                Expect.isFalse
                    (js.Contains "es2015")
                    (sprintf "the wire home `es2015` must not leak into the output, got:\n%s" js)
            }

            test "(b) member access on a mounted global type resolves and emits a native receiver.member call" {
                // `w.ping()` on the `Js.Widget` value resolves through the provider
                // (`MemberLowering.AttachedNative`) and lowers to `w.ping()` — still no import.
                let js = emitGlobal es2015Provider "let w = new Js.Widget()\nw.ping()\n"

                Expect.stringContains js ".ping(" (sprintf "expected the native member call `.ping(`, got:\n%s" js)

                Expect.isFalse
                    (js.Contains "import")
                    (sprintf "member access on a global type must add no import, got:\n%s" js)
            }

            test "(b') a global-pack free function emits its bare export name with no import (addRef skip)" {
                // The value-export sibling: the mounted `Js.spin()` from the `es2015` global
                // home emits the BARE `spin()` — `JsImports.addRef` skips recording (returns the
                // bare export name) for a `globalLibHomes` home.
                let js = emitGlobal es2015Provider "Js.spin()\n"

                Expect.stringContains js "spin(" (sprintf "expected bare `spin(`, got:\n%s" js)

                Expect.isFalse
                    (js.Contains "import")
                    (sprintf "a global-pack free function must emit no import, got:\n%s" js)
            }

            test "(c) CONTROL: a non-global package's free function still emits its normal import" {
                // Global rides the HOME: `somepkg` is absent from `globalLibHomes`, so `poke()`
                // is imported normally from its runtime module — the no-import behaviour fires
                // ONLY for a global-pack home.
                let runtime =
                    Map.ofList
                        [
                            "somepkg",
                            {
                                FileName = "somepkg.mjs"
                                Source = "export function poke() {}\n"
                            }
                        ]

                let js = emitWith (stackTs somepkgManifest) runtime false "poke()\n"

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
                // The consumer ref-minting (`toFrozen`'s `nominal`) mints a `Widget` ref whose
                // `home = es2015` selects the `Js` namespace (from `globalLibHomes`), so its
                // `qualifiedName` equals what the mounted es2015 provider registers — `Js.Widget`
                // — not the bare `Widget`. The home does NOT ride the key (identity is nominal);
                // it rides the SHAPE, so the proof that the ref and the mounted declaration are
                // ONE type is that B's minted key resolves against the mounted pack to a shape
                // homed in es2015.
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
                // The homed `Js.Widget` identity has a shape once the es2015 provider is stacked:
                // `w.ping()` resolves `ping` through the ordinary provider stack.
                let provider = stackTsMany [ manifestB; es2015Manifest ]
                let errors = analyseWith provider "let w = theWidget\nw.ping()\n"

                Expect.isEmpty
                    errors
                    (sprintf "member access on the homed Js.Widget should type-check, got:\n%A" (errorText errors))
            }
        ]
