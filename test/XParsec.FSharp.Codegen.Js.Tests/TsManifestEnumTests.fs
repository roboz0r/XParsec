module XParsec.FSharp.Codegen.Js.Tests.TsManifestEnumTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// Enum support step 7b — the `TsManifestProvider` consumption arm that closes the
// extractor's last stubbed enum case. A TS module exporting a NUMERIC and a STRING
// enum is modelled by the provider as `ExternalTypeShape.Enum` (was `Opaque 0`, members
// dropped). The front end must then (a) resolve the enum TYPE name to its nominal
// `TyEnum`/`FTEnum` (not Opaque), (b) type-check `E.C1` as the enum and emit it as an
// IMPORTED member access (`import { E } … E.Ci`) — the enum object lives in the TS
// module, never re-emitted — and (c) lower a `match` on an external enum value to the
// shared `=== E.Ci` test, all mirroring the external-UNION consumption path.

/// A synthetic TS manifest for package `palette`: a numeric enum `Color` and a string
/// enum `Dir`. Both are top-level exports (flat package = module specifier `palette`).
let private paletteManifestJson =
    """{
  "schemaVersion": 1,
  "package": "palette",
  "version": null,
  "exports": [
    {
      "export": "enum",
      "name": "Color",
      "members": [
        { "name": "Red", "value": { "kind": "int", "value": 0 } },
        { "name": "Green", "value": { "kind": "int", "value": 1 } },
        { "name": "Blue", "value": { "kind": "int", "value": 2 } }
      ]
    },
    {
      "export": "enum",
      "name": "Dir",
      "members": [
        { "name": "Up", "value": { "kind": "string", "value": "up" } },
        { "name": "Down", "value": { "kind": "string", "value": "down" } }
      ]
    }
  ]
}"""

let private paletteManifest: Schema.PackageManifest =
    match Codec.deserialize paletteManifestJson with
    | Error e -> failwithf "palette manifest does not parse: %s" e
    | Ok man -> man

/// The `palette` enum provider, layered over the standard JS provider (so the integer
/// literals / primitives still resolve). The enum shapes come from the TS-manifest provider.
let private paletteProvider: IExternalSymbolProvider =
    ExternalSymbols.stack ValueNone [] [ TsManifestProvider.providerOfManifest paletteManifest; jsProvider.Value ]

/// Emit `input` to JS through the `palette` provider, injecting a fake `palette` runtime
/// module so the enum-object import resolves (the synthetic package ships no runtime
/// asset of its own — the import contract is what this test pins).
let private emitWithPalette (input: string) : string =
    let lexed, file = parseFile input
    let tast = Pipeline.analyseSemForSelfHost paletteProvider input lexed file

    let errors = tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

    if not (List.isEmpty errors) then
        failwithf "analysis errors: %A" (errors |> List.map (fun d -> d.Message))

    let frozen = Freeze.run tast

    let runtime =
        Map.ofList
            [
                "palette",
                {
                    FileName = "palette.mjs"
                    Source = ""
                }
            ]

    let ctx: EmitJs.WalkCtx =
        {
            Resolver = ValueNone
            Source = ValueSome input
            Records = System.Collections.Generic.Dictionary()
            Unions = System.Collections.Generic.Dictionary()
            Classes = System.Collections.Generic.Dictionary()
            Enums = System.Collections.Generic.Dictionary()
            Provider = ValueSome paletteProvider
            ExternalUnions = System.Collections.Generic.Dictionary()
            Imports = JsImports.create runtime
            ExportTopLevel = false
            CompiledFns = System.Collections.Generic.Dictionary()
            LocalInterfaces = System.Collections.Generic.HashSet()
        }

    (JsPrint.print (EmitJs.buildProgram ctx frozen)).Source

[<Tests>]
let tests =
    testList
        "TsManifestEnum"
        [
            test "the provider maps a TS enum to ExternalTypeShape.Enum (was Opaque), members carried" {
                match paletteProvider.TryLookupType "Color" with
                | ValueSome(ExternalTypeShape.Enum(cases, _)) ->
                    Expect.equal
                        [ for c in cases -> c.Name ]
                        [ "Red"; "Green"; "Blue" ]
                        "the numeric enum's ordered case names survive the remap (no longer dropped)"
                | other -> failtestf "expected Color to resolve to an Enum shape, got %A" other

                match paletteProvider.TryLookupType "Dir" with
                | ValueSome(ExternalTypeShape.Enum(cases, _)) ->
                    Expect.equal
                        [ for c in cases -> c.Value ]
                        [ ExternalEnumCaseValue.StringVal "up"; ExternalEnumCaseValue.StringVal "down" ]
                        "the string enum's values are tagged StringVal (variant recoverable on the consumer)"
                | other -> failtestf "expected Dir to resolve to an Enum shape, got %A" other
            }

            test "an external enum type annotation resolves and `E.Ci` emits an IMPORTED member access" {
                // `(c: Color)` exercises the type-annotation resolution (Opaque would have
                // failed to resolve the nominal); `Color.Green` is the value access.
                let js = emitWithPalette "let f (c: Color) = c\nlet g = Color.Green"

                Expect.isTrue
                    (js.Contains "Color as $palette_Color")
                    (sprintf "expected the enum OBJECT to be imported from its module, got:\n%s" js)

                Expect.isTrue
                    (js.Contains "$palette_Color.Green")
                    (sprintf "expected `Color.Green` to lower to an imported member access, got:\n%s" js)

                Expect.isFalse
                    (js.Contains "Object.freeze")
                    (sprintf "an external enum's object map must NOT be re-emitted, got:\n%s" js)
            }

            test "a string-enum case access imports its object too" {
                let js = emitWithPalette "let d = Dir.Up"

                Expect.isTrue
                    (js.Contains "Dir as $palette_Dir")
                    (sprintf "expected the string enum object imported, got:\n%s" js)

                Expect.isTrue
                    (js.Contains "$palette_Dir.Up")
                    (sprintf "expected `Dir.Up` to lower to an imported member access, got:\n%s" js)
            }

            test "a `match` on an external enum value lowers to `=== E.Ci` against the imported object" {
                let js =
                    emitWithPalette (
                        "let describe (c: Color) = match c with | Color.Red -> 0 | Color.Green -> 1 | _ -> 2\n"
                        + "let r = describe Color.Blue"
                    )

                Expect.isTrue
                    (js.Contains "=== $palette_Color.Red")
                    (sprintf "the `| Color.Red` arm tests `=== Color.Red` on the imported object, got:\n%s" js)

                Expect.isTrue
                    (js.Contains "=== $palette_Color.Green")
                    (sprintf "the `| Color.Green` arm tests `=== Color.Green`, got:\n%s" js)
            }
        ]
