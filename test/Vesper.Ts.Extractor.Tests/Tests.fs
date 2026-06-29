module Vesper.Ts.Extractor.Tests.Tests

open System.IO
open Expecto

open Vesper.Ts.Manifest

open Vesper.Ts.Extractor.Tests.TestHelpers

// v1 schema: enum member values are type-tagged (`EnumValue`). The whole point of
// the bump is that a numeric `A = 42` and a string `A = "42"` no longer collapse
// to the same `Some "42"` — int vs string must survive encode→decode. A computed
// member (`None`) must still round-trip too.
[<Tests>]
let enumValueCodecTests =
    testList
        "Codec enum value tag round-trip"
        [
            test "an enum with int, string, and computed members preserves each value's tag" {
                let man: Schema.PackageManifest =
                    {
                        SchemaVersion = Schema.SchemaVersion
                        Package = "tagcheck"
                        Version = None
                        Exports =
                            [
                                Schema.Export.Enum(
                                    "E",
                                    [
                                        "Num", Some(Schema.EnumValue.IntVal 42L)
                                        // Same printed digits as the int case — the discriminator,
                                        // not the lexeme, is what tells them apart on decode.
                                        "Str", Some(Schema.EnumValue.StringVal "42")
                                        "Computed", None
                                    ]
                                )
                            ]
                    }

                match Codec.deserialize (Codec.serialize man) with
                | Error e -> failtestf "round-trip failed to decode: %s" e
                | Ok man2 ->
                    Expect.equal man2 man "manifest must survive encode→decode unchanged"

                    match man2.Exports with
                    | [ Schema.Export.Enum(_, members) ] ->
                        Expect.equal
                            members
                            [
                                "Num", Some(Schema.EnumValue.IntVal 42L)
                                "Str", Some(Schema.EnumValue.StringVal "42")
                                "Computed", None
                            ]
                            "int (42) and string (\"42\") stay distinct; the computed member stays None"
                    | other -> failtestf "expected a single Enum export, got %A" other
            }
        ]

[<Tests>]
let goldenTests =
    testSequenced
    <| testList
        "Vesper.Ts.Extractor golden"
        [
            // The committed manifest is the golden — assert it's in canonical form
            // and round-trips. (No separate rendered snapshot; the JSON is it.)
            testList
                "manifest canonical form"
                [
                    // Single-file specs AND package fixtures: every committed manifest.
                    for path in allManifestFiles.Value do
                        test $"canonical: {Path.GetFileName path}" { testManifestCanonical path }
                ]

            // Loader resolves what each manifest declares.
            testList
                "provider resolution"
                [
                    for path in allManifestFiles.Value do
                        test $"resolves: {Path.GetFileName path}" { testProviderResolves path }
                ]

            test "no orphaned spec files" { testNoOrphans () }

            // The real `.d.ts → manifest` golden: run the compiled extractor on
            // each fixture and assert its output equals the sibling `.manifest.json`.
            // Skips cleanly when the extractor isn't Fable-built or node is absent.
            testList
                "extractor output matches golden"
                [
                    for path in dtsFiles.Value do
                        test $"extract: {Path.GetFileName path}" { testExtractorMatchesGolden path }
                ]

            // Package-entry golden (item 18): run the extractor in package mode over
            // each `packages/<D>` fixture, pulling its cross-file `.d.ts` closure.
            testList
                "package extractor output matches golden"
                [
                    for pkgDir in packageDirs.Value do
                        test $"extract-package: {Path.GetFileName pkgDir}" { testExtractorMatchesGoldenPackage pkgDir }
                ]
        ]
