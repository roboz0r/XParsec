module Vesper.Ts.Extractor.Tests.Tests

open System.IO
open Expecto

open Vesper.Ts.Extractor.Tests.TestHelpers

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
