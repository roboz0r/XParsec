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
                        Diagnostics = []
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

            // Phase 1 diagnostics channel: a manifest carrying a Diagnostic (with a
            // Span) must survive encode→decode unchanged, including the optional span.
            test "a diagnostic with a span round-trips through the codec" {
                let man: Schema.PackageManifest =
                    {
                        SchemaVersion = Schema.SchemaVersion
                        Package = "diagcheck"
                        Version = None
                        Exports = []
                        Diagnostics =
                            [
                                {
                                    Severity = Schema.Severity.Warning
                                    Code = "any-dynamic"
                                    Symbol = "Foo.bar"
                                    Span =
                                        Some
                                            {
                                                File = "foo.d.ts"
                                                Start = 10
                                                End = 25
                                            }
                                    Message = "`any` lowered to the deferred dynamic type"
                                }
                            ]
                    }

                match Codec.deserialize (Codec.serialize man) with
                | Error e -> failtestf "round-trip failed to decode: %s" e
                | Ok man2 -> Expect.equal man2 man "manifest with a diagnostic must survive encode→decode unchanged"
            }
        ]

// Phase 3.5 / Phase 4: the real npm package `mitt@3.0.1` is vendored at
// `ts-fixtures/mitt/` and extracted to a committed golden. The Phase-4 GOAL stated
// "zero diagnostics", but mitt is byte-tiny yet type-theoretically dense and that
// premise is FALSE. Phase 3.5 closed the two TRACTABLE fidelity gaps (method-axis
// typars, function types) and the noise cascade, taking mitt from 42 degrade warnings
// to a SMALL HONEST RESIDUE of exactly FIVE — one clean warning per genuinely-hard
// construct, all the same code, all non-fatal Warnings:
//
//   • `method-axis-typar-erased` — GONE. `on/off/emit<Key extends keyof Events>`'s `Key`
//     now maps FAITHFULLY to `MethodTypar 0` (the schema's method axis), no longer erased.
//   • `Handler<T>` / `WildcardHandler<T>` — now resolve as `TypeRef.Fun`, NOT structural
//     stubs (the function-type arm), so they record no diagnostic at all.
//   • `structural-object-stubbed` (×5, deduped) — the genuinely-hard residue that stays
//     deferred BY DESIGN (item 14): keyof / indexed-access / conditional types. The
//     cascade that harvested `string | symbol`'s apparent prototype members is fixed
//     (no field harvest for non-object structural forms), and per-reference duplicates
//     are deduped at the drain. The five are:
//       - `keyof Events`                                  (keyof,     ×3 sites → 1)
//       - `keyof T`                                       (keyof,     WildcardHandler)
//       - `T[keyof T]`                                    (indexed,   WildcardHandler)
//       - `Events[Key]`                                   (indexed,   emit)
//       - `undefined extends Events[Key] ? Key : never`   (conditional, emit overload)
[<Tests>]
let mittDiagnosticsContract =
    let mittManifestPath = Path.Combine(packagesDir.Value, "mitt", "mitt.manifest.json")

    testList
        "mitt package fidelity diagnostics"
        [
            test "mitt degrades into the five honest keyof/indexed/conditional warnings only" {
                match Codec.deserialize (File.ReadAllText mittManifestPath) with
                | Error e -> failtestf "mitt manifest does not parse: %s" e
                | Ok man ->
                    // Phase 3.5 residue: exactly the five genuinely-hard constructs survive,
                    // each as ONE clean Warning (no cascade, no per-site duplication).
                    Expect.isTrue
                        (man.Diagnostics |> List.forall (fun d -> d.Severity = Schema.Severity.Warning))
                        "every mitt diagnostic must be a non-fatal Warning (graceful degrade, not a throw)"

                    // Phase 3.5 closed the method-axis fidelity gap: `Key` is now faithful, so
                    // NO method-axis-typar-erased diagnostic may remain.
                    Expect.isEmpty
                        (man.Diagnostics |> List.filter (fun d -> d.Code = "method-axis-typar-erased"))
                        "mitt's `on/off/emit<Key>` method-axis typars are now faithful (MethodTypar); no erasure may remain"

                    // The ONLY residue is the structural deferral (keyof/indexed/conditional).
                    Expect.isTrue
                        (man.Diagnostics |> List.forall (fun d -> d.Code = "structural-object-stubbed"))
                        "mitt's honest residue is the structural-object-stubbed deferral only"

                    // Pin the EXACT residue: five constructs, deduped, by symbol. Drift (a new
                    // cascade, a lost dedup, a regressed faithful arm) fails here.
                    let residue = man.Diagnostics |> List.map (fun d -> d.Symbol) |> List.sort

                    Expect.equal
                        residue
                        [
                            "Events[Key]"
                            "T[keyof T]"
                            "keyof Events"
                            "keyof T"
                            "undefined extends Events[Key] ? Key : never"
                        ]
                        "mitt residue must be exactly the five keyof/indexed/conditional constructs, deduped"
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
