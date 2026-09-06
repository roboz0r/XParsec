module Vesper.Ts.Extractor.Tests.Tests

open System.IO
open Expecto

open Vesper.Ts.Manifest

open Vesper.Ts.Extractor.Tests.TestHelpers

// v1 schema: enum member values are type-tagged (`LiteralValue`). The whole point of
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
                                        "Num", Some(Schema.LiteralValue.IntVal 42L)
                                        // Same printed digits as the int case — the discriminator,
                                        // not the lexeme, is what tells them apart on decode.
                                        "Str", Some(Schema.LiteralValue.StringVal "42")
                                        "Computed", None
                                    ]
                                )
                            ]
                        Diagnostics = []
                        Refs = []
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
                                "Num", Some(Schema.LiteralValue.IntVal 42L)
                                "Str", Some(Schema.LiteralValue.StringVal "42")
                                "Computed", None
                            ]
                            "int (42) and string (\"42\") stay distinct; the computed member stays None"
                    | other -> failtestf "expected a single Enum export, got %A" other
            }

            // The diagnostics channel: a manifest carrying a Diagnostic (with a
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
                                    Code = Schema.DiagCode.IntersectionErased
                                    Symbol = "Foo.bar"
                                    Span =
                                        Some
                                            {
                                                File = "foo.d.ts"
                                                Start = 10
                                                End = 25
                                            }
                                    Message = "intersection type erased to obj"
                                }
                                // Forward tolerance: a code minted by a NEWER extractor
                                // decodes as `Unknown` and round-trips losslessly.
                                {
                                    Severity = Schema.Severity.Warning
                                    Code = Schema.DiagCode.Unknown "future-code"
                                    Symbol = "Foo.baz"
                                    Span = None
                                    Message = "a code this build does not know"
                                }
                            ]
                        Refs = []
                    }

                match Codec.deserialize (Codec.serialize man) with
                | Error e -> failtestf "round-trip failed to decode: %s" e
                | Ok man2 -> Expect.equal man2 man "manifest with a diagnostic must survive encode→decode unchanged"
            }

            // The refs table: a foreign-reference-bearing manifest must round-trip
            // (every RefKind included), AND an empty table must be OMITTED from the wire
            // so a ref-free manifest stays byte-identical to a pre-refs golden.
            test "a refs-bearing manifest round-trips and an empty table is omitted from the wire" {
                let man: Schema.PackageManifest =
                    {
                        SchemaVersion = Schema.SchemaVersion
                        Package = "refcheck"
                        Version = None
                        Exports = []
                        Diagnostics = []
                        Refs =
                            [
                                "Box",
                                {
                                    Home = "boxlib"
                                    Kind = Schema.RefKind.Class
                                    TyparArity = 1
                                }
                                "Bus",
                                {
                                    Home = "eventlib"
                                    Kind = Schema.RefKind.Interface
                                    TyparArity = 0
                                }
                                "Handler",
                                {
                                    Home = "eventlib"
                                    Kind = Schema.RefKind.Alias
                                    TyparArity = 1
                                }
                                "Color",
                                {
                                    Home = "palette"
                                    Kind = Schema.RefKind.Enum
                                    TyparArity = 0
                                }
                            ]
                    }

                match Codec.deserialize (Codec.serialize man) with
                | Error e -> failtestf "refs round-trip failed to decode: %s" e
                | Ok man2 -> Expect.equal man2 man "a refs-bearing manifest must survive encode→decode unchanged"

                // Byte-identity guard: a ref-FREE manifest carries no `refs` key at all.
                let refFree = { man with Refs = [] }
                let text = Codec.serialize refFree

                Expect.isFalse
                    (text.Contains "\"refs\"")
                    "an empty refs table must be OMITTED from the wire (pre-refs byte-identity)"
            }
        ]

// The mitt gate: the real npm package `mitt@3.0.1` is vendored at `ts-fixtures/mitt/`
// and extracted to a committed golden. The extractor has FAITHFUL schema arms for
// mitt's five residual constructs (keyof, indexed-access ×2, conditional) instead of
// the `structural-object-stubbed` degrade, so the golden's `Diagnostics` collapses to
// `[]` — mitt extracts with ZERO loss of fidelity:
//   • `keyof Events` / `keyof T`              → `TypeRef.KeyOf` (via `isIndexType()`).
//   • `Events[Key]` / `T[keyof T]`            → `TypeRef.IndexedAccess`.
//   • `undefined extends Events[Key] ? Key : never` → `TypeRef.Conditional`.
//   • `Key extends keyof Events`              → the method typar's constraint is stored on
//     `Signature.TypeParamBounds`, carried rather than evaluated.
[<Tests>]
let mittDiagnosticsContract =
    let mittManifestPath = Path.Combine(packagesDir.Value, "mitt", "mitt.manifest.json")

    testList
        "mitt package fidelity diagnostics"
        [
            test "mitt extracts with ZERO diagnostics (all five constructs are now faithful arms)" {
                match Codec.deserialize (File.ReadAllText mittManifestPath) with
                | Error e -> failtestf "mitt manifest does not parse: %s" e
                | Ok man ->
                    // R4a step 2 gate: keyof / indexed-access / conditional are faithful
                    // schema arms now, so mitt degrades NOTHING — the diagnostics channel is
                    // empty. Any diagnostic reappearing here is a fidelity regression.
                    Expect.isEmpty
                        man.Diagnostics
                        (sprintf
                            "mitt must extract with zero diagnostics; got: %A"
                            (man.Diagnostics |> List.map (fun d -> d.Code, d.Symbol)))
            }
        ]

// The real-scale burndown: the `lib.es2015.*` closure is extracted via
// `--lib-globals` (noLib + explicit lib inputs) and vendored at `ts-fixtures/es2015/`.
// UNLIKE `mittDiagnosticsContract` (a GATE asserting `Diagnostics = []`), this is a
// BURNDOWN: real residue is EXPECTED and the diagnostics ranked by code frequency are
// asserted to equal a COMMITTED table. Any drift — up OR down — fails and forces a
// deliberate golden update; shrinking these counts is the scoreboard. Each code's
// gloss (WHY that construct degraded):
//   • structural-object-stubbed — the genuinely-UNREPRESENTABLE structural residue: a
//     type whose content a named-field list cannot fully carry — a call/construct
//     signature (`new(...)=>R`), an array, a 0-/1-tuple or an optional/rest/variadic tuple
//     (`[K, V?]`, `[K, ...V[]]`), an empty `{}`, or a mapped type (`Readonly<T>`/`Record`)
//     → an OPAQUE (empty-field) content-hashed `Structural`. An INDEX SIGNATURE
//     (`{ [idx]: … }`) no longer stubs — it now carries FAITHFULLY on the `Structural`'s
//     `index` facet (count fell 18 → 14 as those objects graduated). Only a pure
//     named-property record carries its fields (FAITHFULLY, no warning), and a fixed
//     all-required multi-element tuple (`[number, string]`, `[K, V]`) now carries as a real
//     `Tuple` (→ `FTTuple`); everything else here is opaque.
//   • recursion-depth-exceeded — the self-recursive `Awaited<T>` conditional (and the
//     `infer`-introduced pieces it expands into) recurses unbounded; `mapType` degrades
//     the subtree to `obj` at the depth bound so `Promise` still extracts as a class.
//   • method-scope-typar-erased — a type parameter bound by NEITHER scope: `infer` typars
//     inside conditional types and the apply/bind/call typars on `CallableFunction`/
//     `NewableFunction` that TS does not surface on the tracked axes → erased to `obj`.
//   • intersection-erased — only a NON-object intersection, which has no merged member
//     set to carry: a constituent is a primitive-ish `object` (`object & { then… }`) or
//     a type parameter (`NonNullable<T>` = `T & {}`) → erased to `obj`. An OBJECT-ONLY
//     intersection instead MERGES into a faithful `Structural` and no longer warns.
// The PRIMITIVE-OVERLAP skip-list is pinned here too: the pack exports NONE of the
// intrinsic-overlap names (`Array`/`String`/…) and homes no self-ref for them, while
// `Map` (not on the list) IS exported as a class.
[<Tests>]
let es2015BurndownContract =
    let es2015Path = Path.Combine(packagesDir.Value, "es2015", "es2015.manifest.json")

    // The COMMITTED burndown ranking (code, count), sorted by count desc then code asc.
    // Regenerated deliberately (never silently) when the extractor's fidelity changes.
    let committedRanking =
        [
            "recursion-depth-exceeded", 15
            "method-scope-typar-erased", 14
            "structural-object-stubbed", 14
            "intersection-erased", 2
        ]

    testList
        "es2015 ref-pack burndown"
        [
            test "es2015 diagnostics rank matches the committed burndown (drift up OR down fails)" {
                match Codec.deserialize (File.ReadAllText es2015Path) with
                | Error e -> failtestf "es2015 manifest does not parse: %s" e
                | Ok man ->
                    let ranking =
                        man.Diagnostics
                        |> List.countBy (fun d -> d.Code.Wire)
                        |> List.sortByDescending snd
                        // Stable secondary key so equal counts order deterministically.
                        |> List.sortWith (fun (c1, n1) (c2, n2) -> if n1 <> n2 then compare n2 n1 else compare c1 c2)

                    Expect.equal
                        ranking
                        committedRanking
                        "es2015 burndown ranking drifted; if intended, update `committedRanking` and the gloss above"
            }

            test "skip-list pinned: Array/String/… are ABSENT as exports; Map IS present" {
                match Codec.deserialize (File.ReadAllText es2015Path) with
                | Error e -> failtestf "es2015 manifest does not parse: %s" e
                | Ok man ->
                    let exportName =
                        function
                        | Schema.Export.Function(n, _, _)
                        | Schema.Export.Variable(n, _, _, _)
                        | Schema.Export.TypeAlias(n, _, _)
                        | Schema.Export.Interface(n, _, _, _, _)
                        | Schema.Export.Class(n, _, _, _, _, _)
                        | Schema.Export.Enum(n, _)
                        | Schema.Export.Namespace(n, _) -> n

                    let names = man.Exports |> List.map exportName |> Set.ofList

                    for skipped in
                        [
                            "Array"
                            "String"
                            "Number"
                            "Boolean"
                            "Object"
                            "Function"
                            "Symbol"
                            "BigInt"
                        ] do
                        Expect.isFalse
                            (names.Contains skipped)
                            (sprintf "intrinsic-overlap name '%s' must NOT be exported by the es2015 pack" skipped)

                    Expect.isTrue (names.Contains "Map") "Map (not on the skip-list) must be exported as a class"

                    // A skip-list name must not appear as a self-ref either (the es2015
                    // pack homes its own types LOCAL, so refs is empty — but assert no
                    // skip-list name leaked in regardless).
                    let refNames = man.Refs |> List.map fst |> Set.ofList

                    Expect.isFalse
                        (refNames.Contains "Array")
                        "the es2015 pack must carry no self-ref for the skip-listed 'Array'"
            }
        ]

// Index-signature + optional-graduation capture, asserted on the committed
// `indexsig` fixture manifest (the whole-manifest golden below pins the exact bytes;
// this pins the SPECIFIC facets so a regression reads as a named failure, not a diff).
[<Tests>]
let indexSignatureContract =
    let manifestPath =
        Path.Combine(specsDir.Value, "indexsig", "indexsig.manifest.json")

    let load () =
        match Codec.deserialize (File.ReadAllText manifestPath) with
        | Error e -> failtestf "indexsig manifest does not parse: %s" e
        | Ok man -> man

    let stringRef = Schema.TypeRef.Named("string", [])
    let undefinedRef = Schema.TypeRef.Named("undefined", [])

    testList
        "index-signature + optional capture"
        [
            test "Dict carries its string index signature with a string | undefined value" {
                let dictIndex =
                    load().Exports
                    |> List.tryPick (
                        function
                        | Schema.Export.Interface("Dict", _, _, _, index) -> Some index
                        | _ -> None
                    )

                match dictIndex with
                | Some [ (key, value) ] ->
                    Expect.equal key stringRef "Dict index key is string"

                    match value with
                    | Schema.TypeRef.Union ds ->
                        Expect.isTrue (List.contains undefinedRef ds) "Dict index value union includes undefined"
                        Expect.isTrue (List.contains stringRef ds) "Dict index value union includes string"
                    | other -> failtestf "Dict index value should be a union, got %A" other
                | other -> failtestf "Dict must carry an index signature, got %A" other
            }

            test "lookup's anonymous object carries a (string, number) index facet" {
                let lookupTy =
                    load().Exports
                    |> List.tryPick (
                        function
                        | Schema.Export.Variable("lookup", ty, _, _) -> Some ty
                        | _ -> None
                    )

                match lookupTy with
                | Some(Schema.TypeRef.Structural(_, [], [ (key, value) ])) ->
                    Expect.equal key stringRef "lookup index key is string"
                    Expect.equal value (Schema.TypeRef.Named("number", [])) "lookup index value is number"
                | other -> failtestf "lookup should be an index-bearing Structural, got %A" other
            }

            test "Config.foo is optional; settings.timeout carries T | undefined on an anonymous field" {
                let man = load ()

                let configMembers =
                    man.Exports
                    |> List.pick (
                        function
                        | Schema.Export.Interface("Config", _, ms, _, _) -> Some ms
                        | _ -> None
                    )

                let foo = configMembers |> List.find (fun m -> m.Name = "foo")
                Expect.isTrue foo.Optional "Config.foo must carry Member.Optional"

                let settingsTy =
                    man.Exports
                    |> List.pick (
                        function
                        | Schema.Export.Variable("settings", ty, _, _) -> Some ty
                        | _ -> None
                    )

                match settingsTy with
                | Schema.TypeRef.Structural(_, fields, _) ->
                    let _, timeoutTy = fields |> List.find (fun (n, _) -> n = "timeout")

                    match timeoutTy with
                    | Schema.TypeRef.Union ds ->
                        Expect.isTrue (List.contains undefinedRef ds) "settings.timeout carries undefined"
                    | other -> failtestf "settings.timeout should be a union, got %A" other
                | other -> failtestf "settings should be a Structural, got %A" other
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

            // Loader resolves what each manifest declares. Runs on
            // `providerResolutionManifests` — now the FULL manifest set including the
            // es2015 ref pack (it loads and resolves cleanly since ctor overloads dedupe
            // by argSig; the pack mounts under `Js`, so the walk qualifies through it).
            testList
                "provider resolution"
                [
                    for path in providerResolutionManifests.Value do
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

            // Package-entry golden: run the extractor in package mode over
            // each `packages/<D>` fixture, pulling its cross-file `.d.ts` closure.
            testList
                "package extractor output matches golden"
                [
                    for pkgDir in packageDirs.Value do
                        test $"extract-package: {Path.GetFileName pkgDir}" { testExtractorMatchesGoldenPackage pkgDir }
                ]

            // Ambient-globals golden: run the extractor in globals mode over
            // the fixture's sibling `.d.ts`, exercising the fused class-like pair + the
            // cross-file interface merge.
            test "extract-globals: globals" { testExtractorMatchesGoldenGlobals () }

            // Ambient-modules golden: run the extractor in
            // `--ambient-modules` mode over the two-quoted-module fixture and assert BOTH
            // modules enumerate into their own per-module manifest, with the cross-module
            // ref homed to its declaring specifier.
            test "extract-ambient-modules: two-modules" { testExtractorMatchesGoldenAmbientModules () }

            // Real-scale lib-globals golden: run the extractor in `--lib-globals`
            // mode over TypeScript's OWN `lib.es2015.*` + `lib.es5` closure and assert the
            // vendored `es2015.manifest.json`. Regenerated under UPDATE_SNAPSHOTS.
            test "extract-lib-globals: es2015" { testExtractorMatchesGoldenLibGlobals () }
        ]
