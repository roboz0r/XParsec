/// Golden-test helpers for the TS extractor slice. Deliberately lighter than the
/// XParsec `.parsed` infra: there is NO separate rendered snapshot format — the
/// `.manifest.json` IS the golden artifact (human-readable, the actual output).
/// We borrow only the ergonomics: `UPDATE_SNAPSHOTS=1` regeneration,
/// `__SOURCE_DIRECTORY__`-relative discovery, CRLF→LF normalization, and an
/// orphan guard.
module Vesper.Ts.Extractor.Tests.TestHelpers

open System
open System.IO
open System.Diagnostics
open Expecto

open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js

let specsDir =
    lazy DirectoryInfo(Path.Combine(__SOURCE_DIRECTORY__, "specs")).FullName

let manifestFiles =
    lazy
        (Directory.GetFiles(specsDir.Value, "*.manifest.json", SearchOption.AllDirectories)
         |> Array.sort)

let dtsFiles =
    lazy
        (Directory.GetFiles(specsDir.Value, "*.d.ts", SearchOption.AllDirectories)
         |> Array.sort)

// ─── package fixtures (item 18: multi-file / package entry) ─────────────────
//
// A package fixture is a DIRECTORY (entry `.d.ts` + sibling `.d.ts` modules +
// `package.json`), not a flat `.d.ts`, so it lives in a SEPARATE `ts-fixtures/` tree.
// This is deliberate, and it is also how the orphan guard stays correct: that guard
// (`findOrphans`) pairs a `.d.ts` with a same-base `.manifest.json`, but a package
// has MANY `.d.ts` and ONE manifest — globbing them under `specs/` would false-flag
// every sibling. Keeping packages out of the `specs/` globs (above) sidesteps that
// entirely, so the guard needs no special-casing. The fixtures live at the NEUTRAL
// test-level dir `test/ts-fixtures/` (not under either test project) so neither test
// project reaches into the other's tree. (It is `ts-fixtures/`, not the natural
// `packages/`, because the repo `.gitignore` swallows `**/[Pp]ackages/*` as a NuGet
// convention — which would otherwise leave the fixture uncommitted.)
//
// Convention per package dir `D`: the manifest golden is `D/D.manifest.json`, the
// entry is resolved as the relative specifier `./D` from the `ts-fixtures/` dir, and
// the package name is `D`.
let packagesDir =
    lazy DirectoryInfo(Path.Combine(__SOURCE_DIRECTORY__, "..", "ts-fixtures")).FullName

let packageDirs =
    lazy
        (if Directory.Exists packagesDir.Value then
             Directory.GetDirectories packagesDir.Value |> Array.sort
         else
             [||])

let packageManifestOf (pkgDir: string) =
    Path.Combine(pkgDir, Path.GetFileName pkgDir + ".manifest.json")

let packageManifestFiles = lazy (packageDirs.Value |> Array.map packageManifestOf)

/// Every committed manifest — single-file specs AND package fixtures — so the
/// canonical-form and provider-resolution suites cover both (a package manifest is a
/// `PackageManifest` like any other; the cross-file closure is invisible to them).
let allManifestFiles =
    lazy (Array.append manifestFiles.Value packageManifestFiles.Value)

let private updateSnapshots =
    Environment.GetEnvironmentVariable "UPDATE_SNAPSHOTS" |> isNull |> not

let private normalize (s: string) = s.Replace("\r\n", "\n").TrimEnd()

/// The committed `.manifest.json` must be in CANONICAL serialized form:
/// deserialize → re-serialize reproduces the file. Doubles as a parse check and a
/// round-trip check; keeps fixtures tidy and catches schema drift. The manifest
/// itself is the golden — no rendered side-file. `UPDATE_SNAPSHOTS=1` rewrites it.
let testManifestCanonical (path: string) =
    let raw = File.ReadAllText path

    match Codec.deserialize raw with
    | Error e -> failtestf "manifest does not parse: %s" e
    | Ok man ->
        let canonical = Codec.serialize man

        if updateSnapshots then
            File.WriteAllText(path, canonical + "\n")
        else
            Expect.equal
                (normalize canonical)
                (normalize raw)
                "Manifest is not in canonical serialized form (run UPDATE_SNAPSHOTS=1 to refresh)"

        match Codec.deserialize canonical with
        | Ok man2 -> Expect.equal man2 man "Manifest does not round-trip through the codec"
        | Error e -> failtestf "canonical form failed to re-parse: %s" e

/// Mirror of `TsManifestProvider.syntheticTypeName` (it is `private`): the SIMPLE
/// name of the synthetic per-module grouping type that holds a module's overloaded
/// free functions (Tier 2 item 9b). Kept in lock-step with the provider rule —
/// last '/'-segment of the module specifier, first char upper-cased.
let private syntheticTypeName (moduleSpec: string) : string =
    let lastSeg =
        match moduleSpec.Split('/') |> Array.filter (fun s -> s <> "") |> Array.tryLast with
        | Some s -> s
        | None -> moduleSpec

    if lastSeg = "" then
        lastSeg
    else
        string (System.Char.ToUpperInvariant lastSeg.[0]) + lastSeg.Substring 1

/// Loader invariant: every `Function` export resolves via `TryLookup`; every
/// `Interface`/`Class` via `TryLookupType`, and each member via `TryLookupMembers`.
let testProviderResolves (path: string) =
    match Codec.deserialize (File.ReadAllText path) with
    | Error e -> failtestf "manifest does not parse: %s" e
    | Ok man ->
        let prov = TsManifestProvider.providerOfManifest man

        // A nested export is registered/looked up under its DOTTED QUALIFIED name
        // (`NS.Foo`), so recursion threads the namespace `prefix` and every lookup
        // qualifies the bare export name through it — matching the provider's `qualify`.
        let qualify (prefix: string) (name: string) =
            if prefix = "" then name else prefix + "." + name

        let rec check (prefix: string) (ex: Schema.Export) =
            let q name = qualify prefix name

            match ex with
            | Schema.Export.Function(name, signatures, _) ->
                if signatures.Length > 1 then
                    // Tier 2 item 9b: an OVERLOADED free function is no longer a bare
                    // function — it is grouped as static members of the synthetic
                    // per-module type, so it must NOT resolve via `TryLookup`, while the
                    // synthetic type resolves via `TryLookupType` and its overloads via
                    // `TryLookupMembers` (one member per signature, distinct keys).
                    Expect.isTrue
                        (prov.TryLookup(q name)).IsNone
                        $"overloaded function '{q name}' should NOT resolve as a bare free function"

                    let synthName = q (syntheticTypeName man.Package)

                    Expect.isTrue
                        (prov.TryLookupType synthName).IsSome
                        $"synthetic grouping type '{synthName}' should resolve"

                    let overloads = prov.TryLookupMembers(synthName, name)

                    Expect.equal
                        overloads.Length
                        signatures.Length
                        $"overloaded function '{name}' should resolve to one static member per signature"

                    Expect.isTrue
                        (overloads |> Array.forall (fun m -> m.IsStatic))
                        $"overloaded function '{name}' members must be static"

                    Expect.equal
                        (overloads |> Array.map (fun r -> r.Key) |> Array.distinct |> Array.length)
                        overloads.Length
                        $"overloaded function '{name}' members must have distinct keys"
                else
                    Expect.isTrue (prov.TryLookup(q name)).IsSome $"function '{q name}' should resolve"
            | Schema.Export.Interface(name, typeParams, members, heritage)
            | Schema.Export.Class(name, typeParams, members, heritage, _) ->
                let name = q name
                Expect.isTrue (prov.TryLookupType name).IsSome $"type '{name}' should resolve"

                // Generics (Tier 3 item 11): the declaring-axis arity round-trips — a
                // generic `Box<T>`/`Container<T>` resolves to an `ExternalTypeShape.Class`
                // whose `Arity` equals the emitted `typeParams`. Trivially 0 for the
                // (many) non-generic fixtures; exercises the count on `generics`.
                match prov.TryLookupType name with
                | ValueSome(ExternalTypeShape.Class shape) ->
                    Expect.equal shape.Arity typeParams $"type '{name}' arity must equal its typeParams"
                | _ -> ()

                // Heritage (Tier 4 item 16): every heritage entry must land in EXACTLY one
                // provider slot — `FrozenInterfaces` (extended/implemented interfaces) or the
                // single `FrozenBaseType` (base class) — so the populated count equals the
                // emitted heritage count. Trivially satisfied for the (many) empty-heritage
                // fixtures; exercises the disambiguation on the `heritage` fixture.
                match prov.TryLookupType name with
                | ValueSome(ExternalTypeShape.Class shape) ->
                    let baseCount = if shape.FrozenBaseType.IsSome then 1 else 0

                    Expect.equal
                        (shape.FrozenInterfaces.Length + baseCount)
                        heritage.Length
                        $"type '{name}' heritage must populate FrozenInterfaces/FrozenBaseType"
                | _ -> ()

                for m in members do
                    let resolved = prov.TryLookupMembers(name, m.Name)
                    Expect.isGreaterThan resolved.Length 0 $"member '{name}.{m.Name}' should resolve"

                    // Overload identity (Tier 2 item 9): a method/.ctor with N call
                    // signatures must expand into N members, each with its own
                    // `MemberKey` argSig — so the resolved count matches the signature
                    // count AND the keys are all distinct (no argSig collision survived).
                    if m.Signatures.Length > 1 then
                        Expect.equal
                            resolved.Length
                            m.Signatures.Length
                            $"overloaded member '{name}.{m.Name}' should resolve to one member per signature"

                        Expect.equal
                            (resolved |> Array.map (fun r -> r.Key) |> Array.distinct |> Array.length)
                            resolved.Length
                            $"overloaded member '{name}.{m.Name}' members must have distinct keys"
            | Schema.Export.Variable(name, _, _, _) ->
                Expect.isTrue (prov.TryLookup(q name)).IsSome $"variable '{q name}' should resolve"
            | Schema.Export.TypeAlias(name, typeParams, _) ->
                Expect.isTrue (prov.TryLookupType(q name)).IsSome $"type alias '{q name}' should resolve"

                // A generic alias (`Pair<A,B>`) resolves to an `Abbrev` whose arity equals
                // its `typeParams` (item 11 — was hardcoded 0 before generics landed).
                match prov.TryLookupType(q name) with
                | ValueSome(ExternalTypeShape.Abbrev(arity, _)) ->
                    Expect.equal arity typeParams $"type alias '{q name}' arity must equal its typeParams"
                | _ -> ()
            | Schema.Export.Enum(name, _) ->
                // The enum NAME resolves (an `Opaque` shape); its MEMBERS are stubbed on
                // the provider, so only the type-name resolution is asserted.
                Expect.isTrue (prov.TryLookupType(q name)).IsSome $"enum '{q name}' should resolve"
            | Schema.Export.Namespace(nsName, nested) ->
                // Item 17: the namespace container holds no symbol of its own; recurse into
                // its members under the extended prefix so each resolves via its qualified
                // name. Covers the nested namespace, proving the recursion folds depth.
                let childPrefix = qualify prefix nsName

                for nx in nested do
                    check childPrefix nx

        for ex in man.Exports do
            check "" ex

/// `.d.ts` and `.manifest.json` must come in pairs (a fixture with one but not the
/// other is almost always a mistake). Returns human-readable orphan descriptions.
let findOrphans () : string list =
    let baseName (p: string) =
        if p.EndsWith ".manifest.json" then
            p.Substring(0, p.Length - ".manifest.json".Length)
        elif p.EndsWith ".d.ts" then
            p.Substring(0, p.Length - ".d.ts".Length)
        else
            p

    let manBases = manifestFiles.Value |> Array.map baseName |> Set.ofArray
    let dtsBases = dtsFiles.Value |> Array.map baseName |> Set.ofArray

    let missingManifest =
        Set.difference dtsBases manBases
        |> Set.map (fun b -> b + ".d.ts (no sibling .manifest.json)")

    let missingDts =
        Set.difference manBases dtsBases
        |> Set.map (fun b -> b + ".manifest.json (no sibling .d.ts)")

    Set.union missingManifest missingDts |> Set.toList

/// Orphan guard, tolerant of regeneration: under `UPDATE_SNAPSHOTS` a freshly
/// added `.d.ts` has no `.manifest.json` yet (the extractor stage writes it), so
/// skip rather than fail mid-regeneration.
let testNoOrphans () =
    if updateSnapshots then
        skiptest "regenerating snapshots; orphan check skipped"

    match findOrphans () with
    | [] -> ()
    | orphans -> failtestf "orphaned spec files:\n%s" (String.concat "\n" orphans)

/// The Fable-compiled extractor entrypoint (built via
/// `dotnet fable src/Vesper.Ts.Extractor -o src/Vesper.Ts.Extractor/dist`).
let extractorJs =
    lazy
        Path.GetFullPath(
            Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src", "Vesper.Ts.Extractor", "dist", "Program.js")
        )

let private goldenOf (dtsPath: string) =
    dtsPath.Substring(0, dtsPath.Length - ".d.ts".Length) + ".manifest.json"

/// The REAL golden contract: run the compiled extractor on a `.d.ts` and assert
/// its output equals the sibling `.manifest.json`. Skips (CI-safe) when the
/// extractor isn't built or `node` is unavailable. `UPDATE_SNAPSHOTS=1` rewrites
/// the golden from the extractor's output (the extractor is the source of truth).
let testExtractorMatchesGolden (dtsPath: string) =
    if not (File.Exists extractorJs.Value) then
        skiptest "extractor not built — run: dotnet fable src/Vesper.Ts.Extractor -o src/Vesper.Ts.Extractor/dist"

    let packageName = Path.GetFileName(Path.GetDirectoryName dtsPath)
    let golden = goldenOf dtsPath

    let outPath =
        Path.Combine(Path.GetTempPath(), Path.GetFileNameWithoutExtension dtsPath + ".vesper.out.json")

    let started =
        try
            let psi = ProcessStartInfo("node")
            psi.ArgumentList.Add extractorJs.Value
            psi.ArgumentList.Add dtsPath
            psi.ArgumentList.Add packageName
            psi.ArgumentList.Add outPath
            psi.RedirectStandardError <- true
            psi.RedirectStandardOutput <- true
            psi.UseShellExecute <- false
            Some(Process.Start psi)
        with _ ->
            None // node not on PATH

    match started with
    | None -> skiptest "node not available; skipping extractor run"
    | Some p ->
        let stderr = p.StandardError.ReadToEnd()
        p.WaitForExit()

        if p.ExitCode <> 0 then
            failtestf "extractor failed (exit %d): %s" p.ExitCode stderr

        let actual = File.ReadAllText outPath

        if updateSnapshots then
            File.WriteAllText(golden, actual.TrimEnd() + "\n")
        else
            Expect.equal
                (normalize actual)
                (normalize (File.ReadAllText golden))
                "Extractor output does not match the golden (run UPDATE_SNAPSHOTS=1 to refresh)"

/// Package-entry golden contract (item 18): run the compiled extractor in PACKAGE
/// mode (`--package <specifier> <resolveFromDir> <packageName> <outPath>`) on a
/// fixture DIRECTORY and assert its output equals `D/D.manifest.json`. The package is
/// resolved as the relative specifier `./D` from the `ts-fixtures/` dir, so the
/// synthetic-entry + module resolver pull the cross-file `.d.ts` closure. Same
/// skip/refresh semantics as the single-file path.
let testExtractorMatchesGoldenPackage (pkgDir: string) =
    if not (File.Exists extractorJs.Value) then
        skiptest "extractor not built — run: dotnet fable src/Vesper.Ts.Extractor -o src/Vesper.Ts.Extractor/dist"

    let packageName = Path.GetFileName pkgDir
    let specifier = "./" + packageName
    let resolveFromDir = packagesDir.Value
    let golden = packageManifestOf pkgDir

    let outPath = Path.Combine(Path.GetTempPath(), packageName + ".vesper.pkg.out.json")

    let started =
        try
            let psi = ProcessStartInfo("node")
            psi.ArgumentList.Add extractorJs.Value
            psi.ArgumentList.Add "--package"
            psi.ArgumentList.Add specifier
            psi.ArgumentList.Add resolveFromDir
            psi.ArgumentList.Add packageName
            psi.ArgumentList.Add outPath
            psi.RedirectStandardError <- true
            psi.RedirectStandardOutput <- true
            psi.UseShellExecute <- false
            Some(Process.Start psi)
        with _ ->
            None // node not on PATH

    match started with
    | None -> skiptest "node not available; skipping extractor run"
    | Some p ->
        let stderr = p.StandardError.ReadToEnd()
        p.WaitForExit()

        if p.ExitCode <> 0 then
            failtestf "package extractor failed (exit %d): %s" p.ExitCode stderr

        let actual = File.ReadAllText outPath

        if updateSnapshots then
            File.WriteAllText(golden, actual.TrimEnd() + "\n")
        else
            Expect.equal
                (normalize actual)
                (normalize (File.ReadAllText golden))
                "Package extractor output does not match the golden (run UPDATE_SNAPSHOTS=1 to refresh)"
