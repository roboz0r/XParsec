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

// The ambient-GLOBALS fixture is a DIRECTORY holding MANY `.d.ts` (the merge case
// spans two files) but ONE manifest — like a package, not a single-file spec. So it
// is excluded from the single-file `dtsFiles`/`manifestFiles` globs and the orphan
// guard (which pair a `.d.ts` with a same-base `.manifest.json`, and would else
// false-flag the second file as an orphan and run single-file extraction on it), and
// is discovered on its own path below (mirroring the `packageDirs` split).
let globalsDir =
    lazy DirectoryInfo(Path.Combine(specsDir.Value, "globals")).FullName

let private underGlobals (path: string) =
    path.StartsWith(globalsDir.Value, StringComparison.OrdinalIgnoreCase)

// The ambient-MODULES fixture (W1, decision A) is a DIRECTORY holding ONE input `.d.ts`
// but MANY manifests — one per quoted `declare module "…"`. Like the globals fixture it
// is excluded from the single-file globs and the orphan guard (which pair a `.d.ts` with
// a same-base `.manifest.json` and would else false-flag every per-module manifest as an
// orphan), and discovered on its own path below.
let ambientModulesDir =
    lazy DirectoryInfo(Path.Combine(specsDir.Value, "ambient-modules")).FullName

let private underAmbientModules (path: string) =
    path.StartsWith(ambientModulesDir.Value, StringComparison.OrdinalIgnoreCase)

let private excludedFromSpecGlobs (p: string) = underGlobals p || underAmbientModules p

let manifestFiles =
    lazy
        (Directory.GetFiles(specsDir.Value, "*.manifest.json", SearchOption.AllDirectories)
         |> Array.filter (fun p -> not (excludedFromSpecGlobs p))
         |> Array.sort)

let dtsFiles =
    lazy
        (Directory.GetFiles(specsDir.Value, "*.d.ts", SearchOption.AllDirectories)
         |> Array.filter (fun p -> not (excludedFromSpecGlobs p))
         |> Array.sort)

/// The ambient-modules fixture's input `.d.ts` (there is exactly one; passed to the
/// extractor's variadic input list) and its per-module manifest goldens.
let ambientModulesDtsFiles =
    lazy
        (if Directory.Exists ambientModulesDir.Value then
             Directory.GetFiles(ambientModulesDir.Value, "*.d.ts") |> Array.sort
         else
             [||])

let ambientModulesManifests =
    lazy
        (if Directory.Exists ambientModulesDir.Value then
             Directory.GetFiles(ambientModulesDir.Value, "*.manifest.json") |> Array.sort
         else
             [||])

/// The globals fixture's input `.d.ts` (sorted, so program/order is deterministic and
/// matches the extractor invocation) and its single manifest golden.
let globalsDtsFiles =
    lazy
        (if Directory.Exists globalsDir.Value then
             Directory.GetFiles(globalsDir.Value, "*.d.ts") |> Array.sort
         else
             [||])

let globalsManifest = lazy Path.Combine(globalsDir.Value, "globals.manifest.json")

// ─── package fixtures (multi-file / package entry) ──────────────────────────
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

// The real-scale `lib.es2015` ref pack is vendored at `ts-fixtures/es2015/`
// like a package fixture, but it is NOT a resolvable npm package — it has no
// `package.json`/entry `.d.ts`, and its manifest is produced by the `--lib-globals`
// path over TypeScript's OWN lib files, not `--package`. So it is EXCLUDED from
// `packageDirs` (which would else run `--package ./es2015` and fail to resolve) and
// gets its own extraction/burndown wiring below, while its manifest STILL joins the
// canonical-form + provider-resolution suites via `allManifestFiles`.
let es2015Dir = lazy Path.Combine(packagesDir.Value, "es2015")

let es2015Manifest = lazy Path.Combine(es2015Dir.Value, "es2015.manifest.json")

/// The vendored `typescript` package's own `lib.es2015.*` + `lib.es5` `.d.ts` files —
/// the `--lib-globals` inputs. Passed in a FIXED order (es5 first, then the es2015
/// members alphabetically, then the `lib.es2015.d.ts` aggregator) so the extractor's
/// symbol enumeration — and thus the golden — is deterministic. They cross-`/// <reference>`
/// one another, so the full set loads the es2015 closure; `noLib` (set in
/// `libOptions`) makes them extract AS CONTENT rather than as the implicit default lib.
let es2015LibDir =
    lazy Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "node_modules", "typescript", "lib"))

let es2015LibFiles =
    lazy
        ([
            "lib.es5.d.ts"
            "lib.es2015.core.d.ts"
            "lib.es2015.collection.d.ts"
            "lib.es2015.generator.d.ts"
            "lib.es2015.iterable.d.ts"
            "lib.es2015.promise.d.ts"
            "lib.es2015.proxy.d.ts"
            "lib.es2015.reflect.d.ts"
            "lib.es2015.symbol.d.ts"
            "lib.es2015.symbol.wellknown.d.ts"
            "lib.es2015.d.ts"
         ]
         |> List.map (fun f -> Path.Combine(es2015LibDir.Value, f))
         |> Array.ofList)

let packageDirs =
    lazy
        (if Directory.Exists packagesDir.Value then
             Directory.GetDirectories packagesDir.Value
             |> Array.filter (fun d ->
                 not (String.Equals(Path.GetFileName d, "es2015", StringComparison.OrdinalIgnoreCase))
             )
             |> Array.sort
         else
             [||])

let packageManifestOf (pkgDir: string) =
    Path.Combine(pkgDir, Path.GetFileName pkgDir + ".manifest.json")

let packageManifestFiles = lazy (packageDirs.Value |> Array.map packageManifestOf)

/// Every committed manifest — single-file specs AND package fixtures — so the
/// canonical-form and provider-resolution suites cover both (a package manifest is a
/// `PackageManifest` like any other; the cross-file closure is invisible to them).
let allManifestFiles =
    lazy
        (Array.concat
            [
                manifestFiles.Value
                packageManifestFiles.Value
                // The globals fixture's manifest joins the canonical-form + provider-
                // resolution suites like any other `PackageManifest` (its cross-file
                // origin and fused class-like exports are invisible to them).
                (if File.Exists globalsManifest.Value then
                     [| globalsManifest.Value |]
                 else
                     [||])
                // The ambient-modules fixture's per-module manifests join the
                // canonical-form + provider-resolution suites like any other
                // `PackageManifest` (their per-module split is invisible to them).
                ambientModulesManifests.Value
                // The es2015 ref pack's manifest joins the canonical-form + provider-
                // resolution suites like any other `PackageManifest`. Its
                // real-scale export surface exercises those loaders at 100× the fixtures.
                (if File.Exists es2015Manifest.Value then
                     [| es2015Manifest.Value |]
                 else
                     [||])
            ])

/// Manifests the PROVIDER-RESOLUTION suite consumes — now the FULL `allManifestFiles`
/// set including the es2015 ref pack. es2015's genuine ctor-argSig collisions (the six
/// Error subclasses' inherited+own `new(message?: string)` pair, and `Map`'s cross-file
/// `MapConstructor` no-arg merge) load cleanly since `overloadArgSigs` DEDUPES ctor
/// overloads by argSig — a constructor dispatches on arguments alone, so a same-argSig
/// ctor pair is genuinely redundant (see `TsManifestTypes.overloadArgSigs`).
let providerResolutionManifests = lazy allManifestFiles.Value

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

/// Mirror of `TsManifestMembers.syntheticTypeName` (it is `private`): the SIMPLE
/// name of the synthetic per-module grouping type that holds a module's overloaded
/// free functions. Kept in lock-step with the provider rule —
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
                    // An OVERLOADED free function is not a bare function — it is
                    // grouped as static members of the synthetic
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

                    let overloads =
                        prov.TryLookupMembers(SymbolKeyOps.qualifiedTypeKey synthName 0, name)

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
            | Schema.Export.Interface(name, typeParams, members, heritage, _)
            | Schema.Export.Class(name, typeParams, members, heritage, _, _) ->
                // THE LOOKUP CONTRACT (`SymbolKeyOps.arityName`): the provider keys types
                // under their ARITY-SUFFIXED qualified name (`Box\`1`), and CALLERS suffix
                // by arity before lookup — the same law `TsManifestTypes.mint` and the
                // front-end `TypeTranslate` speak. A generic type (`Box<T>`) resolves ONLY
                // under `Box\`1`, never the bare `Box`, so suffix here deliberately.
                let name = SymbolKeyOps.arityName (q name) typeParams
                Expect.isTrue (prov.TryLookupType name).IsSome $"type '{name}' should resolve"

                // Generics: the declaring-axis arity round-trips — a
                // generic `Box<T>`/`Container<T>` resolves to an `ExternalTypeShape.Class`
                // whose `TyparArity` equals the emitted `typeParams`. Trivially 0 for the
                // (many) non-generic fixtures; exercises the count on `generics`.
                match prov.TryLookupType name |> ExternalSymbols.typeShapeOf with
                | ValueSome(ExternalTypeShape.Class shape) ->
                    Expect.equal shape.TyparArity typeParams $"type '{name}' arity must equal its typeParams"
                | _ -> ()

                // Heritage: every heritage entry must land in EXACTLY one
                // provider slot — `FrozenInterfaces` (extended/implemented interfaces) or the
                // single `FrozenBaseType` (base class) — so the populated count equals the
                // emitted heritage count. Trivially satisfied for the (many) empty-heritage
                // fixtures; exercises the disambiguation on the `heritage` fixture. PLUS the
                // provider synthesizes ONE extra `FrozenInterfaces` entry (the erased
                // `IEnumerable`, NOT from heritage) for a type declaring `[Symbol.iterator]`,
                // homing a TS iterable as `seq<'T>` — mirror that gate
                // (`TsManifestMembers.tryIteratorElement`: a `__@iterator…` member returning
                // an applied nominal iterator) so the slot count stays exact.
                let injectedEnumerable =
                    members
                    |> List.exists (fun m ->
                        m.Name.StartsWith "__@iterator"
                        && (
                            match m.Signatures with
                            | sg :: _ ->
                                match sg.Returns with
                                | Schema.TypeRef.Named(_, _ :: _) -> true
                                | _ -> false
                            | [] -> false
                        )
                    )

                match prov.TryLookupType name |> ExternalSymbols.typeShapeOf with
                | ValueSome(ExternalTypeShape.Class shape) ->
                    let baseCount = if shape.FrozenBaseType.IsSome then 1 else 0
                    let enumerableCount = if injectedEnumerable then 1 else 0

                    Expect.equal
                        (shape.FrozenInterfaces.Length + baseCount)
                        (heritage.Length + enumerableCount)
                        $"type '{name}' heritage (+ any synthesized enumerable) must populate FrozenInterfaces/FrozenBaseType"
                | _ -> ()

                for m in members do
                    let resolved = prov.TryLookupMembers(SymbolKeyOps.qualifiedTypeKey name 0, m.Name)
                    Expect.isGreaterThan resolved.Length 0 $"member '{name}.{m.Name}' should resolve"

                    // Overload identity: a method with N call signatures
                    // must expand into N members, each with its own `MemberKey` argSig —
                    // so the resolved count matches the signature count AND the keys are
                    // all distinct (no argSig collision survived). CONSTRUCTORS differ: a
                    // ctor dispatches on ARGUMENTS ALONE, so `overloadArgSigs` DEDUPES
                    // same-argSig ctor signatures (es2015's Error-subclass inherited+own
                    // pair, Map's cross-file merge) rather than throwing — N signatures
                    // collapse to the DISTINCT-argSig count. Assert that bound (≤ N, ≥ 1)
                    // plus distinct keys for ctors; keep strict per-signature parity for
                    // methods.
                    if m.Signatures.Length > 1 then
                        if m.Name = ".ctor" then
                            Expect.isLessThanOrEqual
                                resolved.Length
                                m.Signatures.Length
                                $"ctor '{name}.{m.Name}' resolves to at most one member per signature (same-argSig overloads dedupe)"

                            Expect.isGreaterThan
                                resolved.Length
                                0
                                $"ctor '{name}.{m.Name}' should resolve to at least one member"
                        else
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
                // Same lookup contract: a GENERIC alias (`Handler<T>`, mitt's `Handler`) is
                // keyed under its arity-suffixed name (`Handler\`1`), so suffix before the
                // lookup — the bare-name read is exactly the pre-existing miss this fixes.
                let name = SymbolKeyOps.arityName (q name) typeParams
                Expect.isTrue (prov.TryLookupType name).IsSome $"type alias '{name}' should resolve"

                // A generic alias (`Pair<A,B>`) resolves to an `Abbrev` whose arity equals
                // its `typeParams`.
                match prov.TryLookupType name |> ExternalSymbols.typeShapeOf with
                | ValueSome(ExternalTypeShape.Abbrev(arity, _)) ->
                    Expect.equal arity typeParams $"type alias '{name}' arity must equal its typeParams"
                | _ -> ()
            | Schema.Export.Enum(name, _) ->
                // The enum NAME resolves; its MEMBERS are stubbed on the provider, so only
                // the type-name resolution is asserted.
                Expect.isTrue (prov.TryLookupType(q name)).IsSome $"enum '{q name}' should resolve"
            | Schema.Export.Namespace(nsName, nested) ->
                // Item 17: the namespace container holds no symbol of its own; recurse into
                // its members under the extended prefix so each resolves via its qualified
                // name. Covers the nested namespace, proving the recursion folds depth.
                let childPrefix = qualify prefix nsName

                for nx in nested do
                    check childPrefix nx

        // A MOUNTED pack (`TsGlobalHomes.mountFor` non-empty, e.g. `es2015` → `Js`)
        // mounts every export under its Vesper-facing namespace, so the provider
        // registers `eval` as `Js.eval` and `Map` as `Js.Map\`2`. Start the resolution
        // walk at that mount prefix — the SAME single source the provider flattens from
        // — so a real package stays prefix "" (byte-identical) and es2015 resolves
        // through `Js`.
        let mountPrefix = TsGlobalHomes.mountFor man.Package

        for ex in man.Exports do
            check mountPrefix ex

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

/// Package-entry golden contract: run the compiled extractor in PACKAGE
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

/// Ambient-globals golden contract: run the compiled extractor in GLOBALS
/// mode (`--globals <packageName> <outPath> <dts…>`) over the fixture's sibling
/// `.d.ts` files (all fed to one program so the checker merges cross-file
/// declarations) and assert its output equals `globals/globals.manifest.json`. Same
/// skip/refresh semantics as the single-file path.
let testExtractorMatchesGoldenGlobals () =
    if not (File.Exists extractorJs.Value) then
        skiptest "extractor not built — run: dotnet fable src/Vesper.Ts.Extractor -o src/Vesper.Ts.Extractor/dist"

    let inputs = globalsDtsFiles.Value

    if inputs.Length = 0 then
        skiptest "no globals fixture present"

    let packageName = "globals"
    let golden = globalsManifest.Value
    let outPath = Path.Combine(Path.GetTempPath(), "globals.vesper.globals.out.json")

    let started =
        try
            let psi = ProcessStartInfo("node")
            psi.ArgumentList.Add extractorJs.Value
            psi.ArgumentList.Add "--globals"
            psi.ArgumentList.Add packageName
            psi.ArgumentList.Add outPath

            for dts in inputs do
                psi.ArgumentList.Add dts

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
            failtestf "globals extractor failed (exit %d): %s" p.ExitCode stderr

        let actual = File.ReadAllText outPath

        if updateSnapshots then
            File.WriteAllText(golden, actual.TrimEnd() + "\n")
        else
            Expect.equal
                (normalize actual)
                (normalize (File.ReadAllText golden))
                "Globals extractor output does not match the golden (run UPDATE_SNAPSHOTS=1 to refresh)"

/// Ambient-modules golden contract (W1, decision A): run the compiled extractor in
/// AMBIENT-MODULES mode (`--ambient-modules node <outDir> <dts…>`) over the fixture's
/// input `.d.ts` and assert the per-module manifests it writes into `outDir` equal the
/// committed `ambient-modules/<module>.manifest.json` goldens — SET and CONTENT. The
/// set equality is the "both modules enumerate" assertion; the content equality pins the
/// per-module home + the cross-module ref. Same skip/refresh semantics as the other
/// goldens; `UPDATE_SNAPSHOTS=1` rewrites the goldens from the extractor's output.
let testExtractorMatchesGoldenAmbientModules () =
    if not (File.Exists extractorJs.Value) then
        skiptest "extractor not built — run: dotnet fable src/Vesper.Ts.Extractor -o src/Vesper.Ts.Extractor/dist"

    let inputs = ambientModulesDtsFiles.Value

    if inputs.Length = 0 then
        skiptest "no ambient-modules fixture present"

    // The fixture stands in for `@types/node`, so it uses the same `node` package name —
    // its modules home `node/a`, `node/b` (the real `node/<module>` convention).
    let packageName = "node"
    let outDir = Path.Combine(Path.GetTempPath(), "vesper.ambient-modules.out")

    if Directory.Exists outDir then
        Directory.Delete(outDir, true)

    Directory.CreateDirectory outDir |> ignore

    let started =
        try
            let psi = ProcessStartInfo("node")
            psi.ArgumentList.Add extractorJs.Value
            psi.ArgumentList.Add "--ambient-modules"
            psi.ArgumentList.Add packageName
            psi.ArgumentList.Add outDir

            for dts in inputs do
                psi.ArgumentList.Add dts

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
            failtestf "ambient-modules extractor failed (exit %d): %s" p.ExitCode stderr

        let producedFiles = Directory.GetFiles(outDir, "*.manifest.json") |> Array.sort

        if updateSnapshots then
            // Replace the committed set wholesale: drop stale per-module goldens, write
            // each freshly produced manifest under its module basename.
            for old in ambientModulesManifests.Value do
                File.Delete old

            for produced in producedFiles do
                let golden = Path.Combine(ambientModulesDir.Value, Path.GetFileName produced)
                File.WriteAllText(golden, (File.ReadAllText produced).TrimEnd() + "\n")
        else
            let producedNames = producedFiles |> Array.map Path.GetFileName |> Array.sort

            let committedNames =
                ambientModulesManifests.Value |> Array.map Path.GetFileName |> Array.sort

            // Both modules enumerate: the produced set of per-module manifests equals the
            // committed set (no module dropped, none spuriously added).
            Expect.equal
                producedNames
                committedNames
                "ambient-modules produced manifest set must match the committed per-module goldens"

            for name in producedNames do
                Expect.equal
                    (normalize (File.ReadAllText(Path.Combine(outDir, name))))
                    (normalize (File.ReadAllText(Path.Combine(ambientModulesDir.Value, name))))
                    (sprintf
                        "ambient module manifest '%s' does not match its golden (run UPDATE_SNAPSHOTS=1 to refresh)"
                        name)

/// Real-scale `lib.es2015` golden contract: run the compiled extractor in
/// LIB-GLOBALS mode (`--lib-globals es2015 <outPath> <lib.es*.d.ts…>`) over the
/// vendored `typescript` package's own lib files and assert its output equals
/// `es2015/es2015.manifest.json`. Same skip/refresh semantics as the other goldens;
/// `UPDATE_SNAPSHOTS=1` (re)generates the vendored manifest from the real lib. This is
/// the sole producer of the burndown manifest the diagnostics contract asserts.
let testExtractorMatchesGoldenLibGlobals () =
    if not (File.Exists extractorJs.Value) then
        skiptest "extractor not built — run: dotnet fable src/Vesper.Ts.Extractor -o src/Vesper.Ts.Extractor/dist"

    let inputs = es2015LibFiles.Value

    if inputs |> Array.exists (File.Exists >> not) then
        skiptest "typescript lib files not present under node_modules; skipping es2015 extraction"

    let golden = es2015Manifest.Value
    let outPath = Path.Combine(Path.GetTempPath(), "es2015.vesper.lib.out.json")

    let started =
        try
            let psi = ProcessStartInfo("node")
            psi.ArgumentList.Add extractorJs.Value
            psi.ArgumentList.Add "--lib-globals"
            psi.ArgumentList.Add "es2015"
            psi.ArgumentList.Add outPath

            for dts in inputs do
                psi.ArgumentList.Add dts

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
            failtestf "es2015 lib extractor failed (exit %d): %s" p.ExitCode stderr

        let actual = File.ReadAllText outPath

        if updateSnapshots then
            Directory.CreateDirectory es2015Dir.Value |> ignore
            File.WriteAllText(golden, actual.TrimEnd() + "\n")
        else
            Expect.equal
                (normalize actual)
                (normalize (File.ReadAllText golden))
                "es2015 lib extractor output does not match the golden (run UPDATE_SNAPSHOTS=1 to refresh)"
