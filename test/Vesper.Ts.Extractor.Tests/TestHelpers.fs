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

/// Loader invariant: every `Function` export resolves via `TryLookup`; every
/// `Interface`/`Class` via `TryLookupType`, and each member via `TryLookupMembers`.
let testProviderResolves (path: string) =
    match Codec.deserialize (File.ReadAllText path) with
    | Error e -> failtestf "manifest does not parse: %s" e
    | Ok man ->
        let prov = TsManifestProvider.providerOfManifest man

        for ex in man.Exports do
            match ex with
            | Schema.Export.Function(name, _, _) ->
                Expect.isTrue (prov.TryLookup name).IsSome $"function '{name}' should resolve"
            | Schema.Export.Interface(name, _, members, _)
            | Schema.Export.Class(name, _, members, _, _) ->
                Expect.isTrue (prov.TryLookupType name).IsSome $"type '{name}' should resolve"

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
                Expect.isTrue (prov.TryLookup name).IsSome $"variable '{name}' should resolve"
            | Schema.Export.TypeAlias(name, _, _) ->
                Expect.isTrue (prov.TryLookupType name).IsSome $"type alias '{name}' should resolve"
            | Schema.Export.Enum(name, _) ->
                // The enum NAME resolves (an `Opaque` shape); its MEMBERS are stubbed on
                // the provider, so only the type-name resolution is asserted.
                Expect.isTrue (prov.TryLookupType name).IsSome $"enum '{name}' should resolve"
            | _ -> ()

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
