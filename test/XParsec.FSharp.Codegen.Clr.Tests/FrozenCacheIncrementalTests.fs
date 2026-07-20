module XParsec.FSharp.Codegen.Clr.Tests.FrozenCacheIncrementalTests

open System.IO
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The end-to-end incremental gate for `ClrDriver.compileCached`: a compile → recompile → edit →
// dependency-change sequence, driven through the REAL production driver, proving the opt-in cache
// hits when nothing changed, misses on a source edit, and misses on a changed dependency contract
// (never a stale hit). Hit vs miss is detected by a store that counts `Store` calls: only a MISS
// writes, so a hit leaves the count unchanged. A hit's re-emit is compared by
// `ClrStructuralDigest` (not raw bytes: `Metadata.fs` mints a fresh MVID per compile, so raw PE
// bytes differ run-to-run for identical source — see `ConformanceByteIdentityTests`).

/// Wraps an `InMemoryStore` and counts `Store` calls, so a test reads hit-vs-miss off the count:
/// a MISS stores (count increments), a HIT does not.
type private CountingStore() =
    let inner = Cache.InMemoryStore() :> ICacheStore
    let mutable stores = 0
    member _.Stores = stores

    interface ICacheStore with
        member _.TryLoad key = inner.TryLoad key

        member _.Store key bytes =
            stores <- stores + 1
            inner.Store key bytes

let private bclReferences () : string list =
    match RefPack.resolve "net8.0" with
    | Result.Ok dlls -> dlls
    | Result.Error e -> failtestf "net8.0 ref pack unavailable: %s" e

let private inputsWith (manifests: string list) (name: string) : ClrCompilation =
    {
        Project = ProjectInfo.defaults name
        Manifests = manifests
        BclReferences = bclReferences ()
    }

let private digestOf (artifact: ClrArtifact) : string =
    ClrStructuralDigest.ofBytes(Codegen.toBytes artifact)

let private okArtifact (label: string) (result: Result<ClrArtifact, Diagnostic list>) : ClrArtifact =
    match result with
    | Ok a -> a
    | Error ds -> failtestf "%s: compile failed: %s" label (ds |> List.map (fun d -> d.Message) |> String.concat "\n")

/// A BCL-only static call the default `Vesper.Core` contract + net8.0 ref pack resolve (the same
/// program shape `ClrDriverTests` compiles); the string literal varies to model a source edit.
let private printProgram (message: string) : string =
    sprintf "System.Console.WriteLine \"%s\"" message

[<Tests>]
let tests =
    testList
        "FrozenCache incremental (ClrDriver.compileCached)"
        [
            test "recompiling unchanged source hits and re-emits identically" {
                let store = CountingStore() :> ICacheStore
                let inputs = inputsWith [ vesperCoreManifest ] "IncMissHit"
                let src = printProgram "hello"

                let first = okArtifact "miss" (ClrDriver.compileCached store inputs src)
                Expect.equal (store :?> CountingStore).Stores 1 "the first compile is a MISS and stores once"

                let second = okArtifact "hit" (ClrDriver.compileCached store inputs src)
                Expect.equal (store :?> CountingStore).Stores 1 "the identical recompile is a HIT — no new store"

                Expect.equal
                    (digestOf second)
                    (digestOf first)
                    "the hit re-emits a structurally identical assembly (round-trip is codegen-invariant)"
            }

            test "editing the source misses and emits the edited assembly" {
                let store = CountingStore() :> ICacheStore
                let inputs = inputsWith [ vesperCoreManifest ] "IncEdit"

                // An INTEGER-literal edit (`ldc.i4` operand), not a string edit: the structural
                // digest folds the `ldstr` token (a `#US` heap index), so two lone string literals
                // share a token and would not perturb it — an integer literal is an inline IL
                // operand the digest does fold.
                let first = okArtifact "v1" (ClrDriver.compileCached store inputs "let x = 1")
                Expect.equal (store :?> CountingStore).Stores 1 "the first compile stores once"

                let second = okArtifact "v2" (ClrDriver.compileCached store inputs "let x = 2")
                Expect.equal (store :?> CountingStore).Stores 2 "the edited source is a new key — a MISS that stores again"

                Expect.notEqual
                    (digestOf second)
                    (digestOf first)
                    "the edit changed the emitted assembly (different integer literal)"
            }

            test "a changed dependency contract misses (dependency invalidation)" {
                // A resolvable custom package (its own namespace, no `depends-on`, one inert `val`
                // the program never references) added to `Manifests` beside `Vesper.Core`, so its
                // `.fsi` bytes fold into the driver's cache key via `Hashing.fileInputHash`. The
                // program compiles against `Vesper.Core` alone; the extra contract only perturbs
                // the KEY, which is exactly the dependency-signature seam under test. Fixture lives
                // under repo `./tmp` (repo convention), rewritten fresh so a prior run cannot leak.
                let root = tmpDir "frozen-cache-dep-inval"
                let pkgDir = Path.Combine(root, "Extra")
                Directory.CreateDirectory pkgDir |> ignore
                let fsiPath = Path.Combine(pkgDir, "extra.fsi")
                let manifestPath = Path.Combine(pkgDir, "manifest.toml")

                File.WriteAllText(
                    manifestPath,
                    "[core]\nname = \"Extra\"\nnamespace = \"Extra\"\nfiles = [\"extra.fsi\"]\n"
                )

                let contract (marker: string) : string =
                    sprintf "namespace Extra\n\nmodule ExtraContract =\n\n    /// %s\n    val extraMarker: int\n" marker

                File.WriteAllText(fsiPath, contract "marker v1")

                let store = CountingStore() :> ICacheStore
                let inputs = inputsWith [ vesperCoreManifest; manifestPath ] "IncDep"
                let src = printProgram "hello"

                // The key the driver computes, before the dependency changes.
                let keyBefore = Hashing.fileInputHash src inputs.Manifests

                let first = okArtifact "before" (ClrDriver.compileCached store inputs src)
                Expect.equal (store :?> CountingStore).Stores 1 "the first compile stores once"

                okArtifact "unchanged" (ClrDriver.compileCached store inputs src) |> ignore
                Expect.equal (store :?> CountingStore).Stores 1 "an unchanged recompile hits — dependency bytes unchanged"

                // Change the referenced contract's bytes: the dependency signature hash changes,
                // so the SAME source against the SAME manifest path is now a distinct key.
                File.WriteAllText(fsiPath, contract "marker v2")

                let keyAfter = Hashing.fileInputHash src inputs.Manifests
                Expect.notEqual keyBefore keyAfter "a changed dependency contract changes the driver's cache key"

                let second = okArtifact "after" (ClrDriver.compileCached store inputs src)
                Expect.equal (store :?> CountingStore).Stores 2 "the changed dependency is a MISS, not a stale hit"

                // Same source, so the emitted assembly is unchanged; only the key moved.
                Expect.equal
                    (digestOf second)
                    (digestOf first)
                    "the program is unchanged, so it still emits identically — only the dependency key moved"
            }
        ]
