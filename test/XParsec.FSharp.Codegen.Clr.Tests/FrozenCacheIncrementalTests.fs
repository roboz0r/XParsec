module XParsec.FSharp.Codegen.Clr.Tests.FrozenCacheIncrementalTests

open System.IO
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The incremental gate for `ClrDriver.compileCached`, through the real driver: the cache hits
// when nothing changed, misses on a source edit, and misses on a changed dependency, so never
// a stale hit. Hit vs miss is read off a store that counts `Store` calls; only a MISS writes.

type private CountingStore() =
    let inner = Cache.InMemoryStore() :> ICacheStore
    let mutable stores = 0
    member _.Stores = stores

    interface ICacheStore with
        member _.TryLoad key = inner.TryLoad key

        member _.Store key bytes =
            stores <- stores + 1
            inner.Store key bytes

let private referenceAssemblies () : string list =
    match RefPack.resolve "net8.0" with
    | Result.Ok dlls -> dlls
    | Result.Error e -> failtestf "net8.0 ref pack unavailable: %s" e

let private inputsWith (manifests: string list) (name: string) : ClrCompilation =
    ClrCompilation.consumer (ProjectInfo.defaults name) manifests (referenceAssemblies ())

/// Structural, not raw bytes: a fresh MVID per compile makes identical source emit different
/// PE bytes run-to-run.
let private digestOf (artifact: ClrArtifact) : string =
    ClrStructuralDigest.ofBytes (Codegen.toBytes artifact)

/// The key `ClrDriver.compileCached` computes, so a test can assert the key MOVED without
/// observing the store.
let private keyOf (inputs: ClrCompilation) (source: string) : InputHash =
    Hashing.fileInputHash (Hashing.textOriginPath source) source (ClrDriver.compilationDigest inputs)

let private okArtifact (label: string) (result: Result<ClrArtifact, Diagnostic list>) : ClrArtifact =
    match result with
    | Ok a -> a
    | Error ds -> failtestf "%s: compile failed: %s" label (ds |> List.map (fun d -> d.Message) |> String.concat "\n")

/// A BCL-only static call the default `Vesper.Core` contract + net8.0 ref pack resolve.
let private printProgram (message: string) : string =
    sprintf "System.Console.WriteLine \"%s\"" message

/// A fixture package's manifest. It `depends-on` the real `Vesper.Core` because its contract
/// names `int`, which resolves through Vesper.Core and nowhere else. Forward slashes because a
/// TOML basic string reads `\` as an escape.
let private fixtureManifest (name: string) (lists: string) : string =
    sprintf "[core]\nname = \"%s\"\ndepends-on = [\"%s\"]\n%s" name (vesperCorePackage.Replace('\\', '/')) lists

[<Tests>]
let tests =
    testList
        "FrozenCache incremental (ClrDriver.compileCached)"
        [
            test "recompiling unchanged source hits and re-emits identically" {
                let store = CountingStore() :> ICacheStore
                let inputs = inputsWith [ vesperCorePackage ] "IncMissHit"
                let src = printProgram "hello"

                let first = okArtifact "miss" (ClrDriver.compileCached store inputs src)
                Expect.equal (store :?> CountingStore).Stores 1 "the first compile is a MISS and stores once"

                let second = okArtifact "hit" (ClrDriver.compileCached store inputs src)

                Expect.equal
                    (store :?> CountingStore).Stores
                    1
                    "the identical recompile is a HIT, so it does not store again"

                Expect.equal
                    (digestOf second)
                    (digestOf first)
                    "the hit re-emits a structurally identical assembly (round-trip is codegen-invariant)"
            }

            test "editing the source misses and emits the edited assembly" {
                let store = CountingStore() :> ICacheStore
                let inputs = inputsWith [ vesperCorePackage ] "IncEdit"

                // An INTEGER-literal edit, not a string one: the digest folds `ldstr`'s `#US`
                // heap index, so two lone string literals share a token and would not perturb
                // it. `ldc.i4`'s inline operand does.
                let first = okArtifact "v1" (ClrDriver.compileCached store inputs "let x = 1")
                Expect.equal (store :?> CountingStore).Stores 1 "the first compile stores once"

                let second = okArtifact "v2" (ClrDriver.compileCached store inputs "let x = 2")

                Expect.equal
                    (store :?> CountingStore).Stores
                    2
                    "the edited source is a new key, so it is a MISS that stores again"

                Expect.notEqual
                    (digestOf second)
                    (digestOf first)
                    "the edit changed the emitted assembly (different integer literal)"
            }

            test "a changed dependency contract misses (dependency invalidation)" {
                // A custom package (its own namespace, one inert `val` the program never
                // references) added to `Manifests` beside `Vesper.Core`, so its `.fsi` bytes
                // fold into the cache key. It perturbs only the KEY, which is the seam under test.
                let root = tmpDir "frozen-cache-dep-inval"
                let pkgDir = Path.Combine(root, "Extra")
                Directory.CreateDirectory pkgDir |> ignore
                let fsiPath = Path.Combine(pkgDir, "extra.fsi")
                let manifestPath = Path.Combine(pkgDir, "manifest.clr.toml")

                File.WriteAllText(manifestPath, fixtureManifest "Extra" "files = [\"extra.fsi\"]\n")

                let contract (marker: string) : string =
                    sprintf "namespace Extra\n\nmodule ExtraContract =\n\n    /// %s\n    val extraMarker: int\n" marker

                File.WriteAllText(fsiPath, contract "marker v1")

                let store = CountingStore() :> ICacheStore
                let inputs = inputsWith [ vesperCorePackage; pkgDir ] "IncDep"
                let src = printProgram "hello"

                // The key the driver computes, before the dependency changes.
                let keyBefore = keyOf inputs src

                let first = okArtifact "before" (ClrDriver.compileCached store inputs src)
                Expect.equal (store :?> CountingStore).Stores 1 "the first compile stores once"

                okArtifact "unchanged" (ClrDriver.compileCached store inputs src) |> ignore

                Expect.equal
                    (store :?> CountingStore).Stores
                    1
                    "an unchanged recompile hits, because the dependency bytes are unchanged"

                // Change the referenced contract's bytes: the dependency signature hash changes,
                // so the SAME source against the SAME manifest path is now a distinct key.
                File.WriteAllText(fsiPath, contract "marker v2")

                let keyAfter = keyOf inputs src
                Expect.notEqual keyBefore keyAfter "a changed dependency contract changes the driver's cache key"

                let second = okArtifact "after" (ClrDriver.compileCached store inputs src)
                Expect.equal (store :?> CountingStore).Stores 2 "the changed dependency is a MISS, not a stale hit"

                // Same source, so the emitted assembly is unchanged; only the key moved.
                Expect.equal
                    (digestOf second)
                    (digestOf first)
                    "the program is unchanged, so it still emits identically; only the dependency key moved"
            }

            test "a changed dependency INLINE BODY misses" {
                // An `impl` `.fs` is not a signature file (it is not in `files`), but its bodies are
                // SPLICED into the consumer before freeze, so its bytes are a compile
                // determinant. Only the MISS, not the output, can witness the key moving.
                let root = tmpDir "frozen-cache-inline-body-inval"
                let pkgDir = Path.Combine(root, "Inl")
                Directory.CreateDirectory pkgDir |> ignore
                let fsPath = Path.Combine(pkgDir, "ops.fs")
                let manifestPath = Path.Combine(pkgDir, "manifest.clr.toml")

                File.WriteAllText(manifestPath, fixtureManifest "Inl" "files = [\"inl.fsi\"]\nimpl = [\"ops.fs\"]\n")

                File.WriteAllText(
                    Path.Combine(pkgDir, "inl.fsi"),
                    "namespace Inl\n\nmodule InlContract =\n\n    val inline bump: int -> int\n"
                )

                let body (increment: int) : string =
                    sprintf "namespace Inl\n\nmodule InlContract =\n\n    let inline bump (x: int) = x + %d\n" increment

                File.WriteAllText(fsPath, body 1)

                let store = CountingStore() :> ICacheStore
                let inputs = inputsWith [ vesperCorePackage; pkgDir ] "IncInline"
                let src = printProgram "hello"

                let keyBefore = keyOf inputs src

                let first = okArtifact "before" (ClrDriver.compileCached store inputs src)
                Expect.equal (store :?> CountingStore).Stores 1 "the first compile stores once"

                okArtifact "unchanged" (ClrDriver.compileCached store inputs src) |> ignore
                Expect.equal (store :?> CountingStore).Stores 1 "an unchanged recompile hits"

                // Edit ONLY the inline body. No `.fsi` moves.
                File.WriteAllText(fsPath, body 2)

                Expect.notEqual keyBefore (keyOf inputs src) "an edited inline body changes the driver's cache key"

                let second = okArtifact "after" (ClrDriver.compileCached store inputs src)

                Expect.equal (store :?> CountingStore).Stores 2 "the changed inline body is a MISS, not a stale hit"

                // The program never calls `bump`, so the miss re-emits the same assembly.
                Expect.equal
                    (digestOf second)
                    (digestOf first)
                    "the program does not reference the body, so it still emits identically"
            }
        ]
