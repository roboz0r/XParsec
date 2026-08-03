module XParsec.FSharp.Codegen.Js.Tests.FrozenCacheHitMissTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Common.Tests.Conformance
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// The hit/miss parity gate for the cache-wrapped freeze: a MISS runs `produce` exactly once and
// stores the blob; a subsequent HIT does NOT run `produce` and returns a tree that emits
// byte-identical JS. Freezing + JS codegen are one call here (`frozenOfJs` / the conformance
// pipeline), so this is where "cached output byte-identical" is judged against real programs.
//
// Gated set = programs the JS backend actually COMPILES, mirroring the direct/round-trip
// byte-identity gates so `frozenOfJs` never trips on a `Diagnose` program's diagnostics.

/// Programs the JS backend compiles.
let private gated =
    programs
    |> List.filter (fun p ->
        match Map.tryFind "js" p.Obligations with
        | Some Obligation.Run
        | Some(Obligation.Fault _) -> true
        | _ -> false
    )

/// The compilation every corpus program is keyed under. Folded ONCE for the whole list, which
/// is what `Hashing.CompilationDigest` exists to make natural — the programs stand alone, so
/// the digest is the same for all of them and only the source varies.
let private conformanceDigest =
    Hashing.compilationDigest
        {
            HomeAssembly = "Conformance"
            Target = Target.Js
            ReferenceAssemblies = []
            Manifests = []
            SelfManifest = None
        }

/// The cache key a driver would assemble for a source file with no dependencies — the query tag
/// and the poison-guard version live in the key (the cache module is query-agnostic). An empty
/// environment is fine here: the corpus programs stand alone, and this gate is about hit/miss
/// parity of the blob, not about what moves a key.
let private freezeKey (src: string) : CacheKey =
    {
        Query = QueryId.Freeze
        CodeVersion = Cache.CodeVersion
        Input = Hashing.fileInputHash (Hashing.textOriginPath src) src conformanceDigest
    }

[<Tests>]
let tests =
    testList
        "FrozenCache hit/miss parity"
        [
            for p in gated do
                test p.Name {
                    let name = "conformance-" + p.Name
                    let store = Cache.InMemoryStore() :> ICacheStore
                    let key = freezeKey p.Source

                    let mutable calls = 0

                    let produce () =
                        calls <- calls + 1
                        frozenOfJs p.Source

                    // MISS: produce runs once and the blob lands in the store.
                    let missTree = FrozenCache.freeze store key produce
                    Expect.equal calls 1 "produce called exactly once on the miss"
                    Expect.isTrue (store.TryLoad key).IsSome "store now holds the key"

                    // HIT: produce is NOT run again — the tree comes from the cached blob.
                    let hitTree = FrozenCache.freeze store key produce
                    Expect.equal calls 1 "produce not called again on the hit"

                    // The parity gate: the cached tree emits byte-identical JS.
                    Expect.equal
                        (emitFrozenJs name p.Source hitTree)
                        (emitFrozenJs name p.Source missTree)
                        "cached (hit) tree emits byte-identical JS to the freshly frozen (miss) tree"
                }

            // The gate must exercise a non-trivial corpus, else an empty run passes vacuously.
            test "the gated corpus is non-empty" { Expect.isGreaterThan (List.length gated) 0 "JS-gated programs" }
        ]
