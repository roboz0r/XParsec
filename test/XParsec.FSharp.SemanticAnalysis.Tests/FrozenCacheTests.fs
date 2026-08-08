module XParsec.FSharp.SemanticAnalysis.Tests.FrozenCacheTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// `freezeResult` is the error-aware store wrapper: an errored front end is returned as `Error`
// and NEVER stored, since storing it would serve a stale failure back on every later compile.

let private freezeKey (hex: string) : CacheKey =
    {
        Query = QueryId.Freeze
        CodeVersion = Cache.CodeVersion
        Input = InputHash.ofHex hex
    }

/// Counts `Store` calls over a real `InMemoryStore`.
type private CountingStore() =
    let inner = Cache.InMemoryStore() :> ICacheStore
    let mutable stores = 0
    member _.Stores = stores

    interface ICacheStore with
        member _.TryLoad key = inner.TryLoad key

        member _.Store key bytes =
            stores <- stores + 1
            inner.Store key bytes

/// A real frozen tree — the payload the cache round-trips.
let private frozenSample () : FrozenPools =
    freezeFor "let add x y = x + y\nlet answer = add 1 40\n"

[<Tests>]
let tests =
    testList
        "FrozenCache.freezeResult"
        [
            test "an Error from produce is returned and NOT stored" {
                let store = CountingStore()

                let result: Result<FrozenPools, string> =
                    FrozenCache.freezeResult store (freezeKey "aa01") (fun () -> Error "front-end failed")

                match result with
                | Error e -> Expect.equal e "front-end failed" "the produce error propagates"
                | Ok _ -> failtest "expected the produce Error to propagate, got Ok"

                Expect.equal store.Stores 0 "an errored compile stores nothing"
                Expect.equal ((store :> ICacheStore).TryLoad(freezeKey "aa01")) ValueNone "no blob under the key"
            }

            test "an Ok stores once and a second call hits without re-running produce" {
                let store = CountingStore()
                let key = freezeKey "bb02"
                let frozen = frozenSample ()
                let mutable produced = 0

                let produce () : Result<FrozenPools, string> =
                    produced <- produced + 1
                    Ok frozen

                let first = FrozenCache.freezeResult store key produce
                Expect.equal produced 1 "the miss ran produce"
                Expect.equal store.Stores 1 "the miss stored exactly once"

                match first with
                | Ok f -> Expect.isTrue (obj.ReferenceEquals(f, frozen)) "the miss returns the produced tree itself"
                | Error e -> failtestf "expected Ok on the miss, got Error %A" e

                let second = FrozenCache.freezeResult store key produce
                Expect.equal produced 1 "the hit did NOT re-run produce"
                Expect.equal store.Stores 1 "the hit stored nothing new"

                // A hit thaws the STORED blob, so its own re-flatten equals the bytes stored.
                // Asserting the whole tree by `=` would be the wrong contract: side-table map
                // ordering is free to differ.
                match second with
                | Ok f ->
                    Expect.equal
                        (FrozenCodec.flatten f)
                        (FrozenCodec.flatten frozen)
                        "the hit re-flattens to the stored blob (serialization round-trip)"
                | Error e -> failtestf "expected Ok on the hit, got Error %A" e
            }
        ]
