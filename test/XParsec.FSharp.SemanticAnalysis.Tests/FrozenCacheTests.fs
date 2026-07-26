module XParsec.FSharp.SemanticAnalysis.Tests.FrozenCacheTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// `FrozenCache.freezeResult` is the error-aware store wrapper the production driver keys on:
// an errored front end must be returned as `Error` and NEVER stored (storing it would serve a
// stale failure back), while an `Ok` stores once and re-serves on the next call as a hit. These
// tests pin exactly that — an errored `produce` leaves the store empty; an `Ok` stores once and
// the second call hits without re-running `produce`.

let private freezeKey (hex: string) : CacheKey =
    {
        Query = QueryId.Freeze
        CodeVersion = Cache.CodeVersion
        Input = InputHash.ofHex hex
    }

/// Counts `Store` calls over a real `InMemoryStore`, so a test can assert an errored produce
/// stored nothing (count stays 0) and an `Ok` stored exactly once.
type private CountingStore() =
    let inner = Cache.InMemoryStore() :> ICacheStore
    let mutable stores = 0
    member _.Stores = stores

    interface ICacheStore with
        member _.TryLoad key = inner.TryLoad key

        member _.Store key bytes =
            stores <- stores + 1
            inner.Store key bytes

/// A real frozen tree for a small program the shared contract stack resolves — the payload the
/// cache actually round-trips.
let private frozenSample () : FrozenPools =
    let src = "let add x y = x + y\nlet answer = add 1 40\n"
    let lexed, file = parseFile src
    Pipeline.analyseFor "TestAsm" realProvider.Value src lexed file

[<Tests>]
let tests =
    testList
        "FrozenCache.freezeResult"
        [
            test "an Error from produce is returned and NOT stored" {
                let store = CountingStore()

                let result: Result<FrozenPools, string> =
                    FrozenCache.freezeResult store (freezeKey "aa01") (fun () -> Error "front-end failed")

                Expect.equal result (Error "front-end failed") "the produce error propagates"
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
                | Ok f -> Expect.equal f frozen "the miss returns the produced tree"
                | Error e -> failtestf "expected Ok on the miss, got Error %A" e

                let second = FrozenCache.freezeResult store key produce
                Expect.equal produced 1 "the hit did NOT re-run produce"
                Expect.equal store.Stores 1 "the hit stored nothing new"

                // A hit thaws the STORED blob, so its own re-flatten equals the bytes that were
                // stored — the serialization round-trip that makes the cache sound (asserting the
                // whole `TastFile` by `=` is the wrong contract: side-table map ordering is free to
                // differ, and the codegen-invariance the cache actually needs is gated at the CLR
                // layer by digest, not here).
                match second with
                | Ok f ->
                    Expect.equal
                        (FrozenCodec.flatten f)
                        (FrozenCodec.flatten frozen)
                        "the hit re-flattens to the stored blob (serialization round-trip)"
                | Error e -> failtestf "expected Ok on the hit, got Error %A" e
            }
        ]
