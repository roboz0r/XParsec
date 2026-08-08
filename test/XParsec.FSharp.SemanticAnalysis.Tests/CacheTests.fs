module XParsec.FSharp.SemanticAnalysis.Tests.CacheTests

open System.IO
open Expecto
open XParsec.FSharp.SemanticAnalysis

let private key query codeVersion (hex: string) : CacheKey =
    {
        Query = query
        CodeVersion = codeVersion
        Input = InputHash.ofHex hex
    }

let private freezeKey = key QueryId.Freeze Cache.CodeVersion "abcdef01"

/// A directory under the repo `./tmp`, per test name, emptied on the way in so a prior run
/// cannot leak a hit.
let private freshRoot (name: string) : string =
    let root =
        Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "tmp", "cache-tests", name)

    if Directory.Exists root then
        Directory.Delete(root, true)

    Directory.CreateDirectory root |> ignore
    root

/// The shared round-trip contract, exercised against both store implementations.
let private roundTripSuite (name: string) (make: unit -> ICacheStore) =
    testList
        name
        [
            test "store then load returns the identical bytes" {
                let store = make ()
                let payload = [| 1uy; 2uy; 3uy; 42uy |]
                store.Store freezeKey payload
                Expect.equal (store.TryLoad freezeKey) (ValueSome payload) "round-trips the payload"
            }

            test "load miss returns none" {
                let store = make ()
                Expect.equal (store.TryLoad freezeKey) ValueNone "never-stored key misses"
            }

            test "distinct QueryId does not collide" {
                let store = make ()
                let a = key QueryId.Freeze Cache.CodeVersion "aa"
                let b = key QueryId.Signature Cache.CodeVersion "aa"
                store.Store a [| 10uy |]
                store.Store b [| 20uy |]
                Expect.equal (store.TryLoad a) (ValueSome [| 10uy |]) "a intact"
                Expect.equal (store.TryLoad b) (ValueSome [| 20uy |]) "b intact"
            }

            test "distinct codeVersion does not collide" {
                let store = make ()
                let a = key QueryId.Freeze 1 "bb"
                let b = key QueryId.Freeze 2 "bb"
                store.Store a [| 11uy |]
                store.Store b [| 22uy |]
                Expect.equal (store.TryLoad a) (ValueSome [| 11uy |]) "a intact"
                Expect.equal (store.TryLoad b) (ValueSome [| 22uy |]) "b intact"
            }

            test "distinct inputHash does not collide" {
                let store = make ()
                let a = key QueryId.Freeze Cache.CodeVersion "cc"
                let b = key QueryId.Freeze Cache.CodeVersion "dd"
                store.Store a [| 33uy |]
                store.Store b [| 44uy |]
                Expect.equal (store.TryLoad a) (ValueSome [| 33uy |]) "a intact"
                Expect.equal (store.TryLoad b) (ValueSome [| 44uy |]) "b intact"
            }
        ]

[<Tests>]
let tests =
    testList
        "Cache"
        [
            roundTripSuite "InMemoryStore" (fun () -> Cache.InMemoryStore() :> ICacheStore)

            // Every case wipes and recreates the one `roundtrip` root, so run concurrently a
            // sibling's `freshRoot` would delete this case's blob between its store and load.
            testSequenced (
                roundTripSuite "FileSystemStore" (fun () -> Cache.FileSystemStore(freshRoot "roundtrip") :> ICacheStore)
            )

            test "FileSystemStore persists across a fresh instance on the same root" {
                let root = freshRoot "persistence"
                let payload = [| 7uy; 8uy; 9uy |]
                (Cache.FileSystemStore root :> ICacheStore).Store freezeKey payload
                let reopened = Cache.FileSystemStore root :> ICacheStore
                Expect.equal (reopened.TryLoad freezeKey) (ValueSome payload) "reads back from disk"
                Directory.Delete(root, true)
            }
        ]
