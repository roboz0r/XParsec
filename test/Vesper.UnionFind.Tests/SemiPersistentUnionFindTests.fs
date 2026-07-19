module Vesper.UnionFind.Tests.SemiPersistentUnionFindTests

open System

open Expecto

open Vesper.UnionFind

/// A phantom id space for the tests — proves the measure tag threads through store and UF.
[<Measure>]
type e

/// An ordinary imperative union-find over a fixed domain, used as the oracle for the property
/// test.
type private RefUnionFind(n: int) =
    let parent = Array.init n id

    let rec find i =
        let p = parent[i]

        if p = i then
            i
        else
            let r = find p
            parent[i] <- r
            r

    member _.Union(a, b) =
        let ra = find a
        let rb = find b

        if ra <> rb then
            parent[ra] <- rb

    member _.Equivalent(a, b) = find a = find b

let private letters = [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ]

/// A store with `letters` interned in order (ids "a"=0 .. "j"=9).
let private newStore () =
    let s = DynamicStore<string, e>(letters.Length)

    for x in letters do
        s.GetOrAdd x |> ignore

    s

// A *fresh* empty each call: a SemiPersistentUnionFind is semi-persistent mutable state, so a
// shared instance would leak rerooting/invalidation across tests.
let private empty () : SemiPersistentUnionFind<e> = SemiPersistentUnionFind<e>.Empty

let private repOf (s: DynamicStore<string, e>) (uf: SemiPersistentUnionFind<e>) (x: string) = s[uf.Find(s.GetOrAdd x)]

let private equiv (s: DynamicStore<string, e>) (uf: SemiPersistentUnionFind<e>) (x: string) (y: string) =
    uf.Equivalent(s.GetOrAdd x, s.GetOrAdd y)

let private union (s: DynamicStore<string, e>) (uf: SemiPersistentUnionFind<e>) (x: string) (y: string) =
    uf.Union(s.GetOrAdd x, s.GetOrAdd y)

[<Tests>]
let tests =
    testList
        "SemiPersistentUnionFind"
        [
            test "store mints one stable, monotone id per distinct element" {
                let s = DynamicStore<string, e>(8)
                Expect.equal (s.TryId "a") ValueNone "unseen element has no id"
                let a1 = s.GetOrAdd "a"
                let b = s.GetOrAdd "b"
                let a2 = s.GetOrAdd "a"
                Expect.equal a1 a2 "the same element re-interns to the same id"
                Expect.notEqual a1 b "distinct elements get distinct ids"
                Expect.equal (s.TryId "a") (ValueSome a1) "TryId returns the minted id"
                Expect.equal s["a" |> s.GetOrAdd] "a" "id round-trips back to the element"
                Expect.equal s.Count 2 "two distinct elements interned"
            }

            test "fresh: every element is its own representative" {
                let s = newStore ()

                for x in letters do
                    Expect.equal (repOf s (empty ()) x) x (sprintf "%s should be its own rep" x)
            }

            test "union merges classes; find and equivalent agree" {
                let s = newStore ()
                let uf = union s (empty ()) "a" "b"
                Expect.equal (repOf s uf "a") (repOf s uf "b") "a and b share a representative"
                Expect.isTrue (equiv s uf "a" "b") "a ~ b"
                Expect.isFalse (equiv s uf "a" "c") "a is not merged with c"
            }

            test "equivalence is transitive across a chain of unions" {
                let s = newStore ()
                let uf = empty ()
                let uf = union s uf "a" "b"
                let uf = union s uf "b" "c"
                let uf = union s uf "c" "d"
                Expect.isTrue (equiv s uf "a" "d") "a ~ d through the chain"
                Expect.isFalse (equiv s uf "a" "e") "e stayed separate"
            }

            test "find is idempotent (path compression preserves representatives)" {
                let s = newStore ()
                let uf = union s (union s (empty ()) "a" "b") "a" "c"
                let r1 = repOf s uf "b"
                let r2 = repOf s uf "b"
                Expect.equal r1 r2 "repeated find is stable"
            }

            test "an interned but never-unioned element is its own class" {
                let s = newStore ()
                let uf = union s (empty ()) "a" "b"
                // "j" was interned (id 9) but never unioned, and uf.Count is 2.
                Expect.equal (repOf s uf "j") "j" "an unmaterialized id is its own rep"
                Expect.isFalse (equiv s uf "j" "a") "j is merged with no one"
            }

            test "union of an already-merged pair returns the same instance (no growth)" {
                let s = newStore ()
                let uf1 = union s (empty ()) "a" "b"
                let uf2 = union s uf1 "a" "b"
                Expect.isTrue (Object.ReferenceEquals(uf1, uf2)) "no-op union shares the instance"
            }

            test "the id space grows on demand as unseen ids are unioned" {
                let s = newStore ()
                let uf = union s (empty ()) "a" "b" // materializes ids 0,1
                Expect.equal uf.Count 2 "count tracks the materialized high-water mark"
                let uf = union s uf "h" "i" // ids 7,8 — grows to cover the gap
                Expect.equal uf.Count 9 "count grew to include the highest touched id"
                Expect.isTrue (equiv s uf "h" "i") "h ~ i after the growing union"
                Expect.isTrue (equiv s uf "a" "b") "the earlier merge survived growth"
                Expect.isFalse (equiv s uf "a" "h") "the two merges stayed separate"
            }

            test "an ancestor version is unaffected by unions in its descendants" {
                let s = newStore ()
                let a = empty ()
                let b = union s a "a" "b"
                let c = union s b "c" "d"
                let d = union s c "a" "c" // merges {a,b} with {c,d}

                // Assert everything about the newest version first.
                Expect.isTrue (equiv s d "a" "d") "d: a ~ d"
                Expect.isTrue (equiv s d "b" "c") "d: b ~ c"
                Expect.isFalse (equiv s d "a" "e") "d: a is not merged with e"

                // Rolling back to the ancestor b must show only b's merge — the c/d merges did
                // not leak backwards.
                Expect.isTrue (equiv s b "a" "b") "b: a ~ b"
                Expect.isFalse (equiv s b "c" "d") "b: c and d never merged in b"
                Expect.isFalse (equiv s b "a" "c") "b: a and c never merged in b"
            }

            test "semi-persistence: a newer version is invalid after rolling back past it" {
                let s = newStore ()
                let a = empty ()
                let b = union s a "a" "b"
                let c = union s b "c" "d"

                // Roll back to the ancestor b (reroots the shared array and invalidates c).
                equiv s b "a" "b" |> ignore

                Expect.throwsT<InvalidOperationException>
                    (fun () -> equiv s c "c" "d" |> ignore)
                    "accessing the backtracked-past version raises"
            }

            testProperty "matches an imperative union-find as the id space grows"
            <| fun (ops: (int * int) list) ->
                let dom = 40
                let oracle = RefUnionFind(dom)
                let s = DynamicStore<int, e>(dom)
                let mutable uf = SemiPersistentUnionFind<e>.Empty

                for (i, j) in ops do
                    let a = abs (i % dom)
                    let b = abs (j % dom)
                    oracle.Union(a, b)
                    uf <- uf.Union(s.GetOrAdd a, s.GetOrAdd b) // linear use: always the newest version

                seq {
                    for a in 0 .. dom - 1 do
                        for b in 0 .. dom - 1 -> oracle.Equivalent(a, b) = uf.Equivalent(s.GetOrAdd a, s.GetOrAdd b)
                }
                |> Seq.forall id
        ]
