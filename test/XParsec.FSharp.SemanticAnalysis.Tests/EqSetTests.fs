module XParsec.FSharp.SemanticAnalysis.Tests.EqSetTests

open Expecto
open XParsec.FSharp.SemanticAnalysis

// `EqSet<'T>` is the set-semantic sibling of `EqArray`: insertion-ordered
// storage, order-INSENSITIVE equality + hash, dedupe-on-construction (keep first).
// It backs anonymous-union (`FTOr`/`TyOr`) members (codegen-js-symbol-provider
// design §"Literal types stay structural … EqSet sub-decision"). These pin the
// primitive directly, with nothing else in the loop.

[<Tests>]
let tests =
    testList
        "EqSet"
        [
            test "iteration preserves declared (insertion) order" {
                let s = EqSet.ofSeq [ "a"; "b"; "c" ]
                Expect.equal (EqSet.toList s) [ "a"; "b"; "c" ] "members iterate in declared order"
                Expect.equal s.Length 3 "three distinct members"
                Expect.equal s.[0] "a" "indexer reads insertion order"
                Expect.equal s.[2] "c" "indexer reads insertion order"
            }

            test "equality is order-insensitive: [a;b] = [b;a]" {
                let ab = EqSet.ofSeq [ "a"; "b" ]
                let ba = EqSet.ofSeq [ "b"; "a" ]
                Expect.equal ab ba "same member set, different order, equal"
                Expect.notEqual ab (EqSet.ofSeq [ "a"; "c" ]) "different member set, not equal"
                Expect.notEqual ab (EqSet.ofSeq [ "a" ]) "different cardinality, not equal"
            }

            test "hashes agree for equal (permuted) sets" {
                let ab = EqSet.ofSeq [ 1; 2; 3 ]
                let ba = EqSet.ofSeq [ 3; 1; 2 ]
                Expect.equal ab ba "permutation is equal"
                Expect.equal (ab.GetHashCode()) (ba.GetHashCode()) "equal sets hash identically (commutative combine)"
            }

            test "construction dedupes, keeping the first occurrence" {
                let s = EqSet.ofSeq [ "a"; "b"; "a"; "c"; "b" ]
                Expect.equal (EqSet.toList s) [ "a"; "b"; "c" ] "duplicates dropped, first-seen order kept"
                Expect.equal s.Length 3 "three distinct members"
            }

            test "empty" {
                Expect.isTrue (EqSet.empty<int>.IsEmpty) "empty is empty"
                Expect.equal EqSet.empty<int>.Length 0 "empty has length 0"
            }

            test "default (uninitialised) reads as empty" {
                let d = Unchecked.defaultof<EqSet<int>>
                Expect.equal d.Length 0 "default length is 0"
                Expect.isTrue d.IsEmpty "default is empty"
            }

            test "exists / forall" {
                let s = EqSet.ofSeq [ "x"; "y" ]
                Expect.isTrue (s |> EqSet.forall (fun v -> v.Length = 1)) "all single-char"
                Expect.isTrue (s |> EqSet.exists (fun v -> v = "y")) "exists finds y"
                Expect.isFalse (s |> EqSet.exists (fun v -> v = "z")) "exists misses absent member"
            }

            test "nested EqSet members recurse structurally under equality" {
                let a = EqSet.ofSeq [ EqSet.ofSeq [ 1; 2 ]; EqSet.ofSeq [ 3 ] ]
                let b = EqSet.ofSeq [ EqSet.ofSeq [ 2; 1 ]; EqSet.ofSeq [ 3 ] ]
                Expect.equal a b "nested set members compare by set equality"
            }
        ]
