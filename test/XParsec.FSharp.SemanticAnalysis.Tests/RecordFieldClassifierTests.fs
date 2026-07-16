module XParsec.FSharp.SemanticAnalysis.Tests.RecordFieldClassifierTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.RecordFieldClassifier

// `classifyRecordCandidates` is the pure, stateless, PUBLIC core of record-literal
// resolution — no domain types, no `ctx`. It is tested here DIRECTLY with plain
// `string` keys and `Set<string>` field sets: nothing but the two-field-set verdict,
// no analyse harness. This is the open-box replacement for a rejected IVT white-box
// test — publicness is the whole point of the R4b-1 extraction.

[<Tests>]
let tests =
    testList
        "RecordFieldClassifier"
        [
            test "unique exact: single candidate equal to the typed set" {
                let c = classifyRecordCandidates [ "R", set [ "X"; "Y" ] ] (set [ "X"; "Y" ])
                Expect.equal c.ExactKey (ValueSome "R") "the sole equal-set candidate is the exact match"
                Expect.equal c.PartialKeys [ "R" ] "it is also the sole partial match"
            }

            test "ambiguous: two candidates with the same declared set → no exact match" {
                let c =
                    classifyRecordCandidates [ "R", set [ "X"; "Y" ]; "S", set [ "X"; "Y" ] ] (set [ "X"; "Y" ])

                Expect.equal c.ExactKey ValueNone "two equal-set candidates ⇒ ambiguous, no exact match"
                Expect.equal c.PartialKeys [ "R"; "S" ] "both survive as partial matches"
            }

            test "superset: a candidate declaring MORE than typed is partial, not exact" {
                let c = classifyRecordCandidates [ "R", set [ "X"; "Y"; "Z" ] ] (set [ "X"; "Y" ])
                Expect.equal c.PartialKeys [ "R" ] "the superset declares all typed fields ⇒ partial"
                Expect.equal c.ExactKey ValueNone "but its set is not EQUAL to typed ⇒ not exact"
            }

            test "subset filter: a candidate missing a typed field is excluded" {
                // The crux of correctness: `R` declares only `X`, so it does NOT declare all
                // of `{X;Y}` and must be pruned — proving the subset (typed ⊆ declared) test.
                let c = classifyRecordCandidates [ "R", set [ "X" ] ] (set [ "X"; "Y" ])
                Expect.equal c.PartialKeys [] "a candidate missing a typed field is not a partial match"
                Expect.equal c.ExactKey ValueNone "and certainly not an exact match"
            }

            test "empty candidates → empty verdict" {
                let c = classifyRecordCandidates [] (set [ "X"; "Y" ])
                Expect.equal c.PartialKeys [] "no candidates ⇒ no partial matches"
                Expect.equal c.ExactKey ValueNone "no candidates ⇒ no exact match"
            }

            test "empty typed set → every candidate is a (trivial) partial match" {
                // The empty set is a subset of every declared set, so every candidate survives;
                // an exact match still requires a UNIQUE declared set equal to empty.
                let c = classifyRecordCandidates [ "R", set [ "X" ]; "S", set [ "Y" ] ] Set.empty
                Expect.equal c.PartialKeys [ "R"; "S" ] "empty typed ⊆ every declared set"
                Expect.equal c.ExactKey ValueNone "no candidate has an empty declared set"
            }

            test "dedup: the same key appears once, first occurrence wins" {
                let c =
                    classifyRecordCandidates [ "R", set [ "X"; "Y" ]; "R", set [ "X"; "Y" ] ] (set [ "X"; "Y" ])

                Expect.equal c.PartialKeys [ "R" ] "a duplicate key is collapsed to a single partial match"
                Expect.equal c.ExactKey (ValueSome "R") "and the dedup leaves a unique exact match"
            }
        ]
