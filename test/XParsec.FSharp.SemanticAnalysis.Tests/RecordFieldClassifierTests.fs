module XParsec.FSharp.SemanticAnalysis.Tests.RecordFieldClassifierTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.RecordFieldClassifier

/// Classify tuple candidates, projecting `fst` as the dedup key and `snd` as the field set.
let private classify candidates typed =
    classifyRecordCandidates fst snd candidates typed

[<Tests>]
let tests =
    testList
        "RecordFieldClassifier"
        [
            test "unique exact: single candidate equal to the typed set" {
                let c = classify [ "R", set [ "X"; "Y" ] ] (set [ "X"; "Y" ])

                Expect.equal
                    c.Exact
                    (ExactMatch.Unique("R", set [ "X"; "Y" ]))
                    "the sole equal-set candidate is the exact match"

                Expect.equal (c.Partial |> List.map fst) [ "R" ] "it is also the sole partial match"
            }

            test "ambiguous: two candidates with the same declared set → no exact match" {
                let c = classify [ "R", set [ "X"; "Y" ]; "S", set [ "X"; "Y" ] ] (set [ "X"; "Y" ])
                Expect.equal c.Exact (ExactMatch.Ambiguous 2) "two equal-set candidates ⇒ ambiguous with count 2"
                Expect.equal (c.Partial |> List.map fst) [ "R"; "S" ] "both survive as partial matches"
            }

            test "superset: a candidate declaring MORE than typed is partial, not exact" {
                let c = classify [ "R", set [ "X"; "Y"; "Z" ] ] (set [ "X"; "Y" ])
                Expect.equal (c.Partial |> List.map fst) [ "R" ] "the superset declares all typed fields ⇒ partial"
                Expect.equal c.Exact ExactMatch.NoMatch "but its set is not EQUAL to typed ⇒ not exact"
            }

            test "subset filter: a candidate missing a typed field is excluded" {
                // `R` declares only `X`, so typed ⊆ declared fails and it is pruned.
                let c = classify [ "R", set [ "X" ] ] (set [ "X"; "Y" ])
                Expect.equal c.Partial [] "a candidate missing a typed field is not a partial match"
                Expect.equal c.Exact ExactMatch.NoMatch "and certainly not an exact match"
            }

            test "empty candidates → empty verdict" {
                let c = classify [] (set [ "X"; "Y" ])
                Expect.equal c.Partial [] "no candidates ⇒ no partial matches"
                Expect.equal c.Exact ExactMatch.NoMatch "no candidates ⇒ no exact match"
            }

            test "empty typed set → every candidate is a (trivial) partial match" {
                let c = classify [ "R", set [ "X" ]; "S", set [ "Y" ] ] Set.empty
                Expect.equal (c.Partial |> List.map fst) [ "R"; "S" ] "empty typed ⊆ every declared set"
                Expect.equal c.Exact ExactMatch.NoMatch "no candidate has an empty declared set"
            }

            test "dedup: the same key appears once, first occurrence wins" {
                let c = classify [ "R", set [ "X"; "Y" ]; "R", set [ "X"; "Y" ] ] (set [ "X"; "Y" ])

                Expect.equal
                    (c.Partial |> List.map fst)
                    [ "R" ]
                    "a duplicate key is collapsed to a single partial match"

                Expect.equal
                    c.Exact
                    (ExactMatch.Unique("R", set [ "X"; "Y" ]))
                    "the duplicate collapses to one unique exact match, not an ambiguity"
            }
        ]
