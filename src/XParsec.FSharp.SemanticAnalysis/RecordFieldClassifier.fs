namespace XParsec.FSharp.SemanticAnalysis

/// The pure, stateless core of record-literal resolution — F#'s `BuildFieldMap` verdict as
/// a function of two field-name sets, generic over the candidate (with key/field-set
/// projections). Public with no `InternalsVisibleTo` (this project has none) so the
/// consumer and its unit tests both call it directly.
module RecordFieldClassifier =

    /// The verdict for a typed field set against a candidate record set.
    type FieldSetClassification<'cand> =
        {
            /// The unique candidate whose declared fields EQUAL the typed set, else `ValueNone`
            /// (zero, or ambiguous).
            Exact: 'cand voption
            /// Distinct exact matches after dedup — splits "no match" (0) from "ambiguous" (>1).
            ExactCount: int
            /// Candidates whose declared fields ⊇ the typed set (deduped by key, first wins) —
            /// the still-live / LSP-completion set. `Exact`, when present, is one of these.
            Partial: 'cand list
        }

    /// Classify `candidates` against the `typed` field names. `Partial` is the subset filter
    /// `typed ⊆ declared`; `Exact` is the unique `declared = typed` (an equal-size superset ⟺
    /// an equal set). Dedups by `keyOf`, first occurrence wins.
    let classifyRecordCandidates
        (keyOf: 'cand -> 'key)
        (fieldsOf: 'cand -> Set<string>)
        (candidates: 'cand list)
        (typed: Set<string>)
        : FieldSetClassification<'cand> when 'key: equality =
        let seen = System.Collections.Generic.HashSet<'key>()

        // Keep each survivor's declared set so the exact test below doesn't recompute `fieldsOf`.
        let partial =
            [
                for cand in candidates do
                    let declared = fieldsOf cand

                    if Set.isSubset typed declared && seen.Add(keyOf cand) then
                        struct (cand, declared)
            ]

        let exact = partial |> List.filter (fun struct (_, declared) -> declared = typed)

        {
            Exact =
                match exact with
                | [ struct (only, _) ] -> ValueSome only
                | _ -> ValueNone
            ExactCount = List.length exact
            Partial = partial |> List.map (fun struct (cand, _) -> cand)
        }
