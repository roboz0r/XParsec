namespace XParsec.FSharp.SemanticAnalysis

/// The pure core of record-literal resolution: which record types a `{ a = …; b = … }` literal
/// could mean, as a function of two field-name sets alone.
module RecordFieldClassifier =

    type FieldSetClassification<'cand> =
        {
            /// The unique candidate whose declared fields EQUAL the typed set, else `ValueNone`
            /// (zero, or ambiguous).
            Exact: 'cand voption
            /// Distinct exact matches after dedup — splits "no match" (0) from "ambiguous" (>1).
            ExactCount: int
            /// Candidates whose declared fields ⊇ the typed set (deduped by key, first wins).
            /// `Exact`, when present, is one of these.
            Partial: 'cand list
        }

    let classifyRecordCandidates
        (keyOf: 'cand -> 'key)
        (fieldsOf: 'cand -> Set<string>)
        (candidates: 'cand list)
        (typed: Set<string>)
        : FieldSetClassification<'cand> when 'key: equality =
        let seen = System.Collections.Generic.HashSet<'key>()

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
