namespace XParsec.FSharp.SemanticAnalysis

/// The pure core of record-literal resolution: which record types a `{ a = …; b = … }` literal
/// could mean, as a function of two field-name sets alone.
module RecordFieldClassifier =

    /// The candidates whose declared field set EQUALS the typed set, deduped by key.
    [<RequireQualifiedAccess>]
    type ExactMatch<'cand> =
        | NoMatch
        | Unique of 'cand
        | Ambiguous of count: int

    type FieldSetClassification<'cand> =
        {
            Exact: ExactMatch<'cand>
            /// Candidates whose declared fields ⊇ the typed set (deduped by key, first wins).
            /// A `Unique` exact match is one of these.
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
                | [] -> ExactMatch.NoMatch
                | [ struct (only, _) ] -> ExactMatch.Unique only
                | many -> ExactMatch.Ambiguous(List.length many)
            Partial = partial |> List.map (fun struct (cand, _) -> cand)
        }
