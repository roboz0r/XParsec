namespace XParsec.FSharp.SemanticAnalysis

/// The pure combinatorial core of record-literal resolution — F#'s `BuildFieldMap`
/// verdict with none of its state. Generic over the candidate KEY, so it carries no
/// domain type (`TypeKey`, `RecordTypeInfo`) and no `ctx` dependency: the verdict is a
/// property of two field-name SETS, nothing more. That statelessness is exactly why it
/// is PUBLIC — the consumer (`recordFieldSetVerdict`) and its unit tests both call it
/// directly, with no `InternalsVisibleTo` (this project has none, deliberately).
module RecordFieldClassifier =

    /// The verdict for a typed field set against a candidate record set.
    type FieldSetClassification<'key> =
        {
            /// The UNIQUE candidate whose declared field set EQUALS the typed set — the
            /// resolved record when the literal is complete. `ValueNone` when zero or
            /// more than one candidate matches exactly (ambiguous).
            ExactKey: 'key voption
            /// Every candidate whose declared field set ⊇ the typed set (deduped by key,
            /// first occurrence wins). This is the LSP-completion / R4b-2-facing set —
            /// the records still live given the fields typed so far. `ExactKey`, when
            /// present, is always one of these.
            PartialKeys: 'key list
        }

    /// Classify `candidates` (each a key paired with THAT record's full declared
    /// field-name set) against the literal's `typed` field names.
    ///
    /// A record "declares all typed fields" ⟺ `typed ⊆ declared`, so `PartialKeys` is a
    /// straight subset filter — no per-field intersection is needed. `ExactKey` is the
    /// unique candidate with `declared = typed` (a superset of equal size ⟺ an equal
    /// set, so this subsumes F#'s field-count tie-break). Dedups by key, keeping the
    /// first occurrence, so a candidate reachable through two sources counts once.
    let classifyRecordCandidates
        (candidates: ('key * Set<string>) list)
        (typed: Set<string>)
        : FieldSetClassification<'key> when 'key: equality =
        let seen = System.Collections.Generic.HashSet<'key>()

        let partial =
            [
                for (key, declared) in candidates do
                    if Set.isSubset typed declared && seen.Add key then
                        key, declared
            ]

        let exact =
            match partial |> List.filter (fun (_, declared) -> declared = typed) with
            | [ (only, _) ] -> ValueSome only
            | _ -> ValueNone

        {
            ExactKey = exact
            PartialKeys = partial |> List.map fst
        }
