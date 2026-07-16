namespace XParsec.FSharp.SemanticAnalysis

/// The pure combinatorial core of record-literal resolution — F#'s `BuildFieldMap`
/// verdict with none of its state. Generic over the CANDIDATE (with a key projection for
/// dedup and a field-set projection for the verdict), so it carries no domain type
/// (`TypeKey`, `RecordTypeInfo`) and no `ctx` dependency: the verdict is a property of two
/// field-name SETS, nothing more. Returning the surviving candidates directly (not just
/// their keys) is what lets the consumer skip a redundant key→candidate inverse map and
/// the second first-wins dedup it would carry. That statelessness is exactly why it is
/// PUBLIC — the consumer (`recordFieldSetVerdict`) and its unit tests both call it
/// directly, with no `InternalsVisibleTo` (this project has none, deliberately).
module RecordFieldClassifier =

    /// The verdict for a typed field set against a candidate record set.
    type FieldSetClassification<'cand> =
        {
            /// The UNIQUE candidate whose declared field set EQUALS the typed set — the
            /// resolved record when the literal is complete. `ValueNone` when zero or
            /// more than one candidate matches exactly (ambiguous).
            Exact: 'cand voption
            /// How many DISTINCT candidates matched exactly (`declared = typed`), after
            /// dedup — the 0-vs-ambiguous disambiguator the "no match" / "ambiguous"
            /// diagnostics split on. `Exact` is `ValueSome` iff this is exactly `1`.
            ExactCount: int
            /// Every candidate whose declared field set ⊇ the typed set (deduped by key,
            /// first occurrence wins). This is the LSP-completion / cross-unit-facing set —
            /// the records still live given the fields typed so far. `Exact`, when present,
            /// is always one of these.
            Partial: 'cand list
        }

    /// Classify `candidates` against the literal's `typed` field names, reading each
    /// candidate's dedup key via `keyOf` and its full declared field-name set via
    /// `fieldsOf`.
    ///
    /// A record "declares all typed fields" ⟺ `typed ⊆ declared`, so `Partial` is a
    /// straight subset filter — no per-field intersection is needed. `Exact` is the unique
    /// candidate with `declared = typed` (a superset of equal size ⟺ an equal set, so this
    /// subsumes F#'s field-count tie-break). Dedups by key, keeping the first occurrence,
    /// so a candidate reachable through two sources counts once.
    let classifyRecordCandidates
        (keyOf: 'cand -> 'key)
        (fieldsOf: 'cand -> Set<string>)
        (candidates: 'cand list)
        (typed: Set<string>)
        : FieldSetClassification<'cand> when 'key: equality =
        let seen = System.Collections.Generic.HashSet<'key>()

        // Carry each survivor's declared set alongside it so the exact-match test below
        // does not recompute `fieldsOf`.
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
