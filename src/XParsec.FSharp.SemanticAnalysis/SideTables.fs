namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic

// Side tables hold all in-flight semantic information. The CST is never
// mutated; every pass reads earlier tables and writes its own. Freeze.fs
// consumes the CST + all tables and produces the immutable TAST.
//
// See docs/architecture.md "CST in, TAST out, side tables in between" for
// the rationale.

/// A side table keyed by NodeKey. Just a thin Dictionary wrapper — the goal
/// is to centralise the construction patterns (capacity hints, equality
/// comparer) and give each kind of semantic info a named type.
[<Sealed>]
type SideTable<'V>() =
    let dict = Dictionary<NodeKey, 'V>(HashIdentity.Structural)

    member _.Count = dict.Count

    member _.TryGetValue(key: NodeKey) =
        match dict.TryGetValue(key) with
        | true, v -> ValueSome v
        | false, _ -> ValueNone

    member _.Set(key: NodeKey, value: 'V) = dict[key] <- value

    member _.ContainsKey(key: NodeKey) = dict.ContainsKey key

    /// Snapshot for Freeze. Returns the underlying Dictionary by reference;
    /// callers must treat it as read-only after Freeze starts.
    member _.AsDictionary() : IReadOnlyDictionary<NodeKey, 'V> = dict :> _

/// The bag of tables that flows through the pipeline. Each pass owns one
/// table for writing; later passes may read any number. Diagnostics is a
/// shared accumulator.
[<Sealed>]
type PassContext() =
    /// Written by Desugar.
    member val Desugared = SideTable<DesugaredForm>() with get
    /// Written by NameResolution.
    member val Binding = SideTable<ResolvedBinding>() with get
    /// Written by Unification.
    member val TypeVar = SideTable<TypeVar>() with get
    /// Written by Regions (the EscapeState — RegionId already lives on
    /// the TypeVar itself).
    member val Escape = SideTable<EscapeState>() with get
    /// Written by any pass. Cleared by the caller between compilations.
    member val Diagnostics = ResizeArray<Diagnostic>() with get

/// Compiler diagnostic emitted by any pass. The structure is intentionally
/// minimal — flesh out (range, code, severity) as the passes start needing
/// to differentiate.
and [<Struct>] Diagnostic =
    { Key: NodeKey
      Message: string
      Severity: Severity }

and Severity =
    | Error
    | Warning
    | Info
