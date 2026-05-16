namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic

// Side tables hold all in-flight semantic information. CST is never mutated.
// See docs/architecture.md.

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

    /// Callers must treat the returned dictionary as read-only once Freeze starts.
    member _.AsDictionary() : IReadOnlyDictionary<NodeKey, 'V> = dict :> _

[<Sealed>]
type PassContext(provider: IExternalSymbolProvider) =
    member val Provider = provider with get
    /// Written by Desugar.
    member val Desugared = SideTable<DesugaredForm>() with get
    /// Written by NameResolution.
    member val Binding = SideTable<ResolvedBinding>() with get
    /// Written by Unification.
    member val TypeVar = SideTable<TypeVar>() with get
    /// Written by Regions. RegionId itself lives on the TypeVar; this table
    /// carries the classified EscapeState per expression.
    member val Escape = SideTable<EscapeState>() with get
    member val Diagnostics = ResizeArray<Diagnostic>() with get

/// TODO: flesh out (range, code, sub-severities) once passes need to differentiate.
and [<Struct>] Diagnostic =
    { Key: NodeKey
      Message: string
      Severity: Severity }

and Severity =
    | Error
    | Warning
    | Info
