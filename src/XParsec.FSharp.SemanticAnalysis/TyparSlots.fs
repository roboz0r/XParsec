namespace XParsec.FSharp.SemanticAnalysis

/// Indexes `TyparList.Order`: one position per parameter of either kind, in the order the
/// declaration writes them. A written `<'a, 'u>` argument list, a claim's arity and a
/// shape's `TyparArity` count these.
[<Measure>]
type sigSlot

/// Indexes `TyparList.Types`. A typar leaf carries one, a metadata name's `` `N `` spells
/// this many.
[<Measure>]
type typeSlot

/// Indexes `TyparList.Measures`. A measure atom carries one.
[<Measure>]
type measureSlot

[<RequireQualifiedAccess>]
module TyparIndex =

    let sigSlot (i: int) : int<sigSlot> = LanguagePrimitives.Int32WithMeasure i

    let typeSlot (i: int) : int<typeSlot> = LanguagePrimitives.Int32WithMeasure i

    let measureSlot (i: int) : int<measureSlot> = LanguagePrimitives.Int32WithMeasure i

/// The count a `TypeKey` spells, rendered as the `` `N `` of `SymbolKeyOps.typeSegmentName`.
/// `Compiled 1` and `Written 1` are distinct keys: `float` measured is `Written 1`, a
/// referenced `` List`1 `` is `Compiled 1`.
[<RequireQualifiedAccess; Struct>]
type KeyArity =
    /// A nominal type's type-kinded parameter count, the `` `N `` of its metadata name. A
    /// key minted from a metadata row equals one minted from source.
    | Compiled of typeSlots: int<typeSlot>
    /// An abbreviation's written parameter count: an abbreviation has no metadata name, and
    /// F# keys it at every parameter.
    | Written of sigSlots: int<sigSlot>

    /// The count as `` `N `` renders it.
    member this.Count: int =
        match this with
        | Compiled n -> int n
        | Written n -> int n
