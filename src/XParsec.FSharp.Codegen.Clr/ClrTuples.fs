namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.SemanticAnalysis

/// The `System.ValueTuple` family: which member a tuple of a given arity is, as a `TypeKey`
/// to classify it under and as a metadata name to mint it by.
module ClrTuples =

    [<Literal>]
    let Namespace = "System"

    [<Literal>]
    let Name = "ValueTuple"

    /// `ValueTuple`8` is not eight elements: slots 0–6 are elements, the 8th is a `TRest` nest.
    [<Literal>]
    let MaxArity = 8

    [<Literal>]
    let MaxDirect = 7

    let fitsOneMember (arity: int) : bool = arity <= MaxDirect

    /// `unit` and `(x)` are not tuple values; `ValueTuple`1` arises only as a `TRest`.
    let isTupleArity (arity: int) : bool = arity >= 2

    /// The member a tuple of `arity` instantiates: `ValueTuple`3` for 3, `ValueTuple`8` for 9.
    let memberArity (arity: int) : int = min arity MaxArity

    /// The OUTERMOST member's key, so a 9-tuple classifies as `ValueTuple`8` and the nesting
    /// below it stays in the encoder.
    let typeKey (arity: int) : TypeKey =
        SymbolKeyOps.typeKeyOfArity Namespace Name (TyparIndex.typeSlot (memberArity arity))

    /// A family member's bare metadata name: 8 → `ValueTuple`8`.
    let memberName (memberArity: int) : string = SymbolKeyOps.arityName Name memberArity
