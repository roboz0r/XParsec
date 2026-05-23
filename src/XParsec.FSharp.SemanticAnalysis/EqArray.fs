namespace XParsec.FSharp.SemanticAnalysis

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open XParsec // SmallArrayBuilder

/// An `ImmutableArray<'T>` with **structural (value) equality**: two `EqArray`s
/// are equal iff they have equal length and element-wise-equal contents,
/// recursing into `'T`'s own equality. This is the reason the type exists. BCL
/// `ImmutableArray<'T>` compares by backing-array *reference*, so dropping a raw
/// `ImmutableArray` into an equality-bearing type (a TAST node, `SemType`)
/// silently downgrades that type's structural `=` to reference equality.
/// `EqArray` restores structural `=` while keeping the single-allocation,
/// cache-contiguous backing array. A `default`/uninitialised value reads as
/// empty — no `NullReferenceException` from the underlying default array.
///
/// `NoComparison` is deliberate: nothing orders `SemType`/TAST nodes (the type
/// graph is keyed by reference identity, not sorted). Add `CustomComparison`
/// here — a lexicographic span compare — only if that changes.
[<Struct; IsReadOnly; CustomEquality; NoComparison>]
type EqArray<'T> =
    val private items: ImmutableArray<'T>

    new(items: ImmutableArray<'T>) = { items = items }

    /// The backing array, normalised so a `default(EqArray)` reads as empty.
    member this.Underlying: ImmutableArray<'T> =
        if this.items.IsDefault then
            ImmutableArray<'T>.Empty
        else
            this.items

    member this.Length: int = if this.items.IsDefault then 0 else this.items.Length

    member this.IsEmpty: bool = this.items.IsDefaultOrEmpty

    member this.Item
        with get (index: int): 'T = this.Underlying.[index]

    /// Contiguous read-only view, for hot traversals.
    member this.AsSpan() : ReadOnlySpan<'T> = this.Underlying.AsSpan()

    /// Allocation-free struct enumerator; enables `for x in eqArray do …`.
    member this.GetEnumerator() = this.Underlying.GetEnumerator()

    interface IEquatable<EqArray<'T>> with
        member this.Equals(other: EqArray<'T>) =
            let a = this.Underlying
            let b = other.Underlying
            // `EqualityComparer<'T>.Default` dispatches to `'T`'s own
            // `IEquatable<'T>`, so nested F# values — records, unions, and
            // nested `EqArray`s — recurse structurally.
            a.Length = b.Length
            && MemoryExtensions.SequenceEqual<'T>(
                a.AsSpan(),
                b.AsSpan(),
                EqualityComparer<'T>.Default :> IEqualityComparer<'T>
            )

    override this.Equals(o: obj) =
        match o with
        | :? EqArray<'T> as other -> (this :> IEquatable<EqArray<'T>>).Equals other
        | _ -> false

    override this.GetHashCode() =
        // Order-sensitive fold, consistent with the element-wise equality above.
        let xs = this.Underlying
        let cmp = EqualityComparer<'T>.Default
        let mutable h = 17

        for i in 0 .. xs.Length - 1 do
            h <- (h * 397) ^^^ cmp.GetHashCode xs.[i]

        h

[<RequireQualifiedAccess>]
module EqArray =
    [<GeneralizableValue>]
    let empty<'T> : EqArray<'T> = EqArray<'T>(ImmutableArray<'T>.Empty)

    let inline ofImmutable (xs: ImmutableArray<'T>) : EqArray<'T> = EqArray<'T>(xs)

    let singleton (x: 'T) : EqArray<'T> = EqArray<'T>(ImmutableArray.Create(x))

    let ofSeq (xs: 'T seq) : EqArray<'T> =
        EqArray<'T>(ImmutableArray.CreateRange xs)

    let ofList (xs: 'T list) : EqArray<'T> = ofSeq xs

    let toList (xs: EqArray<'T>) : 'T list = List.ofSeq xs.Underlying

    /// Map into a fresh `EqArray`, building through `SmallArrayBuilder` so small
    /// results stay off the heap-builder path.
    let map (mapping: 'T -> 'U) (xs: EqArray<'T>) : EqArray<'U> =
        let mutable b = SmallArrayBuilder<'U>()

        for x in xs.Underlying do
            b.Add(mapping x)

        EqArray<'U>(b.ToImmutable())
