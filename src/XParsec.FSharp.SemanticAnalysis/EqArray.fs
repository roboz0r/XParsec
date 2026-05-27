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

    /// Copies; the caller may continue to mutate `xs` without affecting the
    /// returned `EqArray`. Use the `Builder` overloads to hand off without copy.
    let ofArray (xs: 'T[]) : EqArray<'T> =
        EqArray<'T>(ImmutableArray.Create<'T>(xs))

    /// Copies; pair with `ResizeArray` accumulators used during a pass build-up.
    let ofResizeArray (xs: ResizeArray<'T>) : EqArray<'T> =
        EqArray<'T>(ImmutableArray.CreateRange xs)

    /// Hands the builder's buffer off without copying when its `Count = Capacity`;
    /// otherwise copies. Matches `ImmutableArray<_>.Builder.MoveToImmutable` /
    /// `ToImmutable` semantics across the portable `ImmutableArrayBuilder` alias.
    /// Use when a pass produces an `ImmutableArray.CreateBuilder<_>()` with the
    /// final size already known (see `CstWalk.implFileElems`).
    let ofImmutableBuilder (b: ImmutableArrayBuilder<'T>) : EqArray<'T> = EqArray<'T>(b.ToImmutable())

    let toList (xs: EqArray<'T>) : 'T list = List.ofSeq xs.Underlying

    /// Stamp `n` elements through `f`, building through `SmallArrayBuilder`.
    let init (n: int) (f: int -> 'T) : EqArray<'T> =
        let mutable b = SmallArrayBuilder<'T>()

        for i in 0 .. n - 1 do
            b.Add(f i)

        EqArray<'T>(b.ToImmutable())

    /// Map into a fresh `EqArray`, building through `SmallArrayBuilder` so small
    /// results stay off the heap-builder path.
    let map (mapping: 'T -> 'U) (xs: EqArray<'T>) : EqArray<'U> =
        let mutable b = SmallArrayBuilder<'U>()

        for x in xs.Underlying do
            b.Add(mapping x)

        EqArray<'U>(b.ToImmutable())

    /// Like `map`, but the mapping receives the element index too — direct
    /// replacement for `List.mapi`.
    let mapi (mapping: int -> 'T -> 'U) (xs: EqArray<'T>) : EqArray<'U> =
        let mutable b = SmallArrayBuilder<'U>()
        let src = xs.Underlying

        for i in 0 .. src.Length - 1 do
            b.Add(mapping i src.[i])

        EqArray<'U>(b.ToImmutable())

    /// `List.iter` on an `EqArray`. A bare `for x in xs do …` works too — use
    /// this when the body is a pre-bound function and the call site reads
    /// cleaner as `EqArray.iter f xs`.
    let iter (action: 'T -> unit) (xs: EqArray<'T>) : unit =
        let src = xs.Underlying

        for i in 0 .. src.Length - 1 do
            action src.[i]

    let iteri (action: int -> 'T -> unit) (xs: EqArray<'T>) : unit =
        let src = xs.Underlying

        for i in 0 .. src.Length - 1 do
            action i src.[i]

    let exists (predicate: 'T -> bool) (xs: EqArray<'T>) : bool =
        let src = xs.Underlying
        let mutable i = 0
        let mutable hit = false

        while not hit && i < src.Length do
            if predicate src.[i] then
                hit <- true

            i <- i + 1

        hit

    let forall (predicate: 'T -> bool) (xs: EqArray<'T>) : bool =
        let src = xs.Underlying
        let mutable i = 0
        let mutable ok = true

        while ok && i < src.Length do
            if not (predicate src.[i]) then
                ok <- false

            i <- i + 1

        ok

    /// Returns `false` immediately on a length mismatch (does NOT throw), unlike
    /// `List.forall2`. Callers that care about the mismatch should check
    /// `.Length` themselves first — every TAST consumer that uses this pattern
    /// already does so.
    let forall2 (predicate: 'T -> 'U -> bool) (xs: EqArray<'T>) (ys: EqArray<'U>) : bool =
        let a = xs.Underlying
        let b = ys.Underlying

        if a.Length <> b.Length then
            false
        else
            let mutable i = 0
            let mutable ok = true

            while ok && i < a.Length do
                if not (predicate a.[i] b.[i]) then
                    ok <- false

                i <- i + 1

            ok

    let tryFind (predicate: 'T -> bool) (xs: EqArray<'T>) : 'T voption =
        let src = xs.Underlying
        let mutable i = 0
        let mutable found = ValueNone

        while found.IsNone && i < src.Length do
            if predicate src.[i] then
                found <- ValueSome src.[i]

            i <- i + 1

        found

    let fold (folder: 'State -> 'T -> 'State) (state: 'State) (xs: EqArray<'T>) : 'State =
        let src = xs.Underlying
        let mutable acc = state

        for i in 0 .. src.Length - 1 do
            acc <- folder acc src.[i]

        acc

    /// Right fold — visits elements last-to-first. Direct replacement for
    /// `List.foldBack`, used in `PrintfSpec.printerType` to curry argument
    /// types onto a tail.
    let foldBack (folder: 'T -> 'State -> 'State) (xs: EqArray<'T>) (state: 'State) : 'State =
        let src = xs.Underlying
        let mutable acc = state

        for i in src.Length - 1 .. -1 .. 0 do
            acc <- folder src.[i] acc

        acc
