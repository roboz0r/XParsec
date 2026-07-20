namespace XParsec.FSharp.SemanticAnalysis

open System
open System.Collections
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open XParsec // SmallArrayBuilder

[<AutoOpen>]
module internal RefEquality =
    let inline refEq (x: 'T) (y: 'T) = System.Object.ReferenceEquals(x, y)

/// An `ImmutableArray<'T>` with **structural (value) equality**: two `EqArray`s
/// are equal iff they have equal length and element-wise-equal contents,
/// recursing into `'T`'s own equality.
/// `default`/uninitialised value reads as empty — no `NullReferenceException` from the underlying default array.
[<Struct; IsReadOnly; CustomEquality; NoComparison>]
type EqArray<'T> =
    // `NoComparison` is deliberate. Add `CustomComparison`
    // here — a lexicographic span compare — only if that changes.
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

    member this.AsSpan() : ReadOnlySpan<'T> = this.Underlying.AsSpan()

    member this.GetEnumerator() = this.Underlying.GetEnumerator()

    interface IEnumerable<'T> with
        member this.GetEnumerator() : IEnumerator<'T> =
            (this.Underlying :> IEnumerable<'T>).GetEnumerator()

    interface IEnumerable with
        member this.GetEnumerator() : IEnumerator =
            (this.Underlying :> IEnumerable).GetEnumerator()

    interface IEquatable<EqArray<'T>> with
        member this.Equals(other: EqArray<'T>) =
            let a = this.Underlying
            let b = other.Underlying

            a.Length = b.Length
            && MemoryExtensions.SequenceEqual<'T>(a.AsSpan(), b.AsSpan(), EqualityComparer<'T>.Default)

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

    // Renders element contents (`%A` on an unrecognised struct falls back to this) so a
    // collection value in a diagnostic reads as its items, not the bare type name.
    override this.ToString() =
        let xs = this.Underlying
        let sb = System.Text.StringBuilder("EqArray [")

        for i in 0 .. xs.Length - 1 do
            if i > 0 then
                sb.Append "; " |> ignore

            sb.Append(sprintf "%A" xs.[i]) |> ignore

        sb.Append(']').ToString()

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
    let ofImmutableBuilder (b: ImmutableArrayBuilder<'T>) : EqArray<'T> = EqArray<'T>(b.ToImmutable())

    let toList (xs: EqArray<'T>) : 'T list = List.ofSeq xs.Underlying

    /// Copies into a fresh mutable array. Prefer this over `toList |> List.toArray`,
    /// which builds a throwaway cons-list on the way.
    let toArray (xs: EqArray<'T>) : 'T[] =
        let src = xs.Underlying

        if src.IsEmpty then
            Array.empty
        else
            let out = Array.zeroCreate src.Length
            src.CopyTo(out)
            out

    let init (n: int) (f: int -> 'T) : EqArray<'T> =
        let mutable b = SmallArrayBuilder<'T>()

        for i in 0 .. n - 1 do
            b.Add(f i)

        EqArray<'T>(b.ToImmutable())

    let map (mapping: 'T -> 'U) (xs: EqArray<'T>) : EqArray<'U> =
        let mutable b = SmallArrayBuilder<'U>()

        for x in xs.Underlying do
            b.Add(mapping x)

        EqArray<'U>(b.ToImmutable())

    /// Reference-preserving map. Returns `struct (false, xs)` — the SAME array, no
    /// allocation — when `mapping` leaves EVERY element reference-unchanged (the common
    /// case when a structural walk like `zonk`/`substitute` hits an already-resolved
    /// subtree); otherwise `struct (true, mapped)`, built through the same stack-only
    /// `SmallArrayBuilder` as `map` so the changed path costs no more than a plain `map`
    /// (one result array for ≤4 elements, no heap builder). Reference-typed `'T` only
    /// (`ReferenceEquals`); a struct element would box each item, so it is constrained out.
    let mapPreserve<'T when 'T: not struct> (mapping: 'T -> 'T) (xs: EqArray<'T>) : EqArray<'T> voption =
        let src = xs.Underlying
        let n = src.Length
        let mutable b = SmallArrayBuilder<'T>()
        let mutable changed = false

        for i in 0 .. n - 1 do
            let x = src.[i]
            let y = mapping x

            if not changed && not (refEq x y) then
                // First change: backfill the unchanged prefix, then accumulate the rest.
                changed <- true

                for j in 0 .. i - 1 do
                    b.Add(src.[j])

            if changed then
                b.Add(y)

        if changed then
            ValueSome(EqArray<'T>(b.ToImmutable()))
        else
            ValueNone

    let mapi (mapping: int -> 'T -> 'U) (xs: EqArray<'T>) : EqArray<'U> =
        let mutable b = SmallArrayBuilder<'U>()
        let src = xs.Underlying

        for i in 0 .. src.Length - 1 do
            b.Add(mapping i src.[i])

        EqArray<'U>(b.ToImmutable())

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

    /// Right fold — visits elements last-to-first.
    let foldBack (folder: 'T -> 'State -> 'State) (xs: EqArray<'T>) (state: 'State) : 'State =
        let src = xs.Underlying
        let mutable acc = state

        for i in src.Length - 1 .. -1 .. 0 do
            acc <- folder src.[i] acc

        acc

    let tryLast (xs: EqArray<'T>) : 'T voption =
        match xs.Length with
        | 0 -> ValueNone
        | n -> ValueSome xs.[n - 1]

    let last (xs: EqArray<'T>) : 'T =
        match xs.Length with
        | 0 -> invalidArg "xs" "EqArray.last: the input array was empty"
        | n -> xs.[n - 1]

    let filter (predicate: 'T -> bool) (xs: EqArray<'T>) : EqArray<'T> =
        let mutable b = SmallArrayBuilder<'T>()
        let src = xs.Underlying

        for i in 0 .. src.Length - 1 do
            if predicate src.[i] then
                b.Add src.[i]

        EqArray<'T>(b.ToImmutable())

    /// Uses `EqualityComparer<'T>.Default`, consistent with this type's own
    /// structural equality — so no `'T: equality` constraint on callers.
    let contains (value: 'T) (xs: EqArray<'T>) : bool =
        let src = xs.Underlying
        let cmp = EqualityComparer<'T>.Default
        let mutable i = 0
        let mutable hit = false

        while not hit && i < src.Length do
            if cmp.Equals(src.[i], value) then
                hit <- true

            i <- i + 1

        hit

    /// Returns the first `count` elements (all of them when `count` exceeds the
    /// length; empty when `count <= 0`). Returns the SAME array when nothing is dropped.
    let truncate (count: int) (xs: EqArray<'T>) : EqArray<'T> =
        let src = xs.Underlying
        let n = min (max count 0) src.Length

        if n = src.Length then
            xs
        else
            let mutable b = SmallArrayBuilder<'T>()

            for i in 0 .. n - 1 do
                b.Add src.[i]

            EqArray<'T>(b.ToImmutable())

/// Fixed-arity deconstruction patterns — allocation-free arity checks that bind
/// elements by index (no cons-list, no tail slice). `[<AutoOpen>]` so consumers
/// that `open XParsec.FSharp.SemanticAnalysis` get them unqualified, replacing the
/// `match EqArray.toList xs with [ … ]` idiom.
[<AutoOpen>]
module EqArrayPatterns =
    [<return: Struct>]
    let inline (|EqEmpty|_|) (xs: EqArray<'T>) : unit voption =
        match xs.Length with
        | 0 -> ValueSome()
        | _ -> ValueNone

    [<return: Struct>]
    let inline (|EqOne|_|) (xs: EqArray<'T>) : 'T voption =
        match xs.Length with
        | 1 -> ValueSome xs.[0]
        | _ -> ValueNone

    [<return: Struct>]
    let inline (|EqTwo|_|) (xs: EqArray<'T>) : struct ('T * 'T) voption =
        match xs.Length with
        | 2 -> ValueSome(struct (xs.[0], xs.[1]))
        | _ -> ValueNone

    [<return: Struct>]
    let inline (|EqThree|_|) (xs: EqArray<'T>) : struct ('T * 'T * 'T) voption =
        match xs.Length with
        | 3 -> ValueSome(struct (xs.[0], xs.[1], xs.[2]))
        | _ -> ValueNone
