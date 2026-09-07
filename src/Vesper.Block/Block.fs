namespace Vesper

// The runtime type tests in `Equals`, `CompareTo` and `IStructuralComparable.CompareTo` see
// an erased measure. Boxing two blocks with different tags and comparing them as `obj`
// therefore compares their contents; every other route to those members is statically typed
// and its operands agree on `'M`.
#nowarn "1240"

open System
open System.Collections
open System.Collections.Generic
open System.Runtime.CompilerServices

/// Forward-only cursor over a `BlockM`, so `for x in block do` allocates nothing.
[<Struct; NoEquality; NoComparison>]
type BlockEnumerator<'T> =
    val private xs: 'T[]
    val mutable private index: int

    internal new(xs: 'T[]) = { xs = xs; index = -1 }

    member this.Current: 'T = this.xs.[this.index]

    member this.MoveNext() : bool =
        this.index <- this.index + 1
        this.index < this.xs.Length

/// An immutable array with structural (value) equality and a measure-tagged index: equal iff
/// equal length and element-wise-equal contents, recursing into `'T`'s own equality. `'M` tags
/// both the index and the length, so a block numbered by one index convention rejects
/// another's index and a bare `int`. An uninitialised value is empty.
///
/// Comparable exactly when `'T` is, and then lexicographic under F# `compare`: element-wise
/// over the shorter length, then by length, as a `list` orders.
[<Struct; IsReadOnly; CustomEquality; CustomComparison>]
type BlockM<[<ComparisonConditionalOn>] 'T, [<Measure>] 'M> =
    val internal _xs: 'T[]

    internal new(xs: 'T[]) = { _xs = xs }

    /// The backing array, with an uninitialised value read as the shared empty array.
    member internal this.Xs: 'T[] = if isNull this._xs then Array.empty else this._xs

    member this.Length: int<'M> =
        LanguagePrimitives.Int32WithMeasure<'M>(if isNull this._xs then 0 else this._xs.Length)

    member this.IsEmpty: bool = isNull this._xs || this._xs.Length = 0

    member this.Item
        with get (index: int<'M>): 'T = this.Xs.[int index]

    member this.AsSpan() : ReadOnlySpan<'T> = ReadOnlySpan<'T>(this.Xs)

    member this.GetEnumerator() = BlockEnumerator<'T>(this.Xs)

    member this.Equals(other: BlockM<'T, 'M>) =
        let a = this.Xs
        let b = other.Xs

        a.Length = b.Length
        && MemoryExtensions.SequenceEqual<'T>(ReadOnlySpan<'T>(a), ReadOnlySpan<'T>(b), EqualityComparer<'T>.Default)

    interface IEnumerable<'T> with
        member this.GetEnumerator() : IEnumerator<'T> =
            (this.Xs :> IEnumerable<'T>).GetEnumerator()

    interface IEnumerable with
        member this.GetEnumerator() : IEnumerator =
            (this.Xs :> IEnumerable).GetEnumerator()

    interface IEquatable<BlockM<'T, 'M>> with
        member this.Equals(other: BlockM<'T, 'M>) = this.Equals(other)

    override this.Equals(o: obj) =
        match o with
        | :? BlockM<'T, 'M> as other -> this.Equals other
        | _ -> false

    member private this.CompareWith(other: BlockM<'T, 'M>, comparer: IComparer) : int =
        let a = this.Xs
        let b = other.Xs
        let n = min a.Length b.Length
        let mutable i = 0
        let mutable c = 0

        while c = 0 && i < n do
            c <- comparer.Compare(box a.[i], box b.[i])
            i <- i + 1

        if c <> 0 then c else compare a.Length b.Length

    interface IComparable with
        member this.CompareTo(o: obj) =
            match o with
            | :? BlockM<'T, 'M> as other -> this.CompareWith(other, LanguagePrimitives.GenericComparer)
            | _ -> invalidArg "o" "Block compared with a value of another type."

    interface IStructuralComparable with
        member this.CompareTo(o: obj, comparer: IComparer) =
            match o with
            | :? BlockM<'T, 'M> as other -> this.CompareWith(other, comparer)
            | _ -> invalidArg "o" "Block compared with a value of another type."

    override this.GetHashCode() =
        // Order-sensitive fold, consistent with the element-wise equality above.
        let xs = this.Xs
        let cmp = EqualityComparer<'T>.Default
        let mutable h = 17

        for i in 0 .. xs.Length - 1 do
            h <- (h * 397) ^^^ cmp.GetHashCode xs.[i]

        h

    // `%A` on a struct falls back to this, so a value in a diagnostic prints as `Block [a; b]`.
    override this.ToString() =
        let xs = this.Xs
        let sb = System.Text.StringBuilder("Block [")

        for i in 0 .. xs.Length - 1 do
            if i > 0 then
                sb.Append "; " |> ignore

            sb.Append(sprintf "%A" xs.[i]) |> ignore

        sb.Append(']').ToString()

/// A `BlockM` indexed by a plain `int`: `1` is F#'s dimensionless measure, so `int<1>` and
/// `int` are the same type.
type Block<'T> = BlockM<'T, 1>

[<RequireQualifiedAccess>]
module Block =
    open System.Runtime.InteropServices
    open System.Collections.Immutable

    [<GeneralizableValue>]
    let empty<'T, [<Measure>] 'M> : BlockM<'T, 'M> = BlockM<'T, 'M>(Array.empty)

    /// Takes ownership of `xs`. The caller must not retain a reference it will mutate.
    let unsafeOfArray (xs: 'T[]) : BlockM<'T, 'M> = BlockM<'T, 'M>(xs)

    /// Copies, so the caller may keep mutating `xs`.
    let ofArray (xs: 'T[]) : BlockM<'T, 'M> =
        match xs.Length with
        | 0 -> BlockM<'T, 'M>(Array.empty)
        | _ -> BlockM<'T, 'M>(Array.copy xs)

    let singleton (x: 'T) : BlockM<'T, 'M> = BlockM<'T, 'M>([| x |])

    let ofSeq (xs: 'T seq) : BlockM<'T, 'M> = BlockM<'T, 'M>(Array.ofSeq xs)

    let ofList (xs: 'T list) : BlockM<'T, 'M> = BlockM<'T, 'M>(List.toArray xs)

    /// Copies, so the caller may keep mutating `xs`.
    let ofResizeArray (xs: ResizeArray<'T>) : BlockM<'T, 'M> = BlockM<'T, 'M>(xs.ToArray())

    let toList (xs: BlockM<'T, 'M>) : 'T list = List.ofArray xs.Xs

    /// Copies into a fresh mutable array.
    let toArray (xs: BlockM<'T, 'M>) : 'T[] =
        match xs.Xs.Length with
        | 0 -> Array.empty
        | _ -> Array.copy xs.Xs

    let init (count: int<'M>) (f: int<'M> -> 'T) : BlockM<'T, 'M> =
        BlockM<'T, 'M>(Array.init (int count) (fun i -> f (LanguagePrimitives.Int32WithMeasure<'M> i)))

    let map (mapping: 'T -> 'U) (xs: BlockM<'T, 'M>) : BlockM<'U, 'M> = BlockM<'U, 'M>(Array.map mapping xs.Xs)

    let mapi (mapping: int<'M> -> 'T -> 'U) (xs: BlockM<'T, 'M>) : BlockM<'U, 'M> =
        BlockM<'U, 'M>(Array.mapi (fun i x -> mapping (LanguagePrimitives.Int32WithMeasure<'M> i) x) xs.Xs)

    /// Reference-preserving map: `ValueNone`, allocating nothing, when `mapping` returns a
    /// reference-equal result for EVERY element, the common case when a structural walk reaches
    /// an already-resolved subtree. Reference types only; a struct element would box per item.
    let mapPreserve<'T, [<Measure>] 'M when 'T: not struct>
        (mapping: 'T -> 'T)
        (xs: BlockM<'T, 'M>)
        : BlockM<'T, 'M> voption =
        let src = xs.Xs
        let mutable out = Unchecked.defaultof<'T[]>

        for i in 0 .. src.Length - 1 do
            let y = mapping src.[i]

            if isNull out && not (Object.ReferenceEquals(src.[i], y)) then
                // Elements before `i` mapped reference-equal, so copying carries them over
                // unchanged; the rest are overwritten as the loop reaches them.
                out <- Array.copy src

            if not (isNull out) then
                out.[i] <- y

        if isNull out then
            ValueNone
        else
            ValueSome(BlockM<'T, 'M>(out))

    let iter (action: 'T -> unit) (xs: BlockM<'T, 'M>) : unit = Array.iter action xs.Xs

    let iteri (action: int<'M> -> 'T -> unit) (xs: BlockM<'T, 'M>) : unit =
        Array.iteri (fun i x -> action (LanguagePrimitives.Int32WithMeasure<'M> i) x) xs.Xs

    let exists (predicate: 'T -> bool) (xs: BlockM<'T, 'M>) : bool = Array.exists predicate xs.Xs

    let forall (predicate: 'T -> bool) (xs: BlockM<'T, 'M>) : bool = Array.forall predicate xs.Xs

    /// Returns `false` on a length mismatch; it does NOT throw, so a caller that must
    /// distinguish "mismatched" from "unequal" has to compare `.Length` itself.
    let forall2 (predicate: 'T -> 'U -> bool) (xs: BlockM<'T, 'M>) (ys: BlockM<'U, 'N>) : bool =
        let a = xs.Xs
        let b = ys.Xs

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

    /// Raises `ArgumentException` on a length mismatch.
    let map2 (mapping: 'T -> 'U -> 'V) (xs: BlockM<'T, 'M>) (ys: BlockM<'U, 'N>) : BlockM<'V, 'M> =
        let a = xs.Xs
        let b = ys.Xs

        if a.Length <> b.Length then
            invalidArg "ys" $"length %d{b.Length} differs from the first block's %d{a.Length}"

        BlockM<'V, 'M>(Array.init a.Length (fun i -> mapping a.[i] b.[i]))

    let tryFind (predicate: 'T -> bool) (xs: BlockM<'T, 'M>) : 'T voption =
        let src = xs.Xs
        let mutable i = 0
        let mutable found = ValueNone

        while found.IsNone && i < src.Length do
            if predicate src.[i] then
                found <- ValueSome src.[i]

            i <- i + 1

        found

    let tryFindIndex (predicate: 'T -> bool) (xs: BlockM<'T, 'M>) : int<'M> voption =
        let src = xs.Xs
        let mutable i = 0
        let mutable found = ValueNone

        while found.IsNone && i < src.Length do
            if predicate src.[i] then
                found <- ValueSome(LanguagePrimitives.Int32WithMeasure<'M> i)

            i <- i + 1

        found

    let fold (folder: 'State -> 'T -> 'State) (state: 'State) (xs: BlockM<'T, 'M>) : 'State =
        Array.fold folder state xs.Xs

    /// Visits elements last-to-first.
    let foldBack (folder: 'T -> 'State -> 'State) (xs: BlockM<'T, 'M>) (state: 'State) : 'State =
        Array.foldBack folder xs.Xs state

    /// `ValueNone` for an out-of-range index, in place of an indexer's throw.
    let tryItem (index: int<'M>) (xs: BlockM<'T, 'M>) : 'T voption =
        let src = xs.Xs
        let i = int index

        if i >= 0 && i < src.Length then
            ValueSome src.[i]
        else
            ValueNone

    let tryLast (xs: BlockM<'T, 'M>) : 'T voption =
        let src = xs.Xs

        match src.Length with
        | 0 -> ValueNone
        | n -> ValueSome src.[n - 1]

    let last (xs: BlockM<'T, 'M>) : 'T =
        let src = xs.Xs

        match src.Length with
        | 0 -> invalidArg "xs" "Block.last: the input block was empty"
        | n -> src.[n - 1]

    let filter (predicate: 'T -> bool) (xs: BlockM<'T, 'M>) : BlockM<'T, 'M> =
        BlockM<'T, 'M>(Array.filter predicate xs.Xs)

    /// Ascending by `Comparer<'T>.Default`, so callers need no `'T: comparison` constraint.
    let sort (xs: BlockM<'T, 'M>) : BlockM<'T, 'M> =
        // `toArray` already copied, and nothing else holds `out`, so the buffer is handed
        // over rather than copied a second time.
        let out = toArray xs
        Array.Sort(out, Comparer<'T>.Default)
        BlockM<'T, 'M>(out)

    let append (xs: BlockM<'T, 'M>) (ys: BlockM<'T, 'M>) : BlockM<'T, 'M> =
        if xs.IsEmpty then ys
        elif ys.IsEmpty then xs
        else BlockM<'T, 'M>(Array.append xs.Xs ys.Xs)

    /// First occurrence of each element wins, so the surviving order is the input's. Uses
    /// `EqualityComparer<'T>.Default`, so callers need no `'T: equality` constraint.
    let distinct (xs: BlockM<'T, 'M>) : BlockM<'T, 'M> =
        let src = xs.Xs
        let seen = HashSet<'T>(EqualityComparer<'T>.Default)
        let out = ResizeArray<'T>(src.Length)

        for i in 0 .. src.Length - 1 do
            if seen.Add src.[i] then
                out.Add src.[i]

        BlockM<'T, 'M>(out.ToArray())

    /// Uses `EqualityComparer<'T>.Default`, so callers need no `'T: equality` constraint.
    let contains (value: 'T) (xs: BlockM<'T, 'M>) : bool =
        let src = xs.Xs
        let cmp = EqualityComparer<'T>.Default
        let mutable i = 0
        let mutable hit = false

        while not hit && i < src.Length do
            if cmp.Equals(src.[i], value) then
                hit <- true

            i <- i + 1

        hit

    /// Returns the first `count` elements (all of them when `count` exceeds the length; empty
    /// when `count <= 0`). Returns the SAME block when nothing is dropped.
    let truncate (count: int<'M>) (xs: BlockM<'T, 'M>) : BlockM<'T, 'M> =
        let src = xs.Xs
        let n = min (max (int count) 0) src.Length

        if n = src.Length then
            xs
        else
            BlockM<'T, 'M>(Array.sub src 0 n)

    /// Wraps the immutable array's own buffer, so nothing is copied.
    let ofImmutable (xs: ImmutableArray<'T>) : Block<'T> =
        match ImmutableCollectionsMarshal.AsArray xs with
        | null -> empty
        | arr -> unsafeOfArray arr

/// Fixed-arity deconstruction patterns: allocation-free arity checks that bind elements by
/// position, in place of `match Block.toList xs with [ … ]`.
[<AutoOpen>]
module BlockPatterns =
    [<return: Struct>]
    let (|BlockEmpty|_|) (xs: BlockM<'T, 'M>) : unit voption =
        if xs.IsEmpty then ValueSome() else ValueNone

    [<return: Struct>]
    let (|BlockOne|_|) (xs: BlockM<'T, 'M>) : 'T voption =
        let src = xs.Xs

        match src.Length with
        | 1 -> ValueSome src.[0]
        | _ -> ValueNone

    [<return: Struct>]
    let (|BlockTwo|_|) (xs: BlockM<'T, 'M>) : struct ('T * 'T) voption =
        let src = xs.Xs

        match src.Length with
        | 2 -> ValueSome(struct (src.[0], src.[1]))
        | _ -> ValueNone

    [<return: Struct>]
    let (|BlockThree|_|) (xs: BlockM<'T, 'M>) : struct ('T * 'T * 'T) voption =
        let src = xs.Xs

        match src.Length with
        | 3 -> ValueSome(struct (src.[0], src.[1], src.[2]))
        | _ -> ValueNone
