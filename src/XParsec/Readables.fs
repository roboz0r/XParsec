namespace XParsec

open System
open System.IO
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices
#if !FABLE_COMPILER
open System.Runtime.InteropServices
#endif

open XParsec

[<AutoOpen>]
module RaiseHelpers =
    // NoInlining keeps this cold code out of your fast-path loops
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let raiseIndexOutOfRange () = 
        raise (IndexOutOfRangeException())

/// A string slice that can be read as input by the parser.
[<Struct>]
type ReadableString(s: string, start: int, length: int) =
    member _.Item
        with get index =
            if uint index >= uint length then
                raiseIndexOutOfRange()

            s.[start + index]

    member _.TryItem(index) =
        // uint cast safely checks 0 <= index < length
        if uint index < uint length then
            ValueSome(s.[start + index])
        else
            ValueNone

    member _.SpanSlice(index, count) =
        if index < 0 then
            invalidArg "index" "Index must be non-negative."

        if count < 0 then
            invalidArg "count" "Count must be non-negative."

        if index >= length then
            ReadOnlySpan.Empty
        else
            // Clamp the count to the remaining length of THIS slice
            let safeCount = min count (length - index)
            s.AsSpan(start + index, safeCount)

    member _.Length = length

    member _.Slice(newStart: int, newLength: int) =
        if newStart < 0 then
            invalidArg "newStart" "New start must be non-negative."

        if newLength < 0 then
            invalidArg "newLength" "New length must be non-negative."

        if newStart >= length then
            ReadableString(s, start + length, 0)
        else
            // Clamp to the remaining length of THIS slice
            let safeLength = min newLength (length - newStart)
            ReadableString(s, start + newStart, safeLength)

    interface IReadable<char, ReadableString> with
        member this.Item
            with get index = this.Item index

        member this.TryItem(index) = this.TryItem index

        member this.SpanSlice(index, count) = this.SpanSlice(index, count)

        member this.Length = this.Length

        member this.Slice(newStart: int, newLength: int) = this.Slice(newStart, newLength)

/// An array slice that can be read as input by the parser.
[<Struct; CustomEquality; NoComparison>]
type ReadableArray<'T>(arr: 'T array, start: int, length: int) =
    static member Empty: ReadableArray<'T> = ReadableArray(Array.empty, 0, 0)

    member _.Item
        with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get index =
            if uint index >= uint length then
                raiseIndexOutOfRange()

            arr.[start + index]

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member _.TryItem(index) =
        // uint cast safely checks 0 <= index < length
        if uint index < uint length then
            ValueSome(arr.[start + index])
        else
            ValueNone

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member _.SpanSlice(index, count) =
        if index < 0 then
            invalidArg "index" "Index must be non-negative."

        if count < 0 then
            invalidArg "count" "Count must be non-negative."

        if index >= length then
            ReadOnlySpan.Empty
        else
            // Clamp the count to the remaining length of THIS slice
            let safeCount = min count (length - index)
#if FABLE_COMPILER
            ArraySpan<'T>(arr, start + index, safeCount) :> ReadOnlySpan<'T>
#else
            ReadOnlySpan<'T>(arr, start + index, safeCount)
#endif

    member _.Length
        with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get () = length

    member _.IsEmpty
        with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get () = length = 0

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member _.Slice(newStart: int, newLength: int) =
        if newStart < 0 then
            invalidArg "newStart" "New start must be non-negative."

        if newLength < 0 then
            invalidArg "newLength" "New length must be non-negative."

        if newStart >= length then
            ReadableArray(arr, start + length, 0)
        else
            // Clamp to the remaining length of THIS slice
            let safeLength = min newLength (length - newStart)
            ReadableArray(arr, start + newStart, safeLength)

    /// True if any element equals `item` (uses EqualityComparer&lt;'T&gt;.Default).
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member _.Contains(item: 'T) : bool =
        let cmp = EqualityComparer<'T>.Default
        let mutable i = 0

        while i < length && not (cmp.Equals(arr.[start + i], item)) do
            i <- i + 1

        i < length

    /// Materialises this view into an ImmutableArray. Escape hatch for consumers
    /// that specifically need an ImmutableArray; allocates a fresh copy.
    member _.ToImmutableArray() : ImmutableArray<'T> =
        if length = 0 then
            ImmutableArray<'T>.Empty
        else
            let builder = ImmutableArray.CreateBuilder<'T>(length)

            for i in 0 .. length - 1 do
                builder.Add(arr.[start + i])

            builder.MoveToImmutable()

    override this.Equals(other: obj) =
        match other with
        | :? ReadableArray<'T> as other -> (this :> IEquatable<ReadableArray<'T>>).Equals(other)
        | _ -> false

    override _.GetHashCode() =
        let cmp = EqualityComparer<'T>.Default
        let mutable h = 17

        for i in 0 .. length - 1 do
            let v = arr.[start + i]
            h <- (h * 31) + (if isNull (box v) then 0 else cmp.GetHashCode(v))

        h

    interface IEquatable<ReadableArray<'T>> with
        member _.Equals(other) =
            if length <> other.Length then
                false
            else
                let cmp = EqualityComparer<'T>.Default
                let mutable i = 0

                while i < length && cmp.Equals(arr.[start + i], other.[i]) do
                    i <- i + 1

                i = length

    interface IReadable<'T, ReadableArray<'T>> with
        member this.Item
            with get index = this.Item index

        member this.TryItem(index) = this.TryItem index

        member this.SpanSlice(index, count) = this.SpanSlice(index, count)

        member this.Length = this.Length

        member this.Slice(newStart: int, newLength: int) = this.Slice(newStart, newLength)

    interface IReadOnlyCollection<'T> with
        member _.Count = length

    interface IReadOnlyList<'T> with
        member _.Item
            with get (i: int) =
                if uint i >= uint length then
                    raiseIndexOutOfRange()

                arr.[start + i]

    interface IEnumerable<'T> with
        member _.GetEnumerator() : IEnumerator<'T> =
            let a = arr
            let s = start
            let len = length
            let mutable i = 0

            { new IEnumerator<'T> with
                member _.Current = a.[s + i - 1]
              interface System.Collections.IEnumerator with
                  member _.Current = box a.[s + i - 1]

                  member _.MoveNext() =
                      if i < len then
                          i <- i + 1
                          true
                      else
                          false

                  member _.Reset() = i <- 0
              interface IDisposable with
                  member _.Dispose() = ()
            }

    interface System.Collections.IEnumerable with
        member this.GetEnumerator() =
            (this :> IEnumerable<'T>).GetEnumerator() :> System.Collections.IEnumerator

/// Append-only builder that materialises into a ReadableArray, usually without copying.
/// Single-shot: after ToReadableArray the builder releases its buffer.
type ReadableArrayBuilder<'T>(initialCapacity: int) =
    let mutable arr: 'T array = Array.zeroCreate (max 4 initialCapacity)
    let mutable count = 0

    new() = ReadableArrayBuilder(4)

    member _.Count
        with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get () = count

    member _.Capacity
        with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get () = arr.Length

    member _.Item
        with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get (index: int) =
            if uint index >= uint count then
                raiseIndexOutOfRange()

            arr.[index]
        and [<MethodImpl(MethodImplOptions.AggressiveInlining)>] set (index: int) (value: 'T) =
            if uint index >= uint count then
                raiseIndexOutOfRange()

            arr.[index] <- value

    // Fast-path/slow-path split (mirrors System.Collections.Generic.List<T>.Add):
    // the hot path is small enough to inline; growth is factored out behind
    // NoInlining so it never inflates the caller's icache footprint.
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.Add(item: 'T) =
        let a = arr
        let c = count

        if uint c < uint a.Length then
            count <- c + 1
            a.[c] <- item
        else
            this.AddWithResize(item)

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    member private this.AddWithResize(item: 'T) =
        Array.Resize(&arr, arr.Length * 2)
        arr.[count] <- item
        count <- count + 1

    /// Returns a ReadableArray viewing the first `count` slots of the backing buffer.
    /// Zero-copy when the buffer is at least half full; right-sizes otherwise so
    /// the resulting array doesn't retain significant unused capacity.
    /// Single-shot: releases the builder's reference — do not reuse.
    member _.ToReadableArray() : ReadableArray<'T> =
        let result =
            if count = 0 then
                ReadableArray<'T>.Empty
            elif count >= (arr.Length >>> 1) then
                // Buffer is at least half full — skip the copy; at most 2x bytes retained.
                ReadableArray(arr, 0, count)
            else
                // Wasteful — right-size so we don't keep the oversized buffer alive.
                let resized = Array.zeroCreate count
                Array.Copy(arr, 0, resized, 0, count)
                ReadableArray(resized, 0, count)

        arr <- Array.empty
        count <- 0
        result


/// An immutable array slice that can be read as input by the parser.
[<Struct>]
type ReadableImmutableArray<'T>(arr: ImmutableArray<'T>, start: int, length: int) =
    member _.Item
        with get index =
            if uint index >= uint length then
                raiseIndexOutOfRange()

            arr.[start + index]

    member _.TryItem(index) =
        // uint cast safely checks 0 <= index < length
        if uint index < uint length then
            ValueSome(arr.[start + index])
        else
            ValueNone

    member _.SpanSlice(index, count) =
        if index < 0 then
            invalidArg "index" "Index must be non-negative."

        if count < 0 then
            invalidArg "count" "Count must be non-negative."

        if index >= length then
            ReadOnlySpan.Empty
        else
            // Clamp the count to the remaining length of THIS slice
            let safeCount = min count (length - index)
            arr.AsSpan(start + index, safeCount)

    member _.Length = length

    member _.Slice(newStart: int, newLength: int) =
        if newStart < 0 then
            invalidArg "newStart" "New start must be non-negative."

        if newLength < 0 then
            invalidArg "newLength" "New length must be non-negative."

        if newStart >= length then
            ReadableImmutableArray(arr, start + length, 0)
        else
            // Clamp to the remaining length of THIS slice
            let safeLength = min newLength (length - newStart)
            ReadableImmutableArray(arr, start + newStart, safeLength)

    interface IReadable<'T, ReadableImmutableArray<'T>> with
        member this.Item
            with get index = this.Item index

        member this.TryItem(index) = this.TryItem index

        member this.SpanSlice(index, count) = this.SpanSlice(index, count)

        member this.Length = this.Length

        member this.Slice(newStart: int, newLength: int) = this.Slice(newStart, newLength)


#if NET5_0_OR_GREATER // No good way to get a span from a ResizeArray in .NET Standard 2.0
/// A ResizeArray slice that can be read as input by the parser.
[<Struct>]
type ReadableResizeArray<'T>(arr: ResizeArray<'T>, start: int, length: int) =
    member _.Item
        with get index =
            if uint index >= uint length then
                raiseIndexOutOfRange()

            arr.[start + index]

    member _.TryItem(index) =
        // uint cast safely checks 0 <= index < length
        if uint index < uint length then
            ValueSome(arr.[start + index])
        else
            ValueNone

    member _.SpanSlice(index, count) =
        if index < 0 then
            invalidArg "index" "Index must be non-negative."

        if count < 0 then
            invalidArg "count" "Count must be non-negative."

        if index >= length then
            ReadOnlySpan.Empty
        else
            // Clamp the count to the remaining length of THIS slice
            let safeCount = min count (length - index)
#if FABLE_COMPILER
            ResizeArraySpan<'T>(arr, start + index, safeCount) :> ReadOnlySpan<'T>
#else
            let span = CollectionsMarshal.AsSpan(arr)
            Span.op_Implicit (span.Slice(start + index, safeCount))
#endif

    member _.Length = length

    member _.Slice(newStart: int, newLength: int) =
        if newStart < 0 then
            invalidArg "newStart" "New start must be non-negative."

        if newLength < 0 then
            invalidArg "newLength" "New length must be non-negative."

        if newStart >= length then
            ReadableResizeArray(arr, start + length, 0)
        else
            // Clamp to the remaining length of THIS slice
            let safeLength = min newLength (length - newStart)
            ReadableResizeArray(arr, start + newStart, safeLength)

    interface IReadable<'T, ReadableResizeArray<'T>> with
        member this.Item
            with get index = this.Item index

        member this.TryItem(index) = this.TryItem index

        member this.SpanSlice(index, count) = this.SpanSlice(index, count)

        member this.Length = this.Length

        member this.Slice(newStart: int, newLength: int) = this.Slice(newStart, newLength)
#endif

#if !FABLE_COMPILER
/// A memory slice that can be read as input by the parser.
[<Struct>]
type ReadableMemory<'T>(memory: ReadOnlyMemory<'T>) =
    member _.Item
        with get index =
            if uint index >= uint memory.Length then
                raiseIndexOutOfRange()

            memory.Span[index]

    member _.TryItem(index) =
        if uint index < uint memory.Length then
            ValueSome(memory.Span[index])
        else
            ValueNone

    member _.SpanSlice(index, count) =
        if index < 0 then
            invalidArg "index" "Index must be non-negative."

        if count < 0 then
            invalidArg "count" "Count must be non-negative."

        if index >= memory.Length then
            ReadOnlySpan.Empty
        else
            let safeCount = min count (memory.Length - index)
            memory.Span.Slice(index, safeCount)

    member _.Length = memory.Length

    member _.Slice(newStart: int, newLength: int) =
        if newStart < 0 then
            invalidArg "newStart" "New start must be non-negative."

        if newLength < 0 then
            invalidArg "newLength" "New length must be non-negative."

        if newStart >= memory.Length then
            ReadableMemory(memory.Slice(memory.Length, 0))
        else
            let safeLength = min newLength (memory.Length - newStart)
            ReadableMemory(memory.Slice(newStart, safeLength))

    new(memory: Memory<'T>) = ReadableMemory(memory)

    interface IReadable<'T, ReadableMemory<'T>> with
        member this.Item
            with get index = this.Item index

        member this.TryItem(index) = this.TryItem index

        member this.SpanSlice(index, count) = this.SpanSlice(index, count)

        member this.Length = this.Length

        member this.Slice(newStart: int, newLength: int) = this.Slice(newStart, newLength)

#endif

module Reader =
    /// Creates a new reader from the input string and state.
    let ofString (s: string) state =
        Reader(ReadableString(s, 0, s.Length), state, 0)

    /// Creates a new reader from the input array and state.
    let ofArray (a: 'T array) state =
        Reader(ReadableArray(a, 0, a.Length), state, 0)

    /// Creates a new reader from the input immutable array and state.
    let ofImmutableArray (a: ImmutableArray<'T>) state =
        Reader(ReadableImmutableArray(a, 0, a.Length), state, 0)

    /// Creates a new reader from an existing ReadableArray slice and state.
    let ofReadableArray (a: ReadableArray<'T>) state = Reader(a, state, 0)

#if NET5_0_OR_GREATER
    /// Creates a new reader from the input resize array and state.
    let ofResizeArray (a: ResizeArray<'T>) state =
        Reader(ReadableResizeArray(a, 0, a.Count), state, 0)
#endif

#if !FABLE_COMPILER
    /// Creates a new reader from the input memory and state.
    let ofMemory (m: ReadOnlyMemory<'T>) state = Reader(ReadableMemory m, state, 0)
#endif
