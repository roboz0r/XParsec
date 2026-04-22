namespace XParsec

open System
open System.IO
open System.Collections.Immutable
#if !FABLE_COMPILER
open System.Runtime.InteropServices
#endif

open XParsec

/// A string slice that can be read as input by the parser.
[<Struct>]
type ReadableString(s: string, start: int, length: int) =
    member _.Item
        with get index =
            if uint index >= uint length then
                raise (IndexOutOfRangeException())

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
[<Struct>]
type ReadableArray<'T>(arr: 'T array, start: int, length: int) =
    member _.Item
        with get index =
            if uint index >= uint length then
                raise (IndexOutOfRangeException())

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
            ArraySpan<'T>(arr, start + index, safeCount) :> ReadOnlySpan<'T>
#else
            ReadOnlySpan<'T>(arr, start + index, safeCount)
#endif

    member _.Length = length

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

    interface IReadable<'T, ReadableArray<'T>> with
        member this.Item
            with get index = this.Item index

        member this.TryItem(index) = this.TryItem index

        member this.SpanSlice(index, count) = this.SpanSlice(index, count)

        member this.Length = this.Length

        member this.Slice(newStart: int, newLength: int) = this.Slice(newStart, newLength)


/// Append-only builder that materialises into a ReadableArray without copying.
/// Single-shot: after ToReadableArray the builder releases its buffer.
type ReadableArrayBuilder<'T>(initialCapacity: int) =
    let mutable arr: 'T array = Array.zeroCreate (max 4 initialCapacity)
    let mutable count = 0

    new() = ReadableArrayBuilder(16)

    member _.Count = count
    member _.Capacity = arr.Length

    member _.Item
        with get (index: int) =
            if uint index >= uint count then
                raise (IndexOutOfRangeException())

            arr.[index]
        and set (index: int) (value: 'T) =
            if uint index >= uint count then
                raise (IndexOutOfRangeException())

            arr.[index] <- value

    // Fast-path/slow-path split (mirrors System.Collections.Generic.List<T>.Add):
    // the hot path is small enough to inline; growth is factored out behind
    // NoInlining so it never inflates the caller's icache footprint.
    member this.Add(item: 'T) =
        let a = arr
        let c = count

        if uint c < uint a.Length then
            count <- c + 1
            a.[c] <- item
        else
            this.AddWithResize(item)

    [<System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)>]
    member private this.AddWithResize(item: 'T) =
        Array.Resize(&arr, arr.Length * 2)
        arr.[count] <- item
        count <- count + 1

    /// Zero-copy: returns a ReadableArray viewing the first `count` slots of the
    /// backing buffer. The builder releases its reference — do not reuse.
    member _.ToReadableArray() : ReadableArray<'T> =
        let result = ReadableArray(arr, 0, count)
        arr <- Array.empty
        count <- 0
        result


/// An immutable array slice that can be read as input by the parser.
[<Struct>]
type ReadableImmutableArray<'T>(arr: ImmutableArray<'T>, start: int, length: int) =
    member _.Item
        with get index =
            if uint index >= uint length then
                raise (IndexOutOfRangeException())

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
                raise (IndexOutOfRangeException())

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
                raise (IndexOutOfRangeException())

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
