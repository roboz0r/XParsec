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
type ReadableStringSlice(s: string, start: int, length: int) =
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
            ReadableStringSlice(s, start + length, 0)
        else
            // Clamp to the remaining length of THIS slice
            let safeLength = min newLength (length - newStart)
            ReadableStringSlice(s, start + newStart, safeLength)

    interface IReadable<char, ReadableStringSlice> with
        member this.Item
            with get index = this.Item index

        member this.TryItem(index) = this.TryItem index

        member this.SpanSlice(index, count) = this.SpanSlice(index, count)

        member this.Length = this.Length

        member this.Slice(newStart: int, newLength: int) = this.Slice(newStart, newLength)

/// A string that can be read as input by the parser.
[<Struct>]
type ReadableString(s: string) =
    member _.Item
        with get index =
            if uint index >= uint s.Length then
                raise (IndexOutOfRangeException())

            s.[index]

    member _.TryItem(index) =
        if uint index < uint s.Length then
            ValueSome(s.[index])
        else
            ValueNone

    member _.SpanSlice(index, count) =
        if index < 0 then
            invalidArg "index" "Index must be non-negative."

        if count < 0 then
            invalidArg "count" "Count must be non-negative."

        if index >= s.Length then
            ReadOnlySpan.Empty
        else
            // Clamp the count to the length
            let safeCount = min count (s.Length - index)
            s.AsSpan(index, safeCount)

    member _.Length = s.Length

    member _.Slice(newStart: int, newLength: int) =
        if newStart < 0 then
            invalidArg "newStart" "New start must be non-negative."

        if newLength < 0 then
            invalidArg "newLength" "New length must be non-negative."

        if newStart >= s.Length then
            ReadableStringSlice(s, s.Length, 0)
        else
            let safeLength = min newLength (s.Length - newStart)
            ReadableStringSlice(s, newStart, safeLength)

    interface IReadable<char, ReadableStringSlice> with
        member this.Item
            with get index = this.Item index

        member this.TryItem(index) = this.TryItem index

        member this.SpanSlice(index, count) = this.SpanSlice(index, count)

        member this.Length = this.Length

        member this.Slice(newStart: int, newLength: int) = this.Slice(newStart, newLength)

/// An array slice that can be read as input by the parser.
[<Struct>]
type ReadableArraySlice<'T>(arr: 'T array, start: int, length: int) =
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
            ReadableArraySlice(arr, start + length, 0)
        else
            // Clamp to the remaining length of THIS slice
            let safeLength = min newLength (length - newStart)
            ReadableArraySlice(arr, start + newStart, safeLength)

    interface IReadable<'T, ReadableArraySlice<'T>> with
        member this.Item
            with get index = this.Item index

        member this.TryItem(index) = this.TryItem index

        member this.SpanSlice(index, count) = this.SpanSlice(index, count)

        member this.Length = this.Length

        member this.Slice(newStart: int, newLength: int) = this.Slice(newStart, newLength)

/// An array that can be read as input by the parser.
[<Struct>]
type ReadableArray<'T>(arr: 'T array) =
    member _.Item
        with get index =
            if uint index >= uint arr.Length then
                raise (IndexOutOfRangeException())

            arr.[index]

    member _.TryItem(index) =
        if uint index < uint arr.Length then
            ValueSome(arr.[index])
        else
            ValueNone

    member _.SpanSlice(index, count) =
        if index < 0 then
            invalidArg "index" "Index must be non-negative."

        if count < 0 then
            invalidArg "count" "Count must be non-negative."

        if index >= arr.Length then
            ReadOnlySpan.Empty
        else
            // Clamp the count to the length
            let safeCount = min count (arr.Length - index)
#if FABLE_COMPILER
            ArraySpan<'T>(arr, index, safeCount) :> ReadOnlySpan<'T>
#else
            ReadOnlySpan<'T>(arr, index, safeCount)
#endif

    member _.Length = arr.Length

    member _.Slice(newStart: int, newLength: int) =
        if newStart < 0 then
            invalidArg "newStart" "New start must be non-negative."

        if newLength < 0 then
            invalidArg "newLength" "New length must be non-negative."

        if newStart >= arr.Length then
            ReadableArraySlice(arr, arr.Length, 0)
        else
            let safeLength = min newLength (arr.Length - newStart)
            ReadableArraySlice(arr, newStart, safeLength)

    interface IReadable<'T, ReadableArraySlice<'T>> with
        member this.Item
            with get index = this.Item index

        member this.TryItem(index) = this.TryItem index

        member this.SpanSlice(index, count) = this.SpanSlice(index, count)

        member this.Length = this.Length

        member this.Slice(newStart: int, newLength: int) = this.Slice(newStart, newLength)


/// An immutable array slice that can be read as input by the parser.
[<Struct>]
type ReadableImmutableArraySlice<'T>(arr: ImmutableArray<'T>, start: int, length: int) =
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
            ReadableImmutableArraySlice(arr, start + length, 0)
        else
            // Clamp to the remaining length of THIS slice
            let safeLength = min newLength (length - newStart)
            ReadableImmutableArraySlice(arr, start + newStart, safeLength)

    interface IReadable<'T, ReadableImmutableArraySlice<'T>> with
        member this.Item
            with get index = this.Item index

        member this.TryItem(index) = this.TryItem index

        member this.SpanSlice(index, count) = this.SpanSlice(index, count)

        member this.Length = this.Length

        member this.Slice(newStart: int, newLength: int) = this.Slice(newStart, newLength)


/// An immutable array that can be read as input by the parser.
[<Struct>]
type ReadableImmutableArray<'T>(arr: ImmutableArray<'T>) =
    member _.Item
        with get index =
            if uint index >= uint arr.Length then
                raise (IndexOutOfRangeException())

            arr.[index]

    member _.TryItem(index) =
        if uint index < uint arr.Length then
            ValueSome(arr.[index])
        else
            ValueNone

    member _.SpanSlice(index, count) =
        if index < 0 then
            invalidArg "index" "Index must be non-negative."

        if count < 0 then
            invalidArg "count" "Count must be non-negative."

        if index >= arr.Length then
            ReadOnlySpan.Empty
        else
            let safeCount = min count (arr.Length - index)
            arr.AsSpan(index, safeCount)

    member _.Length = arr.Length

    member _.Slice(newStart: int, newLength: int) =
        if newStart < 0 then
            invalidArg "newStart" "New start must be non-negative."

        if newLength < 0 then
            invalidArg "newLength" "New length must be non-negative."

        if newStart >= arr.Length then
            ReadableImmutableArraySlice(arr, arr.Length, 0)
        else
            let safeLength = min newLength (arr.Length - newStart)
            ReadableImmutableArraySlice(arr, newStart, safeLength)

    interface IReadable<'T, ReadableImmutableArraySlice<'T>> with
        member this.Item
            with get index = this.Item index

        member this.TryItem(index) = this.TryItem index

        member this.SpanSlice(index, count) = this.SpanSlice(index, count)

        member this.Length = this.Length

        member this.Slice(newStart: int, newLength: int) = this.Slice(newStart, newLength)

#if NET5_0_OR_GREATER // No good way to get a span from a ResizeArray in .NET Standard 2.0
/// A ResizeArray slice that can be read as input by the parser.
[<Struct>]
type ReadableResizeArraySlice<'T>(arr: ResizeArray<'T>, start: int, length: int) =
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
            ReadableResizeArraySlice(arr, start + length, 0)
        else
            // Clamp to the remaining length of THIS slice
            let safeLength = min newLength (length - newStart)
            ReadableResizeArraySlice(arr, start + newStart, safeLength)

    interface IReadable<'T, ReadableResizeArraySlice<'T>> with
        member this.Item
            with get index = this.Item index

        member this.TryItem(index) = this.TryItem index

        member this.SpanSlice(index, count) = this.SpanSlice(index, count)

        member this.Length = this.Length

        member this.Slice(newStart: int, newLength: int) = this.Slice(newStart, newLength)

/// A ResizeArray that can be read as input by the parser.
[<Struct>]
type ReadableResizeArray<'T>(arr: ResizeArray<'T>) =
    member _.Item
        with get index =
            if uint index >= uint arr.Count then
                raise (IndexOutOfRangeException())

            arr.[index]

    member _.TryItem(index) =
        if uint index < uint arr.Count then
            ValueSome(arr.[index])
        else
            ValueNone

    member _.SpanSlice(index, count) =
        if index < 0 then
            invalidArg "index" "Index must be non-negative."

        if count < 0 then
            invalidArg "count" "Count must be non-negative."

        if index >= arr.Count then
            ReadOnlySpan.Empty
        else
            let safeCount = min count (arr.Count - index)
#if FABLE_COMPILER
            ResizeArraySpan<'T>(arr, index, safeCount) :> ReadOnlySpan<'T>
#else
            let span = CollectionsMarshal.AsSpan(arr)
            Span.op_Implicit (span.Slice(index, safeCount))
#endif

    member _.Length = arr.Count

    member _.Slice(newStart: int, newLength: int) =
        if newStart < 0 then
            invalidArg "newStart" "New start must be non-negative."

        if newLength < 0 then
            invalidArg "newLength" "New length must be non-negative."

        if newStart >= arr.Count then
            ReadableResizeArraySlice(arr, arr.Count, 0)
        else
            let safeLength = min newLength (arr.Count - newStart)
            ReadableResizeArraySlice(arr, newStart, safeLength)

    interface IReadable<'T, ReadableResizeArraySlice<'T>> with
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
    let ofString (s: string) state = Reader(ReadableString s, state, 0)

    /// Creates a new reader from the input array and state.
    let ofArray (a: 'T array) state = Reader(ReadableArray a, state, 0)

    /// Creates a new reader from the input immutable array and state.
    let ofImmutableArray (a: ImmutableArray<'T>) state =
        Reader(ReadableImmutableArray a, state, 0)

#if NET5_0_OR_GREATER
    /// Creates a new reader from the input resize array and state.
    let ofResizeArray (a: ResizeArray<'T>) state = Reader(ReadableResizeArray a, state, 0)
#endif

#if !FABLE_COMPILER
    /// Creates a new reader from the input memory and state.
    let ofMemory (m: ReadOnlyMemory<'T>) state = Reader(ReadableMemory m, state, 0)
#endif
