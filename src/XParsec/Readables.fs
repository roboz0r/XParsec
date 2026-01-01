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
    interface IReadable<char, ReadableStringSlice> with
        member _.Item
            with get index =
                if index < 0 || index >= length then
                    raise (IndexOutOfRangeException())

                s.[start + index]

        member _.TryItem(index) =
            if index < length then
                ValueSome(s.[start + index])
            else
                ValueNone

        member _.SpanSlice(index, count) =
            if index < 0 then
                invalidArg "index" "Index must be non-negative."

            if count < 0 then
                invalidArg "count" "Count must be non-negative."

            if index > length then
                ReadOnlySpan.Empty
            else
                let length = min count (length - int index)
                s.AsSpan(start + int index, length)

        member _.Length = length

        member _.Slice(newStart: int, newLength: int) =
            if newStart < 0 then
                invalidArg "newStart" "New start must be non-negative."

            if newLength < 0 then
                invalidArg "newLength" "New length must be non-negative."

            let newStart = start + newStart

            if newStart > s.Length then
                ReadableStringSlice(s, start, 0)
            else
                let newLength = min newLength (s.Length - newStart)
                ReadableStringSlice(s, start + newStart, newLength)

/// A string that can be read as input by the parser.
[<Struct>]
type ReadableString(s: string) =
    interface IReadable<char, ReadableStringSlice> with
        member _.Item
            with get index =
                if index < 0 || index >= s.Length then
                    raise (IndexOutOfRangeException())

                s.[index]

        member _.TryItem(index) =
            if index < s.Length then ValueSome(s.[index]) else ValueNone

        member _.SpanSlice(index, count) =
            if index < 0 then
                invalidArg "index" "Index must be non-negative."

            if count < 0 then
                invalidArg "count" "Count must be non-negative."

            if index > s.Length then
                ReadOnlySpan.Empty
            else
                let length = min count (s.Length - index)
                s.AsSpan(index, length)

        member _.Length = s.Length

        member _.Slice(newStart: int, newLength: int) =
            if newStart < 0 then
                invalidArg "newStart" "New start must be non-negative."

            if newLength < 0 then
                invalidArg "newLength" "New length must be non-negative."

            if newStart > s.Length then
                ReadableStringSlice(s, 0, 0)
            else
                let newLength = min newLength (s.Length - newStart)
                ReadableStringSlice(s, newStart, newLength)

/// An array slice that can be read as input by the parser.
[<Struct>]
type ReadableArraySlice<'T>(arr: 'T array, start: int, length: int) =
    interface IReadable<'T, ReadableArraySlice<'T>> with
        member _.Item
            with get index =
                if index < 0 || index >= length then
                    raise (IndexOutOfRangeException())

                arr.[start + index]

        member _.TryItem(index) =
            if index < length then
                ValueSome(arr.[start + index])
            else
                ValueNone

        member _.SpanSlice(index, count) =
            if index < 0 then
                invalidArg "index" "Index must be non-negative."

            if count < 0 then
                invalidArg "count" "Count must be non-negative."

            if index > length then
                ReadOnlySpan.Empty
            else
                let length = min count (length - index)
#if FABLE_COMPILER
                ArraySpan<'T>(arr, start + index, length) :> ReadOnlySpan<'T>
#else
                ReadOnlySpan<'T>(arr, start + index, length)
#endif

        member _.Length = length

        member _.Slice(newStart: int, newLength: int) =
            if newStart < 0 then
                invalidArg "newStart" "New start must be non-negative."

            if newLength < 0 then
                invalidArg "newLength" "New length must be non-negative."

            let newStart = start + newStart

            if newStart > arr.Length then
                ReadableArraySlice(arr, start, 0)
            else
                let newLength = min newLength (arr.Length - newStart)
                ReadableArraySlice(arr, start + newStart, newLength)

/// An array that can be read as input by the parser.
[<Struct>]
type ReadableArray<'T>(arr: 'T array) =
    interface IReadable<'T, ReadableArraySlice<'T>> with
        member _.Item
            with get index =
                if index < 0 || index >= arr.Length then
                    raise (IndexOutOfRangeException())

                arr.[index]

        member _.TryItem(index) =
            if index < arr.Length then
                ValueSome(arr.[index])
            else
                ValueNone

        member _.SpanSlice(index, count) =
            if index < 0 then
                invalidArg "index" "Index must be non-negative."

            if count < 0 then
                invalidArg "count" "Count must be non-negative."

            if index > arr.Length then
                ReadOnlySpan.Empty
            else
                let length = min count (arr.Length - index)
#if FABLE_COMPILER
                ArraySpan<'T>(arr, index, length) :> ReadOnlySpan<'T>
#else
                ReadOnlySpan<'T>(arr, index, length)
#endif

        member _.Length = arr.Length

        member _.Slice(newStart: int, newLength: int) =
            if newStart < 0 then
                invalidArg "newStart" "New start must be non-negative."

            if newLength < 0 then
                invalidArg "newLength" "New length must be non-negative."

            if newStart > arr.Length then
                ReadableArraySlice(arr, 0, 0)
            else
                let newLength = min newLength (arr.Length - newStart)
                ReadableArraySlice(arr, newStart, newLength)

/// An immutable array slice that can be read as input by the parser.
[<Struct>]
type ReadableImmutableArraySlice<'T>(arr: ImmutableArray<'T>, start: int, length: int) =
    interface IReadable<'T, ReadableImmutableArraySlice<'T>> with
        member _.Item
            with get index =
                if index < 0 || index >= length then
                    raise (IndexOutOfRangeException())

                arr.[start + index]

        member _.TryItem(index) =
            if index < length then
                ValueSome(arr.[start + index])
            else
                ValueNone

        member _.SpanSlice(index, count) =
            if index < 0 then
                invalidArg "index" "Index must be non-negative."

            if count < 0 then
                invalidArg "count" "Count must be non-negative."

            if index > length then
                ReadOnlySpan.Empty
            else
                let length = min count (length - index)
                arr.AsSpan(start + index, length)

        member _.Length = length

        member _.Slice(newStart: int, newLength: int) =
            if newStart < 0 then
                invalidArg "newStart" "New start must be non-negative."

            if newLength < 0 then
                invalidArg "newLength" "New length must be non-negative."

            let newStart = start + newStart

            if newStart > arr.Length then
                ReadableImmutableArraySlice(arr, start, 0)
            else
                let newLength = min newLength (arr.Length - newStart)
                ReadableImmutableArraySlice(arr, start + newStart, newLength)

/// An immutable array that can be read as input by the parser.
[<Struct>]
type ReadableImmutableArray<'T>(arr: ImmutableArray<'T>) =
    interface IReadable<'T, ReadableImmutableArraySlice<'T>> with
        member _.Item
            with get index =
                if index < 0 || index >= arr.Length then
                    raise (IndexOutOfRangeException())

                arr.[index]

        member _.TryItem(index) =
            if index < arr.Length then
                ValueSome(arr.[index])
            else
                ValueNone

        member _.SpanSlice(index, count) =
            if index < 0 then
                invalidArg "index" "Index must be non-negative."

            if count < 0 then
                invalidArg "count" "Count must be non-negative."

            if index > arr.Length then
                ReadOnlySpan.Empty
            else
                let length = min count (arr.Length - index)
                arr.AsSpan(index, length)

        member _.Length = arr.Length

        member _.Slice(newStart: int, newLength: int) =
            if newStart < 0 then
                invalidArg "newStart" "New start must be non-negative."

            if newLength < 0 then
                invalidArg "newLength" "New length must be non-negative."

            if newStart > arr.Length then
                ReadableImmutableArraySlice(arr, 0, 0)
            else
                let newLength = min newLength (arr.Length - newStart)
                ReadableImmutableArraySlice(arr, newStart, newLength)

#if NET5_0_OR_GREATER // No good way to get a span from a ResizeArray in .NET Standard 2.0
/// A ResizeArray slice that can be read as input by the parser.
[<Struct>]
type ReadableResizeArraySlice<'T>(arr: ResizeArray<'T>, start: int, length: int) =
    interface IReadable<'T, ReadableResizeArraySlice<'T>> with
        member _.Item
            with get index =
                if index < 0 || index >= length then
                    raise (IndexOutOfRangeException())

                arr.[start + index]

        member _.TryItem(index) =
            if index < length then
                ValueSome(arr.[start + index])
            else
                ValueNone

        member _.SpanSlice(index, count) =
            if index < 0 then
                invalidArg "index" "Index must be non-negative."

            if count < 0 then
                invalidArg "count" "Count must be non-negative."

            if index > length then
                ReadOnlySpan.Empty
            else
                let length = min count (length - index)
#if FABLE_COMPILER
                ResizeArraySpan<'T>(arr, start + index, length) :> ReadOnlySpan<'T>
#else
                let span = CollectionsMarshal.AsSpan(arr)
                Span.op_Implicit (span.Slice(start + index, length))
#endif

        member _.Length = length

        member _.Slice(newStart: int, newLength: int) =
            if newStart < 0 then
                invalidArg "newStart" "New start must be non-negative."

            if newLength < 0 then
                invalidArg "newLength" "New length must be non-negative."

            let newStart = start + newStart

            if newStart > arr.Count then
                ReadableResizeArraySlice(arr, start, 0)
            else
                let newLength = min newLength (arr.Count - newStart)
                ReadableResizeArraySlice(arr, start + newStart, newLength)

/// A ResizeArray that can be read as input by the parser.
[<Struct>]
type ReadableResizeArray<'T>(arr: ResizeArray<'T>) =
    interface IReadable<'T, ReadableResizeArraySlice<'T>> with
        member _.Item
            with get index =
                if index < 0 || index >= arr.Count then
                    raise (IndexOutOfRangeException())

                arr.[index]

        member _.TryItem(index) =
            if index < arr.Count then
                ValueSome(arr.[index])
            else
                ValueNone

        member _.SpanSlice(index, count) =
            if index < 0 then
                invalidArg "index" "Index must be non-negative."

            if count < 0 then
                invalidArg "count" "Count must be non-negative."

            if index > arr.Count then
                ReadOnlySpan.Empty
            else
                let length = min count (arr.Count - index)
#if FABLE_COMPILER
                ResizeArraySpan<'T>(arr, index, length) :> ReadOnlySpan<'T>
#else
                let span = CollectionsMarshal.AsSpan(arr)
                Span.op_Implicit (span.Slice(index, length))
#endif

        member _.Length = arr.Count

        member _.Slice(newStart: int, newLength: int) =
            if newStart < 0 then
                invalidArg "newStart" "New start must be non-negative."

            if newLength < 0 then
                invalidArg "newLength" "New length must be non-negative."

            if newStart > arr.Count then
                ReadableResizeArraySlice(arr, 0, 0)
            else
                let newLength = min newLength (arr.Count - newStart)
                ReadableResizeArraySlice(arr, newStart, newLength)
#endif

#if !FABLE_COMPILER
/// A memory slice that can be read as input by the parser.
[<Struct>]
type ReadableMemory<'T>(memory: ReadOnlyMemory<'T>) =
    interface IReadable<'T, ReadableMemory<'T>> with
        member _.Item
            with get index =
                if index < 0 || index >= memory.Length then
                    raise (IndexOutOfRangeException())

                memory.Span[index]

        member _.TryItem(index) =
            if index < memory.Length then
                ValueSome(memory.Span[index])
            else
                ValueNone

        member _.SpanSlice(index, count) =
            if index < 0 then
                invalidArg "index" "Index must be non-negative."

            if count < 0 then
                invalidArg "count" "Count must be non-negative."

            if index > memory.Length then
                ReadOnlySpan.Empty
            else
                let length = min count (memory.Length - index)
                memory.Span.Slice(index, length)

        member _.Length = memory.Length

        member _.Slice(newStart: int, newLength: int) =
            if newStart < 0 then
                invalidArg "newStart" "New start must be non-negative."

            if newLength < 0 then
                invalidArg "newLength" "New length must be non-negative."

            if newStart > memory.Length then
                ReadableMemory(memory.Slice(0, 0))
            else
                let newLength = min newLength (memory.Length - newStart)
                ReadableMemory(memory.Slice(newStart, newLength))

    new(memory: Memory<'T>) = ReadableMemory(memory)
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
