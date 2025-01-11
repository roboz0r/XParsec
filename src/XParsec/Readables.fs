namespace XParsec

open System
open System.IO

#if !FABLE_COMPILER
open System.Runtime.InteropServices
#endif

open XParsec

[<Struct>]
type ReadableStringSlice(s: string, start: int, length: int) =
    interface IReadable<char, ReadableStringSlice> with
        member _.TryItem(index) =
            if index < int64 length then
                ValueSome(s.[start + int index])
            else
                ValueNone

        member _.SpanSlice(index, count) =
            if index < 0L then
                invalidArg "index" "Index must be non-negative."

            if count < 0 then
                invalidArg "count" "Count must be non-negative."

            if index > int64 length then
                ReadOnlySpan.Empty
            else
                let length = min count (length - int index)
                s.AsSpan(start + int index, length)

        member _.Length = int64 length

        member _.Slice(newStart: int64, newLength: int64) =
            if newStart < 0L then
                invalidArg "newStart" "New start must be non-negative."

            if newLength < 0 then
                invalidArg "newLength" "New length must be non-negative."

            let newStart = (int64 start) + newStart

            if newStart > int64 s.Length then
                ReadableStringSlice(s, start, 0)
            else
                let newLength = min newLength (int64 s.Length - newStart) |> int
                ReadableStringSlice(s, start + (int newStart), newLength)

[<Struct>]
type ReadableString(s: string) =
    interface IReadable<char, ReadableStringSlice> with
        member _.TryItem(index) =
            if index < int64 s.Length then
                ValueSome(s.[int index])
            else
                ValueNone

        member _.SpanSlice(index, count) =
            if index < 0L then
                invalidArg "index" "Index must be non-negative."

            if count < 0 then
                invalidArg "count" "Count must be non-negative."

            if index > int64 s.Length then
                ReadOnlySpan.Empty
            else
                let length = min count (s.Length - int index)
                s.AsSpan(int index, length)

        member _.Length = int64 s.Length

        member _.Slice(newStart: int64, newLength: int64) =
            if newStart < 0L then
                invalidArg "newStart" "New start must be non-negative."

            if newLength < 0 then
                invalidArg "newLength" "New length must be non-negative."

            if newStart > int64 s.Length then
                ReadableStringSlice(s, 0, 0)
            else
                let newLength = min newLength (int64 s.Length - newStart) |> int
                ReadableStringSlice(s, int newStart, newLength)

[<Struct>]
type ReadableArraySlice<'T>(arr: 'T array, start: int, length: int) =
    interface IReadable<'T, ReadableArraySlice<'T>> with
        member _.TryItem(index) =
            if index < int64 length then
                ValueSome(arr.[start + int index])
            else
                ValueNone

        member _.SpanSlice(index, count) =
            if index < 0L then
                invalidArg "index" "Index must be non-negative."

            if count < 0 then
                invalidArg "count" "Count must be non-negative."

            if index > int64 length then
                ReadOnlySpan.Empty
            else
                let length = min count (length - int index)
#if FABLE_COMPILER
                ArraySpan<'T>(arr, start + int index, length) :> ReadOnlySpan<'T>
#else
                ReadOnlySpan<'T>(arr, start + int index, length)
#endif

        member _.Length = int64 length

        member _.Slice(newStart: int64, newLength: int64) =
            if newStart < 0L then
                invalidArg "newStart" "New start must be non-negative."

            if newLength < 0 then
                invalidArg "newLength" "New length must be non-negative."

            let newStart = (int64 start) + newStart

            if newStart > int64 arr.Length then
                ReadableArraySlice(arr, start, 0)
            else
                let newLength = min newLength (int64 arr.Length - newStart) |> int
                ReadableArraySlice(arr, start + (int newStart), newLength)

[<Struct>]
type ReadableArray<'T>(arr: 'T array) =
    interface IReadable<'T, ReadableArraySlice<'T>> with
        member _.TryItem(index) =
            if index < int64 arr.Length then
                ValueSome(arr.[int index])
            else
                ValueNone

        member _.SpanSlice(index, count) =
            if index < 0L then
                invalidArg "index" "Index must be non-negative."

            if count < 0 then
                invalidArg "count" "Count must be non-negative."

            if index > int64 arr.Length then
                ReadOnlySpan.Empty
            else
                let length = min count (arr.Length - int index)
#if FABLE_COMPILER
                ArraySpan<'T>(arr, int index, length) :> ReadOnlySpan<'T>
#else
                ReadOnlySpan<'T>(arr, int index, length)
#endif

        member _.Length = int64 arr.Length

        member _.Slice(newStart: int64, newLength: int64) =
            if newStart < 0L then
                invalidArg "newStart" "New start must be non-negative."

            if newLength < 0 then
                invalidArg "newLength" "New length must be non-negative."

            if newStart > int64 arr.Length then
                ReadableArraySlice(arr, 0, 0)
            else
                let newLength = min newLength (int64 arr.Length - newStart) |> int
                ReadableArraySlice(arr, int newStart, newLength)

[<Struct>]
type ReadableImmutableArraySlice<'T>(arr: ImmutableArray<'T>, start: int, length: int) =
    interface IReadable<'T, ReadableImmutableArraySlice<'T>> with
        member _.TryItem(index) =
            if index < int64 length then
                ValueSome(arr.[start + int index])
            else
                ValueNone

        member _.SpanSlice(index, count) =
            if index < 0L then
                invalidArg "index" "Index must be non-negative."

            if count < 0 then
                invalidArg "count" "Count must be non-negative."

            if index > int64 length then
                ReadOnlySpan.Empty
            else
                let length = min count (length - int index)
                arr.AsSpan(start + int index, length)

        member _.Length = int64 length

        member _.Slice(newStart: int64, newLength: int64) =
            if newStart < 0L then
                invalidArg "newStart" "New start must be non-negative."

            if newLength < 0 then
                invalidArg "newLength" "New length must be non-negative."

            let newStart = (int64 start) + newStart

            if newStart > int64 arr.Length then
                ReadableImmutableArraySlice(arr, start, 0)
            else
                let newLength = min newLength (int64 arr.Length - newStart) |> int
                ReadableImmutableArraySlice(arr, start + (int newStart), newLength)

[<Struct>]
type ReadableImmutableArray<'T>(arr: ImmutableArray<'T>) =
    interface IReadable<'T, ReadableImmutableArraySlice<'T>> with
        member _.TryItem(index) =
            if index < int64 arr.Length then
                ValueSome(arr.[int index])
            else
                ValueNone

        member _.SpanSlice(index, count) =
            if index < 0L then
                invalidArg "index" "Index must be non-negative."

            if count < 0 then
                invalidArg "count" "Count must be non-negative."

            if index > int64 arr.Length then
                ReadOnlySpan.Empty
            else
                let length = min count (arr.Length - int index)
                arr.AsSpan(int index, length)

        member _.Length = int64 arr.Length

        member _.Slice(newStart: int64, newLength: int64) =
            if newStart < 0L then
                invalidArg "newStart" "New start must be non-negative."

            if newLength < 0 then
                invalidArg "newLength" "New length must be non-negative."

            if newStart > int64 arr.Length then
                ReadableImmutableArraySlice(arr, 0, 0)
            else
                let newLength = min newLength (int64 arr.Length - newStart) |> int
                ReadableImmutableArraySlice(arr, int newStart, newLength)

#if NET5_0_OR_GREATER // No good way to get a span from a ResizeArray in .NET Standard 2.0
[<Struct>]
type ReadableResizeArraySlice<'T>(arr: ResizeArray<'T>, start: int, length: int) =
    interface IReadable<'T, ReadableResizeArraySlice<'T>> with
        member _.TryItem(index) =
            if index < int64 length then
                ValueSome(arr.[start + int index])
            else
                ValueNone

        member _.SpanSlice(index, count) =
            if index < 0L then
                invalidArg "index" "Index must be non-negative."

            if count < 0 then
                invalidArg "count" "Count must be non-negative."

            if index > int64 length then
                ReadOnlySpan.Empty
            else
                let length = min count (length - int index)
#if FABLE_COMPILER
                ResizeArraySpan<'T>(arr, start + int index, length) :> ReadOnlySpan<'T>
#else
                let span = CollectionsMarshal.AsSpan(arr)
                Span.op_Implicit (span.Slice(start + int index, length))
#endif

        member _.Length = int64 length

        member _.Slice(newStart: int64, newLength: int64) =
            if newStart < 0L then
                invalidArg "newStart" "New start must be non-negative."

            if newLength < 0 then
                invalidArg "newLength" "New length must be non-negative."

            let newStart = (int64 start) + newStart

            if newStart > int64 arr.Count then
                ReadableResizeArraySlice(arr, start, 0)
            else
                let newLength = min newLength (int64 arr.Count - newStart) |> int
                ReadableResizeArraySlice(arr, start + (int newStart), newLength)

[<Struct>]
type ReadableResizeArray<'T>(arr: ResizeArray<'T>) =
    interface IReadable<'T, ReadableResizeArraySlice<'T>> with
        member _.TryItem(index) =
            if index < int64 arr.Count then
                ValueSome(arr.[int index])
            else
                ValueNone

        member _.SpanSlice(index, count) =
            if index < 0L then
                invalidArg "index" "Index must be non-negative."

            if count < 0 then
                invalidArg "count" "Count must be non-negative."

            if index > int64 arr.Count then
                ReadOnlySpan.Empty
            else
                let length = min count (arr.Count - int index)
#if FABLE_COMPILER
                ResizeArraySpan<'T>(arr, int index, length) :> ReadOnlySpan<'T>
#else
                let span = CollectionsMarshal.AsSpan(arr)
                Span.op_Implicit (span.Slice(int index, length))
#endif

        member _.Length = int64 arr.Count

        member _.Slice(newStart: int64, newLength: int64) =
            if newStart < 0L then
                invalidArg "newStart" "New start must be non-negative."

            if newLength < 0 then
                invalidArg "newLength" "New length must be non-negative."

            if newStart > int64 arr.Count then
                ReadableResizeArraySlice(arr, 0, 0)
            else
                let newLength = min newLength (int64 arr.Count - newStart) |> int
                ReadableResizeArraySlice(arr, int newStart, newLength)
#endif

#if !FABLE_COMPILER
[<Struct>]
type ReadableStreamSlice(stream: Stream, start: int64, length: int64, buffer: byte[]) =
    interface IReadable<byte, ReadableStreamSlice> with
        member _.TryItem(index) =
            if index < length then
                if stream.Position <> start + index then
                    stream.Seek(start + index, IO.SeekOrigin.Begin) |> ignore

                let buffer = Array.zeroCreate<byte> 1
                stream.Read(buffer, 0, 1) |> ignore
                ValueSome(buffer.[0])
            else
                ValueNone

        member _.SpanSlice(index, count) =
            if index < 0L then
                invalidArg "index" "Index must be non-negative."

            if count < 0 then
                invalidArg "count" "Count must be non-negative."

            if index > length then
                ReadOnlySpan<byte>.Empty
            else
                let length = min (int64 count) (length - (int64 index)) |> int

                if length > buffer.Length then
                    invalidArg "count" "Count must be less than or equal to buffer length."
                    ReadOnlySpan<byte>.Empty
                else
                    if stream.Position <> start + index then
                        stream.Seek(start + index, IO.SeekOrigin.Begin) |> ignore

                    let read = stream.Read(buffer, 0, length)
                    ReadOnlySpan(buffer).Slice(0, read)

        member _.Length = int64 length

        member _.Slice(newStart: int64, newLength: int64) =
            if newStart < 0L then
                invalidArg "newStart" "New start must be non-negative."

            if newLength < 0 then
                invalidArg "newLength" "New length must be non-negative."

            let newStart = start + newStart

            if newStart > stream.Length then
                ReadableStreamSlice(stream, start, 0, buffer)
            else
                let newLength = min newLength (stream.Length - newStart) |> int
                ReadableStreamSlice(stream, newStart, newLength, buffer)

[<Struct>]
type ReadableStream(stream: Stream, buffer: byte[]) =
    interface IReadable<byte, ReadableStreamSlice> with
        member _.TryItem(index) =
            if index < 0L then
                invalidArg "index" "Index must be non-negative."

            if index < stream.Length then
                if stream.Position <> index then
                    stream.Seek(index, IO.SeekOrigin.Begin) |> ignore

                let buffer = Array.zeroCreate<byte> 1
                stream.Read(buffer, 0, 1) |> ignore
                ValueSome(buffer.[0])
            else
                ValueNone

        member _.SpanSlice(index, count) =
            // TODO: ReadableStream could probably be an object instead of a struct
            // We should track what bytes are in the buffer and only read more if necessary
            if index < 0L then
                invalidArg "index" "Index must be non-negative."

            if count < 0 then
                invalidArg "count" "Count must be non-negative."

            if index > stream.Length then
                ReadOnlySpan<byte>.Empty
            else
                let length = min (int64 count) (stream.Length - index) |> int

                if length > buffer.Length then
                    invalidArg "count" "Count must be less than or equal to buffer length."
                    ReadOnlySpan<byte>.Empty
                else
                    if stream.Position <> index then
                        stream.Seek(index, IO.SeekOrigin.Begin) |> ignore

                    let read = stream.Read(buffer, 0, length)
                    ReadOnlySpan(buffer).Slice(0, read)

        member _.Length = stream.Length

        member _.Slice(newStart: int64, newLength: int64) =
            if newStart < 0L then
                invalidArg "newStart" "New start must be non-negative."

            if newLength < 0 then
                invalidArg "newLength" "New length must be non-negative."

            if newStart > stream.Length then
                ReadableStreamSlice(stream, 0, 0, buffer)
            else
                let newLength = min newLength (stream.Length - newStart)
                ReadableStreamSlice(stream, newStart, newLength, buffer)

[<Struct>]
type ReadableMemory<'T>(memory: ReadOnlyMemory<'T>) =
    interface IReadable<'T, ReadableMemory<'T>> with
        member _.TryItem(index) =
            if index < int64 memory.Length then
                ValueSome(memory.Span[int index])
            else
                ValueNone

        member _.SpanSlice(index, count) =
            if index < 0L then
                invalidArg "index" "Index must be non-negative."

            if count < 0 then
                invalidArg "count" "Count must be non-negative."

            if index > int64 memory.Length then
                ReadOnlySpan.Empty
            else
                let length = min count (memory.Length - int index)
                memory.Span.Slice(int index, length)

        member _.Length = int64 memory.Length

        member _.Slice(newStart: int64, newLength: int64) =
            if newStart < 0L then
                invalidArg "newStart" "New start must be non-negative."

            if newLength < 0 then
                invalidArg "newLength" "New length must be non-negative."

            if newStart > int64 memory.Length then
                ReadableMemory(memory.Slice(0, 0))
            else
                let newLength = min newLength (int64 memory.Length - newStart)
                ReadableMemory(memory.Slice(int newStart, int newLength))

    new(memory: Memory<'T>) = ReadableMemory(memory)
#endif

module Reader =
    let ofString (s: string) state = Reader(ReadableString s, state, 0L)
    let ofArray (a: 'T array) state = Reader(ReadableArray a, state, 0L)

    let ofImmutableArray (a: ImmutableArray<'T>) state =
        Reader(ReadableImmutableArray a, state, 0L)

#if NET5_0_OR_GREATER
    let ofResizeArray (a: ResizeArray<'T>) state =
        Reader(ReadableResizeArray a, state, 0L)
#endif

#if !FABLE_COMPILER
    let ofStream (stream: Stream) bufferSize state =
        Reader(ReadableStream(stream, Array.zeroCreate<byte> bufferSize), state, 0L)
#endif
