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
    let raiseIndexOutOfRange () = raise (IndexOutOfRangeException())

/// A string slice that can be read as input by the parser.
[<Struct>]
type ReadableString(s: string, start: int, length: int) =
    /// Construct a view over the whole string.
    new(s: string) = ReadableString(s, 0, s.Length)

    static member Empty: ReadableString = ReadableString(String.Empty, 0, 0)

    member _.Item
        with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get index =
            if uint index >= uint length then
                raiseIndexOutOfRange ()

            s.[start + index]

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member _.TryItem(index) =
        // uint cast safely checks 0 <= index < length
        if uint index < uint length then
            ValueSome(s.[start + index])
        else
            ValueNone

#if !FABLE_COMPILER
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member _.AsSpan() : ReadOnlySpan<char> = s.AsSpan(start, length)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member _.AsSpan(viewStart: int) : ReadOnlySpan<char> =
        if uint viewStart > uint length then
            raise (ArgumentOutOfRangeException(nameof viewStart))

        s.AsSpan(start + viewStart, length - viewStart)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member _.AsSpan(viewStart: int, viewLength: int) : ReadOnlySpan<char> =
        if uint viewStart > uint length then
            raise (ArgumentOutOfRangeException(nameof viewStart))

        if uint viewLength > uint (length - viewStart) then
            raise (ArgumentOutOfRangeException(nameof viewLength))

        s.AsSpan(start + viewStart, viewLength)
#else
    member _.AsSpan(?viewStart: int, ?viewLength: int) : ReadOnlySpan<char> =
        let viewStart = defaultArg viewStart 0
        let viewLength = defaultArg viewLength (length - viewStart)

        if uint viewStart > uint length then
            raise (ArgumentOutOfRangeException(nameof viewStart))

        if uint viewLength > uint (length - viewStart) then
            raise (ArgumentOutOfRangeException(nameof viewLength))

        s.AsSpan(start + viewStart, viewLength)
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
            ReadableString(s, start + length, 0)
        else
            // Clamp to the remaining length of THIS slice
            let safeLength = min newLength (length - newStart)
            ReadableString(s, start + newStart, safeLength)

    /// Materialises this view into a String. Allocates a fresh substring.
    override _.ToString() : string =
        if length = 0 then String.Empty
        else s.Substring(start, length)

    interface IReadable<char, ReadableString> with
        member this.Item
            with get index = this.Item index

        member this.TryItem(index) = this.TryItem index

#if !FABLE_COMPILER
        member this.AsSpan() = this.AsSpan()

        member this.AsSpan(start: int) = this.AsSpan(start)

        member this.AsSpan(start: int, length: int) = this.AsSpan(start, length)
#else
        member this.AsSpan(?start: int, ?length: int) =
            this.AsSpan(?viewStart = start, ?viewLength = length)
#endif

        member this.Length = this.Length

        member this.Slice(newStart: int, newLength: int) = this.Slice(newStart, newLength)

    interface IReadOnlyCollection<char> with
        member _.Count = length

    interface IReadOnlyList<char> with
        member _.Item
            with get (i: int) =
                if uint i >= uint length then
                    raiseIndexOutOfRange ()

                s.[start + i]

    interface IEnumerable<char> with
        member _.GetEnumerator() : IEnumerator<char> =
            let str = s
            let st = start
            let len = length
            let mutable i = 0

            { new IEnumerator<char> with
                member _.Current = str.[st + i - 1]
              interface System.Collections.IEnumerator with
                  member _.Current = box str.[st + i - 1]

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
            (this :> IEnumerable<char>).GetEnumerator() :> System.Collections.IEnumerator

/// An array slice that can be read as input by the parser.
[<Struct>]
type ReadableArray<'T>(arr: 'T array, start: int, length: int) =
    /// Construct a view over the whole array.
    new(arr: 'T array) = ReadableArray(arr, 0, arr.Length)

    static member Empty: ReadableArray<'T> = ReadableArray(Array.empty, 0, 0)

    member _.Item
        with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get index =
            if uint index >= uint length then
                raiseIndexOutOfRange ()

            arr.[start + index]

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member _.TryItem(index) =
        // uint cast safely checks 0 <= index < length
        if uint index < uint length then
            ValueSome(arr.[start + index])
        else
            ValueNone

#if !FABLE_COMPILER
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member _.AsSpan() : ReadOnlySpan<'T> = ReadOnlySpan<'T>(arr, start, length)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member _.AsSpan(viewStart: int) : ReadOnlySpan<'T> =
        if uint viewStart > uint length then
            raise (ArgumentOutOfRangeException(nameof viewStart))

        ReadOnlySpan<'T>(arr, start + viewStart, length - viewStart)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member _.AsSpan(viewStart: int, viewLength: int) : ReadOnlySpan<'T> =
        if uint viewStart > uint length then
            raise (ArgumentOutOfRangeException(nameof viewStart))

        if uint viewLength > uint (length - viewStart) then
            raise (ArgumentOutOfRangeException(nameof viewLength))

        ReadOnlySpan<'T>(arr, start + viewStart, viewLength)
#else
    member _.AsSpan(?viewStart: int, ?viewLength: int) : ReadOnlySpan<'T> =
        let viewStart = defaultArg viewStart 0
        let viewLength = defaultArg viewLength (length - viewStart)

        if uint viewStart > uint length then
            raise (ArgumentOutOfRangeException(nameof viewStart))

        if uint viewLength > uint (length - viewStart) then
            raise (ArgumentOutOfRangeException(nameof viewLength))

        ArraySpan<'T>(arr, start + viewStart, viewLength) :> ReadOnlySpan<'T>
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

    /// Materialises this view into an ImmutableArray. Escape hatch for consumers
    /// that specifically need an ImmutableArray; allocates a fresh copy.
    member _.ToImmutableArray() : ImmutableArray<'T> =
        if length = 0 then
            ImmutableArray<'T>.Empty
        else
            ImmutableArray.Create(arr, start, length)

    interface IReadable<'T, ReadableArray<'T>> with
        member this.Item
            with get index = this.Item index

        member this.TryItem(index) = this.TryItem index

#if !FABLE_COMPILER
        member this.AsSpan() = this.AsSpan()

        member this.AsSpan(start: int) = this.AsSpan(start)

        member this.AsSpan(start: int, length: int) = this.AsSpan(start, length)
#else
        member this.AsSpan(?start: int, ?length: int) =
            this.AsSpan(?viewStart = start, ?viewLength = length)
#endif

        member this.Length = this.Length

        member this.Slice(newStart: int, newLength: int) = this.Slice(newStart, newLength)

    interface IReadOnlyCollection<'T> with
        member _.Count = length

    interface IReadOnlyList<'T> with
        member _.Item
            with get (i: int) =
                if uint i >= uint length then
                    raiseIndexOutOfRange ()

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
                raiseIndexOutOfRange ()

            arr.[index]
        and [<MethodImpl(MethodImplOptions.AggressiveInlining)>] set (index: int) (value: 'T) =
            if uint index >= uint count then
                raiseIndexOutOfRange ()

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
    /// Always zero-copy: parsing consumers typically work with reasonably-sized
    /// buffers and the result is one-shot, so retaining some unused capacity is
    /// cheaper than allocating a fresh array + copy (which also punishes LOH-sized
    /// buffers by round-tripping through the Gen2 heap).
    /// Single-shot: releases the builder's reference — do not reuse.
    member _.ToReadableArray() : ReadableArray<'T> =
        let result =
            if count = 0 then
                ReadableArray<'T>.Empty
            else
                ReadableArray(arr, 0, count)

        arr <- Array.empty
        count <- 0
        result


/// An immutable array slice that can be read as input by the parser.
[<Struct>]
type ReadableImmutableArray<'T>(arr: ImmutableArray<'T>, start: int, length: int) =
    /// Construct a view over the whole immutable array.
    new(arr: ImmutableArray<'T>) = ReadableImmutableArray(arr, 0, arr.Length)

    static member Empty: ReadableImmutableArray<'T> = ReadableImmutableArray(ImmutableArray<'T>.Empty, 0, 0)

    member _.Item
        with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get index =
            if uint index >= uint length then
                raiseIndexOutOfRange ()

            arr.[start + index]

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member _.TryItem(index) =
        // uint cast safely checks 0 <= index < length
        if uint index < uint length then
            ValueSome(arr.[start + index])
        else
            ValueNone

#if !FABLE_COMPILER
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member _.AsSpan() : ReadOnlySpan<'T> = arr.AsSpan(start, length)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member _.AsSpan(viewStart: int) : ReadOnlySpan<'T> =
        if uint viewStart > uint length then
            raise (ArgumentOutOfRangeException(nameof viewStart))

        arr.AsSpan(start + viewStart, length - viewStart)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member _.AsSpan(viewStart: int, viewLength: int) : ReadOnlySpan<'T> =
        if uint viewStart > uint length then
            raise (ArgumentOutOfRangeException(nameof viewStart))

        if uint viewLength > uint (length - viewStart) then
            raise (ArgumentOutOfRangeException(nameof viewLength))

        arr.AsSpan(start + viewStart, viewLength)
#else
    member _.AsSpan(?viewStart: int, ?viewLength: int) : ReadOnlySpan<'T> =
        let viewStart = defaultArg viewStart 0
        let viewLength = defaultArg viewLength (length - viewStart)

        if uint viewStart > uint length then
            raise (ArgumentOutOfRangeException(nameof viewStart))

        if uint viewLength > uint (length - viewStart) then
            raise (ArgumentOutOfRangeException(nameof viewLength))

        arr.AsSpan(start + viewStart, viewLength)
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
            ReadableImmutableArray(arr, start + length, 0)
        else
            // Clamp to the remaining length of THIS slice
            let safeLength = min newLength (length - newStart)
            ReadableImmutableArray(arr, start + newStart, safeLength)

    /// Materialises this view into an ImmutableArray. Zero-copy when the view
    /// covers the whole backing array; otherwise allocates a fresh copy.
    member _.ToImmutableArray() : ImmutableArray<'T> =
        if length = 0 then
            ImmutableArray<'T>.Empty
        elif start = 0 && length = arr.Length then
            arr
        else
            let builder = ImmutableArray.CreateBuilder<'T>(length)

            for i in 0 .. length - 1 do
                builder.Add(arr.[start + i])

            builder.MoveToImmutable()

    interface IReadable<'T, ReadableImmutableArray<'T>> with
        member this.Item
            with get index = this.Item index

        member this.TryItem(index) = this.TryItem index

#if !FABLE_COMPILER
        member this.AsSpan() = this.AsSpan()

        member this.AsSpan(start: int) = this.AsSpan(start)

        member this.AsSpan(start: int, length: int) = this.AsSpan(start, length)
#else
        member this.AsSpan(?start: int, ?length: int) =
            this.AsSpan(?viewStart = start, ?viewLength = length)
#endif

        member this.Length = this.Length

        member this.Slice(newStart: int, newLength: int) = this.Slice(newStart, newLength)

    interface IReadOnlyCollection<'T> with
        member _.Count = length

    interface IReadOnlyList<'T> with
        member _.Item
            with get (i: int) =
                if uint i >= uint length then
                    raiseIndexOutOfRange ()

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


#if NET5_0_OR_GREATER // No good way to get a span from a ResizeArray in .NET Standard 2.0
/// A ResizeArray slice that can be read as input by the parser.
[<Struct>]
type ReadableResizeArray<'T>(arr: ResizeArray<'T>, start: int, length: int) =
    /// Construct a view over the whole ResizeArray.
    new(arr: ResizeArray<'T>) = ReadableResizeArray(arr, 0, arr.Count)

    static member Empty: ReadableResizeArray<'T> = ReadableResizeArray(ResizeArray<'T>(0), 0, 0)

    member _.Item
        with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get index =
            if uint index >= uint length then
                raiseIndexOutOfRange ()

            arr.[start + index]

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member _.TryItem(index) =
        // uint cast safely checks 0 <= index < length
        if uint index < uint length then
            ValueSome(arr.[start + index])
        else
            ValueNone

#if !FABLE_COMPILER
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member _.AsSpan() : ReadOnlySpan<'T> =
        let span = CollectionsMarshal.AsSpan(arr)
        Span.op_Implicit (span.Slice(start, length))

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member _.AsSpan(viewStart: int) : ReadOnlySpan<'T> =
        if uint viewStart > uint length then
            raise (ArgumentOutOfRangeException(nameof viewStart))

        let span = CollectionsMarshal.AsSpan(arr)
        Span.op_Implicit (span.Slice(start + viewStart, length - viewStart))

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member _.AsSpan(viewStart: int, viewLength: int) : ReadOnlySpan<'T> =
        if uint viewStart > uint length then
            raise (ArgumentOutOfRangeException(nameof viewStart))

        if uint viewLength > uint (length - viewStart) then
            raise (ArgumentOutOfRangeException(nameof viewLength))

        let span = CollectionsMarshal.AsSpan(arr)
        Span.op_Implicit (span.Slice(start + viewStart, viewLength))
#else
    member _.AsSpan(?viewStart: int, ?viewLength: int) : ReadOnlySpan<'T> =
        let viewStart = defaultArg viewStart 0
        let viewLength = defaultArg viewLength (length - viewStart)

        if uint viewStart > uint length then
            raise (ArgumentOutOfRangeException(nameof viewStart))

        if uint viewLength > uint (length - viewStart) then
            raise (ArgumentOutOfRangeException(nameof viewLength))

        ResizeArraySpan<'T>(arr, start + viewStart, viewLength) :> ReadOnlySpan<'T>
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
            ReadableResizeArray(arr, start + length, 0)
        else
            // Clamp to the remaining length of THIS slice
            let safeLength = min newLength (length - newStart)
            ReadableResizeArray(arr, start + newStart, safeLength)

    /// Materialises this view into an ImmutableArray. Allocates a fresh copy.
    member _.ToImmutableArray() : ImmutableArray<'T> =
        if length = 0 then
            ImmutableArray<'T>.Empty
        else
            let builder = ImmutableArray.CreateBuilder<'T>(length)

            for i in 0 .. length - 1 do
                builder.Add(arr.[start + i])

            builder.MoveToImmutable()

    interface IReadable<'T, ReadableResizeArray<'T>> with
        member this.Item
            with get index = this.Item index

        member this.TryItem(index) = this.TryItem index

#if !FABLE_COMPILER
        member this.AsSpan() = this.AsSpan()

        member this.AsSpan(start: int) = this.AsSpan(start)

        member this.AsSpan(start: int, length: int) = this.AsSpan(start, length)
#else
        member this.AsSpan(?start: int, ?length: int) =
            this.AsSpan(?viewStart = start, ?viewLength = length)
#endif

        member this.Length = this.Length

        member this.Slice(newStart: int, newLength: int) = this.Slice(newStart, newLength)

    interface IReadOnlyCollection<'T> with
        member _.Count = length

    interface IReadOnlyList<'T> with
        member _.Item
            with get (i: int) =
                if uint i >= uint length then
                    raiseIndexOutOfRange ()

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
#endif

#if !FABLE_COMPILER
/// A memory slice that can be read as input by the parser.
[<Struct>]
type ReadableMemory<'T>(memory: ReadOnlyMemory<'T>) =
    new(memory: Memory<'T>) = ReadableMemory(memory)

    static member Empty: ReadableMemory<'T> = ReadableMemory(ReadOnlyMemory<'T>.Empty)

    member _.Item
        with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get index =
            if uint index >= uint memory.Length then
                raiseIndexOutOfRange ()

            memory.Span[index]

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member _.TryItem(index) =
        if uint index < uint memory.Length then
            ValueSome(memory.Span[index])
        else
            ValueNone

    /// `ReadOnlyMemory<'T>.Span.Slice` already enforces BCL bounds — we delegate
    /// directly. The interface validation contract is upheld by that BCL throw.
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member _.AsSpan() : ReadOnlySpan<'T> = memory.Span

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member _.AsSpan(viewStart: int) : ReadOnlySpan<'T> = memory.Span.Slice(viewStart)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member _.AsSpan(viewStart: int, viewLength: int) : ReadOnlySpan<'T> =
        memory.Span.Slice(viewStart, viewLength)

    member _.Length
        with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get () = memory.Length

    member _.IsEmpty
        with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get () = memory.IsEmpty

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
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

    /// Materialises this view into an ImmutableArray. Allocates a fresh copy.
    member _.ToImmutableArray() : ImmutableArray<'T> =
        let len = memory.Length

        if len = 0 then
            ImmutableArray<'T>.Empty
        else
            let span = memory.Span
            let builder = ImmutableArray.CreateBuilder<'T>(len)

            for i in 0 .. len - 1 do
                builder.Add(span[i])

            builder.MoveToImmutable()

    interface IReadable<'T, ReadableMemory<'T>> with
        member this.Item
            with get index = this.Item index

        member this.TryItem(index) = this.TryItem index

        member this.AsSpan() = this.AsSpan()

        member this.AsSpan(start: int) = this.AsSpan(start)

        member this.AsSpan(start: int, length: int) = this.AsSpan(start, length)

        member this.Length = this.Length

        member this.Slice(newStart: int, newLength: int) = this.Slice(newStart, newLength)

    interface IReadOnlyCollection<'T> with
        member _.Count = memory.Length

    interface IReadOnlyList<'T> with
        member _.Item
            with get (i: int) =
                if uint i >= uint memory.Length then
                    raiseIndexOutOfRange ()

                memory.Span[i]

    interface IEnumerable<'T> with
        member _.GetEnumerator() : IEnumerator<'T> =
            let m = memory
            let len = m.Length
            let mutable i = 0

            { new IEnumerator<'T> with
                member _.Current = m.Span[i - 1]
              interface System.Collections.IEnumerator with
                  member _.Current = box m.Span[i - 1]

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

#if NET5_0_OR_GREATER
    /// Creates a new reader from the input resize array and state.
    let ofResizeArray (a: ResizeArray<'T>) state =
        Reader(ReadableResizeArray(a, 0, a.Count), state, 0)
#endif

#if !FABLE_COMPILER
    /// Creates a new reader from the input memory and state.
    let ofMemory (m: ReadOnlyMemory<'T>) state = Reader(ReadableMemory m, state, 0)
#endif
