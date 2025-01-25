namespace XParsec

// This file contains types to allow the XParsec library to be compiled with Fable.
// API compatibility is maintained with the original types but not necessarily with an efficient implementation.

#if FABLE_COMPILER
type ImmutableArrayBuilder<'T> = ResizeArray<'T>
#else
type ImmutableArrayBuilder<'T> = System.Collections.Immutable.ImmutableArray<'T>.Builder
#endif

#if FABLE_COMPILER
namespace System

open System.Runtime.CompilerServices

type ReadOnlySpan<'T> =
    abstract Length: int
    abstract Slice: newStart: int * newLength: int -> ReadOnlySpan<'T>
    abstract Item: int -> 'T with get
    abstract IsEmpty: bool
    abstract ToArray: unit -> 'T array

type Span<'T> =
    inherit ReadOnlySpan<'T>

type ArraySpan<'T>(array: 'T array, start: int, length: int) =
    interface Span<'T> with
        member _.Length = length

        member _.Slice(newStart, newLength) =
            ArraySpan(array, start + newStart, newLength)

        member _.Item
            with get (i) = array.[start + i]

        member _.IsEmpty = length = 0

        member _.ToArray() =
            Array.init length (fun i -> array.[start + i])

type ResizeArraySpan<'T>(array: ResizeArray<'T>, start: int, length: int) =
    interface Span<'T> with
        member _.Length = length

        member _.Slice(newStart, newLength) =
            ResizeArraySpan(array, start + newStart, newLength)

        member _.Item
            with get (i) = array.[start + i]

        member _.IsEmpty = length = 0

        member _.ToArray() =
            Array.init length (fun i -> array.[start + i])

type StringSpan(string: string, start: int, length: int) =
    interface ReadOnlySpan<char> with
        member _.Length = length

        member _.Slice(newStart, newLength) =
            StringSpan(string, start + newStart, newLength)

        member _.Item
            with get (i) = string[start + i]

        member _.IsEmpty = length = 0

        member _.ToArray() =
            Array.init length (fun i -> string[start + i])
#endif

#if FABLE_COMPILER
namespace System.Collections.Immutable

open System

[<StructuralEquality; StructuralComparison>]
type ImmutableArray<'T> =
    internal
        {
            // Using a record as the simplest way to get structural equality
            Array: 'T array
        }

    member this.Item
        with get (i) = this.Array.[i]

    member this.Length = this.Array.Length

    member this.AsSpan() =
        ArraySpan<'T>(this.Array, 0, this.Length)

    static member Empty: ImmutableArray<'T> = { Array = Array.empty<'T> }

    interface System.Collections.Generic.IEnumerable<'T> with
        member this.GetEnumerator() : System.Collections.Generic.IEnumerator<'T> =
            (this.Array :> System.Collections.Generic.IEnumerable<'T>).GetEnumerator()

        member this.GetEnumerator() : System.Collections.IEnumerator =
            (this.Array :> System.Collections.Generic.IEnumerable<'T>).GetEnumerator()

type ImmutableArray =
    static member inline CreateRange<'T>(xs: 'T seq) = { Array = Array.ofSeq<'T> xs }
    static member inline CreateBuilder<'T>() = ResizeArray<'T>()
    static member inline CreateBuilder<'T>(initialCapacity: int) = ResizeArray<'T>(initialCapacity)
#endif

#if FABLE_COMPILER
namespace System

open System.Runtime.CompilerServices
open System.Collections.Immutable

[<Extension>]
type MemoryExtensions =
    static member SequenceEqual<'T when 'T: equality>(x: ReadOnlySpan<'T>, y: ReadOnlySpan<'T>) =
        if x.Length <> y.Length then
            false
        else
            let rec f i =
                if i = x.Length then true
                elif x[i] = y[i] then f (i + 1)
                else false

            f 0

    static member Equals(x: ReadOnlySpan<char>, y: ReadOnlySpan<char>, comparison: System.StringComparison) =
        let sx = System.String(x.ToArray())
        let sy = System.String(y.ToArray())
        sx.Equals(sy, comparison)

    [<Extension>]
    static member inline AsSpan<'T>(array: 'T array) : Span<'T> = ArraySpan(array, 0, array.Length)

    [<Extension>]
    static member inline AsSpan<'T>(array: 'T array, start: int, length: int) : Span<'T> =
        ArraySpan(array, start, length)

    [<Extension>]
    static member inline AsSpan(string: string) : ReadOnlySpan<char> = StringSpan(string, 0, string.Length)

    [<Extension>]
    static member inline AsSpan(string: string, start: int, length: int) : ReadOnlySpan<char> =
        StringSpan(string, start, length)

    [<Extension>]
    static member inline AsSpan<'T>(array: ImmutableArray<'T>) : ReadOnlySpan<'T> =
        ArraySpan(array.Array, 0, array.Length)

    [<Extension>]
    static member inline AsSpan<'T>(array: ImmutableArray<'T>, start: int, length: int) : ReadOnlySpan<'T> =
        ArraySpan(array.Array, start, length)

[<AutoOpen>]
module Extensions =
    type ReadOnlySpan<'T> with
        static member Empty: ReadOnlySpan<'T> = ArraySpan<'T>([||], 0, 0) :> ReadOnlySpan<'T>

    type System.Collections.Generic.List<'T> with
        member inline this.ToImmutable() = { Array = this.ToArray() }

        member inline this.MoveToImmutable() =
            let xs = this.ToArray()
            this.Clear()
            { Array = xs }

    type ``[]``<'T> with
        member inline this.ToImmutableArray() = { Array = Array.copy this }
#endif
