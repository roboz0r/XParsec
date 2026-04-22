namespace XParsec

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices
#if !FABLE_COMPILER
open System.Runtime.InteropServices
#endif

/// Stack-allocated builder for small ImmutableArray results (<= 4 items).
/// Holds four inline slots and spills to an ImmutableArray.Builder on the 5th item.
/// ToImmutable() dispatches to the exact-size ImmutableArray.Create overload for
/// small counts, avoiding both the Builder allocation and the size-fitting copy.
///
/// Use as a mutable local:
///     let mutable xs = SmallArrayBuilder<'T>()
///     xs.Add(item)
///     ...
///     xs.ToImmutable()
///
/// Single-shot: do not reuse after ToImmutable().
/// IsByRefLike forces the struct to stay on the stack — it can't be boxed, captured in
/// a closure, stored in a heap field, or cross an async boundary. This prevents the
/// silent-copy bugs typical of mutable value types.
[<Struct; IsByRefLike>]
type SmallArrayBuilder<'T> =
    [<DefaultValue(false)>]
    val mutable private count: int

    [<DefaultValue(false)>]
    val mutable private t0: 'T

    [<DefaultValue(false)>]
    val mutable private t1: 'T

    [<DefaultValue(false)>]
    val mutable private t2: 'T

    [<DefaultValue(false)>]
    val mutable private t3: 'T

    [<DefaultValue(false)>]
    val mutable private overflow: ImmutableArrayBuilder<'T>

    member this.Count = this.count

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.Add(item: 'T) =
        match this.count with
        | 0 ->
            this.t0 <- item
            this.count <- 1
        | 1 ->
            this.t1 <- item
            this.count <- 2
        | 2 ->
            this.t2 <- item
            this.count <- 3
        | 3 ->
            this.t3 <- item
            this.count <- 4
        | 4 ->
            let b = ImmutableArray.CreateBuilder<'T>(8)
            b.Add(this.t0)
            b.Add(this.t1)
            b.Add(this.t2)
            b.Add(this.t3)
            b.Add(item)
            this.overflow <- b
            this.count <- 5
        | _ ->
            this.overflow.Add(item)
            this.count <- this.count + 1

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.ToImmutable() : ImmutableArray<'T> =
        match this.count with
        | 0 -> ImmutableArray.Empty
        | 1 -> ImmutableArray.Create(this.t0)
        | 2 -> ImmutableArray.Create(this.t0, this.t1)
        | 3 -> ImmutableArray.Create(this.t0, this.t1, this.t2)
        | 4 -> ImmutableArray.Create(this.t0, this.t1, this.t2, this.t3)
        | _ -> this.overflow.ToImmutable()

type ImmutableArrayCE() =
    member inline _.Yield(x) =
        fun (b: ImmutableArrayBuilder<'T>) -> b.Add(x)

    member inline _.YieldFrom(xs: 'T seq) =
        fun (b: ImmutableArrayBuilder<'T>) -> b.AddRange(xs)

    member inline _.Combine([<InlineIfLambda>] f, [<InlineIfLambda>] g) =
        fun (b: ImmutableArrayBuilder<'T>) ->
            f b
            g b

    member inline _.Delay([<InlineIfLambda>] f) =
        fun (builder: ImmutableArrayBuilder<'T>) -> (f ()) builder

    member inline _.Zero() =
        fun (b: ImmutableArrayBuilder<'T>) -> ()

    member inline __.For(xs: seq<'T>, [<InlineIfLambda>] f: 'T -> ImmutableArrayBuilder<'T> -> unit) =
        fun (b: ImmutableArrayBuilder<'T>) ->
            for x in xs do
                (f x) b

    member inline __.While
        ([<InlineIfLambda>] p: unit -> bool, [<InlineIfLambda>] f: ImmutableArrayBuilder<'T> -> unit)
        =
        fun (b: ImmutableArrayBuilder<'T>) ->
            while p () do
                f b

    member inline __.Run([<InlineIfLambda>] f) =
        let builder = ImmutableArray.CreateBuilder<'T>()
        f builder
        builder.ToImmutable()

type ImmutableArrayCE with

    member inline _.YieldFrom(xs: 'T array) =
        fun (b: ImmutableArrayBuilder<'T>) -> b.AddRange(xs)

    member inline _.YieldFrom(xs: ResizeArray<'T>) =
        fun (b: ImmutableArrayBuilder<'T>) ->
#if NET5_0_OR_GREATER && !FABLE_COMPILER
            b.AddRange(CollectionsMarshal.AsSpan xs)
#else
            b.AddRange(xs)
#endif
    member inline _.YieldFrom(xs: ImmutableArray<'T>) =
        fun (b: ImmutableArrayBuilder<'T>) -> b.AddRange(xs)

#if !FABLE_COMPILER
    member inline _.YieldFrom(xs: Memory<'T>) =
        fun (b: ImmutableArrayBuilder<'T>) -> b.AddRange(xs.Span)

    member inline _.YieldFrom(xs: ReadOnlyMemory<'T>) =
        fun (b: ImmutableArrayBuilder<'T>) -> b.AddRange(xs.Span)
#endif

    member inline __.For(xs: array<'T>, [<InlineIfLambda>] f: 'T -> ImmutableArrayBuilder<'T> -> unit) =
        fun (b: ImmutableArrayBuilder<'T>) ->
            for x in xs do
                (f x) b

#if !FABLE_COMPILER
    member inline __.For(xs: ImmutableArray<'T>, [<InlineIfLambda>] f: 'T -> ImmutableArrayBuilder<'T> -> unit) =
        fun (b: ImmutableArrayBuilder<'T>) ->
            for x in xs.AsSpan() do
                (f x) b
#endif

    member inline __.For(xs: ResizeArray<'T>, [<InlineIfLambda>] f: 'T -> ImmutableArrayBuilder<'T> -> unit) =
        fun (b: ImmutableArrayBuilder<'T>) ->
#if NET5_0_OR_GREATER && !FABLE_COMPILER
            for x in CollectionsMarshal.AsSpan xs do
                (f x) b
#else
            for x in xs do
                (f x) b
#endif

#if !FABLE_COMPILER
    member inline __.For(xs: Memory<'T>, [<InlineIfLambda>] f: 'T -> ImmutableArrayBuilder<'T> -> unit) =
        fun (b: ImmutableArrayBuilder<'T>) ->
            for x in xs.Span do
                (f x) b

    member inline __.For(xs: ReadOnlyMemory<'T>, [<InlineIfLambda>] f: 'T -> ImmutableArrayBuilder<'T> -> unit) =
        fun (b: ImmutableArrayBuilder<'T>) ->
            for x in xs.Span do
                (f x) b
#endif
[<AutoOpen>]
module Builders =
    let imm = ImmutableArrayCE()
