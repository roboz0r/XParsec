[<AutoOpen>]
module ImmutableArrayCE

open System
open System.Collections.Immutable
open System.Runtime.InteropServices

type ImmutableArrayBuilder() =
    member inline _.Yield(x) =
        fun (b: ImmutableArray<'T>.Builder) -> b.Add(x)

    member inline _.YieldFrom(xs: 'T seq) =
        fun (b: ImmutableArray<'T>.Builder) -> b.AddRange(xs)

    member inline _.Combine([<InlineIfLambda>] f, [<InlineIfLambda>] g) =
        fun (b: ImmutableArray<'T>.Builder) ->
            f b
            g b

    member inline _.Delay([<InlineIfLambda>] f) =
        fun (builder: ImmutableArray<'T>.Builder) -> (f ()) builder

    member inline _.Zero() =
        fun (b: ImmutableArray<'T>.Builder) -> ()

    member inline __.For(xs: seq<'T>, [<InlineIfLambda>] f: 'T -> ImmutableArray<'T>.Builder -> unit) =
        fun (b: ImmutableArray<'T>.Builder) ->
            for x in xs do
                (f x) b

    member inline __.While
        ([<InlineIfLambda>] p: unit -> bool, [<InlineIfLambda>] f: ImmutableArray<'T>.Builder -> unit)
        =
        fun (b: ImmutableArray<'T>.Builder) ->
            while p () do
                f b

    member inline __.Run([<InlineIfLambda>] f) =
        let builder = ImmutableArray.CreateBuilder<'T>()
        f builder
        builder.ToImmutable()

type ImmutableArrayBuilder with

    member inline _.YieldFrom(xs: 'T array) =
        fun (b: ImmutableArray<'T>.Builder) -> b.AddRange(xs)

    member inline _.YieldFrom(xs: ResizeArray<'T>) =
        fun (b: ImmutableArray<'T>.Builder) -> b.AddRange(CollectionsMarshal.AsSpan xs)

    member inline _.YieldFrom(xs: ImmutableArray<'T>) =
        fun (b: ImmutableArray<'T>.Builder) -> b.AddRange(xs)

    member inline _.YieldFrom(xs: Memory<'T>) =
        fun (b: ImmutableArray<'T>.Builder) -> b.AddRange(xs.Span)

    member inline _.YieldFrom(xs: ReadOnlyMemory<'T>) =
        fun (b: ImmutableArray<'T>.Builder) -> b.AddRange(xs.Span)

    member inline __.For(xs: array<'T>, [<InlineIfLambda>] f: 'T -> ImmutableArray<'T>.Builder -> unit) =
        fun (b: ImmutableArray<'T>.Builder) ->
            for x in xs do
                (f x) b

    member inline __.For(xs: ImmutableArray<'T>, [<InlineIfLambda>] f: 'T -> ImmutableArray<'T>.Builder -> unit) =
        fun (b: ImmutableArray<'T>.Builder) ->
            for x in xs.AsSpan() do
                (f x) b

    member inline __.For(xs: ResizeArray<'T>, [<InlineIfLambda>] f: 'T -> ImmutableArray<'T>.Builder -> unit) =
        fun (b: ImmutableArray<'T>.Builder) ->
            for x in CollectionsMarshal.AsSpan xs do
                (f x) b

    member inline __.For(xs: Memory<'T>, [<InlineIfLambda>] f: 'T -> ImmutableArray<'T>.Builder -> unit) =
        fun (b: ImmutableArray<'T>.Builder) ->
            for x in xs.Span do
                (f x) b

    member inline __.For(xs: ReadOnlyMemory<'T>, [<InlineIfLambda>] f: 'T -> ImmutableArray<'T>.Builder -> unit) =
        fun (b: ImmutableArray<'T>.Builder) ->
            for x in xs.Span do
                (f x) b

let imm = ImmutableArrayBuilder()
