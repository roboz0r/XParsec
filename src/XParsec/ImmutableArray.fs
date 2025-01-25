namespace XParsec

open System
open System.Collections.Immutable
#if !FABLE_COMPILER
open System.Runtime.InteropServices
#endif

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
