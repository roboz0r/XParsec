namespace Vesper.Collections

open System
open System.Collections
open System.Collections.Generic

// Marker interfaces, used ONLY as generic constraints (never as a variable
// type). `IStructSeq` tracks the concrete enumerator `'E` so the compiler chains
// by value; `IStructEnumerator` is the duck-typed `MoveNext`/`Current` slot.
type IStructEnumerator<'T> =
    abstract member MoveNext: unit -> bool
    abstract member Current: 'T

type IStructSeq<'T, 'E when 'E :> IStructEnumerator<'T>> =
    abstract member GetEnumerator: unit -> 'E

[<Struct>]
type ArrayEnumerator<'T> =
    val Arr: 'T[]
    val mutable Idx: int
    new(arr: 'T[]) = { Arr = arr; Idx = -1 }

    interface IStructEnumerator<'T> with
        member this.MoveNext() : bool =
            this.Idx <- this.Idx + 1
            this.Idx < this.Arr.Length

        member this.Current: 'T = this.Arr.[this.Idx]

    // §7.2 escape hatch: implement the BCL enumerator interfaces so the struct
    // boxes transparently when handed to a standard .NET API.
    interface IEnumerator<'T> with
        member this.Current: 'T = this.Arr.[this.Idx]

    interface IEnumerator with
        member this.MoveNext() : bool =
            this.Idx <- this.Idx + 1
            this.Idx < this.Arr.Length

        member this.Current: obj = box (this.Arr.[this.Idx])
        member this.Reset() : unit = ()

    interface IDisposable with
        member this.Dispose() : unit = ()

[<Struct>]
type ArraySeq<'T> =
    val Arr: 'T[]
    new(arr: 'T[]) = { Arr = arr }

    interface IStructSeq<'T, ArrayEnumerator<'T>> with
        member this.GetEnumerator() : ArrayEnumerator<'T> = ArrayEnumerator<'T>(this.Arr)

    interface IEnumerable<'T> with
        member this.GetEnumerator() : IEnumerator<'T> = (ArrayEnumerator<'T>(this.Arr) :> IEnumerator<'T>)

    interface IEnumerable with
        member this.GetEnumerator() : IEnumerator = (ArrayEnumerator<'T>(this.Arr) :> IEnumerator)

[<Struct>]
type MapEnumerator<'E, 'TFunc, 'T, 'U when 'E :> IStructEnumerator<'T> and 'TFunc :> Fun<'T, 'U>> =
    val mutable Source: 'E
    val F: 'TFunc
    new(source: 'E, f: 'TFunc) = { Source = source; F = f }

    interface IStructEnumerator<'U> with
        member this.MoveNext() : bool = this.Source.MoveNext()
        member this.Current: 'U = this.F.Invoke(this.Source.Current)

    interface IEnumerator<'U> with
        member this.Current: 'U = this.F.Invoke(this.Source.Current)

    interface IEnumerator with
        member this.MoveNext() : bool = this.Source.MoveNext()
        member this.Current: obj = box (this.F.Invoke(this.Source.Current))
        member this.Reset() : unit = ()

    interface IDisposable with
        member this.Dispose() : unit = ()

[<Struct>]
type MapSeq<'S, 'E, 'TFunc, 'T, 'U when 'S :> IStructSeq<'T, 'E> and 'E :> IStructEnumerator<'T> and 'TFunc :> Fun<'T, 'U>>
    =
    val Source: 'S
    val F: 'TFunc
    new(source: 'S, f: 'TFunc) = { Source = source; F = f }

    interface IStructSeq<'U, MapEnumerator<'E, 'TFunc, 'T, 'U>> with
        member this.GetEnumerator() : MapEnumerator<'E, 'TFunc, 'T, 'U> =
            MapEnumerator<'E, 'TFunc, 'T, 'U>(this.Source.GetEnumerator(), this.F)

    interface IEnumerable<'U> with
        member this.GetEnumerator() : IEnumerator<'U> =
            (MapEnumerator<'E, 'TFunc, 'T, 'U>(this.Source.GetEnumerator(), this.F) :> IEnumerator<'U>)

    interface IEnumerable with
        member this.GetEnumerator() : IEnumerator =
            (MapEnumerator<'E, 'TFunc, 'T, 'U>(this.Source.GetEnumerator(), this.F) :> IEnumerator)

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module StructSeq =

    let ofArray (arr: 'T[]) : ArraySeq<'T> = ArraySeq<'T>(arr)

    let map
        (f: 'TFunc when 'TFunc :> Fun<'T, 'U>)
        (source: 'S when 'S :> IStructSeq<'T, 'E> and 'E :> IStructEnumerator<'T>)
        : MapSeq<'S, 'E, 'TFunc, 'T, 'U> =
        MapSeq<'S, 'E, 'TFunc, 'T, 'U>(source, f)

    let fold
        (f: 'TFunc when 'TFunc :> Fun<'State, 'T, 'State>)
        (seed: 'State)
        (source: 'S when 'S :> IStructSeq<'T, 'E> and 'E :> IStructEnumerator<'T>)
        : 'State =
        let mutable state = seed

        for y in source do
            state <- f.Invoke(state, y)

        state
