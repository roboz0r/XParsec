namespace Vesper.Collections

// Runtime implementation target for this repo's own backend — the
// zero-allocation, struct-chaining `StructSeq` surface of
// `brainstorm-seq-module.md`, generic over the element type `'T`. The contract
// lives in `struct-seq.fsi`. BCL-only — no `FSharp.Core`.
//
// This is the GRADUATION of the rung-3 inline slice (proven `int`-element in
// `test/XParsec.FSharp.Codegen.Clr.Tests/StructSeqTests.fs`) into a real,
// generic-over-`'T` library:
//   - `IStructEnumerator<'T>` / `IStructSeq<'T, 'E>` marker interfaces (used only
//     as generic constraints, never as variable types), so the compiler tracks
//     the concrete enumerator type and chains by value.
//   - `ArrayEnumerator<'T>` / `ArraySeq<'T>` — entering the pipeline from `'T[]`.
//   - `MapEnumerator<'E, 'T, 'U>` / `MapSeq<'S, 'E, 'T, 'U>` — the `Seq.map` node;
//     a generic struct enumerator chaining a generic inner enumerator `'E` via
//     `constrained. !E callvirt`.
//   - `ofArray` / `map` / `fold` — the entry, the combinator, and the consuming
//     terminal (`fold` drives `for y in s` and threads a state accumulator,
//     applying a `Vesper.Fun` per element).
//
// The functional arguments are `Vesper.Fun`s (reference-type closures for now —
// rung 4 flips them to struct closures); each application lowers to
// `callvirt Fun::Invoke`.

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
type MapEnumerator<'E, 'T, 'U when 'E :> IStructEnumerator<'T>> =
    val mutable Source: 'E
    val F: 'T -> 'U
    new(source: 'E, f: 'T -> 'U) = { Source = source; F = f }

    interface IStructEnumerator<'U> with
        member this.MoveNext() : bool = this.Source.MoveNext()
        member this.Current: 'U = this.F(this.Source.Current)

    interface IEnumerator<'U> with
        member this.Current: 'U = this.F(this.Source.Current)

    interface IEnumerator with
        member this.MoveNext() : bool = this.Source.MoveNext()
        member this.Current: obj = box (this.F(this.Source.Current))
        member this.Reset() : unit = ()

    interface IDisposable with
        member this.Dispose() : unit = ()

[<Struct>]
type MapSeq<'S, 'E, 'T, 'U when 'S :> IStructSeq<'T, 'E> and 'E :> IStructEnumerator<'T>> =
    val Source: 'S
    val F: 'T -> 'U
    new(source: 'S, f: 'T -> 'U) = { Source = source; F = f }

    interface IStructSeq<'U, MapEnumerator<'E, 'T, 'U>> with
        member this.GetEnumerator() : MapEnumerator<'E, 'T, 'U> =
            MapEnumerator<'E, 'T, 'U>(this.Source.GetEnumerator(), this.F)

    interface IEnumerable<'U> with
        member this.GetEnumerator() : IEnumerator<'U> =
            (MapEnumerator<'E, 'T, 'U>(this.Source.GetEnumerator(), this.F) :> IEnumerator<'U>)

    interface IEnumerable with
        member this.GetEnumerator() : IEnumerator =
            (MapEnumerator<'E, 'T, 'U>(this.Source.GetEnumerator(), this.F) :> IEnumerator)

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module StructSeq =

    let ofArray (arr: 'T[]) : ArraySeq<'T> = ArraySeq<'T>(arr)

    let map (f: 'T -> 'U) (source: 'S when 'S :> IStructSeq<'T, 'E> and 'E :> IStructEnumerator<'T>) : MapSeq<'S, 'E, 'T, 'U> =
        MapSeq<'S, 'E, 'T, 'U>(source, f)

    let fold
        (f: 'State -> 'T -> 'State)
        (seed: 'State)
        (source: 'S when 'S :> IStructSeq<'T, 'E> and 'E :> IStructEnumerator<'T>)
        : 'State =
        let mutable state = seed

        for y in source do
            state <- f state y

        state
