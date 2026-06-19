namespace Vesper.Collections

// Vesper.Seq struct-seq contract — the zero-allocation, struct-chaining
// `StructSeq` surface of `brainstorm-seq-module.md`, generic over the element
// type `'T`. The front-end symbol contract (parsed by XParsec.FSharp, walked into
// an IExternalSymbolProvider); the runtime impl is `struct-seq.fs`.
//
// This is the GRADUATION of the rung-3 inline slice (proven `int`-element in
// StructSeqTests.fs) into a real generic-over-`'T` library: the marker interfaces
// `IStructEnumerator<'T>` / `IStructSeq<'T, 'E>`, the `ArrayEnumerator<'T>` /
// `ArraySeq<'T>` entry pair, the `MapEnumerator<'E, 'T, 'U>` /
// `MapSeq<'S, 'E, 'T, 'U>` map node, and the `ofArray` / `map` / `fold` module
// surface. Functional arguments are `Vesper.Fun`s.

/// Duck-typed enumerator slot — `MoveNext`/`Current`, used only as a generic
/// constraint so a struct enumerator dispatches by value.
type IStructEnumerator<'T> =
    abstract member MoveNext: unit -> bool
    abstract member Current: 'T

/// A struct sequence tracking its concrete enumerator type `'E`, so the compiler
/// chains operations by value with full type information.
type IStructSeq<'T, 'E when 'E :> IStructEnumerator<'T>> =
    abstract member GetEnumerator: unit -> 'E

open System
open System.Collections
open System.Collections.Generic

/// A by-value enumerator over a `'T[]`. Implements the BCL enumerator interfaces
/// (§7.2 escape hatch) so it boxes transparently when handed to a .NET API.
[<Struct>]
type ArrayEnumerator<'T> =
    new: arr: 'T[] -> ArrayEnumerator<'T>
    interface IStructEnumerator<'T>
    interface IEnumerator<'T>
    interface IEnumerator
    interface IDisposable

/// Wraps a `'T[]` as a struct sequence (`StructSeq.ofArray`). Also implements
/// `IEnumerable<'T>` (§7.2 escape hatch).
[<Struct>]
type ArraySeq<'T> =
    new: arr: 'T[] -> ArraySeq<'T>
    interface IStructSeq<'T, ArrayEnumerator<'T>>
    interface IEnumerable<'T>
    interface IEnumerable

/// The enumerator of a mapped sequence — chains a generic inner enumerator `'E`
/// and applies `'T -> 'U` per element.
[<Struct>]
type MapEnumerator<'E, 'T, 'U when 'E :> IStructEnumerator<'T>> =
    new: source: 'E * f: ('T -> 'U) -> MapEnumerator<'E, 'T, 'U>
    interface IStructEnumerator<'U>
    interface IEnumerator<'U>
    interface IEnumerator
    interface IDisposable

/// The `Seq.map` node — a struct sequence over a struct source `'S`.
[<Struct>]
type MapSeq<'S, 'E, 'T, 'U when 'S :> IStructSeq<'T, 'E> and 'E :> IStructEnumerator<'T>> =
    new: source: 'S * f: ('T -> 'U) -> MapSeq<'S, 'E, 'T, 'U>
    interface IStructSeq<'U, MapEnumerator<'E, 'T, 'U>>
    interface IEnumerable<'U>
    interface IEnumerable

/// The struct-chaining `Seq` combinators of `brainstorm-seq-module.md`: each
/// builds a concrete generic struct (zero heap allocation); `fold` is the
/// consuming terminal that drives the pipeline as a single stack loop. The
/// `ModuleSuffix` representation gives the compiled name `StructSeqModule`.
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module StructSeq =

    /// Wraps a `'T[]` as a struct sequence to enter the pipeline.
    val ofArray: arr: 'T[] -> ArraySeq<'T>

    /// `map f source` builds a zero-allocation `MapSeq` node over `source`.
    val map:
        f: ('T -> 'U) ->
        source: 'S ->
            MapSeq<'S, 'E, 'T, 'U>
                when 'S :> IStructSeq<'T, 'E> and 'E :> IStructEnumerator<'T>

    /// `fold f seed source` threads `seed` through the sequence, applying `f` to
    /// the running state and each element. The consuming terminal.
    val fold:
        f: ('State -> 'T -> 'State) ->
        seed: 'State ->
        source: 'S ->
            'State when 'S :> IStructSeq<'T, 'E> and 'E :> IStructEnumerator<'T>
