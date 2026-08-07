namespace Vesper.Collections

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

/// A by-value enumerator over a `'T[]`. Also implements the BCL enumerator
/// interfaces, so it boxes transparently when handed to a .NET API.
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
/// and applies the struct closure `'TFunc :> Fun<'T, 'U>` per element via
/// `constrained. !TFunc callvirt Fun::Invoke`.
[<Struct>]
type MapEnumerator<'E, 'TFunc, 'T, 'U when 'E :> IStructEnumerator<'T> and 'TFunc :> Fun<'T, 'U>> =
    new: source: 'E * f: 'TFunc -> MapEnumerator<'E, 'TFunc, 'T, 'U>
    interface IStructEnumerator<'U>
    interface IEnumerator<'U>
    interface IEnumerator
    interface IDisposable

/// The `Seq.map` node — a struct sequence over a struct source `'S` carrying the
/// mapping struct closure `'TFunc :> Fun<'T, 'U>` by value.
[<Struct>]
type MapSeq<'S, 'E, 'TFunc, 'T, 'U when 'S :> IStructSeq<'T, 'E> and 'E :> IStructEnumerator<'T> and 'TFunc :> Fun<'T, 'U>>
    =
    new: source: 'S * f: 'TFunc -> MapSeq<'S, 'E, 'TFunc, 'T, 'U>
    interface IStructSeq<'U, MapEnumerator<'E, 'TFunc, 'T, 'U>>
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

    /// `map f source` builds a zero-allocation `MapSeq` node over `source`. The
    /// mapping function is the struct closure `'TFunc :> Fun<'T, 'U>`, dispatched
    /// by value (`constrained. !TFunc callvirt`).
    val map:
        f: 'TFunc ->
        source: 'S ->
            MapSeq<'S, 'E, 'TFunc, 'T, 'U>
                when 'S :> IStructSeq<'T, 'E> and 'E :> IStructEnumerator<'T> and 'TFunc :> Fun<'T, 'U>

    /// `fold f seed source` threads `seed` through the sequence, applying the flat
    /// arity-2 struct closure `'TFunc :> Fun<'State, 'T, 'State>` to the running
    /// state and each element in a single constrained 2-arg `Invoke`. The
    /// consuming terminal.
    val fold:
        f: 'TFunc ->
        seed: 'State ->
        source: 'S ->
            'State
                when 'S :> IStructSeq<'T, 'E>
                and 'E :> IStructEnumerator<'T>
                and 'TFunc :> Fun<'State, 'T, 'State>
