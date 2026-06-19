namespace Vesper.Collections

// Vesper.Seq struct-seq contract — the zero-allocation, struct-chaining
// `StructSeq` surface of `brainstorm-seq-module.md`, generic over the element
// type `'T`. The front-end symbol contract (parsed by XParsec.FSharp, walked into
// an IExternalSymbolProvider); the runtime impl is `struct-seq.fs`.
//
// This is the GRADUATION of the rung-3 inline slice (proven `int`-element in
// StructSeqTests.fs) into a real generic-over-`'T` library: the marker interfaces
// `IStructEnumerator<'T>` / `IStructSeq<'T, 'E>`, the `ArrayEnumerator<'T>` /
// `ArraySeq<'T>` entry pair, the `MapEnumerator<'E, 'TFunc, 'T, 'U>` /
// `MapSeq<'S, 'E, 'TFunc, 'T, 'U>` map node, and the `ofArray` / `map` / `fold`
// module surface.
//
// RUNG 4 — the functional arguments are carried as EXPLICIT CONSTRAINED TYPARS,
// not reference-type `Vesper.Fun` values: `map` rides `'TFunc :> Fun<'T, 'U>` and
// `fold` rides `'TFunc :> Fun2<'State, 'T, 'State>` (the flat arity-2 interface, a
// single constrained 2-arg `Invoke`). Each application lowers to `constrained.
// !TFunc callvirt` (no heap, JIT-devirtualizable) — exactly how the library
// already threads its explicit `'S` / `'E` enumerator typars. A struct closure
// passed as `'TFunc` is dispatched by value with no box.

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
    /// arity-2 struct closure `'TFunc :> Fun2<'State, 'T, 'State>` to the running
    /// state and each element in a single constrained 2-arg `Invoke`. The
    /// consuming terminal.
    val fold:
        f: 'TFunc ->
        seed: 'State ->
        source: 'S ->
            'State
                when 'S :> IStructSeq<'T, 'E>
                and 'E :> IStructEnumerator<'T>
                and 'TFunc :> Fun2<'State, 'T, 'State>
