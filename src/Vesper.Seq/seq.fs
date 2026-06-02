namespace Vesper.Collections

// Runtime implementation target for this repo's own backend. The `Seq` module
// over `seq<'T>` (= `IEnumerable<'T>`); the contract lives in `seq.fsi`. Compiles
// to the `Vesper.Collections.SeqModule` static class (the `ModuleSuffix`
// representation gives the module the `SeqModule` holder name, matching the
// FSharp.Core surface). BCL-only — no `FSharp.Core`.
//
// These are the *minimal* reference impls (vesper-set-sprint-phase-8.md §8.4),
// not the zero-allocation struct-chaining design of brainstorm-seq-module.md (a
// future sprint). The split follows operation shape:
//   - The eager terminals (`fold` / `reduce` / `toArray`) are explicit-enumerator
//     loops: each pulls an `IEnumerator<'T>` from `source.GetEnumerator()` under a
//     `use` (so the enumerator is disposed — Phase 4's `IDisposable` support) and
//     drives it with `MoveNext` / `Current`. Their functional arguments are
//     `Vesper.Fun`s, so each application lowers to `callvirt Fun::Invoke`.
//   - The lazy `truncate` delegates to `System.Linq.Enumerable.Take`, which yields
//     BCL-correct lazy semantics without an F# `seq { }` state machine (the backend
//     does not lower sequence expressions). `Take` takes no delegate, so this needs
//     no `Vesper.Fun → System.Func` bridge.
// A focused starter surface (just what `set.fs` consumes); the rest of the
// FSharp.Core `Seq` surface is additive later.

open System.Collections.Generic
open System.Linq

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Seq =

    let fold (folder: 'State -> 'T -> 'State) (state: 'State) (source: seq<'T>) : 'State =
        use e = source.GetEnumerator()
        let mutable acc = state

        while e.MoveNext() do
            acc <- folder acc e.Current

        acc

    let reduce (reduction: 'T -> 'T -> 'T) (source: seq<'T>) : 'T =
        use e = source.GetEnumerator()

        if not (e.MoveNext()) then
            invalidArg "source" "The input sequence was empty."

        let mutable acc = e.Current

        while e.MoveNext() do
            acc <- reduction acc e.Current

        acc

    let truncate (count: int) (source: seq<'T>) : seq<'T> =
        Enumerable.Take(source, count)

    let toArray (source: seq<'T>) : 'T[] =
        let res = ResizeArray<'T>()
        use e = source.GetEnumerator()

        while e.MoveNext() do
            res.Add(e.Current)

        res.ToArray()
