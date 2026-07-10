namespace Vesper.Collections

// Runtime implementation target for this repo's own backend. The `Seq` module
// over `seq<'T>` (= `IEnumerable<'T>`); the contract lives in `seq.fsi`. Compiles
// to the `Vesper.Collections.SeqModule` static class (the `ModuleSuffix`
// representation gives the module the `SeqModule` holder name, matching the
// FSharp.Core surface). BCL-only — no `FSharp.Core`.
//
// These are the *minimal* reference impls, not the zero-allocation struct-chaining
// design of brainstorm-seq-module.md (a future sprint). The split follows operation shape:
//   - The eager terminals (`fold` / `reduce` / `toArray`) iterate with `for x in source`
//     over a mutable accumulator. `for … in` is the ONE iteration construct both backends
//     lower: CLR takes the `IEnumerator` interface path, JS lowers to `for…of` over the
//     native `Symbol.iterator`. The manual `GetEnumerator()`/`MoveNext()`/`Current` pull
//     protocol is CLR-idiomatic and has NO JS lowering yet (JS's `next()→{value,done}`
//     combines advance+read, which the split `MoveNext`/`Current` capability cannot express
//     without a runtime adapter — see `get-enumerator-gaps.md`), so these terminals stay
//     portable by expressing iteration as `for … in`. Their functional arguments are
//     `Vesper.Fun`s, so each application lowers to `callvirt Fun::Invoke`.
//   - The lazy `truncate` delegates to `System.Linq.Enumerable.Take`, which yields
//     BCL-correct lazy semantics without an F# `seq { }` state machine (the backend
//     does not lower sequence expressions). `Take` takes no delegate, so this needs
//     no `Vesper.Fun → System.Func` bridge. It is a CLR-Linq concern, not portable here.
// A focused starter surface (just what `set.fs` consumes); the rest of the
// FSharp.Core `Seq` surface is additive later.

open System.Collections.Generic
open System.Linq

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Seq =

    // `fold` / `reduce` / `toArray` iterate via `for x in source` for cross-target
    // portability: `for … in` lowers on BOTH backends (CLR interface path, JS `for…of`),
    // whereas the manual `GetEnumerator`/`MoveNext`/`Current` protocol is CLR-idiomatic and
    // has no JS lowering yet (see `get-enumerator-gaps.md`).
    let fold<'T, 'State> (folder: 'State -> 'T -> 'State) (state: 'State) (source: seq<'T>) : 'State =
        let mutable acc = state

        for x in source do
            acc <- folder acc x

        acc

    let reduce (reduction: 'T -> 'T -> 'T) (source: seq<'T>) : 'T =
        // `for … in` has no explicit first-move to seed the accumulator, so seed `acc` with
        // `Unchecked.defaultof<'T>` (the default-of-'T primitive) and gate on `seen`: the default
        // is never observed — the first element overwrites it before any `reduction`. A single
        // mutable slot, so `reduce` is O(1) in space and streams `source` through the one portable
        // enumeration construct (`for … in`) — no buffer.
        let mutable acc: 'T = Unchecked.defaultof<'T>
        let mutable seen = false

        for x in source do
            acc <- if seen then reduction acc x else x
            seen <- true

        if not seen then
            invalidArg "source" "The input sequence was empty."

        acc

    let truncate (count: int) (source: seq<'T>) : seq<'T> =
        Enumerable.Take(source, count)

    let toArray (source: seq<'T>) : 'T[] =
        let res = ResizeArray<'T>()

        for x in source do
            res.Add(x)

        res.ToArray()
