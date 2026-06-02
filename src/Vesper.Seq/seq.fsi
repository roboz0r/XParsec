namespace Vesper.Collections

// Vesper.Seq contract — the `Seq` module over `seq<'T>` (= `IEnumerable<'T>`,
// the abbreviation declared in Vesper.List's `list.fsi`). A standalone package
// (package-split-plan PS1: one package per type/module) mirroring Vesper.Array's
// `module Array`. Like the rest of the Vesper tree this is the front-end symbol
// contract: parsed by XParsec.FSharp and walked into an IExternalSymbolProvider.
// The runtime impl is `seq.fs` (→ Vesper.Seq.dll, BCL-only, our own backend).
// Depends on Vesper.Core (`Fun`, `int`, `'T[]`) and Vesper.List (the `seq<'T>`
// and `ResizeArray<'T>` abbreviations).
//
// Per package-split-plan PS5 the package is named `Vesper.Seq` but it contributes
// the `Seq` module into namespace `Vesper.Collections`, not `Vesper.Seq` — the
// same namespace `set.fs` lives in, so its `Seq.fold` / `Seq.reduce` /
// `Seq.truncate` calls resolve without an extra `open`.
//
// This is the *minimal* reference surface (vesper-set-sprint-phase-8.md §8.4):
// just the four operations `set.fs` consumes — `fold` (line 820), `reduce`
// (line 823), `truncate` (line 961), plus `toArray` for symmetry. The full
// zero-allocation, struct-chaining, deforesting `Seq` design lives in
// brainstorm-seq-module.md and is a future sprint; the eager terminals (`fold` /
// `reduce` / `toArray`) are explicit-enumerator reference impls, while the lazy
// `truncate` delegates to `System.Linq.Enumerable.Take` (BCL-correct laziness with
// no `seq { }` state machine). The rest of the FSharp.Core `Seq` surface is
// additive later, the same "grow the module additively" stance as Vesper.List /
// Vesper.Array.

open System.Collections.Generic


    /// Operations over `seq<'T>`. The `ModuleSuffix` representation gives the
    /// module the compiled name `SeqModule` (matching the FSharp.Core surface) and
    /// lets it coexist with the `seq<'T>` abbreviation / `seq { }` builder. Each
    /// functional argument's arrow desugars to `Vesper.Fun`.
    [<RequireQualifiedAccess>]
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Seq =

        /// <summary>Applies a function to each element of the collection, threading an accumulator
        /// argument through the computation. If the input function is <c>f</c> and the elements are
        /// <c>i0...iN</c> then computes <c>f (... (f s i0) i1 ...) iN</c>.</summary>
        ///
        /// <param name="folder">A function that updates the state with each element from the sequence.</param>
        /// <param name="state">The initial state.</param>
        /// <param name="source">The input sequence.</param>
        ///
        /// <returns>The state object after the folding function is applied to each element of the sequence.</returns>
        val fold<'T, 'State> : folder: ('State -> 'T -> 'State) -> state: 'State -> source: seq<'T> -> 'State

        /// <summary>Applies a function to each element of the sequence, threading an accumulator
        /// argument through the computation. Begin by applying the function to the first two elements.
        /// Then feed this result into the function along with the third element and so on. Return the
        /// final result.</summary>
        ///
        /// <param name="reduction">A function that takes in the current accumulated result and the next
        /// element of the sequence to produce the next accumulated result.</param>
        /// <param name="source">The input sequence.</param>
        ///
        /// <returns>The final result of the reduction function.</returns>
        ///
        /// <exception cref="T:System.ArgumentException">Thrown when the input sequence is empty.</exception>
        val reduce: reduction: ('T -> 'T -> 'T) -> source: seq<'T> -> 'T

        /// <summary>Returns a sequence that when enumerated returns at most <c>count</c> elements.</summary>
        ///
        /// <param name="count">The maximum number of items to enumerate.</param>
        /// <param name="source">The input sequence.</param>
        ///
        /// <returns>The result sequence.</returns>
        val truncate: count: int -> source: seq<'T> -> seq<'T>

        /// <summary>Builds an array from the given collection.</summary>
        ///
        /// <param name="source">The input sequence.</param>
        ///
        /// <returns>The result array.</returns>
        val toArray: source: seq<'T> -> 'T[]
