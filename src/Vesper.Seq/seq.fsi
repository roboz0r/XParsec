namespace Vesper.Collections

open System.Collections.Generic


    /// Operations over `seq<'T>`. `ModuleSuffix` gives the compiled name `SeqModule`
    /// and lets the module coexist with the `seq<'T>` abbreviation and `seq { }` builder.
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
