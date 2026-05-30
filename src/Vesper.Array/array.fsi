namespace Vesper.Collections

// Vesper.Array contract — the `Array` module over the intrinsic `'T[]` type
// (declared in Vesper.Core's `prim-types-min.fsi`). A standalone package
// (package-split-plan PS1: one package per type/module) mirroring Vesper.List's
// `module List`. Like the rest of the Vesper tree this is the front-end symbol
// contract: parsed by XParsec.FSharp and walked into an IExternalSymbolProvider.
// The runtime impl is `array.fs` (→ Vesper.Array.dll, BCL-only, our own
// backend). Depends on Vesper.Core (`'T[]`, `Fun`, `int`).
//
// Per package-split-plan PS5 the package is named `Vesper.Array` but it
// contributes the `Array` module into namespace `Vesper.Collections`, not
// `Vesper.Array` — the same namespace `set.fs` lives in, so its `Array.fold` /
// `Array.zeroCreate` calls resolve without an extra `open`.
//
// Sole consumer today is `set.fs`: `toArray` (line 713) calls `Array.zeroCreate`
// and `ofArray` (line 728) calls `Array.fold`. A focused starter surface — the
// rest of the FSharp.Core `Array` surface is additive later, the same "grow the
// module additively" stance as Vesper.List / Vesper.Result. See
// vesper-set-sprint-phase-8.md §8.2.

/// Operations over `'T[]`. The `ModuleSuffix` representation gives the module
/// the compiled name `ArrayModule` (matching the FSharp.Core surface) and lets
/// it coexist with the BCL `System.Array` type name. Each functional argument's
/// arrow desugars to `Vesper.Fun`.
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Array =

    /// <summary>Creates an array whose elements are all initially the default value.</summary>
    ///
    /// <param name="count">The length of the array to create.</param>
    ///
    /// <returns>The created array.</returns>
    val zeroCreate: count: int -> 'T[]

    /// <summary>Applies a function to each element of the array, threading an accumulator
    /// argument through the computation. Apply the function to the first two elements of the
    /// array. Then feed this result into the function along with the third element and so on.
    /// Return the final result.</summary>
    ///
    /// <param name="folder">The function to update the state given the input elements.</param>
    /// <param name="state">The initial state.</param>
    /// <param name="array">The input array.</param>
    ///
    /// <returns>The final state.</returns>
    val fold<'T, 'State> : folder: ('State -> 'T -> 'State) -> state: 'State -> array: 'T[] -> 'State
