namespace Vesper.Collections

/// <summary>Array primitives with no target-neutral F# body; each target's
/// <c>array-prelude.*.fs</c> supplies its own.</summary>
module ArrayPrelude =

    /// <summary>Allocates a 1-D zero-based array of <c>count</c> elements.</summary>
    ///
    /// <param name="count">The length of the array to allocate.</param>
    ///
    /// <returns>The allocated array, dense in every slot. Initial values are the
    /// element type's zero on CLR, but <c>null</c> on JS.</returns>
    val inline NewArray: count: int -> 'T[]
