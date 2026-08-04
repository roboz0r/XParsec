namespace Vesper.Collections

/// <summary>The array primitives whose bodies are an INTRINSIC rather than F# — the
/// reason this file exists is that they cannot be written target-neutrally, so they
/// are split out and `array.fs` (the module proper) carries no inline IL at all.
/// The `.clr.fs` and `.js.fs` bodies are identical today; being separate files is
/// what lets them stop being.
///
/// Deliberately NOT <c>[&lt;AutoOpen&gt;]</c>, unlike <c>ops-platform</c>'s modules:
/// those are ambient by design, this is an implementation detail of one module and
/// has no business in a consumer's scope.</summary>
module ArrayPrelude =

    /// <summary>Allocates a 1-D zero-based array of <c>count</c> elements. Named for
    /// the <c>GetArray</c> / <c>SetArray</c> / <c>GetArrayLength</c> family it joins;
    /// unlike those three it is NOT a front-end desugaring target — nothing resolves
    /// it by name — which is why it lives here and not in <c>ops-platform</c>.</summary>
    ///
    /// <param name="count">The length of the array to allocate.</param>
    ///
    /// <returns>The allocated array. Element values are the target's own zero.</returns>
    val inline NewArray: count: int -> 'T[]
