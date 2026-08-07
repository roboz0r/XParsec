namespace Vesper

// CLR-ONLY. A function value here is a nominal `Fun` interface, so adapting between the
// flat and curried shapes needs a reified object holding the captured operand — the two
// classes below.

/// <summary>Partial application of a flat 2-arg function reified once: holds the
/// flat `Fun<'A,'B,'C>` and its first argument, exposing the residual
/// `Fun<'B,'C>`.</summary>
/// <category>Basic Types</category>
/// <exclude />
type Curried<'A, 'B, 'C> =
    interface Fun<'B, 'C>
    new: f: Fun<'A, 'B, 'C> * a: 'A -> Curried<'A, 'B, 'C>

/// <summary>A curried function forced into a flat `Fun<'A,'B,'C>` slot — the slow
/// boundary path that re-dispatches through the curried chain on each `Invoke`.</summary>
/// <category>Basic Types</category>
/// <exclude />
type Flattened<'A, 'B, 'C> =
    interface Fun<'A, 'B, 'C>
    new: f: Fun<'A, Fun<'B, 'C>> -> Flattened<'A, 'B, 'C>

/// Auto-opened so a saturated 2-arg dispatch site reaches these unqualified.
[<AutoOpen>]
module FunAdapters =

    /// <summary>Curries a flat 2-arg function: <c>curryFun f a</c> yields the
    /// residual <c>Fun&lt;'B,'C&gt;</c> that applies <c>f</c> with <c>a</c> fixed.</summary>
    val curryFun: f: Fun<'A, 'B, 'C> -> a: 'A -> Fun<'B, 'C>

    /// <summary>Flattens a curried function into a flat <c>Fun&lt;'A,'B,'C&gt;</c>.</summary>
    val flatten: f: Fun<'A, Fun<'B, 'C>> -> Fun<'A, 'B, 'C>
