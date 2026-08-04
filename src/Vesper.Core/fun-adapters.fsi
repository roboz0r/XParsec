namespace Vesper

// CLR-ONLY. On CLR a function value is a nominal `Fun` interface, so adapting between the
// flat and curried shapes needs a reified object holding the captured operand — the two
// classes below. On JS a function value IS a function, so the same adaptation is emitted at
// the site (functions calling functions) with no library type behind it, and the backend
// owns that lowering. Nothing here is a JS surface awaiting a body.

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

/// The flat<->curried adapters. Auto-opened so a saturated 2-arg dispatch site can
/// reach `curryFun` / `flatten` unqualified. The flat `Fun<'A,'B,'C>` overloads the
/// curried `Fun<'A,'B>` by generic arity — no interface-inheritance bridge between
/// them; adaptation is reference-typed.
[<AutoOpen>]
module FunAdapters =

    /// <summary>Curries a flat 2-arg function: <c>curryFun f a</c> yields the
    /// residual <c>Fun&lt;'B,'C&gt;</c> that applies <c>f</c> with <c>a</c> fixed.</summary>
    val curryFun: f: Fun<'A, 'B, 'C> -> a: 'A -> Fun<'B, 'C>

    /// <summary>Flattens a curried function into a flat <c>Fun&lt;'A,'B,'C&gt;</c>.</summary>
    val flatten: f: Fun<'A, Fun<'B, 'C>> -> Fun<'A, 'B, 'C>
