namespace Vesper

// This file precedes `compiler-attributes.fsi`, so no attribute written here would resolve to
// a compiler marker. Both modules are therefore opened explicitly by their consumers rather
// than carrying `[<AutoOpen>]`.

module JsInterop =

    /// <summary>The body of an <c>[&lt;Import&gt;]</c>-attributed binding, whose
    /// implementation the attribute names. Throws if evaluated.</summary>
    val inline jsNative<'T> : 'T

module ArithmeticRuntime =

    /// The zero-divisor guard behind every fixed-width integral `/` and `%` (`bigint` has
    /// no mask and does not use it). JS `1 / 0 | 0` is a silent `0`, so it throws instead;
    /// it returns its argument, so `($0 / $1) | 0` reads the divisor exactly once.
    val inline checkedDivisor: divisor: 'T -> 'T
