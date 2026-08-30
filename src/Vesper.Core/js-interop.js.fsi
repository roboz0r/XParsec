namespace Vesper

// This file precedes `compiler-attributes.fsi`, so an attribute written here would not
// resolve. Consumers open the module explicitly instead of it carrying `[<AutoOpen>]`.

module ArithmeticRuntime =

    /// The zero-divisor guard behind every fixed-width integral `/` and `%` (`bigint` has
    /// no mask and does not use it). JS `1 / 0 | 0` is a silent `0`, so it throws instead;
    /// it returns its argument, so `($0 / $1) | 0` reads the divisor exactly once.
    val inline checkedDivisor: divisor: 'T -> 'T
