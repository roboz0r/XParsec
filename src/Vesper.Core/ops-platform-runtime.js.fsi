namespace Vesper

[<AutoOpen>]
module StructuralRuntime =

    /// Structural equality of two values (JS runtime entry for aggregate operands).
    val structuralEquals: x: 'T -> y: 'T -> bool when 'T: equality

    /// Structural hash of a value (JS runtime entry for aggregate operands).
    val structuralHash: obj: 'T -> int when 'T: equality

[<AutoOpen>]
module ArithmeticRuntime =

    /// The zero-divisor guard behind every fixed-width integral `/` and `%` (`bigint` has
    /// no mask and does not use it). JS `1 / 0 | 0` is a silent `0`, so it throws instead;
    /// it returns its argument, so `($0 / $1) | 0` reads the divisor exactly once.
    val checkedDivisor: divisor: 'T -> 'T
