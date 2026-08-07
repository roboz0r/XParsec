namespace Vesper

[<AutoOpen>]
module StructuralRuntime =

    /// Structural equality of two values (JS runtime entry for aggregate operands).
    val structuralEquals: x: 'T -> y: 'T -> bool when 'T: equality

    /// Structural hash of a value (JS runtime entry for aggregate operands).
    val structuralHash: obj: 'T -> int when 'T: equality

[<AutoOpen>]
module ArithmeticRuntime =

    /// The zero-divisor guard behind every INTEGRAL `/` and `%`. JS `/` yields `Infinity`
    /// and `Infinity | 0` is a silent `0`, so it throws instead — and it RETURNS its
    /// argument, letting a clause read the divisor exactly once inside the width mask.
    val checkedDivisor: divisor: 'T -> 'T
