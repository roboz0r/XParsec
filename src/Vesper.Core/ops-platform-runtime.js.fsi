namespace Vesper

[<AutoOpen>]
module StructuralRuntime =

    /// Structural equality of two values (JS runtime entry for aggregate operands).
    val structuralEquals: x: 'T -> y: 'T -> bool when 'T: equality

    /// Structural hash of a value (JS runtime entry for aggregate operands).
    val structuralHash: obj: 'T -> int when 'T: equality

[<AutoOpen>]
module ArithmeticRuntime =

    /// The zero-divisor guard behind every INTEGRAL `/` and `%` clause. CIL `div` / `rem`
    /// fault on a zero divisor; JS `/` yields `Infinity`, and `Infinity | 0` is `0` — so a
    /// bare masking template would quietly answer `0` where the CLR raises. This returns
    /// its argument (or throws), which is what lets a clause wrap it in the width mask and
    /// still read the divisor exactly ONCE: `(# "($0 / $1) & 0xFF" x (checkedDivisor y) #)`.
    /// A template repeating a `$N` hole would DOUBLE-EVALUATE the operand, which is why the
    /// check cannot be an inline ternary guard.
    ///
    /// ONE guard covers every integral width — the ≤32-bit widths are all JS `number` and
    /// int64 / uint64 are `bigint`s, and the body tests both zeros. `float` / `float32` deliberately
    /// do NOT route through it: IEEE division by zero yielding `Infinity` is the correct
    /// answer for a float, not a fault.
    val checkedDivisor: divisor: 'T -> 'T
