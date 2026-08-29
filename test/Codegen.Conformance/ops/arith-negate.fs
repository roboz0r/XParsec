// Unary negation (`~-`) per SIGNED width — the only widths it is defined on (see
// `arith-negate-unsigned.fs` for the rejection). A literal `-5` is a negative CONSTANT,
// not the operator, so each width routes through a function parameter to reach the
// contract's own negation clause.
//
// Each width is negated twice: once at an ordinary value, and once at its MINIMUM, where
// the negation overflows and must wrap back onto itself. The minimum is the row that
// needs the width mask — a bare CIL `neg` (or a bare JS `-`) computes on a wider stack
// and yields +128 where -128y belongs.
let negI (x: int) = -x
let negL (x: int64) = -x
let negF (x: float) = -x
let negG (x: float32) = -x
let negS (x: sbyte) = -x
let negH (x: int16) = -x

printfn "%d" (negI 5)
printfn "%d" (negI (-5))
printfn "%d" (negI (-2147483648))
printfn "%O" (negL 1000000000000L)
printfn "%O" (negL (-9223372036854775808L))
printfn "%f" (negF 2.5)
printfn "%O" (negG 2.5f)
printfn "%d" (int (negS 100y))
printfn "%d" (int (negS (-128y)))
printfn "%d" (int (negH 30000s))
printfn "%d" (int (negH (-32768s)))
