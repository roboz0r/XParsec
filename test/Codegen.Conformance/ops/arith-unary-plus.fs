// Prefix plus (`~+`) — the identity, at every width that declares it. It spans several
// widths, so it pins no single (width, operator) pair and carries no `width`; the
// per-width rows put `~+` in the matrix.
//
// Each row routes through a FUNCTION PARAMETER rather than `+5` directly: a literal with
// a sign is a constant, and the point here is that the operator resolves to the width's
// own declared member and yields the operand unchanged — including at the unsigned widths,
// where `~-` is undefined and rejected.
let plusI (x: int) = +x
let plusB (x: byte) = +x
let plusS (x: sbyte) = +x
let plusH (x: uint16) = +x
let plusU (x: uint32) = +x
let plusL (x: int64) = +x
let plusW (x: uint64) = +x
let plusF (x: float) = +x
let plusG (x: bigint) = +x

printfn "%d" (plusI 42)
printfn "%d" (plusI (-42))
printfn "%d" (int (plusB 200uy))
printfn "%d" (int (plusS (-128y)))
printfn "%d" (int (plusH 65535us))
printfn "%O" (plusU 4000000000u)
printfn "%O" (plusL (-9223372036854775808L))
printfn "%O" (plusW 18446744073709551615UL)
printfn "%f" (plusF 2.5)
printfn "%O" (plusG (bigint 1000000007))
