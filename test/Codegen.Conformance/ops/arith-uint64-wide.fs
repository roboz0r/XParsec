// uint64 at magnitudes ABOVE 2^32 — the width's whole reason for existing, and the
// rows no narrower model can fake. A backend that carries a uint64 LITERAL in 32 bits
// (or, on JS, as a `number` rather than a BigInt) answers a truncated value from the
// first row on. The last row's dividend is beyond int64's range as well, so it also
// needs the UNSIGNED quotient: read signed it is -1, and -1 / 2 is 0.
printfn "%O" (10000000000UL + 1UL)
printfn "%O" (10000000000UL - 1UL)
printfn "%O" (10000000000UL * 3UL)
printfn "%O" (10000000000UL / 3UL)
printfn "%O" (10000000000UL % 3UL)
printfn "%O" (18446744073709551615UL / 2UL)
