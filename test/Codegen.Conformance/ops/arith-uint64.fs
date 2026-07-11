// uint64 — the unsigned 64-bit width, at small magnitudes; `arith-uint64-wide.fs`
// carries the ones a 32-bit fold would truncate. `0UL - 1UL` is the row that pins
// the wrap (18446744073709551615, never -1). Reported with `%O`, as for int64.
printfn "%O" (10UL + 3UL)
printfn "%O" (10UL - 3UL)
printfn "%O" (10UL * 3UL)
printfn "%O" (10UL / 3UL)
printfn "%O" (10UL % 3UL)
printfn "%O" (0UL - 1UL)
