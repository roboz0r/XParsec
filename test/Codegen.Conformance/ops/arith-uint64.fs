// uint64 — the unsigned 64-bit width. Magnitudes stay small because a uint64
// literal folds through int32 in the lexer's constant model; `0UL - 1UL` is the
// row that pins the wrap (18446744073709551615, never -1). Reported with `%O`,
// as for int64.
printfn "%O" (10UL + 3UL)
printfn "%O" (10UL - 3UL)
printfn "%O" (10UL * 3UL)
printfn "%O" (10UL / 3UL)
printfn "%O" (10UL % 3UL)
printfn "%O" (0UL - 1UL)
