// float32 (IEEE 32) — the single-precision width. `%f` types its argument as
// float, so a float32 is printed with `%O` (shortest round-trip stringification).
//
// The last row is the whole point: `0.1f + 0.2f` rounds to the float32 nearest 0.3
// and prints "0.3". Computing it in DOUBLE precision instead prints
// 0.30000000000000004 — close, but not this width's result. The first four rows are
// exactly representable and cannot tell the two apart.
printfn "%O" (1.5f + 2.5f)
printfn "%O" (3.0f - 1.5f)
printfn "%O" (1.5f * 2.0f)
printfn "%O" (7.5f / 2.5f)
printfn "%O" (0.1f + 0.2f)
