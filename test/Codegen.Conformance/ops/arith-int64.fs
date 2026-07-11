// int64 — magnitudes beyond int32, so a 32-bit opcode would visibly wrap. The
// value is reported with `%O` (stringification): `%d` types its argument as int32,
// so an int64 cannot ride it, and no int64 -> string conversion exists in the
// contract.
printfn "%O" (1000000000000L + 1L)
printfn "%O" (1000000000000L - 1L)
printfn "%O" (1000000000000L * 3L)
printfn "%O" (3000000000000L / 3L)
printfn "%O" (1000000000001L % 10L)
printfn "%O" (-7L / 2L)
