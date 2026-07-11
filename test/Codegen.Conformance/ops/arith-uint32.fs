// uint32 — the mod-2^32 wrap and the UNSIGNED `/` and `%`. `int (…)` reinterprets
// the bits (it does not widen), and `%u` reads them back unsigned, so the pair is
// a lossless report of a uint32 value through the int32-typed `%d`/`%u` hole.
//
// The last two rows are the ones a 32-bit-masking report cannot fake: `0u - 1u` is
// 4294967295, and DIVIDING that must use the unsigned quotient. A backend that
// leaves -1 on the wire divides a negative and lands nowhere near.
printfn "%u" (int (4000000000u + 1u))
printfn "%u" (int (4000000000u + 1000000000u))
printfn "%u" (int (0u - 1u))
printfn "%u" (int (100000u * 100000u))
printfn "%u" (int (4000000000u / 2u))
printfn "%u" (int (4000000000u % 3u))
printfn "%u" (int ((0u - 1u) / 2u))
printfn "%u" (int ((0u - 1u) % 10u))
