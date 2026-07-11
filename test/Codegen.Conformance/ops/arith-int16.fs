// int16 — the sign-extending 16-bit wrap (30000s + 10000s = 40000 truncates to
// -25536s). Reported through `int (…)`, as for sbyte: a negative int16 literal has
// no `TConstValue` representation.
printfn "%d" (int (30000s + 10000s))
printfn "%d" (int (0s - 30000s))
printfn "%d" (int (300s * 300s))
printfn "%d" (int (30000s / 3s))
printfn "%d" (int (30000s % 7s))
