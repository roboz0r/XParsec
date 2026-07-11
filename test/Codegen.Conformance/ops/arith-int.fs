// int32 — the five binary clauses at the native width, plus precedence and
// left-associativity (a break in operator routing or the Pratt RHS shows up here
// first), plus the two overflow rows that pin the 32-bit wrap.
printfn "%d" (2 + 3)
printfn "%d" (10 - 4)
printfn "%d" (6 * 7)
printfn "%d" (100 / 7)
printfn "%d" (100 % 7)
printfn "%d" (3 - 10)
printfn "%d" (7 - 3 - 2)
printfn "%d" (100 / 5 / 2)
printfn "%d" (2 + 3 * 4)
printfn "%d" ((10 + 5) * 2 - 3)
printfn "%d" (100 - 2 * 3 + 1)
printfn "%d" (-7 / 2)
printfn "%d" (-7 % 2)
printfn "%d" (2000000000 + 2000000000)
printfn "%d" (100000 * 100000)
