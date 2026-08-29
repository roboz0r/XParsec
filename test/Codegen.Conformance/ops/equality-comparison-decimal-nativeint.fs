// `=` and `<` at the three widths the contract declares equatable and comparable but JS
// binds no repr for. Neither operator carries a static-optimization clause at these
// widths, so both lower to the generic `Comparer` / `EqualityComparer` base rather than a
// CIL mnemonic — this program is what judges that the base compares as the opcodes do.
// CLR only: the same eight rows are unreachable on JS, which represents none of the three.
printfn "%b" (1.5M = 1.5M)
printfn "%b" (1.5M = 2.5M)
printfn "%b" (1.5M < 2.5M)
printfn "%b" (1n = 1n)
printfn "%b" (1n = 2n)
printfn "%b" (1n < 2n)
printfn "%b" (1un = 1un)
printfn "%b" (2un < 1un)
