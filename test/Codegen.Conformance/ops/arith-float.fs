// float (IEEE 64) — `%f` is the exact reporting channel (6 fractional digits, as
// F#'s default). The last row is the one that would expose an integer-division
// clause hiding under `/`.
printfn "%f" (1.5 + 2.5)
printfn "%f" (3.0 - 1.5)
printfn "%f" (1.5 * 2.0)
printfn "%f" (7.5 / 2.5)
printfn "%f" (7.5 % 2.0)
printfn "%f" (1.0 / 3.0)
