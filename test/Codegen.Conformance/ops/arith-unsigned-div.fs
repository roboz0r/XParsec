// Integer `/` must TRUNCATE. A target whose `/` is true division leaves 3.333…
// where 3 belongs, and the fraction survives into the next operation — so
// `x / y * y` is the probe no report-site conversion can launder: it is 9 at every
// integral width, and 10 wherever the quotient kept its fraction.
printfn "%d" (int (10uy / 3uy * 3uy))
printfn "%d" (int (7uy / 2uy * 2uy))
printfn "%d" (int (10us / 3us * 3us))
printfn "%d" (int (10y / 3y * 3y))
printfn "%d" (int (10s / 3s * 3s))
printfn "%u" (int (10u / 3u * 3u))
printfn "%d" (10 / 3 * 3)
