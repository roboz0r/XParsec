// sbyte — the sign-extending 8-bit wrap (100y + 100y = 200 truncates to -56y).
// The result must be reported through `int (…)`: a NEGATIVE sbyte literal has no
// `TConstValue` representation, so `-56y` cannot be written as an expected value,
// but `int (100y + 100y)` prints -56 and reads the same truncation.
printfn "%d" (int (100y + 100y))
printfn "%d" (int (0y - 100y))
printfn "%d" (int (100y * 2y))
printfn "%d" (int (100y / 3y))
printfn "%d" (int (100y % 7y))
