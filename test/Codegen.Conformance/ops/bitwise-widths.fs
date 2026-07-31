// The bitwise family per width. Each width now DECLARES `&&& ||| ^^^ ~~~ <<< >>>` on
// itself, so each has its own body — where a single width-blind opcode used to serve
// them all. These are the rows that tell the two apart.
//
// The complement and the right shift carry the weight. `~~~` leaves an unsigned width
// unless masked back (`~~~0uy` is 255, not -1), and `>>>` must zero-fill on an unsigned
// width and sign-extend on a signed one — the same `shr.un`/`shr` split CIL makes, and
// which JS spells `>>>`/`>>`.
//
// Negative operands are written `(0 - n)`: a negative literal at a narrow width has no
// `TConstValue` representation (see `arith-sbyte.fs`). Reported through `int (…)` /
// `%O`, as the arith programs are.

// int32 — the reference width.
printfn "%d" (13 &&& 11)
printfn "%d" (13 ||| 11)
printfn "%d" (13 ^^^ 11)
printfn "%d" (~~~0)
printfn "%d" (1 <<< 4)
printfn "%d" (208 >>> 2)
printfn "%d" ((0 - 16) >>> 2)

// byte — unsigned 8, so `~~~` masks back and `>>>` zero-fills.
printfn "%d" (int (13uy &&& 11uy))
printfn "%d" (int (13uy ||| 11uy))
printfn "%d" (int (13uy ^^^ 11uy))
printfn "%d" (int (~~~0uy))
printfn "%d" (int (200uy >>> 4))
printfn "%d" (int (3uy <<< 2))

// sbyte — signed 8, so `>>>` sign-extends.
printfn "%d" (int (~~~0y))
printfn "%d" (int ((0y - 16y) >>> 2))
printfn "%d" (int (3y <<< 2))

// int16 / uint16.
printfn "%d" (int (~~~0s))
printfn "%d" (int ((0s - 16s) >>> 2))
printfn "%d" (int (~~~0us))
printfn "%d" (int (60000us >>> 4))

// uint32 — the top-bit-set rows, which a signed read gets wrong.
printfn "%u" (int (~~~0u))
printfn "%u" (int (4000000000u >>> 8))
printfn "%u" (int (4000000000u &&& 4278190080u))

// int64 / uint64.
printfn "%O" (13L &&& 11L)
printfn "%O" (~~~0L)
printfn "%O" ((0L - 16L) >>> 2)
printfn "%O" (1L <<< 40)
printfn "%O" (13UL ||| 3UL)
printfn "%O" (~~~0UL)
printfn "%O" ((0UL - 1UL) >>> 8)
