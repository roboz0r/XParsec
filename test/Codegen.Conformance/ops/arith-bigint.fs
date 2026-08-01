// bigint — arbitrary precision, so the rows that matter are the ones NO fixed width holds:
// `big * big` runs past 2^64 and must still be exact on both backends (BCL `BigInteger` on
// the CLR, the JS `BigInt` primitive). Like `decimal`, its operator bodies are BCL calls on
// the CLR rather than mnemonics; unlike `decimal` it has a JS repr, so both targets run it.
//
// The signed `/` and `%` rows are the agreement that is not free: both must truncate toward
// zero and take the sign of the DIVIDEND. `%O` rather than `%A` — `%A` renders a JS bigint
// with the `L` suffix it uses for `int64`, which the CLR has no reason to print.
let a = bigint 1000000007
let b = bigint 1000000009
let big = a * b
printfn "%O" big
printfn "%O" (big * big)
printfn "%O" (bigint 6 + bigint 7)
printfn "%O" (bigint 6 - bigint 7)
printfn "%O" (bigint 20 / bigint 6)
printfn "%O" (bigint 20 % bigint 6)
printfn "%O" (bigint (-20) / bigint 6)
printfn "%O" (bigint (-20) % bigint 6)
printfn "%O" (-(bigint 5))
