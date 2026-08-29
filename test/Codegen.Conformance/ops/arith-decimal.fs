// decimal — the one width whose operator bodies are BCL CALLS (`Decimal.Add`, …) rather
// than a CIL mnemonic, so this program is what judges that a spliced call computes the
// same value a spliced opcode does. It runs on the CLR only: JS has no decimal repr, so
// the type is unrepresentable there and the same six declarations are unreachable.
//
// The last two rows are why the width exists: `0.1M + 0.2M` is EXACTLY `0.3` (the float
// row of this corpus is not), and multiplying keeps the operand's scale.
let a = 1.5M
let b = 2.5M
printfn "%M" (a + b)
printfn "%M" (10M - 4M)
printfn "%M" (6M * 7M)
printfn "%M" (10M / 4M)
printfn "%M" (10M % 3M)
printfn "%M" (-a)
printfn "%M" (0.1M + 0.2M)
printfn "%M" (1.50M * 2M)
