// byte — the mod-256 wrap. `int (…)` is the reporting channel (`%d` types its
// argument as int32), and it is a WIDENING conversion here: it cannot repair a
// missing 8-bit mask, so a backend whose `+` leaves 300 on the wire prints 300.
printfn "%d" (int (10uy + 20uy))
printfn "%d" (int (200uy + 100uy))
printfn "%d" (int (10uy - 20uy))
printfn "%d" (int (20uy * 20uy))
printfn "%d" (int (200uy / 3uy))
printfn "%d" (int (200uy % 7uy))
