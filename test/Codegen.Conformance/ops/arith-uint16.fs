// uint16 — the mod-65536 wrap (10us - 20us = 65526us, never -10).
printfn "%d" (int (60000us + 10000us))
printfn "%d" (int (10us - 20us))
printfn "%d" (int (300us * 300us))
printfn "%d" (int (60000us / 3us))
printfn "%d" (int (60000us % 7us))
