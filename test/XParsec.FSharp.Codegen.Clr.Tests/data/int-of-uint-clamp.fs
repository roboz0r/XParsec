open System
let h (x: int) = int (Math.Clamp(uint x, 0u, 100u))
printfn "%d" (h 50)
