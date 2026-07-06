open System
let MaxChars = 0x3FFFFFDFu

let grow (pos: int) (additional: int) (cap: int) =
    let needed = uint pos + uint additional
    let doubled = uint cap * 2u
    let size = Math.Max(needed, doubled)
    Math.Min(Math.Clamp(size, 256u, MaxChars), MaxChars)

printfn "%b" (grow 10 20 8 = 256u)
