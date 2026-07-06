open System

let g () =
    let s = Span<char>()
    s.Length

printfn "%d" (g ())
