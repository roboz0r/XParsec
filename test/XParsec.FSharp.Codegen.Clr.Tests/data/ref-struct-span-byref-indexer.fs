open System

[<Struct; IsByRefLike>]
type SpanView(chars: Span<char>) =
    member this.At(i) = chars.[i]

let arr = [| 'h'; 'e'; 'l'; 'l'; 'o' |]
let v = SpanView(Span<char>(arr))
printfn "%c" (v.At 1)
printfn "%c" (v.At 4)
