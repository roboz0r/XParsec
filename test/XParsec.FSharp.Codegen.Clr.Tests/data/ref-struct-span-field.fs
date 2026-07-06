open System

[<Struct; IsByRefLike>]
type SpanView(chars: Span<char>) =
    member this.Len = chars.Length
    member this.Tail = chars.Slice(2, 3)

let arr = [| 'h'; 'e'; 'l'; 'l'; 'o' |]
let v = SpanView(Span<char>(arr))
printfn "%d" v.Len
printfn "%d" v.Tail.Length
