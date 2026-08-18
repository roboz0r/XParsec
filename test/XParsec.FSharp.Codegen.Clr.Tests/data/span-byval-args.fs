open System.Runtime.CompilerServices
open System

[<Struct; IsByRefLike>]
type Buf(chars: Span<char>) =
    member this.Build() =
        "ab".CopyTo(chars.Slice(0, 2))
        chars.Slice(2, 3).Fill('-')
        "cd".CopyTo(chars.Slice(5, 2))
        chars.Slice(0, 7).ToString()

    member this.Dup() =
        chars.Slice(0, 2).CopyTo(chars.Slice(7, 2))
        chars.Slice(0, 9).ToString()

    member this.Fits() = "xy".TryCopyTo(chars.Slice(0, 2))
    member this.Overflows() = "toolong".TryCopyTo(chars.Slice(0, 2))

let arr = [| '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.'; '.' |]
let b = Buf(Span<char>(arr))
printfn "%s" (b.Build())
printfn "%s" (b.Dup())
printfn "%b" (b.Fits())
printfn "%b" (b.Overflows())
