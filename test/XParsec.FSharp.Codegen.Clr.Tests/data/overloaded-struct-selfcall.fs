open System.Runtime.CompilerServices

[<Struct; IsByRefLike>]
type S =
    val mutable Acc: int
    new(a: int) = { Acc = a }
    member this.G(x: 'T) : unit = this.Acc <- this.Acc + 100
    member this.G(x: 'T, k: string) : unit = this.Acc <- this.Acc + 10
    member this.G(x: 'T, k: int) : unit = this.Acc <- this.Acc + 1
    member this.U(y: int) : unit = this.G(y, 0)

let run () =
    let mutable s = S(0)
    s.U 5
    s.U 5
    s.Acc

printfn "%d" (run ())
