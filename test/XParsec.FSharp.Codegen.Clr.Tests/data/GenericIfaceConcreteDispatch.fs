type IBox<'T> =
    abstract member Get : unit -> 'T
[<Struct>]
type IntBox =
    val N : int
    new(n: int) = { N = n }
    interface IBox<int> with
        member this.Get() = this.N
let callIt (x: 'T when 'T :> IBox<int>) : int = x.Get()
printfn "%d" (callIt (IntBox 5))
