type IGetVal =
    abstract member GetVal : unit -> int
[<Struct>]
type SBox =
    val N : int
    new(n: int) = { N = n }
    interface IGetVal with
        member this.GetVal() = this.N
let callIt (x: 'T when 'T :> IGetVal) : int = x.GetVal()
printfn "%d" (callIt (SBox 9))
