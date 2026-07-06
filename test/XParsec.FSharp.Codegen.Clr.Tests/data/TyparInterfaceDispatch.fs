type IGetVal =
    abstract member GetVal : unit -> int
type Holder(n: int) =
    interface IGetVal with
        member _.GetVal() = n
let callIt (x: 'T when 'T :> IGetVal) : int = x.GetVal()
printfn "%d" (callIt (Holder 7))
