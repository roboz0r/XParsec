type IGetVal =
    abstract member GetVal : unit -> int
type Holder(n: int) =
    interface IGetVal with
        member _.GetVal() = n
let h = Holder(42)
let v = (h :> IGetVal).GetVal()
printfn "%d" v
