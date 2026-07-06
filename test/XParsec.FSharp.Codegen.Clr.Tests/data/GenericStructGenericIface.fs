type IBox<'E> =
    abstract member Unwrap : unit -> 'E
[<Struct>]
type Box<'T> =
    val Value : 'T
    new(value: 'T) = { Value = value }
    interface IBox<'T> with
        member this.Unwrap() : 'T = this.Value
let b = Box<int>(42)
let i = (b :> IBox<int>)
printfn "%d" (i.Unwrap())
