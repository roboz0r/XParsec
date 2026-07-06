type IBox<'T> =
    abstract member Get : unit -> 'T
[<Struct>]
type IntBox =
    val N : int
    new(n: int) = { N = n }
    interface IBox<int> with
        member this.Get() = this.N
[<Struct>]
type Wrap<'S, 'T when 'S :> IBox<'T>> =
    val Inner : 'S
    new(inner: 'S) = { Inner = inner }
    member this.Fetch() : 'T = this.Inner.Get()
let w = Wrap<IntBox, int>(IntBox 7)
printfn "%d" (w.Fetch())
