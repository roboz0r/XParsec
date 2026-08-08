[<Struct>]
type Inner =
    val Cur : int
    new(c: int) = { Cur = c }
    member this.Get() : int = this.Cur
[<Struct>]
type Outer =
    val I : Inner
    new(i: Inner) = { I = i }
let run () =
    let o = Outer(Inner(42))
    printfn "%d" (o.I.Get())
run ()
