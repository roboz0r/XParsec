[<Struct>]
type Inner =
    val Cur : int
    new(c: int) = { Cur = c }
    member this.Get() : int = this.Cur
[<Struct>]
type Outer =
    val I : Inner
    new(i: Inner) = { I = i }
    member this.StepGet() : int = this.I.Get()
let run () =
    let o = Outer(Inner(7))
    printfn "%d" (o.StepGet())
run ()
