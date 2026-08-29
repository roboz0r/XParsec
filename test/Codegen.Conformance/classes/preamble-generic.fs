// Instance `let`s on a GENERIC class: the preamble storage is typed by the type parameter, and
// a `let` closing over a `'T`-typed ctor param has to keep it at each instantiation. Two
// instantiations, so a backend that erased the preamble to one shared slot is caught.
type Cell<'T>(x: 'T, n: int) =
    let k = n * 2 + 1
    let get () = x
    member this.K() = k
    member this.X() = get ()

let s = Cell<string>("hi", 3)
let i = Cell<int>(9, 10)
printfn "%d %s" (s.K()) (s.X())
printfn "%d %d" (i.K()) (i.X())
