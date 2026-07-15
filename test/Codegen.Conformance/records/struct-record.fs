// A [<Struct>] record. On the CLR it emits as a System.ValueType-based value type
// (sealed, base-chain-free ctor, value-type-shaped structural equality); on JS, which
// has no value-type concept, it emits as an ordinary reference object. Both must agree
// on every observable result below: construction, field reads, structural equality, and
// a `{ r with … }` copy-update.
[<Struct>]
type P = { X: int; Y: int }

let a = { X = 3; Y = 4 }
let b = { X = 3; Y = 4 }
let c = { X = 5; Y = 4 }

printfn "%d" a.X
printfn "%d" a.Y
printfn "%b" (a = b)
printfn "%b" (a = c)

let d = { a with X = 10 }
printfn "%d" (d.X + d.Y)
