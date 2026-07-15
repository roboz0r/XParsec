// A `[<Struct>]` record: constructs, reads fields, and compares by structural
// equality (the synthesised value-type equality triple + `%A`-free hashing). On the
// CLR it must emit as a `System.ValueType`-based value type.
[<Struct>]
type P = { X: int; Y: int }

let a = { X = 3; Y = 4 }
let b = { X = 3; Y = 4 }
let c = { X = 5; Y = 4 }

printfn "%d" a.X
printfn "%d" a.Y
printfn "%b" (a = b)
printfn "%b" (a = c)
printfn "%b" (hash a = hash b)

let d = { a with X = 10 }
printfn "%d" d.X
printfn "%d" d.Y
