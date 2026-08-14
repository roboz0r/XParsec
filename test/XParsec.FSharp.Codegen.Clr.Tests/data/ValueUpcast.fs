// Three CLR value types with nothing in common at the front end: `unit` reprs as
// `System.ValueTuple`, a tuple as `System.ValueTuple`n`, an enum as a `System.Enum` subclass.
// Reaching `obj` through any of them must box — without it the raw value sits where a
// reference is required, which is invalid IL the runtime rejects at JIT.
type Color =
    | Red = 1
    | Green = 2

let unitAsObj (u: unit) : obj = u :> obj
let tupleAsObj (t: int * int) : obj = t :> obj
let enumAsObj (c: Color) : obj = c :> obj

let describeUnit () : string = (unitAsObj ()).ToString()
let describeTuple () : string = (tupleAsObj (3, 4)).ToString()
let describeEnum () : string = (enumAsObj Color.Red).ToString()
