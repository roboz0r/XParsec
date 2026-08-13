// A `[<Struct>]` record is laid out as a value on the CLR and erased to an ordinary object
// on JS, so `when 'a : struct` holds at it exactly where it holds at `int`.
[<Struct>]
type Point = { X: int; Y: int }

let onlyStruct<'a when 'a: struct> (x: 'a) = x

let p = onlyStruct { X = 1; Y = 2 }
ignore p
