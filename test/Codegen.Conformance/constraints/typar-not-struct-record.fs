// A `[<Struct>]` record is laid out as a value on the CLR, so `when 'a : not struct` is
// refused there; on JS it erases to an ordinary object and the constraint holds.
[<Struct>]
type Point = { X: int; Y: int }

let onlyRef<'a when 'a: not struct> (x: 'a) = x

let p = onlyRef { X = 1; Y = 2 }
ignore p
