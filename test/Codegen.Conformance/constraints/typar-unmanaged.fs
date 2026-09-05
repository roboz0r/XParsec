// `when 'a : unmanaged` instantiated at a scalar, an enum, a struct record of scalars and a
// tuple of scalars. The CLR lays a tuple out as a `ValueTuple`, so it holds there; JS lays
// out no value type, so the record refuses it.
type Colour =
    | Red = 1
    | Green = 2

[<Struct>]
type Point = { X: int; Y: int }

let onlyUnmanaged<'a when 'a: unmanaged> (x: 'a) = x

let i = onlyUnmanaged 42
let e = onlyUnmanaged Colour.Red
let p = onlyUnmanaged { X = 1; Y = 2 }
let t = onlyUnmanaged (1, 2)
ignore i
ignore e
ignore p
ignore t
