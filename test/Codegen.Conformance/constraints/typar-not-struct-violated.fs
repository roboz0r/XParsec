// `int` is a value type on the CLR, so `when 'a : not struct` is refused there; on JS it is
// a `number` like every other numeric, and the constraint holds.
let onlyRef<'a when 'a: not struct> (x: 'a) = x

let v = onlyRef 1
ignore v
