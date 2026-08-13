// `int` is a value type on both targets, so `when 'a : not struct` is refused here.
let onlyRef<'a when 'a: not struct> (x: 'a) = x

let v = onlyRef 1
ignore v
