// `string` is a reference type on both targets, so `when 'a : struct` is refused here.
let onlyStruct<'a when 'a: struct> (x: 'a) = x

let v = onlyStruct "x"
ignore v
