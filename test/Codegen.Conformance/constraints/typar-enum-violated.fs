// `int` is the underlying type of an enum, not an enum, so `when 'a : enum<int>` is refused
// here.
let onlyEnum<'a when 'a: enum<int>> (x: 'a) = x

let i = onlyEnum 1
ignore i
