// `when 'a : not struct` at a reference primitive.
let onlyRef<'a when 'a: not struct> (x: 'a) = x

let s = onlyRef "x"
ignore s
