// `when 'a : not struct` at a reference primitive. Nothing is printed: the constraint
// holding has no result, so this is an `accept` row.
let onlyRef<'a when 'a: not struct> (x: 'a) = x

let s = onlyRef "x"
ignore s
