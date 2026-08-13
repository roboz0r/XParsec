// `when 'a : not null` at bare primitives, which carry no `null` member on either target.
let notNull<'a when 'a: not null> (x: 'a) = x

let i = notNull 42
let s = notNull "x"
ignore i
ignore s
