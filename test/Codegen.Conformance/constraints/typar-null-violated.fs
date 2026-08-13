// `null` is a union MEMBER, so a bare `string` does not admit it on either target and
// `when 'a : null` is refused here.
let onlyNull<'a when 'a: null> (x: 'a) = x

let v = onlyNull "x"
ignore v
