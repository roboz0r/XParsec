// `when 'a : null` at a union that HAS a `null` member.
let onlyNull<'a when 'a: null> (x: 'a) = x

let f (s: string | null) = onlyNull s
ignore f
