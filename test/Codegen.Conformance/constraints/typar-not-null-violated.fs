// `objnull` is `obj | null`, so `when 'a : not null` is refused here.
let notNull<'a when 'a: not null> (x: 'a) = x

let f (o: objnull) = notNull o
ignore f
