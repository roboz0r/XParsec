// `string` declares no parameterless constructor on either target, so
// `when 'a : (new : unit -> 'a)` is refused here.
let construct<'a when 'a: (new: unit -> 'a)> (x: 'a) = x

let s = construct "x"
ignore s
