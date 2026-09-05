// A reference record is a heap object on both targets, so `when 'a : unmanaged` is refused
// here.
type Box = { Item: int }

let onlyUnmanaged<'a when 'a: unmanaged> (x: 'a) = x

let b = onlyUnmanaged { Item = 1 }
ignore b
