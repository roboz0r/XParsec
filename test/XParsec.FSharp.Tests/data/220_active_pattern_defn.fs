module Test

let (|Last|) xs = List.last xs
let (|Even|Odd|) n = if n % 2 = 0 then Even else Odd
let (|PointFree|) = List.last
