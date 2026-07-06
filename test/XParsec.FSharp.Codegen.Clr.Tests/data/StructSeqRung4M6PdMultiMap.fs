//#include _struct-seq-types.fs
//#include _struct-seq-combinators.fs
let xs = [| 1; 2; 3; 4 |]
let s0 = ofArray xs
let s1 = map (fun x -> x + 1) s0
let s2 = map (fun x -> x * 2) s1
let total = fold (fun acc x -> acc + x) 0 s2
printfn "%d" total
