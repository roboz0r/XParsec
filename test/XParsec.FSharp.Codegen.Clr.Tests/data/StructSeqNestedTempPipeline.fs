//#include _struct-seq-types.fs
//#include _struct-seq-combinators.fs
let xs = [| 1; 2; 3; 4 |]
// fully nested: NO stored `let s1` — the mapped seq is a temp
// sub-expression of the `total` initialiser.
let total = fold (fun acc x -> acc + x) 0 (map (fun x -> x + 1) (ofArray xs))
printfn "%d" total
