open Vesper.Collections
let xs = [| 1; 2; 3; 4 |]
let total = StructSeq.fold (fun a x -> a + x) 0 (StructSeq.map (fun x -> x + 1) (StructSeq.ofArray xs))
printfn "%d" total
