type Bag = | Items of int list

let rec sumList (xs: int list) : int =
    match xs with
    | [] -> 0
    | h :: t -> h + sumList t

let count (b: Bag) : int =
    match b with
    | Items xs -> sumList xs

let direct = Items [ 1; 1; 1 ]
let total = Items [ 1; 2; 3 ]
printfn "%d" (count direct)
printfn "%d" (count total)
