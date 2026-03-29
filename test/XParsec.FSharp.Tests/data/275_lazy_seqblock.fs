module Test

let x =
    lazy
        let y = 1
        y + 1

let z: seq<int> =
    upcast
        [| 1; 2; 3 |]

let w: obj =
    downcast
        box 42

let a = lazy (1 + 2)
