module Test169

type R = { A: int; B: string }

let f x =
    match x with
    | {
            A = a
            B = b
        } -> true
    | _ -> false
