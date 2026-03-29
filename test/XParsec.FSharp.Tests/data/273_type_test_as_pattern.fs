module Test

type Wrapper = Wrapper of string

let test (x: obj, y: obj) =
    match x, y with
    | Wrapper this, (:? Wrapper as Wrapper other) -> true
    | :? string as s -> false
    | _ -> false
