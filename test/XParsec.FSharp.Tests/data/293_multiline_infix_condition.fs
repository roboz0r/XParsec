module Test

let f x y =
    if x > 0 &&
        (match y with
         | Some _ -> true
         | None -> false) then
        1
    else
        0

let g a b =
    a > 0 &&
    b > 0

let h x =
    x = // comment
        42
