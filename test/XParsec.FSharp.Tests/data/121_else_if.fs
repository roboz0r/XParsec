module M

let f x y =
    if x then 1
    else
    if y then 2
    elif x && y then 3
    else 4
