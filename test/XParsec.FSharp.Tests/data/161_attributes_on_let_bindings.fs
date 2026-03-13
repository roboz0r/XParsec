module Test161

[<Literal>]
let MaxSize = 100

[<System.Obsolete("Use newFunc")>]
let oldFunc x = x + 1

[<System.Obsolete>]
let rec factorial n =
    if n <= 1 then 1
    else n * factorial (n - 1)
