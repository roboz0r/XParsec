module Test

let items = [ (1, "a"); (2, "b"); (3, "c") ]

// Shorthand member access lambda (F# 8+)
let names = items |> List.map _.ToString()
let lengths = [ "hello"; "world" ] |> List.map _.Length
