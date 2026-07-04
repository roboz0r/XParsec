printfn "%*d" 5 42
printf "%-*d" 5 42
printfn "%0*d" 5 42
printfn "%+*d" 5 42
printfn "%.*f" 1 123.456
printfn "%*.*f" 12 1 123.456
printfn "%*A" 1 [ 1; 2; 3 ]
let g () = printf "%*d"
let s = sprintf "%*s" 10 "hi"
