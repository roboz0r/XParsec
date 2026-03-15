module Test

let a = printfn $"hello {name}"
let b = printfn $"value: {x + 1}"
let c = printfn $@"verbatim {path}"
let d = printfn $"""triple {value}"""
let e = f $"no holes"
let g =
    match x with
    | Some v -> sprintf $"got {v}"
    | None -> failwith $"missing {key}"
