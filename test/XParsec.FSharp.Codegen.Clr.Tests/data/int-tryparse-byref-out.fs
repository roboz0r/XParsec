open System

let parse (s: string) : int =
    let mutable r = 0
    let ok = Int32.TryParse(s, &r)
    if ok then r else -1

printfn "%d" (parse "123")
printfn "%d" (parse "oops")
