open System.Collections
open System.Runtime.CompilerServices

let formatTuple (o: obj) : string =
    match o with
    | :? ITuple as t ->
        let mutable acc = "("

        for i in 0 .. t.Length - 1 do
            if i > 0 then
                acc <- acc + ", "

            acc <- acc + (t.[i]).ToString()

        acc + ")"
    | _ -> "?"

let formatEnum (o: obj) : string =
    match o with
    | :? IEnumerable as xs ->
        let mutable acc = "["
        let mutable first = true

        for item in xs do
            if not first then
                acc <- acc + "; "

            acc <- acc + item.ToString()
            first <- false

        acc + "]"
    | _ -> "?"

printfn "%s" (formatTuple (box (1, 2)))
printfn "%s" (formatEnum (box [| 1; 2; 3 |]))
