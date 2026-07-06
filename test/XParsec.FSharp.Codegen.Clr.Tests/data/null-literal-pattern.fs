let describe (s: string) =
    match s with
    | null -> "null"
    | _ -> "value"

printfn "%s" (describe null)
printfn "%s" (describe "hi")
