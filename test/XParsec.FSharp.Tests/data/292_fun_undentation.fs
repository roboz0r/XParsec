module Test

let f items =
    (0, items)
    ||> List.fold (fun acc item ->
        let result = acc + item
        result)

let g items =
    items
    |> List.map (fun x ->
        let y = x + 1
        y)
