// Tuple pattern followed by an inline block comment and then `->`. The prior
// match arm has a multi-line body ending with a list literal of tuples.
// Affects: ServiceDeclarationLists.fs
type Item = { Name: string }

let split xs =
    xs
    |> List.map
        (function
            | _, ([_] as items)
            | _, items when List.length items = 1 ->
                let item = List.head items
                [ item.Name, item.Name, items ]
            | _, items (* RFC-1137 we have both property and extension method ...*) ->
                let toSplit, together = List.partition (fun _ -> true) items
                [ for item in toSplit -> item.Name, item.Name, together ])
