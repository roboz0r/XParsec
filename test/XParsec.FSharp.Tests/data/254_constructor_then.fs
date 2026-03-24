module Test

// Secondary constructor with 'then' block
type MyCollection(capacity: int) =
    let items = ResizeArray<int>(capacity)

    new(entries: seq<int>) as this =
        MyCollection(16)
        then entries |> Seq.iter (fun e -> this.Add(e))

    member _.Add(x) = items.Add(x)
    member _.Count = items.Count
