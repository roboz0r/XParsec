// List pattern with multiple record patterns separated by newlines (no ';').
// FSharpCheckerResults.fs uses this to match a fixed-length list by shape.

type Item = { Kind: string }

let classify items =
    match items with
    | [ { Kind = "Property" } as prop
        { Kind = "Field" }
        { Kind = "Field" } ] -> prop.Kind
    | _ -> "other"
