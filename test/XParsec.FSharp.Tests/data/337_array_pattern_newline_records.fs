// Array pattern with multiple record patterns separated by newlines (no ';').
// Parity with 332_list_pattern_newline_records.fs for the [| ... |] form.

type Item = { Kind: string }

let classify items =
    match items with
    | [| { Kind = "Property" } as prop
         { Kind = "Field" }
         { Kind = "Field" } |] -> prop.Kind
    | _ -> "other"
