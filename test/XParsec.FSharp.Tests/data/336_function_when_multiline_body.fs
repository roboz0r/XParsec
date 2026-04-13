// `function` arm with a multi-line `when` guard where the body `->`
// continues on the next line at a column less than the pattern position.
// TypedTreeOps.fs uses this idiom in piped-function applications.

type Info = { Flag: bool; Name: string }

let isEmpty g =
    g
    |> (function { Flag = flag; Name = name } when (not flag) || (name = "")
                -> true | _ -> false)
