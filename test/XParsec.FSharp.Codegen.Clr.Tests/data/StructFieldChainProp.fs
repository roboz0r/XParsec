type List<'T> =
    | ([]): List<'T>
    | (::): Head: 'T * Tail: List<'T> -> List<'T>
    member this.IsEmpty = match this with | [] -> true | _ -> false
and 'T list = List<'T>
[<Struct>]
type Wrap =
    val mutable Stack: int list
    new(n: int) = { Stack = (if n = 0 then [] else n :: []) }
    member this.NotEmpty() = not this.Stack.IsEmpty
