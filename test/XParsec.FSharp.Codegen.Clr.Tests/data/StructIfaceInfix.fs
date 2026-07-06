type List<'T> =
    | ([]): List<'T>
    | (::): Head: 'T * Tail: List<'T> -> List<'T>
and 'T list = List<'T>
type Node(h: int) =
    member this.Height = h
[<Struct>]
type Iter =
    val mutable Stack: Node list
    val mutable Hit: bool
    new(n: Node) = { Stack = n :: []; Hit = false }
    interface System.Collections.IEnumerator with
        member this.Current = box 0
        member this.MoveNext() =
            match this.Stack with
            | [] -> false
            | t :: rest ->
                if t.Height = 1 then
                    this.Stack <- rest
                    this.Hit <- true
                    true
                else
                    false
        member this.Reset() = ()
