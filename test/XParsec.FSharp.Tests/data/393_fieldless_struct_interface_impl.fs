module M

type Fun<'a, 'b> =
    abstract member Invoke: 'a -> 'b

[<Struct>]
type AddOne =
    interface Fun<int, int> with
        member _.Invoke(x: int) : int = x + 1
