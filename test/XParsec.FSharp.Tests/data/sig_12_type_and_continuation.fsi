module MutualTypes

type A =
    | A1 of int
    | A2 of B

and B =
    {
        name: string
        tag: A
    }

and [<NoComparison>] C =
    abstract member Process: A -> B -> unit

and D = A * B * C
