module MyModule

type Tree =
    | Leaf of int
    | Node of Forest
and Forest =
    | Empty
    | Cons of Tree * Forest
