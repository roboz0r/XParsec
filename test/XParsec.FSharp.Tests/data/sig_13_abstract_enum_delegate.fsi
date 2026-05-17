namespace Units

[<Measure>] type kg
[<Measure>] type m
[<Measure>] type s

type Opaque

type Color =
    | Red = 0
    | Green = 1
    | Blue = 2

type Callback = delegate of sender: obj * args: System.EventArgs -> unit

type BinaryOp<'T> = delegate of 'T * 'T -> 'T
