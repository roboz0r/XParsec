module Test

type Foo =
    {
        Root : int
    }

    static member Empty : Foo = { Root = 0 }
    member this.GetValue (x: int) : int = this.Root + x
