namespace My.Lib

[<AutoOpen>]
module internal Helpers =
    [<CompiledName("Run")>]
    val internal run: x: int -> int

    val mutable counter: int
