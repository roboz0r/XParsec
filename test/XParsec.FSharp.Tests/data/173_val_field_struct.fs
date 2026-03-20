[<Struct>]
type Foo =
    val private value: uint64
    private new(value: uint64) = { value = value }

    static member Create(tokenValue: int, startIndex: int) =
        let indexPart = uint64 startIndex <<< 16
        let tokenPart = uint64 tokenValue
        Foo(indexPart ||| tokenPart)

    member this.Token: int =
        int this.value

    member this.StartIndex: int = int (this.value >>> 16)
