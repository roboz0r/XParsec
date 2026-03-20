module Test

[<Literal>]
let KindSpecial = 0xE000us

module Special =
    [<Literal>]
    let EOF = 1us

type Token =
    | None = 0us
    | EOF = (KindSpecial ||| Special.EOF)
