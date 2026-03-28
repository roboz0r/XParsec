module TestOptionalArg

let f (?x: int) = x

let g () =
    someMethod(layout, xml, ?typeMapping=typeMapping, ?symbol=symbol)

type MyClass() =
    member _.Method(?label: string, ?count: int) =
        defaultArg label "default"
