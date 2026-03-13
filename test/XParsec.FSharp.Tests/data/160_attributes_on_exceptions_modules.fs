module Test160

module Outer =
    [<System.Serializable>]
    exception MyError of message: string * code: int

    [<AutoOpen>]
    module Inner =
        let x = 1

    [<RequireQualifiedAccess>]
    module Qualified =
        let y = 2

[<System.Serializable>]
exception SimpleError of string
