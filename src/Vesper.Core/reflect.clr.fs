namespace Vesper

#nowarn "42"

type Type = (# "System.Type" #)

[<AutoOpen>]
module TypeIntrinsics =

    let inline typeof<'T> : Type = (# "ldtoken" type ('T) : Type #)

    let inline typedefof<'T> : Type = (# "ldtokendef" type ('T) : Type #)
