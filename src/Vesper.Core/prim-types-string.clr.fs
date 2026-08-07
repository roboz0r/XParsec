namespace Vesper

#nowarn "42"

type char = (# "System.Char" #)

type string =
    (# "System.String" #)
    with
        // Not `op_Addition`: the metadata walk filters `SpecialName` methods out.
        static member inline (+)(x: string, y: string) : string = System.String.Concat(x, y)
    end
