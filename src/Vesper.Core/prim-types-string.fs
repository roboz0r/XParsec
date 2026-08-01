namespace Vesper

#nowarn "42"

type char = (# "System.Char" #)

type string =
    (# "System.String" #)
    with
        // The BCL sibling, not `op_Addition`: operator methods are `SpecialName` and the
        // eager metadata walk filters them out.
        static member inline (+)(x: string, y: string) : string = System.String.Concat(x, y)
    end
