namespace Vesper

#nowarn "42"

type char = (# "System.Char" #)

type string =
    (# "System.String" #)
    with
        // Not `op_Addition`: the metadata walk filters `SpecialName` methods out.
        static member inline (+)(x: string, y: string) : string = System.String.Concat(x, y)

        /// Named `get_Chars` on the BCL side, and spelled that way here: `this.[index]`
        /// would resolve back into this very accessor.
        member inline this.Item
            with get (index: int) : char = this.get_Chars(index)
    end
