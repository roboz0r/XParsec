namespace Vesper

#nowarn "42"

type char = (# "string" #)

type string =
    (# "string" #)
    with
        static member inline (+)(x: string, y: string) : string = (# "$0 + $1" x y : string #)

        /// A JS string is indexable and `char` is a length-1 string, so `s[i]` answers directly.
        member inline this.Item
            with get (index: int) : char = (# "$0[$1]" this index : char #)
    end
