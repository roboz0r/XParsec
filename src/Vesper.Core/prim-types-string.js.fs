namespace Vesper

#nowarn "42"

type char = (# "string" #)

type string =
    (# "string" #)
    with
        static member inline (+)(x: string, y: string) : string = (# "$0 + $1" x y : string #)
    end
