namespace Vesper

#nowarn "42" // This construct is deprecated: it is only for use in the F# library

type int =
    (# "System.Int32" #)
    with
        static member inline (+)(x: int, y: int) : int = (# "add" x y : int #)
        static member inline (-)(x: int, y: int) : int = (# "sub" x y : int #)
        static member inline ( * )(x: int, y: int) : int = (# "mul" x y : int #)
        static member inline (/)(x: int, y: int) : int = (# "div" x y : int #)
        static member inline (%)(x: int, y: int) : int = (# "rem" x y : int #)
        static member inline (~-)(n: int) : int = (# "neg" n : int #)
        static member inline (&&&)(x: int, y: int) : int = (# "and" x y : int #)
        static member inline (|||)(x: int, y: int) : int = (# "or" x y : int #)
        static member inline (^^^)(x: int, y: int) : int = (# "xor" x y : int #)
        static member inline (~~~)(value: int) : int = (# "not" value : int #)
        static member inline (<<<)(value: int, shift: int) : int = (# "shl" value shift : int #)
        // Signed width: arithmetic (sign-extending) `shr`.
        static member inline (>>>)(value: int, shift: int) : int = (# "shr" value shift : int #)
    end
type bool = (# "System.Boolean" #)
type unit = (# "System.ValueTuple" #)
type 'T ``[]`` = (# "!0[]" #)
type 'T array = 'T[]

type Fun<'A, 'B> =
    abstract member Invoke: arg: 'A -> 'B

type Fun<'A, 'B, 'C> =
    abstract member Invoke: a: 'A * b: 'B -> 'C

type Fun<'A, 'B, 'C, 'D> =
    abstract member Invoke: a: 'A * b: 'B * c: 'C -> 'D

type Fun<'A, 'B, 'C, 'D, 'E> =
    abstract member Invoke: a: 'A * b: 'B * c: 'C * d: 'D -> 'E
