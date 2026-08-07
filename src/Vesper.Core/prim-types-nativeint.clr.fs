namespace Vesper

#nowarn "42"

type nativeint =
    (# "native int" #)
    with
        static member inline (+)(x: nativeint, y: nativeint) : nativeint = (# "add" x y : nativeint #)
        static member inline (-)(x: nativeint, y: nativeint) : nativeint = (# "sub" x y : nativeint #)
        static member inline ( * )(x: nativeint, y: nativeint) : nativeint = (# "mul" x y : nativeint #)
        static member inline (/)(x: nativeint, y: nativeint) : nativeint = (# "div" x y : nativeint #)
        static member inline (%)(x: nativeint, y: nativeint) : nativeint = (# "rem" x y : nativeint #)
        static member inline (~+)(value: nativeint) : nativeint = value
        static member inline (~-)(n: nativeint) : nativeint = (# "neg" n : nativeint #)
        static member inline (&&&)(x: nativeint, y: nativeint) : nativeint = (# "and" x y : nativeint #)
        static member inline (|||)(x: nativeint, y: nativeint) : nativeint = (# "or" x y : nativeint #)
        static member inline (^^^)(x: nativeint, y: nativeint) : nativeint = (# "xor" x y : nativeint #)
        static member inline (~~~)(value: nativeint) : nativeint = (# "not" value : nativeint #)
        static member inline (<<<)(value: nativeint, shift: int) : nativeint = (# "shl" value shift : nativeint #)
        static member inline (>>>)(value: nativeint, shift: int) : nativeint = (# "shr" value shift : nativeint #)
    end

type unativeint =
    (# "unsigned native int" #)
    with
        static member inline (+)(x: unativeint, y: unativeint) : unativeint = (# "add" x y : unativeint #)
        static member inline (-)(x: unativeint, y: unativeint) : unativeint = (# "sub" x y : unativeint #)
        static member inline ( * )(x: unativeint, y: unativeint) : unativeint = (# "mul" x y : unativeint #)
        static member inline (/)(x: unativeint, y: unativeint) : unativeint = (# "div.un" x y : unativeint #)
        static member inline (%)(x: unativeint, y: unativeint) : unativeint = (# "rem.un" x y : unativeint #)
        static member inline (~+)(value: unativeint) : unativeint = value
        static member inline (&&&)(x: unativeint, y: unativeint) : unativeint = (# "and" x y : unativeint #)
        static member inline (|||)(x: unativeint, y: unativeint) : unativeint = (# "or" x y : unativeint #)
        static member inline (^^^)(x: unativeint, y: unativeint) : unativeint = (# "xor" x y : unativeint #)
        static member inline (~~~)(value: unativeint) : unativeint = (# "not" value : unativeint #)
        static member inline (<<<)(value: unativeint, shift: int) : unativeint = (# "shl" value shift : unativeint #)
        static member inline (>>>)(value: unativeint, shift: int) : unativeint = (# "shr.un" value shift : unativeint #)
    end
type nativeptr<'T when 'T : unmanaged> = (# "native int" #)
type voidptr = (# "void*" #)
type ilsigptr<'T> = (# "!0*" #)
