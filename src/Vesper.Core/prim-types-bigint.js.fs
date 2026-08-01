namespace Vesper

#nowarn "42"

// JS `BigInt` is arbitrary-precision like the BCL's, so every operator is the bare JS
// operator — no mask (there is no width to wrap to) and no `checkedDivisor`: `/` truncates
// toward zero and `%` takes the sign of the dividend, which is what `BigInteger` does.

type bigint =
    (# "bigint" #)
    with
        static member inline (+)(x: bigint, y: bigint) : bigint = (# "$0 + $1" x y : bigint #)
        static member inline (-)(x: bigint, y: bigint) : bigint = (# "$0 - $1" x y : bigint #)
        static member inline ( * )(x: bigint, y: bigint) : bigint = (# "$0 * $1" x y : bigint #)
        static member inline (/)(x: bigint, y: bigint) : bigint = (# "$0 / $1" x y : bigint #)
        static member inline (%)(x: bigint, y: bigint) : bigint = (# "$0 % $1" x y : bigint #)
        static member inline (~-)(n: bigint) : bigint = (# "-$0" n : bigint #)
    end
