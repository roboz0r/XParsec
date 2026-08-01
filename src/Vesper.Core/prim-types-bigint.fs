namespace Vesper

#nowarn "42"

// Like `decimal`, `bigint` is not a CIL primitive: every body is a BCL CALL to the
// non-special-name sibling (`op_Addition` and friends are `SpecialName`, which the eager
// metadata walk filters out). `/` truncates toward zero and `%` takes the sign of the
// dividend — the same answers JS `BigInt` gives, which is what lets one golden judge both.

type bigint =
    (# "System.Numerics.BigInteger" #)
    with
        static member inline (+)(x: bigint, y: bigint) : bigint = System.Numerics.BigInteger.Add(x, y)
        static member inline (-)(x: bigint, y: bigint) : bigint = System.Numerics.BigInteger.Subtract(x, y)
        static member inline ( * )(x: bigint, y: bigint) : bigint = System.Numerics.BigInteger.Multiply(x, y)
        static member inline (/)(x: bigint, y: bigint) : bigint = System.Numerics.BigInteger.Divide(x, y)
        static member inline (%)(x: bigint, y: bigint) : bigint = System.Numerics.BigInteger.Remainder(x, y)
        static member inline (~-)(n: bigint) : bigint = System.Numerics.BigInteger.Negate(n)
    end
