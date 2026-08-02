namespace Vesper

type bigint =
    (# "System.Numerics.BigInteger" #)
    with
        static member inline (+)(x: bigint, y: bigint) : bigint = System.Numerics.BigInteger.Add(x, y)
        static member inline (-)(x: bigint, y: bigint) : bigint = System.Numerics.BigInteger.Subtract(x, y)
        static member inline ( * )(x: bigint, y: bigint) : bigint = System.Numerics.BigInteger.Multiply(x, y)
        static member inline (/)(x: bigint, y: bigint) : bigint = System.Numerics.BigInteger.Divide(x, y)
        static member inline (%)(x: bigint, y: bigint) : bigint = System.Numerics.BigInteger.Remainder(x, y)
        static member inline (~+)(value: bigint) : bigint = value
        static member inline (~-)(n: bigint) : bigint = System.Numerics.BigInteger.Negate(n)
    end
