namespace Vesper

type decimal =
    (# "System.Decimal" #)
    with
        static member inline (+)(x: decimal, y: decimal) : decimal = System.Decimal.Add(x, y)
        static member inline (-)(x: decimal, y: decimal) : decimal = System.Decimal.Subtract(x, y)
        static member inline ( * )(x: decimal, y: decimal) : decimal = System.Decimal.Multiply(x, y)
        static member inline (/)(x: decimal, y: decimal) : decimal = System.Decimal.Divide(x, y)
        static member inline (%)(x: decimal, y: decimal) : decimal = System.Decimal.Remainder(x, y)
        static member inline (~+)(value: decimal) : decimal = value
        static member inline (~-)(n: decimal) : decimal = System.Decimal.Negate(n)
    end
