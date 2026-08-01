namespace Vesper

#nowarn "42"

// `decimal` is not a CIL primitive: no mnemonic adds two of them, so every body here is a
// BCL CALL. The non-special-name siblings, not `op_Addition` — operator methods are
// `SpecialName` and the eager metadata walk filters them out.
//
// There is no `.js.fs` companion: JS has no decimal, so the type is unrepresentable there
// and these declarations are unreachable rather than separately gated.

type decimal =
    (# "System.Decimal" #)
    with
        static member inline (+)(x: decimal, y: decimal) : decimal = System.Decimal.Add(x, y)
        static member inline (-)(x: decimal, y: decimal) : decimal = System.Decimal.Subtract(x, y)
        static member inline ( * )(x: decimal, y: decimal) : decimal = System.Decimal.Multiply(x, y)
        static member inline (/)(x: decimal, y: decimal) : decimal = System.Decimal.Divide(x, y)
        static member inline (%)(x: decimal, y: decimal) : decimal = System.Decimal.Remainder(x, y)
        static member inline (~-)(n: decimal) : decimal = System.Decimal.Negate(n)
    end
