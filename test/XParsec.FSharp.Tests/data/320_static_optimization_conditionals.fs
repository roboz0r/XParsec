module Test

// Static optimization conditionals: (expr :?> 'T) when 'T: ConcreteType = alternateExpr
// This is internal FSharp.Core syntax for type-specialized dispatch
// From z.fs lines 51-61

let FromZero () : 'T =
    (get32 0 :?> 'T) when 'T: BigInteger = BigInteger.Zero

let FromOne () : 'T =
    (get32 1 :?> 'T) when 'T: BigInteger = BigInteger.One

let FromInt32 (value: int32) : 'T =
    (get32 value :?> 'T) when 'T: BigInteger = new BigInteger(value)

let FromInt64 (value: int64) : 'T =
    (FromInt64Dynamic value :?> 'T) when 'T: BigInteger = new BigInteger(value)
