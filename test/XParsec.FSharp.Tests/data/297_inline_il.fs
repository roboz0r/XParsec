module Test

// Inline IL assembly (# ... #) — basic cases
let inline zero () : int = (# "ldc.i4.0" : int #)
let inline isNullBasic (p: nativeint) : bool = (# "ceq" 0n p : bool #)
let inline initObj (address: nativeint) = (# "initobj !0" type(int) address #)

// IL arg with type-instantiated identifier: nullPtr<'T>
// The IL arg parser only accepts atomic exprs, but ident<'T> requires full Pratt parsing.
// From nativeptr.fs line 69: (# "ceq" nullPtr<'T> address : bool #)
let inline myNullPtr<'T> : nativeint = (# "" 0n : nativeint #)
let inline isNullPtr (address: nativeint) = (# "ceq" myNullPtr<int> address : bool #)
