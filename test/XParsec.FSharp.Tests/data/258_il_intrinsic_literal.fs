module Test

// IL inline assembly / intrinsic literal syntax
let inline ofNativeInt (address: nativeint) = (# "" address : nativeptr<int> #)

type ``[]``<'T> = (# "!0[]" #)
