module Test

// Explicit type parameters in let/value bindings
// From nativeptr.fs line 65: let inline nullPtr<'T when 'T : unmanaged> : nativeptr<'T> = ...

// Generic value with explicit type param
let myNullValue<'T> : 'T option = None

// With constraint
let inline myDefault<'T when 'T : struct> : 'T = Unchecked.defaultof<'T>

// Explicit type arg on identifier in expression context
// From nativeptr.fs: (# "ceq" nullPtr<'T> address : bool #)
let getValue<'T> (arr: 'T[]) (i: int) : 'T = arr.[i]
