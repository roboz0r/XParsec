namespace Vesper

#nowarn "42"

// Impl side: per-target intrinsic bindings for the native-int and pointer
// primitives. Pointer bodies use the IL shapes, as in FSharp.Core.

type nativeint = (# "native int" #)
type unativeint = (# "unsigned native int" #)
type nativeptr<'T when 'T : unmanaged> = (# "native int" #)
type voidptr = (# "void*" #)
type ilsigptr<'T> = (# "!0*" #)
