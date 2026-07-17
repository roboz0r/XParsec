namespace Vesper

#nowarn "42"

type nativeint = (# "native int" #)
type unativeint = (# "unsigned native int" #)
type nativeptr<'T when 'T : unmanaged> = (# "native int" #)
type voidptr = (# "void*" #)
type ilsigptr<'T> = (# "!0*" #)
