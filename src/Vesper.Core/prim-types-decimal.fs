namespace Vesper

#nowarn "42"

// Impl side: per-target intrinsic binding for the decimal primitive. Retarget
// here in one line.

type decimal = (# "System.Decimal" #)
