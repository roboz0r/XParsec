namespace Vesper

#nowarn "42"

// Impl side: per-target intrinsic binding for the arbitrary-precision integer
// primitive. Retarget here in one line.

type bigint = (# "System.Numerics.BigInteger" #)
