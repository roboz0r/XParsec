namespace Vesper

#nowarn "42"

// Impl side: per-target intrinsic binding for the exception root. Retarget here
// in one line.

type exn = (# class "System.Exception" #)
