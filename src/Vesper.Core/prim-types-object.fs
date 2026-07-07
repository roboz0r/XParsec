namespace Vesper

#nowarn "42"

// Impl side: per-target intrinsic binding for the object root. Retarget here in
// one line; `objnull` mirrors the .fsi.

type obj = (# class "System.Object" #)
type objnull = obj | null
