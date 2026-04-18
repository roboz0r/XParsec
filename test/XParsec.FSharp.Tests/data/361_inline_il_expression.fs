// Inline IL expression syntax `(# "instr" args : type #)` used pervasively in
// FSharp.Core for generic primitive operations (e.g. prim-types.fs).
//
// Two forms exercised:
//   1. Bare emission: `(# "cgt" x y : bool #)` — CIL instruction applied to
//      args, annotated with return type.
//   2. Reinterpret cast: `(# "" x : DateTime #)` — empty instruction string
//      for bit-identity reinterpret.
//
// Also exercised via the `when 'T : Type = expr` static optimization clauses
// that feed these into `let inline ...` bodies.
//
// Affects: prim-types.fs (13 errors).

let inline GenericEqualityFast (x: 'T) (y: 'T) =
    GenericEqualityIntrinsic x y
    when 'T : bool     = (# "ceq" x y : bool #)
    when 'T : int32    = (# "ceq" x y : bool #)
    when 'T : float    =
        if (# "ceq" x y : bool #) then
            true
        else
            not (# "ceq" x x : bool #) && not (# "ceq" y y : bool #)
    when 'T : string   = System.String.Equals((# "" x : string #), (# "" y : string #))
