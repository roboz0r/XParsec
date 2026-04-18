module Repro

// FSharp.Core-internal surface syntax: DU cases with explicit type
// signatures (GADT-style). Each case is written as `: returnType` for
// nullary cases or `: name:argType -> returnType` for cases with a
// single named argument. See FSharp.Core/prim-types.fs Option<'T>.

type Option<'T> =
    | None :       'T option
    | Some : Value:'T -> 'T option

type Choice<'T1, 'T2> =
    | Choice1Of2 : 'T1 -> Choice<'T1, 'T2>
    | Choice2Of2 : 'T2 -> Choice<'T1, 'T2>
