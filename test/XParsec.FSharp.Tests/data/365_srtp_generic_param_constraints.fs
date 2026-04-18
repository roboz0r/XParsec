// Member declaration whose generic parameters include a SRTP type parameter
// `^Value` followed by a multi-line `when ... and ... and ...` constraint
// clause inside the `<...>` brackets, plus a `default` constraint.
//
// Canonical form from Query.fs (lines 143-150):
//
//   member inline _.SumByNullable<'T, 'Q, ^Value
//                                     when ^Value :> ValueType
//                                     and ^Value : struct
//                                     and ^Value : (new : unit -> ^Value)
//                                     and ^Value : (static member ( + ) : ^Value * ^Value -> ^Value)
//                                     and default ^Value : int>
//                 (source: QuerySource<'T, 'Q>) = ...
//
// Exercises: SRTP typars (`^Value`), member-shape constraints
// `(static member Name : Sig)`, `default` constraint keyword, and multi-line
// constraint continuation inside type-parameter brackets.

type Builder =
    member inline _.SumByNullable< 'T, 'Q, ^Value
                                       when ^Value :> System.ValueType
                                       and ^Value : struct
                                       and ^Value : (new : unit -> ^Value)
                                       and ^Value : (static member ( + ) : ^Value * ^Value -> ^Value)
                                       and ^Value : (static member Zero : ^Value)
                                       and default ^Value : int>
                  (source: QuerySource< 'T, 'Q >) : System.Nullable< ^Value > =
        System.Nullable ()
