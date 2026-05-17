module ExceptionSigs

exception Simple

exception OfData of int * string

[<NoComparison>]
exception WithAttrs of msg: string

[<NoEquality; NoComparison>]
exception MultipleAttrs of code: int * detail: string
