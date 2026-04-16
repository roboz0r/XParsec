// Generic constraints using `:>` (subtype) and `(new: unit -> 'T)` (default
// constructor), combined with struct and comparison constraints. Also exercises
// `Nullable<'Key>` in method return type.
// Affects: Query.fs
open System

type T() =
    member _.MinByNullable<'T, 'Q, 'Key
                            when 'Key: equality
                            and 'Key: comparison
                            and 'Key: (new: unit -> 'Key)
                            and 'Key: struct
                            and 'Key:> ValueType> (x: 'T, selector: 'T -> Nullable<'Key>) =
        selector x
