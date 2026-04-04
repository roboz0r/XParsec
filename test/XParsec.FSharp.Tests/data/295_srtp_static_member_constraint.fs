module Test

// SRTP constraint: 'T: (static member op: ...)
// From collections.fs line 27

let inline eq<'T when 'T: equality and 'T: (static member (=): 'T * 'T -> bool)> (x: 'T) (y: 'T) =
    LanguagePrimitives.GenericEquality x y

type Comparer<'T when 'T: (static member (=): 'T * 'T -> bool)>() =
    member _.Equals(x: 'T, y: 'T) = x = y
