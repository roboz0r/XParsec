namespace Vesper

[<AutoOpen>]
module LogicalOperators =
    let inline (&&) (e1: bool) ([<CallAtMostOnce>] e2: bool) : bool = if e1 then e2 else false
    let inline (||) (e1: bool) ([<CallAtMostOnce>] e2: bool) : bool = if e1 then true else e2


[<AutoOpen>]
module CompositionOperators =
    let inline (>>) (func1: 'T1 -> 'T2) (func2: 'T2 -> 'T3) : 'T1 -> 'T3 = fun x -> func2 (func1 x)

    let inline (<<) (func2: 'T2 -> 'T3) (func1: 'T1 -> 'T2) : 'T1 -> 'T3 = fun x -> func2 (func1 x)

    let inline (|>) (arg: 'T1) (func: 'T1 -> 'U) : 'U = func arg

    let inline (||>) (arg1: 'T1, arg2: 'T2) (func: 'T1 -> 'T2 -> 'U) : 'U = func arg1 arg2

    let inline (|||>) (arg1: 'T1, arg2: 'T2, arg3: 'T3) (func: 'T1 -> 'T2 -> 'T3 -> 'U) : 'U = func arg1 arg2 arg3

    let inline (<|) (func: 'T -> 'U) (arg1: 'T) : 'U = func arg1

    let inline (<||) (func: 'T1 -> 'T2 -> 'U) (arg1: 'T1, arg2: 'T2) : 'U = func arg1 arg2

    let inline (<|||) (func: 'T1 -> 'T2 -> 'T3 -> 'U) (arg1: 'T1, arg2: 'T2, arg3: 'T3) : 'U = func arg1 arg2 arg3

[<AutoOpen>]
module EnumOperators =

    let inline enum (value: int32) : ^U = LanguagePrimitives.EnumOfValue value
