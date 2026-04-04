module Test

// Statically resolved type parameters ^T
// From tasks.fs lines 294-299

type MyBuilder() =

    [<NoEagerConstraintApplication>]
    static member inline BindDynamic< ^TaskLike, 'TResult, ^Awaiter
        when ^TaskLike: (member GetAwaiter: unit -> ^Awaiter)
        and ^Awaiter: (member get_IsCompleted: unit -> bool)
        and ^Awaiter: (member GetResult: unit -> 'TResult)>
        (task: ^TaskLike, cont: 'TResult -> int) =
        42
