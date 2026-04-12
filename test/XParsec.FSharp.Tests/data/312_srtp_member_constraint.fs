module Test

// Statically Resolved Type Parameter (SRTP) member constraints
// From tasks.fs line 303, Query.fs line 137

let inline getAwaiter (task: ^T) =
    (^T: (member GetAwaiter: unit -> ^Awaiter) task)

let inline getResult (awaiter: ^T) =
    (^T: (member GetResult: unit -> 'TResult) awaiter)

let inline isCompleted (awaiter: ^T) =
    (^T: (member get_IsCompleted: unit -> bool) awaiter)

// Multiple SRTP constraints with new, struct, subtype
type Query() =
    member _.MaxBy<'T, 'Key when 'Key: equality and 'Key: comparison> (xs: 'T list, f: 'T -> 'Key) =
        xs |> List.maxBy f
