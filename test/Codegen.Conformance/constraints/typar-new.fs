// `when 'a : (new : unit -> 'a)` instantiated at a class declaring a parameterless
// constructor, which both targets accept.
type Counter() =
    member _.Count = 0

let construct<'a when 'a: (new: unit -> 'a)> (x: 'a) = x

let c = construct (Counter())
ignore c
