module Test

type Trampoline() =
    static let x = 1
    static member X = x
    let mutable y = 0
    let mutable z = 0
    member _.Y = y
    member _.Z = z
