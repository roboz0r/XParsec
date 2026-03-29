module Test

[<System.Obsolete "Use newMethod instead">]
let oldMethod x = x

[<System.Diagnostics.DebuggerDisplay "Count = {Count}">]
type Foo() =
    member _.Count = 1
