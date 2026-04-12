module Test

// Type annotation suffix on class member bodies (parsed outside Binding.parse)
// F# grammar: member binding body is typedSeqExprBlock

type Foo =
    // Property: static member Name = expr
    static member A = 1 + 2 : int

    // Property with explicit return type: static member Name : T = expr
    static member B : int = 1 + 2 : int

    // Method: static member Name args = expr
    static member C x = x + 1 : int
