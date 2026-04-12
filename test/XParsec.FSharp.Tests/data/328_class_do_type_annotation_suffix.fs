module Test

// Type annotation suffix on class-level `do` binding body
// F# grammar: ClassFunctionOrValueDefn `do` body is typedSeqExprBlock

type Foo() =
    do printfn "init" : unit

    static do printfn "static init" : unit
