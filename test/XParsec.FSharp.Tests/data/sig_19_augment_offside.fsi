module Offside

module Siblings =
    type Shown = { value: int }

    // A `val` left of the representation is a MODULE val, not a member of `Shown`.
    val shown: int -> int

    val named: x: int -> int

    [<CompiledName("Attributed")>]
    val attributed: int -> int

module Members =
    type Augmented =
        { value: int }
        // At the representation's column, so these ARE members.
        member Doubled: int
            member Indented: int
        static member Make: int -> Augmented

    val make: int -> Augmented

module Cases =
    type Tagged =
        | A
        | B

        member Tag: int

    val tag: Tagged -> int
