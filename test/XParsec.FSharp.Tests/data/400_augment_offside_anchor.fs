module AugmentOffside

module Siblings =
    type Shown = { value: int }

    // Left of the representation, so this ends the type rather than augmenting it.
    let shown (x: int) = x

module Members =
    type Augmented =
        { value: int }
        // At the representation's column, so these ARE members.
        member this.Doubled = this.value * 2
            member this.Indented = this.value + 1
        static member Make(v: int) = { value = v }

    let make (v: int) = Augmented.Make v

module Cases =
    type Rankged =
        | A
        | B

        member this.Rank =
            match this with
            | A -> 0
            | B -> 1

    let tag (t: Rankged) = t.Rank
