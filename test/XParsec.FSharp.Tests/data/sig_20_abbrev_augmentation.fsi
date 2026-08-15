module AbbrevAugmentation

module Intrinsic =
    type Shown = int

        with

            member Doubled: int
            static member Make: int -> Shown

    val shown: Shown -> int

module Generic =
    type Pair<'T> = 'T * 'T with

        member Swapped: Pair<'T>

    val swap: Pair<'T> -> Pair<'T>
