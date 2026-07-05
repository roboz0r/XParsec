module Widgets

// W9 Stage 1a — the `.fs` host for a concrete member surface on an intrinsic /
// `extern` type: an inline-IL type abbreviation augmented with a `(# … #)`-bodied
// member. The trailing `with member …` was previously dropped by the abbreviation
// branch of `parseAbbrevOrImplicitClass`.
type widget =
    (# "object" #)
    with
        member _.Poke (x: int) : int = (# "$0 + 1" x : int #)
    end

// A plain abbreviation with no augmentation is unaffected (the common case).
type alias = (# "System.Int32" #)
