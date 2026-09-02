namespace Test

// A type abbreviation that closes an indented module body: the token after its RHS is the
// dedented next declaration, and the RHS parses as a TYPE (tuple, generic, named), never as a
// unit-of-measure expression.
module Tuple =
    type intpair = int * int

module Generic =
    type pairs = List<int * int>

module Named =
    type myint = int

// A measure abbreviation keeps parsing as a measure in the same position.
module Units =
    [<Measure>]
    type kg

    [<Measure>]
    type s

    [<Measure>]
    type N = kg * s

module Use =
    let both (p: Tuple.intpair) : Named.myint =
        let (a, b) = p
        a + b
