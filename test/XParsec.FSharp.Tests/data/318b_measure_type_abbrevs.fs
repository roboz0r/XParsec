module Test

// Comprehensive measure type abbreviation test cases
// These require [<Measure>] attribute context to parse correctly,
// which is not available at the parser level.
// TODO: Resolve ambiguous measure type abbreviations.

[<Measure>]
type second

[<Measure>]
type meter

[<Measure>]
type kg

// Quotient: meter / second
[<Measure>]
type speed = meter / second

// Product: kg * meter
[<Measure>]
type momentum_unit = kg * meter

// Power: meter ^ 2
[<Measure>]
type area = meter^2

// Compound: meter / second ^ 2
[<Measure>]
type acceleration = meter / second^2

// Parenthesized: kg * meter / (second * second)
[<Measure>]
type newton = kg * meter / (second * second)

[<Measure>]
type newton2 = kg * (meter / (second * second))

// Juxtaposition (implicit multiplication): kg meter
[<Measure>]
type kg_meter = kg meter
