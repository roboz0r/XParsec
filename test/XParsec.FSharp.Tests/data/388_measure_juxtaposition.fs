module Test

// Boundary fixture for measure type abbreviations vs postfix type application.
// Per project policy (relax_parser_defer_to_typecheck): the parser does NOT
// inspect the [<Measure>] attribute. Type.parse runs first; it only falls back
// to Measure.parse when it cannot continue (dangling '/', '*', '^', or '^-').
// Consequence:
//   - Pure juxtaposition (`kg m`, `kg m s`) parses as Type.SuffixedType — the
//     type checker is responsible for reclassifying when in measure context.
//   - Any measure operator in the trailing context triggers the existing
//     retry-as-measure path, so the whole RHS comes back as Type.MeasureType.

[<Measure>]
type kg

[<Measure>]
type m

[<Measure>]
type s

// 2-element juxtaposition
[<Measure>]
type kgm = kg m

// 3-element juxtaposition
[<Measure>]
type kgms = kg m s

// Juxtaposition followed by quotient: (kg m) / s
[<Measure>]
type kgm_per_s = kg m / s

// Power followed by juxtaposition: (m^2) s
[<Measure>]
type m2s = m^2 s

// Parenthesised juxtaposition divided
[<Measure>]
type wrap = (kg m) / s
