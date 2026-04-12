module Test

// Reciprocal measure abbreviation with leading /
// From SI.fs

[<Measure>]
type second

[<Measure>]
type hertz = / second

let freq: float<hertz> = 440.0<hertz>
