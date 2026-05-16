module Test

// Generic types instantiated with measure-expression arguments.
// Exercises the speculative Type → Measure retry in pTypeArg.

[<Measure>]
type kg

[<Measure>]
type m

[<Measure>]
type s

type Vector<[<Measure>] 'U> = { X: float<'U>; Y: float<'U> }

// Simple identifier measure arg — stays TypeArg.Type (parser-level ambiguity, type checker resolves).
let v1: Vector<kg> = Unchecked.defaultof<_>

// Quotient — triggers retry via dangling '/'.
let v2: Vector<m / s> = Unchecked.defaultof<_>

// Power — triggers retry via '^'.
let v3: Vector<m^2> = Unchecked.defaultof<_>

// Fused negative exponent — triggers retry via peek startsWith '^'.
let v4: Vector<s^-1> = Unchecked.defaultof<_>

// Product then quotient — triggers retry via dangling '/' after partial tuple parse.
let v5: Vector<kg * m / s> = Unchecked.defaultof<_>

// Typar quotient inside generic args.
let f<[<Measure>] 'a, [<Measure>] 'b> (x: Vector<'a / 'b>) = x

// Mixed: measure followed by plain ident.
type Pair<[<Measure>] 'U, 'T> = { Value: float<'U>; Tag: 'T }
let p1: Pair<m / s, int> = Unchecked.defaultof<_>

// Plain ident followed by measure arg.
type FlipPair<'T, [<Measure>] 'U> = { Tag: 'T; Value: float<'U> }
let p2: FlipPair<int, m / s> = Unchecked.defaultof<_>
