# XParsec.FSharp — lexer and CST parser

## Adding Grammar

Mirror `pars.fsy` from the F# reference compiler. Where F# models a construct as a distinct
production, mirror the production boundary with its own parser and entry point rather than
reusing one Pratt parser at a different minimum binding power. `:` in patterns is the worked
case: it belongs to the `parenPattern`/`simplePat` productions and is absent from
`headBindingPattern`, so no single precedence can be right for both `A3(x: int, y: int)` and a
let LHS. Name a restricted parser after the grammar rule it implements. A shim that bypasses
Pratt entirely is the red flag; the fix is a properly scoped Pratt entry point.

Prefer the general rule that accepts a superset of valid F# over one enforcing a semantic
restriction at parse time. Mixed named and positional constructor arguments parse in any order,
and the AST shape discriminates afterwards. Add a parse-time restriction only for a genuine
grammar ambiguity, or where the restriction decides which AST shape to emit.

## AST

Give an optional source token a slot on the node (`inTok: SyntaxToken voption`) rather than
consuming and discarding it. Range accuracy is a first-class criterion here, and F#'s own AST
drops tokens such as `mIn` once it has used them for ranges.

## Recovery

Recovery to a `Missing` node must consume at least one token when it sits inside `many` or
another looping combinator, because `many` compares the reader position after each `Ok` and
throws `InfiniteLoopException` on a zero-width success. At a stopping-token boundary with
nothing to skip, return `Error` so the loop terminates, or apply the recovery outside the loop.
