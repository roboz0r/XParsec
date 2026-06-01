# 9. Pattern parsing — the same engine, twice

## Hook

Patterns share most of their operators with expressions: tuple `,`, sequence `;`, or `|`, and `&`, cons `::`. But the F# spec has *two* productions for patterns — `parenPattern` and `headBindingPattern` — that share every operator except one. Inside parens, `:` is a type annotation that produces `Pat.Typed`; at the head of a `let` binding, `:` belongs to the binding's `optReturnType` and the pattern parser must *not* consume it. Implementing this with a runtime flag would entangle every operator dispatch. Implementing it as two `Operators` instances over a shared LHS and a shared parameterised dispatch is what XParsec's interface from post 5 makes natural — and what `PatternParsing.fs` does.

## What this post covers

- **Two `Operators`, one `tokenToOp`.** `PatOperatorParser` and `PatHeadOperatorParser` share `patLhsParser` and a parameterised `patTokenToOp`. The full delta between them is two booleans: `allowColon` (consume `:` as `Typed`?) and `semiCompletesElems` (does `;` complete elements or just sequence?). Each parser plugs the booleans into the same builder and returns a different `RhsParser`. Show the pair side by side.
- **`PatAux` and the InfixMapped RHS.** Where expressions had `ExprAux` carrying `TypeCast`, `Range`, `DotIndex`, etc., patterns have `PatAux` with two cases: `Type` (for the `:` operator's RHS, parsed by `Type.parse`) and `AsIdent` (for the `as` operator's RHS, just an identifier). Same shape from post 5, different cases — every InfixMapped grows its own Aux.
- **`pSepVirtPat` mirrors `pSepVirt`.** Newline-separated patterns inside list/array literals (`[1; 2; 3]` written across lines) get the same virtual-`;` treatment expressions get. Lives separately because `canStartPattern` is its own predicate; otherwise structurally identical.
- **`pOrAsChain`: re-applying precedence the Pratt parser was bounded out of.** Field-like contexts (record field patterns, union case args) run Pratt at a min-binding-power *above* `|` and `as`, so those operators don't bind across field boundaries. But patterns inside a single field still want or-and-as: `{ Field = (0 | 1) as v }`. The fix is a manual outer fold — a `many pBarToken >>. pat` for the or-chain, then an `opt pAs` for the as-clause. The Pratt parser stays clean; the bookkeeping moves outside.
- **`pNamedFieldPats` commits on evidence.** `Foo(x = 1, y)` could be a positional `Pat.Named` or a named-field `Pat.NamedFieldPats`. The parser tries the named-field path first, parses all the args, *then* checks `hasNamed` — if no argument is actually a named field, it backtracks and lets the positional `pNamed` take over. "Try the more specific shape; commit only when something specific actually appears" is a recurring corpus-driven pattern; this is the cleanest example.
- **`pParenOpHeadPat` and parameterised active patterns.** `(::)` as a binding name is the simple case. `NLambdas ((-) n 1) (vs, b) -> …` is the surprise: the `(-) n 1` parses *as a pattern* and the typechecker reinterprets it as an expression in active-pattern parameter positions. The lexer's `(*)` story (post 2) reappears at the parser layer, because `IdentOrOp.ParenOp` patterns can appear as both binding heads and parameterised active-pattern arguments.
- **`Operator.parserAt` for field contexts.** `parseFieldPat` runs Pratt above `Semicolon`; `parseUnionFieldPat` runs above `Comma`. The same pattern grammar, three different cutoffs, no separate parsers. The 0.3 redesign exposed an entry point parameterised by min binding power, and the field-context cutoffs are where it earns its rent.
- **Dual atomic dispatchers: `parseAtomic` vs `parseAtomicBindingArg`.** Identifier-position parsing differs between rule patterns (consume curried args, e.g. `Some x` binds `x`) and binding-arg patterns (each parameter is independent atomic, e.g. `let f x y = …` has two separate atomic patterns). One parser couldn't serve both without a context flag; two parsers serve both cleanly. The duplication is the design.

## Anchor commits / files

- `src/XParsec.FSharp/PatternParsing.fs` (~780 lines: `PatOperatorParser`, `PatHeadOperatorParser`, `pOrAsChain`, `pNamedFieldPats`, two atomic dispatchers)
- `pattern_parser_structure` (memory: `PatOperatorParser` (parenPattern) vs `PatHeadOperatorParser` (headBindingPattern) split — established before this post)
- `feedback_match_fsharp_grammar` (memory: mirror `pars.fsy` productions; this is the canonical case)
- `e324543 Parse or and as patterns in records`, `7ac1311 Fix nested named fields in record patterns`
- `419ef18 Parse multiple named argument patterns` (the corpus-driven commit-on-evidence path)

## Takeaway

One grammar engine, two interface implementations, one shared LHS — the F# pattern grammar fits on top of XParsec's operator parser cleanly because XParsec already had `Operators` as an interface (post 5). Patterns look like expressions until the `:` arm; the seam is one boolean wide. Everything else — manual or-as chaining, commit-on-evidence backtracking, dual atomic dispatchers — is what happens when a grammar uses the same surface syntax to mean different things in different contexts and the parser has to keep them apart without a context flag in every operator entry.
