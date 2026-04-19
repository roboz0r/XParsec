# 6. The offside rule, part 2 — allowed undentations

## Hook

If F# strictly enforced "no token below the offside line," a lot of idiomatic F# wouldn't parse. `elif` after a deeply indented `then` branch. A multi-line `fun x -> ...` whose body starts under the `fun`. A trailing `)` on its own line. The spec acknowledges this: §15.1.8–15.1.10 enumerate specific *permitted undentations*. Implementing them is where half the project's subtle bugs lived.

## What this post covers

- **The spec rules, plain-English.** §15.1.8 (undentation of braces), §15.1.9 (parentheses and begin-end), §15.1.10.1–4 (undentation of expressions and the exceptions for `fun`/`function` bodies). Make each concrete with a small example.
- **The function that does all the work.** `isPermittedUndentation` in `ParsingHelpers.fs` is the heart of the system, called by the lexical filter (post 3) every time a token's column is left of the innermost context's offside line: given the token and the context stack (post 5), can this undentation be permitted? Walk through the signature, the rule dispatching, and *why the other rules keep turning out to be dead code.*
- **The dead-code discovery.** Commits `0ee9fa9 Eliminate undentation rule 15.1.10.3 as dead code` and `d261f35 Eliminate undentation rule 15.1.10.2 as dead code` are instructive. The rules were implemented faithfully; then the corpus showed they never actually fired — every real case was already covered by 15.1.10.4 or by structural context checks. Great story about trusting the tests.
- **Walking the stack for SeqBlock+Paren pairs.** The trickiest case. Inside a paren, an expression's effective offside line is *not* the paren's `Indent=0`; it's the enclosing `let` or `match` or top-level declaration, which may be several `SeqBlock+Paren` frames up. `checkCollectionUndent` (and the fun/function body path) walk past arbitrarily many `SeqBlock`-then-`Paren` pairs to find it. Show the loop. Call out how this interacts with computation expressions.
- **`fun` and `function` as undentation anchors.** Commit `a67b3cf Allow undentation rule 15.1.10.4 for fun and function keywords` fixed a long-tail of multi-line lambdas. Reveals that the anchor of an undentation isn't always a keyword the reader would name (`let`, `match`) — sometimes it's the expression-introducing keyword itself.
- **elif anchoring.** `046d26e Fix indentation context of elif branches` and `2adcbb9 Fix anchoring offside column for if-elif-else if chains`. `elif` is a joint operator: it continues the enclosing `if`'s column, not its own branch's. Parsing it wrong produces very confusing cascading failures.
- **Corpus-driven discoveries.** Many of the commits in this area (`616b892`, `cc5933a`, `6ee954a`, `930c99c`, `d0af4ca`, `c5bb9cd`) came from running the F# compiler's own source code through the parser and watching which files failed. Post 10 tells that story in full.

## Anchor commits / files

- `src/XParsec.FSharp/ParsingHelpers.fs` (`isPermittedUndentation`, `checkCollectionUndent`)
- `a86e642`, `ebbb01e`, `1a19aff`, `a67b3cf`, `0ee9fa9`, `d261f35`

## Takeaway

The offside rule is cheap in principle and expensive in every specific case. A small number of permitted-undentation patterns subsume a surprisingly large fraction of real F#.
