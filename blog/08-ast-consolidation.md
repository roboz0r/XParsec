# 8. AST design through consolidation

## Hook

The first draft of the AST had an `Expr` union with thirty-plus cases. Nearly half have since been merged, generalized, or deleted. That's not regression — it's what healthy AST design looks like. This post is about the discipline of removing variants you thought you needed, and the philosophy that drove the removals.

## The spec was the wrong starting point

The first AST was built by reading the [F# language specification](https://fsharp.github.io/fslang-spec/) top to bottom and transcribing its grammar productions into discriminated union cases. That seemed like the disciplined thing to do. It was two different kinds of wrong.

- **The spec was incomplete.** The published grammar is years behind the language. Anonymous records, SRTP member constraints, static optimization syntax, `fixed`, light-mode record expressions, mixed named/positional arguments in union patterns — the spec doesn't have them, or has earlier, stricter versions of them. Transcribing the spec gave me a parser that couldn't handle the F# actually being written.
- **The spec was overly rigid.** Where the spec *does* have productions, they're written to communicate the *language* to a reader — with distinctions like "function binding" vs "value binding," "simple pattern" vs "pattern," "computation expression body" vs "expression." Those distinctions are real at the typechecker's level. They're noise at the parser's level. Carrying them in the AST meant every downstream walker had to reason about them even when it shouldn't care.

Once this was visible, the operating rule became **parse syntax, not semantics.** If two nodes look structurally similar, they're candidates for fusion. If fusing them lets the parser accept input that's syntactically well-formed but semantically invalid, that's *fine* — emit a diagnostic from a later phase. The only thing that blocks a merge is genuine grammatical ambiguity: two productions that would become indistinguishable and break parsing of something required.

This is the same philosophy captured by the memory note `feedback_relax_parser_defer_to_typecheck`: prefer general grammar rules, defer semantic rejection to later phases.

## What this post covers

- **The early AST.** Show a snapshot from around commit `dab4d90 Patterns types and measures parsing` or `223b6e1 FSharp Type refinements`. Lots of narrow variants: `Expr.MatchBang`, `Expr.ForInShort`, separate `CompExpr` vs `Expr`, separate `Functions` vs `Values` vs `Binding`. Each of these was a literal transcription of a spec production; every one of them merged later.
- **Merging by semantics, not syntax.** `163059f Remove dead Expr.MatchBang and Expr.ForInShort branches` — these were distinct cases only because the parser split on syntactic shape, not meaning. Once the AST settled, they collapsed into `MatchExpr` and `ForExpr` with a bang/non-bang flag.
- **`CompExpr` merges into `Expr`.** Commit `bedd319 Fully merge CE parsing into expression parsing`. Computation expressions used to have their own parser tree; in reality CE bodies are just expressions in a computation context. Unifying the grammar unified the AST.
- **`Binding` absorbs `Functions`, `Values`, and `CompExpr`.** `67ab380 Combine CompExpr with Expr, Functions and Values into Binding`. F# distinguishes `let f x = ...` (function) from `let x = ...` (value) syntactically but not structurally — both are `Binding pat body`. The type system flags the difference after parsing.
- **`SimplePat` merges into `Pat`.** `4c75c51 Merge SimplePat with Pat`. Lambda parameters used to parse as a restricted `SimplePat`. Merging means one pattern parser serves every context — and errors for "not a simple pattern" move from the parser to a later validation pass, which is a better place for them anyway.
- **`let` and `use` merge.** `b0fe694 Combine let and use parsers`. Same grammar, different keyword. Before: two near-duplicate parsers. After: one parser, a keyword flag, half the code.
- **Paren-like expressions merge.** `c1ec36e Merge paren-like expressions` and `00471b5 Generalize list and array patterns and expressions`. `()`, `(expr)`, `(e, e)`, `begin e end` all share structure; list and array expressions share almost all of their structure with their pattern counterparts.
- **Keep parsed separators.** `d3af88f Include parsed separators in AST`. Often consolidation means *keeping more information*, not less. The list expression used to throw away the commas; tooling (formatters, refactorings) needs them.
- **Immutable arrays.** `8361557 Switch to immutable array in AST nodes`. A performance and correctness win. Stop leaking F# lists (which are singly-linked) into downstream tooling that needs O(1) indexing.

## Pattern: the shape of consolidation

Every merge commit above follows the same pattern:
1. Start with a spec-literal variant, because the spec is the available source of truth.
2. Discover, during broader feature work, that it overlaps structurally with another variant — usually because the spec's distinction was semantic, not syntactic.
3. Check for genuine ambiguity with a required grammar. If none, merge. Delete the now-dead parser branch. Any invalid input the merged parser now accepts becomes a diagnostic in a later phase, not a parse error.
4. Keep the distinction (if any) as a flag or a subtype on the shared node, so downstream tools that *do* care can still see it.

## Anchor commits / files

- `src/XParsec.FSharp/Expr.fs` (current state)
- `285e817`, `67ab380`, `73b25cc`, `4c75c51`, `b0fe694`, `bedd319`, `c1ec36e`, `00471b5`, `b6b7e93`, `32b72d7`, `163059f`
- `d3af88f`, `8361557`

## Takeaway

The spec is a description of the *language*, not a blueprint for the *parser*. Transcribing its productions one-for-one gives you an AST with ten times more shape than the parser needs, every piece of which is a future consolidation commit. Parse syntax, not semantics. Merge freely whenever two nodes tell the same structural story. Let the typechecker — or at minimum, a later validation pass — reject what's syntactically legal but semantically wrong. The only hard stop is genuine grammatical ambiguity; everything else is taste, and taste favors a small AST.
