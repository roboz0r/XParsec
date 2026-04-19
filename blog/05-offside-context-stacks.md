# 5. The offside rule, part 1 — context stacks

## Hook

The F# language specification devotes an entire chapter (§15) to layout. Lightweight syntax — the mode essentially every F# programmer writes in — turns indentation and newlines into syntactic tokens. Implementing it isn't optional. It's also where parser combinator libraries traditionally give up.

The offside work is split across two layers: the *check* happens in the lexical filter (post 3), invisibly, on every token fetch. This post is about the other layer — the *context stack* that the filter consults, which is maintained by the syntactic parsers as they recognize structural keywords.

## What this post covers

- **The F# spec, briefly.** `§15.1` defines an *offside line* for every open context: `let` bindings, `match` clauses, `type` definitions, `if/then/else` branches, record and sequence expressions. A token is "offside" if its column is strictly less than the enclosing context's offside line. The parser's job is to push and pop these contexts correctly so the filter can see them.
- **The context stack.** `ParseState.Context` is a stack of `OffsideContext` values, each with a `Kind` (e.g., `Let`, `Match`, `Type`, `Paren`, `Brace`) and an `Indent` (column). `withContext` pushes and pops cleanly. Most layout-sensitive parsers use it.
- **Paren-like contexts as markers.** `Paren`, `Bracket`, `BracketBar`, `Brace`, `Begin`, `Quote` are pushed with `Indent = 0`. They don't establish an offside line of their own — they're *markers* that let deeper code see "we're inside a paren group." This distinction wasn't in the first draft; commit `ebbb01e Handle offside exceptions and permitted undentations` and the refactor around `a86e642 Add permitted undentation infrastructure` are where it settles.
- **Why `pEnclosed` pushes context manually.** `withContext` does its push/pop around a single parser. For `(... )`, the open-paren must be on the stack *before* the inner parser runs — otherwise the inner parser's own offside checks don't see it. So `pEnclosed` pushes and pops explicitly, bracketing an arbitrary body. Same story for `pRecordOrObjectExpr` and `Brace`.
- **`LexBuilder.Context` vs `Offside.Context`.** A small but recurring gotcha: two types share the name `Context`. Lambdas that touch `.Context` or `.Indent` need type annotations, or F# resolves them to whichever came later. Worth a callout.
- **Building up from `withContext`.** Walk through a small example — parsing a `let` binding and its body, showing the stack contents at each point. Then do the same for a nested `match` inside a paren.

## Anchor commits / files

- `4b63566 Add context-aware hook to parsing helpers`
- `ca3e05f Preliminary offside context parsing`
- `a86e642 Add permitted undentation infrastructure`
- `src/XParsec.FSharp/ParsingTypes.fs`, `src/XParsec.FSharp/ParsingHelpers.fs`

## Takeaway

A stack of (kind, column) pairs is almost the entire offside machinery. The hard part is the next post: what's *allowed* to violate it, and why.
