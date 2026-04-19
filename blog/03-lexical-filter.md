# 3. The lexical filter: one invisible pass behind the parsers

## Hook

The F# compiler has three distinct passes over tokens: lex, preprocess conditional directives, and "lexical filter" — the phase that resolves the offside rule and inserts virtual tokens for layout. Each pass in the reference compiler materializes its output before the next reads it.

After lexing XParsec.FSharp does preprocessing and filtering in a single function: `nextNonTriviaTokenImpl` in `ParsingHelpers.fs`. Every time a syntactic parser asks for the next token it actually runs through this filter, which skips trivia, evaluates `#if`, processes `#nowarn`, synthesizes virtual delimiters, splits fused operators on demand, and enforces the offside rule. The parsers upstairs never see any of it. They just see a clean stream of meaningful tokens that obey layout.

This post is about that layer — what it does, why fusing it into one pass is worth it, and how the main AST parsers stay simple because of it.

## What this post covers

- **The shape of `nextNonTriviaTokenImpl`.** A single `match` on the next raw token:
  - `IfDirective` → hand off to `processIfDirective`
  - `ElseDirective` in an active then-branch → skip to matching `#endif` via `skipElseBranch`
  - `EndIfDirective` leftover after an active branch → skip silently
  - `NoWarnDirective` / `WarnOnDirective` → `processWarnDirective`
  - trivia (whitespace, comments, line continuations) → recurse
  - any other token → run split-flag rewrites, then run offside check, then either `fail "Offside"` or return the token (peeked or consumed, depending on `isPeek`)

  One function, seven concerns. Each bullet below is one of them.

- **Trivia skipping.** The simplest job but done here rather than in the lexer so trivia token kinds survive into the token array for formatters and tooling. The filter just doesn't hand them to parsers.

- **`#if` evaluation.** `processIfDirective` slices the directive line into its own `Reader`, runs `IfExpr.parseSlice` on it, evaluates the resulting boolean expression against `state.DefinedSymbols`, and branches: active → recurse into the then-branch; inactive → call `skipInactiveBranch` and recurse into whatever follows (`#else`-branch or the tokens after `#endif`). Nesting is tracked by a depth counter; EOF during a skip is a graceful stop (unclosed `#if` becomes a diagnostic, not a crash). Commits: `a2bf409 Lexing IfDirective`, `a4237ab WIP handling #if directives`, `8faaf58 Fix #if directive parsing`, `2f374be Add #if true test`, `153294a Add IfExprState`, `83baaf0 Merge skipInactiveBranch and skipElseBranch`.

- **`#nowarn` / `#warnon`.** `processWarnDirective` walks the rest of the directive's line, extracts integer warning codes (including codes written as string literals, now fragmented — see post 2), and appends them to `state.WarnDirectives`. Diagnostics emitted later consult this list to decide whether to show a warning.

- **Virtual tokens.** The bit-flag was defined in the lexer (post 2); here is where they're produced. `nextNonTriviaTokenVirtualIfNot t` returns a real `t` if present, otherwise a virtual `t` without consuming input. `nextNonTriviaTokenVirtualWithDiagnostic` does the same but also emits an `UnclosedDelimiter` diagnostic. `makeVirtualSep` synthesizes a virtual `;` at the current position. Commit `67787e4 Merge nextNonTriviaTokenVirtualIfNot and nextNonTriviaTokenVirtualWithDiagnostic` consolidates the duplicated core into `nextNonTriviaTokenVirtualCore`.

- **Fused-operator splitting.** Two state flags: `SplitRAttrBracket` and `SplitPowerMinus`. When the measure parser sees a fused `>]` or `^-` that it wants to split, it does *not* consume the token — it sets the flag. The next call to `nextNonTriviaTokenImpl` checks the flag: if set, it rewrites the next token to the right half (`KWRBracket` at `StartIndex + 1`, or `OpSubtraction` at `StartIndex + 1`), clears the flag, and hands it up. This is why the measure parser is one-ended: it takes the left half of the fused operator as a virtual token and leaves the right half in the stream for the enclosing parser to consume normally.

- **Offside check.** If the next token is strictly left of the innermost context's offside line and the mode is `Syntax.Light`, call `isPermittedUndentation`. If the result is `ValueSome rule`, emit a `PermittedUndentation` trace event and let the token through; if `ValueNone`, fail with `Message "Offside"`. The *context stack* is maintained by the syntactic parsers upstairs (pushed by `withContext`, `withContextAt`, and the manual push in `pEnclosed` / `pRecordOrObjectExpr`); the *check* happens here, every token. Posts 5 and 6 cover the stack and the permitted-undentation rules in detail.

- **Peek vs consume.** A single `isPeek` flag threads through the whole function. Peek takes the filtering pass without advancing the reader; consume does the same and advances. This duplication exists because split-flag clearing and trace-event emission differ between the two. Worth discussing how keeping peek and consume *both* going through the same filter is what makes `peekNextNonTriviaToken` / `consumePeeked` the workhorse pattern for every downstream parser.

- **Dispatch on the filtered token.** Because parsers work on the filtered stream, `dispatchNextNonTriviaTokenL` is a thin wrapper: peek, table-lookup the token kind, run the matching parser. With three passes materialized separately, every downstream parser would need to remember which pass had already run on which bytes. With one lazy filter, they don't.

- **Why lazy, not materialized.** Two reasons. (1) Error recovery: `recoverWith` backtracks the reader position and replays tokens through the filter — a materialized filter would have to either re-run the preceding passes or carry an index into a separate buffer. (2) Context-sensitive rewrites: the split flags are set by parsers based on *syntactic* context (we're inside a measure), not lexical context. A pre-materialized filter would have no way to know.

- **The public surface.** Parsers don't call `nextNonTriviaTokenImpl` directly. The exposed helpers are:
  - `peekNextNonTriviaToken` / `consumePeeked` — the primary interface
  - `assertKeywordToken` / `assertKeywordTokens` — keyword-expecting shorthand
  - `nextNonTriviaTokenVirtualIfNot` / `nextNonTriviaTokenVirtualWithDiagnostic` — layout synthesis
  - `nextNonTriviaTokenIsL` / `nextNonTriviaTokenSatisfiesL` — predicate matches with a custom error
  - `dispatchNextNonTriviaTokenL` / `dispatchNextNonTriviaTokenFallback` — O(1) branching
  - `withContext` / `withContextAt` — offside-context management
  - `recoverWith` / `recoverWithVirtualToken` / `recoverLongIdent` — recovery variants
  Every syntactic parser in the project is built out of these.

## Anchor commits / files

- `src/XParsec.FSharp/ParsingHelpers.fs` (the whole file is this layer)
- `src/XParsec.FSharp/Preprocessing.fs` (`IfExpr.parseSlice`, `IfExpr.evaluateStateful`)
- `94d6547 Preprocess lexical blocks in the lexer` (the name is historical — the logic moved to the filter)
- `9895243 Add directives`, `a2bf409 Lexing IfDirective`
- `a9899c9 Add virtual tokens`, `67787e4`, `83baaf0`

## Takeaway

Lex, preprocess, and lexical-filter don't need to be three passes. Folded into one lazy filter they become a fat helper function that the rest of the parser can treat as its token source — and the rest of the parser can then look like a textbook parser combinator. The architectural win is that half the project's hardest features (preprocessor handling, virtual tokens, offside) disappear from the AST-parser files entirely.
