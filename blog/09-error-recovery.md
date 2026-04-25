# 9. Error recovery you can actually ship

## Hook

A parser that fails on the first error is a compiler's internal tool. A parser that keeps going, reports every error it finds, and produces a partial AST the rest of the toolchain can still work with is an IDE's best friend. This post is about getting to the second one without building a whole new parser framework.

## What this post covers

- **What "recovery" means in XParsec.** Unlike some parser combinator libraries, XParsec has no special "error production" mechanism — recovery is a combinator. `recoverWith` backtracks on failure, emits a diagnostic, skips tokens until a synchronization point (`;;`, `end`, `)`, `]`, a lower-indented offside line), and returns a placeholder AST node.
- **Skeleton recovery first.** The earliest approach, `6eb77b3 Add skeleton parser recovery`, wraps only the top-level declaration parser. A broken module member is replaced by an error node; the rest of the file still parses. This is already enough for most practical IDE cases.
- **Recovery happens in the lexer too.** The same philosophy applies a layer down. An unterminated string at line 5 must not stop the lexer from tokenising line 200 — emit a distinct token kind (`UnterminatedStringLiteral`, `UnterminatedBacktickedIdentifier`, `InvalidDirective`), synthesize a sensible close, keep going. The lexer's never-fail discipline (post 2's "Failing without giving up" section) is the same idea as `recoverWith`: don't propagate a local failure into a global one. `bb959c3 Handle unterminated interpolated strings` is the worked example for the interpolated-string state machine — when the source ends mid-`$"..."`, the lexer pops the interpolation context, emits the unterminated marker, and the parser sees a well-formed token stream with one diagnostic attached.
- **Structured recovery nodes.** `d3e849d Add more structured parser recovery` and `15dd3d8 Remove unused inner node from X.SkipsTokens`. The parser emits a dedicated `Expr.SkipsTokens` (or equivalent) that retains the skipped tokens. Tooling can render them as-is; the parser doesn't lose information.
- **Sync points.** Show the table of synchronization tokens and the rationale for each. `;;` and `end` are obvious; `)` and `]` close paren-like contexts; a token whose column drops below the enclosing `let`/`module` offside line is a soft sync. The soft sync is what makes recovery feel non-local.
- **Layout sensitivity is a *gift* to error recovery.** This is the counterintuitive part. Layout-sensitive languages are often framed as harder to parse — they are, at the happy path. But the offside rule gives every open context a free, always-available sync signal: *the next token whose column drops below the context's indent.* That signal exists whether or not the user remembered to type `end`, `)`, or `;;`. A brace-delimited language has no equivalent. When a `{` is missing or a `}` is misplaced in C, Java, TypeScript, or Rust, the parser's block-tracking goes wrong from that point on, and the error cascade often runs to end of file. Even production-quality parsers for brace languages choke on a single mismatched brace — the IDE highlights every subsequent line red until the user finds and fixes it. In F#, a stray `let` at a wrong column simply closes its enclosing context at the first lower-indented token and the parser resumes at the next declaration. Dozens of commits' worth of recovery bugs that a brace language would have had never existed in the first place because the offside rule handled them for free.
- **Diagnostic positions.** `4324da9 Improve error formatting`. The diagnostic is only as useful as the position it points to. Reports go through `FSharpErrorFormatting.fs`, which gets line/column from the token span, not from a running counter — essential for files with mixed-mode line endings (golden files are normalized, real-world files aren't).
- **The `.fs.parsed` golden file.** `b376f4d Improve .fs.parsed output format`, `f0ccb59 Create .parsed file in parseWithStackProbe`. A text dump of the AST plus every diagnostic with its position. It's the primary debugging artifact for the whole project and the substrate for corpus testing (post 10).
- **Testing recovery.** `986d0f1 Begin testing parser recovery comprehensively`. Every broken input gets a golden `.fs.parsed` file. The tests aren't "does it fail?" — they're "does it fail *this way*?". Hence `-UpdateSnapshots` in the dev harness.

## Anchor commits / files

- `src/XParsec.FSharp/ParsingHelpers.fs` (`recoverWith`, sync token table)
- `src/XParsec.FSharp/FSharpErrorFormatting.fs`
- `6eb77b3`, `0db7a63 Implement recovery for some nodes`, `62b68e1 Add error recovery`
- `d3e849d`, `15dd3d8`, `4324da9`, `986d0f1`

## Takeaway

Error recovery doesn't need special framework features — it needs a clear synchronization policy, dedicated AST nodes that retain the skipped tokens, and a test harness that pins the *shape* of every failure. And if you're working on a layout-sensitive language: the offside rule you've been paying for in grammar complexity quietly pays you back here, because a dropped indent is a sync signal a brace language doesn't get.
