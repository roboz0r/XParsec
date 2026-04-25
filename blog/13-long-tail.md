# 13. The long tail of bugs

## Hook

Once the harness from post 11 was running, the corpus triage queue started draining at a rate that no single-developer evening pace could have matched. This post is the evidence: three worked examples, one set of live commits captured during the drafting of this very series, and the moment the corpus closed to `249 clean / 0 DIAG`. It's also the post that makes the case that the long tail of a real-world parser — the obscure spec corner, the undocumented intrinsic, the syntax used only by `FSharp.Core` — is not actually intractable. It just needs the right shape of work.

## What this post covers

- **A lexer worked example.** Commit `91f1db2 Fix lexing some interpolated strings` (2026-03-21) is the cleanest small-scale demonstration in the repo. Two long-standing lexer bugs in `Lexing.fs` resolved in a single change:
  1. `pFormatSpecifierTokens` consumed the `%`s with `many1Chars (pchar '%')` and then ran `skipN percents.Length` after emitting `EscapePercent` tokens — advancing past the closing quote. The fix is `lookAhead (many1Chars ...)` to count without consuming, then explicit `skipN`. A textbook lookahead-vs-consume distinction that's easy to miss.
  2. A lone `"` inside `$"""..."""` was routed to `pInterpolated3EndToken`, which required `"""` and failed. The fix introduces `pInterpolated3QuoteOrFragment`, which tries `"""` first and falls back to treating the `"`s as fragment content.

  Both bugs were diagnosed from `.fs.parsed` failures on the existing fixtures `lo_210_interp_escape_percent.fs` and `lo_211_interp_triple_quote_embed.fs`. The fixtures predated the bugs by months — they were checked in October 2025 (post 3) when the lexer state machine was first written; the parser only started exercising the failing arms once enough downstream code was in place to notice. This is the loop's bread and butter: a fixture that tests a documented spec corner, a `.fs.parsed` diff that pins the failure, a fix-and-snapshot session, done.

- **The sharpest example, captured live.** Commit `7786a60 Reduce stack consumption in AST traversal` (2026-04-18, +787/-523) landed while this very series was being outlined. A stack overflow in `AstTraversal.fs` — the same class of bug that took *weeks* to tame the first time (post 12) — was fixed in seven minutes of agent time from this one prompt:

  > I think the other crash was previously diagnosed as a stack overflow in `AstTraversal.fs`, the most likely contributor is `walkExpr` as it is so big. The pattern to fix this is that each "arm" from a pattern match becomes its own function, taking the DU fields as arguments.

  The prompt names the file, names the suspect function, and specifies the remedy in two sentences. The agent did the rest — mechanical refactoring across 1,300 lines of pattern-match arms, one small function per arm. I reviewed the diff for correctness and committed. This is what the harness-plus-memory-plus-pattern-recognition combination looks like at full stride: the human supplies the diagnosis and the shape of the fix; the agent supplies the hours of careful mechanical work. Post 12 has the original stack-taming saga; this is the second occurrence of the same pattern, which is where the payoff curve really shows.

- **Writing this post is itself the pattern.** The most on-the-nose evidence that the harness works is the git log from the session in which this series was being drafted. About ten commits landed on `fsharp` in parallel with my review of these outlines, each from a separate Claude Code session that I'd nudge, check, and commit:
  - `419ef18 Parse multiple named argument patterns`
  - `0b5df16 Parse in keywords between declarations at module level`
  - `7786a60 Reduce stack consumption in AST traversal` (the one above)
  - `ee120cc Parse GADT-style DUs` (the `Option<'T>` canonical form from post 10)
  - `c63f3c0 Parse GADT style operators as case names for F# list` (the `List<'T>` canonical form)
  - `324c946 Allow SRTP struct constraint without colon`
  - `9310e2c Fix lexing colon in operators`
  - plus tooling improvements (`.fs.parsed` printing, LongIdent rendering, missing `and` output)

  The pattern in each case: I pause the blog work, read the diff, confirm the `.fs.parsed` change is what I want, commit. Resume editing. The parser is measurably better than it was four posts ago, and I haven't touched parser code once during this session. That's the whole pitch in one paragraph.

- **The corpus closed to 249/0 during this series.** The most emphatic data point: while these outlines were being drafted, a parallel Claude Code session walked the last three open fixtures (SRTP struct constraint, a lexer `+` handling bug guarded by `inComment`, and the `.. ..` operator-name fusion covered in the bonus post) and closed the triage queue entirely. The corpus report went from `248 clean / 1 DIAG` to `249 clean / 0 DIAG` in a single session, with a human-readable summary table of each intermediate step. The last thing standing between the parser and the entire source of the F# compiler was a grammar surprise in `prim-types.fs`. This is the harness at full stride.

## Anchor commits / files

- `91f1db2 Fix lexing some interpolated strings`
- `7786a60 Reduce stack consumption in AST traversal`
- `ee120cc Parse GADT-style DUs`, `c63f3c0 Parse GADT style operators as case names for F# list`, `324c946 Allow SRTP struct constraint without colon`, `9310e2c Fix lexing colon in operators`
- `ms-fsharp-progress.md` — the running triage notes that drove most of these
- `test/XParsec.FSharp.Tests/data/manual/` — the per-fixture reproductions

## Takeaway

The "long tail" isn't a budget problem. Every individual bug in it is small; the difficulty is sustaining the *thousands* of small focused investigations needed to close them all. With the right harness — a corpus that manufactures reproductions, a constrained agent loop that closes them, and a human-in-the-loop reviewing only the golden-file diffs — the long tail goes from "three months of evenings" to "the background hum while you work on the next architectural piece." The 248→249 step happened in a single afternoon. Most of the others did too.
