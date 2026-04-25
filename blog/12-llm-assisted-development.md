# 12. LLM-assisted parser development: a harness that pays off

## Hook

Over the course of two weeks in early 2026, dozens of parser bugs went from "captured in the corpus" to "fixed and tested" with minimal direct review. I wrote almost none of those fixes myself. Claude Code sessions did — alternating between "run the corpus and make reproductions" and "here is a reproduction, please fix it." What made the pattern work wasn't the model. It was the harness around it.

This is a post about the *scaffolding* that turns a good code assistant into something that can close out a long tail of parser bugs overnight.

## What this post covers

- **The premise.** Parser bugs have a nice property: a reproduction is a short input file, a fix is usually local, and correctness is a golden file. That's about the best possible shape of work to hand off. The corpus test (post 10) already manufactures reproductions in bulk. The question was whether a coding agent could pick them up from the other end.
- **The harness ingredients.**
  1. **`DEVELOPER.md`** — a terse, opinionated architectural overview of the project. The compilation DAG, the `ParserRefs.fs` pattern, the ParseState fields, the offside rule, the testing model. Written for a human joining the project; equally useful as a system-prompt floor for an agent.
  2. **`claude_tools.ps1` and `claude_tools.cmd`** — a *constrained* wrapper around `dotnet build`, `dotnet test`, and `dotnet fantomas`. `[ValidateSet]` restricts the projects and test suites the agent can touch. Output is truncated to the last thirty lines in the terminal but saved in full to `claude_tools_output.log`. So the agent gets a summary in context and the long form on demand.
  3. **An `xparsec-dev` skill** (`.claude/skills/xparsec-dev/SKILL.md`) that tells the agent *when* to use the harness ("do not use raw `dotnet`") and *how* to handle truncated output ("don't re-run; read the log file").
  4. **`.claude/settings.local.json`** with a narrow allow-list: the wrapper, `git log`, `ls`, `grep`, and the corpus test binary. Everything else prompts. This is the difference between a agent that can iterate freely and one that interrupts every few minutes.
  5. **Golden-file tests with `-UpdateSnapshots`** — the agent has a single-command way to regenerate `.fs.parsed` files after a fix. The diff is what the human reviews.
  6. **`ms-fsharp-progress.md`** — a living document the agent reads and writes. It captures the triage workflow, bugs fixed, and notes for future sessions. Closer to lab notes than documentation.
  7. **Memory system** (`.claude/projects/.../memory/`). Short, specific lessons learned: "integration tests don't use mocks," "use `Write` tool not bash heredocs," "`while` inside `parser { }` has imperative semantics as of 2026-04-16." The agent checks these before doing anything non-obvious.

- **The loop.** Two alternating kinds of session:
  - **"Find and reproduce" session.** Run the corpus. Read `REPORT.txt`. Pick a DIAG(1-5) file. Copy it to `data/manual/`, run the step-debug test, write a minimal `.fs` repro that fails the same way. Commit nothing — just leave the repro and diagnostic in place.
  - **"Fix this" session.** Given a repro file, investigate (often with `serena` MCP for symbolic navigation), propose a fix, build, run tests with `-UpdateSnapshots`, verify the repro now parses and existing tests still pass. Surface the diff for review.

- **Why this works.** Parser fixes have tight feedback loops: a build takes seconds, a test pass a few more, and the `.fs.parsed` diff *is* the specification. The agent is effectively doing end-to-end TDD with me as the designer of the tests. The harness makes this cheap per iteration and safe per side effect.

- **What the agent is bad at.** Deciding which bug is worth fixing. Choosing whether to merge two AST variants. Noticing when a repeated symptom is actually one root cause in another file. The triage and architectural calls still sit with me. But "reproduce, fix, verify" is not what I want to be spending my evenings on anymore.

- **A lexer worked example.** Commit `91f1db2 Fix lexing some interpolated strings` (2026-03-21) is the cleanest small-scale demonstration in the repo. Two long-standing lexer bugs in `Lexing.fs` resolved in a single change:
  1. `pFormatSpecifierTokens` consumed the `%`s with `many1Chars (pchar '%')` and then ran `skipN percents.Length` after emitting `EscapePercent` tokens — advancing past the closing quote. The fix is `lookAhead (many1Chars ...)` to count without consuming, then explicit `skipN`. A textbook lookahead-vs-consume distinction that's easy to miss.
  2. A lone `"` inside `$"""..."""` was routed to `pInterpolated3EndToken`, which required `"""` and failed. The fix introduces `pInterpolated3QuoteOrFragment`, which tries `"""` first and falls back to treating the `"`s as fragment content.

  Both bugs were diagnosed from `.fs.parsed` failures on the existing fixtures `lo_210_interp_escape_percent.fs` and `lo_211_interp_triple_quote_embed.fs`. The fixtures predated the bugs by months — they were checked in October 2025 (post 3) when the lexer state machine was first written; the parser only started exercising the failing arms once enough downstream code was in place to notice. This is the loop's bread and butter: a fixture that tests a documented spec corner, a `.fs.parsed` diff that pins the failure, a fix-and-snapshot session, done.

- **What the agent is surprisingly good at.** Designing instrumentation. The stack probe in post 11 — dedicated-thread parse with explicit stack size, SP captured via `&&marker` at every context push/pop, deepest `StackTrace` snapshotted on depth increase, `.fs.stack` output with SP deltas per frame — came out of one prompt with Claude. Both the design and the output format. The probe is now a load-bearing piece of the test infrastructure, and reading its output (matching up the SP-delta column with the deepest trace) is itself something I hand back to Claude. Instrumentation is a good fit for the model because the inputs are narrow (what do I want to see?), the outputs have clear right answers (did the format surface the bug?), and nothing depends on taste.

- **The memory system as guardrails.** Specific examples: the memory `feedback_recovery_many` documents that "zero-width recovery inside `many`/looping combinators causes infinite loops" — because the agent had produced that bug and I corrected it. Future sessions see the memory, don't reintroduce the bug. The memory `feedback_match_fsharp_grammar` says "mirror `pars.fsy` productions, don't fake context-sensitivity with precedence numbers" — a philosophical correction that kept surfacing until it got written down. A surprising amount of LLM-assisted work is *compiling* your own preferences into a form the model can re-read.

- **Reviewing at scale.** The review discipline shifted. For a human collaborator I review code. For an agent I review the *diff of the golden file* first — because that's the behavioral specification — and then the parser diff if the behavioral change looks right. In half the cases the golden diff is small and obviously correct and I barely read the parser change at all. In the other half the parser change requires real attention, but at least I'm reading it knowing exactly what it's supposed to do.

- **The commits.** Go through the `fsharp` branch and pick ten consecutive commits from late March / early April 2026. Show message, diff size, approximate agent time. The shape of work is mostly DIAG-driven: one repro, one fix, one golden file update, done.

- **Writing this post is itself the pattern.** The most on-the-nose evidence that the harness works is the git log from the session in which this series was being drafted. About ten commits landed on `fsharp` in parallel with my review of these outlines, each from a separate Claude Code session that I'd nudge, check, and commit:
  - `419ef18 Parse multiple named argument patterns`
  - `0b5df16 Parse in keywords between declarations at module level`
  - `7786a60 Reduce stack consumption in AST traversal` (the one covered in detail below)
  - `ee120cc Parse GADT-style DUs` (the `Option<'T>` canonical form from post 10)
  - `c63f3c0 Parse GADT style operators as case names for F# list` (the `List<'T>` canonical form)
  - `324c946 Allow SRTP struct constraint without colon`
  - `9310e2c Fix lexing colon in operators`
  - plus tooling improvements (`.fs.parsed` printing, LongIdent rendering, missing `and` output)

  The pattern in each case: I pause the blog work, read the diff, confirm the `.fs.parsed` change is what I want, commit. Resume editing. The parser is measurably better than it was four posts ago, and I haven't touched parser code once during this session. That's the whole pitch in one paragraph.

- **The corpus closed to 249/0 during this series.** The most emphatic data point: while the outline for this post was being drafted, a parallel Claude Code session walked the last three open fixtures (SRTP struct constraint, a lexer `+` handling bug guarded by `inComment`, and the `.. ..` operator-name fusion covered in the bonus post) and closed the triage queue entirely. The corpus report went from `248 clean / 1 DIAG` to `249 clean / 0 DIAG` in a single session, with a human-readable summary table of each intermediate step. The last thing standing between the parser and the entire source of the F# compiler was a grammar surprise in `prim-types.fs`. This is the harness at full stride.

- **The sharpest example, captured live.** Commit `7786a60 Reduce stack consumption in AST traversal` (2026-04-18, +787/-523) landed while this very series was being outlined. A stack overflow in `AstTraversal.fs` — the same class of bug that took *weeks* to tame the first time (post 11) — was fixed in seven minutes of agent time from this one prompt:

  > I think the other crash was previously diagnosed as a stack overflow in `AstTraversal.fs`, the most likely contributor is `walkExpr` as it is so big. The pattern to fix this is that each "arm" from a pattern match becomes its own function, taking the DU fields as arguments.

  The prompt names the file, names the suspect function, and specifies the remedy in one sentence. The agent did the rest — mechanical refactoring across 1,300 lines of pattern-match arms, one small function per arm. I reviewed the diff for correctness and committed. This is what the harness-plus-memory-plus-pattern-recognition combination looks like at full stride: the human supplies the diagnosis and the shape of the fix; the agent supplies the hours of careful mechanical work.

- **What I wouldn't hand off.** Anything that touches the AST shape (post 8), the offside rule (posts 6–7), the ParserCE, or the recovery policy (post 9). Those are architectural; getting them wrong costs weeks, and the tight-feedback loop doesn't exist. Everything downstream of them — concrete parser productions, golden-file maintenance, diagnostic wording — is fair game.

- **The honest caveat.** This works for a parser. Parsers have exceptionally strong test ergonomics: inputs are text, outputs are trees, errors are positions. Not every project is shaped like this. A project that can't produce a golden-file loop this tight would need a different harness, or a different role for the agent.

## Anchor files

- `DEVELOPER.md`
- `claude_tools.ps1`, `claude_tools.cmd`
- `.claude/skills/xparsec-dev/SKILL.md`
- `.claude/settings.local.json`
- `ms-fsharp-progress.md`
- `CLAUDE.md` memory files under `.claude/projects/.../memory/`

## Takeaway

A coding agent does not replace the architect. It replaces the evening spent fixing the eighth corpus diagnostic that has the same shape as the previous seven. Build the harness, write the constraints down, let the loop run. The interesting work is still the architecture; the tail of bugs is what finally gets done.
