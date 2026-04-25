# 11. LLM-assisted parser development: the harness

## Hook

Post 10 ended with the corpus showing a third of files producing diagnostics and another third crashing the process. The triage queue had on the order of a hundred entries. At the rate I'd been working — one or two bugs per evening — that was three months of nothing-but-corpus. The work was also unglamorous: most of the bugs were narrow lexer or parser fixes whose investigation followed exactly the same template (read `.fs.parsed`, find the `[error]` line, write a minimal repro, locate the parser rule, fix, regenerate the snapshot).

That's the shape of work where a coding agent earns its keep — *if* the surrounding scaffolding lets it iterate without micromanagement. So before doing any more bug-fixing by hand, I built the harness. Post 13 is the evidence that it worked; this post is the harness itself.

## What this post covers

- **The premise.** Parser bugs have a nice property: a reproduction is a short input file, a fix is usually local, and correctness is a golden file. That's about the best possible shape of work to hand off. The corpus test (post 10) already manufactures reproductions in bulk. The question was whether a coding agent could pick them up from the other end.

- **The harness ingredients.**
  1. **`DEVELOPER.md`** — a terse, opinionated architectural overview of the project. The compilation DAG, the `ParserRefs.fs` pattern, the ParseState fields, the offside rule, the testing model. Written for a human joining the project; equally useful as a system-prompt floor for an agent.
  2. **`claude_tools.ps1` and `claude_tools.cmd`** — a *constrained* wrapper around `dotnet build`, `dotnet test`, and `dotnet fantomas`. `[ValidateSet]` restricts the projects and test suites the agent can touch. Output is truncated to the last thirty lines in the terminal but saved in full to `claude_tools_output.log`. So the agent gets a summary in context and the long form on demand.
  3. **An `xparsec-dev` skill** (`.claude/skills/xparsec-dev/SKILL.md`) that tells the agent *when* to use the harness ("do not use raw `dotnet`") and *how* to handle truncated output ("don't re-run; read the log file").
  4. **`.claude/settings.local.json`** with a narrow allow-list: the wrapper, `git log`, `ls`, `grep`, and the corpus test binary. Everything else prompts. This is the difference between an agent that can iterate freely and one that interrupts every few minutes.
  5. **Golden-file tests with `-UpdateSnapshots`** — the agent has a single-command way to regenerate `.fs.parsed` files after a fix. The diff is what the human reviews.
  6. **`ms-fsharp-progress.md`** — a living document the agent reads and writes. It captures the triage workflow, bugs fixed, and notes for future sessions. Closer to lab notes than documentation.
  7. **Memory system** (`.claude/projects/.../memory/`). Short, specific lessons learned: "integration tests don't use mocks," "use `Write` tool not bash heredocs," "`while` inside `parser { }` has imperative semantics as of 2026-04-16." The agent checks these before doing anything non-obvious.

- **The loop.** Two alternating kinds of session:
  - **"Find and reproduce" session.** Run the corpus. Read `REPORT.txt`. Pick a DIAG(1-5) file. Copy it to `data/manual/`, run the step-debug test, write a minimal `.fs` repro that fails the same way. Commit nothing — just leave the repro and diagnostic in place.
  - **"Fix this" session.** Given a repro file, investigate (often with `serena` MCP for symbolic navigation), propose a fix, build, run tests with `-UpdateSnapshots`, verify the repro now parses and existing tests still pass. Surface the diff for review.

- **Why this works.** Parser fixes have tight feedback loops: a build takes seconds, a test pass a few more, and the `.fs.parsed` diff *is* the specification. The agent is effectively doing end-to-end TDD with me as the designer of the tests. The harness makes this cheap per iteration and safe per side effect.

- **What the agent is bad at.** Deciding which bug is worth fixing. Choosing whether to merge two AST variants. Noticing when a repeated symptom is actually one root cause in another file. The triage and architectural calls still sit with me. But "reproduce, fix, verify" is not what I want to be spending my evenings on anymore.

- **What the agent is surprisingly good at.** Designing instrumentation. The stack probe in post 12 — dedicated-thread parse with explicit stack size, SP captured via `&&marker` at every context push/pop, deepest `StackTrace` snapshotted on depth increase, `.fs.stack` output with SP deltas per frame — came out of one prompt with Claude. Both the design and the output format. The probe is now a load-bearing piece of the test infrastructure, and reading its output (matching up the SP-delta column with the deepest trace) is itself something I hand back to Claude. Instrumentation is a good fit for the model because the inputs are narrow (what do I want to see?), the outputs have clear right answers (did the format surface the bug?), and nothing depends on taste.

- **The memory system as guardrails.** Specific examples: the memory `feedback_recovery_many` documents that "zero-width recovery inside `many`/looping combinators causes infinite loops" — because the agent had produced that bug and I corrected it. Future sessions see the memory, don't reintroduce the bug. The memory `feedback_match_fsharp_grammar` says "mirror `pars.fsy` productions, don't fake context-sensitivity with precedence numbers" — a philosophical correction that kept surfacing until it got written down. A surprising amount of LLM-assisted work is *compiling* your own preferences into a form the model can re-read.

- **Reviewing at scale.** The review discipline shifted. For a human collaborator I review code. For an agent I review the *diff of the golden file* first — because that's the behavioral specification — and then the parser diff if the behavioral change looks right. In half the cases the golden diff is small and obviously correct and I barely read the parser change at all. In the other half the parser change requires real attention, but at least I'm reading it knowing exactly what it's supposed to do.

- **What I wouldn't hand off.** Anything that touches the AST shape (post 8), the offside rule (posts 6–7), or the recovery policy (post 9). Those are architectural; getting them wrong costs weeks, and the tight-feedback loop doesn't exist. Everything downstream of them — concrete parser productions, golden-file maintenance, diagnostic wording — is fair game.

- **The caveat.** This works for a parser. Parsers have exceptionally strong test ergonomics: inputs are text, outputs are trees, errors are positions. Not every project is shaped like this. A project that can't produce a golden-file loop this tight would need a different harness, or a different role for the agent.

## Anchor files

- `DEVELOPER.md`
- `claude_tools.ps1`, `claude_tools.cmd`
- `.claude/skills/xparsec-dev/SKILL.md`
- `.claude/settings.local.json`
- `ms-fsharp-progress.md`
- `CLAUDE.md` memory files under `.claude/projects/.../memory/`

## Takeaway

A coding agent does not replace the architect. It does the eighth fix that has the same shape as the previous seven. Build the harness once: a constrained tool wrapper, a golden-file loop, an architectural primer, and a memory system that compiles your preferences into something the agent can re-read. The interesting work stays with you. Posts 12 and 13 are what the harness then does — first against a single hard problem (taming the stack, which itself was built collaboratively), then against the corpus's long tail.
