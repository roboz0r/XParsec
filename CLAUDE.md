# XParsec / Vesper

An XParsec-based F# compiler: lexer and CST parser (`XParsec.FSharp`), semantic analysis, and
CLR and JS backends. `src/Vesper.*` is the standard library initially ported from FSharp.Core.

## How to read this codebase

The CST parse covers the modern F# grammar and has earned some deference. SemanticAnalysis and
both backends are prototype with many open issues, so when the parser and a later pass disagree,
suspect the later pass. A tightening change that turns a green SemanticAnalysis or Codegen test
red is a finding rather than a regression, because the test was probably pinning the old wrong
behaviour.

Nothing here is shipped, so no current behaviour is a baseline to preserve. F# parity is a goal, however where correct
semantics and F# parity conflict, choose correct semantics.

Where comments and code disagree confirm with the user, the expected semantics.

## Tooling

- Build, test, format and benchmark through `./claude_tools.cmd`, never raw `dotnet`. The
  `xparsec-dev` skill has the actions and the valid project names.
- `dotnet fsi --nologo x.fsx` is the sanctioned exception to that rule, and it is the oracle for
  F# semantics: probe it for what F# accepts and match its exact FS code instead of inventing rules.
- Scratch files belong in repo-root `./tmp/`, because writes outside the repo prompt for
  permission.
- Create and edit files with Write/Edit, not shell heredocs. Do not process text with Python;
  write a small F# file and iterate with the test loop.

## Working agreement

- Do not run `git commit`. Leave the work green and summarised for the user to review and
  commit. A per-task "commit once I approve" overrides this for that task alone.
- Do not curate commit boundaries. Finish entangled work rather than unwinding it to keep a
  boundary clean.
- Open the cited lines and check the disk before calling anything superseded, legacy or dead. A
  plan doc states the intent of the moment it was written, and is likely to shift as multiple plans are pursued in parallel.

## F# design rules

- Put a durable fact in the type system where it can be correct by construction, else in a test
  that fails without it, else in a sited comment, in that order of preference.
- Immutable records and discriminated unions processed by pure functions should be the default shape of code.
  - Use `[Value]Option` and `Result` extensively but beware of `[Value]None` standing in for multiple downstream interpretations. That signals the need for a specific DU.
- Mutation is justified on performance or algorithmic elegance grounds but must have a clear scope boundary.
- Interfaces shall be used to segregate deterministic and non-deterministic code.
- A tuple of three or more components becomes a record. Pairs are fine, except `string * string`.
- "Parse, don't validate". Use types to encode facts about values. Score the change
  by the runtime checks it deletes, not the sites it touches.
- A stage that discards an intermediate makes consumers re-derive it is wrong.
  Inevitably the two re-derivations disagree, and the disagreement then needs to be fixed by passing the intermediate, which should have happened when it was written.
- `list` has two uses: a collection always iterated from 0, and an immutable stack. Reaching for
  `@`, `List.rev` beside a consumer, `List.item` or `.Length` in a loop means the structure is
  wrong, so change the structure rather than the call site.
  - Use `EqArray` or `EqSet` for array-backed immutable collections with structural equality.
- `[<Struct>]` only where object creation dominates passing. A value threaded through a provider chain
  stays a reference record.
- Swap a widely-used underlying type additively, behind a central alias, and delete the old one
  in a separate change.
- Excise a wart documented as harmless. The cleanup usually surfaces an invariant the compiler
  can enforce, such as a field that proves to be write-once and can lose its setter.
- A guard excused by "a consumer would fail" is papering over that consumer. Enumerate what it
  drops on real input before accepting it.
- Scoping is strictly top-down. Machinery that resolves a name written later is a bug, not a feature. `let private` helpers must precede their use, and `and`-joined recursive groups are the only exception.
- A record of closures (function-typed fields) is a smell that requires strong justification. Consider alternatives in order of preference:
  - Reordering to break a cycle
  - For narrow scopes, use indivudal function parameters
  - For sharing state across a boundary, use an interface
- Do not rely on access modifiers to "protect" state. Being concerned that a user will use the
  type wrongly is a signal the API is poorly designed.

## F# Nits

- `match xs.Length with | 0 -> … | n -> …` over `if xs.Length = 0`.

## Writing Comments

Do not write the case for why the code is right. Assume the reader is a caller looking for a contract, not a reviewer looking for a justification.

Seven constructions to stop producing:

1. **Definition by derivation** — Defines a type by the operation that made it, forcing the reader to replay a procedure. Define a type by what is *is* and/or what it *guarantees*. (Functions are the exception: a function's doc SHOULD say what it computes).
   - *DON'T:* "A compilation's defined symbols, narrowed to those..."
   - *DO:* "The subset of compilation symbols referenced by..."
2. **Premise-then-inference (Defensive Writing)** — Do not state a premise just to prove the conclusion. State the consequence alone, with committed modality (*will*, *guarantees*). The caller wants the contract asserted, not proved.
   - *DON'T:* "Parsing consults no other symbol, so two compilations that agree here parse identically."
   - *DO:* "If two compilations share this exact subset, they will parse identically."
3. **Object-extracted relative clauses with heavy possessive subjects** — The verb lands last, the subject is a two-level possessive, and the reader has to re-read. Use a reduced passive.
   - *DON'T:* "those one file's `#if` lines name" / "branches no define set makes active"
   - *DO:* "referenced by a file's `#if` directives" / "inactive branches"
4. **Exclusion framing** — Forces the reader to hold a universe and subtract. Assert positively.
   - *DON'T:* "consults no other symbol" / "drops nothing parsing can read"
   - *DO:* "depends exclusively on" / "retains all parsed elements"
5. **Anthropomorphism & Coined Verbs** — Do not assign cognitive actions to code processes, and avoid spatial/temporal deixis.
   - *DON'T:* "parsing *consults*", "agree *here*", "to hand", "name" (as a verb)
   - *DO:* "evaluates", "share", "available", "referenced by"
6. **Emphatic singularity** — Marks a singularity nobody questioned, which costs a referent re-establishment in every later sentence.
   - *DON'T:* "that file" / "one file's"
   - *DO:* "the file" / "a file's"
7. **Restating the type signature** — A member whose meaning is exhausted by its name and type gets NO comment. An `Empty` static needs nothing. Do not explain that a type is a `Set` if the F# type signature already says `Set<_>`. Write type annotations on parameters where the function's inferred type is non-obvious.

**Why:** The underlying instinct of an AI is to write comments as an argument for the design instead of a description of the value. Exclusion framing, premise-then-inference, and derivation histories are symptoms of trying to persuade someone the code is correct.

**How to apply:** After drafting any doc comment, check whether it would survive the code being obviously correct. If a clause only exists to prove the code is right, delete it. Then check for a trailing verb in a relative clause, cognitive verbs assigned to systems, and the words "no/nothing/never".

## src/Vesper.* (the runtime port)

Transliterate FSharp.Core as literally as possible, carrying `// FSharp.Core/list.fs:142` style
references, so the port stays diffable against upstream as it evolves. Do not propose a
signature-only port, a rewrite or a reorganisation unless asked.

The `.fsi` XML docs are FSharp.Core's own. They are user documentation, so comment density scores
do not apply and a comment sweep must leave them alone. The only target is
agent-authored prose layered around them, and the discriminator is vocabulary, because
FSharp.Core docs never say splice, intrinsic, repr, backend, target, front end or manifest.

## Skills and More Specific References

- `.claude/skills/comment-hygiene` — writing or cutting comments, plus `vocabulary.md` for terms
  this repo has retired.
- `.claude/skills/planning-workflow` — design docs, plan docs, and orchestrating subagents.
- `.claude/skills/perf-tuning` — benchmarking, and the optimisations already tried and reverted.
- `.claude/skills/xparsec-dev` — build, test, format, benchmark, Fable.

A rule that applies to one subsystem belongs in that subsystem's `CLAUDE.md`, and a procedure belongs in a skill.
