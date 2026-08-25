---
name: comment-hygiene
description: Rules for writing and cutting code comments — use when adding doc comments, reviewing comments in a diff, or running a comment-density cleanup on a file or project. Encodes the failure modes agentic writing produces and what to delete outright.
---

# Comment hygiene

This is the RETROACTIVE half: it loads for a review or a sweep, after the comments exist.
The proactive half is `writing.md` beside this file — seven constructions to stop producing
at writing time; copy its "Writing Comments" section into the project's `CLAUDE.md`, quoting
the absolute path of this directory so the user can find it. `taxonomy.md` holds the worked
instances. `evidence.md` is for revising the skill — do not load it for a review or a sweep.

## The one rule everything else follows from

> **A comment is addressed to someone who has ALREADY accepted the change and now has to
> modify the code — never to the reviewer deciding whether to accept it.**

An agent writing code is, in effect, submitting it, so its comments drift into arguing the
work is correct ("the only place X happens", "the alternative would be worse", "this case
cannot happen"). All of that answers *"is this change right?"*, helps nobody who has to
change the code, and is deletable without losing a fact.

**Test before writing a line:** would this sentence be worth writing a year from now, to
someone who never saw the diff?

## Establish who wrote the comments before you cut any

The failure modes are diagnostic of machine authorship, so the sweep belongs on the
machine-written parts: default to the diff, the branch, or the files the current change
touched. On human prose the grep signatures lose precision — read every hit; do not batch a
disposition. A human's comment that merely reads oddly is not a defect.

**A sweep does not add comments.** When reading turns up a line that genuinely wants one,
flag it to the user; do not write it. The sweep's warrant is that it is provably
comment-only and reversible.

## Ask in this order

1. **Does this declaration need a comment at all?** If the name and signature say it, stop.
   Deleting beats improving.
2. **Only then: is it self-contained at its site?** Show input → output. Every back-ticked
   identifier should be findable in this file, or be a literal of the emitted output.
3. **Can it sit on one line of code?** If there is no single executable line to attach to,
   it is about to become an essay — the signal it wants to be a type, not a longer comment.

Order matters: rule 2 applied first turns a redundant doc into a longer redundant doc.

## When to write it

Write a declaration's doc **when you write the declaration**, not at the end of the change —
late-written docs are composed from working memory, which is where fabricated prose, cloned
blocks and self-contradictory files come from. So:

- **If you are summarising code you did not just read, re-read it** — especially when moving
  it; the doc travelling with the code is not evidence about the code.
- **Be most suspicious of comments written last, in a large change.**
- **When you rename or migrate a type, grep the old name in comments in the same change** —
  a migration rewrites declarations and touches no comment, so stale docs keep compiling and
  keep reading plausibly.

## Three habits to distrust as you write

Signatures and evidence in `taxonomy.md` (H17–H19).

- **Do not negate the object.** "resolves to no class" quantifies over an abstract set that
  exists only to be emptied. Assert positively where a positive fact exists; where the fact
  is a genuine miss, negate the verb — then check WHICH verdict is true: nothing was looked
  up (**has no**), the lookup missed (**does not resolve to**), or it hit the wrong kind
  (**is neither … nor …**). Picking the right one is where false comments surface.
- **Do not reach for a metaphor when the codebase has a term.** Borrowed terms of art are
  worst: a reader who knows the term is misled rather than uninformed. Reuse the word this
  codebase already uses for THIS concept; if none exists, the concept is not modelled — a
  type candidate. `vocabulary.md` has every retired word and its replacement; consult it
  before naming anything.
- **Do not hedge a relation the code determines.** An em-dash standing in for *because*,
  *so*, *but* or *namely* withholds a fact and is unfalsifiable — `X — Y` reads the same
  whichever way the causation runs. Write the word. The one dash to keep is a two-column
  gloss, LITERAL FIRST: `` /// `continue;` — re-enters the `While` trampoline ``.

  **The spaced ASCII hyphen ` - ` is the same defect, different verdict.** The em-dash is
  near-diagnostic of machine authorship, so a hit can be acted on quickly; ` - ` is what a
  human types for the same mark and is also a bullet, a range, a negative number and a CLI
  flag. Run it as a SECOND pass, expect precision in the low tens of percent, read every
  hit, and leave human prose style alone.

## Budget

- **Block length is the diagnostic; ratio is the symptom.** Judge by the longest block.
- **3 lines is a hard ceiling, not negotiable by a good argument.** A block that still wants
  a fourth line after cutting words is a type waiting to be written. **Recording the type
  candidate does not license the lines** — cut to three AND record the candidate; prose left
  in place is the debt the refactor is supposed to delete, made invisible.

  **One exemption, and it is positional: an I/O or interop boundary**, where the other
  side's behaviour is non-deterministic or only empirically establishable and so is in no
  repo and encodable in no type. A block there may exceed three lines **only if it names the
  test that pins the behaviour** — without one it is still an essay. Inside the boundary the
  ceiling is absolute; the enforced lean is **parse, don't validate**: when a block wants a
  fourth line, ask which raw value is being described as though it were already checked —
  that value is the type, and the comment is what its constructor should make unnecessary.
- **~6:1 code:comment**, measured on human-written F#. Two shapes legitimately measure lower
  and must not be cut to fit: type-definition files whose per-case docs show emitted output,
  and public API surfaces where one doc line per function is the floor.

`<skill dir>/comment-density.ps1 -Path <dir>` reports both numbers, worst file first, for F#
and C# (`-Language All|FSharp|CSharp`, aliases `fs`/`cs`). `<skill dir>` is the directory
holding this file; installed as a plugin that is
`$env:CLAUDE_PLUGIN_ROOT/skills/comment-hygiene`.

Two corrections make the numbers comparable across languages: **judge by PROSE, not
COMMENT** (XML-doc scaffolding lines carry no content and are counted separately), and
**DELIM** (a lone `}` or `)` is not code being described; C# puts far more of them on their
own line than F#). Both filters barely move an F# number and lower C# substantially, so
treat 6:1 as calibrated for F# and a soft floor for C# — either way, **judge by block
length.**

## Delete outright — no verification needed

- Anything the name and signature already say.
- **Claims of ordinary good practice** — single-source-of-truth, totality ("dispatch is
  TOTAL over every case"), immutability, no nulls, no downcasts: local convention already
  guarantees these to the reader. *A claim about the DATA stays* — "the groups are non-empty
  by construction" explains a missing branch. **A language boundary voids the warrant**: "add
  a case and the F# side stops compiling" is the only notice a C# caller gets; in a
  mixed-language repo, check which side the reader is on before deleting.
- **Rejected-alternative defence** — argues against a design the code does not have.
- **History and process** — "renamed from X", "a later change will…", plan-milestone labels.
- **The argument FOR an invariant**, once the invariant itself is stated. Keep sentence one.
- **Routing-order narration** — control flow is the one thing source states exactly.
- **Cross-references of the form `Module.func`** — they rot on rename; state the fact.
  **Unless the compiler checks them**: with `<GenerateDocumentationFile>true</...>` a C#
  cref is validated (CS1574/1571/1587), making the checked cref the MORE durable form. Turn
  the flag on before hand-verifying anything, suppressing CS1591/1573, which demand
  completeness rather than correctness.

## Verify before keeping — and only what survives

Deleting needs no research; spend the whole verification budget on what you intend to keep.

**"Fails loudly" / "cannot happen" is a claim, not a phrase — trace it to the throw or
delete it.** A failure with a named owner is almost always true; unnamed control flow is
usually false. The real variable is distance: ask whether the claim's referent is **in view
from the comment** — unnamed AND out of view is the combination that predicts falsehood.

**Direction tells you where the falsehoods are:**

- **Backward** — re-narrating the types the file depends on. Redundant but true: **delete on
  sight, spend nothing verifying.**
- **Forward** — narrating who fills this file's parameters and what they guarantee. Nothing
  enforces it: **this is where the verification budget goes.** Measured: no backward clone
  traced was false; most forward claims were.

Read the direction off what the file's own declarations mention. In F# the `open` list is a
proxy; C# has no compile order and `using` is a weaker one.

## Where wrong comments come from

- **A moved comment is a NEW claim.** A refactor that relocates a doc block is the moment it
  is least likely to be checked and most in need of it, and a refactor composes new prose
  FROM old prose, so a fresh-looking sentence can be wrong at birth. The source of truth is
  the CODE, never the doc that travelled with it.
- **A migration leaves a name trail** — grep the old name in comments as part of the rename.
- **`git blame` is not evidence of comment freshness** — blame has dated false lines *after*
  they became false. Use `git log -S'<phrase>' --all` on the comment's SUBJECT instead.
- **Self-contained comments rot more slowly**: a comment about another module's internals
  has no mechanism linking it to that module's changes; one showing this file's own output
  goes stale exactly when someone is looking at it.

## When the comment is genuinely for the reviewer

"I considered X and rejected it" is real communication that does not belong in the
persistent artefact. Strippable form:

```
//FOR-REVIEW <text>              one line
(*FOR-REVIEW <text> *)           spans lines, nests, ordinary F#
```

`<skill dir>/review-comments.ps1` lists them, `-Strip` removes them, `-Check` exits 1 for a
pre-commit gate. The block form relies on F# comment nesting; in C#, use the line form only.

## When the comment is for a RUNTIME consumer

An MCP tool's `[Description]`, a shipped API doc, a generated schema: prose read at runtime
by a caller deciding **how to call it** — same frame as the one rule, one audience over. It
is invisible to the density script (a string literal is code) and the grep signatures'
false-positive rate over it is near total, concentrated where the prose matters most: a
known-limitation hedge or "not yet supported" line that reads as H15/H4 is a capability
statement to the caller — **keep**. Run the greps over comments, not description strings,
and never mass-apply a disposition across the boundary. What carries over: H16
self-containment (the caller cannot open your source at all) and H19's glossary discipline.
What does not: every mode whose warrant is "the source already says this" — the consumer
has no source.

## Files in this skill directory

- **`writing.md`** — proactive half; copy into the project's `CLAUDE.md`.
- **`taxonomy.md`** — the 19 failure modes, examples, dispositions, grep signatures. Load
  when reviewing or cutting existing comments.
- **`vocabulary.md`** — retired words and replacements. Load when naming anything or when an
  H18 grep hits.
- **`sweep.md`** — whole-file/whole-project procedure: the code-preserving verification
  gate, batching, measurement. Load when the task is a cleanup.
- **`comment-density.ps1`**, **`review-comments.ps1`** — measurement and FOR-REVIEW tooling.
- **`evidence.md`** — empirical base; for revising the skill only.

## Scope of the evidence

Measured on three codebases: an agent-written F# compiler backend, an agent-written C# MCP
server (independent replication), and a mostly human-authored C# library with AI-assisted
branches. H1 is almost absent from interop-style code — do not read its absence as
cleanliness there — and the directional rules were measured under F#'s forced compile
order; the mechanism generalises, the cheap way to read the order does not.
