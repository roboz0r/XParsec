---
name: planning-workflow
description: How design docs and plan docs work in this repo, and how to run a handed-off multi-step plan through subagents. Use when a change turns out to be an architectural redesign, when reading or writing a src/**/docs/*-plan.md, or when orchestrating subagents through a scoped plan.
---

# Planning workflow

## Plan docs are ephemeral

`src/**/docs/*-plan.md` are working documents. Each exists to land one scope of work, and once
that work is complete the document is deleted, together with every comment reference
to it across the codebase. The code and its tests are the canonical record.

- Do not cite a plan doc as authoritative in a review finding. Where code and a plan doc
  disagree, the code wins and the doc is the thing to delete.
- When a scope completes, offer to delete its `*-plan.md` and to strip the parenthetical
  citations from source and test comments, replacing each with the substantive point.
- The `§` glyph in those citations breaks Edit's exact match. Use PowerShell
  `[IO.File]::ReadAllText` and `.Replace` for lines containing it.
- Never put a milestone label ("Wall 3", "Step 1/2", "tranche", "R5") in code or test comments.
  The comment has to still make sense once the doc is gone.

**A decision record is not an implementation plan, whatever it is named.** A doc with numbered
decisions and revisit triggers, rather than steps and commit boundaries, binds work that has not
started, so it cannot be landed and deleted. Check which kind it is before offering to delete it.

## Verify a plan doc's premises

A plan doc states the intent of the moment it was written, and a subagent citing one will
happily invert its meaning. "Not yet pointed at X" meant *ready, awaiting adoption* and was read
as *abandoned*. The direction of travel between two mechanisms is exactly the fact a stale doc
gets backwards, and it is the fact deciding whether code is dead or pending.

Before calling anything superseded, legacy or dead:

1. Open the lines the claim cites. Never relay a subagent's characterisation of a doc it read
   for you.
2. Check what still exists on disk and what reads it. A build with zero references is proof; a
   doc sentence is not.
3. Ask the user whether the plan's premise still holds, since they know the project moved on.

The same applies to an in-code comment in SemanticAnalysis or either backend.

## When a change turns out to be a redesign

Capture the analysis into the relevant design or plan doc as the review artifact **before**
writing code, and before asking a meta "how should I proceed" question. The doc is the durable,
reviewable deliverable; a three-way "proceed?" question is noise when the real need is a written
plan the user can read and correct.

Write root cause, staged plan, and scope and risk. Supersede any stale section. Surface every
semantic assumption for explicit confirmation, because the user vets those personally as part of
reviewing the doc.

## Design forks

Present the tradeoffs in prose with file:line grounding and a recommendation, then ask an open
question and let the user respond freely or sketch code. Do not force an AskUserQuestion
multiple-choice: it collapses a nuanced design space too early, and the options are usually
underspecified or a false binary. Reserve it for genuinely orthogonal, well-specified choices
such as a scope toggle.

## Orchestrating a handed-off plan

When the user hands off an already-scoped, detailed plan, dispatch background subagents one step
at a time along the dependency chain and act as the review and commit gate. Keep the channel
open so design forks can be discussed while an agent runs.

### Duplication is a separate gate from correctness

Subagents reliably open-code a helper rather than reuse or introduce one, because each works in
isolation and cannot see that a sibling wrote the same four lines. An adversarial review agent
scoped to find *bugs* will pass a diff that is correct but duplicative, so run the duplication
check explicitly on every diff, alongside the bug pass, and fix it before the commit lands.

- Grep the shape the diff just added at the other sites doing the same thing. Three or more
  open-coded copies means write the helper.
- Prefer a helper whose *signature* carries the invariant the duplication was violating.
- **Mirror-shaped tasks are the worst offenders.** Telling an agent to "mirror `ClrDriver.parse`"
  all but guarantees a verbatim copy, so instruct such an agent up front to factor the shared
  parts into the common home rather than copying.
- The best fix removes the *reason* to copy: a builder that wires the shared idiom internally, so
  the next producer cannot re-roll it, beats extracting two helpers.

## Systematic tests over whack-a-mole

When fixing one bug surfaces several interacting issues in inference or the unifier, stop
patching. Write systematic isolation tests, then a plan classifying root causes and sequencing
the fixes, and pause for agreement before implementing. Expect red: the red surface is the
deliverable.

## Before a plan doc is deleted

Migrate each durable fact into code and strike it off a migration checklist in the doc itself.
Preference order: a shape that cannot be expressed wrongly, then a test that fails if it is,
then a comment that explains why. A comment alone is the weakest, last-resort form, and a doc is
not a durable home at all.
