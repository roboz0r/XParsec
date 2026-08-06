# Running a cleanup sweep

For a whole-file or whole-project pass, as opposed to writing a single comment. The procedure
below took 20 files from 2544 to 1033 comment lines with zero code changes, verified
mechanically rather than by inspection.

## 1. Measure, and build the worklist

```powershell
<skill dir>/comment-density.ps1 -Path src/<Project>
```

`<skill dir>` is the directory this file is in — the scripts ship with the skill and are not
on the repo root. Substitute the base directory given when the skill was loaded.

Order the worklist **worst-first by block count and longest block**, not by ratio. That is
where the essays are. Ratio is reported for reference and mis-ranks two shapes — see the
budget section of `SKILL.md`.

Record the baseline numbers. You will be asked what the sweep achieved, and "it feels
tighter" is not an answer.

## 2. Edit per block. Never rewrite the file.

The single most important mechanical rule. A whole-file rewrite in an earlier pass pasted one
active pattern's body into another's; both versions compiled, and only the diff caught it.
Targeted edits, one comment block at a time, pass the gate first try.

## 3. The gate — three conditions, per file, before it is offered for commit

**(1) Code untouched**, proven by diffing with comment lines stripped:

```bash
diff <(git show HEAD:"$f" | grep -vE '^\s*(///|//)' | grep -vE '^\s*$') \
     <(grep -vE '^\s*(///|//)' "$f" | grep -vE '^\s*$')
```

**(2) Format leaves the file unchanged and the build is clean.**

**(3) The project's test suite is green.**

A file that cannot meet (1) has had its code edited: revert and redo, do not reconcile.

### (1) has a known false positive — stage 2

Deleting a comment CAN legitimately change layout. A comment between a `->` and its body
forces a line break; with the comment gone Fantomas collapses the arm onto one line and the
line-wise diff flags it. Same tokens, different layout. When stage 1 fails, run stage 2
before assuming an edit:

```bash
norm() { sed -e 's://.*::' "$1" | tr -s ' \t\n' ' '; }
git show HEAD:"$f" > /tmp/old.fs
[ "$(norm /tmp/old.fs)" = "$(norm "$f")" ] && echo "TOKENS IDENTICAL"
```

Stage 1 fails + stage 2 passes = pure re-layout, acceptable. **Both** failing = a real edit;
revert.

Run stage 2 AFTER formatting, since the formatter is what produces the re-layout. An agent's
own pre-format gate result does not settle it.

**Stage 2 strips `//` to end-of-line; stage 1 does not.** That is deliberate — the two stages
must differ, or stage 2 tells you nothing stage 1 did not. It also means stage 1 flags every
deleted TRAILING comment as a diff, which is a second false positive, unrelated to re-layout
and much more common. `sed -e 's://.*::'` is not a lexer: it truncates at a `//` inside a
string literal. Any file containing one will fail stage 2 spuriously — read that hunk rather
than reverting on it.

## 4. Parallelise — this is what the gate buys you

Comment-only edits are mechanically verifiable, so they are safe to fan out. One file per
background agent, three to five at a time. Files are independent; there is no ordering
constraint between them.

Each agent carries the rules from `SKILL.md`, self-checks gate (1), and reports what it
deleted and what it verified.

**The orchestrator re-runs every gate itself.** Do not accept an agent's report that its gate
passed. Across the sweep this caught nothing wrong, which is the point: it costs one command
per file and it is the only thing standing between a comment sweep and a silent code edit.

Then, per batch: format, full build, full suite, re-measure. The commit gate is the user's,
per file group.

## 5. Record type candidates — do not act on them

When a comment is genuinely load-bearing and wants more than three lines, the fix is a type
that makes it unnecessary. **Do not perform that refactor during the sweep** — it breaks the
gate, which is the only reason parallel editing is safe.

Write it into a follow-ups document instead, naming the comment it would delete. That naming
is the acceptance test: if the refactor lands and the sentence would still need writing, the
refactor was the wrong shape.

**Cut the block to three lines anyway.** Recording a candidate is not a reason to leave the
prose long — the comment is precisely what the refactor exists to delete. Check before
reporting:

```bash
awk 'FNR==1{n=0} /^[[:space:]]*(\/\/\/|\/\/)/{n++; if(n==1) s=FNR; next} \
     {if(n>3) print FILENAME": "s"-"FNR-1" ("n")"; n=0} \
     END{if(n>3) print FILENAME": "s"-"FNR" ("n")"}' <files>
```

Empty output, or the ceiling was not applied. This is the one part of the sweep an agent has
been observed to skip while reporting success, so the orchestrator should run it too.

Same for code defects found while reading comments against their code. Reading every comment
as an unverified claim is an unusually good bug-finding pass — it found four in 20 files,
including a tail-call optimisation that silently never fires — but fixing them mid-sweep
destroys the gate. Record and move on.

## 6. Order of files

Sweep dependencies before dependents. The reason is general — a consumer knows its
dependency, a dependency does not know its consumers — even though the cheap way to read that
order (compile order) is F#-specific.

Two caveats measured in practice:

- Compile position alone predicts nothing; the `open` list does.
- Container/state types are exceptions in either direction: a passive container is filled and
  drained elsewhere, so it has no behaviour of its own to describe and its prose reaches for
  whoever operates on it.

## 7. What to expect

Calibration from the one completed project, so a batch that comes back with much less can be
questioned:

| | before | after |
| --- | --- | --- |
| comment lines | 2544 | 1033 (−59%) |
| code : comment | 1.7 : 1 | 4.1 : 1 |
| blocks of 3+ lines | 364 | 91 |
| longest block | 42 | 4 |

Roughly half the deletions were H1 and H8 — mechanical, no code read. The verification budget
went almost entirely to H2 and H12, which is where every falsehood was.

The project did not reach 6:1, and should not have: nine of twenty files did, and the
stragglers were the public API surface and a 693-line file of lowering arms each carrying one
correct emitted-shape line. Ratio was never the target.

## Repo commands

Build, test and format go through the wrapper, never raw `dotnet`:

```bash
./claude_tools.cmd -Action Format
./claude_tools.cmd -Action Build -SourceProject "<Project>"
./claude_tools.cmd -Action Test -TestProject "<Project>.Tests"
```
