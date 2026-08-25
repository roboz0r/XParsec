# Running a cleanup sweep

For a whole-file or whole-project pass. The point of the procedure: a comment sweep can be
proven code-preserving *mechanically*, and that proof is what makes it safe to fan out across
parallel agents.

## 0. Get a clean baseline first — a prerequisite, not a nicety

**Run the formatter and confirm it is clean before touching a comment.** If it is not, stop
and put that to the user: format-then-commit as a separate change, then sweep — against a
dirty baseline gate (2) fails no matter what, and comment edits change where the formatter
wraps, entangling your edits with reflow stage 2 cannot disambiguate.

If the user declines, record the baseline's failures and require the *set* to be unchanged
afterwards, rather than empty:

```powershell
git show :"$f" > $env:TEMP/base.cs      # or HEAD:"$f"
dotnet csharpier check $env:TEMP/base.cs
```

Same for the test suite: record which tests were failing, and hold that set constant.

## 1. Measure, and build the worklist

```powershell
$skill = "$env:CLAUDE_PLUGIN_ROOT/skills/comment-hygiene"   # or the skill dir, if not a plugin
& $skill/comment-density.ps1 -Path <project dir> -Detail
```

The scripts ship with the skill; `$env:CLAUDE_PLUGIN_ROOT` is set only when it is installed
as a plugin, so fall back to the directory this file is in.

Order the worklist **worst-first by block count and longest block**, not by ratio — that is
where the essays are; ratio mis-ranks two shapes (see the budget section of `SKILL.md`).
Record the baseline numbers: you will be asked what the sweep achieved.

## 2. Edit per block. Never rewrite the file.

The single most important mechanical rule: a whole-file rewrite once pasted one active
pattern's body into another's, both compiled, and only the diff caught it.

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

Deleting a comment inside an expression can let the formatter collapse the construct onto one
line: same tokens, different layout. When stage 1 fails, run stage 2 before assuming an edit:

```bash
norm() { sed -e 's://.*::' "$1" | tr -s ' \t\n' ' '; }
git show HEAD:"$f" > /tmp/old.src
[ "$(norm /tmp/old.src)" = "$(norm "$f")" ] && echo "TOKENS IDENTICAL"
```

Stage 1 fails + stage 2 passes = pure re-layout. **Both** failing = a real edit; revert. Run
stage 2 AFTER formatting, since the formatter produces the re-layout.

Re-layout is not automatically acceptable: a match arm re-joined onto one line is a
whitespace edit the user did not ask for, even with identical tokens. Expect it wherever a
trailing or interior comment held a construct open, and list every such hunk (file, line,
before/after shape) in the report for the user to accept or reject case by case. Do not fold
it into "code preserved".

**Stage 2 strips `//` to end-of-line; stage 1 does not** — deliberately, or stage 2 tells you
nothing new; stage 1 therefore also flags every deleted TRAILING comment, a second, more
common false positive. And `sed -e 's://.*::'` is not a lexer: it truncates at a `//` inside
a string literal, failing stage 2 spuriously — read that hunk rather than reverting on it.

## 4. Parallelise — this is what the gate buys you

Comment-only edits are mechanically verifiable, so fan out: one or more files per background agent,
three to five at a time; files are independent. approx. 500 comment-lines per agent. Each agent
carries the rules from `SKILL.md`, self-checks gate (1), and reports what it deleted and what it verified.

**The orchestrator re-runs every gate itself** — one command per file, and the only thing
standing between a comment sweep and a silent code edit. Then, per batch: format, full build,
full suite, re-measure.

## 5. Record type candidates — do not act on them

When a comment is genuinely load-bearing and wants more than three lines, the fix is a type
that makes it unnecessary. **Do not perform that refactor during the sweep** — it breaks the
gate. Write it into a follow-ups document, naming the comment it would delete; that naming is
the acceptance test.

**Cut the block to three lines anyway.** Check before reporting:

```bash
awk 'FNR==1{n=0} /^[[:space:]]*(\/\/\/|\/\/)/{n++; if(n==1) s=FNR; next} \
     {if(n>3) print FILENAME": "s"-"FNR-1" ("n")"; n=0} \
     END{if(n>3) print FILENAME": "s"-"FNR" ("n")"}' <files>
```

Empty output, or the ceiling was not applied. Agents have been observed to skip this while
reporting success, so the orchestrator runs it too.

Same for code defects found while reading comments against their code — an unusually good
bug-finding pass, but fixing them mid-sweep destroys the gate. Record and move on.

**And the same for the inverse defect: a line that WANTS a comment and has none.** Gate (1)
passes an added comment, but the sweep's mandate is to cut prose, not to decide what the code
should say. **Flag it with the file and line,
and let the user call it**, even when the comment is obviously right.

## 6. Order of files

Sweep dependencies before dependents: a dependency does not know its consumers, so its
forward claims about its callers are where the falsehoods are. In F#, read the order from
compile order plus the `open` list; in C#, from what a file's own declarations mention —
compile position alone predicts nothing in either. One exception, in both directions: a
passive container or state type is filled and drained elsewhere, so its prose reaches for
whoever operates on it.

## 7. What to expect

One completed F# project, as calibration, not targets:

| | before | after |
| --- | --- | --- |
| comment lines | 2544 | 1033 (−59%) |
| code : comment | 1.7 : 1 | 4.1 : 1 |
| blocks of 3+ lines | 364 | 91 |
| longest block | 42 | 4 |

Roughly half the deletions were H1 and H8 — mechanical, no code read; the verification budget
went almost entirely to H2 and H12, where every falsehood was. **Both generalise less well**
(H1 nearly absent from a C# interop codebase; H12 there mostly a real calling convention that
had to stay) — re-measure rather than assuming this split. Ratio was never the target.

## Build, test and format

Use whatever this repo uses; do not invent a command or assume a wrapper exists. Check
`CLAUDE.md` and the build scripts before the first batch — gates (2) and (3) are worthless if
the commands are wrong. Formatters are per-language and a sweep touching both will need both:
for .NET, typically `fantomas` for `.fs` and `csharpier` for `.cs`.
