# 10. The corpus test: where theory meets reality

## Hook

For six months I wrote tests by imagining what F# syntax might look like. Then I pointed the parser at the full source of the F# compiler itself — 249 `.fs` files of the actual implementation, including `FSharp.Core`. Not the compiler's test suite (which would be a bag of weird edge cases by design) but the production code that is the F# language. About a third parsed clean. A third produced diagnostics. A third crashed the process with a stack overflow. The next three months of commit history is mostly working through that list. At the time of writing, all 249 files parse clean with zero diagnostics. This post is the story of how.

I wrote the parser as an *F# user*, not as a language designer. That's exactly the wrong hat for catching the syntax that actually lives in the long tail of the corpus. Idiomatic F# — records, discriminated unions, pattern matching, computation expressions — was well-covered by my hand-written tests because I'd written so much of it. What the corpus kept surfacing was a different shape of code entirely: features I'd never written, because I'd never needed to.

## What this post covers

- **The infrastructure.** Commit `8d00638 Add corpus parsing infrastructure`. A subprocess-per-file runner (`--parse-file <path>` mode) because .NET cannot catch `StackOverflowException` in the same process; a `--corpus <subdir>` runner that walks a directory and aggregates results; a `REPORT.txt` summary plus one `.fs.parsed` file per corpus entry. Show the invocation:
  ```
  ./test/XParsec.FSharp.Tests/bin/Debug/net10.0/XParsec.FSharp.Tests.exe --corpus corpus
  ```
- **The REPORT.txt format.** One line per file: status (CLEAN / DIAG(n) / ERROR / CRASH) and counts. The triage policy is "start with DIAG(1)" — those have exactly one thing failing, which almost always has a single root cause.
- **Why stack isolation matters.** Without subprocess isolation one bad input ends the run. With it, crashes become tagged data points. The stack-probe itself (`parseWithStackProbe`) intentionally pre-checks remaining stack before deep recursion, so many overflows now produce a clean error instead of process death.
- **The triage loop.** The workflow documented in `ms-fsharp-progress.md`:
  1. Read `REPORT.txt`. Pick a file with DIAG(1-5).
  2. Copy it to `data/manual/`, change the step-debug test to `ftest`, run with `-UpdateSnapshots`.
  3. Read the `[error]` lines in the resulting `.fs.parsed`.
  4. Write a minimal repro (`222_primary_constructor.fs` style), run tests, verify the repro fails the same way.
  5. Fix the parser, verify the repro now parses cleanly.
  6. Re-run corpus, compare `REPORT.txt` to previous.
- **What the corpus found — three categories.** The bugs sorted cleanly into three buckets that nicely explain why hand-written tests had missed them.

  1. **Features the spec didn't document yet.** The published grammar (post 8) is years behind the language. Anonymous records, `fixed`, static optimization syntax, SRTP member constraints, mixed named/positional arguments, light-mode record expressions — all real, all in the corpus, all missing from my initial AST. Example commits: `4f30b3a Parse anonymous records`, `0e86188 Support fixed keyword`, `65d7336 Parse SRTP member constraints`.

  2. **Internal-only, undocumented features.** `FSharp.Core` uses syntax that no application code ever touches: IL intrinsics `(# "add" x 1 : int #)`, static optimization conditionals `when 'T : int`, the `.N` positional field access on discriminated unions, static typars `^T` in constraint positions. These aren't "edge cases" — they're deliberately undocumented, because they exist to let the standard library do things the language otherwise wouldn't allow. I couldn't have written a test for them because I didn't know they existed. Example commits: `f02a8d6 Improve IL intrinsic parsing`, `243d29f Parse expressions containing the dot operator in IL intrinsics`, `d9a06a4 Parse StaticTypars, .N positional DU field access, f[]`, `4cc638b Parse static optimization syntax`.

  3. **Obscure details of the original ML-compatible and verbose-mode syntax.** F# has a `#light "off"` mode that restores the original OCaml-ish whitespace-insensitive syntax — `begin`/`end` for blocks, explicit `in` for every `let`, ML-style type annotations, semicolon-separated sequences. Nobody *writes* this anymore, but the corpus and `FSharp.Core` both exercise it, and the spec treats it as first-class. Example commits: `70dede2 Parse ml style types`, `391b8de Parse begin-end block`, `6cf7f88 Parse implicit in, expression sequence`, `ad1c44c Fix parsing keyword prefixed expressions`.

  Plus the long tail: `5dd1a88 Fixing several parsing bugs` (a *lot* of fixes in one commit, all corpus-originated), `17172d4 Parse all string literals as fragments` (the old "one string = one token" assumption was wrong), `9c21b90 Fix parsing multiple secondary constructors`, `51691aa Allow parsing inherit anywhere in type declarations`, `ef56004 Fix parsing of flexible types`, and half a dozen offside fixes referenced in post 6.

- **The last things to parse were the first things a beginner learns.** The closing irony of the corpus work, and maybe the whole project. Among the very last files to parse cleanly were `369_gadt_du_case_signature.fs` and `370_gadt_operator_case_name.fs` — minimized reproductions of how `Option<'T>` and `List<'T>` are actually declared in `FSharp.Core/prim-types.fs`:

  ```fsharp
  type Option<'T> =
      | None :       'T option
      | Some : Value:'T -> 'T option

  type List<'T> =
      | ([])  :                  'T list
      | ( :: )  : Head: 'T * Tail: 'T list -> 'T list
  ```

  GADT-style per-case type signatures. Parenthesized-operator case names (`([])`, `( :: )`). Every F# tutorial starts with `Option` and `List` — they are the first two discriminated unions any learner meets. The definitions of those two types use syntax obscure enough that they landed in the last 1% of the corpus to pass. That gap between "the language as taught" and "the language as implemented in its standard library" is what the corpus test was really measuring.
- **Emergent bug patterns.** Fixing one DIAG(1) file often flips others from CRASH to DIAG(1) — the parser now reaches deeper and hits a different pre-existing bug. Progress looks wiggly on any single file and monotonic on the aggregate count. This is the right shape to look for.
- **The final three.** The last session's chain from 248/1 to 249/0 was captured in a single table: an SRTP struct-constraint fix (fixture 371), a lexer `+` handling bug guarded by `inComment` (fixture 372), and the `.. ..` operator-name fusion (fixture 373) — this last one covered in the bonus post. The final diagnostic was in `FSharp.Core/prim-types.fs` at line 5198, a `when 'T : struct` constraint. Once that parsed, the report read `249 clean / 0 DIAG`. What had been a persistent triage queue for months resolved in a single session.
- **What the corpus didn't find.** A reality check: the corpus is the F# compiler's own implementation, which means it's biased toward the code the compiler authors write — liberal use of intrinsics, inline IL, GADT-style DU declarations, `#light "off"` holdouts. It under-samples idiomatic application-level F#: domain records, discriminated unions modeling business data, computation expressions used for real work. A second "idiomatic corpus" drawn from popular GitHub F# projects is the natural next step, and is likely where the next class of bugs lives.

## Anchor commits / files

- `test/XParsec.FSharp.Tests/TestHelpers.fs` (`corpusTestData`, `tryParseCorpusFile`)
- `test/XParsec.FSharp.Tests/CorpusReport.fs`
- `test/XParsec.FSharp.Tests/Program.fs` (`--parse-file`, `--corpus`)
- `ms-fsharp-progress.md` (the whole doc)
- `8d00638`, `5dd1a88`, `17172d4`, plus the ~80 commits that follow

## Takeaway

You cannot hand-write a test suite that finds what a real-world corpus finds — especially if, like me, your internal model of the language is "what I write," which is a tiny slice of what the language actually admits. The corpus exposes the code written by *other people for other reasons*: the compiler authors exercising their own intrinsics, the `FSharp.Core` maintainers using syntax the spec hasn't caught up to yet, and the occasional ML-compatible holdout from ten years ago. Build the harness first, triage by diagnostic count, and trust the aggregate trend over any single file.
