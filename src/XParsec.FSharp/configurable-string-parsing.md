F# allows multiline inside string literals but I'm not sure this is a good design. Particularly considering the later addition of triple quote strings. If I were designing an F# successor should I restrict ordinary strings to a single line. Newline characters become an unterminated string literal parse error at the line ending

---

Short answer: yes, restricting ordinary `"..."` strings to a single line is a defensible and increasingly common design choice for a modern successor language.

The arguments in favor are strong. Unterminated strings are one of the most damaging parse errors in any language because the lexer happily eats subsequent code as string contents until it finds another quote, often hundreds of lines away. Forcing newlines to terminate `"..."` turns "missing closing quote" into a tight, local error instead of a cascading one. It also matches what most editors already assume when doing syntax highlighting and bracket matching, so tooling gets simpler and more accurate. Rust, Swift (for regular strings), Kotlin, and C# (non-verbatim) all take this line. The fact that F# has `"""..."""` (and `@"..."`) means the multiline case is already served by a dedicated construct, so single-line `"..."` loses no expressiveness — it just removes a footgun.

The arguments against are mostly legacy and ergonomics. Some people genuinely use `"\n"`-containing literals for short two-line messages without reaching for triple quotes, and existing F# code would need migration. There's also a mild inconsistency cost: you now have two string syntaxes with different rules rather than one syntax with optional extensions. Neither is a serious objection for a successor language where you control the migration story.

A few design details worth nailing down if you go this route: decide whether a backslash-newline continuation is allowed (Swift and Python both permit it and it's genuinely useful), whether a literal tab inside `"..."` is allowed or must be `\t` (strict is better — invisible characters in strings cause real bugs), and whether the error message specifically suggests `"""` when it sees a newline mid-string. That last one is a small thing that makes the restriction feel helpful rather than punitive.

So: restrict `"..."` to one line, keep `"""..."""` for multiline, and make the lexer error message point users at the triple-quote form. That's a clean, modern design.

---

Should I also follow C# semantics for triple quote strings? i.e. whitespace to the first character establishing a LHS indent that gets trimmed in the output.

---

Yes, the C# 11 raw string literal design is the right model to follow, and it's become something of a de facto standard for new languages. The core rules worth adopting:

The closing delimiter's indentation establishes the margin, and that amount of leading whitespace is stripped from every content line. Any content line with less indentation than the closing delimiter is a compile error, not a silent pass-through — this catches mistakes where someone accidentally outdents a line. The opening delimiter must be followed by a newline for the multiline form (a single-line triple-quoted string is allowed but doesn't engage the indentation logic). Tabs and spaces are not interchangeable when computing the margin; the prefix must match exactly. And the number of quotes is flexible (`"""`, `""""`, etc.) so embedded triple quotes work without escaping.

This design has several things going for it. The indent is visually obvious from the closing delimiter rather than inferred from the "least indented line" (which is what Python's `textwrap.dedent` and Swift do, and which has surprising failure modes when you add a line that happens to be flush left). Errors are local and clear. The result reads naturally in source — the string visually lines up with surrounding code but the runtime value is clean. Swift's multiline strings work essentially the same way and the experience has been positive.

A couple of choices to make explicitly: decide whether the leading newline after the opening `"""` is stripped (C# yes, and you should too — otherwise every multiline string starts with an awkward `\n`), and whether the trailing newline before the closing `"""` is stripped (C# strips it; this is the right call because `"""` on its own line then means "no trailing newline" and `"""` after content on the same line means "trailing newline" — actually wait, C# does include behavior here worth checking). The cleaner rule is: the newline immediately after opening `"""` and immediately before closing `"""` are both part of the delimiter, not the content. If you want a trailing newline, add a blank line before the closer.

One small extension worth considering that C# doesn't have: allow a language-level `\` line-continuation inside raw strings to suppress a single newline, which is occasionally useful for very long lines that you want to wrap in source but not in the value. Swift has this. It's a minor convenience but it composes well with the indentation rules.

So: yes, copy C# 11's design, strip the bracketing newlines, and you'll have something users coming from C#, Swift, Rust (which is moving this direction), and Java (text blocks, similar idea) will all find familiar.

---

What about line endings? I feel like a triple quote string should emit Environment.Newline to not bind the source ending to the output. It's common for e.g. git to silently rewrite line endings

---

I'd push back on this one. Emitting `Environment.NewLine` from string literals is a surprisingly bad idea in practice, even though the motivation is sympathetic.

The core problem is that string literals should be deterministic values. If I write a literal and hash it, write it to a network protocol, compare it in a test, or embed it in a file format, I need to know exactly what bytes it contains. Making the runtime platform determine the content means the same program produces different values on Windows vs Linux, which breaks: golden-file tests, cryptographic signatures and HMACs over literal content, network protocols that mandate CRLF (HTTP, SMTP) or LF (most everything else), file formats with specified line endings, snapshot tests, and any cross-platform reproducibility story. CI passes on Linux, fails on a Windows dev machine, or vice versa.

The git-rewriting concern is real but it's the wrong layer to fix it at. The standard solutions are well-established: a `.gitattributes` file with `* text=auto eol=lf` (or explicit per-extension rules) pins line endings in the repository, and `core.autocrlf=input` on developer machines prevents the silent rewrite. Most modern projects just standardize on LF everywhere including Windows, and it works fine. If your language's literals silently change based on platform, you've moved the nondeterminism from "fixable with a config file" to "baked into the language semantics."

There's also a subtle issue: what counts as a "newline" for this rewriting? Just the ones from source line breaks, or also `\n` escapes? If only source breaks, you now have two ways to write a newline with different semantics, which is confusing. If both, then `\n` no longer means LF, which breaks every programmer's mental model and every piece of code that assumed `\n` is `0x0A`.

The right design, I think, is: triple-quoted strings normalize source line endings to `\n` at parse time (so CRLF in the source file becomes LF in the value), and `\n` always means LF, `\r\n` always means CRLF. This gives you reproducibility and a simple mental model. If a user wants platform newlines, they call an explicit API — something like `String.toPlatformLineEndings(s)` or a formatted-print function that does the conversion on output. That keeps the platform-dependence at the I/O boundary where it belongs, not inside literal values.

Swift, Rust, C#, and Java text blocks all normalize to LF in the literal. It's the consensus answer and I think it's the right one.

---

If I added some flags into the parser config I could support parsing F# and my "F#2" Swift /Rust/C# string literals fairly easily and write a simple transformer

---

That's a sensible migration strategy and the parser-flag approach is exactly how most language successors handle this. A few thoughts on making it work well.

The flag set you probably want is something like: `allow-multiline-regular-strings` (F# legacy on, F#2 off), `triple-quote-indent-mode` (none for F# legacy, c-sharp-style for F#2), and `triple-quote-newline-normalization` (off for F# legacy preserving source bytes, lf for F#2). Keep them orthogonal rather than a single "dialect" flag — it makes the transformer's job clearer and lets you handle weird in-between files during migration.

For the transformer itself, the regular-string case is the interesting one. When you encounter a multiline `"..."` in F# source, you have a few choices: convert it to a triple-quoted string (cleanest if the content has no `"""` in it), convert it to single-line with explicit `\n` escapes (always safe, sometimes ugly), or concatenate single-line literals with `+` or your equivalent. I'd lean toward triple-quote conversion when the string has two or more lines and escape-conversion when it has exactly one embedded newline — the latter usually reads better. Make this configurable per-codebase since taste varies.

A subtle thing to watch: F# multiline strings preserve the source's line endings verbatim, but your F#2 triple-quoted strings normalize to LF. If you convert a multiline `"..."` to `"""..."""` you're potentially changing the runtime value on Windows checkouts where the source has CRLF. For most code this doesn't matter, but if anyone is hashing literal content or writing them to byte-sensitive formats, you'll silently break them. The safe transformer behavior is to emit `\n` escapes (which were always LF) rather than rely on the new triple-quote normalization — that guarantees byte-identical output. You can offer a `--prefer-triple-quotes` flag for users who know their code is fine.

The indentation translation is the other thing worth thinking through. F#'s existing `"""..."""` doesn't do C#-style indent stripping, so any existing triple-quoted strings in F# source will have different runtime values under F#2 rules. The transformer needs to detect existing `"""..."""` usage and either rewrite the indentation to match the new semantics or flag them for manual review. I'd flag rather than auto-rewrite — these are rare enough in practice and the value change is exactly the kind of silent breakage you want a human to look at.

One more practical suggestion: have the transformer emit a comment or sidecar report listing every literal it touched and the rationale, especially for the indent-sensitive cases. Migration tools that show their work get trusted; ones that don't get blamed for every subsequent bug.

---

I'd make @"..." and $"... " also single line only

---

Agreed, and this is the right call for consistency. Once you've decided that `"..."` is single-line, having `@"..."` and `$"..."` keep multiline behavior creates an inconsistent rule set where users have to remember which prefix opts into which behavior.

The cleaner mental model is: single quote count means single line, regardless of prefix; triple quote count means multiline with indent stripping, regardless of prefix. So `"..."`, `@"..."`, `$"..."`, `$@"..."` are all single-line, and `"""..."""`, `$"""..."""`, etc. are all multiline. The prefix controls escaping and interpolation semantics; the quote count controls line behavior. Two orthogonal axes, easy to remember, easy to teach.

This also lets you reconsider whether `@"..."` even needs to exist in F#2. Its original purpose in C# was avoiding `\\` in Windows paths and regex patterns, but if your single-line `"..."` is already restricted (no embedded newlines) and your triple-quoted form handles the "I want to paste content verbatim" case, the verbatim prefix is mostly redundant. Most modern languages have dropped it: Swift doesn't have one, Rust uses `r"..."` and `r#"..."#` but the raw-string family is really doing the job that triple quotes do in C#/F#. You could plausibly say `"..."` is the normal escaped form, `"""..."""` is the raw multiline form, and skip `@` entirely. If you want raw single-line, `r"..."` is the conventional spelling and it composes with the quote-count rule (`r"""..."""` is raw multiline).

Interpolation with `$"..."` is worth keeping as a prefix because it changes the grammar of the string contents (brace handling), which is a real semantic difference unlike `@` which only changes escaping. And `$"""..."""` for interpolated multiline is genuinely useful — C# 11's `$$"""..."""` with doubled braces for literal `{` is a nice touch worth borrowing if you want to support JSON-like content in interpolated strings.

For the transformer, `@"..."` to `"""..."""` conversion is mostly mechanical but has the same line-ending caveat as before: if the verbatim string spans multiple source lines, you're changing the value semantics on CRLF checkouts. Same answer as before — prefer escape conversion for safety, offer triple-quote conversion as an opt-in.

---

F# also allows $$$"" "..." "" and number of $ defines the number of braces to create an interpolation hole

---

Right, and that's actually a nicer design than C#'s, which I'd forgotten about. C# fixed the dollar count at one or two (`$"..."` and `$$"..."`) which covers the common cases but breaks down if you ever need to embed content that contains `{{`. F#'s scheme where N dollars means N braces opens the hole is fully general and composes cleanly with the N-quote rule for embedded quotes.

The mental model becomes beautifully orthogonal: quote count handles "what literal quote sequences can appear in my content," dollar count handles "what literal brace sequences can appear in my content," and the two scale independently. So `$$$"""...{{{expr}}}..."""` lets you write content with `{{` and `}}` freely while still interpolating with triple braces. This is exactly the kind of design where you pick the minimum N that's larger than anything in your content, same as the quote rule. Once a user learns the pattern for one delimiter, they've learned it for the other.

Definitely worth preserving this in F#2. It's one of the cases where F# actually got ahead of C# on a string design question, and there's no reason to regress. The parser implementation is also straightforward — you're just parameterizing the interpolation lexer on a brace count the same way you parameterize the string lexer on a quote count.

A small thing worth specifying explicitly in the language reference: what happens with mismatched brace counts inside the hole. If I write `$$"""...{{ expr }}..."""` and the expression itself contains `}}` as part of, say, a nested record or generic type, the lexer needs a rule. I believe F# requires you to bump the dollar count to disambiguate, same as you'd bump the quote count for embedded quotes. That's the consistent answer and worth stating clearly rather than leaving as "whatever the implementation does."

One related design question while you're here: do you want to allow the dollar prefix on single-quote strings too, so `$"...{x}..."`, `$$"...{{x}}..."`, `$$$"...{{{x}}}..."` all work for single-line interpolation? I'd say yes — it's the same orthogonality argument as before. The quote count axis and the dollar count axis should each work independently of the other, and any combination should be legal. Users will rarely need `$$$"..."` on a single line, but the rule is simpler if there are no arbitrary restrictions.

---

May as well allow it. All the arithmetic is already done in the lexer

---

Exactly right. Once the lexer is parameterized on (quote-count, dollar-count) the cost of allowing all combinations is essentially zero, and disallowing some would actually be more code — you'd need explicit checks and error messages for the forbidden combinations. The principle of "if the implementation falls out naturally, don't artificially restrict it" applies cleanly here.

It also future-proofs the design. You don't know in advance which weird combination some user will need — embedding a code generator's output, writing a templating DSL, quoting another language that happens to use `{{{` as its own delimiter (Mustache, Handlebars). Better to have a single uniform rule than to discover later that you need to relax restrictions and deal with the compatibility implications.

---

## Actionable plan

### Design principles

- **Orthogonal flags, not a dialect knob.** Each semantic change gets its own boolean / discriminated union so dual-mode tooling can mix-and-match during migration.
- **CST preserves source bytes.** All cooked-value transforms (indent strip, CRLF→LF) live in a lazy `StringValues.evaluate` helper. The parser stays config-aware only for decisions that affect *tokenisation* (where the string ends, what's an error).
- **Default = today's F#.** New flags default to legacy behaviour so the existing corpus still parses byte-identically.

### Config surface

New record in `Lexing.fs` (alongside `LexContext`):

```fsharp
type LexerConfig =
    { AllowMultilineRegularStrings : bool          // " ... " spans lines
      AllowMultilineVerbatimStrings : bool         // @" ... " spans lines
      AllowMultilineInterpolatedStrings : bool     // $" ... " / $@" ... " span lines
      AllowNDollarSingleQuoted : bool              // $$"...{{x}}..." etc.
    }
    static member Legacy
    static member FSharp2
```

`StringValues.EvaluateConfig` (separate, only consumed by the value helper):

```fsharp
type EvaluateConfig =
    { TripleQuoteIndent : TripleQuoteIndentMode    // Off | CSharpStyle
      NewlineNormalization : NewlineMode           // Preserve | Lf
    }
```

Threading: add `Config: LexerConfig` to `LexBuilder`; `LexBuilder.init` gains an optional `?config` arg defaulting to `LexerConfig.Legacy`. `lexString` adds the same overload. No other call-site changes — the state already flows through every reader.

### Phase 1 — Config plumbing (one PR, no behaviour change)

1. `Lexing.fs`: add `LexerConfig` + the two static defaults.
2. Add `Config` field to `LexBuilder`; thread through `init`.
3. New overload `lexString (input, config)`; old `lexString input` calls it with `LexerConfig.Legacy`.
4. Smoke: existing test corpus + golden files unchanged. This PR is a no-op behaviourally — only structural plumbing — and lands the seam everything else depends on.

### Phase 2 — Single-line regular / verbatim / interpolated strings

The fragment predicates currently let `\n` through. Make the predicates always stop at `\n` (`\r`), and resolve the newline in the per-context dispatch in `lex` based on config:

| Context | Legacy | `Allow*MultilineX = false` |
|---|---|---|
| `PlainString` | newline consumed as fragment char (today's `pPlainStringFragmentToken` keeps eating) | emit `Token.NewlineInSingleLineString` (new Invalid token), `CtxOp.Pop`, message: *"Newline in `\"…\"` literal; use `\"\"\"…\"\"\"` for multiline strings"* |
| `VerbatimString` | same | same, gated on `AllowMultilineVerbatimStrings` |
| `InterpolatedString` / `VerbatimInterpolatedString` | same | same, gated on `AllowMultilineInterpolatedStrings` |
| `TripleQuotedString` / `Interpolated3String` | unchanged — `\n` is always legal in triple-quoted | unchanged |

Mechanics: pick the predicate via the per-context dispatch (cheap, ~6 sites in `Lexing.fs` lines 3025–3042); add the `'\n'`/`'\r'` arm to each of the four single-line contexts. The "consume newline as fragment continuation" legacy path needs a single small parser (`pConsumeNewlineInString`) so we don't duplicate fragment-emit logic.

New token: `Token.NewlineInSingleLineString` under `KindInvalid`. The diagnostic suggests `"""` — the small thing that makes the restriction feel helpful rather than punitive.

### Phase 3 — `StringValues.evaluate` helper

New module `StringValues` (likely in `Expr.fs` or its own file `StringValues.fs` between `Expr.fs` and the parsers):

```fsharp
module StringValues =
    val evaluate : StringKind<SyntaxToken> -> ImArr<StringPart<SyntaxToken>> -> EvaluateConfig -> string
    val evaluateConstant : Constant<SyntaxToken> -> source: string -> EvaluateConfig -> string voption
```

Responsibilities:

1. **Triple-quote indent strip** (C# 11 semantics):
   - The `String3Close` / `Interpolated3StringClose` token's column establishes the margin.
   - Strip exactly that prefix from each content line; any content line with less indent is a diagnostic, not a silent pass-through. (Diagnostic surfaces via a separate `evaluateWithDiagnostics` overload; the plain `evaluate` either throws or returns `voption`.)
   - Tabs and spaces are not interchangeable — exact prefix match.
   - Strip the bracketing newlines (the LF immediately after opening `"""` and immediately before closing `"""`).
2. **Newline normalisation**: when `Lf`, rewrite source `\r\n` and bare `\r` inside fragments to `\n`. Leave `\n` escape sequences alone (`\n` was always LF).
3. **No-op for single-line forms**: regular / verbatim / single-line interpolated strings just concatenate parts.

Only the *Text* parts and `Expr` hole formatting need this treatment; escape sequences, format specifiers, etc. already cook to their semantic value via existing logic in `ExpressionParsing.fs:2035–2110` — refactor that loop to delegate to `StringValues.evaluate` rather than duplicate.

### Phase 4 — Unify the three interpolated-string opens (N-dollar / N-brace everywhere)

Today `LexContext.InterpolatedString` and `VerbatimInterpolatedString` are level-less; only `Interpolated3String of level: int` carries the dollar count. Brainstorm conclusion: allow all (quote-count, dollar-count) combinations.

1. Change both single-line interp contexts to carry `level: int`: `InterpolatedString of level: int`, `VerbatimInterpolatedString of level: int`.
2. `pInterpolatedStringStartToken` and `pVerbatimInterpolatedStartToken` become parsers that count dollars then expect the opener char(s) — same shape as `pInterpolated3StartToken` (line 1724).
3. `pInterpolatedExpressionStartToken` (line 1472) — currently hardcoded to "one `{` opens, `{{` is escape" — becomes the level-aware variant (matches `pInterpolated3ExpressionStartToken` at line 1501). Same for the matching close.
4. Dispatch in `pDollarToken` (line 2676) and `pAtToken` (line 2687) updated: peek dollar-count, then look at the next non-`$` char to pick single vs triple.

Gate: `AllowNDollarSingleQuoted`. When false, `$$"..."` lexes as today (`$$` operator + string open) — i.e. the higher-level start parsers only accept `$` count = 1 before a single quote, and surface as failure so the operator fallback fires. This keeps F# legacy byte-identical without separate code paths — just a guard at the start parser.

### Phase 5 — `LexerConfig.FSharp2`

Once Phases 2–4 land, define `LexerConfig.FSharp2 = { AllowMultilineRegularStrings = false; AllowMultilineVerbatimStrings = false; AllowMultilineInterpolatedStrings = false; AllowNDollarSingleQuoted = true }`. Add a parallel test fixture set (`test/XParsec.FSharp.Tests/data-fsharp2/`) so both modes are golden-tested side by side.

### Phase 6 — Diagnostic polish

`FSharpErrorFormatting.fs` learns three new messages:
- *"Newline in single-line string literal; use `\"\"\"…\"\"\"` for multiline"*
- *"Content line indented less than closing delimiter"* (triple-quote dedent violation, raised by `StringValues.evaluate` not the parser)
- *"Mixed tabs and spaces in triple-quote indent"* (same source)

The last two surface only when `StringValues.evaluate` is called; they're semantic-phase errors, not parse errors. The CST itself is well-formed.

### Phase 7 (separate effort) — Migration transformer

Out of scope for the configurable-lexer effort itself. Once dual-mode lexing is solid, build a `tools/fs-to-fs2` CLI:
- Reads CST in legacy mode.
- For each string literal, emits the F#2-compatible equivalent per the brainstorm conversion rules (multiline `"..."` → `\n`-escaped single-line for byte-safety; `@"..."` → escape conversion by default, opt-in `--prefer-triple-quotes` for triple-quote conversion).
- Flags (does not auto-rewrite) existing `"""..."""` in F# source whose runtime value would change under C# 11 dedent.
- Emits a sidecar report listing every literal touched and the rationale.

### Files touched (Phases 1–6)

| File | Phase | Change |
|---|---|---|
| `Lexing.fs` | 1 | `LexerConfig` record + `Legacy`/`FSharp2` defaults; `Config` field on `LexBuilder`; `lexString` overload |
| `Lexing.fs` | 2 | Predicate-strict-on-newline; per-context `\n`/`\r` arms in `lex` dispatch |
| `Token.fs` | 2 | New `Token.NewlineInSingleLineString` under `KindInvalid` |
| `Lexing.fs` | 4 | `LexContext.InterpolatedString` / `VerbatimInterpolatedString` gain `level`; unify three start/expression-open/expression-close parsers around `level: int` |
| `Expr.fs` *(or new `StringValues.fs`)* | 3 | `StringValues.evaluate` + `EvaluateConfig`, dedent algorithm, LF normalisation |
| `ExpressionParsing.fs` | 3 | `loop` at ~2035 delegates cooked-value logic to `StringValues` (refactor, not behaviour change) |
| `FSharpErrorFormatting.fs` | 6 | Three new diagnostic messages |
| `test/XParsec.FSharp.Tests/data-fsharp2/` | 5 | Parallel golden-file fixture set |

### Ordering / merge strategy

Phases 1, 4, 3 are independent and can land in any order. Phase 2 depends on Phase 1's config seam. Phase 5 depends on 2+3+4. Phase 6 depends on 2+3. Phase 7 depends on everything.

Suggested merge order: **1 → 3 → 4 → 2 → 5 → 6** (transformer after). Phase 3 first so legacy tests round-trip through the new helper before the lexer changes; Phase 4 next because it touches a different surface and won't conflict; Phase 2 last among the lexer changes since it introduces the new error token and the most carefully-scoped dispatch additions.

### Bench guard

`XParsec.FSharp.Benchmarks` already runs on legacy mode by construction (defaults). After Phase 4 (the only phase that adds work to the hot dispatch) re-run Small/Medium/Large and confirm no regression — the change is `match` on `LexContext.InterpolatedString` becoming `LexContext.InterpolatedString _`, which is a no-op IL-wise, but worth confirming.
