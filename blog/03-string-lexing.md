# 3. Lexing the awkward bits: interpolated strings

## Hook

Five days into the project the lexer didn't yet recognise a `for` loop, but it could lex this:

```fsharp
$$$"""This string has {a} set of curlies, and {{a}} set of curlies, and an interpolated value: {{{a}}}"""
```

Triple-dollar interpolation, triple-quoted multiline body, single/double/triple curlies — only the third pair is a hole. That fixture (`test/data/04_triple_dollar_with_curlies.fs`) was one of nineteen interpolated-string variants checked in on **2025-10-02**, before the AST parser, before offside, before any operator parsing. The pure lexer *was* the project at that point; interpolated strings were what made the pure lexer interesting.

## What this post covers

- **Why this story is post 3, not post 7.** Most of the famous bugs in interpolated-string lexing surfaced six months later, in the corpus loop (post 10) and the agentic-loop sessions (post 12). The *state machine* underneath was designed and tested up front. This post is about the design; the bugs belong with the loop that found them.

- **The spec didn't help.** The published F# spec predates interpolated strings by years and doesn't describe them at all. The triple-dollar and improved triple-quoted variants don't exist in any reference grammar. My working sources were two:

  - [*New syntax for string interpolation in F#*](https://devblogs.microsoft.com/dotnet/new-syntax-for-string-interpolation-in-fsharp/) — the original devblog announcement, which gave the high-level shape.
  - [*FS-1132: Better interpolated triple-quoted strings*](https://github.com/fsharp/fslang-design/blob/main/FSharp-8.0/FS-1132-better-interpolated-triple-quoted-strings.md) — the F# 8 design document for `$$"..."`, `$$$"..."`, and the variable-arity brace rule. This is the only place the dollar/brace counting rule is written down.

  Everything else came from the F# REPL: type a candidate string, see whether it compiles, then write the matching fixture. A pleasant change of pace from the rest of the project, where the spec is the source of truth and the implementation is the cross-check; here the implementation *was* the truth and I was reverse-engineering it.

- **A lexer with no state is a tokenizer.** Pure F# tokens (`let`, `42`, `+`) yield to a memoryless function: read the next characters, emit a token, repeat. Interpolated strings break that. The same `{` character can be a curly inside a string fragment, the opener of a hole that begins arbitrary F# expression syntax, half of an escape (`{{`), or part of a triple-arity hole (`{{{`). The lexer cannot decide which without remembering *where it is*. That memory is what makes the code non-trivial.

- **The `LexContext` enum.** The state machine's load-bearing cases:
  - `InterpolatedString` — `$"..."`, single-line, single-curly holes
  - `VerbatimInterpolatedString` — `$@"..."`, `""` escapes for literal quotes
  - `Interpolated3String of level: int` — `$"""..."""`, `$$"""..."""`, `$$$"""..."""`; multiline, `level`-many curlies open a hole
  - `InterpolatedExpression` — inside a hole, parse arbitrary F# until the balanced close

  Plus a stack: a hole can contain a string can contain a hole, ad infinitum. The stack is what lets `$"""value = "{x}" done"""` parse — the inner expression `x` lives in `InterpolatedExpression`, then the lexer pops back to `Interpolated3String`.

- **Dollar-count chooses the brace arity.** F#'s rule, formalised in FS-1132: `n` leading `$` means `n` curlies open a hole; fewer-than-`n` curlies are literal. So `$$"text {{x}} more"` has `{{` and `}}` as the hole markers and `{x}` as literal text. This isn't decoration — it's how you embed JSON, Razor, or any template language without escape-tax on every brace. The `level` field on `Interpolated3String` is what makes it work; the opener counts dollars and remembers.

- **Triple-quoted means multiline and quote-tolerant.** Inside `$"""..."""` a lone `"` is just text. Only `"""` closes (and only when not followed by another `"`). The dispatcher can't route every `"` to "close"; it needs a rule that tries `"""` first and falls back to fragment. The fixture that demanded that rule — `test/data/18_interpolated_verbatim_with_quotes.fs` — was checked in October 2025. The actual implementation that satisfied it landed much later, which is why post 10 has the bug story; this post just notes that the requirement was visible from day one.

- **Format specifiers are a parallel grammar.** `$"%d{x}"` interleaves a printf-style format specifier (`%d`, `%5.2f`, `%A`) with an interpolation hole. Both belong to the string but neither is a fragment of literal text. The lexer emits them as distinct tokens — `FormatSpecifier`, `InterpolatedHoleOpen`/`Close` — so downstream tooling can render or validate either piece without rescanning. `%%` is the literal `%` escape, also its own token.

- **Nineteen fixtures as a spec-coverage strategy.** `6fd50c3 Fully lex some basic files` introduced files numbered 00–19, each a different cell in the matrix: (single/triple/verbatim) × (with/without curlies) × (with/without format specifier) × (single/multi-line). Reading the directory listing tells you what the lexer was being designed against. The project's first corpus was small, deliberate, and complete — written before the implementation was, and exactly the kind of harness post 10 generalises to 249 files.

- **What this layer hands downstream.** Each interpolated string emerges as `StringOpen`, alternating `StringFragment` / `InterpolatedHoleOpen`...`InterpolatedHoleClose` / `FormatSpecifier`, then `StringClose`. The lexical filter (post 4) preserves the shape; the parser (post 5+) consumes the holes' contents as ordinary expression tokens. Plain non-interpolated strings were rewritten into the same shape much later — that's `17172d4 Parse all string literals as fragments`, and it lives in post 10 as a corpus finding, not here.

## Anchor commits / files

- `8e509da WIP: Creating an F# compatible lexer` (2025-09-28) — first `LexContext.Interpolated{,3,Verbatim}String`
- `6fd50c3 Fully lex some basic files` (2025-10-02) — the nineteen fixtures
- `src/XParsec.FSharp/Lexing.fs` — dispatch table, `pFormatSpecifierTokens`, `pInterpolated3QuoteOrFragment`
- `src/XParsec.FSharp/Token.fs` — `StringOpen`, `StringFragment`, `InterpolatedHole*`, `FormatSpecifier`, `StringClose`
- `test/XParsec.FSharp.Tests/data/00_simple_string.fs` … `19_interpolated_string_with_escapes.fs`

## External references

- [New syntax for string interpolation in F#](https://devblogs.microsoft.com/dotnet/new-syntax-for-string-interpolation-in-fsharp/) — devblog announcement
- [FS-1132: Better interpolated triple-quoted strings](https://github.com/fsharp/fslang-design/blob/main/FSharp-8.0/FS-1132-better-interpolated-triple-quoted-strings.md) — F# 8 design document; the canonical source for the dollar/brace counting rule

## Takeaway

If you're writing a lexer for a modern language, *start* with the contextual cases. They decide whether your lexer is a function or a state machine, and that decision propagates through every layer above — the token type, the filter, the recovery story. Designing the state machine against a small named-fixture corpus before any parser exists is cheap, exhaustive, and forces you to confront the cases the spec mumbles about — or, in this instance, has nothing to say about at all — in isolation. Most of the bugs that *will* surface later, including the ones in posts 10 and 12, are bugs in this code being exercised harder, not in code you forgot to write.
