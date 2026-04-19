# 7. Interpolated strings and contextual lexing

## Hook

```fsharp
$"""value = "{x}" done"""
```

That parses. So does `$"%%"` (a literal `%`), `$"%d{x}"` (a format specifier followed by an interpolation hole), and `$"""line1
line2 = {y}"""` (multiline, triple-quoted, with embedded newlines and holes). One grammar, five or six lexical sub-modes. This is where a non-trivial lexer earns its keep.

## What this post covers

- **Why interpolated strings aren't a single token.** A `$"..."` contains *fragments* of literal text, *holes* of F# expression syntax, and *format specifiers* — each with different escape rules. You can't just scan to the closing `"` because `{x}` contains arbitrary expression code, including further strings. The lexer is a state machine.
- **The five contexts.** `String`, `InterpolatedString`, `Interpolated3String`, `VerbatimString`, `Interpolated3QuoteOrFragment`. Each routes `"`, `"""`, `{`, `}`, `%`, and escapes differently. Illustrate with the dispatch table in `Lexing.fs`.
- **The `%%` escape bug.** Case study: `pFormatSpecifierTokens` originally used `many1Chars (pchar '%')` and then `skipN percents.Length`, which advanced past the *closing quote*. Fix: `lookAhead (many1Chars (pchar '%'))` to count without consuming, then explicit `skipN`. Documented in `ms-fsharp-progress.md`. Perfect teaching bug — demonstrates exactly when lookahead-with-skip beats consume-and-count.
- **`"` inside `$"""..."""`.** Commit-level story: a lone `"` after a `}` inside a triple-quoted interpolated string used to fail because the dispatcher routed it to `pInterpolated3EndToken`, which required `"""`. The fix adds `pInterpolated3QuoteOrFragment`, which tries `"""` first and falls back to treating lone `"`s as fragment content. Show both the before and after dispatch tables.
- **Block comments with trailing operator chars.** A relative of interpolation-weirdness: commit `037467c Fix lexing a block comment with trailing operator chars` fixes `(* x *)*` and its friends. Also: `fbfc4cd`'s application of block comments to format placeholders.
- **Unterminated strings.** Commit `bb959c3 Handle unterminated interpolated strings`. Recovery matters *in the lexer*, not just the parser — emit a diagnostic, synthesize a close, keep going. Otherwise a single unterminated string at line 5 kills the lexer's ability to highlight errors on line 200.
- **Plain strings became fragments too.** Commit `17172d4 Parse all string literals as fragments`. A decision that arrived much later: if interpolated strings expand into `StringOpen StringFragment... StringClose`, regular strings should too. Originally `"hello"` was a single `StringLiteral` token and `$"hello %d{x}"` was a multi-token sequence, which meant every downstream consumer had to check both shapes. The fix is structural — one string model, uniformly fragmented — but the *reason* is tooling. A formatter that wants to line up escape sequences, a linter that flags `%d` without a matching hole, or a diagnostic that points inside a string: they all need access to fragments and specifiers without reparsing the source span. One-token strings hide that information; fragmented strings expose it for free.
- **The fragmentation pattern generalizes.** The same principle drives `d3af88f Include parsed separators in AST` and later `feedback_ast_preserve_tokens`. The pattern is: when you have a structured lexical thing (a string, a list expression, a record), don't collapse its internal structure at lex/parse time just because the first consumer doesn't need it. The second consumer usually does.

## Anchor commits / files

- `src/XParsec.FSharp/Lexing.fs` (string sub-modes, `pFormatSpecifierTokens`, `pInterpolated3QuoteOrFragment`)
- `b15fad4 Handle parsing interpolated strings`
- `bb959c3`, `a5464d3 Fix interpolated string application parsing`
- `91f1db2 Fix lexing some interpolated strings`
- `17172d4 Parse all string literals as fragments`
- `ms-fsharp-progress.md` (bugs 1 and 2)

## Takeaway

Contextual lexing isn't optional for any modern language. A dispatch table keyed on the current lex state keeps the code readable; a small number of carefully chosen fallbacks (like `pInterpolated3QuoteOrFragment`) keep it correct. And when the same structure turns out to work for a simpler sibling construct — a plain string that used to be one token — generalize. Uniformity is worth more than the bytes saved by early collapse.
