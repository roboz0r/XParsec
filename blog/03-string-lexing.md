# 3. Lexing the awkward bits: interpolated strings

Five days into the project the lexer didn't yet recognise a `for` loop, but it could lex this:

```fsharp
$$$"""This string has {a} set of curlies, and {{a}} set of curlies, and an interpolated value: {{{a}}}"""
```

Triple-dollar interpolation, triple-quoted multiline body, single/double/triple curlies — only the third pair is a hole. That fixture (`test/XParsec.FSharp.Tests/data/lex-only/04_triple_dollar_with_curlies.fs`) was one of nineteen interpolated-string variants checked in on **2025-10-02**, before the AST parser, before offside, before any operator parsing. The pure lexer *was* the project at that point; interpolated strings were what made the pure lexer interesting.

Post 2 introduced the eight-byte `PositionedToken`, the bit-packed `Token` enum, and the dispatch loop. It also gestured at strings as fragments and promised the contextual-lexing story would arrive here. This post is that story: why the lexer is a state machine rather than a memoryless function, what the state actually contains, and how the FS-1132 dollar/brace rule encodes itself directly into the type of one of the contexts on the stack.

The famous bugs surfaced six months later under the corpus harness (post 10) and the agentic-loop sessions (posts 11 and 13). The state machine underneath was designed and tested up front. This post is about the design, which survived contact with reality; the bugs belong with the loop that found them.

## The spec didn't help

The published [F# language specification](https://fsharp.org/specs/language-spec/) predates interpolated strings by years. It doesn't describe `$"..."` at all, and the triple-dollar and improved triple-quoted variants don't exist in any reference grammar. My working sources were two:

- [*New syntax for string interpolation in F#*](https://devblogs.microsoft.com/dotnet/new-syntax-for-string-interpolation-in-fsharp/) — the original devblog announcement, which gave the high-level shape: `$"text {expr}"`, verbatim variants, format specifiers.
- [*FS-1132: Better interpolated triple-quoted strings*](https://github.com/fsharp/fslang-design/blob/main/FSharp-8.0/FS-1132-better-interpolated-triple-quoted-strings.md) — the F# 8 design document for `$$"..."`, `$$$"..."`, and the variable-arity brace rule. This is the only place the dollar/brace counting rule is written down.

Everything else came from the F# REPL. Type a candidate string, see whether it compiles, then write the matching fixture. A pleasant change of pace from the rest of the project, where the spec is the source of truth and the implementation is the cross-check; here the implementation *was* the truth and I was reverse-engineering it.

## A lexer with no state is a tokenizer

Pure F# tokens (`let`, `42`, `+`) yield to a memoryless function: read the next characters, emit a token, repeat. Interpolated strings break that. The same `{` character can be:

- a curly inside a string fragment that should pass through as text,
- the opener of a hole that begins arbitrary F# expression syntax,
- half of an escape (`{{`) that produces one literal `{` in the output,
- one of N curlies in a triple-arity hole opener (`{{{`) inside a `$$$"""..."""` string.

The lexer cannot decide which without remembering *where it is*. That memory is what makes the code non-trivial. In post 2 the dispatch loop was a flat `match` on `(character, context)`. The character side is easy: 256 ASCII characters minus the boring ones. The context side is what this post is about.

## The `LexContext` cases

The state lives on a stack inside `LexBuilder`. The cases relevant to strings (from `Lexing.fs:160-178`):

```fsharp
[<RequireQualifiedAccess>]
type LexContext =
    | Normal
    | InterpolatedString                    // $"..."
    | VerbatimInterpolatedString            // $@"..." or @$"..."
    | Interpolated3String of level: int     // $"""...""", $$"""...""", $$$"""..."""
    | InterpolatedExpression                // inside { ... }
    // ... non-string contexts elided
    | PlainString                           // "..."
    | VerbatimString                        // @"..."
    | TripleQuotedString                    // """..."""
```

A context is *pushed* when the lexer enters an environment with different rules — `$"` pushes `InterpolatedString`, `{` inside that string pushes `InterpolatedExpression` — and *popped* when the corresponding closer is seen. The stack is what lets nesting work. Consider:

```fsharp
$"""value = "{x}" done"""
```

The opening `$"""` pushes `Interpolated3String 1`. Inside, the lone `"` is text — the `Interpolated3String` rules treat single quotes as fragment characters. The `{x}` pushes `InterpolatedExpression`, lexes the identifier `x`, pops the expression on `}`, and the lexer is back in `Interpolated3String`. The lone `"` after `}` is text again, then `"""` closes the whole thing.

A hole can contain a string can contain a hole, ad infinitum:

```fsharp
$"outer {if b then $"inner {x}" else "none"} done"
```

The outermost `$"..."` pushes `InterpolatedString`. The `{` after `outer ` pushes `InterpolatedExpression`. Inside that expression, the `$"inner {x}"` pushes a fresh `InterpolatedString`, lexes through to its own `"`, and pops. The `"none"` after `else` is a plain string in expression context — no special arm for `"` inside `InterpolatedExpression`, so the fallback `'"', _ -> pDoubleQuoteToken` fires and pushes `PlainString`. Each layer is a stack frame. The seven cases above plus the discipline of push/pop are the complete state model for string lexing.

`InterpolatedExpression` deserves a brief note. Inside it, the lexer is back in expression context — the same dispatch arms that lex `let`, `42`, and `+` outside the string apply unchanged. The only special arm is `}`, which pops the frame and returns to whichever string context was below. The `:` arm routes to `pInterpolatedFormatClause`, which consumes a `.NET` format specifier like `{x:F2}` up to the closing `}` and emits it as `InterpolatedFormatClause`.

## A look at the dispatch table

The dispatch loop from post 2 grew most of its string handling into the `(character, context)` table. The string-relevant arms (from `Lexing.fs:2548-2605`):

```fsharp
| '{', LexContext.InterpolatedString          -> pInterpolatedExpressionStartToken
| '{', LexContext.VerbatimInterpolatedString  -> pInterpolatedExpressionStartToken
| '{', LexContext.Interpolated3String level   -> pInterpolated3ExpressionStartToken
| '}', LexContext.InterpolatedString          -> pInterpolatedStringFragmentRBraces
| '}', LexContext.VerbatimInterpolatedString  -> pInterpolatedStringFragmentRBraces
| '}', LexContext.Interpolated3String level   -> pInterpolated3StringFragmentRBraces
| '}', LexContext.InterpolatedExpression      -> pInterpolatedExpressionEndToken
| '"', LexContext.InterpolatedString          -> pInterpolatedStringEndToken
| '"', LexContext.VerbatimInterpolatedString  -> pVerbatimInterpolatedStringQuoteToken
| '"', LexContext.Interpolated3String _       -> pInterpolated3QuoteOrFragment
| '"', LexContext.PlainString                 -> pPlainStringCloseToken
| '"', LexContext.VerbatimString              -> pVerbatimStringQuoteToken2
| '"', LexContext.TripleQuotedString          -> pTripleStringQuoteOrFragment
| '\\', LexContext.PlainString                -> pStringEscapeToken
| '"', _                                      -> pDoubleQuoteToken
| '$', ExpressionCtx                          -> pDollarToken
| '@', ExpressionCtx                          -> pAtToken
| '%', (LexContext.InterpolatedString
      | LexContext.VerbatimInterpolatedString
      | LexContext.Interpolated3String _)     -> pFormatSpecifierTokens
| ':', LexContext.InterpolatedExpression      -> pInterpolatedFormatClause
```

Three observations. First, the same character routes to different sub-parsers based on context — the `{` arms alone span four contexts. Second, the *fallback* arms at the bottom (`'"', _ -> pDoubleQuoteToken` and the like) handle the case where the lexer is in `Normal` or some non-string context and sees the character that would *open* a string. Third, the `'$'` and `'@'` arms only fire in expression context: `pDollarToken` then sub-dispatches based on what follows (`$"`, `$@"`, `$"""`, `$$"""`, `$$$"""`, …) and pushes the appropriate string context. The interpolated-string state machine is entirely on this side of the dispatch table, but the entry points are just two characters.

## Dollar-count chooses the brace arity

FS-1132 introduced `$$"..."` and `$$$"..."` to make embedding template languages bearable. The rule, paraphrased: in a triple-quoted interpolated string with `N` leading `$` signs, exactly `N` consecutive `{` characters open a hole, and `N` consecutive `}` close it. Fewer than `N` curlies are literal text. So:

```fsharp
let html = $$"""<div class="{{className}}">{content}</div>"""
```

`{{className}}` is a hole (count == level == 2). `{content}` is literal text (count == 1 < level == 2), passed through to the output untouched. The cost of writing JSON, Razor, or any template language inside an F# string drops from "escape every brace" to "use one more dollar than the language under your cursor uses braces."

Encoding that rule directly into a context case turns it from a runtime check into a type. From `Lexing.fs:1631-1643`:

```fsharp
let pInterpolated3StartToken =
    parser {
        let! pos = getPosition
        let! (dollars, _) = many1Chars (pchar '$') .>>. pstring "\"\"\""

        do!
            updateUserState (
                LexBuilder.append
                    Token.Interpolated3StringOpen
                    pos
                    (CtxOp.Push(LexContext.Interpolated3String dollars.Length))
            )
    }
```

The opener counts dollars, asserts a `"""` follows, and pushes `Interpolated3String dollars.Length`. The `level: int` is now part of the context's identity; every subsequent dispatch can read it.

The `{` arm inside `Interpolated3String` does the work. From `Lexing.fs:1408-1443`:

```fsharp
let pInterpolated3ExpressionStartToken =
    parser {
        let! pos = getPosition
        let! braces = many1Chars (pchar '{')

        do!
            updateUserState (fun state ->
                let level = LexBuilder.level state
                let count = braces.Length
                let idx = pos.Index
                let diff = count - level

                if diff < 0 then
                    // Fewer { than level — the whole run is literal.
                    LexBuilder.appendI Token.Interpolated3StringFragment idx CtxOp.NoOp state
                elif diff = 0 then
                    // Exactly level — open an expression hole.
                    LexBuilder.appendI Token.InterpolatedExpressionOpen idx
                        (CtxOp.Push LexContext.InterpolatedExpression) state
                elif diff >= level then
                    // Too many — flag as invalid.
                    LexBuilder.appendI Token.TooManyLBracesInInterpolated3String idx CtxOp.NoOp state
                else
                    // Some leading literals, then an open at the boundary.
                    state
                    |> LexBuilder.appendI Token.Interpolated3StringFragment idx CtxOp.NoOp
                    |> LexBuilder.appendI Token.InterpolatedExpressionOpen (idx + diff)
                        (CtxOp.Push LexContext.InterpolatedExpression)
            )
    }
```

A ternary on `diff = count - level`. Walk it through `$$$"""...{{{a}}}"""` (level = 3):

- The lexer sees `{{{` (count = 3). `diff = 0`, so it emits `InterpolatedExpressionOpen` and pushes `InterpolatedExpression`. The `a` lexes as an identifier in expression context. Then `}}}` (handled by the symmetric `}` arm) emits `InterpolatedExpressionClose` and pops back to `Interpolated3String`.

Now walk `{a}` and `{{a}}` from the same fixture:

- `{` (count = 1). `diff = -2 < 0`, so it emits a single `Interpolated3StringFragment` token covering the `{`. The `a` lexes as a fragment character. Then `}` (count = 1) emits another fragment.
- `{{` (count = 2). `diff = -1 < 0`, fragment. The same for `}}`.

The "too many" branch (`diff >= level`) catches inputs like `{{{{{{` in a level-3 string — six leading `{`s with the assumption that the user meant three literals followed by three openers, but ambiguity makes it safer to reject. The maximum valid run is `2 * level - 1`: enough leading literals to need splitting, but never enough to exceed one full extra hole opener. `TooManyLBracesInInterpolated3String` and the symmetric `TooManyRBracesInInterpolated3String` are the diagnostic carriers; downstream tooling can report them with full source position.

The symmetric `}` arm runs the same ternary in reverse.

### Single-dollar uses parity, not arity

Single-dollar `$"..."` predates FS-1132 and has a simpler rule: `{{` is the literal-`{` escape, and a lone `{` opens a hole. The implementation (`Lexing.fs:1379-1406`) walks the run two characters at a time:

```fsharp
let pInterpolatedExpressionStartToken =
    parser {
        let! pos = getPosition
        let! braces = many1Chars (pchar '{')

        do!
            updateUserState (fun state ->
                let mutable count = braces.Length
                let mutable idx = int pos.Index

                while count > 1 do
                    // {{ is an escape sequence for '{'
                    LexBuilder.appendI Token.EscapeLBrace idx CtxOp.NoOp state |> ignore
                    idx <- idx + 2
                    count <- count - 2

                match count with
                | 0 -> state
                | _ ->
                    LexBuilder.appendI Token.InterpolatedExpressionOpen idx
                        (CtxOp.Push LexContext.InterpolatedExpression) state
            )
    }
```

`{{` is one `EscapeLBrace`. `{{{` is one `EscapeLBrace` followed by an `InterpolatedExpressionOpen`. `{{{{` is two `EscapeLBrace`s. The parity of the run decides how many escapes and whether a hole follows. This is the rule from the original devblog post, and it predates the more general arity rule that `Interpolated3String` uses. Both rules coexist in the same lexer; the dispatcher routes to the right one based on whether the current context is `InterpolatedString` (parity) or `Interpolated3String` (arity).

The reason FS-1132 needed a different rule is exactly the JSON-embedding case. With parity, embedding a `{ "key": "value" }` inside a `$"..."` string forces every brace to be doubled. With arity, you bump the dollar count once and write JSON literally. The parity rule is fine when the host text rarely contains braces; it falls over the moment the host text *is* a brace-heavy template.

## Triple-quoted means multiline and quote-tolerant

Inside `$"""..."""` a lone `"` is just text. Only `"""` closes — and only when not followed by another `"`. The dispatcher can't route every `"` to "close"; it needs a rule that tries `"""` first and falls back to fragment. From `Lexing.fs:1660-1663`:

```fsharp
let pInterpolated3QuoteOrFragment =
    pInterpolated3EndToken
    <|> pToken (many1Chars (pchar '"')) Token.Interpolated3StringFragment
```

A two-arm `choice`: try the close, fall back to a run-of-quotes fragment. `pInterpolated3EndToken` parses exactly `"""` and emits `Interpolated3StringClose` with a `Pop` of the `Interpolated3String` context. If the input is `""` or a single `"`, the close fails, the fragment alternative consumes whatever quote run is there, and the lexer continues.

That two-line parser is enough to make `$"""He said "hi" then \nleft"""` lex correctly without any escape gymnastics. The fixture that stress-tested this — `18_interpolated_verbatim_with_quotes.fs` — was checked in early. The actual implementation that satisfied it landed much later, which is why post 10 has the bug story; this post just notes that the requirement was visible from day one.

## Verbatim is a different escape rule

`$@"..."` and `@$"..."` push `VerbatimInterpolatedString`. Inside, `""` is an escaped quote (the token is `VerbatimEscapeQuote`), not a string close. Backslash escapes are *off* — `\n` is two characters, `\` followed by `n`, neither special. The hole rule (`{` opens, `{{` is the literal-`{` escape) is the same as plain `InterpolatedString`; only the closing-quote rule and the backslash rule change.

```fsharp
let s = $@"He said, ""The operation was a {message}."""
```

`""` lexes as `VerbatimEscapeQuote`, not a close-then-reopen. The next `"` (the third in the run) actually closes the string. One context, one swapped rule: just enough state to flip the behaviour.

The fragment parser inside `VerbatimInterpolatedString` is a one-liner — `many1Chars (satisfy (fun c -> c <> '"' && c <> '{' && c <> '}'))` — because everything except the three special characters is fragment content, no backslash interpretation, no escape processing. Compare to the non-verbatim fragment parser in `Lexing.fs:1361-1372`, which has to handle `\` as a one-character lookahead escape: `\n` consumes both characters, `\\` consumes both, `\"` consumes both. The two contexts differ in two rules — close-on-`""` versus close-on-`"`, and ignore-`\` versus consume-`\`-and-next — and the dispatcher's job is just to route to the right fragment parser.

Why is verbatim worth having at all when triple-quoted strings handle multiline and embedded quotes more cleanly? Verbatim predates triple-quoted. It survives because regex patterns and Windows file paths look better with `@"C:\path\to\file"` than with either `"C:\\path\\to\\file"` or `"""C:\path\to\file"""`. The lexer doesn't get to opine; it just supports what the language has.

## Format specifiers are a parallel grammar

`$"%d{x}"` interleaves a printf-style format specifier with an interpolation hole. Both belong to the string but neither is a fragment of literal text. The lexer emits them as distinct tokens — `FormatPlaceholder`, `InterpolatedExpressionOpen`/`Close` — so downstream tooling can render or validate either piece without rescanning. `%%` is the literal `%` escape, also its own token (`EscapePercent`).

The shape of `FormatPlaceholder`'s payload (from `Lexing.fs:152-158`):

```fsharp
type FormatPlaceholder =
    {
        Flags: string
        Width: bigint voption
        Precision: bigint voption
        Type: FormatType
    }
```

`FormatType` is a discriminated union over the printf type characters (`d`, `i`, `f`, `A`, `O`, etc.). The width and precision are `bigint voption` because F#'s printf accepts arbitrary integer widths in theory; in practice anything past 32 bits is silly, but the parsed AST shouldn't bake in a limit the language doesn't enforce.

What's interesting is that `pFormatSpecifierTokens` (from `Lexing.fs:2217-2289`) reuses the same arity rule the brace handler uses. At level 1 (a single `$`), `%%` is an `EscapePercent` and a final lone `%` opens a `FormatPlaceholder`. At level N ≥ 2, `(count - level)` leading `%` chars are emitted as fragment tokens; the remainder forms a `FormatPlaceholder`. If `leading >= level`, the run is rejected as `InvalidFormatPercents`.

The `%` rule is structurally identical to the `{` rule. Same dollar-count drives it; same ternary on `diff`; same error branch on overflow. Two arity-counting rules in the same lexer is not duplication — it's the same FS-1132 design idea applied twice. Once you accept that `level: int` is part of the context, both rules write themselves.

## What this layer hands downstream

Every interpolated string emerges from the lexer with the same shape:

- `InterpolatedStringOpen` / `Interpolated3StringOpen` / `VerbatimInterpolatedStringOpen` — the boundary, carrying the syntax flavour.
- `InterpolatedStringFragment` / `Interpolated3StringFragment` / `VerbatimInterpolatedStringFragment` — runs of plain characters between escapes and holes.
- `EscapeSequence`, `EscapeLBrace`, `EscapeRBrace`, `EscapePercent`, `VerbatimEscapeQuote` — distinct tokens for the backslash and brace and quote escapes, so tooling can highlight or rewrite them individually.
- `InterpolatedExpressionOpen` ... arbitrary expression tokens ... `InterpolatedExpressionClose` — the hole, with its contents lexed by the same dispatch loop the rest of the file uses.
- `InterpolatedFormatClause` — the `:F2` in `{x:F2}`, captured as a single token.
- `FormatPlaceholder` — the `%d`, `%5.2f`, `%A` runs.
- `InterpolatedStringClose` / `Interpolated3StringClose` / `VerbatimInterpolatedStringClose` — the matching closer.

A simple `$"x = {x}"` lexes as:

```
InterpolatedStringOpen     ($")
InterpolatedStringFragment (x = )
InterpolatedExpressionOpen ({)
Identifier                 (x)
InterpolatedExpressionClose(})
InterpolatedStringClose    (")
```

Plain non-interpolated strings emit the same shape with different token names: `StringOpen`, `StringFragment`, `EscapeSequence`, `StringClose`. That uniformity wasn't designed in from day one. The original implementation had a single `StringLiteral` token containing the entire string body, and only interpolated strings used the fragment-based shape. The unification landed much later, in `17172d4 Parse all string literals as fragments`. The motivation was concrete: a formatter walking the AST can reconstruct the source character-for-character only if escapes and fragments are first-class tokens; a syntax highlighter wants to colour `\n` differently from the surrounding text; a linter that warns about unused `printf` specifiers needs to find them without reparsing string contents. Once interpolated strings had the shape, plain strings followed.

None of these tokens are classified as trivia. The lexical filter (post 4) skips whitespace, comments, and indentation when handing tokens to the parser, but its `isTriviaToken` predicate (`ParsingTypes.fs:382-398`) lists comment and whitespace kinds only. Every string token reaches the parser. That's deliberate: the parser needs to see them all to construct an AST node that preserves source fidelity, and downstream consumers — formatters especially — need every byte accounted for.

The lexer also emits explicit broken-input tokens when something goes wrong: `UnterminatedInterpolatedString` for an opener with no matching close before EOF, `UnmatchedInterpolatedRBrace` for a single `}` outside a hole, plus the `TooManyLBracesInInterpolated3String` and `TooManyRBracesInInterpolated3String` arms covered above. The handling of unterminated interpolated strings was particularly fiddly because the string might leave the context stack with stale entries: a `$"` pushes `InterpolatedString`, and if the closing `"` is missing, the stack has to be unwound at EOF. `LexBuilder.complete` walks the remaining contexts and emits an `Unterminated*` token for each unclosed opener. That's `bb959c3 Handle unterminated interpolated strings`, which simplified a recovery story that would otherwise have had to live in the parser.

## Nineteen fixtures as a spec-coverage strategy

`6fd50c3 Fully lex some basic files` introduced files numbered 00 through 19, each a different cell of the matrix:

```
(plain | verbatim | triple) × (with | without curlies) × (with | without format specifier) × (single | multiline)
```

Reading the directory listing tells you what the lexer was being designed against:

```
00_simple_string.fs
01_simple_double_dollar.fs
02_double_dollar_with_curlies.fs
03_simple_triple_dollar.fs
04_triple_dollar_with_curlies.fs
05_double_dollar_with_format_specifier.fs
06_triple_dollar_with_format_specifier.fs
07_double_dollar_escaping.fs
08_triple_dollar_escaping.fs
09_multiline_double_dollar.fs
10_multiline_triple_dollar.fs
11_simple_interpolated_string.fs
12_interpolated_string_with_expression.fs
13_interpolated_string_with_format_specifier.fs
14_interpolated_string_with_escaped_curlies.fs
15_interpolated_verbatim_string.fs
16_interpolated_verbatim_string_alt_syntax.fs
17_multiline_interpolated_verbatim_string.fs
18_interpolated_verbatim_with_quotes.fs
19_interpolated_string_with_escapes.fs
```

The methodology is the part to keep. Name the cell. Write the smallest fixture that demonstrates it. Add a `.lexed` snapshot. Run the test. If the snapshot looks right, commit; if not, fix the lexer and try again. The fixture file is the spec for that cell, and the snapshot is the lexer's claim about what it does. Both are checked in.

This is the same pattern that scales up to the corpus harness in post 10, which runs the lexer-and-parser against all 249 files of the F# compiler source. The early fixtures and the corpus harness are continuous: the corpus is what happens when the same idea — write a fixture, snapshot the result, regenerate when the lexer changes — runs against a real codebase instead of a hand-curated 20-file matrix.

The project's first corpus was small, deliberate, and complete: written before the implementation was, and exactly the kind of harness post 10 generalises. Starting with the contextual cases — interpolated strings, verbatim quote escapes, format specifiers in triple-dollar contexts — meant the state machine was forced into the design from day one, not retrofitted.

## Anchor commits / files

- `8e509da WIP: Creating an F# compatible lexer` (2025-09-28) — first appearance of `LexContext.InterpolatedString`, `Interpolated3String`, `VerbatimInterpolatedString`.
- `6fd50c3 Fully lex some basic files` (2025-10-02) — the nineteen-fixture corpus.
- `bb959c3 Handle unterminated interpolated strings` — `LexBuilder.complete` and the `Unterminated*` tokens.
- `91f1db2 Fix lexing some interpolated strings` — early-corpus shakedown.
- `17172d4 Parse all string literals as fragments` — plain strings adopt the fragment shape interpolated strings already had.
- `fbf38d4 Fix applying block comments to FormatPlaceholder and related tokens` — `InComment` flag propagation through `pFormatSpecifierTokens`.
- `src/XParsec.FSharp/Lexing.fs:160-178` — `LexContext` cases.
- `src/XParsec.FSharp/Lexing.fs:1408-1443` — `pInterpolated3ExpressionStartToken`, the dollar/brace arity rule.
- `src/XParsec.FSharp/Lexing.fs:1631-1643` — `pInterpolated3StartToken`, where `level` enters the context.
- `src/XParsec.FSharp/Lexing.fs:1660-1663` — `pInterpolated3QuoteOrFragment`, the close-or-fragment two-line parser.
- `src/XParsec.FSharp/Lexing.fs:2217-2289` — `pFormatSpecifierTokens`, the same arity rule applied to `%`.
- `src/XParsec.FSharp/Lexing.fs:2548-2605` — the dispatch arms for `'$'`, `'"'`, `'{'`, `'}'`, `'%'` across the string contexts.
- `src/XParsec.FSharp/Token.fs:1686-1716` — the string-related token enum.
- `src/XParsec.FSharp/Token.fs:1727-1735` — the broken-input tokens.

## External references

- [New syntax for string interpolation in F#](https://devblogs.microsoft.com/dotnet/new-syntax-for-string-interpolation-in-fsharp/) — devblog announcement.
- [FS-1132: Better interpolated triple-quoted strings](https://github.com/fsharp/fslang-design/blob/main/FSharp-8.0/FS-1132-better-interpolated-triple-quoted-strings.md) — the canonical source for the dollar/brace counting rule.

## Takeaway

Five ideas hold this layer up.

**Stateful lexers are tokenizers with memory.** The same character means different things in different contexts. A stack of contexts is the cheapest way to remember which one you're in, and a flat `match` on `(character, context)` is the cheapest way to dispatch.

**Encode the spec rule in the type.** `Interpolated3String of int` makes FS-1132's brace arity a parameter of the context, not a flag stored elsewhere. The opener counts dollars once and the level travels with the frame; every dispatch that needs it reads it directly off the stack.

**One arity rule, two grammars.** Braces and `%` runs share the same `level`-driven ternary. The format-specifier handler is structurally identical to the brace handler — fewer-than-`level` is fragment, exactly-`level` opens, more-than-`level` errors. Reusing the design once paid for itself.

**Unify the output shape across string flavours.** Every string emits `Open` → fragments / escapes / holes / format placeholders → `Close`. Plain, verbatim, triple, interpolated — all the same shape, different token names. Downstream tools see one structure to walk regardless of which surface syntax the user wrote.

**Fixtures before parsers.** The nineteen-file corpus existed before the production lexer did. Naming each cell of the matrix and writing the smallest fixture for it forced the contextual cases into the design from the start, surfaced what the spec doesn't say, and gave the implementation a hard target before there was anything to test.

Most of the bugs that *will* surface later, including the ones in posts 10 and 12, are bugs in this code being exercised harder, not in code I forgot to write. Post 4 picks up the token stream produced here and shows what the lexical filter does with it before the parser ever sees a token.
