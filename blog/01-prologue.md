# 1. Prologue: Why build an F# parser with XParsec?

Every parser combinator library starts with a JSON example. XParsec did too.

XParsec is my library, which means the question "is this real or just an elegant toy?" is one I had to answer for myself. Elegant I already had — [a one-page recursive-descent parser](https://github.com/roboz0r/XParsec/blob/main/src/XParsec.Json/JsonParser.fs) for null, booleans, numbers, strings, arrays, and objects, small enough to fit on a slide. Real meant something else: a language that fights back on every axis a language can fight back on, parsed by the same library, with no escape hatches.

So the bar was F#. Not "some F#" — the actual source of the F# compiler, 249 production files, clean, no diagnostics, no crashes. F# is written in F#; if a parser can hold up to the language's own self-hosting source, it can hold up to anything I would ever throw at it. Layout-sensitive grammar. Contextual lexing. Fused operators that split based on syntactic context. Preprocessor directives. Error recovery that keeps the rest of the file parseable after a broken declaration. A published specification the language has already outgrown.

Over two hundred and fifty commits on the `fsharp` branch, that bar got cleared. This series is what it takes.

Spoiler: The short answer is yes, parser combinators can parse real languages. If I had failed this series likely wouldn't have been written. The longer answer is that the combinators you start with are not the combinators you finish with. Halfway through the project I had to rewrite the operator parser in XParsec itself — a rewrite that shipped as XParsec 0.3, because F#'s operators are not the kind of think you find in a high school algebra problem. Later I had to rewrite it a second time, same feature set but stack-bounded, because the first rewrite blew through a megabyte of stack on deeply operator-chained expressions. The F# work pushed back into the library at every layer, and that pushback — more than the parse result itself — is what makes the answer interesting.

If you came here for the parser you'll get it: the lexer, the lexical filter, the offside rule, Pratt precedence, interpolated strings, error recovery, AST design, a stack probe, a set of grammar surprises, and the LLM-assisted harness that chased down the long tail of bugs in reaching my goal. If you came here for the library story, it runs underneath all of it — every post is also a note about which combinator primitive or naive assumption bent under the weight of a real language, and which ones held.

## What this post covers

- **XParsec in one paragraph.** A parser combinator library whose core `Parser` type is just `Reader -> ParseResult`. No hidden state, no monad transformer stack, no attempt combinator needed — backtracking on `choice` is automatic. Pratt-style operator parsing is a first-class concept. Work on XParsec.FSharp started against XParsec 0.2 and quickly exposed limits in the operator parser that the library couldn't express when it came to parsing F# expressions and customer operator syntax; the redesign that followed became the 0.3 release. Post 4 tells that story.
- **What "F# parsing in F#" actually means.** A lexer (tokens, trivia, interpolated strings, preprocessor directives), an offside-rule aware token stream, and a parser that produces a faithful AST. No typechecker, no name resolution — just syntax. Could this serve as alternative to the current F# parser or as the basis for a distant F#2? Possibly, but that wasn't my goal and I specifically avoided looking at the F# source code for a long time to not bias any designs I would come up with.
- **Why not FParsec or FsLexYacc?** FParsec is a wonderful library; it was the inspiration for XParsec, but XParsec is my library and I wanted to prove to myself that I had built a production quality parser library. FsLexYacc is what the F# compiler itself uses; going parser-combinator-first was a deliberate constraint, both to put XParsec under real pressure and to see how close a combinator parser could get to a hand-rolled table-driven one.
- **Ground rules.** Strict DAG project structure (no `namespace rec`), `ParserRefs.fs` to break recursion, statically allocated parsers, golden-file testing.

## Anchor commits

- `77cc5a2` — WIP: Creating an F# compatible lexer (the start)
- `686f202` — Initial commit (XParsec itself)
- `94875a0` — Light mode for record expressions and patterns

## Takeaway

Parser combinators can parse real languages. The trick is knowing which parts of the grammar deserve their own primitives — and that's the whole rest of the series.
