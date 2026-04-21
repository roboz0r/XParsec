# 2. Building the lexer: numbers, strings, operators, and mechanical sympathy

A lexer sounds simple until you open the F# spec. Nested block comments. Interpolated strings whose holes contain further F# source. Fused multi-character operators whose shape only disambiguates at parse time. Numeric literal forms for every integer size in .NET plus BigInteger, decimal, and native-sized integer. And every bit of source text has to come out with a position, because every diagnostic downstream needs one.

This post is about the *pure* lexer — `Token.fs` and `Lexing.fs`. It doesn't cover preprocessor directive evaluation, virtual tokens for layout, or the offside rule. Those live one layer up, in the lexical filter; that's the next post.

## Tokens fit in 16 bits

Every token in F# is one of: a keyword (~70 of them), an identifier (a few flavors), a textual literal (a string or char in one of seven syntaxes), a numeric literal (about twenty integer/float types crossed with four bases), an operator (F# allows practically infinite custom operators defined by their first character), or a "special" token (whitespace, comments, directives). Round generously, that's under five hundred distinct token IDs.

`uint16` gives 65,536 values — two orders of magnitude more headroom than the language needs. The interesting question isn't *do we have enough bits*. It's *how do we partition them so the bits do useful work.*

The partition I settled on, after several rewrites visible in `d99bb47` and `0d9a31b`, looks like this:

```
 bit:  15 14 13 | 12 | 11 | 10  9  8 | 7  6  5  4  3  2  1  0
        Kind    | IC | IV |  Spare   |         Payload
        (3)     | (1)| (1)|  (3)     |         (8)
```

- **Bits 15-13 (`Kind`, 3 bits, 8 values).** The broad category: `Keyword`, `Identifier`, `TextLiteral`, `NumericLiteral`, `Operator`, `Special`, `Invalid`, plus one `Spare` reserved for future use. Most "is this token a..." checks are a single mask-and-compare against these three bits.
- **Bit 12 (`InComment`).** The token came from inside a block comment. The lexer never gates on this bit; it's metadata for tooling.
- **Bit 11 (`IsVirtual`).** The token was synthesized rather than read from source. The lexer never sets this bit; only the lexical filter does, by OR-ing it into an otherwise-real kind.
- **Bits 10-8 (Spare).** Currently unused. Reserved for cheap future extensions.
- **Bits 7-0 (`Payload`, 8 bits, 256 values).** The within-kind discriminator. For a keyword, this is the keyword ID (`KW.Let = 34us`). For an operator, bits 0-5 are precedence and bit 6 is a `CanBePrefix` flag. For a numeric literal, bits 6-5 are the base (decimal/hex/octal/binary) and bits 0-4 are the type (`Int32`, `UInt64`, `IEEE32`, `BigIntegerQ`, …). For a textual literal, bits 0-5 select the string flavor.

A `Token` enum value is constructed by bitwise OR:

```fsharp
type Token =
    | KWLet      = (KindKeyword        ||| KW.Let)
    | OpComma    = (KindOperator       ||| Operator.Comma)
    | NumInt32   = (KindNumericLiteral ||| Numeric.Int32)
    | StringOpen = (KindTextLiteral    ||| Text.StringOpen)
    | EOF        = (KindSpecial        ||| Special.EOF)
    // ... about 450 more
```

The payoff isn't just compactness — it's that *queries* about a token are usually one bitwise op. *Is this an operator?* Mask the kind bits and compare. *What's its precedence?* Mask the low six bits. *Could it be used as a prefix?* Test bit 6. None of these dispatch through a switch or a dictionary. The Pratt parser walks chains of operators thousands of times per file; making each "what's the precedence" lookup a single AND mattered for the rest of the system.

The most-cited consequence of this encoding is that comparison operators *collide on purpose*. `OpGreaterThan`, `OpLessThan`, `OpInequality`, `OpLessThanOrEqual`, `OpGreaterThanOrEqual` all equal the same numeric value: `KindOperator ||| Precedence.ComparisonAndBitwise`. They're not distinguishable as `Token` values. They aren't supposed to be — the parser cares about precedence, not spelling. When the spelling actually matters (which happens, e.g., when closing a type-parameter list), it's recovered from the source span via `ParseState.tokenStringIs`. The encoding deliberately throws away information the parser doesn't need. Selective semantic erasure is crucial here. It lets the lexer provide everything the parser needs while retaining mechanical sympathy.

## Eight bytes per token

The kind fits in 16 bits; the position has to live somewhere too. I held the whole `PositionedToken` to 64 bits — one machine word — from very early on:

```fsharp
[<Struct>]
type PositionedToken =
    val private value: uint64
    static member Create(token: Token, startIndex: int) =
        let indexPart = uint64 startIndex <<< 16
        let tokenPart = uint64 token
        PositionedToken(indexPart ||| tokenPart)
    member this.Token      = this.value &&& TokenMask |> uint16 |> EnumOfValue
    member this.StartIndex = int (this.value >>> 16)
```

That's `uint16 Token` in the low 16 bits and `int StartIndex` in the upper 48 bits. No length field: a token's length is `tokens[i+1].StartIndex - tokens[i].StartIndex`. That works only because *nothing is missing* between adjacent tokens — which is what the next section is about.

The index width was the one I wavered on. Originally I had 48 bits, on the theory that you might want to lex a file that doesn't fit in 32 bits of address space. The lexer work pushed the question. F#'s `prim-types.fs`, the worst-case file in the compiler corpus, is <500 KB — comfortably under twenty bits of index. The whole F# compiler source totals about 30 MB. A 32-bit index covers 2 GB, which is more than any single source file the .NET process will ever realistically see.

So I made the structural call honestly: **XParsec is in-memory only.** I deleted the `ReadableStream` abstraction from the library, narrowed `XParsec.Reader` from 64-bit to 32-bit indexing, and committed to that as an explicit constraint rather than a soft default. The escape hatch for genuinely large inputs is *user-owned buffering* — read into a 4 MB buffer, hand it to a `Reader`, parse, repeat, with the caller responsible for any token that straddles a chunk boundary. That's a real workflow, not a missing feature. Trying to make the library do it transparently would mean carrying buffering machinery, async readers, and partial-token recovery through every primitive in XParsec — to serve a use case the F# parser (and the majority of real world parsers) would never need. Rejecting it explicitly was worth more than 16 bits of index headroom.

So the layout settled at:

- **Bits 0–15:** token (kind + flags + payload)
- **Bits 16–47:** start index in source (32 bits, max 2 GB)
- **Bits 48–63:** unused

Sixteen bits sit idle. They're the obvious place to put things later — a token-length cap for tighter slicing, a hash for fast equality, an interned-name index for identifiers, source-fragment IDs for multi-file parses. None of those are needed today. The point is that the room exists, the layout is fixed, and adding any of them is a non-breaking additive change rather than a refactor.

For an F# audience this design is the most obvious deviation from idiom in the codebase. The natural F# choice would be a discriminated union with ~450 cases — one per token kind — pattern-matched at every site that asks a question. A close variant is a DU (or record) carrying precomputed fields: `{ Kind; Precedence; CanBePrefix; … }`, allocated once per token. Both are clear, both are easy to extend, both are what I'd reach for in almost any other context.

Both lose, badly, on a hot path that walks 50,000 tokens per file. F# represents reference-type DU cases as heap-allocated objects. A DU bundling a `Token` reference with a StartIndex is at minimum 16 bytes — .NET object overhead and word alignment make that a fair floor — which is double the encoded approach per token before any consideration of query cost. Worse, queries like *is this an operator?* or *what's the precedence?* can't be a bitwise op against a value; they're a function call, usually a sequence of pattern arms, against a single-cycle binary AND.

Another instinct is to reach for a struct DU instead — same exhaustive matching, no object overhead, but, F# struct DUs don't union their fields the way a C union does: they allocate space for *all* cases' payloads side by side, with a discriminator tag. Two or three cases is fine. A 450-case struct DU would size for the union of every case's fields and balloon to hundreds of bytes per token. Not viable.

A record with a handful of fields plus alignment typically puts each token in the 24–32 byte range, which puts a 50,000-token stream beyond L2 on most CPUs and forces the parser to eat memory bandwidth on a workload that should sit on the L1/L2 boundary.

At 8 bytes per token, the same 50,000-token stream is 400 KB and fits comfortably in L2 on every machine the parser will ever run on, and every query is one mask-and-compare. The cost is that the design is no longer recognizable as idiomatic F# — queries go through `TokenInfo` helpers rather than a `match` expression, and adding a new flag means picking a bit rather than adding a case. **This is the only place in the codebase I went to this length on bit packing.** For a single hot type at the bottom of the stack — touched by every parser, walked thousands of times per file — it was worth doing once. Repeating the trick anywhere else would be a smell.

## Full Fidelity Token Streams

Every space, every newline, every line comment, every block comment emits a token. The token stream is a *complete cover* of the source — index 0 to EOF, no gaps.

Most lexers throw trivia away. Keeping it costs about 30% more tokens on real F# files. Why pay?

Three reasons.

First, the no-length-field trick from the previous section depends on it. If trivia were dropped, "length of token `i`" would no longer equal "start of token `i+1` minus start of token `i`" — there'd be invisible gaps wherever a comment or a space had been. The eight-byte token only works if the array is gap-free.

Second, F#'s grammar has *adjacency-sensitive* rules. `f x.y` and `f x .y` parse differently. `f[i]` is high-precedence indexer access; `f [i]` is normal application. `f<T>` is generic instantiation only when there's no whitespace before `<`. Type measures, custom operator suffixes, and the `..` range operator all behave differently based on whether two tokens touch. With trivia in the stream, "are these two tokens adjacent?" becomes "is the previous array slot a non-trivia token?" — see `isPrevTokenSyntax` in `ParsingHelpers.fs`. Without trivia, the parser would need to carry column deltas through every rule.

Third, formatters, refactoring tools, and syntax highlighters all need trivia. There's no point producing two different token streams for two different consumers when one stream serves both — especially when "the parser shouldn't see trivia" can be solved cheaply at one layer up. The lexical filter (post 3) hides trivia from the parsers' point of view; the parsers consume `nextSyntaxToken` and never know it's there.

## The dispatch: one match on (char, context)

The lexer's main loop is one big `match` statement on a pair: the next character, and the current context. Context-free single-character dispatch isn't enough — the same `'{'` is a record opener in expression context, an interpolated-expression opener inside a `$"..."` string, and an escape character (`{{`) inside a different interpolated string. The current context disambiguates, and the context lives on a stack inside `LexBuilder`:

```fsharp
type LexContext =
    | Normal
    | InterpolatedString
    | VerbatimInterpolatedString
    | Interpolated3String of level: int
    | InterpolatedExpression
    | BracedExpression
    | BraceBarExpression
    | ParenthesExpression
    | BracketedExpression
    | QuotedExpression
    | TypedQuotedExpression
    | IfDirective
    | PlainString
    | VerbatimString
    | TripleQuotedString

type LexBuilder = {
    Source: string
    Tokens: ImmutableArray<PositionedToken>.Builder
    mutable Context: LexContext list      // stack
    mutable AtStartOfLine: bool
    mutable IsInBlockComment: bool
    LineStarts: ImmutableArray<int<token>>.Builder
    // ... a few more tracking fields
}
```

A context is *pushed* when the lexer enters an environment with different rules (a `(` pushes `ParenthesExpression`; a `$"` pushes `InterpolatedString`) and *popped* when the corresponding closer is seen. The push/pop happens via a `CtxOp` value returned alongside each emitted token, which keeps the dispatch and the stack manipulation in one place rather than scattered across each sub-parser. 15 contexts sprang out early reading the lexical specification, an assumption I hadn't questioned until writing them here is whether they were all necessary for lexical correctness and something to consider optimizing in the future.

The dispatch itself is a flat `match` — about 60 arms — that picks the right sub-parser:

```fsharp
let rec lex (reader: Reader<char, LexBuilder, ReadableString, _>) =
    match reader.Peek() with
    | ValueNone -> Ok (LexBuilder.complete reader.Position.Index reader.State)
    | ValueSome c ->
        let ctx = LexBuilder.currentContext reader.State
        let p =
            match c, ctx with
            | ('\r' | '\n'), LexContext.IfDirective -> IfDirective.pNewlineToken
            | '(',           LexContext.IfDirective -> IfDirective.pLParenToken
            | ('\r' | '\n'), ExpressionCtx          -> pNewlineToken
            | ' ',           ExpressionCtx          -> pIndentOrWhitespaceToken
            | ',',           ExpressionCtx          -> pCommaToken
            | '(',           ExpressionCtx          -> pLParenToken
            | '{', LexContext.InterpolatedString    -> pInterpolatedExpressionStartToken
            | '{',           ExpressionCtx          -> 
                choiceL [pOpenBraceBarExpressionContext; pOpenBraceExpressionContext] "Left brace"
            | '"', LexContext.InterpolatedString    -> pInterpolatedStringEndToken
            | '"', _                                -> pDoubleQuoteToken
            | c, ExpressionCtx when NumericLiterals.isDecimalDigit c -> NumericLiterals.parseToken
            | c, ExpressionCtx when isIdentStartChar c                -> pIdentifierOrKeywordToken
            // ... about fifty more arms
            | _, _ -> pOtherToken
        match p reader with
        | Ok ()   -> lex reader
        | Error e -> Error e
```

The shape matters more than the specific arms. Single-character lookahead picks the sub-parser; the sub-parser does its own multi-character matching internally; the loop tail-recurses on success. Errors propagate up, but in practice the lexer almost never fails — it has separate token kinds for malformed inputs that let it keep going.

`ExpressionCtx` and `NonInterpolatedExpressionCtx` in those patterns are *active patterns* over the context list. They match the contexts where F# expression syntax is in effect (effectively: anything that isn't string-fragment lexing or `#if`-directive lexing). Active patterns let the dispatch read like the spec — *"in expression context, `(` starts a paren"* — without enumerating each context explicitly.

The important point is not the number of contexts, but the direction of information flow: the lexer records only the minimum context needed to emit correct tokens, and nothing downstream has to rediscover it.

## From `lex` to `dispatch`

`lex`'s inner shape — *peek, pick a parser, run it* — is general. It's the canonical LL(1) predictive-parsing pattern, and a substantial fraction of any real grammar exposes stretches where the next decision is uniquely determined by one lookahead item.

The reason `lex` is written by hand rather than with combinators is allocation. The obvious combinator version of its inner step,

```fsharp
peek >>= fun t -> getUserState >>= fun ctx -> pickParserFor t ctx
```

costs a closure allocation per `bind`, and a per-character loop is the worst place to pay that. So I wrote `lex` raw — and then watched myself reach for the same peek-and-dispatch shape in three different places in the F# parsers, sometimes in a loop, sometimes just once.

After the third one, I lifted the pattern into XParsec itself as `dispatch` and `dispatchWithState`:

```fsharp
let inline dispatch
    ([<InlineIfLambda>] f: 'T voption -> Parser<'Parsed, 'T, 'State, 'Input, 'InputSlice>)
    (reader: Reader<_, _, _, _>) =
    let t = reader.Peek()
    let p = f t
    p reader

let inline dispatchWithState
    ([<InlineIfLambda>] f: 'State -> 'T voption -> Parser<'Parsed, 'T, 'State, 'Input, 'InputSlice>)
    (reader: Reader<_, _, _, _>) =
    let s = reader.State
    let t = reader.Peek()
    let p = f s t
    p reader
```

`InlineIfLambda` on the function parameter means the call site's `match t with ...` is inlined into the dispatcher body. Nothing is allocated, and the call to the picked parser is a direct invocation — the combinator collapses to the same machine code the hand-written version would emit. The caller gets to write their dispatch as `dispatch (fun t -> match t with …)` instead of opening the reader directly.

This is a small example of the thesis from the prologue, but it's a clean one. XParsec didn't have `dispatch` until the F# parsers demanded it; once the primitive was there, every other parser that wanted single-token lookahead got the same hand-written speed without dropping out of combinator style. Most of what landed in the XParsec 0.3 release came in via this same loop: a parser in XParsec.FSharp got written raw because the combinator version was too expensive, the same shape appeared a few more times, and then it became a primitive that every later parser could use cheaply.

## Fused operators, split later

The most-encountered example of this is `>>`. Inside an expression it's the function-composition operator — one fused token. Inside a type annotation like `Map<int, Option<string>>` it's two `>`s, each closing one of the nested type-parameter brackets — and the lexer can't tell which from looking at characters alone. So it doesn't try. It always *fuses*: `>>` is one token in the stream, and it's the type-parameter-closing parser's job to peel one `>` off the front and leave the rest behind for the next consumer.

The same call applies further afield. `>]` could be the close of an attribute target (`[<Foo>]`) or array indexer with units of measure `xs.[1<token>]`, where `>]` is lexically ambiguous and the source of many hours searching for failures in the early days of the parser.

The cost lives in `ParseState`. The filter and the parsers carry explicit state to track partial consumption: `SplitRAttrBracket` and `SplitPowerMinus` flags for the binary cases (the fused token has been half-eaten by an inner parser; the next read should yield the other half), and a `CharsConsumedAfterTypeParams` counter for the `>>` family, where any number of `>`s might need to be peeled off in sequence. Post 3 is where that story actually lands.

## Context the lexer does keep

The rule so far has been *lex now, decide later* — fuse what's ambiguous, defer disambiguation to whoever has the context. There's one class of decision the lexer can't defer: token kinds whose identity depends on *where on the line the character appears*. There's no later for those; the kind is fixed at lex time, and downstream consumers see the kind, not the source bytes.

The clearest case is `#`. F#'s preprocessor directives — `#if`, `#else`, `#endif`, `#nowarn`, `#warnon`, `#load`, `#r`, and the rest — are only directives when they appear *as the first non-whitespace token of a line*. A `#` anywhere else is lexed as `Token.InvalidDirective`. The kind isn't recoverable from the source span; downstream parsers don't reach back to ask whether the `#` was line-leading. The decision has to be right when the token is emitted.

The lexer carries one bit of state to support this: `LexBuilder.AtStartOfLine`. It's set when a newline token is emitted and cleared the first time a non-trivia token is emitted on the line. When the dispatcher sees `'#'` and `AtStartOfLine` is true, it routes to the directive parser; otherwise it falls into the invalid-token path. The *lex-now* part is just identification — the lexer doesn't evaluate `#if FOO` to decide what to suppress, only records that an `IfDirective` token sat at the start of a line. Evaluation is still the filter's job (post 3).

That position-tracking flag is the only piece of context sensitivity in the lexer outside the `LexContext` stack covered earlier. Both kinds of state exist for the same reason: the token's identity depends on where it occurs, and only the lexer is in a position to record that.

## Failing without giving up

A broken string literal at the top of a file shouldn't kill the lexer's ability to highlight an error on line 200. So the lexer doesn't *fail* on broken inputs — it emits a distinct token kind for each kind of breakage and keeps going.

- `UnterminatedStringLiteral` — a string opener with no matching closer before EOF or an illegal newline
- `UnterminatedBacktickedIdentifier` — a `` ` `` with no matching `` ` ``
- `InvalidDirective` — a `#` followed by something that doesn't match any known directive

Diagnostics are attached at *parse* time, where there's enough context to say what was expected. The lexer's job is just to produce the token stream that lets the parse continue. A file with one bad string and a thousand good lines still parses the thousand good lines.

The handling of unterminated interpolated strings (`bb959c3`) was particularly fiddly because the string might leave the context stack with stale entries — a `$"` pushes `InterpolatedString`, and if the closing `"` is missing, the stack needs to be unwound at EOF. `LexBuilder.complete` handles this: at end of input, it walks the remaining context stack and emits an `Unterminated*` token for each unclosed opener, then drains the stack. Recovery at the lexer layer is one of the two reasons the parser's recovery layer (post 9) doesn't have to handle every kind of corruption itself.

## A bug story: `(*)` is not a comment

Commit `22a8fc7 Fix lexing "(*)"`.

`(*` opens a block comment. So what's `let star = (*)` — the parenthesized multiplication operator, exposed as a function value? That's three tokens: `(`, `*`, `)`. But the lexer's `(` dispatch had a `choiceL` whose first alternative was *block-comment open* and whose second was *paren*. When the source said `(*)`, the lexer matched `(*` first, then tried to find the matching `*)` — and did, half a character later, with an unbalanced paren left over.

The fix is one parser and one ordering tweak:

```fsharp
let pParenStarOperator =
    parser {
        let! pos = getPosition
        let! _   = pstring "(*)"
        let idx  = int pos.Index
        do! updateUserState (fun state ->
            state
            |> LexBuilder.appendI Token.KWLParen   idx       (CtxOp.Push LexContext.ParenthesExpression)
            |> LexBuilder.appendI Token.OpMultiply (idx + 1) CtxOp.NoOp
            |> LexBuilder.appendI Token.KWRParen   (idx + 2) (CtxOp.Pop LexContext.ParenthesExpression))
    }

let pLParenToken =
    choiceL [
        pToken (pstring "(*IF-CAML*)")  Token.StartOCamlBlockComment
        pToken (pstring "(*IF-OCAML*)") Token.StartOCamlBlockComment
        pToken (pstring "(*IF-FSHARP")  Token.StartFSharpBlockComment
        pToken (pstring "(*F#")         Token.StartFSharpBlockComment
        pParenStarOperator                                  // ← inserted here
        pToken (pstring "(*") Token.BlockCommentStart
        // ...
    ]
```

`pParenStarOperator` matches the literal three characters `(*)` and emits all three tokens at once, including the proper push and pop of the `ParenthesExpression` context. Crucially, it has to come *before* `(*` in the choice list — backtracking on `choice` is automatic in XParsec, but ordering still decides which alternative wins for inputs that match multiple.

The (*) bug is a one-paragraph story, but it’s representative of the whole lexing phase. The lexer is a pile of small ordering-and-precedence decisions, each correct in isolation. A 7,000-line file is how you find the exact spot where two of them collide.

At this point, we have a dense, CPU friendly stream of 8-byte tokens. But it's full of whitespace, comments, un-evaluated `#if` directives, and operators that haven't been split yet. To turn this raw stream into something the parser can actually understand, we need to filter it. That’s the Lexical Filter, and the offside rule, which is coming up in Part 3.

## Going deeper on the details

This final section is optional reading, but it captures the kind of detail that only shows up when lexing a real language.

### Numeric literals with a suffix matrix

Numeric literals are a small but vivid example of how much detail a real language demands. F# supports four bases (decimal, `0x` hex, `0o` octal, `0b` binary) and roughly twenty target types selected by an optional suffix. Some suffixes are valid in any base; some are decimal-only. Some are case-sensitive; some have both upper and lower variants. Here's the integer suffix dispatch from `getIntToken`, lightly trimmed:

```fsharp
let private getIntToken (numBase: NumericBase) (suffix: string) =
    let token =
        if suffix.Length > 2 then
            Token.ReservedNumericLiteral
        else
            match suffix with
            | "y"         -> Token.NumSByte
            | "uy"        -> Token.NumByte
            | "s"         -> Token.NumInt16
            | "us"        -> Token.NumUInt16
            | "" | "l"    -> Token.NumInt32
            | "u" | "ul"  -> Token.NumUInt32
            | "n"         -> Token.NumNativeInt
            | "un"        -> Token.NumUNativeInt
            | "L"         -> Token.NumInt64
            | "uL" | "UL" -> Token.NumUInt64
            | "I" ->
                match numBase with
                | NumericBase.Decimal -> Token.NumBigIntegerI
                | _ -> Token.ReservedNumericLiteral
            | "Q"  | "R" | "Z" | "N" | "G" -> // More library defined BigInteger variants
            | // and more to lex permitted decimals as floating point values
            | _ -> Token.ReservedNumericLiteral

    // Combine the base into the token
    let numBase = uint16 numBase <<< TokenRepresentation.NumericBaseShift
    Token.ofUInt16 (uint16 token ||| numBase)
```

The structure of the parser around it is straightforward:

```fsharp
let parseToken = parser {
    let! pos     = getPosition
    let! numBase = pXIntBase      // peeks "0x"/"0o"/"0b" or defaults to Decimal
    match numBase with
    | Decimal -> 
        // a decimal literal might be int (`123L`), exponent-float (`1e5f`), 
        // or fraction-float (`1.5m`). Try them in that order.
        return! parseDecimalToken
    | _ -> 
        let! suffix = manyChars pIdentChar
        return getIntToken numBase suffix
}
```

The interesting moments are at the seams. Decimal-only suffixes like `M` produce `ReservedNumericLiteral` (rather than failing) when applied to a hex literal — the spec reserves them, and a `0xFFM` is more likely to be a typo than a valid construct under some future revision. Underscores between digits are skipped silently. The `.` in `1.0` has to *not* be followed by another `.`, otherwise it's the range operator (`1..10`) and the integer parse needs to hand back without consuming the dot. All of these were corpus-test bugs at one point or another, and `e89a38a Fix lexing of floating point literals` is a representative entry on that list.

### Strings as fragments

Originally a simple `"hello"` was one `StringLiteral` token. But eventually I realised compilers and tooling needs to know what's in the characters. So, I rewrote string lexing so that *every* string literal — plain, verbatim, triple-quoted, interpolated — emits the same token shape: an opener, a sequence of fragments and escape sequences, a closer. Commit `17172d4 Parse all string literals as fragments`.

The token kinds a string expands into:

- `StringOpen` / `StringClose` — the boundary tokens, carrying the syntax (plain, verbatim, triple, interpolated, …)
- `StringFragment` — a run of plain characters between escapes/holes
- `EscapeSequence` — `\n`, `\u0041`, `\x41`, etc.
- `FormatPlaceholder` — the `%d` family in `printf`-style strings (interpolated strings carry these for type-checked formatting)
- `InterpolatedExpressionOpen` / `InterpolatedExpressionClose` — the `{` and `}` around an interpolation hole

A plain string `"hello"` lexes as:

```
StringOpen("\"")  StringFragment("hello")  StringClose("\"")
```

A simple interpolation `$"x = {x}"` lexes as:

```
StringOpen("$\"")  StringFragment("x = ")
InterpolatedExpressionOpen("{")  Identifier("x")  InterpolatedExpressionClose("}")
StringClose("\"")
```

Everything inside the holes is regular F# tokens, lexed by the same dispatch loop, with `InterpolatedExpression` pushed onto the context stack so `}` knows to close the hole instead of being a record-end. Triple-quoted interpolated strings (`$"""..."""`) carry an extra "level" parameter on the context to track how many `{` braces start a hole vs. how many are literal — that's an irritating piece of detail that gets its own post (post 7).

The unification matters more than it looks. Parsers downstream don't have to distinguish "plain string" from "interpolated string" — they consume `StringOpen ... StringClose` and the contents are uniform. Tools that build expressions out of string contents (XML-ish or DSL-ish embeddings, custom operators that take strings) get the same token shape regardless of which surface syntax the user wrote.

## Anchor commits / files

- `src/XParsec.FSharp/Token.fs` (2,443 lines — the `Token` enum, the bit-packing helpers, `TokenInfo` queries)
- `src/XParsec.FSharp/Lexing.fs` (2,621 lines — the dispatch loop, sub-parsers, `LexBuilder` state)
- `d99bb47 Reworking Token definition`
- `17172d4 Parse all string literals as fragments`
- `e89a38a Fix lexing of floating point literals`
- `22a8fc7 Fix lexing "(*)"`
- `037467c Fix lexing a block comment with trailing operator chars`
- `bb959c3 Handle unterminated interpolated strings`

## Takeaway

Five decisions carried the weight.

**Sixteen-bit tokens with partitioned bits** turned every "what is this token" query into a single bitwise op, which is what made the Pratt parser's hot path tolerable.

**Eight bytes per token, in-memory only** was a constraint that paid for itself a dozen ways: no length field, room for a 32-bit byte index, the entire 50,000-token stream of a large F# file fitting into 400 KB of contiguous L2-friendly memory. The 16 bits I left unused are a deliberate runway for future additions.

**Trivia retained in the stream** turned adjacency checks into a single array lookup, made the no-length-field trick sound, and gave tooling the same data structure as parsing.

**Fuse-and-defer** kept the lexer simple at the cost of extra state in the filter. Two layers, each of which only sees what it needs.

**Distinct kinds for broken inputs** instead of failures kept the lexer running across malformed files and let the rest of the pipeline produce useful diagnostics.

I started with the unglamorous parts — keywords, integers, line comments, block comments. The interesting design decisions all came later, often from corpus tests. None of them would read as load-bearing on their own. Together they set the ceiling for the entire rest of the parser.
