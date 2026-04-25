# 4. The lexical filter: An invisible pass behind the parsers

Post 2 ended with the lexer's materialised output: two immutable arrays: `Tokens` and `LineStarts`, sitting in memory and ready to be consumed. Post 3 took a detour through the most contextual corner of that lexer — interpolated strings — and the state machine that produces the token stream this post operates on. The reference F# compiler would now run two more passes over them. One evaluates `#if` directives and removes inactive branches. The next pass, what the spec calls the *Lexical Filter*, tracks indentation, enforces the offside rule, and inserts virtual delimiters so the syntactic grammar can stay context-free over layout. Each pass materialises its own output before handing it to the next.

XParsec.FSharp folds those stages into a single lazy step on top of the eager lexer.

> The name lexical filter comes from the spec, but it undersells what the layer actually does. A more accurate description is *whitespace sensitivity and virtual token synthesis*. Whitespace sensitivity is the offside rule: a token's column determines whether it continues the current construct, starts a nested one, or is rejected outright. Virtual token synthesis inserts tokens the grammar requires but the source does not contain: record-field separators, the `in` closing a `let` binding, and recovery tokens like a missing `)`.

I decided both belong here, rather than in the AST parsers, for the same reason: the decision depends on **position in the token stream**, not on **which grammar rule happens to be running**.

Conceptually, this layer is a portal. What enters it is the lexer's full-fidelity token stream: trivia, fused operators, unevaluated directives, positional information intact. What emerges is not the same stream:

- whitespace has been removed,
- inactive preprocessing branches have vanished,
- virtual delimiters have appeared,
- context-dependent tokens have been split or rewritten,
- and offside violations have been rejected.

The parsers never observe the lexical stream directly. They consume only the tokens that emerge on the other side: a clean, layout-respecting stream that lets the grammar read like a textbook parser combinator.

This post is about that layer: what it does, why collapsing multiple passes into one lazy view over an eager lexer is worth it, and what the AST parsers get to not worry about because of it.

## One match, seven jobs

The core of the transformation is a single recursive function. The branches tell the story:

```fsharp
let rec private nextSyntaxTokenImpl isPeek (reader: Reader<_, ParseState, _, _>) =
    match reader.Peek() with
    | ValueNone -> fail EndOfInput reader
    | ValueSome token when token.Token = Token.IfDirective ->
        processIfDirective (nextSyntaxTokenImpl isPeek) token reader
    | ValueSome token when token.Token = Token.ElseDirective ->
        // An active then-branch reached its #else. Skip the else-branch.
        reader.Skip(); skipElseBranch reader
        nextSyntaxTokenImpl isPeek reader
    | ValueSome token when token.Token = Token.EndIfDirective ->
        reader.Skip()
        nextSyntaxTokenImpl isPeek reader
    | ValueSome token when token.Token = Token.NoWarnDirective ->
        processWarnDirective (nextSyntaxTokenImpl isPeek) true  token reader
    | ValueSome token when token.Token = Token.WarnOnDirective ->
        processWarnDirective (nextSyntaxTokenImpl isPeek) false token reader
    | ValueSome token when isTriviaToken reader.State token ->
        reader.Skip()
        nextSyntaxTokenImpl isPeek reader
    | ValueSome token ->
        // split-flag rewrites, offside check, then either fail or return
        ...
```

Each branch dispatches to the necessary transformation: preprocess, skip trivia, synthesise, split, check layout. Nothing in the list is particularly clever in isolation. What's interesting is that they all live behind the same `peek`/`consume` interface and the rest of the parser never has to know which one is running.

## Trivia stays in the token array, but not in the parser's view

Post 2 argued for keeping trivia in the lexer's output: every space, every newline, every comment emits a `PositionedToken`. The array is a complete cover of the source, and that's what lets the eight-byte `PositionedToken` get away without a length field.

The parser, though, doesn't want to see any of that. A sea of whitespace tokens between two identifiers is noise. The filter turns the noise off:

```fsharp
| ValueSome token when isTriviaToken reader.State token ->
    reader.Skip()
    nextSyntaxTokenImpl isPeek reader
```

`isTriviaToken` classifies whitespace, line comments, block comment markers, newlines, and indent tokens as trivia. Anything else falls through. This is one of those "decide once, benefit everywhere" calls. The alternative, having the lexer emit fewer tokens and force tools that want trivia reconstruct it from source spans, would have pushed a lot of fiddly bookkeeping into every consumer that wasn't the parser. Keeping the array whole and filtering at the reading end inverts that.

## Virtual tokens with just-in-time delivery

Post 2 reserved a bit for `IsVirtual` in the `Token` enum but never set it. The filter is where it gets used. A *virtual token* is a token that wasn't in the source but needs to be seen by the parser for the grammar to terminate sensibly.

The most-used virtual token in the language is one most F# users have never written. Every `let` binding in light syntax has an invisible `in` at the end of it. The spec form is:

```fsharp
let x = 1 in x + 2
```

The everyday form is:

```fsharp
let x = 1
x + 2
```

In verbose syntax these parse differently: only the first has an `in`, so the second becomes `let x = 1 x + 2` — an error saying `1` isn't a function. In light syntax they're the same code: `in` is synthesised at the boundary between the binding and the body based on layout. Because almost every F# expression is reached through a `let` or `use`, the virtual `in` is among the most common tokens in a light-syntax parse, and it isn't in the source at all.

The synthesiser is `pLetOrUseIn` in `ExpressionParsing.fs`, called once per `let`/`use` binding after the RHS has been parsed:

```fsharp
let pLetOrUseIn letIndent (reader: Reader<_, ParseState, _, _>) =
    match peekNextSyntaxToken reader with
    | Ok t when t.Token = Token.KWIn -> consumePeeked t reader
    | Ok t ->
        // Not 'in'. Synthesise one if the next token is at the 'let' column,
        // or at the enclosing context's offside line.
        let indent = ParseState.getIndent reader.State (reader.Index * 1<token>)
        let state = reader.State

        let atContextIndent =
            match state.Context with
            // Inside a paren-like context (Indent = 0), the delimiter governs scope,
            // not indentation — always emit virtual 'in'.
            | { Indent = 0 } :: _ -> true
            | { Indent = ctxIndent } :: _ -> indent = ctxIndent || indent = letIndent
            | [] -> indent = 0 || indent = letIndent

        if atContextIndent then
            Ok (virtualToken (PositionedToken.Create(Token.VirtualIn, t.StartIndex)))
        else
            fail (Message "Expected 'in' at the same indent as 'let'") reader
    | Error _ ->
        // Peek failed — EOF or an offside fail caused by the next token.
        // Emit a virtual 'in' so `use _ = expr` at the end of a scope still closes cleanly.
        ...
        Ok (virtualToken (PositionedToken.Create(Token.VirtualIn, startIndex)))
```

Three things are worth noticing.

It's a grammar-level question, not a lexer question. The lexer can't synthesise the `in`; knowing the boundary between a binding and its body requires having parsed the RHS, which only happens during parsing. `pLetOrUseIn` runs when the `let`-binding parser has reached the end of its RHS; at that moment it has the column where the `let` started, and it knows whatever context was pushed around it. Neither of those is available to a pre-pass.

It's three rules in trenchcoat. If the next token really is `in`, consume it — verbose-syntax files work unchanged. If the next token is at the `let`'s column, or at the enclosing block's column, synthesise. If neither — if the next token is indented *past* the `let` — something else is going on: the RHS parse stopped too early, and synthesising `in` would be wrong. The parser fails and lets the enclosing recovery rule handle it.

Paren-like contexts short-circuit the check. Inside `(...)` or `[...]` the delimiter itself bounds the scope, so the offside check can't fire — `pLetOrUseIn` emits the virtual `in` unconditionally. `let x = 1 in x + 2` and `(let x = 1 in x + 2)` both parse via this path, with different reasons for reaching it. Post 6 covers why paren-like contexts sit on the stack as markers rather than offside lines.

The `EOF` branch at the bottom is the tail case: `use _ = disposable` as the last expression of a module. Peek fails (no tokens left after the binding's RHS), and without the synthesis the enclosing scope would never see a body. The virtual `in` closes it cleanly.

Every nested `let` in every F# file — `let x = ... let y = ... f x y`, the shape of most F# code people actually write — goes through this three or four times before the expression is complete. None of it is in the source, all of it shows up in the parse tree with `IsVirtual` set, so any formatter walking the tree can tell the difference between `in`s the user wrote and `in`s the parser invented.

### The rest of the virtual-token machinery

Around `pLetOrUseIn` the same shape repeats for other grammar-mandated tokens. `pDoneVirt` synthesises a virtual `done` to close `for` and `while` bodies.

The synthesiser is `nextSyntaxTokenVirtualCore`:

```fsharp
let private nextSyntaxTokenVirtualCore mkDiag t reader =
    match peekNextSyntaxToken reader with
    | Ok token when token.Token = t ->
        consumePeeked token reader                  // real token matches — consume
    | result ->
        // Either a different token, or an offside failure. Either way we synthesise.
        let startIndex, diagToken =
            match result with
            | Ok token -> token.StartIndex, token.PositionedToken
            | Error _ ->
                match reader.Peek() with
                | ValueSome tok -> tok.StartIndex, tok
                | ValueNone     -> 0, PositionedToken.Create(Token.EOF, 0)

        match mkDiag diagToken with
        | ValueSome code -> reader.State <- addErrorDiagnostic code diagToken reader.State
        | ValueNone -> ()

        let pt = mkVirtualPT t startIndex
        preturn { PositionedToken = pt; Index = TokenIndex.Virtual } reader
```

Two thin wrappers sit over the core. `nextSyntaxTokenVirtualIfNot` synthesises silently (no diagnostic). `nextSyntaxTokenVirtualWithDiagnostic` — used by `pEnclosed` for every paren-like construct — records an `UnclosedDelimiter` error that carries both the opening and the missing closing tokens, so the message downstream can say "`(` on line 12 was never closed" rather than just "expected `)`".

The `TokenIndex.Virtual` discriminator on the returned token matters. Real tokens carry their index into `Lexed.Tokens`, which lets later code extract the source span or walk around them in the array. Virtual tokens carry no index because they have no source span. `consumePeeked` throws (`invalidOp "Cannot consume a virtual token"`) if the reader is no longer at the peek position — an internal invariant violation that the parser can't continue from.

`nextSyntaxTokenVirtualIfNot Token.KWEnd` synthesises the `end` that closes a class or `begin` block when it's not present. `nextSyntaxTokenVirtualWithDiagnostic` does the same for `)`, `]`, `}`, `|]`, `|}` whenever `pEnclosed` hits a failed inner parse. `makeVirtualSep` synthesises a virtual `;` between record fields when the offside rule says a new field has started.

This is the whole virtual-token machinery: one shared core, a handful of wrappers, one virtual-token variant per grammatical rule that benefits from it. To the AST parsers this is transparent; they don't care whether a given closer is real or invented — they just know they got a `SyntaxToken` where they expected one, the `Index` discriminator tells them which kind if they later care.

## Fused operators, split on demand

Post 2 introduced fused operators: the lexer emits `>]` as a single `KWRAttrBracket` token, `>>` and  `^-` as custom operators, whose split depends on context. Splitting is the filter's problem.

Two pieces of state in `ParseState` carry the request across tokens:

```fsharp
type ParseState = {
    ...
    SplitRAttrBracket: bool
    SplitPowerMinus:   bool
    ...
}
```

When the measure parser in `ConstantParsing.fs` sees a `>]` that it wants to close a measure with, it does *not* consume the token. It returns a virtual `>` and sets `SplitRAttrBracket`. The next call to `nextSyntaxTokenImpl` checks the flag:

```fsharp
let token =
    if reader.State.SplitRAttrBracket && token.Token = Token.KWRAttrBracket then
        reader.State.Trace.Invoke(TraceEvent.SplitRAttrBracketConsumed(token.StartIndex))
        PositionedToken.Create(Token.KWRBracket, token.StartIndex + 1)
    elif reader.State.SplitPowerMinus then
        let span = reader.State.Lexed.GetTokenSpan(reader.Index * 1<token>, reader.State.Input)
        if span.Length >= 2 && span.[0] = '^' && span.[1] = '-' then
            PositionedToken.Create(Token.OpSubtraction, token.StartIndex + 1)
        else
            token
    else
        token
```

The rewrite is positional: the returned `KWRBracket` starts one character past the original `>]`, so downstream code that extracts a source span for the `]` gets the right byte. Once the rewritten token is consumed, the flag is cleared so it affects exactly one token.

The `SplitPowerMinus` case is the same idea for unit-of-measure exponents: `kg^-1` arrives as `^-` plus `1`, and the measure parser wants the `^` to be interpreted on its own. It takes the `^`, sets the flag, and the next read rewrites the remaining bytes as `OpSubtraction`, which the enclosing parser consumes through the normal path. The measure parser's rule: take the left half of a fused operator as a virtual token, leave the right half on the reader, and let the enclosing parser pick it up.

Fuse-and-defer at the lexer was a simplification. Split-on-demand at the filter is its symmetric cost. Together they push the question "what does this operator mean?" to the one layer that has the context to answer it — and leave every other layer dealing with one-token-at-a-time without lookahead.

## The offside check

One more thing happens on every token that survives the earlier branches: the offside check. The mechanics — the context stack, the push/pop discipline, and the permitted-undentation rules that let real F# parse at all — fill two posts of their own (posts 5 and 6). What matters here is *where* the check lives: in the same function, in front of every token the parser sees.

The reason is the same as for virtual tokens. The enforcement is uniform and unconditional, and lifting it into every parsing rule would both cost performance and risk forgetting it in one place. One enforcement point, one source of truth.

## `#if`, evaluated lazily

F# preprocessor directives operate on tokens: `#if DEBUG && !RELEASE` is a four-token condition, not a string. The filter evaluates them when it encounters them, and skips tokens from the inactive branches. The driver is `processIfDirective`:

```fsharp
let processIfDirective nextSyntaxToken (ifToken: PositionedToken) reader =
    let state  = reader.State
    let lexed  = state.Lexed
    let currentLine = findLineNumber state (tokenIndex reader)
    let nextLine    = currentLine + 1<_>

    let nextLineTokenIndex =
        if nextLine < lexed.LineStarts.LengthM then
            lexed.LineStarts[nextLine]
        else
            lexed.Tokens.LengthM - 1<_>

    // Slice just the #if line and hand it to the expression parser
    let sliceLen    = (nextLineTokenIndex * 1< / token>) - reader.Index
    let sliceReader = reader.Slice(0, sliceLen, { AbsoluteStart = reader.Index })
    reader.Index  <- nextLineTokenIndex * 1< / token>

    match IfExpr.parseSlice sliceReader with
    | Ok ifExpr ->
        if IfExpr.evaluateStateful ifExpr reader.State then
            nextSyntaxToken reader                    // active then-branch
        else if skipInactiveBranch reader then
            nextSyntaxToken reader                    // skipped to #else
        else
            nextSyntaxToken reader                    // skipped to #endif
    | Error e ->
        // Malformed #if: record a diagnostic, treat the whole block as inactive
        reader.State <- addErrorDiagnostic (DiagnosticCode.Other $"Invalid #if expression: {e}") ifToken reader.State
        skipInactiveBranch reader |> ignore
        nextSyntaxToken reader
```

The first is the slice. `reader.Slice` hands the expression parser its own bounded `Reader` whose index 0 is the `#if` token itself. The inner parser can't run off the end of the directive line because the slice ends where the line ends. The `AbsoluteStart` in the slice's user state preserves the outer token index so that when the expression parser reads an identifier token, it can still extract the source text and check whether the symbol is defined. The F# compiler does this with a separate tokeniser mode; XParsec does it with a slice and a two-word user-state struct.

The second is the expression parser itself. `IfExpr.parseSlice` (in `Preprocessing.fs`) is a full Pratt parser — `&&`, `||`, `!`, parenthesised sub-expressions, identifier terms. Four binding powers, a half-dozen completion functions. The same Pratt machinery the real expression parser uses, applied to a ten-token sub-grammar. It's a small but load-bearing demonstration that the library's operator primitives aren't only for the big grammar they were built for.

The third is `skipInactiveBranch`. Its job is to walk forward past tokens without parsing them until it hits `#else` or `#endif` at the right nesting depth. Nested `#if`s inside the inactive branch are counted via a depth counter; `EOF` mid-skip is treated as a graceful stop (an unclosed `#if` surfaces as a diagnostic, not a crash). The companion `skipElseBranch` handles the symmetric case where the then-branch was active and the subsequent `#else` needs to be discarded.

Both skip functions share `skipConditionalBranch`, parameterised on whether `#else` is a stop or not. They were originally separate until `83baaf0 Merge skipInactiveBranch and skipElseBranch` folded the common walk into one helper — a useful reminder that duplication in a parser is often easier to see in hindsight than at the time.

## Peek and consume share the same path

The lexical filter has a single entry point: `nextSyntaxTokenImpl`. Both peeking and consuming go through it. The entire pipeline runs either way: preprocessor directives, trivia skipping, virtual token synthesis, split-flag rewrites, offside checks. The difference between peek and consume is minimal and intentional:

- Peek stops with the reader positioned on the next meaningful token.
- Consume advances past that token and clears any one-shot rewrite flags.

Everything else is shared.

That design makes "peek" semantics here different from what many parsers assume. In most systems, peeking is strictly side-effect free. Here it is not. A peek over `// comment\n    foo` advances the reader past the comment and whitespace, then returns `foo`. The reader has moved. The mutation is safe because all the skipped tokens are trivia: they carry no grammatical meaning, and skipping them only makes future reads cheaper.

That cheapness is the point.

### Peek is allowed to mutate so the grammar can stay cheap

Once `peekNextSyntaxToken` has landed on `foo`, subsequent peeks are O(1). They inspect the same token and return immediately. This common dispatch pattern already only peeks once:

```fsharp
match! peekNextSyntaxToken with
| t when t.Token = Token.KWLet -> return! consumePeeked t
| t when t.Token = Token.KWDo  -> return! consumePeeked t
| _ -> return! fail (Message "Expected 'let' or 'do'")
```

But what about peeks made by adjacent parsers? We don't want them to have to pay twice. The initial peek pays the cost of filtering; `consumePeeked` advances one token.

If peek were required to be side-effect free, every peek would have to re-walk trivia from the last committed position. On real F# code, where a large fraction of tokens are whitespace, that would introduce a linear factor into almost every parsing rule.

The crucial invariant is that **peek's side effects are idempotent with respect to the grammar**. They only affect:

- trivia removal,
- virtual synthesis bookkeeping,
- layout/offside state.

They never change which significant token is next. Repeated peeks always return the same token until it is consumed.

### Backtracking still works

Even with mutating peeks, backtracking remains cheap. The reader's entire observable state is its `Position` the index and the `ParseState`. Saving and restoring `reader.Position` rewinds both trivia skips and token consumption in one assignment. There's no log to replay and no auxiliary cursor to reset.

Because of that, there's no separate "skip whitespace" primitive in the API. Filtering is how you see tokens at all. Consuming is explicit; skipping trivia is implicit and shared.

### Dispatch builds on the same invariant

`dispatchNextSyntaxTokenL` is just a thin wrapper over this model. It peeks once, selects a parser based on the returned token, and runs it:

```fsharp
let dispatchNextSyntaxTokenFallback (routes: (Token * Parser<_,_,_,_,_>) list) pFallback =
    let items   = routes |> List.map fst |> Array.ofList
    let parsers = routes |> List.map snd |> Array.ofList
    parser {
        let! next = peekNextSyntaxToken
        match Array.tryFindIndexV (fun t -> next.Token = t) items with
        | ValueSome i -> return! parsers[i]
        | ValueNone   -> return! pFallback
    }
```

Routes are mostly five items or fewer, so a linear scan beats a dictionary. More importantly, dispatch operates on the **filtered stream**. Every decision is based on a meaningful token: never trivia, never something that should already have been rejected for layout.

The parsers above this layer can be written as if whitespace didn't exist, preprocessing didn't exist, and layout were someone else's problem—because it is. All of that is enforced once, here, at the point where tokens become syntax.

## Why lazy, not materialised

Lexing itself is *not* lazy. The `Tokens` and `LineStarts` arrays are built eagerly in a single sweep before a parser ever runs. That choice pays for itself everywhere post 2 touched it: derived token lengths, binary-search diagnostic rendering, and O(1) indexed reads from whichever grammar rule happens to be active. Materialising the lexer's output is the right call.

The obvious next step would be to materialise the following stages too: evaluate all `#if` directives up front, build a filtered token array, and hand that array to the parser. That is, in fact, what the reference F# compiler does, and it would simplify several of the functions described above.

XParsec does not, for three reasons.

**First: error recovery.** When a parse rule fails partway through, `recoverWith` rewinds the reader's position and skips forward until it reaches a stopping token. Those tokens must flow through the filter *again*, because recovery can change the surrounding context: a virtual closer may have been pushed, or the failed rule may have installed its own offside frame. With a materialised filtered array, the implementation would either need to re-run the filter from the recovery point or maintain parallel indices into "raw" and "filtered" streams. Lazy filtering avoids the problem entirely: one reader position is the whole truth.

**Second: context-sensitive rewrites.** The split flags are the clearest example. `SplitRAttrBracket` is set by the measure parser because it has determined—*during parsing*—that `>]` must be split into `>` and `]`. A pre-materialised filter has no access to that information: the decision is discovered after filtering would have completed. Lazy filtering allows the parser upstairs to mutate shared state that the filter consults on the *next* token. The mechanism is small: two booleans and an integer for types, but it only works because filter and parser share one reader, not one buffer.

Both of these are direct consequences of using combinators. The reference compiler stages its pipeline because its passes do not share mutable state beyond what they explicitly serialise. A combinator pipeline naturally *does* share state: the reader and its context, so letting the filter consult that state is essentially free. Eager lexing, lazy filtering: each stage pays only for what it actually needs.

**Third: performance.** `#if` directives are rare in real code, so a fully materialised preprocessor pass would mostly duplicate the lexed array. Trivia, by contrast, is densely interspersed; almost every significant token is preceded by whitespace, but at eight bytes per token the next non-trivia token is usually in the same cache line and a mere handful of cycles away. Filtering lazily trades an occasional branch for avoiding whole-array copies and extra indirection.

Taken together, these reasons point in the same direction. The lexer benefits from being eager and indexable. The filter benefits from being stateful, replayable, and context-aware. Making the former materialised and the latter lazy keeps each stage honest about what it actually knows—and avoids inventing an intermediate representation solely to throw it away again.

## The public surface

Parsers don't call `nextSyntaxTokenImpl` directly. The exposed helpers are narrow, and every syntactic parser in the project is built from some combination of them:

- `peekNextSyntaxToken` / `consumePeeked` — the workhorse pair.
- `assertKeywordToken` / `assertKeywordTokens` — peek, assert, consume, return `(token, indent)` for offside-context use.
- `nextSyntaxTokenVirtualIfNot` / `nextSyntaxTokenVirtualWithDiagnostic` — synthesis.
- `nextSyntaxTokenIsL` / `nextSyntaxTokenSatisfiesL` — predicate match with a custom error message.
- `dispatchNextSyntaxTokenL` / `dispatchNextSyntaxTokenFallback` — LL(1)-ish branching.
- `withContext` / `withContextAt` — push/pop offside context around an inner parser.
- `recoverWith` / `recoverWithVirtualToken` / `recoverLongIdent` — recovery variants.

Roughly a dozen entry points. Everything in `ExpressionParsing.fs`, `PatternParsing.fs`, `TypeParsing.fs`, and `DeclarationParsing.fs` goes through them. None of those files knows that preprocessor directives exist, or that trivia exists, or that offside exists.

Three passes become one fat helper function. The helper does one thing — hand the next meaningful token to the parser — and happens to do whatever bookkeeping that takes: preprocessor, trivia, virtual synthesis, operator splitting, offside enforcement. Each concern earns a few lines in one `match`, not a pass of its own.

The architectural payoff is that half the project's hardest features become invisible to the most important part. Preprocessor handling, virtual tokens, offside, none of them appear in the AST-parser files. They're enforced at the door. The parsers beyond can look *almost* exactly like a textbook parser combinator grammar, because every place where F# breaks the textbook model is enforced once, at the portal.

It's also the smallest piece of the project by line count that has the highest leverage. `ParsingHelpers.fs` is ~1,400 lines, maybe ~500 of which are the filter proper. Every one of the hundreds of parsing rules in the rest of `XParsec.FSharp` runs through those 500 lines on every token.

Post 5 moves firmly to the other side: Pratt-style operator parsing over the transformed stream, and the operator zoo of F# forced the library to grow a second time.

## Anchor commits / files

- `src/XParsec.FSharp/ParsingHelpers.fs` — the whole file is this layer.
- `src/XParsec.FSharp/Preprocessing.fs` — `IfExpr.parseSlice`, `IfExpr.evaluateStateful`.
- `94d6547 Preprocess lexical blocks in the lexer` — the name is historical; the logic moved to the filter.
- `9895243 Add directives`, `a2bf409 Lexing IfDirective` — the `#if` pipeline lands.
- `8faaf58 Fix #if directive parsing`, `2f374be Add #if true test`, `153294a Add IfExprState` — iteration on the slice/user-state design.
- `83baaf0 Merge skipInactiveBranch and skipElseBranch` — the skip helpers fold into one.
- `a9899c9 Add virtual tokens`, `67787e4 Merge nextSyntaxTokenVirtualIfNot and nextSyntaxTokenVirtualWithDiagnostic` — virtual synthesis consolidates.

## Takeaway

Five ideas hold this layer up.

**One function, many passes folded.** Preprocessing, trivia removal, virtual synthesis, operator splitting, and the offside check all share a single `match`. Each concern earns a few lines, not a pass.

**Lazy filter over eager lexer.** The lexer materialises because the array is indexed and shared. The filter stays lazy because it needs to see state the parser is still mutating — split flags and context frames that didn't exist at lex time.

**Virtual tokens carry source positions.** Every invented token gets a `StartIndex` and an `IsVirtual` bit, so formatters and error messages can tell user-written tokens from parser-invented ones without reparsing.

**Split-on-demand is the price of fuse-and-defer.** Fused operators keep the lexer simple; two booleans in `ParseState` and a character counter let the filter hand back the halves the grammar asks for.

**Peek is allowed to mutate, carefully.** Skipping trivia and advancing past filtered tokens keep repeated peeks O(1), and one `reader.Position` assignment still rewinds everything for backtracking.

The filter is the smallest piece of the project by line count — maybe 500 lines of `ParsingHelpers.fs` — but everything downstream runs through it on every token. Getting it right once meant every parser above it could be written as if the problems it solves didn't exist.