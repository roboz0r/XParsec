# 5. Pratt parsing for F#'s operator zoo

A textbook operator parser expects operators to come in a small handful of shapes: infix, prefix, postfix, maybe ternary. Each has a precedence, each has an associativity, and the hard work is deciding which one binds tighter.

F# doesn't fit this model. Users can define their own operators, and the lexer assigns them precedence from their leading characters: `**>` is high, `||>` is low, anything starting with `-` is Additive, anything starting with `<` is Comparison. Some of them are prefix-only, some optionally prefix, and some whose *spelling* is `-` behave as prefix or infix depending on whitespace around them. A set of the most interesting "operators": `=` in bindings, `<-` for assignment, `:` for annotations, `as`, `when`, `|` in match clauses, aren't what a textbook would call operators at all. Function application is juxtaposition: two expressions in a row with whitespace between, no token of its own just adjacency. And `;` is variadic: `e1; e2; e3; e4` is *one* sequential expression, not three nested right-associative ones.

This is also the post where the F# work starts paying rent back to XParsec itself. Work on `XParsec.FSharp` began against XParsec 0.2. The first serious grammar features that didn't fit (variadic, mixed-arity, context-sensitive operator shapes) forced a redesign of the operator parser that eventually shipped as XParsec 0.3. The F# branch was rebased onto the redesigned API once it stabilised. What follows is the shape the operator parser ended up with after that pass, and the pieces of F#'s grammar that each addition was there to serve.

## Pratt, in one sentence

[Pratt parsing](./Vaughan.Pratt.TDOP.pdf) assigns every operator a *left binding power* and a *right binding power*, then parses expressions by asking at each step: *is the operator I'm about to consume tighter than the one that called me?* If yes, descend; if no, return. Associativity falls out of the gap between the two powers: left-associative operators have `LBP = N`, `RBP = N + 1`; right-associative operators flip it; non-associative operators have `LBP = RBP = N` and reject chaining.

XParsec encodes this directly:

```fsharp
[<Measure>] type bp

module BindingPower =
    let leftAssocRhs  (basePower: byte<bp>) = basePower + 1uy<bp>
    let rightAssocLhs (basePower: byte<bp>) = basePower + 1uy<bp>
```

A unit of measure on a byte. Precedences live in a 30-level enum (`P1`..`P30`) that maps to odd numbers 1, 3, 5, …, 59 — odd so the even slots are available for the asymmetric side. Eight-byte tokens (post 2), one-byte binding powers, and a Pratt loop that compares bytes. The whole mechanism is as close to the machine as it is to the textbook.

## What the 0.2 model got wrong

The shape that XParsec 0.2 shipped with was, in retrospect, the textbook one: an operator was uniformly binary or unary prefix, with a single `BindingPower` per token and a `complete` function of a fixed arity. A `choice` over a handful of cases covered `+`, `*`, prefix `-`, postfix `!`, parenthesisation. For most grammars that is more than enough.

F# broke it on four axes, in order of appearance:

- **Variadic operators**. Sequential composition `e1; e2; e3` is a single flat list, not nested right-associative infix. A tuple `a, b, c` is a single `Tuple` node with three elements, not `Tuple(a, Tuple(b, c))`. Modeling either as ordinary right-associative infix produced a tree shape the AST didn't want and a parse that lost the separator positions.
- **Mixed-arity operators whose RHS isn't an expression**. `x :> SomeType` is infix in shape but its RHS is a `Type`, not an `Expr`. `x.Name` is infix in shape but its RHS is an identifier, and `x.[0]` is infix in shape but its RHS is a whole bracketed index. The 0.2 API could not express "operator at precedence N whose right operand is parsed by a different parser and folded into the result via a three-argument completion".
- **Operators that bring their own open/close**. `arr.[i]` and `f(x)` are postfix in shape but they enclose content. `f<int>` is postfix but closes on `>` which may itself be fused with another `>`. A postfix case in 0.2 had no room for a close parser.
- **Control flow that wants to live in the operator table**. `if e1 then e2 else e3`, `fun x -> body`, `let x = e1 in e2`, `match e with | …` all sit at well-defined precedence levels in the F# spec and all should bind to the surrounding expression exactly the way a prefix operator does. But their bodies are not "parse another expression at power N"; they are fixed syntactic constructs that control their own structure.

Any one of these on its own could have been papered over with a special-case parser. Together, they indicated the abstraction wasn't pulling its weight. `85fa6a7 Rework operator parsing` is where the split happened; the rest of XParsec 0.3's operator additions are each named after the F# construct that forced them.

## The operator taxonomy after 0.3

The operator parser's central types split cleanly into two halves. Everything that can appear on the LHS of an expression is an `LHSOperator`; everything that extends an existing expression on the RHS is an `RHSOperator`:

```fsharp
type LHSOperator<'Op, 'Aux, 'Expr, 'T, 'State, 'Input, 'InputSlice ...> =
    | Prefix        of op * parseOp * rightPower * completePrefix
    | Enclosed      of op * parseOp * rightPower * closeOp * parseCloseOp * complete
    | LHSTernary    of op * parseOp * rightPower * delimiter * parseDelimiter * complete
    | PrefixMapped  of op * parseOp * parseRight * complete

type RHSOperator<'Op, 'Aux, 'Expr, 'T, 'State, 'Input, 'InputSlice ...> =
    | InfixLeft            of op * parseOp * leftPower * completeInfix
    | InfixRight           of op * parseOp * leftPower * completeInfix
    | InfixNonAssociative  of op * parseOp * leftPower * completeInfix
    | InfixNary            of op * parseOp * leftPower * allowTrailingOp * completeNary
    | InfixMapped          of op * parseOp * leftPower * parseRight * complete
    | Postfix              of op * parseOp * leftPower * completePostfix
    | Indexer              of op * parseOp * leftPower * closeOp * parseCloseOp * parseInnerExpr * completeIndexer
    | Ternary              of op * parseOp * leftPower * parseTernaryOp * completeTernary
```

Twelve cases total. Each one is there because an F# grammar rule refused to fit the previous case. `'Aux` is the escape hatch: when a completion function takes something other than an `Expr`, the something-else is an `'Aux`. `ExprAux` in `ExpressionParsing.fs` is the corresponding union, with cases for qualified identifiers, dot-indices, type casts, type applications, range endpoints, high-precedence argument lists, wrapped keyword expressions, and slice markers.

The cases pair to specific F# constructs:

- **`InfixLeft` / `InfixRight` / `InfixNonAssociative`**. The textbook infix cases, with the associativity encoded in the gap between `leftPower` and `rightPower`. These cover `+`, `*`, `|>`, `<|`, `&&`, `||`, `::`, `@`, `**`, custom user operators.
- **`InfixNary`**. Sequential composition (`;`), tuple construction (`,`), and — importantly — function application. All three want a flat list of operands, not a right-associative tree. `allowTrailingOp: bool` lets `;` silently accept a trailing separator (`[1; 2; 3;]`); `,` sets it to false because a trailing comma is an error in F# expression tuples. Commit `316eab5 Add RHSOperator.InfixNary`, refined by `ea3afec Add InfixNary.allowTrailingOp to RHSOperator`.
- **`InfixMapped`**. `x :> Type`, `x.Name`, `e :? Type` — binary in shape, RHS parsed by a non-Pratt parser, result folded via a 3-arg completion. The same mechanism handles `x.[i]` indexed lookup and even F#'s range operator (`..`), whose RHS can be either an expression or a slice terminator. Commit `62c8785 Add InfixMapped and LHSTernary operators`.
- **`Indexer`**. A postfix shape with both an opener and a closer around inner content, used for F# 6+ dotless indexing (`arr[i]`). Distinct from `InfixMapped` because the close token must be parsed and returned so the AST can retain its span.
- **`Ternary` / `LHSTernary`**. RHS ternary is the pattern for `cond ? then : else` in C-style languages — F# doesn't have that, but the shape is available. The one that F# actually uses is `LHSTernary`: a single LHS-side prefix-with-delimiter case that captures `if <cond> then <body>`, `while <cond> do <body>`, and `for <ident> in <seq> do <body>` without any of them needing a hand-rolled parser.
- **`Postfix`**. Plain postfix (`x!` in active patterns, `x?` in pattern matching).
- **`Prefix`**. Plain prefix operators (`!x`, `~~~x`, `-x`, `&x`).
- **`Enclosed`**. Parenthesised subexpressions `(e)`, `[e]`, `{e}`. Separate from `Prefix` because the close needs to be parsed and threaded through the completion.
- **`PrefixMapped`**. The one that did the most work. A prefix-shaped operator whose RHS is fully controlled by a custom parser rather than by the Pratt loop. This is how `if`, `match`, `fun`, `function`, `let ... in`, `use`, `do`, `yield`, `return`, `lazy`, `assert`, `upcast`, `downcast`, `fixed` all enter the operator table — see the `kwPrefixRoutes` table in `ExpressionParsing.fs:1789-1810`. Each of those keywords has a fully structured body grammar that has nothing to do with Pratt descent, but they still need to bind to the surrounding expression at a specific precedence. `PrefixMapped` provides the binding without pretending to own the body.

That final case was the pivot. Before it, the F# parser had a parallel track for keyword-led expressions that sat alongside the operator parser and duplicated the "what's the precedence of this thing relative to its surroundings?" logic. After `ab9853d Add PrefixMapped to operator parsing`, `if`, `match`, `fun`, and the rest all live in the same table as `+` and `*`, and the Pratt loop treats them the same.

## One interface, one lookup point

The 0.2 API handed operators to the parser as a concrete data structure. That made the lookup cheap but gave the caller no opportunity to consult parse state when deciding what an operator *means*. `4e8455f Make Operators an interface` turned the handoff into an abstract trait:

```fsharp
type Operators<'Op, 'Aux, 'Expr, 'T, 'State, 'Input, 'InputSlice ...> =
    abstract LhsParser: Parser<LHSOperator<...>, ...>
    abstract RhsParser: Parser<RHSOperator<...>, ...>
    abstract OpComparer: IEqualityComparer<'Op>
```

F# needs this because the same token means different things depending on context. `|` is a pattern-match clause separator, the bar in a discriminated union, a bitwise or, a computation-expression shape discriminator, and more. `>` is both a comparison operator and a type-parameter closer — and in the type-parameter case it may have been half-consumed already (post 4's `SplitRAttrBracket`). Resolving these requires reading from `ParseState`, which the concrete-lookup API couldn't do.

Splitting `LhsParser`, `RhsParser`, and `OpComparer` behind an interface lets `XParsec.FSharp` provide its own implementation that consults `ParseState` on every lookup:

```fsharp
interface Operators<SyntaxToken, ExprAux, Expr<SyntaxToken>, ...> with
    member _.LhsParser = lhsParser
    member _.RhsParser = rhsParser
    member _.OpComparer = opComparer
```

The `rhsParser` implementation is where the state-dependent decisions live. Its dispatch over `handleToken` is responsible for rejecting tokens that look like RHS operators but aren't RHS operators in *this* context. Commit `200115b Remove Operators.OpComparer` later dropped the interface's equality member to a generic default for most consumers, but the extension point remained open for the F# parser — `2348ae7 Add custom Operators.OpComparer` reopens it when the F# specific comparer is needed.

## LHS-only operators in RHS position

Several F# precedence levels exist on the LHS only. `PrecedenceLevel.Prefix`, `Function`, `If`, `Let`, `As`, `When`, `PatternMatchBar`, `Parens` — none of these have a legitimate meaning as an infix operator. A bare `if` in the middle of an expression `x if …` is nonsense; so is `x let y = …`.

Under 0.2's single-shape model, these slots were filled with `invalidOp` placeholders and the Pratt loop would throw if it ever dispatched to them. That was a latent bug. The first time a corpus test accidentally routed a keyword token into RHS position — usually because a preceding parse had failed to consume what it meant to — the parser crashed instead of failing gracefully.

The fix is in `handleToken` in `ExpressionParsing.fs:1076`:

```fsharp
| _ when
    (match opInfo.Precedence with
     | PrecedenceLevel.Prefix
     | PrecedenceLevel.Function
     | PrecedenceLevel.If
     | PrecedenceLevel.Let
     | PrecedenceLevel.As
     | PrecedenceLevel.When
     | PrecedenceLevel.PatternMatchBar
     | PrecedenceLevel.Parens -> true
     | _ -> false)
    ->
    fail (Message "LHS-only operator cannot appear in infix position")
```

Cheap soft failure. The Pratt loop treats this the same way it treats any other "no operator here": it rewinds the reader and hands control back to whoever called `parseRhsInternal`, usually so that function application or an outer enclosing parser can take the next token instead. The distinction between *"this token isn't an RHS operator"* and *"this token is an operator but not here"* matters for error recovery; both need to surface cleanly as "stop parsing RHS", not as an exception.

## Prefix-only vs optionally-prefix

`!`, `~`, `%`, `&`, `~~~` are prefix-only. They can't appear in infix position at all. `-`, `+` are *optionally* prefix: still infix in `f + g`, prefix in `f -x`, and prefix again in `-x`. The F# spec (§3.8.1, "Post-filtering of Adjacent Prefix Tokens") defines when the dual-use spelling means prefix: the operator must be *adjacent* to the following token (no whitespace between them) and *separated* from the preceding token (whitespace, newline, or comment before).

The parser needs both bits of information: the operator's `CanBePrefix` flag (from the token's bit-packed representation, post 2), and the adjacency of the surrounding raw tokens in the stream. `isAdjacentPrefixOp` in `ExpressionParsing.fs:889` reads the raw (pre-filter) token array because the filter's trivia-skipping would destroy the whitespace information the spec rule needs:

```fsharp
let isAdjacentPrefixOp (state: ParseState) (rawIndex: int<token>) =
    let tokens = state.Lexed.Tokens
    let rightAdjacent =
        rawIndex + 1<token> < tokens.Length * 1<token>
        && not (ParseState.isTriviaToken state tokens[rawIndex + 1<token>])
    let leftSeparated =
        rawIndex = 0<token>
        || (rawIndex > 0<token>
            && ParseState.isTriviaToken state tokens[rawIndex - 1<token>])
    rightAdjacent && leftSeparated
```

`pApplication` then decides whether a token can *start* an application argument:

```fsharp
let isAtomicExprToken (state: ParseState) (t: SyntaxToken) =
    match t.Token with
    | Token.Identifier | Token.KWLParen | ... -> true
    | _ ->
        if Constant.isLiteralToken t.Token then true
        else
            match OperatorInfo.TryCreate t.PositionedToken with
            | ValueSome opInfo when opInfo.CanBePrefix ->
                opInfo.Precedence = PrecedenceLevel.Prefix
                || (match t.Index with
                    | TokenIndex.Regular rawIndex -> isAdjacentPrefixOp state rawIndex
                    | TokenIndex.Virtual -> false)
            | _ -> false
```

Prefix-only operators (`Precedence = Prefix`) always start arguments. Dual-use operators only start arguments when the adjacency/separation test passes. This is commit `09d9d40 Fix handling of adjacent prefix operators in application`: before it, `f -x` either parsed as the subtraction `f - x` or failed with an ambiguity error, neither of which matched what F# programmers actually write.

The small, unglamorous lesson here is that the lexer's token-encoding work from post 2 was the thing that made this rule tractable. `CanBePrefix` is a single bit off the token's payload, and `OperatorInfo.TryCreate` is a bitwise mask. If the check had been a dictionary lookup it would have run on every application candidate on every file in the corpus, and the hot path would have eaten it.

## Nary rollback and infinite loops

`InfixNary` is straightforward in concept: parse an item, look for the separator, loop. The gotchas only emerge when the item parse fails partway.

The first version of `rhsInfixNary` hit two of them on the F# corpus.

The first is *trailing-separator rollback*. When `allowTrailingOp` is true, a failed item parse after a successful separator means the source ended with a dangling `;` and the whole construct should still succeed. The fix is one line: reset the reader to the position *before* the attempted item parse, so the trailing separator isn't consumed, and drop the last operator from the parsed list so the tree is well-formed:

```fsharp
| Error e when allowTrailingOp ->
    reader.Position <- nextItemPos                   // rewind past failed item
    if parsedOps.Count > 0 then
        parsedOps.RemoveAt(parsedOps.Count - 1)      // drop trailing op
    preturn (items, ValueSome(mergeWithError e accumulatedErr)) reader
```

The second is *zero-progress infinite recursion*. If the nary loop exits at the same position it entered (no items parsed, no separators consumed), and the enclosing `parseRhsInternal` is called again with the same minimum binding power, the same dispatch picks the same `InfixNary` operator and the same zero-progress loop runs again. This is reachable from virtual tokens: virtual separators are zero-width, and a virtual `;` that fires before a closing `]` in a list literal can drive the loop. Guard with an entry-position check:

```fsharp
let entryPos = reader.Position
match loopNary items parsedOps ValueNone with
| Ok(items, errOpt) ->
    let result = completeNary items parsedOps
    if reader.Position = entryPos then
        // No progress was made (e.g. trailing separator with no following item).
        // Return directly to prevent infinite recursion when virtual tokens
        // can repeatedly fire at the same position.
        preturn (PrattParsed.withError result errOpt) reader
    else
        match parseRhsInternal ... with ...
```

Commit `278ca3a Fix rollback and infinite loop protection in InfixNary operator parsing` combines both. Neither shows up on the tests a library ships with — simple separated lists don't do this. They showed up after the F# corpus started exercising combinations the tests hadn't imagined: trailing `;` in a list at the end of a module, virtual `;` firing on a line the filter wasn't sure about, `fbf09bf Fix infinite loop detection in OperatorParser InfixNary` one commit prior for the same family of problems. The shape is always the same: a zero-width token in a loop that doesn't enforce progress.

## `(+)`, `( * )`, `(*)`

F# lets you refer to an operator as a function value by parenthesising it. `(+)` is `fun x y -> x + y`. `List.fold (+) 0 [1;2;3]` works. So does `(( * ))`, with spaces because `(*` otherwise opens a block comment (post 2's `(*)` story is the lexer's half of this). And `(*)` — three characters, no spaces — is just the multiplication operator in parens, identical to `( * )` after lexing, once the lexer's `pParenStarOperator` rule has fired.

The parser's job here is to recognise the shape. `500619e Parse point-free operators in parens` inserts `pParenOpExpr` in front of the standard paren-expression rule:

```fsharp
let private pParenOpExpr =
    parser {
        let! l = pLParen
        let! op = OpName.parse
        let! r = pRParen
        return Expr.LongIdentOrOp(LongIdentOrOp.Op(IdentOrOp.ParenOp(l, op, r)))
    }

let pParen =
    choiceL
        [ pParenOpExpr
          pEnclosed pLParen Token.KWRParen ParenKind.Paren
                    OffsideContext.Paren DiagnosticCode.ExpectedRParen pExprOrTypedPat ]
        "pParen"
```

Ordering again matters. `pParenOpExpr` comes first because `OpName.parse` accepts *only* an operator token, so it fails fast on anything else and the fallback parses a normal parenthesised expression. If the order were reversed, `pEnclosed` would commit to parsing an expression inside, and `(+)` would try to parse `+` as an expression and fail. The fallback makes the two shapes mutually compatible without a lookahead pass.

The related `adb41f3 Parse parenthesised operators in active patterns` is the same idea for active-pattern identifiers, which also accept parenthesised operators as binding names.

## A preview: stack-bounded Pratt is post 11

The 0.3 redesign covered *features*. It did not cover stack consumption. The Pratt loop as described above still recurses along the left spine of the expression tree: every infix operator in a chain opens a new stack frame, because the recursive call into `parseLhsInternal` happens before the current frame has a chance to unwind. `1 + 2 + 3 + … + 500` opens 500 stack frames on the way down, unwinds 500 on the way back.

F#'s `prim-types.fs` has expressions with more than 1,000 consecutive operators. Parsing that file on .NET's default stack size — and especially on Fable-compiled JavaScript in a browser, where the stack budget is smaller and much less predictable — meant a stack overflow somewhere in the high hundreds. The fix is a second rewrite, same feature set, but transforming the left-recursion into a loop with an explicit operand stack. It's mechanical once you see it, and tedious to get right because the soft-error merging from the original recursive implementation has to thread through the new loop.

That story is post 11. The operator-parsing story as of XParsec 0.3 sits across the two — this post is *what the parser knows how to parse*, post 11 is *how the parser survives doing it*.

## Anchor commits / files

- `src/XParsec/OperatorParsing.fs` (~960 lines: types, Pratt loop, `Operator` module)
- `src/XParsec.FSharp/ExpressionParsing.fs` (~2,600 lines: `ExprOperatorParser` wires the F# grammar to the combinators)
- `85fa6a7 Rework operator parsing` — the 0.3 split
- `4e8455f Make Operators an interface` — the abstraction point
- `316eab5 Add RHSOperator.InfixNary`, `ea3afec Add InfixNary.allowTrailingOp to RHSOperator`
- `62c8785 Add InfixMapped and LHSTernary operators`
- `ab9853d Add PrefixMapped to operator parsing`
- `278ca3a Fix rollback and infinite loop protection in InfixNary operator parsing`
- `09d9d40 Fix handling of adjacent prefix operators in application`
- `500619e Parse point-free operators in parens`
- `adb41f3 Parse parenthesised operators in active patterns`
- `e060d8b Expose BindingPower in OperatorParsing`

## Takeaway

Four ideas hold the operator parser up.

**Twelve cases, each named for a grammar rule that refused the previous case.** Variadic tuple/sequence/application forced `InfixNary`. Type casts, dot access, and indexing forced `InfixMapped` and `Indexer`. Keyword-led expressions forced `PrefixMapped`. Ternary control flow forced `LHSTernary`. The taxonomy didn't come from theory; it came from the F# spec pushing back on each abstraction in turn.

**The interface split is where state-aware operator lookup hides.** Once `Operators` became an interface, the F# parser could implement lookups that consult `ParseState` on every dispatch, for split operators, context-sensitive keywords, and LHS-only guards. XParsec itself never learns what `|` means; it asks the implementation.

**Prefix-vs-infix ambiguity is resolved by the lexer's token encoding.** `CanBePrefix` is one bit on the `OperatorInfo`, produced by bit-packing in the lexer (post 2). The adjacency rule that distinguishes `f -x` from `f - x` then reduces to a check on neighbouring raw tokens — no dictionaries, no lookahead parsers.

**Progress is a Pratt invariant, not a property of well-behaved inputs.** The zero-width virtual tokens introduced by the lexical filter (post 4) turned several Pratt cases into potential infinite loops. Every recursive descent into `parseLhsInternal` checks advancement; the nary case checks its own entry position. Without those, the parser deadlocks rather than failing cleanly.

This post told the feature story. Post 6 turns to whitespace — the context stack, the offside rule, and the F# spec's promise that carefully indented code parses the same as verbose code with explicit delimiters.
