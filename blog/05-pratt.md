# 5. Pratt parsing for F#'s operator zoo

A textbook operator parser expects operators to come in a small handful of shapes: infix, prefix, postfix, maybe ternary. Each has a precedence, each has an associativity, and the hard work is deciding which one binds tighter.

F# doesn't fit this model. Users can define their own operators, and the lexer assigns them precedence from their leading characters: `**>` is high, `||>` is low, anything starting with `+` or `-` is Additive, anything starting with `<`, `>`, or `=` is Comparison. Some of them are prefix-only, and some, like `-`, behave as prefix or infix depending on tokens around them.

That's only the operators that look like operators. F#'s grammar also threads a handful of things through the precedence table that few would call operators at all like `let`, `if`, and `fun`. Function application is juxtaposition: two expressions in a row with whitespace between, no token of its own, just adjacency. And `,` is variadic: `e1, e2, e3, e4` is *one* tuple expression, not three nested right-associative ones.

## Pratt, in brief

[Pratt parsing](./Vaughan.Pratt.TDOP.pdf) assigns every operator a *left binding power* and a *right binding power*, then parses expressions by asking at each step: *does the operator I'm about to consume bind tighter than the one that called me?* If yes, descend; if no, return. Associativity falls out of the gap between the two powers: left-associative operators have `LBP = N`, `RBP = N + 1`; right-associative operators flip it; non-associative operators have `LBP = RBP = N` and reject chaining.

XParsec encodes this directly:

```fsharp
[<Measure>] type bp

module BindingPower =
    let leftAssocRhs  (basePower: byte<bp>) = basePower + 1uy<bp>
    let rightAssocLhs (basePower: byte<bp>) = basePower + 1uy<bp>
```

A unit of measure on a byte is what the internals to the parser see. To most consumers, precedences live in a 30-level enum (`P1`..`P30`) that maps to odd numbers 1, 3, 5, …, 59 (odd so the adjacent even slot is free to hold the `+1` offset that encodes associativity).

This isn't quite how Pratt describes his system in the original paper. Pratt uses even numbers and takes one away for associativity and asserts that left-associativity should dominate in the case of a tie. My initial inspiration to write this algorithm came from [another blog post](https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html) and non-associative operators came from a request in the [F# Discord](https://discord.gg/fsharp-196693847965696000).

## What the v0.2 model got wrong

The shape that XParsec v0.2 shipped with was, in retrospect, the textbook one: an operator was uniformly binary or unary prefix, with a single `BindingPower` per token and a `complete` function of a fixed arity. A `choice` over a handful of cases covered `+`, `*`, prefix `-`, postfix `!`, parenthesisation. For most grammars that is more than enough.

F# broke it on four axes, in order of appearance:

- **Variadic operators**. A tuple `a, b, c` is a single `Tuple` node with three elements, not `Tuple(a, Tuple(b, c))`. Sequential composition `e1; e2; e3` is right-associative infix in the F# language specification, but in practice it makes sense to also optimize this into a linear structure.
- **Mixed-arity operators whose RHS isn't an expression**. `x :> SomeType` is infix in shape but its RHS is a `Type`, not an `Expr`. `x.Name` is infix in shape but its RHS is an identifier, and `x.[0]` is infix in shape but its RHS is a whole bracketed index. The v0.2 API could not express "operator at precedence N whose right operand is parsed by a different parser and folded into the result".
- **Operators that bring their own open/close**. `arr.[i]` and `f(x)` are postfix in shape but they enclose content. `f<int>` is postfix but closes on `>` which may itself be fused with another `>` (e.g. `Map<int,Option<string>>`). A postfix case in v0.2 had no room for a close parser.
- **Control flow that wants to live in the operator table**. `if e1 then e2 else e3`, `fun x -> body`, `let x = e1 in e2`, `match e with | …` all sit at well-defined precedence levels in the F# spec and all should bind to the surrounding expression exactly the way a prefix operator does. But their bodies are not "parse another expression at power N"; they are fixed syntactic constructs that control their own structure.

Any one of these on its own could have been papered over with a special-case parser. Together, they indicated the abstraction wasn't pulling its weight. `85fa6a7 Rework operator parsing` is where the split happened; the rest of XParsec v0.3's operator additions are each named after the F# construct that forced them.

## The operator taxonomy after v0.3

The operator parser's central types split cleanly into two halves. Everything that can appear on the LHS of an expression is an `LHSOperator`; everything that extends an existing expression on the RHS is an `RHSOperator`. The `'Aux` parameter visible in both is the escape hatch: when a completion function takes something other than an `'Expr`, the something-else is an `'Aux`. `ExprAux` in `ExpressionParsing.fs` is the corresponding F# union, with cases for qualified identifiers, dot-indices, type casts, type applications, range endpoints, high-precedence argument lists, wrapped keyword expressions, and slice markers.

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

The cases pair to specific F# constructs:

- **`InfixLeft` / `InfixRight` / `InfixNonAssociative`**. The infix cases, with the associativity encoded in the gap between `leftPower` and `rightPower`. These cover `+`, `*`, `|>`, `<|`, `&&`, `||`, `::`, `@`, `**`, custom user operators.
- **`InfixNary`**. Sequential composition (`;`), tuple construction (`,`), and, importantly, function application. All three want a flat list of operands, not a tree. `allowTrailingOp: bool` lets `InfixNary` instances silently accept a trailing separator (`[1; 2; 3;]`); `,` sets it to false because a trailing comma is an error in F# tuples. Commit `316eab5 Add RHSOperator.InfixNary`, refined by `68bef68 Add InfixNary.allowTrailingOp to RHSOperator`.
- **`InfixMapped`**. `x :> Type`, `x.Name`, `e :? Type`: binary in shape, RHS parsed by a non-Pratt parser, result folded via a 3-arg completion. The same mechanism handles `x.[i]` and `arr[i]` indexed lookup (via `HighIndexApplication` at `ExpressionParsing.fs:840`), `f(x)` high-precedence application, and F#'s range operator (`..`), whose RHS can be either an expression or a slice terminator. Commit `62c8785 Add InfixMapped and LHSTernary operators`.
- **`Indexer`**. A postfix shape with an opener, an inner parser, and a closer. The typical fit for `arr[i]` in most languages. XParsec exposes it for grammars that want exactly that. F# doesn't reach for it: its indexing flows through `InfixMapped` because in F#, `arr[i]` could be an array indexer, or it could be the application of the list `[i]` to the function `arr`. Furthermore, arbitrary expressions are permitted in place of `i`.
- **`Postfix`**. The natural primitive for `x!` in a language where `!` is a factorial, or `x++` in a C-style increment. F# has nothing of this shape at the expression level, so `Postfix` is one of XParsec's general-purpose primitives that the F# grammar happens not to need.
- **`Prefix`**. Plain prefix operators (`-x`, `!x`, `&x`, `~~~x`).
- **`Enclosed`**. Parenthesised subexpressions `(e)`, `[e]`, `{e}`. Separate from `Prefix` because the close needs to be parsed and threaded through the completion. F# and most other programming languages treat parentheses as atom tokens but I provided it in the operator parser to facilitate parsing grammars with very simple rules like a calculator input.
- **`Ternary` / `LHSTernary`**. RHS ternary is the pattern for `cond ? then : else` in C-style languages. `LHSTernary`: a single LHS-side prefix-with-delimiter case that captures `if <cond> then <body>`, `while <cond> do <body>`, and `for <ident> in <seq> do <body>` without any of them needing a hand-rolled parser. While `LHSTernary` looked like a perfect fit for F#, the chaining rules with `elif` and complex whitespace rules ultimately forced me to hand them over to `PrefixMapped` instead.
- **`PrefixMapped`**. This did the most work. A prefix-shaped operator whose RHS is fully controlled by a custom parser rather than by the enclosing Pratt loop's expression parser. This is how `if`, `match`, `fun`, `function`, `try`, `while`, `for`, `let`, `use`, `do`, `yield`, `return`, and their bang variants (`let!`) where applicable, all enter the operator table. The `kwPrefixRoutes` lookup table in `ExpressionParsing.fs:1789-1809`. Each of those keywords has a fully structured body grammar that has its own rules, particularly when whitespace sensitivity gets involved, but they still need to bind to the surrounding expression at a specific precedence. `PrefixMapped` provides the binding without pretending to own the body.

Before `PrefixMapped`, the F# parser had a parallel track for keyword-led expressions that sat alongside the operator parser and duplicated the "what's the precedence of this thing relative to its surroundings?" logic. After `ab9853d Add PrefixMapped to operator parsing`, `if`, `match`, `fun`, and the rest all live in the same table as `+` and `*`, and the Pratt loop treats them the same.

## One interface, one lookup point

The v0.2 API handed operators to the parser as a concrete data structure. That made the lookup cheap but gave the caller no opportunity to consult parse state when deciding what an operator *means*. `4e8455f Make Operators an interface` turned the handoff into an abstract trait:

```fsharp
type Operators<'Op, 'Aux, 'Expr, 'T, 'State, 'Input, 'InputSlice ...> =
    abstract LhsParser: Parser<LHSOperator<...>, ...>
    abstract RhsParser: Parser<RHSOperator<...>, ...>
    abstract OpComparer: IEqualityComparer<'Op>
```

F# needs this because it allows arbitrary custom operators. There's no way to predefine all the token kinds up front for a language that allows `><+-*/=~%.&|@^!?:` as an operator, or uses no token at all, just adjacency, as function application. This was further complicated by my choice to pack the token index into the type. Resolving this requires reading from the token and `ParseState` at parse-time, which the lookup API couldn't do.

Splitting `LhsParser`, `RhsParser`, and `OpComparer` behind an interface lets `XParsec.FSharp` provide its own implementation that consults `ParseState` on every lookup:

```fsharp
interface Operators<SyntaxToken, ExprAux, Expr<SyntaxToken>, ...> with
    member _.LhsParser = lhsParser
    member _.RhsParser = rhsParser
    member _.OpComparer = opComparer
```

The `rhsParser` implementation is where the state-dependent decisions live. Its dispatch over `handleToken` is responsible for rejecting tokens that look like RHS operators but aren't RHS operators in *this* context.

`OpComparer` is the F#-specific extension point. `2348ae7 Add custom Operators.OpComparer` added the abstract member when the F# parser surfaced a need that default structural equality couldn't satisfy: `PositionedToken` carries position metadata in its bit-packed payload, so two semantically equal operator tokens at different source positions would never compare equal under `=`. `OperatorsCollection` provides `EqualityComparer<'Op>.Default` as a default impl so consumers with simpler tokens don't have to think about it.

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

Prefix-only operators (`Precedence = Prefix`) always start arguments. Dual-use operators only start arguments when the adjacency/separation test passes. This is commit `09d9d40 Fix handling of adjacent prefix operators in application`: before it, `f -x` parsed as the subtraction `f - x`, not what F# programmers actually meant.

The small, unglamorous lesson here is that the lexer's token-encoding work from post 2 was the thing that made this rule tractable. `CanBePrefix` is a single bit off the token's payload, and `OperatorInfo.TryCreate` is a bitwise mask. If the check had been a dictionary lookup it would have run on every application candidate on every file, and the hot path would've been much slower as a result.

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

Here, we catch the error, rewind the state, and return a **success** containing the items parsed so far. The error is attached as a soft/recoverable error, carries back out the stack so that if the parser ultimately fails it can report everywhere the operator parsing fails. Without this, automatic rollback semantics of XParsec meant no useful errors inside the Pratt loop were available for the caller.

The second is *zero-progress infinite recursion*. If the nary loop exits at the same position it entered (no items parsed, no separators consumed), and the enclosing `parseRhsInternal` is called again with the same minimum binding power, the same dispatch picks the same `InfixNary` operator and the same zero-progress loop runs again. This is deadly when combined with virtual tokens: virtual separators are zero-width, and a virtual `;` that fires before a closing `]` in a list literal can drive the loop. Guard with an entry-position check:

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

`fbf09bf Fix infinite loop detection in OperatorParser InfixNary` fixed a related case one commit earlier; `278ca3a Fix rollback and infinite loop protection in InfixNary operator parsing` cleaned up the rollback path. Synthetic test cases didn't discover these. They only showed up after the F# corpus started exercising combinations the tests hadn't imagined: trailing `;` in a list at the end of a module, virtual `;` firing on a line the filter wasn't sure about.

## Stack-bounded Pratt

The v0.3 redesign covered *features*. It did not cover stack consumption. The Pratt loop as described above still recurses along the left and right sides of the expression tree: every infix operator in a chain opens a new stack frame, because the recursive call into `parseLhsInternal` happens before the current frame has a chance to unwind. `1 + 2 + 3 + … + 500` opens 500 stack frames on the way down, unwinds 500 on the way back.

F#'s [`prim-types.fs`](https://github.com/dotnet/fsharp/blob/5ea172f853fa8f19bf9bfb0ef75f02c7defe0b4f/src/FSharp.Core/prim-types.fs) has expressions that found the limits of this design spectacularly. Parsing that file on .NET's default stack size meant a stack overflow almost immediately. Not from infinite recursion, just *too much recursion*; about 150 stack frames was enough. The fix is a second rewrite, same feature set, but transforming the RHS path to be tail recursive and refactoring the functions to manage the stack depth consumed by each function call. It's mechanical once you see it, and tedious to get right because the soft-error merging from the original recursive implementation has to thread through the new loop.

The operator-parsing story sits across the two: this post is *what the parser knows how to parse*, post 15 is *how the parser survives doing it*.

## Anchor commits / files

- `src/XParsec/OperatorParsing.fs` (~960 lines: types, Pratt loop, `Operator` module)
- `src/XParsec.FSharp/ExpressionParsing.fs` (~2,600 lines: `ExprOperatorParser` wires the F# grammar to the combinators)
- `85fa6a7 Rework operator parsing` — the 0.3 split
- `4e8455f Make Operators an interface` — the abstraction point
- `316eab5 Add RHSOperator.InfixNary`, `68bef68 Add InfixNary.allowTrailingOp to RHSOperator`
- `62c8785 Add InfixMapped and LHSTernary operators`
- `ab9853d Add PrefixMapped to operator parsing`
- `278ca3a Fix rollback and infinite loop protection in InfixNary operator parsing`
- `09d9d40 Fix handling of adjacent prefix operators in application`
- `e060d8b Expose BindingPower in OperatorParsing`

## Takeaway

Four ideas hold the operator parser up.

**Twelve cases, each named for a grammar rule** Variadic tuple/sequence/application forced `InfixNary`. Type casts, dot access, and indexing forced `InfixMapped`. Keyword-led expressions forced `PrefixMapped`. C-style ternary control flow inspired `Ternary` and `LHSTernary`. The taxonomy didn't come from theory; it came from the F# spec pushing back on each abstraction in turn. `Indexer` and `Postfix` round out the type for different grammars than F#'s.

**The interface split is where state-aware operator lookup hides.** Once `Operators` became an interface, the F# parser could implement lookups that consult `ParseState` on every dispatch, for split operators, context-sensitive keywords, and LHS-only guards.

**Prefix-vs-infix ambiguity costs one bit at parse time, because the lexer encoded it.** `CanBePrefix` is one bit on the `OperatorInfo`, produced by bit-packing in the lexer (post 2). The adjacency rule that distinguishes `f -x` from `f - x` then reduces to a check on neighbouring raw tokens, made simpler by retaining trivia in lexed array.

**Progress is a Pratt invariant, not a property of well-behaved inputs.** The zero-width virtual tokens introduced by the lexical filter (post 4) turned several Pratt cases into potential infinite loops. Every recursive descent into `parseLhsInternal` checks advancement; the n-ary case checks its own entry position. Without those, the parser infinite loops rather than failing.

Post 6 turns back to whitespace: the context stack, the offside rule, and the way to make carefully indented code parse the same as verbose code with explicit delimiters.
