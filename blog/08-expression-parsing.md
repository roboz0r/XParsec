# 8. Expression parsing — where the operator engine meets reality

*"The Pratt engine was the easy part. This post is the workload it had to carry."*

## The engine was never the point

Post 5 built the operator engine: precedence climbing, binding power, the
twelve shapes an operator can take. It was satisfying to write — a small, dense
loop driven by a data table instead of a tower of grammar rules. But an engine
in isolation proves nothing. The interesting question is what happens when you
point it at a real language.

`ExpressionParsing.fs` is the answer, and it's the largest file in the parser by
a wide margin — around 2,600 lines. In F#, "expression" means most of the
language: operators, function application, lambdas, `let`/`in`, `match`,
computation expressions, comprehensions, type annotations, and a long tail of
keyword-led special forms. Almost every shape that *doesn't* fit the textbook
infix/prefix/postfix model lives here. This is the post where the 12-case
taxonomy from post 5 actually fires, and where I found out which of those cases
were load-bearing and which were vanity.

What follows is a tour of the parts that fought back.

## The PrefixMapped zoo

A whole family of F#'s keywords introduce an expression: `if`, `match`,
`match!`, `function`, `fun`, `try`, `while`, `for`, `let`/`let!`, `use`/`use!`,
`do`/`do!`, `return`/`return!`, and `yield`/`yield!`. Each one has a fully
structured body grammar that has nothing to do with Pratt descent — `if` parses
a condition, a `then` branch, and an optional `else`; `match` parses a scrutinee
and a list of clauses. None of that is operator-shaped. (Keyword-like *prefix
operators* such as `lazy`, `assert`, and `upcast`/`downcast` are a different
animal — they take an operand and recurse straight back into the operator loop,
so they stay in the ordinary operator table rather than this route table.)

And yet each of these keywords still has to *bind* into the surrounding
expression at a defined precedence. `let x = if c then a else b in x` has to
know where the `if` expression ends and the enclosing `let` body resumes. That's
exactly the question the operator engine answers — so the keyword-led forms get
folded into the operator table as `PrefixMapped` entries.

The mechanism is a flat dispatch array, `kwPrefixRoutes` — a struct-tuple table
pairing each keyword token with the handler that parses its body:

```fsharp
let kwPrefixRoutes: struct (Token * (SyntaxToken -> Parser<_, _, _, _, _>))[] =
    [|
        struct (Token.KWIf, kwPrefixNoConsume pIfExpr completeFor)
        struct (Token.KWMatch, kwPrefixNoConsume pMatchExpr completeFor)
        struct (Token.KWMatchBang, kwPrefixNoConsume pMatchExpr completeFor)
        struct (Token.KWFunction, kwPrefixNoConsume pFunctionExpr completeFor)
        struct (Token.KWFun, kwPrefixNoConsume pFunExpr completeFor)
        struct (Token.KWTry, kwPrefixNoConsume pTryExpr completeFor)
        struct (Token.KWWhile, kwPrefixNoConsume pWhileExpr completeFor)
        struct (Token.KWFor, kwPrefixNoConsume pForExpr completeFor)
        struct (Token.KWLet, kwPrefixNoConsume pLetOrUseBody completeKeyword)
        struct (Token.KWLetBang, kwPrefixNoConsume pLetOrUseBody completeKeyword)
        struct (Token.KWUse, kwPrefixNoConsume pLetOrUseBody completeKeyword)
        struct (Token.KWUseBang, kwPrefixNoConsume pLetOrUseBody completeKeyword)
        struct (Token.KWDo, kwPrefixConsume pYieldReturnDoBody completeKeyword)
        struct (Token.KWDoBang, kwPrefixConsume pYieldReturnDoBody completeKeyword)
        struct (Token.KWReturn, kwPrefixConsume pYieldReturnDoBody completeKeyword)
        struct (Token.KWReturnBang, kwPrefixConsume pYieldReturnDoBody completeKeyword)
        struct (Token.KWYield, kwPrefixConsume pYieldReturnDoBody completeKeyword)
        struct (Token.KWYieldBang, kwPrefixConsume pYieldReturnDoBody completeKeyword)
    |]
```

Each entry ties a keyword token to its body parser, plus a "completion" function
(`completeFor` or `completeKeyword`) that wraps the parsed body back into the
operator world. Note the two flavors of route: `kwPrefixNoConsume` leaves the
keyword token for the body parser to read (it needs the token's position for the
offside context), while `kwPrefixConsume` eats it first. The `do`/`return`/`yield`
family even shares a single body parser, `pYieldReturnDoBody`, because their
grammars are identical bar the leading keyword.

Dispatch is a tight linear scan — `lhsParser` peeks the next token and walks the
array:

```fsharp
while handler.IsNone && i < kwPrefixRoutes.Length do
    let struct (tok, h) = kwPrefixRoutes.[i]
    if token.Token = tok then handler <- ValueSome h
    i <- i + 1

match handler with
| ValueSome h -> return! h token         // a keyword-led form
| ValueNone   -> return! pOperatorPrefix token   // fall through to ordinary prefix ops
```

The struct-tuple array plus linear scan is deliberate — it's the same
sparse-dispatch pattern used elsewhere in the parser, and on a table this small
it beats a hashed lookup. The precedence each keyword binds at lives in the
operator table the loop consults; the route table's only job is to pick which
body grammar runs, falling through to `pOperatorPrefix` for everything that
isn't a keyword.

This is `PrefixMapped` earning its place in the taxonomy. When I wrote post 5 I
wasn't sure the case justified its own slot — it looked like it might be a
special case of plain prefix. It isn't. A prefix operator consumes a token and
then recurses back into the operator loop for its operand. These keywords
consume a token and then run an *entirely different grammar* for their body,
only rejoining the operator world at the bind point. That's a genuinely
different shape, and the zoo is what proves it.

## Application is an operator with no token

`f x y` is function application. There is no operator between `f` and `x` — just
whitespace, or a comment, or a line break. Juxtaposition is the operator.

This is the most quietly demanding thing the engine does. Every other operator
has a token to consume; application has nothing. So `pApplication` synthesizes
one: when it sees two expressions adjacent with only trivia between them, it
emits a virtual `VirtualApp` token and feeds it to the operator loop as though
the source had contained an application operator all along. `InfixNary` —
designed in post 5 for flattened n-ary sequences — turns out to have been built
for exactly this. `f a b c` is one n-ary application node, not three nested
binary ones.

The hard part isn't recognizing juxtaposition; it's knowing when to *stop*. In a
layout-sensitive language you can't just keep gobbling adjacent expressions —
the offside rule decides whether the next token is an argument to `f` or a new
statement at the enclosing indentation. The easy case is a column check:
`indent > ctxIndent` means "still inside the application." The hard case is
arguments nested inside paren groups, where the relevant column is buried under
a `SeqBlock`/`Paren` pair and you have to walk past it to find the real offside
reference (this is the territory of rule 15.1.10.4 in the spec). The offside
rule is a recurring antagonist across these parsing posts; application is where
it first draws blood.

## Virtual `;` and the SeqBlock

Newline-separated expressions inside a block form a sequence:

```fsharp
do
    printfn "a"
    printfn "b"
```

There's no `;` in the source, but semantically there is one. So `pSepVirt`
synthesizes a virtual `;`, and the sequence becomes an `InfixNary` fold over the
SeqBlock — flat, not right-nested, which keeps the AST shallow and the later
passes simple.

Two preconditions guard the virtual separator, and both exist for the same
reason: progress. First, the next token must `canStartExpression` — there has to
be something for the separator to join to. Second, the indentation must equal
the context indent — a token further left belongs to an enclosing block, not
this sequence. Drop either guard and you can emit a zero-width virtual `;` in
front of a token that can't start an expression, at which point the `InfixNary`
loop consumes the virtual token, makes no progress on the operand, and loops
forever.

Post 5 called out "progress is a Pratt invariant" — every iteration of the
operator loop must consume real input or the loop is unbounded. Virtual tokens
are the obvious way to violate that invariant, because they cost nothing to
emit. `pSepVirt`'s two guards are the corollary: a virtual token is only safe
when you can prove the operand after it will make progress.

## `pLetOrUseBody` and the stack ceiling

Sequential bindings are the common case in real F#:

```fsharp
let x = 1
let y = 2
let z = x + y
z
```

The natural way to parse this is recursively: parse one `let`, then recurse to
parse the body, which is itself another `let`. It reads beautifully and it
blows the stack.

Each `let` in that chain threads roughly thirty frames through the Pratt loop →
SeqBlock → `pLetOrUseBody` path before it reaches the next `let`. A hundred
sequential bindings — an entirely ordinary module — is three thousand frames
deep, and a few hundred is enough to overflow the default .NET stack. The input
isn't adversarial; it's just *normal code at scale*, which is the most
embarrassing way to fall over.

So `pLetOrUseBody` collects sequential bindings iteratively — a `while` loop
accumulating into a `ResizeArray` — rather than recursively. The depth of the
binding list no longer maps to stack depth; the loop runs in constant stack
regardless of how many `let`s it sees. This is the same disease, and the same
cure, that shows up in AST traversal later; the full stack story is post 15.
It's worth saying plainly: a combinator parser cannot be naive about recursion
once the input is real. This file has more carefully iterative loops than any
other in the codebase, and every one of them is a place where the elegant
recursive version overflowed.

## The dot family is one `InfixMapped`

`a.b`, `a.[i]`, `a.(op)`, and — surprisingly — `a.123` are all the same
operator. They sit at `PrecedenceLevel.Dot`, the tightest-binding level; they
all dispatch through `parseDotRhs` to decide what follows the dot; and they all
fold through `completeDot` into the AST. One `InfixMapped` entry, four
right-hand-side shapes:

- `.Name` — ordinary member access.
- `.[index]` — indexer access.
- `.(op)` — dynamic/operator member access.
- `.123` — positional field access on a DU, e.g. `x.123`.

That last one is undocumented and I didn't know it existed until the corpus test
threw it at me. `FSharp.Core` uses positional DU field access internally, and
the parser has to accept `.` followed by an integer literal as a field selector,
not as a malformed floating-point number. It's a one-line case in `parseDotRhs`
and it's the difference between the corpus closing and not — more on that in
post 13. `InfixMapped` justified itself in post 5 with the dot; the dot
justified `InfixMapped` right back here.

## `f<int>` and undoing the lexer's helpfulness

Type application looks like generic instantiation: `f<int>`, `Seq.map<int,
string>`. Parsing it means deciding whether a `<` is "less-than" or "open a type
argument list," and that decision is genuinely ambiguous in F# — `a < b` and
`a<b>` start identically.

The disambiguation has two conditions. The `<` must be *adjacent* — no trivia
between `f` and `<` in the raw token stream, because `f < int` (with spaces) is
a comparison, not an instantiation. And it must be *spelled* `<`: the lexer
fuses `<<` into a single token, and `<<` shares the same numeric operator value
as some other operators, so checking the token's tag isn't enough. The parser
has to recover the actual source spelling.

That's where it gets interesting relative to post 2. The lexer practices
"selective semantic erasure" — it throws away spelling information that the
grammar doesn't need, keeping tokens lean. Type application is one of the few
places that needs the spelling back, so `pTypeApplication` calls `tokenStringIs`
to retrieve the original source span and confirm the token really is a single
`<`. It's a small, deliberate reach back through the abstraction — the erasure
was the right default, and this is the documented exception that proves the
parser knows when it can't afford it.

## `else if` and the elif collapse

F# treats `else if` and `elif` as equivalent — usually. `ElifBranches` collects
a chain of conditions, and the subtle part is deciding when `else if` is sugar
for `elif` versus when it's a nested `if` living inside the `else` body. The
distinction is layout:

```fsharp
// elif chain — flat
if a then 1
else if b then 2
else 3

// nested if inside else — the if is indented past else
if a then 1
else
    if b then 2 else 3
```

The collapse to `elif` happens only when the `if` is on the same line as the
`else`, or undentated to align with it. When the `if` sits strictly to the right
of `else`, the user has deliberately nested, and the parser preserves that
structure rather than flattening it.

To make the chain offside-correct, its reference column is `min(else_col,
if_col)` — the leftmost of the pair — anchored at `+1` so a multi-line condition
can wrap onto the following line without tripping the offside check. This is a
small rule, but it's the kind of thing that's invisible when it works and
generates a baffling parse error when it doesn't. It earned its own test cases.

## What's next

Expressions are most of the language, but not all of it. Patterns have their own
operator grammar — `::`, `&`, `|`, `as`, tuples, active patterns — that mirrors
the expression engine closely enough to reuse the Pratt machinery and diverges
in just enough places to be its own post. That's post 9.

## Closing thought

The expression parser is where every escape hatch the operator engine shipped
with got tested against reality. `PrefixMapped` looked speculative until the
keyword zoo needed it. `InfixMapped` looked like overkill until the dot family
arrived with three shapes and a secret fourth. `InfixNary` was quietly designed
for application and sequencing all along. And the whole file is a standing
reminder that the elegant recursive formulation — the one that reads like the
grammar — is the one that overflows the stack on a perfectly ordinary input. The
engine was the clean idea. This was the part where it had to survive contact
with F#.
