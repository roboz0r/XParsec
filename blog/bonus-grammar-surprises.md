# Bonus: Surprises in the F# grammar

Short post. The greatest hits of "wait, *that's* legal syntax?" — each with the commit that made it parse.

## `(*)` is a function identifier

Not a block comment. `let mul = (*)` is a valid `let` binding. The lexer has to prefer the paren-operator-paren reading over the `(*` comment-open.

- Fix: `22a8fc7 Fix lexing "(*)"`

## `>]` is one token, except when it isn't

`[<Measure>] type m` and `[< Measure >]` both close an attribute. But inside `<@ T : measure @>` and in a few measure-type contexts the `>]` is actually `>` followed by `]`. The lexer fuses; the parser has a `SplitRAttrBracket` escape hatch that rewrites the fused token at `StartIndex + 1` when the context demands.

- Fix: `pMeasure` in `ConstantParsing.fs`, `nextNonTriviaTokenImpl` in `ParsingHelpers.fs`

## SRTP default constraints

```fsharp
let inline f (x: ^T) = (^T : (static member Zero : ^T) ())
```

Statically resolved type parameters with default constraints are rare enough that you might go years without seeing one. The parser has to handle them anyway.

- Fix: `f4c2d95 Parse SRTP default constraint`, `65d7336 Parse SRTP member constraints`

## Inline IL intrinsics

```fsharp
let inline addInt (x: int) (y: int) = (# "add" x y : int #)
```

The `(# "opcode" args : resultType #)` form that embeds raw IL instructions inside F# source. Used by `FSharp.Core`'s numeric primitives to emit the actual `add`, `sub`, `mul`, `conv.i4`, etc. opcodes — the language has no other way to say "I literally want this IL instruction." Completely undocumented in the surface-language spec.

- Fix: `f02a8d6 Improve IL intrinsic parsing`, `243d29f Parse expressions containing the dot operator in IL intrinsics`, `8cb80cb Parsing named field patterns, IL intrinsic, backtick identifiers`

## Static optimization conditionals

```fsharp
let inline f (x: 'T) =
    if typeof<'T> = typeof<int> then unbox<int> (box x) + 1
    else failwith "not supported"
    when 'T : int
```

The `when 'T : someType` *trailing* constraint on the body of an `inline` function. This is a separate feature from inline IL — a branch-on-the-type-argument that's resolved statically at each inline site. The compiler picks the matching branch per concrete instantiation and discards the others. Also a `FSharp.Core`-only feature in practice, although it's a different kind of obscure than inline IL.

- Fix: `4cc638b Parse static optimization syntax`

## Measure type aliases

```fsharp
[<Measure>] type N = kg m / s^2
```

Not an alias for a type — an alias for a *measure expression*, which has its own mini-grammar of `*`, `/`, and `^` with negative-integer exponents.

- Fix: `7eeb559 Parse measure type aliases`, `46c8616 Parse negative exponents with fused operator token`

## Or-patterns, as-patterns, and named fields

```fsharp
match x with
| { Field = (0 | 1) as v } -> v
```

Or-patterns inside named-field patterns inside record patterns, with an `as` binding on the inner group. Gets every priority interaction wrong the first time.

- Fix: `e324543 Parse or and as patterns in records`, `7ac1311 Fix nested named fields in record patterns`

## Light-mode record expressions

You can write `{ X = 1 \n Y = 2 }` (newline-delimited, no semicolons) in light mode but not verbose mode. The parser has to know which mode it's in, and what counts as "the end" of a record field's value.

- Fix: `94875a0 Light mode for record expressions and patterns`, `e4e11fd Allow trailing semicolons in record expressions`

## `fixed`

A keyword. For pinning managed pointers. Almost no F# code uses it; it's in the spec anyway.

- Fix: `0e86188 Support fixed keyword`

## Anonymous records

```fsharp
let r = {| Name = "a"; Age = 1 |}
```

Their own delimiter shape, their own subtype of record expression, their own interaction with the offside rule (`{|` is *not* the same `Brace` context as `{`).

- Fix: `4f30b3a Parse anonymous records`, `99c8c1e Fix anon record at isAtomicExprToken`

## Split operator names

```fsharp
let inline (.. // line-ending comment
               ..) start step finish = 0
```

`(.. ..)` is the point-free name of the three-argument range-with-step operator (`start .. step .. finish`). It is one identifier made of two `..` tokens separated by whitespace. And that whitespace can include trivia — a line-ending comment, a block comment, a newline, any combination. The parser has to accept *trivia inside an operator name* as long as the surrounding parens hold it together. The same applies to more mundane cases like `( + )` with arbitrary whitespace between the parens and the operator. The `RangeOpName` AST node keeps both tokens and the trivia between them so a formatter can round-trip the exact source.

Fittingly, this was the last fixture to close the corpus — `.. ..` in `FSharp.Core/prim-types.fs` was the final thing standing between the parser and a 249/0 report.

- Fix: `RangeOpName.parse` in `PrimitivesParsing.fs`, `Expr.fs`, `AstTraversal.fs` (fixture 373)

## Undented infix operators

```fsharp
let f a b c =
     a
   + b
  |> c
```

Each continuation line's leading operator is *further left* than the previous line. To anyone reading this with a brace-language brain it looks like malformed code. It is, in fact, perfectly legal F#: spec rule §15.1.9 lets an infix operator be offside by up to `(tokenLength + 1)` characters. `+` (length 1) can hang one column left; `|>` (length 2) can hang two columns left. The indentation lines the *values* up, not the operators. Post 6 covers the offside machinery; mentioning it here because visually it's one of the most unexpected things F# allows.

- Fix: `isPermittedUndentation` in `ParsingHelpers.fs` — the `15.1.9 InfixUndent` arm

## Comments inside type parameter names

```fsharp
let f (x: ' (* block comment *)          T) = x
let g (x: ' // line comment
         T) = x
```

`'T` is not an atomic token. The leading `'` is its own token, and the identifier that names the type parameter is a separate token that happens to follow it. Which means trivia — whitespace, block comments, line comments — can appear between the two. Both `f` and `g` above have a single type parameter named `'T`. I have never seen this written in real F# code; the corpus has it.

- Fix: typar parsing in `TypeParsing.fs`

## The canonical definitions of `Option` and `List`

If the rest of this post is a tour of obscure syntax, this one is the headline. Here is how `Option<'T>` and `List<'T>` — the first two types any F# learner encounters — are actually declared in `FSharp.Core/prim-types.fs`:

```fsharp
type Option<'T> =
    | None :       'T option
    | Some : Value:'T -> 'T option

type List<'T> =
    | ([])  :                  'T list
    | ( :: )  : Head: 'T * Tail: 'T list -> 'T list
```

GADT-style per-case return-type signatures. Parenthesized operators as case names. Nothing an F# tutorial ever shows. Everything the standard library relies on. These were, fittingly, among the last files in the corpus to parse cleanly.

- Fix: `369_gadt_du_case_signature.fs`, `370_gadt_operator_case_name.fs` reproductions plus the corresponding grammar work

## Takeaway

Every general-purpose language has a tail of rarely-used syntax that exists because *some* library needs it. A parser built against a handwritten test suite will miss every one of them. A parser run against a real corpus (post 10) will find them on day one.
