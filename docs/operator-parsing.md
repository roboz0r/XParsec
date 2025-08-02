---
category: Documentation
categoryindex: 0
index: 6
---

# Operator Precedence Parsing

Parsing mathematical or logical expressions can be complex due to rules like operator precedence (e.g., `*` before `+`) and associativity (e.g., `1 - 2 - 3` means `(1 - 2) - 3`). Manually handling these rules with combinators can be tricky and lead to confusing, left-recursive parsers.

XParsec solves this with a built-in **operator precedence parser** based on the powerful Pratt parsing algorithm. This allows you to simply define your operators and their precedence levels, and XParsec will generate a fast and correct expression parser for you, **without needing to manage the recursion yourself**.

## Building an Expression Parser

The main entry point is the `XParsec.OperatorParsing.Operator` module. It provides the tools to build your expression parser in a few straightforward steps:

1. **Define an AST:** Create an F# type (usually a discriminated union) to represent your expression's structure.
2. **Define a Term Parser:** Create a parser for the most basic units of your expression—the "terms" or "atoms" like numbers or identifiers.
3. **Define an Operator Table:** Create a list of all your operators, specifying their type, precedence, and how they map to your AST.
4. **Create the Expression Parser:** Combine the term and operator table to generate the final parser.

### Defining Operators and Precedence

You define each operator using functions from the `Operator` module. **Precedence** determines the order of operations and is defined by the `Precedence` type, which has cases from `P1` (lowest) to `P30` (highest).

#### Infix Operators (e.g., `+`, `*`)

These appear between two operands.

* `Operator.infixLeftAssoc`: For left-associative operators like `+` and `*`.
* `Operator.infixRightAssoc`: For right-associative operators like `**` (power).

```fsharp
// Defines a left-associative '+' operator that builds an `Add` AST node.
Operator.infixLeftAssoc '+' P1 (pchar '+') (fun lhs rhs -> Add(lhs, rhs))
```

#### Prefix and Postfix Operators (e.g., unary `-`, `!`)

These appear before or after a single operand.

* `Operator.prefix`: For operators like unary `-`.
* `Operator.postfix`: For operators like factorial `!`.

```fsharp
// Defines a prefix '-' operator for negation.
Operator.prefix '-' P5 (pchar '-') (fun expr -> Negate expr)
```

#### Enclosing and Other Operators

* `Operator.enclosedBy`: For grouping constructs like `()` or `[]`. The parser handles the recursion automatically.

* `Operator.ternary`: For operators like C's `?:`.
* `Operator.indexer`: For array/map access like `expr[index]`.

```fsharp
// Defines standard parentheses for grouping. The `id` function means the
// parentheses only affect precedence and don't add a node to the AST.
Operator.enclosedBy '(' ')' P10 (pchar '(') (pchar ')') id
```

---

## Complete Example: A Simple Calculator

Let's build a parser for an arithmetic language with `+`, `-`, `*`, `**` (power), unary `-`, and parentheses.

### 1. Define the AST

First, we model the structure of our expressions.

```fsharp
open XParsec
open XParsec.CharParsers
open XParsec.Combinators
open XParsec.OperatorParsing

type Expr =
    | Number of int
    | Add of Expr * Expr
    | Multiply of Expr * Expr
    | Power of Expr * Expr
    | Negate of Expr
```

### 2. Define the Term (Atom) Parser

Next, we define a parser for the most basic element of our language: a number. This parser knows nothing about operators or parentheses.

```fsharp
// A parser for an integer, wrapped in our `Number` AST case.
let pAtom = pint32 |>> Expr.Number
```

### 3. Define the Operator Table

Now, we create a list of all our operators and their precedences. This is where the magic happens.

```fsharp
// A helper to parse an operator token and skip any trailing whitespace.
let op p = p .>> spaces

let operators =
    [
        // P1: Addition and Subtraction (Left-associative)
        Operator.infixLeftAssoc '+' P1 (op (pchar '+')) (fun l r -> Add(l, r))
        Operator.infixLeftAssoc '-' P1 (op (pchar '-')) (fun l r -> Add(l, Negate r)) // Subtraction as adding a negation

        // P2: Multiplication (Left-associative)
        Operator.infixLeftAssoc '*' P2 (op (pchar '*')) (fun l r -> Multiply(l, r))

        // P3: Exponentiation (Right-associative)
        Operator.infixRightAssoc "**" P3 (op (pstring "**")) (fun l r -> Power(l, r))

        // P4: Unary Negation (Prefix)
        Operator.prefix '-' P4 (op (pchar '-')) (fun expr -> Negate expr)

        // P10: Grouping (Highest precedence)
        // This tells the main parser how to handle parentheses. Using `id` means the
        // parentheses only control precedence and don't add a node to the AST.
        Operator.enclosedBy '(' ')' P10 (op (pchar '(')) (pchar ')') id
    ]
    |> Operator.create // Compile the list into an efficient lookup table.
```

### 4. Create and Run the Final Parser

Finally, we combine the `pAtom` parser and the `operators` table. The `Operator.parser` function does the hard work, returning a complete parser that correctly handles all our defined rules.

```fsharp
// The full expression parser. It handles optional leading whitespace,
// then calls the generated operator parser.
let pExpression =
    spaces >>. (Operator.parser pAtom operators)

let runParser input =
    printfn $"Parsing: '{input}'"
    match run pExpression input with
    | Ok success -> printfn "  Success: %A{success.Parsed}"
    | Error e ->
        let errorMsg = ErrorFormatting.formatStringError input e
        printfn "  Error:\n%s{errorMsg}"

runParser "1 + 2 * 3"
runParser "(1 + 2) * 3"
runParser "-5 ** 2"
runParser "2 ** 3 ** 2"
```

### Expected Output

The generated parser correctly constructs the abstract syntax tree according to the rules of precedence and associativity we defined.

```text
Parsing: '1 + 2 * 3'
  Success: Add (Number 1, Multiply (Number 2, Number 3))
Parsing: '(1 + 2) * 3'
  Success: Multiply (Add (Number 1, Number 2), Number 3)
Parsing: '-5 ** 2'
  Success: Power (Negate (Number 5), Number 2)
Parsing: '2 ** 3 ** 2'
  Success: Power (Number 2, Power (Number 3, Number 2))
```
