namespace XParsec.FSharp.Codegen.Js

/// A 0-based source position — V3 source-map coordinates (`Line`, then `Column`
/// counted in UTF-16 code units).
[<Struct>]
type JsLoc = { Line: int; Column: int }

/// One case of an emitted union. `ClassName` is the emitted subclass name
/// (`<Union>_<Case>`); `Tag` is the declaration-order integer the base-class `tag`
/// field carries; `Fields` are the case's declaration-order field names (positional
/// fields synthesised as `Item` / `Item1` / `Item2` / …, named fields verbatim).
type JsUnionCaseDecl =
    {
        CaseName: string
        ClassName: string
        Tag: int
        Fields: string list
    }

/// `Literal` payload. `Number` and `BigInt` carry the *already-formatted* numeric
/// text (round-trippable), so the printer never re-formats and can't drift from
/// the value the walker resolved.
[<RequireQualifiedAccess>]
type JsLiteral =
    | String of string
    /// A `number` literal — the formatted text of an int32 / byte / float / float32 `Const`.
    | Number of raw: string
    /// A `bigint` literal — the digits of an int64 `Const`, printed with the trailing `n`.
    | BigInt of digits: string
    | Boolean of bool

/// One piece of a `Raw` template expansion: a verbatim chunk of the `$N` template
/// string, or a substituted operand expression (which the printer wraps in
/// parentheses for the precedence-by-construction guarantee).
[<RequireQualifiedAccess>]
type JsRawSeg =
    | Verbatim of string
    | Hole of JsExpr

and [<RequireQualifiedAccess>] JsExpr =
    | Identifier of name: string * loc: JsLoc voption
    | Literal of JsLiteral * loc: JsLoc voption
    /// `object.property` when `computed = false`, `object[property]` when `computed = true`.
    | Member of object: JsExpr * property: JsExpr * computed: bool * loc: JsLoc voption
    | Call of callee: JsExpr * arguments: JsExpr list * loc: JsLoc voption
    | New of callee: JsExpr * arguments: JsExpr list * loc: JsLoc voption
    /// `test ? consequent : alternate`. The printer parenthesises the whole node.
    | Conditional of test: JsExpr * consequent: JsExpr * alternate: JsExpr * loc: JsLoc voption
    /// `(a, b, …)`: evaluate each in order, yield the last.
    | Sequence of expressions: JsExpr list * loc: JsLoc voption
    /// `[a, b, …]`. A tuple is a JS array; a tuple pattern indexes it positionally.
    | Array of elements: JsExpr list * loc: JsLoc voption
    /// A `$N`-template `ILIntrinsic` expanded to verbatim text + parenthesised operand
    /// holes; the whole wrapped in parentheses by the printer. Not an ESTree node.
    | Raw of segments: JsRawSeg list * loc: JsLoc voption
    /// `(p0, …) => body`. F# functions emit as chains of *unary* arrows so currying
    /// and partial application fall out for free.
    | Arrow of parameters: string list * body: JsFnBody * loc: JsLoc voption
    /// `left <op> right` — the whole parenthesised (universal-parenthesization), so no
    /// precedence table is needed.
    | Binary of operator: string * left: JsExpr * right: JsExpr * loc: JsLoc voption
    /// `left <op> right` for the short-circuiting `&&` / `||`. Kept distinct from
    /// `Binary` for ESTree exactness; the printer treats them identically.
    | Logical of operator: string * left: JsExpr * right: JsExpr * loc: JsLoc voption

/// An arrow function's body: a concise expression (`=> e`) or a brace-delimited
/// statement block (`=> { … }`) — the latter is the self-tail-call loop form.
and [<RequireQualifiedAccess>] JsFnBody =
    | Expr of JsExpr
    | Block of JsStatement list

and [<RequireQualifiedAccess>] JsStatement =
    /// An expression evaluated for effect.
    | Expression of JsExpr
    | Const of name: string * init: JsExpr
    /// `export const <name> = <init>;` — top-level binding in library compile mode.
    | Export of name: string * init: JsExpr
    | Import of specifiers: string list * source: string
    /// `if (test) { … } else { … }`. An empty alternate prints without the `else`.
    | If of test: JsExpr * consequent: JsStatement list * alternate: JsStatement list
    /// `while (test) { … }` — the self-tail-call trampoline.
    | While of test: JsExpr * body: JsStatement list
    | Return of JsExpr
    | Continue
    /// `target = value;` — param-shadow mutation in a self-tail-call.
    | Assign of target: string * value: JsExpr
    /// A record's emitted JS class: one positional constructor storing each
    /// declaration-order field into the like-named property.
    | Class of name: string * fields: string list
    /// A union's emitted JS classes: a `baseName` base class (`tag` + `cases()`) plus
    /// one `extends`-subclass per case carrying its named fields after `super(tag)`.
    | Union of baseName: string * cases: JsUnionCaseDecl list
    /// A bare lexical block `{ … }` — scopes a match arm's pattern bindings so two
    /// arms binding the same name don't collide as sibling `const`s.
    | Block of body: JsStatement list
    | Throw of JsExpr

/// `Program` with `sourceType: "module"` (ESM output).
type JsProgram = { Body: JsStatement list }
