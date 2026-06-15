namespace XParsec.FSharp.Codegen.Js

// The ESTree node subset — grown one band at a time alongside the walker
// (codegen-js-steps.md). Step 0a carried exactly the nodes a `printfn "hi"`
// program needs: `Program → ExpressionStatement → CallExpression →
// MemberExpression → Identifier`, plus `Literal`. Step 1 adds the scalar
// literal flavours (number / bigint / boolean), the `Conditional` (ternary) and
// `Sequence` (comma) expressions for control flow, a `const` variable
// declaration statement, and the `Raw` escape hatch that the `ILIntrinsic`
// template path (`$N`-substituted JS expressions) emits — the **(a\*)** MVP from
// codegen-js-steps §"Template → ESTree": universal parenthesization, zero
// JS-grammar knowledge in the compiler.
//
// Discriminators + field names are kept ESTree-exact so the DU can be serialised
// to JSON for-free later (the `Raw` node is the one deliberate non-ESTree
// addition — it carries text, retired per-construct once a bounded template
// parser lands). Every `JsExpr` carries a `loc` (source position) for V3 source
// maps, resolved from the originating `TExprG` node's `'tok` (`SyntaxToken`),
// `ValueNone` on synthesised sub-nodes with no source origin.

/// A 0-based source position — V3 source-map coordinates (`Line`, then `Column`
/// counted in UTF-16 code units). ESTree spells a full `loc` as
/// `{ source, start, end }`; the V3 map only consumes the `start` point, so the
/// node carries exactly that.
[<Struct>]
type JsLoc = { Line: int; Column: int }

/// `Literal` payload. `Number` and `BigInt` carry the *already-formatted* numeric
/// text (round-trippable), so the printer never re-formats and can't drift from
/// the value the walker resolved.
[<RequireQualifiedAccess>]
type JsLiteral =
    /// A double-quoted string (`Const(String)`, and `Const(Char)` as a length-1
    /// string).
    | String of string
    /// A `number` literal — the formatted text of an int32 / byte / float /
    /// float32 `Const` (`"4"`, `"2.5"`, `"NaN"`).
    | Number of raw: string
    /// A `bigint` literal — the digits of an int64 `Const`, printed with the
    /// trailing `n` (`123` → `123n`).
    | BigInt of digits: string
    /// `true` / `false`.
    | Boolean of bool

/// One piece of a `Raw` template expansion: a verbatim chunk of the `$N` template
/// string, or a substituted operand expression (which the printer wraps in
/// parentheses for the **(a\*)** precedence-by-construction guarantee).
[<RequireQualifiedAccess>]
type JsRawSeg =
    | Verbatim of string
    | Hole of JsExpr

and [<RequireQualifiedAccess>] JsExpr =
    /// `Identifier` — a bare name (`console`, `log`, a `let`-bound variable, or
    /// `undefined` for the unit value).
    | Identifier of name: string * loc: JsLoc voption
    /// `Literal` — a primitive constant.
    | Literal of JsLiteral * loc: JsLoc voption
    /// `MemberExpression` — `object.property` when `computed = false`,
    /// `object[property]` when `computed = true`.
    | Member of object: JsExpr * property: JsExpr * computed: bool * loc: JsLoc voption
    /// `CallExpression` — `callee(arguments…)`.
    | Call of callee: JsExpr * arguments: JsExpr list * loc: JsLoc voption
    /// `ConditionalExpression` — `test ? consequent : alternate` (an
    /// `IfThenElse`). The printer parenthesises the whole node.
    | Conditional of test: JsExpr * consequent: JsExpr * alternate: JsExpr * loc: JsLoc voption
    /// `SequenceExpression` — `(a, b, …)`: evaluate each in order, yield the last
    /// (a `Sequential` used in expression position).
    | Sequence of expressions: JsExpr list * loc: JsLoc voption
    /// The **(a\*)** template escape hatch (codegen-js-steps §"Template →
    /// ESTree"): a `$N`-template `ILIntrinsic` expanded to verbatim text +
    /// parenthesised operand holes, the whole wrapped in parentheses by the
    /// printer. Not an ESTree node — retired per-construct when the bounded
    /// template parser (b) lands.
    | Raw of segments: JsRawSeg list * loc: JsLoc voption

[<RequireQualifiedAccess>]
type JsStatement =
    /// `ExpressionStatement` — an expression evaluated for effect.
    | Expression of JsExpr
    /// `VariableDeclaration` with `kind = "const"` — a top-level `let x = e`
    /// module value (Step 1 binds it as a `const`; mutation / hoisting awaits a
    /// later step).
    | Const of name: string * init: JsExpr
    /// `ImportDeclaration` — `import { specifiers… } from "source"`. Carried in the
    /// subset for the runtime-import step; Steps 0–1 emit none.
    | Import of specifiers: string list * source: string

/// `Program` with `sourceType: "module"` (ESM output).
type JsProgram = { Body: JsStatement list }
