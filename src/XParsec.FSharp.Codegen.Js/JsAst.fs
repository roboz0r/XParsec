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
//
// Step 2 adds functions: the `Arrow` expression (F# functions emit as nested
// *unary* arrows so currying / partial application are free), and the
// statement-level `If` / `While` / `Return` / `Continue` / `Assign` the
// self-tail-call loop form needs (`while (true)` + param-shadow mutation +
// `continue`, codegen-js-steps Step 2 / plan §"Self tail calls").
//
// Step 3 adds records: the `New` expression (`new R(…)` — `RecordCons` and the
// `RecordClone` copy-update construct positionally), the `Class` statement (a
// record's emitted JS class), and field access reuses the existing `Member`
// node (`r.X`).

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
    /// `NewExpression` — `new callee(arguments…)`. A record literal
    /// (`RecordCons`) / copy-update (`RecordClone`) constructs its emitted class
    /// positionally; the callee is the class-name `Identifier`.
    | New of callee: JsExpr * arguments: JsExpr list * loc: JsLoc voption
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
    /// `ArrowFunctionExpression` — `(p0, …) => body`. An F# function emits as a
    /// chain of *unary* arrows (`(x) => (y) => …`) so currying and partial
    /// application fall out for free, so `parameters` is conventionally a single
    /// name; the list form is kept for a possible later flat-call optimisation.
    | Arrow of parameters: string list * body: JsFnBody * loc: JsLoc voption

/// An arrow function's body: a concise expression (`=> e`) or a brace-delimited
/// statement block (`=> { … }`) — the latter is the self-tail-call loop form.
and [<RequireQualifiedAccess>] JsFnBody =
    | Expr of JsExpr
    | Block of JsStatement list

and [<RequireQualifiedAccess>] JsStatement =
    /// `ExpressionStatement` — an expression evaluated for effect.
    | Expression of JsExpr
    /// `VariableDeclaration` with `kind = "const"` — a top-level `let x = e`
    /// module value (Step 1 binds it as a `const`; mutation / hoisting awaits a
    /// later step). Also the loop form's per-call argument temporaries.
    | Const of name: string * init: JsExpr
    /// `ImportDeclaration` — `import { specifiers… } from "source"`. Carried in the
    /// subset for the runtime-import step; Steps 0–1 emit none.
    | Import of specifiers: string list * source: string
    /// `IfStatement` — statement-position `if (test) { … } else { … }`. Used in
    /// the self-tail-call loop body, where the branches carry `return`/`continue`
    /// (the expression-position conditional stays the `Conditional` ternary).
    /// An empty `alternate` prints without the `else`.
    | If of test: JsExpr * consequent: JsStatement list * alternate: JsStatement list
    /// `WhileStatement` — the `while (true) { … }` self-tail-call trampoline.
    | While of test: JsExpr * body: JsStatement list
    /// `ReturnStatement` — yields a tail value out of a loop-form function body.
    | Return of JsExpr
    /// `ContinueStatement` — re-enters the `while (true)` loop after the self-call
    /// arguments have been written back to the parameter variables.
    | Continue
    /// `AssignmentExpression` (as a statement) — `target = value;`, the
    /// param-shadow mutation a self-tail-call performs before `continue`.
    | Assign of target: string * value: JsExpr
    /// A record's emitted JS class (Step 3) — a compressed stand-in for the
    /// ESTree `ClassDeclaration`/`ClassBody`/`MethodDefinition` tree (same
    /// compression `Const` makes of `VariableDeclaration`). `fields` are the
    /// record's *declaration-order* field names; the printer renders the
    /// canonical positional constructor (`constructor(X, Y) { this.X = X; … }`)
    /// against which `RecordCons` / `RecordClone` build with `new`. Augmentation
    /// members + the structural triple (Step 6) grow this later.
    | Class of name: string * fields: string list

/// `Program` with `sourceType: "module"` (ESM output).
type JsProgram = { Body: JsStatement list }
