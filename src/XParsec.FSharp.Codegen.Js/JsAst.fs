namespace XParsec.FSharp.Codegen.Js

// The ESTree node subset — grown one band at a time alongside the walker
// (codegen-js-steps.md). Step 0a carries exactly the nodes a `printfn "hi"`
// program needs: `Program → ExpressionStatement → CallExpression →
// MemberExpression → Identifier`, plus `Literal` and an `ImportDeclaration`
// placeholder for the first runtime import.
//
// Discriminators + field names are kept ESTree-exact so the DU can be serialised
// to JSON for-free later. Step 0b threads `loc` (source positions) for V3 source
// maps: every `JsExpr` carries one, resolved from the originating `TExprG` node's
// `'tok` (`SyntaxToken`), `ValueNone` on synthesised sub-nodes with no source
// origin.

/// A 0-based source position — V3 source-map coordinates (`Line`, then `Column`
/// counted in UTF-16 code units). ESTree spells a full `loc` as
/// `{ source, start, end }`; the V3 map only consumes the `start` point, so the
/// node carries exactly that.
[<Struct>]
type JsLoc = { Line: int; Column: int }

/// `Literal` payload. Only the string flavour is reachable in Step 0a.
[<RequireQualifiedAccess>]
type JsLiteral = | String of string

[<RequireQualifiedAccess>]
type JsExpr =
    /// `Identifier` — a bare name (`console`, `log`).
    | Identifier of name: string * loc: JsLoc voption
    /// `Literal` — a primitive constant.
    | Literal of JsLiteral * loc: JsLoc voption
    /// `MemberExpression` — `object.property` when `computed = false`,
    /// `object[property]` when `computed = true`.
    | Member of object: JsExpr * property: JsExpr * computed: bool * loc: JsLoc voption
    /// `CallExpression` — `callee(arguments…)`.
    | Call of callee: JsExpr * arguments: JsExpr list * loc: JsLoc voption

[<RequireQualifiedAccess>]
type JsStatement =
    /// `ExpressionStatement` — an expression evaluated for effect.
    | Expression of JsExpr
    /// `ImportDeclaration` — `import { specifiers… } from "source"`. Carried in the
    /// subset for the runtime-import step; Step 0a emits none (`console` is a
    /// global, so `printfn` needs no import).
    | Import of specifiers: string list * source: string

/// `Program` with `sourceType: "module"` (ESM output).
type JsProgram = { Body: JsStatement list }
