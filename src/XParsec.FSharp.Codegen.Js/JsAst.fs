namespace XParsec.FSharp.Codegen.Js

// The ESTree node subset — grown one band at a time alongside the walker
// (codegen-js-steps.md). Step 0a carries exactly the nodes a `printfn "hi"`
// program needs: `Program → ExpressionStatement → CallExpression →
// MemberExpression → Identifier`, plus `Literal` and an `ImportDeclaration`
// placeholder for the first runtime import.
//
// Discriminators + field names are kept ESTree-exact so the DU can be serialised
// to JSON for-free later. `loc` (source positions) is a deferred concern — Step 0b
// threads it for V3 source maps; Step 0a is text only.

/// `Literal` payload. Only the string flavour is reachable in Step 0a.
[<RequireQualifiedAccess>]
type JsLiteral = | String of string

[<RequireQualifiedAccess>]
type JsExpr =
    /// `Identifier` — a bare name (`console`, `log`).
    | Identifier of name: string
    /// `Literal` — a primitive constant.
    | Literal of JsLiteral
    /// `MemberExpression` — `object.property` when `computed = false`,
    /// `object[property]` when `computed = true`.
    | Member of object: JsExpr * property: JsExpr * computed: bool
    /// `CallExpression` — `callee(arguments…)`.
    | Call of callee: JsExpr * arguments: JsExpr list

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
