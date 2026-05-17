namespace XParsec.FSharp.SemanticAnalysis

// TAST does NOT preserve trivia, parens, or token layout — tooling consumers
// query the CST for that. TAST exists for consumers that only care about
// semantics (codegen, target plugins).
//
// Each node carries its inferred SemType inline so target lowering doesn't
// need to re-query the side tables.

/// Literal-value payload. TODO: extend with Float / String / Char / Unit as
/// the supported subset grows.
[<RequireQualifiedAccess>]
type TConstValue =
    | Int of int
    | Bool of bool

[<RequireQualifiedAccess>]
type TExpr =
    | Const of value: TConstValue * ty: SemType
    /// `binding` is the NodeKey of the *binding site*, not the use site.
    | Var of binding: NodeKey * ty: SemType
    /// Symbol resolved through IExternalSymbolProvider. Carries the compiled
    /// name so target plugins can dispatch (`op_Addition` -> CIL `add` on
    /// .NET, native `+` on Rust, etc. — see [[project_inline_il_target_specific]]).
    | External of compiledName: string * ty: SemType
    /// `param` is the NodeKey of the parameter pattern.
    | Lambda of param: NodeKey * body: TExpr * ty: SemType
    /// Curried; multi-arg applications nest.
    | App of fn: TExpr * arg: TExpr * ty: SemType
    | Let of binding: NodeKey * value: TExpr * body: TExpr * ty: SemType
    | IfThenElse of cond: TExpr * thenExpr: TExpr * elseExpr: TExpr * ty: SemType

[<RequireQualifiedAccess>]
type TDecl =
    | Let of binding: NodeKey * value: TExpr * ty: SemType
    /// Top-level expression (script fragments parse as a module with one
    /// Expression element).
    | Expression of expr: TExpr * ty: SemType

type TastFile =
    {
        /// Source order.
        Decls: TDecl list
        /// Non-empty Errors mean the TAST is best-effort and not safe to emit from.
        Diagnostics: Diagnostic list
    }
