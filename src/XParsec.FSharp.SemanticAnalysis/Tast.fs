namespace XParsec.FSharp.SemanticAnalysis

// TAST does NOT preserve trivia, parens, or token layout — tooling consumers
// query the CST for that. TAST exists for consumers that only care about
// semantics (codegen, target plugins).
//
// Each node carries its inferred SemType inline so target lowering doesn't
// need to re-query the side tables.

[<RequireQualifiedAccess>]
type TExpr =
    /// `ty` is carried explicitly so widening to int64/int8/etc. doesn't
    /// require a structural change.
    | Const of value: int * ty: SemType
    /// `binding` is the NodeKey of the *binding site*, not the use site.
    | Var of binding: NodeKey * ty: SemType
    /// `param` is the NodeKey of the parameter pattern.
    | Lambda of param: NodeKey * body: TExpr * ty: SemType
    /// Curried; multi-arg applications nest.
    | App of fn: TExpr * arg: TExpr * ty: SemType
    | Let of binding: NodeKey * value: TExpr * body: TExpr * ty: SemType

[<RequireQualifiedAccess>]
type TDecl =
    | Let of binding: NodeKey * value: TExpr * ty: SemType

type TastFile =
    { /// Source order.
      Decls: TDecl list
      /// Non-empty Errors mean the TAST is best-effort and not safe to emit from.
      Diagnostics: Diagnostic list }
