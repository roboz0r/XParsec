namespace XParsec.FSharp.SemanticAnalysis

// The frozen Typed AST. Produced by Freeze.fs from the CST + all side
// tables, then consumed by downstream target plugins (.NET, JS, Rust — none
// of which exist yet).
//
// The TAST does NOT preserve trivia, parens, or token-level layout: that
// information stays on the CST, which tooling consumers query directly.
// The TAST exists for consumers that only care about semantics.
//
// All types here are placeholders. Real shapes will be filled in once
// Freeze.fs has a concrete projection to write — premature commitment here
// just creates churn.

[<RequireQualifiedAccess>]
type TExpr =
    | TPlaceholder

[<RequireQualifiedAccess>]
type TDecl =
    | TPlaceholder

/// The root of the frozen Typed AST for one compilation unit.
type TastFile =
    { /// Declarations in source order.
      Decls: TDecl list
      /// Diagnostics accumulated during semantic analysis. Errors here means
      /// the TAST is best-effort and not safe to emit code from.
      Diagnostics: Diagnostic list }
