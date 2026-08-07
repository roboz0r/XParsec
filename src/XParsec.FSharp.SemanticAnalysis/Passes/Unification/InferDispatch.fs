namespace XParsec.FSharp.SemanticAnalysis.Passes

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationEngine

module internal UnificationInferDispatch =

    /// The back-edge into top-level expression inference: the per-concern `Infer*.fs`
    /// modules holding its match arms compile first, so each takes it as a parameter.
    type Infer = PassContext -> Expr<SyntaxToken> -> SemType
