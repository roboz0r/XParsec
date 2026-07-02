namespace XParsec.FSharp.SemanticAnalysis.Passes

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationEngine

module internal UnificationInferDispatch =

    /// The back-edge into top-level expression inference. `infer` and its ~50
    /// match arms once formed a single `let rec … and …` clique too large for one
    /// file (Infer.fs, 2100+ lines). The arms now live in per-concern `Infer*.fs`
    /// modules; each receives `infer` as this typed delegate. The clique's *only*
    /// edge that crosses a file boundary is the call back into `infer` — every
    /// other cross-arm call is a forward reference resolved by compile order, so
    /// this single abbreviation is all the indirection the split needs (no mutable
    /// dispatch cell).
    type Infer = PassContext -> Expr<SyntaxToken> -> SemType
