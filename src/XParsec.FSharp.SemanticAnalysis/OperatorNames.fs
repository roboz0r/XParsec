namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// The single authority mapping a symbolic operator (token or parenthesised
/// binding head) to its compiled member name (`op_Addition`, `op_LeftShift`, …).
/// Both the desugarer (`Desugar`, bare infix / value-position operators) and the
/// contract extractor (`VesperLibTypeTranslate`, `.fsi` `val` operator heads)
/// resolve through here, so the mapping cannot drift between them — a drifted
/// hand-copy of the text table is exactly what silently dropped the bitwise /
/// shift family off the contract surface (git `4b08661`).
module OperatorNames =

    /// Compiled name for an operator the lexer emits as a *distinct* `Token`.
    /// A bare use site (`a &&& b` → `OpBitwiseAnd`, `+` → `OpAddition`) and the
    /// keyword-encoded operators (`&&`/`||`, which share `OpFamily.OpGeneric` yet
    /// carry the dedicated `OpAmpAmp`/`OpBarBar` tokens) land here. Generic-token
    /// operators — every operator in *parenthesised binding-head* position, e.g.
    /// `(<<<)` / `(~-)` — are NOT covered: they collapse to `OpGeneric` and must
    /// be recovered from source text by `ofParenSymbolic`.
    ///
    /// `::` is deliberately absent: in expression position cons constructs the
    /// list union directly, not an `op_` member, so the desugarer must keep
    /// seeing `ValueNone` for it. A caller that wants the `(::)` *binding head*
    /// (the contract surface) names `op_ColonColon` itself before delegating.
    let ofToken (t: Token) : string voption =
        match t with
        | Token.OpAddition -> ValueSome "op_Addition"
        | Token.OpSubtraction -> ValueSome "op_Subtraction"
        | Token.OpMultiply -> ValueSome "op_Multiply"
        | Token.OpDivision -> ValueSome "op_Division"
        | Token.OpModulus -> ValueSome "op_Modulus"
        | Token.OpLessThan -> ValueSome "op_LessThan"
        | Token.OpGreaterThan -> ValueSome "op_GreaterThan"
        | Token.OpLessThanOrEqual -> ValueSome "op_LessThanOrEqual"
        | Token.OpGreaterThanOrEqual -> ValueSome "op_GreaterThanOrEqual"
        | Token.OpEquality -> ValueSome "op_Equality"
        | Token.OpInequality -> ValueSome "op_Inequality"
        | Token.OpBitwiseAnd -> ValueSome "op_BitwiseAnd"
        | Token.OpBitwiseOr -> ValueSome "op_BitwiseOr"
        | Token.OpExclusiveOr -> ValueSome "op_ExclusiveOr"
        | Token.OpLeftShift -> ValueSome "op_LeftShift"
        | Token.OpRightShift -> ValueSome "op_RightShift"
        | Token.OpAmpAmp -> ValueSome "op_BooleanAnd"
        | Token.OpBarBar -> ValueSome "op_BooleanOr"
        // Pipes and composition are polymorphic FSharp.Core functions, not
        // language intrinsics — they resolve through the same provider path as
        // any other named operator.
        | Token.OpPipeRight -> ValueSome "op_PipeRight"
        | Token.OpPipeLeft -> ValueSome "op_PipeLeft"
        | Token.OpComposeRight -> ValueSome "op_ComposeRight"
        | Token.OpComposeLeft -> ValueSome "op_ComposeLeft"
        | _ -> ValueNone

    /// Compiled name for a parenthesised *symbolic* operator head (`(<<<)`,
    /// `(~-)`, `(|>)`). The dedicated-token operators resolve through `ofToken`;
    /// everything else — the generic-token family a `(...)` head collapses to —
    /// defers to the parser's canonical operator-name function
    /// (`OperatorInfo.getOperatorName`, fed the head's lexed `text`), the one
    /// place the symbol→name table actually lives. `text` is the operator's lexed
    /// source (`&&&`, `~-`, …).
    let ofParenSymbolic (text: string) (tok: SyntaxToken) : string voption =
        match ofToken tok.Token with
        | ValueSome _ as found -> found
        | ValueNone ->
            match OperatorInfo.TryCreate tok.PositionedToken with
            | ValueSome op -> ValueSome(op.GetName text)
            | ValueNone -> ValueNone

    /// Compiled member name for the operator segment of a parenthesised operator
    /// reference (`(+)` in `A.B.(+)`). Only the symbolic-op form has an `op_`
    /// member; the active-pattern / nil / range op-name forms have none, so a
    /// `ValueNone` here is the signal to leave the reference unresolved.
    let ofIdentOp (nameOf: SyntaxToken -> string) (idOp: IdentOrOp<SyntaxToken>) : string voption =
        match idOp with
        | IdentOrOp.ParenOp(opName = OpName.SymbolicOp op) -> ofParenSymbolic (nameOf op) op
        | _ -> ValueNone

    /// The fully-qualified compiled name an operator-form long ident resolves to
    /// (`A.B.(+)` ⇒ `"A.B.op_Addition"`), the single translation shared by the
    /// resolver (`NameResolution`), the typer (`Unification.qualifiedNameOf`), and
    /// the projector (`Freeze.translateIdent`) so the qualified-operator form is
    /// keyed identically everywhere. `ValueNone` for a non-symbolic op segment
    /// (no `op_` member to qualify). The bare-operator form (`(+)` with no
    /// qualifier) resolves through the prelude and never reaches here.
    let qualifiedOpName
        (nameOf: SyntaxToken -> string)
        (li: LongIdent<SyntaxToken>)
        (idOp: IdentOrOp<SyntaxToken>)
        : string voption =
        match ofIdentOp nameOf idOp with
        | ValueSome opName ->
            let prefix = li.Idents |> Seq.map nameOf |> String.concat "."
            ValueSome(if prefix.Length = 0 then opName else prefix + "." + opName)
        | ValueNone -> ValueNone
