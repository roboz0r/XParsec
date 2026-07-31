namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp
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
        | Token.OpAddition -> ValueSome OperatorData.OpAddition
        | Token.OpSubtraction -> ValueSome OperatorData.OpSubtraction
        | Token.OpMultiply -> ValueSome OperatorData.OpMultiply
        | Token.OpDivision -> ValueSome OperatorData.OpDivision
        | Token.OpModulus -> ValueSome OperatorData.OpModulus
        | Token.OpLessThan -> ValueSome OperatorData.OpLessThan
        | Token.OpGreaterThan -> ValueSome OperatorData.OpGreaterThan
        | Token.OpLessThanOrEqual -> ValueSome OperatorData.OpLessThanOrEqual
        | Token.OpGreaterThanOrEqual -> ValueSome OperatorData.OpGreaterThanOrEqual
        | Token.OpEquality -> ValueSome OperatorData.OpEquality
        | Token.OpInequality -> ValueSome OperatorData.OpInequality
        | Token.OpBitwiseAnd -> ValueSome OperatorData.OpBitwiseAnd
        | Token.OpBitwiseOr -> ValueSome OperatorData.OpBitwiseOr
        | Token.OpExclusiveOr -> ValueSome OperatorData.OpExclusiveOr
        | Token.OpLeftShift -> ValueSome OperatorData.OpLeftShift
        | Token.OpRightShift -> ValueSome OperatorData.OpRightShift
        | Token.OpAmpAmp -> ValueSome OperatorData.OpBooleanAnd
        | Token.OpBarBar -> ValueSome OperatorData.OpBooleanOr
        // Pipes and composition are polymorphic FSharp.Core functions, not
        // language intrinsics — they resolve through the same provider path as
        // any other named operator.
        | Token.OpPipeRight -> ValueSome OperatorData.OpPipeRight
        | Token.OpPipeLeft -> ValueSome OperatorData.OpPipeLeft
        | Token.OpComposeRight -> ValueSome OperatorData.OpComposeRight
        | Token.OpComposeLeft -> ValueSome OperatorData.OpComposeLeft
        // The dynamic-access operators. `?` / `?<-` lex to KEYWORD-kind tokens
        // (`OpDynamic` / `OpDynamicAssignment`), so `OperatorInfo.GetName` returns the
        // bare token name ("OpDynamic") rather than the compiled `op_*` form — map them
        // here (the single symbol→compiled-name authority) so the `(?)` / `(?<-)`
        // binding heads in `ops-dynamic.js.fsi` extract under the names the front end
        // resolves (`op_Dynamic` / `op_DynamicAssignment`). Never appear as a bare
        // `InfixApp` op (a `?` use site is `Expr.DynamicLookup`), so this is additive.
        | Token.OpDynamic -> ValueSome OperatorData.OpDynamic
        | Token.OpDynamicAssignment -> ValueSome OperatorData.OpDynamicAssignment
        | _ -> ValueNone

    /// Compiled name for a parenthesised *symbolic* operator head (`(<<<)`,
    /// `(~-)`, `(|>)`). The dedicated-token operators resolve through `ofToken`;
    /// everything else — the generic-token family a `(...)` head collapses to —
    /// defers to the canonical operator-name function, fed the head's lexed `text`
    /// (the operator's source spelling: `&&&`, `~-`, …).
    let ofParenSymbolic (text: string) (tok: SyntaxToken) : string voption =
        match ofToken tok.Token with
        | ValueSome _ as found -> found
        | ValueNone ->
            match OperatorInfo.TryCreate tok.PositionedToken with
            | ValueSome op -> ValueSome(op.GetName text)
            | ValueNone -> ValueNone

    /// The canonical **union-case constructor name** for a case head. The
    /// cons-list's operator cases map to their source ctor names (`([])` →
    /// `Empty`, `(::)` → `Cons`) — the names `ElaborateExpr` mints and codegen
    /// resolves through — *not* the `op_Nil`/`op_ColonColon` compiled-op form
    /// (`ofIdentOp`/`identOrOpName`, the *value*-position binding-head surface).
    /// Shared by the front-end union registration (`TypeRegistration`) and the
    /// contract extractor (`VesperLib`) so a locally-compiled union and its
    /// extracted contract name their cases identically. `ValueNone` for a head
    /// with no nameable ctor form (`(*)`, range / active-pattern ops), which the
    /// caller treats as "drop this case".
    let unionCaseCtorName (nameOf: SyntaxToken -> string) (head: IdentOrOp<SyntaxToken>) : string voption =
        match head with
        | IdentOrOp.Ident t -> ValueSome(nameOf t)
        | IdentOrOp.ParenOp(opName = OpName.NilOp _) -> ValueSome "Empty"
        | IdentOrOp.ParenOp(opName = OpName.SymbolicOp op) ->
            match nameOf op with
            | "::" -> ValueSome "Cons"
            | s -> ValueSome s
        | _ -> ValueNone

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
    /// the projector (`Elaborate.translateIdent`) so the qualified-operator form is
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
