namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// Symbolic operator, whether a token or a parenthesised binding name, to its
/// compiled member name: `+` → `op_Addition`, `<<<` → `op_LeftShift`.
module OperatorNames =

    /// Only operators the lexer emits as a *distinct* `Token`. A parenthesised
    /// name (`(<<<)`, `(~-)`) collapses to `OpGeneric`, so `ofParenSymbolic`
    /// recovers it from source text. `::` → `ValueNone`: cons builds the list union.
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
        | Token.OpPipeRight -> ValueSome OperatorData.OpPipeRight
        | Token.OpPipeLeft -> ValueSome OperatorData.OpPipeLeft
        | Token.OpComposeRight -> ValueSome OperatorData.OpComposeRight
        | Token.OpComposeLeft -> ValueSome OperatorData.OpComposeLeft
        // `?` / `?<-` lex as KEYWORD-kind tokens, whose generic name is the bare
        // token name ("OpDynamic"), so the `op_Dynamic` / `op_DynamicAssignment`
        // spellings the front end resolves exist only here.
        | Token.OpDynamic -> ValueSome OperatorData.OpDynamic
        | Token.OpDynamicAssignment -> ValueSome OperatorData.OpDynamicAssignment
        | _ -> ValueNone

    /// `(<<<)` → `op_LeftShift`. `text` is the operator's source spelling (`&&&`,
    /// `~-`), the only thing an `OpGeneric` token can be named from.
    let ofParenSymbolic (text: string) (tok: SyntaxToken) : string voption =
        match ofToken tok.Token with
        | ValueSome _ as found -> found
        | ValueNone ->
            match OperatorInfo.TryCreate tok.PositionedToken with
            | ValueSome op -> ValueSome(op.GetName text)
            | ValueNone -> ValueNone

    /// Union-case ctor name: `([])` → `Empty`, `(::)` → `Cons`. These are the source ctor
    /// spellings, NOT the `op_Nil` / `op_ColonColon` compiled-op form. `ValueNone`
    /// for a name with no ctor form (range / active-pattern op): drop that case.
    let unionCaseCtorName (nameOf: SyntaxToken -> string) (ident: IdentOrOp<SyntaxToken>) : string voption =
        match ident with
        | IdentOrOp.Ident t -> ValueSome(nameOf t)
        | IdentOrOp.ParenOp(opName = OpName.NilOp _) -> ValueSome "Empty"
        | IdentOrOp.ParenOp(opName = OpName.SymbolicOp op) ->
            match nameOf op with
            | "::" -> ValueSome "Cons"
            | s -> ValueSome s
        | _ -> ValueNone

    let ofIdentOp (nameOf: SyntaxToken -> string) (idOp: IdentOrOp<SyntaxToken>) : string voption =
        match idOp with
        | IdentOrOp.ParenOp(opName = OpName.SymbolicOp op) -> ofParenSymbolic (nameOf op) op
        | _ -> ValueNone

    /// The compiled name an operator-named DEFINITION binds, the name its use sites reference:
    /// `let (=) x y = …` → `op_Equality`, `let (~-) n = …` → `op_UnaryNegation`,
    /// `([])` → `op_Nil`.
    let ofPatOp (nameOf: SyntaxToken -> string) (idOp: IdentOrOp<SyntaxToken>) : string voption =
        match idOp with
        | IdentOrOp.ParenOp(opName = OpName.NilOp _) -> ValueSome OperatorData.OpNil
        | _ -> ofIdentOp nameOf idOp

    /// The name a `val` / `member` signature DECLARES, plain or operator: `f` → `f`,
    /// `(+)` → `op_Addition`, `(::)` → `op_ColonColon`, `([])` → `op_Nil`. `ValueNone` for an
    /// active-pattern name, whose compiled form is not modelled.
    let ofDeclaredName (nameOf: SyntaxToken -> string) (idOp: IdentOrOp<SyntaxToken>) : string voption =
        match idOp with
        | IdentOrOp.Ident tok -> ValueSome(nameOf tok)
        // `::` has no `op_` member in expression position, so cons is named here rather
        // than through the shared symbolic resolver.
        | IdentOrOp.ParenOp(opName = OpName.SymbolicOp opTok) when opTok.Token = Token.KWColonColon ->
            ValueSome OperatorData.OpColonColon
        | IdentOrOp.ParenOp(opName = OpName.RangeOp(RangeOpName.DotDot _)) -> ValueSome OperatorData.OpRange
        | IdentOrOp.ParenOp(opName = OpName.RangeOp(RangeOpName.DotDotDotDot _)) -> ValueSome OperatorData.OpRangeStep
        | IdentOrOp.ParenOp(opName = OpName.ActivePatternOp _) -> ValueNone
        | _ -> ofPatOp nameOf idOp

    /// `A.B.(+)` → `("A.B", "op_Addition")`: the qualifier and the operator's compiled short
    /// name, which resolution reads against a container. An empty qualifier is the bare form.
    /// `ValueNone` for a non-symbolic op segment.
    let qualifiedOpParts
        (nameOf: SyntaxToken -> string)
        (li: LongIdent<SyntaxToken>)
        (idOp: IdentOrOp<SyntaxToken>)
        : struct (string * string) voption =
        match ofIdentOp nameOf idOp with
        | ValueSome opName -> ValueSome(struct (li.Idents |> Seq.map nameOf |> String.concat ".", opName))
        | ValueNone -> ValueNone

    /// `qualifiedOpParts` joined: `A.B.(+)` → `"A.B.op_Addition"`.
    let qualifiedOpName
        (nameOf: SyntaxToken -> string)
        (li: LongIdent<SyntaxToken>)
        (idOp: IdentOrOp<SyntaxToken>)
        : string voption =
        qualifiedOpParts nameOf li idOp
        |> ValueOption.map (fun (struct (prefix, opName)) -> SymbolKeyOps.qualify prefix opName)
