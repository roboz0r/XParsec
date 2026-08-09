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

    /// `A.B.(+)` → `"A.B.op_Addition"`; an empty qualifier gives the bare
    /// `"op_Addition"`. `ValueNone` for a non-symbolic op segment.
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
