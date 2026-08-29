namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// Symbolic operator, whether a token or a parenthesised binding name, to its
/// compiled member name: `+` → `op_Addition`, `<<<` → `op_LeftShift`.
module OperatorNames =

    /// Compiled name of an operator USE spelled `text`, whether a well-known operator
    /// (`+` → `op_Addition`) or a custom one (`>=>` → `op_GreaterEqualsGreater`).
    /// `ValueNone` for `::` (cons builds the list union), structural punctuation
    /// (`;`, `->`), non-operator tokens, and a virtual token's empty spelling.
    let ofSymbolic (text: string) (tok: SyntaxToken) : string voption =
        if System.String.IsNullOrEmpty text then
            ValueNone
        else
            OperatorInfo.TryGetOpName(tok.Token, text)

    /// Compiled name of a PREFIX operator use. A dual-use spelling resolves as its
    /// `~`-prefixed form (`-` → `op_UnaryNegation`, `&` → `op_AddressOf`); a spelling
    /// already beginning `!` or `~` resolves as written (`!` → `op_Dereference`).
    let ofPrefix (text: string) (tok: SyntaxToken) : string voption =
        if System.String.IsNullOrEmpty text then
            ValueNone
        elif text.[0] = '!' || text.[0] = '~' then
            ofSymbolic text tok
        else
            ofSymbolic ("~" + text) tok

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
        | IdentOrOp.ParenOp(opName = OpName.SymbolicOp op) -> ofSymbolic (nameOf op) op
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

    /// `A.B.(+)` → `([| "A"; "B" |], "op_Addition")`: the qualifier's segments and the
    /// operator's compiled short name, which resolution reads against a container. Empty
    /// segments are the bare form. `ValueNone` for a non-symbolic op segment.
    let qualifiedOpParts
        (nameOf: SyntaxToken -> string)
        (li: LongIdent<SyntaxToken>)
        (idOp: IdentOrOp<SyntaxToken>)
        : struct (string[] * string) voption =
        match ofIdentOp nameOf idOp with
        | ValueSome opName -> ValueSome(struct (li.Idents |> Seq.map nameOf |> Seq.toArray, opName))
        | ValueNone -> ValueNone

    /// `qualifiedOpParts` joined: `A.B.(+)` → `"A.B.op_Addition"`.
    let qualifiedOpName
        (nameOf: SyntaxToken -> string)
        (li: LongIdent<SyntaxToken>)
        (idOp: IdentOrOp<SyntaxToken>)
        : string voption =
        qualifiedOpParts nameOf li idOp
        |> ValueOption.map (fun (struct (segments, opName)) -> SymbolKeyOps.qualify (String.concat "." segments) opName)
