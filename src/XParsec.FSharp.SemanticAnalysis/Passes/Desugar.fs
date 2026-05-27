namespace XParsec.FSharp.SemanticAnalysis.Passes

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// Pre:  none.
// Post: ctx.Desugared populated for every CST node whose semantics differ
//       from its surface form.
//
// Annotation-only: NEVER rewrites the CST.

module Desugar =

    /// Only the supported subset is listed; extend as more operators come online.
    let private infixOpName (t: Token) : string voption =
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
        // Bitwise binary ops. A *bare* use site (`a &&& b`) lexes to the distinct
        // `Token` (`Token.ofCustomOperator`'s isBare arms), so the enum match is
        // reliable here; a binding head `(&&&)` is generic and resolves by text
        // (`opCompiledNameOfText`).
        | Token.OpBitwiseAnd -> ValueSome "op_BitwiseAnd"
        | Token.OpBitwiseOr -> ValueSome "op_BitwiseOr"
        | Token.OpExclusiveOr -> ValueSome "op_ExclusiveOr"
        | Token.OpLeftShift -> ValueSome "op_LeftShift"
        | Token.OpRightShift -> ValueSome "op_RightShift"
        // Source `&&` / `||` lex as OpAmpAmp / OpBarBar, not OpBooleanAnd /
        // OpBooleanOr (those share OpFamily.OpGeneric — see memory note
        // about Token-encoding aliases).
        | Token.OpAmpAmp -> ValueSome "op_BooleanAnd"
        | Token.OpBarBar -> ValueSome "op_BooleanOr"
        // Pipes and composition are polymorphic FSharp.Core functions,
        // not language intrinsics — they resolve through the same provider
        // path as any other named operator.
        | Token.OpPipeRight -> ValueSome "op_PipeRight"
        | Token.OpPipeLeft -> ValueSome "op_PipeLeft"
        | Token.OpComposeRight -> ValueSome "op_ComposeRight"
        | Token.OpComposeLeft -> ValueSome "op_ComposeLeft"
        | _ -> ValueNone

    /// Compiled name for a symbolic operator used as a *value* (`(+)` →
    /// "op_Addition"). A parenthesised operator denotes the same FSharp.Core
    /// member the infix form desugars to, so the mapping is shared. Consumed
    /// by NameResolution / Unification / Freeze to resolve `(op)` references.
    /// NOTE: a *bare* operator at a use site lexes to its distinct `Token`
    /// (`+` → `OpAddition`), so this enum match is reliable there; an operator
    /// inside parens (a binding head / value) lexes to a *generic* operator token
    /// and must be resolved by source text — see `opPatCompiledName`.
    let symbolicOpCompiledName (t: Token) : string voption = infixOpName t

    /// Compiled name for an operator-named binding *head* (`let (=) x y = …` →
    /// "op_Equality", `let (~-) n = …` → "op_UnaryNegation"), so an operator
    /// definition freezes under the same compiled member name its use sites
    /// reference (and is then collectable as a cross-package inline body).
    ///
    /// Well-known / keyword-encoded operators (`=` `+` `&&` …) lex to their
    /// distinct `Token` even inside parens, so the enum match
    /// (`symbolicOpCompiledName`) resolves them; the *generic*-token operators
    /// (`~-` `~+`, and the bitwise ops in non-bare position, which share
    /// `OpFamily.OpGeneric`) fall back to the parser's canonical operator-name
    /// function (`OperatorInfo.GetName`, fed the head token's lexed text via
    /// `nameOf`/`ctx.NameOf`). `( * )` parses to a dedicated `IdentOrOp.StarOp`
    /// (its star is a *virtual* token, so it has no usable lexed text) and is
    /// mapped directly. Range / active-pattern heads return `ValueNone`.
    let opPatCompiledName (nameOf: SyntaxToken -> string) (io: IdentOrOp<SyntaxToken>) : string voption =
        match io with
        | IdentOrOp.StarOp _ -> ValueSome "op_Multiply"
        | IdentOrOp.ParenOp(opName = OpName.NilOp _) -> ValueSome "op_Nil"
        | IdentOrOp.ParenOp(opName = OpName.SymbolicOp tok) ->
            match symbolicOpCompiledName tok.Token with
            | ValueSome n -> ValueSome n
            | ValueNone ->
                match OperatorInfo.TryCreate tok.PositionedToken with
                | ValueSome op -> ValueSome(op.GetName(nameOf tok))
                | ValueNone -> ValueNone
        | _ -> ValueNone

    /// Token.OpSubtraction is used by both binary `a - b` (InfixApp) and
    /// unary `-x` (PrefixApp). The PrefixApp form maps to op_UnaryNegation.
    let private prefixOpName (t: Token) : string voption =
        match t with
        | Token.OpSubtraction -> ValueSome "op_UnaryNegation"
        // `~~~x` (bitwise complement) lexes to the distinct `OpLogicalNot`
        // (wellKnownOps); `~-`/`~+` only appear as binding heads / values today,
        // not as their own prefix use site (`-x` is `OpSubtraction`).
        | Token.OpLogicalNot -> ValueSome "op_LogicalNot"
        | _ -> ValueNone

    /// `[ … ]` / `[| … |]` literals share the same lowering target — the
    /// nested `Cons` / `Nil` chain, with arrays adding an `Array.ofList`
    /// wrap at Freeze time.
    let private literalFormOfParen (pk: ParenKind<SyntaxToken>) : DesugaredForm voption =
        match pk with
        | ParenKind.List _ -> ValueSome DesugaredForm.ListLiteral
        | ParenKind.Array _ -> ValueSome DesugaredForm.ArrayLiteral
        | _ -> ValueNone

    let private visit (ctx: PassContext) (_env: unit) (e: Expr<SyntaxToken>) : unit =
        match e with
        | Expr.InfixApp(_, op, _) ->
            match infixOpName op.Token with
            | ValueSome name -> ctx.Desugared.Set(CstKeys.ofExpr e, DesugaredForm.OpName name)
            | ValueNone -> ()
        | Expr.PrefixApp(op, _) ->
            match prefixOpName op.Token with
            | ValueSome name -> ctx.Desugared.Set(CstKeys.ofExpr e, DesugaredForm.OpName name)
            | ValueNone -> ()
        | Expr.EnclosedBlock(lParen = pk)
        | Expr.EmptyBlock(lParen = pk) ->
            match literalFormOfParen pk with
            | ValueSome form -> ctx.Desugared.Set(CstKeys.ofExpr e, form)
            | ValueNone -> ()
        | _ -> ()

    let private mkWalker (ctx: PassContext) : CstWalk.ExprWalker<unit> =
        {
            Visit = visit ctx
            EnterFun = fun env _ -> env
            EnterBindingRhs = fun env _ _ _ -> env
            EnterLetBody = fun env _ -> env
            EnterForTo = fun env _ -> env
            EnterForIn = fun env _ -> env
            EnterMatchArm = fun env _ -> env
        }

    let private walkModuleElem (walker: CstWalk.ExprWalker<unit>) (m: ModuleElem<SyntaxToken>) =
        match m with
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bindings)) ->
            for b in bindings do
                CstWalk.iterExpr walker () b.expr
        | ModuleElem.Expression e -> CstWalk.iterExpr walker () e
        | ModuleElem.Type defs ->
            // Recurse into member bodies (incl. union augmentation, P3d.3) so the
            // ops they contain get compiled-name entries. Without this,
            // Unification's `inferInfix` falls through to a free TyVar and the
            // member's body type doesn't pin to a concrete type.
            let walkMemberElems (elems: TypeDefnElement<SyntaxToken> seq) =
                for el in elems do
                    match el with
                    | TypeDefnElement.Member(MemberDefn.Member(defn = d)) ->
                        match d with
                        | MethodOrPropDefn.Method(defn = b)
                        | MethodOrPropDefn.Property(defn = b) -> CstWalk.iterExpr walker () b.expr
                        | MethodOrPropDefn.AutoProperty(expr = e) -> CstWalk.iterExpr walker () e
                        | _ -> ()
                    | _ -> ()

            for td in defs do
                match TypeDefnPatterns.tryObjectModelBody td with
                | ValueSome b -> walkMemberElems b.elements
                | ValueNone ->
                    match td with
                    | TypeDefn.Union(extensions = ValueSome(TypeExtensionElements(elements = elems))) ->
                        walkMemberElems elems
                    | TypeDefn.Record(extensions = ValueSome(TypeExtensionElements(elements = elems))) ->
                        walkMemberElems elems
                    | _ -> ()
        | _ -> ()

    let private walkElems (walker: CstWalk.ExprWalker<unit>) (elems: ModuleElems<SyntaxToken>) =
        for m in elems do
            walkModuleElem walker m

    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
        let walker = mkWalker ctx
        walkElems walker (CstWalk.implFileElems file)
