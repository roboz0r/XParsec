namespace XParsec.FSharp.SemanticAnalysis.Passes

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// Pre:  none.
// Post: ctx.Desugared populated for every CST node whose semantics differ
//       from its surface form.
//
// Annotation-only: NEVER rewrites the CST. Mints synthetic NodeKeys via
// NodeKey.ofSynthetic and stores a DesugaredForm describing how to interpret
// each construct.
//
// TODO — constructs that will need desugaring:
//   - `x |> f`, `x ||> f y`             -> Application
//   - List / array / seq comprehensions -> yield + CE method chain
//   - `for x in xs do …`                -> IEnumerator pattern
//   - Computation expressions           -> Builder method chain
//   - Active pattern uses               -> Match + discriminator calls
//   - Range expressions                 -> Seq.initInfinite + take, or fast path
//   - Object expressions                -> Type instantiation + interface impl

module Desugar =

    /// Maps an infix operator's Token enum value to its compiled name. Only
    /// the supported subset is listed; extend as more operators come online.
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
        // Source `&&` / `||` lex as OpAmpAmp / OpBarBar, not OpBooleanAnd /
        // OpBooleanOr (those share OpFamily.OpGeneric — see memory note
        // about Token-encoding aliases).
        | Token.OpAmpAmp -> ValueSome "op_BooleanAnd"
        | Token.OpBarBar -> ValueSome "op_BooleanOr"
        | _ -> ValueNone

    /// Token.OpSubtraction is used by both binary `a - b` (InfixApp) and
    /// unary `-x` (PrefixApp). The PrefixApp form maps to op_UnaryNegation.
    let private prefixOpName (t: Token) : string voption =
        match t with
        | Token.OpSubtraction -> ValueSome "op_UnaryNegation"
        | _ -> ValueNone

    let rec private walkExpr (ctx: PassContext) (e: Expr<SyntaxToken>) =
        match e with
        | Expr.InfixApp(left, op, right) ->
            match infixOpName op.Token with
            | ValueSome name -> ctx.Desugared.Set(CstKeys.ofExpr e, DesugaredForm.OpName name)
            | ValueNone -> ()

            walkExpr ctx left
            walkExpr ctx right
        | Expr.PrefixApp(op, operand) ->
            match prefixOpName op.Token with
            | ValueSome name -> ctx.Desugared.Set(CstKeys.ofExpr e, DesugaredForm.OpName name)
            | ValueNone -> ()

            walkExpr ctx operand
        | Expr.App(fn, args) ->
            walkExpr ctx fn

            for a in args do
                walkExpr ctx a
        | Expr.Fun(expr = body) -> walkExpr ctx body
        | Expr.LetOrUse(bindings = bindings; body = body) ->
            for b in bindings do
                walkExpr ctx b.expr

            match body with
            | ValueSome b -> walkExpr ctx b
            | ValueNone -> ()
        | Expr.EnclosedBlock(expr = inner) -> walkExpr ctx inner
        | Expr.IfThenElse(condition = cond; thenExpr = thenE; elseBranch = elseB) ->
            // Tiny subset: elifBranches not yet handled; ignored here, will
            // be rejected by Unification / Freeze if non-empty.
            walkExpr ctx cond
            walkExpr ctx thenE

            match elseB with
            | ValueSome(ElseBranch(expr = e)) -> walkExpr ctx e
            | ValueNone -> ()
        | _ -> ()

    let private walkModuleElem (ctx: PassContext) (m: ModuleElem<SyntaxToken>) =
        match m with
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bindings)) ->
            for b in bindings do
                walkExpr ctx b.expr
        | ModuleElem.Expression e -> walkExpr ctx e
        | _ -> ()

    let private walkElems (ctx: PassContext) (elems: ModuleElems<SyntaxToken>) =
        for m in elems do
            walkModuleElem ctx m

    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
        match file with
        | ImplementationFile.AnonymousModule elems -> walkElems ctx elems
        | ImplementationFile.NamedModule(NamedModule.NamedModule(elements = elems)) -> walkElems ctx elems
        | ImplementationFile.Namespaces _ -> ()
