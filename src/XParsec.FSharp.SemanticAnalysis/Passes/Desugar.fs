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
//   - PrefixApp (-x -> op_UnaryNegation x)
//   - `x |> f`, `x ||> f y`             -> Application
//   - List / array / seq comprehensions -> yield + CE method chain
//   - `for x in xs do …`                -> IEnumerator pattern
//   - Computation expressions           -> Builder method chain
//   - Active pattern uses               -> Match + discriminator calls
//   - Range expressions                 -> Seq.initInfinite + take, or fast path
//   - Object expressions                -> Type instantiation + interface impl

module Desugar =

    /// Maps the operator's Token enum value to its compiled name. Only the
    /// tiny-subset operators are listed; extend as the subset grows.
    let private infixOpName (t: Token) : string voption =
        match t with
        | Token.OpAddition -> ValueSome "op_Addition"
        | Token.OpSubtraction -> ValueSome "op_Subtraction"
        | Token.OpMultiply -> ValueSome "op_Multiply"
        | _ -> ValueNone

    let rec private walkExpr (ctx: PassContext) (e: Expr<SyntaxToken>) =
        match e with
        | Expr.InfixApp(left, op, right) ->
            match infixOpName op.Token with
            | ValueSome name -> ctx.Desugared.Set(CstKeys.ofExpr e, DesugaredForm.OpName name)
            | ValueNone -> ()

            walkExpr ctx left
            walkExpr ctx right
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
