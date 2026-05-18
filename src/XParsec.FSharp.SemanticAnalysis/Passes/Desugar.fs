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
// Recursion is delegated to CstWalk.iterExpr; this pass only supplies the
// Visit hook. No scope state is needed.
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
        // Pipes and composition are polymorphic FSharp.Core functions,
        // not language intrinsics — they resolve through the same provider
        // path as any other named operator.
        | Token.OpPipeRight -> ValueSome "op_PipeRight"
        | Token.OpPipeLeft -> ValueSome "op_PipeLeft"
        | Token.OpComposeRight -> ValueSome "op_ComposeRight"
        | Token.OpComposeLeft -> ValueSome "op_ComposeLeft"
        | _ -> ValueNone

    /// Token.OpSubtraction is used by both binary `a - b` (InfixApp) and
    /// unary `-x` (PrefixApp). The PrefixApp form maps to op_UnaryNegation.
    let private prefixOpName (t: Token) : string voption =
        match t with
        | Token.OpSubtraction -> ValueSome "op_UnaryNegation"
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
            // Recurse into class / anon-class member bodies so the
            // InfixApp / PrefixApp ops they contain pick up their
            // compiled-name entries in `ctx.Desugared`. Without this,
            // Unification's `inferInfix` falls through to a free TyVar
            // and the member's body type doesn't pin to a concrete type.
            for td in defs do
                let bodyOpt =
                    match td with
                    | TypeDefn.Class(body = b)
                    | TypeDefn.Anon(body = b)
                    | TypeDefn.Struct(body = b)
                    | TypeDefn.Interface(body = b) -> ValueSome b
                    | _ -> ValueNone

                match bodyOpt with
                | ValueSome body ->
                    for el in body.elements do
                        match el with
                        | TypeDefnElement.Member(MemberDefn.Member(defn = d)) ->
                            match d with
                            | MethodOrPropDefn.Method(defn = b)
                            | MethodOrPropDefn.Property(defn = b) -> CstWalk.iterExpr walker () b.expr
                            | MethodOrPropDefn.AutoProperty(expr = e) -> CstWalk.iterExpr walker () e
                            | _ -> ()
                        | _ -> ()
                | ValueNone -> ()
        | _ -> ()

    let private walkElems (walker: CstWalk.ExprWalker<unit>) (elems: ModuleElems<SyntaxToken>) =
        for m in elems do
            walkModuleElem walker m

    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
        let walker = mkWalker ctx

        match file with
        | ImplementationFile.AnonymousModule elems -> walkElems walker elems
        | ImplementationFile.NamedModule(NamedModule.NamedModule(elements = elems)) -> walkElems walker elems
        | ImplementationFile.Namespaces _ -> ()
