namespace XParsec.FSharp.SemanticAnalysis.Passes

open XParsec.FSharp
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// Pre:  none.
// Post: ctx.Desugared populated for every CST node whose semantics differ from its
//       surface form. Annotation-only: NEVER rewrites the CST.

module Desugar =

    /// Compiled name for a symbolic operator (`+` → `"op_Addition"`). Reliable for a
    /// BARE operator, which lexes to its own `Token`; one inside parens can lex to a
    /// generic operator token and needs a source-text fallback.
    let symbolicOpCompiledName (t: Token) : string voption = OperatorNames.ofToken t

    /// `Token.OpSubtraction` / `OpAddition` serve both `a - b` and `-x`; only the prefix
    /// form reaches here, mapping to `op_UnaryNegation` / `op_UnaryPlus`. The spellings
    /// `~-` / `~+` never do because they appear only as bound names / values.
    let private prefixOpName (t: Token) : string voption =
        match t with
        | Token.OpSubtraction -> ValueSome OperatorData.OpUnaryNegation
        | Token.OpAddition -> ValueSome OperatorData.OpUnaryPlus
        // `~~~x` (bitwise complement) lexes to the distinct `OpLogicalNot`.
        | Token.OpLogicalNot -> ValueSome OperatorData.OpLogicalNot
        // `&local`, the managed address-of. Unlike the other prefix ops it has no
        // provider symbol: the name is special-cased downstream into the byref
        // intrinsic, so it never reaches operator resolution.
        | Token.OpAmp -> ValueSome OperatorData.OpAddressOf
        | _ -> ValueNone

    /// `[ … ]` lowers to a nested `Cons` / `Nil` chain, `[| … |]` to an array node.
    let private literalFormOfParen (pk: ParenKind<SyntaxToken>) : DesugaredForm voption =
        match pk with
        | ParenKind.List _ -> ValueSome DesugaredForm.ListLiteral
        | ParenKind.Array _ -> ValueSome DesugaredForm.ArrayLiteral
        | _ -> ValueNone

    let private visit (ctx: PassContext) (_env: unit) (e: Expr<SyntaxToken>) : unit =
        match e with
        | Expr.InfixApp(_, op, _) ->
            // `::` is not a provider-resolved operator: it constructs the list union
            // directly, so it gets its own form rather than an `op_*` member name.
            match op.Token with
            | Token.KWColonColon -> ctx.Desugared.Set(CstKeys.ofExpr e, DesugaredForm.ConsExpr)
            | _ ->
                match symbolicOpCompiledName op.Token with
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
            // A secondary ctor's body (`new(args) = …`) is an `AdditionalConstrExpr`,
            // not a plain `Expr`, so each embedded expression (`let`-preamble RHS,
            // chain-call arg, field init `{ f = e }`) has to be walked by hand.
            let rec walkCtorBody (ace: AdditionalConstrExpr<SyntaxToken>) : unit =
                match ace with
                | AdditionalConstrExpr.LetIn(binding = b; body = body) ->
                    CstWalk.iterExpr walker () b.expr
                    walkCtorBody body
                | AdditionalConstrExpr.SequenceAfter(stmt = s; rest = rest) ->
                    CstWalk.iterExpr walker () s
                    walkCtorBody rest
                | AdditionalConstrExpr.SequenceBefore(before = before; expr = e) ->
                    walkCtorBody before
                    CstWalk.iterExpr walker () e
                | AdditionalConstrExpr.Conditional(cond = c; thenBranch = t; elseBranch = el) ->
                    CstWalk.iterExpr walker () c
                    walkCtorBody t
                    walkCtorBody el
                | AdditionalConstrExpr.Init initExpr ->
                    match initExpr with
                    | AdditionalConstrInitExpr.Expression e
                    | AdditionalConstrInitExpr.Delegated(expr = e) -> CstWalk.iterExpr walker () e
                    | AdditionalConstrInitExpr.Explicit(inherits = inh; initializers = inits) ->
                        match inh with
                        | ValueSome(ClassInheritsDecl(expr = ValueSome e)) -> CstWalk.iterExpr walker () e
                        | _ -> ()

                        for FieldInitializer(expr = e) in inits do
                            CstWalk.iterExpr walker () e

            let rec walkMemberElems (elems: TypeDefnElement<SyntaxToken> seq) =
                for el in elems do
                    match el with
                    | TypeDefnElement.Member(MemberDefn.Member(defn = d)) ->
                        match d with
                        | MethodOrPropDefn.AutoProperty(expr = e) -> CstWalk.iterExpr walker () e
                        | d ->
                            for b in CstWalk.memberBindings d do
                                CstWalk.iterExpr walker () b.expr
                    | TypeDefnElement.Member(MemberDefn.AdditionalConstructor(body = body)) -> walkCtorBody body
                    // An `interface Foo with member …` body holds member bodies too, nested
                    // under `ObjectMembers`; reproject each onto a `TypeDefnElement.Member`
                    // and recurse.
                    | TypeDefnElement.InterfaceImpl(InterfaceImpl.InterfaceImpl(objectMembers = objMembersOpt)) ->
                        match objMembersOpt with
                        | ValueSome(ObjectMembers(memberDefns = mds)) ->
                            walkMemberElems (seq { for md in mds -> TypeDefnElement.Member md })
                        | ValueNone -> ()
                    | _ -> ()

            // The class preamble (`[static] let` / `[static] do`) and the primary
            // `inherit Base(args)` argument are ordinary expressions, so their operators
            // need compiled-name entries exactly as a member body's do.
            let walkClassBody (b: ObjectModelBody<SyntaxToken>) =
                for d in b.classPreamble do
                    match d with
                    | ClassFunctionOrValueDefn.LetBindings(bindings = bindings) ->
                        for binding in bindings do
                            CstWalk.iterExpr walker () binding.expr
                    | ClassFunctionOrValueDefn.Do(expr = e) -> CstWalk.iterExpr walker () e

                match b.inherits with
                | ValueSome(ClassInheritsDecl(expr = ValueSome e)) -> CstWalk.iterExpr walker () e
                | _ -> ()

                walkMemberElems b.elements

            for td in defs do
                match TypeDefnPatterns.tryObjectModelBody td with
                | ValueSome b -> walkClassBody b
                | ValueNone ->
                    match td with
                    | TypeDefn.Union(extensions = ValueSome(TypeExtensionElements(elements = elems))) ->
                        walkMemberElems elems
                    | TypeDefn.Record(extensions = ValueSome(TypeExtensionElements(elements = elems))) ->
                        walkMemberElems elems
                    // An inline intrinsic-abbrev host (`type X = (# … #) with member …`)
                    // carries member bodies too.
                    | TypeDefn.Abbrev(extensions = ValueSome(TypeExtensionElements(elements = elems))) ->
                        walkMemberElems elems
                    | _ -> ()
        | _ -> ()

    let private walkElems (walker: CstWalk.ExprWalker<unit>) (elems: ModuleElems<SyntaxToken>) =
        for m in elems do
            walkModuleElem walker m

    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
        let walker = mkWalker ctx
        walkElems walker (CstWalk.implFileElems file)
