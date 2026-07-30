namespace XParsec.FSharp.SemanticAnalysis.Passes

open XParsec.FSharp
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// Pre:  none.
// Post: ctx.Desugared populated for every CST node whose semantics differ
//       from its surface form.
//
// Annotation-only: NEVER rewrites the CST.

module Desugar =

    /// Compiled name for a symbolic operator used as a *value* (`(+)` →
    /// "op_Addition"). A parenthesised operator denotes the same FSharp.Core
    /// member the infix form desugars to, so the mapping is shared. Consumed
    /// by NameResolution / Unification / Elaborate to resolve `(op)` references.
    /// NOTE: a *bare* operator at a use site lexes to its distinct `Token`
    /// (`+` → `OpAddition`), so this enum match is reliable there; an operator
    /// inside parens (a binding head / value) lexes to a *generic* operator token
    /// and must be resolved by source text — see `opPatCompiledName`.
    let symbolicOpCompiledName (t: Token) : string voption = OperatorNames.ofToken t

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
        | IdentOrOp.ParenOp(opName = OpName.NilOp _) -> ValueSome OperatorData.OpNil
        | IdentOrOp.ParenOp(opName = OpName.SymbolicOp tok) -> OperatorNames.ofParenSymbolic (nameOf tok) tok
        | _ -> ValueNone

    /// Token.OpSubtraction is used by both binary `a - b` (InfixApp) and
    /// unary `-x` (PrefixApp). The PrefixApp form maps to op_UnaryNegation.
    let private prefixOpName (t: Token) : string voption =
        match t with
        | Token.OpSubtraction -> ValueSome OperatorData.OpUnaryNegation
        // `~~~x` (bitwise complement) lexes to the distinct `OpLogicalNot`
        // (wellKnownOps); `~-`/`~+` only appear as binding heads / values today,
        // not as their own prefix use site (`-x` is `OpSubtraction`).
        | Token.OpLogicalNot -> ValueSome OperatorData.OpLogicalNot
        // `&local` is the managed address-of (byref): its prefix compiled name is
        // `op_AddressOf`. Unlike the other prefix ops it has no provider symbol —
        // `inferPrefix` / `translatePrefix` special-case the name (the byref
        // intrinsic + an `ldloca` of the local), so it never reaches operator
        // resolution. `&&` (`OpAmpAmp`, native int address-of) is left unmapped
        // (no consumer) and the boolean `&&` is an `InfixApp`, not a prefix.
        | Token.OpAmp -> ValueSome OperatorData.OpAddressOf
        | _ -> ValueNone

    /// `[ … ]` / `[| … |]` literals share the same lowering target — the
    /// nested `Cons` / `Nil` chain, with arrays adding an `Array.ofList`
    /// wrap at Elaborate time.
    let private literalFormOfParen (pk: ParenKind<SyntaxToken>) : DesugaredForm voption =
        match pk with
        | ParenKind.List _ -> ValueSome DesugaredForm.ListLiteral
        | ParenKind.Array _ -> ValueSome DesugaredForm.ArrayLiteral
        | _ -> ValueNone

    let private visit (ctx: PassContext) (_env: unit) (e: Expr<SyntaxToken>) : unit =
        match e with
        | Expr.InfixApp(_, op, _) ->
            // `::` is not a provider-resolved operator: it constructs the list
            // union directly, so it carries its own desugared form (consumed by
            // Unification/Elaborate) rather than an `op_*` member name.
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
            // Recurse into member bodies (incl. union augmentation, P3d.3) so the
            // ops they contain get compiled-name entries. Without this,
            // Unification's `inferInfix` falls through to a free TyVar and the
            // member's body type doesn't pin to a concrete type.
            // A secondary ctor's body (`new(args) = …`, B-11) is an
            // `AdditionalConstrExpr`, not a plain `Expr` — walk each embedded
            // expression so a `let`-preamble RHS, a chain-call arg, or an explicit
            // field-init `{ f = e }` gets its operator
            // compiled-name entries. Mirrors `NameResolution.walkCtorBody`.
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
                        | MethodOrPropDefn.Method(defn = b)
                        | MethodOrPropDefn.Property(defn = b) -> CstWalk.iterExpr walker () b.expr
                        | MethodOrPropDefn.AutoProperty(expr = e) -> CstWalk.iterExpr walker () e
                        | _ -> ()
                    | TypeDefnElement.Member(MemberDefn.AdditionalConstructor(body = body)) -> walkCtorBody body
                    // An `interface Foo with member …` body holds member bodies too —
                    // their operators need compiled-name entries exactly as the type's
                    // own members do, or Unification's `inferInfix` falls through to a
                    // free TyVar and Elaborate throws `InfixApp … missing DesugaredForm`.
                    // The members nest under `ObjectMembers`; reproject each onto a
                    // `TypeDefnElement.Member` and recurse (mirrors `extractInterfaceImpls`).
                    | TypeDefnElement.InterfaceImpl(InterfaceImpl.InterfaceImpl(objectMembers = objMembersOpt)) ->
                        match objMembersOpt with
                        | ValueSome(ObjectMembers(memberDefns = mds)) ->
                            walkMemberElems (seq { for md in mds -> TypeDefnElement.Member md })
                        | ValueNone -> ()
                    | _ -> ()

            // The class PREAMBLE (`[static] let` initialisers, `[static] do` bodies) and the
            // primary `inherit Base(args)` argument expression are ordinary expressions that
            // NameResolution scopes and Unification infers, so their operators need
            // compiled-name entries exactly as a member body's do — without them `inferInfix`
            // falls through to a free TyVar and Elaborate throws `InfixApp … missing
            // DesugaredForm`.
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
                    // carries member bodies too — their operators need the same
                    // compiled-name entries the union/record augmentation members get.
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
