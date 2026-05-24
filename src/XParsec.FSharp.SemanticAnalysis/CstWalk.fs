namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Immutable
open XParsec.FSharp.Parser

// Single point where Expr's recursion shape is enumerated. Passes layer
// their pass-specific work on top via the ExprWalker record's hooks.
//
// `iterExpr` matches every Expr case explicitly (no `| _ -> ()` catchall),
// so when the parser adds a new Expr case the F# warning-25 incomplete-
// pattern check fires here — one place to update instead of every pass
// silently no-oping the new case.
//
// State is *environment*, not accumulator: scope changes from EnterFun /
// EnterBindingRhs / EnterLetBody apply only to the relevant child
// traversal. Side-effecting outputs (side tables, diagnostics) live in
// the closure captured by `Visit`.

module CstWalk =

    type ExprWalker<'env> =
        {
            /// Called on every Expr node before recursing into its children.
            Visit: 'env -> Expr<SyntaxToken> -> unit
            /// Environment a lambda body sees.
            EnterFun: 'env -> ImmutableArray<Pat<SyntaxToken>> -> 'env
            /// Environment a binding's RHS sees. Args, in order:
            ///   env, isRec, siblings, this binding.
            /// `isRec` is true when the enclosing `let rec` (or
            /// `let rec … and …`) is present, in which case the binding
            /// sees all its siblings (and itself). `siblings` is the full
            /// binding group from the enclosing LetOrUse / module-level Let.
            EnterBindingRhs: 'env -> bool -> ImmutableArray<Binding<SyntaxToken>> -> Binding<SyntaxToken> -> 'env
            /// Environment a let body sees. NameResolution uses this to push
            /// all the bound names into scope.
            EnterLetBody: 'env -> ImmutableArray<Binding<SyntaxToken>> -> 'env
            /// Environment a `for i = … do …` body sees. The argument is the
            /// loop variable's ident token; NameResolution binds it as an int.
            EnterForTo: 'env -> SyntaxToken -> 'env
            /// Environment a `for pat in xs do …` body sees. Pattern types
            /// are still resolved against the (unknown) element type of the
            /// enumerable in Unification.
            EnterForIn: 'env -> Pat<SyntaxToken> -> 'env
            /// Environment a match-arm's guard + body sees. The argument is
            /// the arm's pattern.
            EnterMatchArm: 'env -> Pat<SyntaxToken> -> 'env
        }

    let rec iterExpr (walker: ExprWalker<'env>) (env: 'env) (e: Expr<SyntaxToken>) : unit =
        walker.Visit env e

        match e with
        | Expr.Const _
        | Expr.EmptyBlock _
        | Expr.LongIdentOrOp _
        | Expr.OptionalArgExpr _
        | Expr.Null _
        | Expr.Wildcard _
        | Expr.Missing
        | Expr.SkipsTokens _
        | Expr.Ident _
        | Expr.SliceAll _ -> ()

        // Interpolated-string hole exprs share the enclosing scope (no new
        // bindings). NameResolution / Unification must see them so an
        // interpolated `{name}` resolves and types.
        | Expr.String(parts = parts) ->
            for part in parts do
                match part with
                | StringPart.Expr(expr = holeExpr) -> iterExpr walker env holeExpr
                | _ -> ()

        | Expr.EnclosedBlock(expr = inner)
        | Expr.DotLookup(expr = inner)
        | Expr.TypeApp(expr = inner)
        | Expr.PrefixApp(expr = inner)
        | Expr.DynamicLookup(expr = inner)
        | Expr.New(expr = inner)
        | Expr.ControlFlow(expr = inner)
        | Expr.TypeAnnotation(expr = inner)
        | Expr.StaticUpcast(expr = inner)
        | Expr.DynamicTypeTest(expr = inner)
        | Expr.DynamicDowncast(expr = inner)
        | Expr.StaticMemberInvocation(expr = inner)
        | Expr.SliceFrom(expr = inner)
        | Expr.SliceTo(expr = inner) -> iterExpr walker env inner

        | Expr.App(funcExpr = fn; argExprs = args) ->
            iterExpr walker env fn

            for a in args do
                iterExpr walker env a

        | Expr.HighPrecedenceApp(funcExpr = fn; argExpr = arg) ->
            iterExpr walker env fn
            iterExpr walker env arg

        | Expr.InfixApp(leftExpr = left; rightExpr = right)
        | Expr.Assignment(leftExpr = left; rightExpr = right) ->
            iterExpr walker env left
            iterExpr walker env right

        | Expr.IndexedLookup(expr = inner; indexExpr = idx) ->
            iterExpr walker env inner
            iterExpr walker env idx

        | Expr.Tuple(exprs = exprs)
        | Expr.StructTuple(exprs = exprs)
        | Expr.Sequential(exprs = exprs) ->
            for x in exprs do
                iterExpr walker env x

        | Expr.TryFinally(tryExpr = tryE; finallyExpr = finallyE) ->
            iterExpr walker env tryE
            iterExpr walker env finallyE

        | Expr.Range(fromExpr = a; toExpr = b)
        | Expr.SliceFromTo(startExpr = a; endExpr = b) ->
            iterExpr walker env a
            iterExpr walker env b

        | Expr.SteppedRange(fromExpr = a; stepExpr = s; toExpr = b) ->
            iterExpr walker env a
            iterExpr walker env s
            iterExpr walker env b

        | Expr.LibraryOnlyStaticOptimization(expr = a; optimizedExpr = b) ->
            iterExpr walker env a
            iterExpr walker env b

        | Expr.ILIntrinsic(args = args) ->
            for x in args do
                iterExpr walker env x

        | Expr.Fun(argumentPats = argPats; expr = body) ->
            let bodyEnv = walker.EnterFun env argPats
            iterExpr walker bodyEnv body

        | Expr.LetOrUse(isRec = isRec; bindings = bindings; body = body) ->
            let isRecursive = isRec.IsSome

            for b in bindings do
                let rhsEnv = walker.EnterBindingRhs env isRecursive bindings b
                iterExpr walker rhsEnv b.expr

            match body with
            | ValueSome b ->
                let bodyEnv = walker.EnterLetBody env bindings
                iterExpr walker bodyEnv b
            | ValueNone -> ()

        | Expr.IfThenElse(condition = cond; thenExpr = thenE; elifBranches = elifs; elseBranch = elseB) ->
            iterExpr walker env cond
            iterExpr walker env thenE

            for elif_ in elifs do
                let elifCond, elifExpr =
                    match elif_ with
                    | ElifBranch.Elif(condition = c; expr = e)
                    | ElifBranch.ElseIf(condition = c; expr = e) -> c, e

                iterExpr walker env elifCond
                iterExpr walker env elifExpr

            match elseB with
            | ValueSome(ElseBranch(expr = elseExpr)) -> iterExpr walker env elseExpr
            | ValueNone -> ()

        | Expr.While(condition = cond; body = body) ->
            iterExpr walker env cond
            // Loop body has no new scope of its own; the cond/body share env.
            iterExpr walker env body

        | Expr.ForTo(ident = ident; startExpr = startE; endExpr = endE; body = body) ->
            iterExpr walker env startE
            iterExpr walker env endE
            let bodyEnv = walker.EnterForTo env ident
            iterExpr walker bodyEnv body

        | Expr.ForIn(pat = pat; enumerableExpr = src; body = body) ->
            iterExpr walker env src
            let bodyEnv = walker.EnterForIn env pat
            iterExpr walker bodyEnv body

        | Expr.Match(matchExpr = scrutinee; rules = Rules(rules = rules)) ->
            iterExpr walker env scrutinee
            iterRules walker env rules

        | Expr.Function(rules = Rules(rules = rules)) ->
            // `function …` is shorthand for `fun x -> match x with …`. The
            // scrutinee is implicit; only the arms are walked. NameResolution
            // doesn't see the synthesised parameter — that's modelled
            // entirely inside Unification's inferFunction.
            iterRules walker env rules

        | Expr.TryWith(expr = body; rules = Rules(rules = rules)) ->
            iterExpr walker env body
            iterRules walker env rules

        // TODO: Object needs its own iter helper — for now the Visit hook
        // still fires on the outer node so passes see it, just not its
        // inner Exprs.
        | Expr.Object _ -> ()

        | Expr.Record(fieldInitializers = inits) ->
            for FieldInitializer(expr = inner) in inits do
                iterExpr walker env inner

        | Expr.RecordClone(expr = src; fieldInitializers = inits) ->
            iterExpr walker env src

            for FieldInitializer(expr = inner) in inits do
                iterExpr walker env inner

        // Patterns can embed expressions (Pat.Expr); not walked yet. None of
        // the current passes care, and Pat traversal will get its own iter.
        | Expr.Pat _ -> ()

    and iterRules (walker: ExprWalker<'env>) (env: 'env) (rules: ImmutableArray<Rule<SyntaxToken>>) : unit =
        for r in rules do
            match r with
            | Rule.Rule(pat = pat; guard = guard; expr = body) ->
                let armEnv = walker.EnterMatchArm env pat

                match guard with
                | ValueSome(PatternGuard(expr = g)) -> iterExpr walker armEnv g
                | ValueNone -> ()

                iterExpr walker armEnv body
            | _ -> ()

    /// The module elements an analysis pass walks for an implementation file.
    /// A `namespace`-headed file contributes every group's elements in source
    /// order: the passes don't yet track namespace qualification (v1 has no
    /// namespace-scoped types), so the groups are concatenated and walked as a
    /// single element list — the same shape a module file already presents.
    ///
    /// A nested `module Foo = …` is flattened the same way: its body elements
    /// are spliced into the enclosing list (in source order, recursing through
    /// arbitrary nesting) rather than surfaced as a `ModuleElem.Module`. v1 has
    /// no module-scoped types, so every pass that walks `implFileElems` analyses
    /// a nested module's contents without needing its own `ModuleElem.Module`
    /// arm. (Proper module nesting / qualification is a later rung — see
    /// docs/selfhost-handoff.md G10.)
    let implFileElems (file: ImplementationFile<SyntaxToken>) : ModuleElems<SyntaxToken> =
        let b = ImmutableArray.CreateBuilder<ModuleElem<SyntaxToken>>()

        let rec add (elems: ModuleElems<SyntaxToken>) =
            for e in elems do
                match e with
                | ModuleElem.Module(ModuleDefn.ModuleDefn(body = ModuleDefnBody(elements = inner))) ->
                    match inner with
                    | ValueSome innerElems -> add innerElems
                    | ValueNone -> ()
                | _ -> b.Add e

        match file with
        | ImplementationFile.AnonymousModule elems -> add elems
        | ImplementationFile.NamedModule(NamedModule.NamedModule(elements = elems)) -> add elems
        | ImplementationFile.Namespaces groups ->
            for g in groups do
                match g with
                | NamespaceDeclGroup.Named(elements = elems)
                | NamespaceDeclGroup.Global(elements = elems) -> add elems

        b.ToImmutable()
