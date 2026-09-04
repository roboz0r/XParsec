namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Immutable
open XParsec.FSharp.Parser

// The BODY walks: every expression and pattern node, and the types an expression embeds. A
// `type` declaration's own type positions are declared STRUCTURE, and are walked apart from it.

module CstWalk =

    /// Every binding a member definition holds: one per accessor of a `with get`/`set`
    /// clause, one for a plain member, none for an auto-property (whose body is an
    /// initialiser, not a binding) or an abstract slot.
    let memberBindings (d: MethodOrPropDefn<'T>) : ImmutableArray<Binding<'T>> =
        match d with
        | MethodOrPropDefn.Method(defn = b)
        | MethodOrPropDefn.Property(defn = b) -> ImmutableArray.Create b
        | MethodOrPropDefn.PropertyWithGetSet(defns = bs) -> bs
        | MethodOrPropDefn.AutoProperty _
        | MethodOrPropDefn.AbstractSignature _ -> ImmutableArray.Empty

    type ExprWalker<'env> =
        {
            /// Called on every Expr node before recursing into its children.
            Visit: 'env -> Expr<SyntaxToken> -> unit
            /// Environment a lambda body sees.
            EnterFun: 'env -> ImmutableArray<Pat<SyntaxToken>> -> 'env
            /// Environment a binding's RHS sees; args are env, isRec, the whole enclosing
            /// binding group, this binding. Under `let rec` the RHS sees every sibling.
            EnterBindingRhs: 'env -> bool -> ImmutableArray<Binding<SyntaxToken>> -> Binding<SyntaxToken> -> 'env
            /// Environment a let body sees.
            EnterLetBody: 'env -> ImmutableArray<Binding<SyntaxToken>> -> 'env
            /// Environment a `for i = … do …` body sees; the argument is the loop
            /// variable's ident token.
            EnterForTo: 'env -> SyntaxToken -> 'env
            /// Environment a `for pat in xs do …` body sees.
            EnterForIn: 'env -> Pat<SyntaxToken> -> 'env
            /// Environment a match-arm's guard + body sees; the argument is its pattern.
            EnterMatchArm: 'env -> Pat<SyntaxToken> -> 'env
        }

    /// Visits every node, changes no environment.
    let identityExprWalker<'env> : ExprWalker<'env> =
        {
            Visit = fun _ _ -> ()
            EnterFun = fun env _ -> env
            EnterBindingRhs = fun env _ _ _ -> env
            EnterLetBody = fun env _ -> env
            EnterForTo = fun env _ -> env
            EnterForIn = fun env _ -> env
            EnterMatchArm = fun env _ -> env
        }

    /// `Expr.LetOrUse(body = ValueNone)` is `use fixed`, which pins a managed value to a
    /// pointer.
    let requireLetBody (body: Expr<SyntaxToken> voption) : Expr<SyntaxToken> =
        match body with
        | ValueSome b -> b
        | ValueNone -> failwith "Expr.LetOrUse with no body (UseFixed) not supported"

    // An `Enter*` scope change applies only to the child traversal it is handed to;
    // nothing accumulates across siblings.
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

        // Interpolated-string hole exprs share the enclosing scope (no new bindings).
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

        | Expr.LibraryOnlyStaticOptimization(defaultExpr = d; clauses = clauses) ->
            iterExpr walker env d

            for clause in clauses do
                iterExpr walker env clause.OptimizedExpr

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
            // `function …` is shorthand for `fun x -> match x with …`: the scrutinee is
            // implicit, so only the arms are walked.
            iterRules walker env rules

        | Expr.TryWith(expr = body; rules = Rules(rules = rules)) ->
            iterExpr walker env body
            iterRules walker env rules

        | Expr.Object(baseCall = baseCall; members = members; interfaceImpls = interfaceImpls) ->
            iterObjectExpr walker env baseCall members interfaceImpls

        | Expr.Record(fieldInitializers = inits) ->
            for FieldInitializer(expr = inner) in inits do
                iterExpr walker env inner

        | Expr.RecordClone(expr = src; fieldInitializers = inits) ->
            iterExpr walker env src

            for FieldInitializer(expr = inner) in inits do
                iterExpr walker env inner

        // Patterns can embed expressions (`Pat.Expr`); not walked here.
        | Expr.Pat _ -> ()

    and private iterObjectMembers
        (walker: ExprWalker<'env>)
        (env: 'env)
        (defns: ImmutableArray<MemberDefn<SyntaxToken>>)
        : unit =
        for d in defns do
            match d with
            | MemberDefn.Member(defn = mdef) ->
                match mdef with
                | MethodOrPropDefn.Method(defn = b)
                | MethodOrPropDefn.Property(defn = b) -> iterExpr walker env b.expr
                | MethodOrPropDefn.PropertyWithGetSet(defns = bs) ->
                    for b in bs do
                        iterExpr walker env b.expr
                | MethodOrPropDefn.AutoProperty(expr = e) -> iterExpr walker env e
                | MethodOrPropDefn.AbstractSignature _ -> ()
            | MemberDefn.Value _ -> ()
            | MemberDefn.AdditionalConstructor _ ->
                // Secondary ctors in an object expression are not a legal F# shape.
                failwith "CstWalk.iterObjectMembers: TODO AdditionalConstructor in object expression"

    and private iterObjectExpr
        (walker: ExprWalker<'env>)
        (env: 'env)
        (baseCall: BaseCall<SyntaxToken>)
        (members: ObjectMembers<SyntaxToken>)
        (interfaceImpls: ImmutableArray<InterfaceImpl<SyntaxToken>>)
        : unit =
        let construction =
            match baseCall with
            | BaseCall.AnonBaseCall c
            | BaseCall.NamedBaseCall(construction = c) -> c

        match construction with
        | ObjectConstruction.ObjectConstruction(expr = e) -> iterExpr walker env e
        // `interface Foo with …` carries a type name only, so there is no argument expression.
        | ObjectConstruction.InterfaceConstruction _ -> ()

        let (ObjectMembers(memberDefns = memberDefns)) = members
        iterObjectMembers walker env memberDefns

        for InterfaceImpl.InterfaceImpl(objectMembers = objMembers) in interfaceImpls do
            match objMembers with
            | ValueSome(ObjectMembers(memberDefns = intfDefns)) -> iterObjectMembers walker env intfDefns
            | ValueNone -> ()

    and private iterRules (walker: ExprWalker<'env>) (env: 'env) (rules: ImmutableArray<Rule<SyntaxToken>>) : unit =
        for r in rules do
            match r with
            | Rule.Rule(pat = pat; guard = guard; expr = body) ->
                let armEnv = walker.EnterMatchArm env pat

                match guard with
                | ValueSome(PatternGuard(expr = g)) -> iterExpr walker armEnv g
                | ValueNone -> ()

                iterExpr walker armEnv body
            | _ -> ()

    /// Every `Type` (and member-signature) node syntactically embedded in ONE expression
    /// node: its OWN types only; child expressions are `iterExpr`'s job. Pattern
    /// annotations belong to the pattern walk; a binding contributes its return type.
    let iterExprEmbeddedTypes (it: CstTypeWalk.TypeIter) (e: Expr<SyntaxToken>) : unit =
        let onType = CstTypeWalk.iterType it
        let onMemberSig = CstTypeWalk.iterTypeMemberSig it

        let memberDefnSigs (defns: ImmutableArray<MemberDefn<SyntaxToken>>) : unit =
            for d in defns do
                CstTypeWalk.iterMemberDefnSigTypes it ignore d

        match e with
        // A measured literal's annotation (`1.0<m>`) is a measure position.
        | Expr.Const(Constant.MeasuredLiteral(measure = m)) -> CstTypeWalk.iterMeasure it m

        // No directly-embedded `Type`.
        | Expr.Const _
        | Expr.EmptyBlock _
        | Expr.LongIdentOrOp _
        | Expr.OptionalArgExpr _
        | Expr.Null _
        | Expr.Wildcard _
        | Expr.Missing
        | Expr.SkipsTokens _
        | Expr.Ident _
        | Expr.SliceAll _
        | Expr.String _
        | Expr.EnclosedBlock _
        | Expr.DotLookup _
        | Expr.PrefixApp _
        | Expr.DynamicLookup _
        | Expr.ControlFlow _
        | Expr.SliceFrom _
        | Expr.SliceTo _
        | Expr.App _
        | Expr.HighPrecedenceApp _
        | Expr.InfixApp _
        | Expr.Assignment _
        | Expr.IndexedLookup _
        | Expr.Tuple _
        | Expr.StructTuple _
        | Expr.Sequential _
        | Expr.TryFinally _
        | Expr.Range _
        | Expr.SliceFromTo _
        | Expr.SteppedRange _
        | Expr.Fun _
        | Expr.IfThenElse _
        | Expr.While _
        | Expr.ForTo _
        | Expr.ForIn _
        | Expr.Match _
        | Expr.Function _
        | Expr.TryWith _
        | Expr.Record _
        | Expr.RecordClone _
        | Expr.Pat _ -> ()

        | Expr.New(typ = t)
        | Expr.TypeAnnotation(typ = t)
        | Expr.StaticUpcast(typ = t)
        | Expr.DynamicTypeTest(typ = t)
        | Expr.DynamicDowncast(typ = t) -> onType t

        // An inline-IL body's result annotation (`(# "…" : T #)`). Its type-arg
        // slot (`type('T)`) carries raw tokens, not a `Type` node, so there is
        // nothing to visit there.
        | Expr.ILIntrinsic(returnType = rt) ->
            match rt with
            | ValueSome(ReturnType(typ = t)) -> onType t
            | ValueNone -> ()

        | Expr.TypeApp(types = types) ->
            for t in types do
                onType t

        | Expr.LetOrUse(bindings = bindings) ->
            for b in bindings do
                CstTypeWalk.iterBindingReturnType onType b

        // An SRTP member-trait invocation (`((^T): (static member …) args`).
        | Expr.StaticMemberInvocation(membersign = ms) -> onMemberSig ms

        // A static-optimization clause's tycon-equality constraint writes a type on
        // its RHS (`when ^T : int` / `when ^T : System.DateTime`).
        | Expr.LibraryOnlyStaticOptimization(clauses = clauses) ->
            for clause in clauses do
                for c in clause.Constraints do
                    match c with
                    | StaticOptimizationConstraint.WhenTyparTyconEqualsTycon(rhsType = rhs) -> onType rhs
                    | StaticOptimizationConstraint.WhenTyparIsStruct _ -> ()

        | Expr.Object(baseCall = baseCall; members = members; interfaceImpls = impls) ->
            let ctorTy =
                match baseCall with
                | BaseCall.AnonBaseCall c
                | BaseCall.NamedBaseCall(construction = c) ->
                    match c with
                    | ObjectConstruction.ObjectConstruction(typ = t)
                    | ObjectConstruction.InterfaceConstruction(typ = t) -> t

            onType ctorTy

            let (ObjectMembers(memberDefns = memberDefns)) = members
            memberDefnSigs memberDefns

            for InterfaceImpl.InterfaceImpl(typ = t; objectMembers = objMembers) in impls do
                onType t

                match objMembers with
                | ValueSome(ObjectMembers(memberDefns = ds)) -> memberDefnSigs ds
                | ValueNone -> ()

    /// `VisitPat` fires on every `Pat` node before its children; returning `false` skips
    /// the default child recursion.
    [<NoEquality; NoComparison>]
    type PatIter =
        {
            VisitPat: PatIter -> Pat<SyntaxToken> -> bool
        }

    let rec iterPat (it: PatIter) (p: Pat<SyntaxToken>) : unit =
        if it.VisitPat it p then
            let walk = iterPat it

            match p with
            | Pat.EnclosedBlock(pat = inner)
            | Pat.Typed(pat = inner)
            | Pat.Attributed(pat = inner)
            | Pat.As(pat = inner)
            | Pat.Optional(pat = inner)
            | Pat.TypeTestAs(pat = inner) -> walk inner
            | Pat.Named(argumentPats = args)
            | Pat.OpNamed(argumentPats = args) ->
                for sub in args do
                    walk sub
            | Pat.NamedFieldPats(args = args) ->
                for a in args do
                    match a with
                    | UnionArgPat.Named(pat = sub)
                    | UnionArgPat.Positional(pat = sub) -> walk sub
            | Pat.Tuple(patterns = pats)
            | Pat.StructTuple(patterns = pats)
            | Pat.Elems(pats = pats) ->
                for sub in pats do
                    walk sub
            | Pat.Record(fieldPats = fieldPats) ->
                for FieldPat(pat = sub) in fieldPats do
                    walk sub
            | Pat.Cons(head = a; tail = b)
            | Pat.Or(left = a; right = b)
            | Pat.And(left = a; right = b) ->
                walk a
                walk b
            // Leaves: no sub-pattern. `TypeTest` carries a written type but no inner
            // pattern; `Pat.Expr` embeds an *expression*, not walked here.
            | Pat.NamedSimple _
            | Pat.TypeTest _
            | Pat.Const _
            | Pat.EmptyBlock _
            | Pat.Wildcard _
            | Pat.Null _
            | Pat.Op _
            | Pat.String _
            | Pat.Expr _
            | Pat.Missing
            | Pat.SkipsTokens _ -> ()
