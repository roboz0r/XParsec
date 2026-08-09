namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Immutable
open XParsec.FSharp.Parser

/// An `open` written in this unit, with the positional facts the flat dotted `Prefixes`
/// list cannot express.
[<NoComparison>]
type LocalOpen =
    {
        /// The dotted path as WRITTEN (`"A"`, `"N.A"`).
        Path: string
        /// The dotted SOURCE path of the scope the `open` is written in (`"N.M"`; `""` at
        /// the top of an anonymous module).
        Scope: string
        /// How many `module`s enclose the `open` (a namespace body is 0).
        ScopeDepth: int
        /// Source offset of the `open` keyword: what orders it against the declarations
        /// of its own scope.
        Offset: int
    }

/// The active namespace prefixes in a lexical scope, most-recent-first (a later `open`
/// shadows an earlier one). Under `open System.Collections.Generic`, a bare
/// `EqualityComparer` qualifies to `System.Collections.Generic.EqualityComparer`.
type OpenScope =
    {
        /// Empty prefixes are never stored.
        Prefixes: string list
        /// The opens WRITTEN IN THIS FILE, with their positions: what orders a name an
        /// `open` brings against the declarations around it.
        Locals: LocalOpen list
        /// Module-abbrev aliases (`module R = A.B.C` ⇒ `"R" → "A.B.C"`), expanded
        /// on the anchor segment of a dotted name before probing.
        Abbrevs: Map<string, string>
    }

module OpenScope =

    let empty: OpenScope =
        {
            Prefixes = []
            Locals = []
            Abbrevs = Map.empty
        }

    let private candidates (scope: OpenScope) (name: string) : string list =
        let expanded =
            let dot = name.IndexOf '.'
            let anchor = if dot < 0 then name else name.Substring(0, dot)

            match Map.tryFind anchor scope.Abbrevs with
            | Some target -> target + (if dot < 0 then "" else name.Substring dot)
            | None -> name

        expanded
        :: [
            for p in scope.Prefixes do
                if p.Length > 0 then
                    yield p + "." + expanded
        ]

    /// Resolve `name` to a value via `lookup`, trying the bare/abbrev-expanded name then
    /// each active prefix; first hit wins.
    let tryResolve (scope: OpenScope) (lookup: string -> 'a voption) (name: string) : 'a voption =
        let rec go cs =
            match cs with
            | [] -> ValueNone
            | c :: rest ->
                match lookup c with
                | ValueSome _ as r -> r
                | ValueNone -> go rest

        go (candidates scope name)

    /// The fully-qualified name `name` resolves under, trying the bare/abbrev-expanded
    /// name then each active prefix; first `probe` hit wins.
    let tryQualify (scope: OpenScope) (probe: string -> bool) (name: string) : string voption =
        let rec go cs =
            match cs with
            | [] -> ValueNone
            | c :: rest -> if probe c then ValueSome c else go rest

        go (candidates scope name)

/// Helpers for projecting `TypeDefn` shapes. The parser emits `TypeDefn.Anon` for the
/// bare `type C(…) = member …` form with no `class`/`end`; every projection here treats
/// it as `TypeDefn.Class`.
module TypeDefnPatterns =

    [<NoEquality; NoComparison>]
    type ClassLikeDecl<'T> =
        {
            TypeName: TypeName<'T>
            PrimaryConstr: PrimaryConstrArgs<'T> voption
            AsDefn: AsDefn<'T> voption
            Body: ObjectModelBody<'T>
        }

    /// The `Class`, `Anon` and `Struct` shapes only; `ValueNone` for every other one.
    let tryClassLikeDecl (td: TypeDefn<'T>) : ClassLikeDecl<'T> voption =
        match td with
        | TypeDefn.Class(typeName = tn; primaryConstr = pc; asDefn = asD; body = body)
        | TypeDefn.Anon(typeName = tn; primaryConstr = pc; asDefn = asD; body = body)
        | TypeDefn.Struct(typeName = tn; primaryConstr = pc; asDefn = asD; body = body) ->
            ValueSome
                {
                    TypeName = tn
                    PrimaryConstr = pc
                    AsDefn = asD
                    Body = body
                }
        | _ -> ValueNone

    /// `true` for the explicit `type X = struct … end` shape: a value type even without
    /// a `[<Struct>]` attribute, which instead lands as `Class`/`Anon`.
    let isStructShape (td: TypeDefn<'T>) : bool =
        match td with
        | TypeDefn.Struct _ -> true
        | _ -> false

    /// `true` for the explicit `interface … end` shape, or the all-abstract object-model
    /// form (`type IFoo = abstract member …`).
    let isInterfaceShape (td: TypeDefn<'T>) : bool =
        match td with
        | TypeDefn.Interface _ -> true
        | _ ->
            match tryClassLikeDecl td with
            | ValueNone -> false
            | ValueSome d ->
                d.PrimaryConstr.IsNone
                && d.Body.inherits.IsNone
                && d.Body.classPreamble.IsEmpty
                && not d.Body.elements.IsEmpty
                && d.Body.elements
                   |> Seq.forall (fun el ->
                       match el with
                       | TypeDefnElement.Member(MemberDefn.Member(defn = MethodOrPropDefn.AbstractSignature _)) -> true
                       | _ -> false
                   )

    /// A single-ident `Union` or `Record` name with its `with`-block elements: the
    /// channel an `interface … with` / augmentation member rides.
    let tryNonClassMemberHostDecl (td: TypeDefn<'T>) : struct (TypeName<'T> * TypeDefnElements<'T> voption) voption =
        let extElems (ext: TypeExtensionElements<'T> voption) =
            match ext with
            | ValueSome(TypeExtensionElements(elements = elems)) -> ValueSome elems
            | ValueNone -> ValueNone

        match td with
        | TypeDefn.Union(typeName = (TypeName(ident = nameLi) as tn); extensions = ext)
        | TypeDefn.Record(typeName = (TypeName(ident = nameLi) as tn); extensions = ext) when nameLi.Idents.Length = 1 ->
            ValueSome(struct (tn, extElems ext))
        // An inline intrinsic-abbrev augmented with `with member …`
        // (`type X = (# … #) with member …`) hosts its members on the same path.
        | TypeDefn.Abbrev(typeName = (TypeName(ident = nameLi) as tn); extensions = ext & ValueSome _) when
            nameLi.Idents.Length = 1
            ->
            ValueSome(struct (tn, extElems ext))
        | _ -> ValueNone

    /// The `body` shared by `Class | Anon | Struct | Interface`; `ValueNone` otherwise.
    let tryObjectModelBody (td: TypeDefn<'T>) : ObjectModelBody<'T> voption =
        match td with
        | TypeDefn.Class(body = b)
        | TypeDefn.Anon(body = b)
        | TypeDefn.Struct(body = b)
        | TypeDefn.Interface(body = b) -> ValueSome b
        | _ -> ValueNone

/// The declaring containment of an element: the `namespace` group it sits in (dotted;
/// `""` for an anonymous / global / named-module file) and the `module` declarations it
/// is nested in.
type DeclContainment<'T> =
    {
        Namespace: string
        /// Outermost first.
        Modules: ModuleDefn<'T> list
    }

module DeclContainment =

    let ofNamespace (ns: string) : DeclContainment<'T> = { Namespace = ns; Modules = [] }

    let enter (md: ModuleDefn<'T>) (c: DeclContainment<'T>) : DeclContainment<'T> =
        { c with Modules = c.Modules @ [ md ] }

    /// `None` for the global namespace / file module.
    let namespaceOpt (c: DeclContainment<'T>) : string option =
        if c.Namespace = "" then None else Some c.Namespace

    /// The dotted SOURCE path of this containment (`"N.A.B"`; `""` at the top of an
    /// anonymous module): the namespace plus each enclosing module's name AS WRITTEN,
    /// never its compiled module name (`ListModule`).
    let sourcePath (nameOf: 'T -> string) (c: DeclContainment<'T>) : string =
        let mutable path = c.Namespace

        for ModuleDefn.ModuleDefn(ident = ident) in c.Modules do
            let seg = nameOf ident
            path <- if path.Length = 0 then seg else path + "." + seg

        path

/// One flattened leaf element of a module tree, with the ambient facts a pass needs at
/// that position.
type WalkedElem<'T> =
    {
        Elem: ModuleElem<'T>
        Scope: OpenScope
        Containment: DeclContainment<'T>
        /// Source offset of the `module` / `namespace` keyword of the INNERMOST enclosing
        /// `rec` scope, `ValueNone` outside one. Under `rec` a declaration is visible from
        /// the top of that scope rather than from where it is written.
        RecScopeOffset: int voption
    }

module CstWalk =

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
    let iterExprEmbeddedTypes
        (onType: Type<SyntaxToken> -> unit)
        (onMemberSig: MemberSig<SyntaxToken> -> unit)
        (e: Expr<SyntaxToken>)
        : unit =
        let bindingSig (b: Binding<SyntaxToken>) : unit =
            match b.returnType with
            | ValueSome(ReturnType(typ = t)) -> onType t
            | ValueNone -> ()

        let memberDefnSigs (defns: ImmutableArray<MemberDefn<SyntaxToken>>) : unit =
            for d in defns do
                match d with
                | MemberDefn.Member(defn = mdef) ->
                    match mdef with
                    | MethodOrPropDefn.Method(defn = b)
                    | MethodOrPropDefn.Property(defn = b) -> bindingSig b
                    | MethodOrPropDefn.PropertyWithGetSet(defns = bs) ->
                        for b in bs do
                            bindingSig b
                    | MethodOrPropDefn.AutoProperty(returnType = rt) ->
                        match rt with
                        | ValueSome(ReturnType(typ = t)) -> onType t
                        | ValueNone -> ()
                    | MethodOrPropDefn.AbstractSignature(sign = ms) -> onMemberSig ms
                | MemberDefn.Value _
                | MemberDefn.AdditionalConstructor _ -> ()

        match e with
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
                bindingSig b

        // An SRTP member-trait invocation (`((^T): (static member …) args`).
        | Expr.StaticMemberInvocation(membersign = ms) -> onMemberSig ms

        // A static-optimization clause's tycon-equality constraint names a type on
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

    /// `VisitType` fires on every `Type` node before its children; returning `false`
    /// skips the default child recursion.
    [<NoEquality; NoComparison>]
    type TypeIter =
        {
            VisitType: TypeIter -> Type<SyntaxToken> -> bool
        }

    let identityTypeIter: TypeIter = { VisitType = fun _ _ -> true }

    let rec iterType (it: TypeIter) (ty: Type<SyntaxToken>) : unit =
        if it.VisitType it ty then
            let walk = iterType it

            match ty with
            | Type.ParenType(typ = inner)
            | Type.SuffixedType(baseType = inner)
            | Type.DottedType(baseType = inner)
            | Type.ArrayType(baseType = inner)
            | Type.AnonymousSubtype(typ = inner)
            | Type.SubtypeConstraint(typ = inner) -> walk inner
            | Type.FunctionType(fromType = f; toType = t) ->
                walk f
                walk t
            | Type.TupleType(types = ts)
            | Type.StructTupleType(types = ts) ->
                for t in ts do
                    walk t
            | Type.GenericType(typeArgs = args) ->
                for a in args do
                    match a with
                    | TypeArg.Type at -> walk at
                    // A measure arg carries no `Type` node.
                    | TypeArg.Measure _ -> ()
            | Type.WhenConstrainedType(typ = inner; constraints = cs) ->
                walk inner
                iterTypeConstraints it cs
            | Type.UnionType(left = l; right = r) ->
                walk l
                walk r
            | Type.AnonRecordType(fields = fs) ->
                for AnonRecordField(typ = t) in fs do
                    walk t
            // Leaves: no nested `Type`.
            | Type.VarType _
            | Type.NamedType _
            | Type.Null _
            | Type.MeasureType _
            | Type.ILIntrinsic _
            | Type.Missing
            | Type.SkipsTokens _ -> ()

    and iterTypeConstraints (it: TypeIter) (cs: TyparConstraints<SyntaxToken>) : unit =
        for c in cs.Constraints do
            iterTypeConstraint it c

    and iterTypeConstraint (it: TypeIter) (c: Constraint<SyntaxToken>) : unit =
        match c with
        | Constraint.Coercion(typ = t)
        | Constraint.Enum(typ = t)
        | Constraint.Default(typ = t) -> iterType it t
        | Constraint.Delegate(type1 = t1; type2 = t2) ->
            iterType it t1
            iterType it t2
        | Constraint.MemberTrait(membersign = ms) -> iterTypeMemberSig it ms
        // Constraints with no embedded `Type`.
        | Constraint.Nullness _
        | Constraint.DefaultConstructor _
        | Constraint.Struct _
        | Constraint.ReferenceType _
        | Constraint.NotNull _
        | Constraint.Unmanaged _
        | Constraint.Equality _
        | Constraint.Comparison _ -> ()

    and iterTypeMemberSig (it: TypeIter) (ms: MemberSig<SyntaxToken>) : unit =
        match ms with
        | MemberSig.MethodOrPropSig(sign = cs)
        | MemberSig.PropSig(sign = cs) -> iterTypeCurriedSig it cs

    and iterTypeCurriedSig (it: TypeIter) (cs: CurriedSig<SyntaxToken>) : unit =
        let (CurriedSig(args = argGroups; returnType = ret)) = cs

        for struct (argsSpec, _) in argGroups do
            let (ArgsSpec.ArgsSpec(args = args)) = argsSpec

            for (ArgSpec(typ = t)) in args do
                iterType it t

        iterType it ret

    /// An uncurried signature: a `DelegateSig`, or a GADT-syntax union case's
    /// `Name : arg * arg -> ret`.
    and iterTypeUncurriedSig (it: TypeIter) (sign: UncurriedSig<SyntaxToken>) : unit =
        let (UncurriedSig(args = ArgsSpec.ArgsSpec(args = args); returnType = ret)) = sign

        for (ArgSpec(typ = t)) in args do
            iterType it t

        iterType it ret

    /// The type positions a `type` definition's DECLARED STRUCTURE writes; member bodies
    /// are not part of it.
    let iterTypeDefnTypes
        (it: TypeIter)
        (onPat: Pat<SyntaxToken> -> unit)
        (onInherit: Type<SyntaxToken> -> unit)
        (td: TypeDefn<SyntaxToken>)
        : unit =
        let ty (t: Type<SyntaxToken>) = iterType it t

        let unionField (f: UnionTypeField<SyntaxToken>) =
            match f with
            | UnionTypeField.Unnamed(typ = t)
            | UnionTypeField.Named(typ = t) -> ty t

        let unionCase (UnionTypeCase(data = data)) =
            match data with
            | UnionTypeCaseData.Nullary _ -> ()
            | UnionTypeCaseData.Nary(fields = fs) ->
                for f in fs do
                    unionField f
            | UnionTypeCaseData.GadtNary(sign = s) -> iterTypeUncurriedSig it s
            | UnionTypeCaseData.GadtNullary(typ = t) -> ty t

        let returnTypeOf (b: Binding<SyntaxToken>) =
            match b.returnType with
            | ValueSome(ReturnType(typ = t)) -> ty t
            | ValueNone -> ()

        // A member's declared signature is its argument annotations plus its return type;
        // the annotations are pattern-embedded, so they go through `onPat`.
        let memberSig (b: Binding<SyntaxToken>) =
            for ap in b.argumentPats do
                onPat ap

            returnTypeOf b

        let methodOrProp (d: MethodOrPropDefn<SyntaxToken>) =
            match d with
            | MethodOrPropDefn.Method(defn = b)
            | MethodOrPropDefn.Property(defn = b) -> memberSig b
            | MethodOrPropDefn.PropertyWithGetSet(defns = bs) ->
                for b in bs do
                    memberSig b
            | MethodOrPropDefn.AutoProperty(returnType = ValueSome(ReturnType(typ = t))) -> ty t
            | MethodOrPropDefn.AutoProperty _ -> ()
            | MethodOrPropDefn.AbstractSignature sign -> iterTypeMemberSig it sign

        let memberDefn (md: MemberDefn<SyntaxToken>) =
            match md with
            | MemberDefn.Member(defn = d) -> methodOrProp d
            | MemberDefn.Value(typ = t) -> ty t
            | MemberDefn.AdditionalConstructor(pat = p) -> onPat p

        let element (el: TypeDefnElement<SyntaxToken>) =
            match el with
            | TypeDefnElement.Member md -> memberDefn md
            | TypeDefnElement.InterfaceImpl(InterfaceImpl.InterfaceImpl(typ = t; objectMembers = oms)) ->
                ty t

                match oms with
                | ValueSome(ObjectMembers(memberDefns = mds)) ->
                    for md in mds do
                        memberDefn md
                | ValueNone -> ()
            | TypeDefnElement.InterfaceSpec(InterfaceSpec(typ = t)) -> ty t
            | TypeDefnElement.Inherit(ClassInheritsDecl(typ = t)) -> onInherit t

        // A `[static] let` in a class preamble is a BODY, not declared structure, so only its
        // return annotation is part of the type's surface.
        let preamble (d: ClassFunctionOrValueDefn<SyntaxToken>) =
            match d with
            | ClassFunctionOrValueDefn.LetBindings(bindings = bs) ->
                for b in bs do
                    returnTypeOf b
            | ClassFunctionOrValueDefn.Do _ -> ()

        let body (b: ObjectModelBody<SyntaxToken>) =
            match b.inherits with
            | ValueSome(ClassInheritsDecl(typ = t)) -> onInherit t
            | ValueNone -> ()

            for d in b.classPreamble do
                preamble d

            for el in b.elements do
                element el

        let extensions (ext: TypeExtensionElements<SyntaxToken> voption) =
            match ext with
            | ValueSome(TypeExtensionElements(elements = els)) ->
                for el in els do
                    element el
            | ValueNone -> ()

        // A type header's `when` clause (`type M<'F when 'F :> …>`) lives on its
        // `TypeName`, reachable from no field, member or parameter.
        let headerConstraints (tn: TypeName<SyntaxToken>) =
            let (TypeName(typarDefns = tds; postfixConstraints = post)) = tn

            match tds with
            | ValueSome(TyparDefns(constraints = ValueSome cs)) -> iterTypeConstraints it cs
            | _ -> ()

            match post with
            | ValueSome cs -> iterTypeConstraints it cs
            | ValueNone -> ()

        match td with
        | TypeDefn.Abbrev(typeName = tn)
        | TypeDefn.Record(typeName = tn)
        | TypeDefn.Union(typeName = tn)
        | TypeDefn.Anon(typeName = tn)
        | TypeDefn.Class(typeName = tn)
        | TypeDefn.Struct(typeName = tn)
        | TypeDefn.Interface(typeName = tn)
        | TypeDefn.Delegate(typeName = tn)
        | TypeDefn.TypeExtension(typeName = tn)
        | TypeDefn.Enum(typeName = tn)
        | TypeDefn.AbstractType(typeName = tn) -> headerConstraints tn
        | TypeDefn.Missing
        | TypeDefn.SkipsTokens _ -> ()

        match td with
        | TypeDefn.Abbrev(typ = t; extensions = ext) ->
            ty t
            extensions ext
        | TypeDefn.Record(fields = fs; extensions = ext) ->
            for RecordField(typ = t) in fs do
                ty t

            extensions ext
        | TypeDefn.Union(cases = cs; extensions = ext) ->
            for c in cs do
                unionCase c

            extensions ext
        | TypeDefn.Anon(primaryConstr = pc; body = b)
        | TypeDefn.Class(primaryConstr = pc; body = b)
        | TypeDefn.Struct(primaryConstr = pc; body = b) ->
            // Primary-constructor parameter annotations (`type Point(x: int, …)`) are
            // pattern-embedded.
            match pc with
            | ValueSome(PrimaryConstrArgs(pat = ValueSome p)) -> onPat p
            | _ -> ()

            body b
        | TypeDefn.Interface(body = b) -> body b
        | TypeDefn.Delegate(sign = DelegateSig(sign = s)) -> iterTypeUncurriedSig it s
        | TypeDefn.TypeExtension(elements = TypeExtensionElements(elements = els)) ->
            for el in els do
                element el
        | TypeDefn.Enum _
        | TypeDefn.AbstractType _
        | TypeDefn.Missing
        | TypeDefn.SkipsTokens _ -> ()

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

    /// The module elements an analysis pass walks for an implementation file, FLATTENED:
    /// every `namespace` group's elements in source order, and a nested `module Foo = …`
    /// spliced into the enclosing list rather than surfaced as a `ModuleElem.Module`.
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

    /// Scope-preserving sibling of `implFileElems`: the same flattened leaf elements, each
    /// paired with the `OpenScope` active at its position, and `onScope` fired once per
    /// body entered. A non-rec scope accumulates; under `rec` every `open` covers it all.
    let walkModuleTreeWith
        (nameOf: SyntaxToken -> string)
        (ambient: OpenScope)
        (onScope: ModuleElems<SyntaxToken> -> bool -> unit)
        (file: ImplementationFile<SyntaxToken>)
        : WalkedElem<SyntaxToken> list =
        let out = ResizeArray<WalkedElem<SyntaxToken>>()

        let longIdentText (li: LongIdent<SyntaxToken>) : string =
            li.Idents |> Seq.map nameOf |> String.concat "."

        // The implicit prefix a `namespace N` header contributes: a dotted prefix and no
        // `LocalOpen`, because the namespace holds the body rather than importing it.
        let addNamespacePrefix (scope: OpenScope) (li: LongIdent<SyntaxToken>) : OpenScope =
            let prefix = longIdentText li

            if prefix.Length = 0 then
                scope
            else
                { scope with
                    Prefixes = prefix :: scope.Prefixes
                }

        let addOpen
            (scope: OpenScope)
            (containment: DeclContainment<SyntaxToken>)
            (openToken: SyntaxToken)
            (li: LongIdent<SyntaxToken>)
            : OpenScope =
            let prefix = longIdentText li

            if prefix.Length = 0 then
                scope
            else
                { scope with
                    Prefixes = prefix :: scope.Prefixes
                    Locals =
                        {
                            Path = prefix
                            Scope = DeclContainment.sourcePath nameOf containment
                            ScopeDepth = List.length containment.Modules
                            Offset = openToken.StartIndex
                        }
                        :: scope.Locals
                }

        let addAbbrev (scope: OpenScope) (alias: string) (target: string) : OpenScope =
            if alias.Length = 0 || target.Length = 0 then
                scope
            else
                { scope with
                    Abbrevs = Map.add alias target scope.Abbrevs
                }

        // `open type` is a member channel, not a namespace prefix, so only
        // `ImportDecl.ImportDecl` contributes a prefix.
        let accumulate
            (containment: DeclContainment<SyntaxToken>)
            (scope: OpenScope)
            (e: ModuleElem<SyntaxToken>)
            : OpenScope =
            match e with
            | ModuleElem.Import(ImportDecl.ImportDecl(openToken = kw; longIdent = li)) ->
                addOpen scope containment kw li
            | ModuleElem.ModuleAbbrev(ModuleAbbrev.ModuleAbbrev(ident = id; longIdent = li)) ->
                addAbbrev scope (nameOf id) (longIdentText li)
            | _ -> scope

        // The innermost enclosing rec scope wins.
        let innerRecScope (keyword: SyntaxToken) (isRec: SyntaxToken voption) (inherited: int voption) : int voption =
            if isRec.IsSome then
                ValueSome keyword.StartIndex
            else
                inherited

        // `isRec` is the scope's OWN rec flag (drives the constant-prelude shape).
        // `recScope` is the PROPAGATED one, so `onScope`'s flag is true in a rec
        // namespace's non-rec submodule too.
        let rec processElems
            (elems: ModuleElems<SyntaxToken>)
            (start: OpenScope)
            (isRec: bool)
            (recScope: int voption)
            (containment: DeclContainment<SyntaxToken>)
            : unit =
            onScope elems recScope.IsSome

            if isRec then
                // Constant prelude: every open/abbrev in this scope applies to the whole
                // body, regardless of position.
                let constScope = (start, elems) ||> Seq.fold (accumulate containment)

                for e in elems do
                    emit e constScope recScope containment
            else
                let mutable s = start

                for e in elems do
                    emit e s recScope containment
                    s <- accumulate containment s e

        and emit
            (e: ModuleElem<SyntaxToken>)
            (scope: OpenScope)
            (recScope: int voption)
            (containment: DeclContainment<SyntaxToken>)
            : unit =
            match e with
            | ModuleElem.Module((ModuleDefn.ModuleDefn(
                moduleToken = kw; isRec = innerRec; body = ModuleDefnBody(elements = inner))) as md) ->
                // A module is a *container*: it extends the containment's module chain and
                // leaves its `Namespace` alone.
                match inner with
                | ValueSome innerElems ->
                    processElems
                        innerElems
                        scope
                        innerRec.IsSome
                        (innerRecScope kw innerRec recScope)
                        (DeclContainment.enter md containment)
                | ValueNone -> ()
            | _ ->
                out.Add
                    {
                        Elem = e
                        Scope = scope
                        Containment = containment
                        RecScopeOffset = recScope
                    }

        let top = DeclContainment.ofNamespace ""

        match file with
        | ImplementationFile.AnonymousModule elems -> processElems elems ambient false ValueNone top
        | ImplementationFile.NamedModule(NamedModule.NamedModule(moduleToken = kw; isRec = isRec; elements = elems)) ->
            processElems elems ambient isRec.IsSome (innerRecScope kw isRec ValueNone) top
        | ImplementationFile.Namespaces groups ->
            for g in groups do
                match g with
                | NamespaceDeclGroup.Named(namespaceToken = kw; isRec = isRec; longIdent = nsLi; elements = elems) ->
                    processElems
                        elems
                        (addNamespacePrefix ambient nsLi)
                        isRec.IsSome
                        (innerRecScope kw isRec ValueNone)
                        (DeclContainment.ofNamespace (longIdentText nsLi))
                | NamespaceDeclGroup.Global(elements = elems) -> processElems elems ambient false ValueNone top

        List.ofSeq out

    /// Scope-preserving walk for a consumer that needs neither the per-scope hook nor the
    /// declaring containment.
    let walkModuleTree
        (nameOf: SyntaxToken -> string)
        (ambient: OpenScope)
        (file: ImplementationFile<SyntaxToken>)
        : (ModuleElem<SyntaxToken> * OpenScope) list =
        walkModuleTreeWith nameOf ambient (fun _ _ -> ()) file
        |> List.map (fun w -> w.Elem, w.Scope)

    /// The `.fsi` analogue of `implFileElems`: every `namespace` group's elements in
    /// source order, with nested `module Foo = …` bodies spliced into the enclosing list.
    let sigFileElems (file: SignatureFile<SyntaxToken>) : ModuleSignatureElements<SyntaxToken> =
        let b = ImmutableArray.CreateBuilder<ModuleSignatureElement<SyntaxToken>>()

        let rec add (elems: ModuleSignatureElements<SyntaxToken>) =
            for e in elems do
                match e with
                | ModuleSignatureElement.Module(ModuleSignature.ModuleSignature(
                    body = ModuleSignatureBody(elements = inner))) -> add inner
                | _ -> b.Add e

        match file with
        | SignatureFile.AnonymousModule elems -> add elems
        | SignatureFile.NamedModule(NamedModuleSignature.NamedModuleSignature(elements = elems)) -> add elems
        | SignatureFile.Namespaces groups ->
            for g in groups do
                match g with
                | NamespaceDeclGroupSignature.Named(elements = elems)
                | NamespaceDeclGroupSignature.Global(elements = elems) -> add elems

        b.ToImmutable()
