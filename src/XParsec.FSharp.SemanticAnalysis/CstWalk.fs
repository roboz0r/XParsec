namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Immutable
open XParsec.FSharp.Parser

/// The active namespace prefixes in a lexical scope, most-recent-first (so a
/// later `open` shadows an earlier one on a name collision — F# semantics).
/// Drives short-name resolution: a bare `EqualityComparer` (under
/// `open System.Collections.Generic`) becomes the qualified
/// `System.Collections.Generic.EqualityComparer` before a provider probe.
///
/// The prefix list is FLAT and kind-blind: it cannot distinguish a type from a
/// module of the same name, so F#'s type-vs-module shadowing rules are not modelled.
/// Deliberate — a kind tag (or a real namespace tree) is a large change and nothing
/// in the corpus forces it yet. TRIGGER: surface the ambiguity as a diagnostic first;
/// generalise only against a case that actually needs it, not speculatively.
type OpenScope =
    {
        /// Each entry is a dotted namespace/module prefix (`"System.Collections.Generic"`),
        /// in shadowing order — head wins. Empty prefixes are never stored.
        Prefixes: string list
        /// Module-abbrev aliases (`module R = A.B.C` ⇒ `"R" → "A.B.C"`), expanded
        /// on the head segment of a dotted name before probing.
        Abbrevs: Map<string, string>
    }

module OpenScope =

    /// No opens, no abbrevs — the seed for a file with an empty ambient prelude.
    let empty: OpenScope = { Prefixes = []; Abbrevs = Map.empty }

    /// Candidate fully-qualified names for `name`, in priority order: the
    /// abbrev-expanded name as written (covers already-qualified and root-scope
    /// names), then each active prefix applied. The head segment of a dotted name
    /// is abbrev-expanded first (`R.X` ⇒ `A.B.C.X` under `module R = A.B.C`).
    let private candidates (scope: OpenScope) (name: string) : string list =
        let expanded =
            let dot = name.IndexOf '.'
            let head = if dot < 0 then name else name.Substring(0, dot)

            match Map.tryFind head scope.Abbrevs with
            | Some target -> target + (if dot < 0 then "" else name.Substring dot)
            | None -> name

        expanded
        :: [
            for p in scope.Prefixes do
                if p.Length > 0 then
                    yield p + "." + expanded
        ]

    /// Resolve `name` to a value via `lookup`, trying each candidate (see `candidates`)
    /// in priority order; first hit wins. The value-returning sibling of
    /// `tryQualify`, for the typing sites that need the resolved descriptor, not
    /// just its name.
    ///
    /// UNCACHED, on purpose: a per-`PassContext` memo keyed by
    /// `(OpenScope identity, name)` — file-lifetime — would cover the ENTIRE
    /// spelling-lookup seam now that the resolver face is the only string surface.
    /// That is precisely why it should wait: land it against a MEASURED hot path, not
    /// on principle. Resolution already happens once per written name (NameResolution
    /// stamps; consumers read keys), so the memo's remaining win is repeated spellings
    /// within a file, which may not be worth the invalidation surface.
    let tryResolve (scope: OpenScope) (lookup: string -> 'a voption) (name: string) : 'a voption =
        let rec go cs =
            match cs with
            | [] -> ValueNone
            | c :: rest ->
                match lookup c with
                | ValueSome _ as r -> r
                | ValueNone -> go rest

        go (candidates scope name)

    /// The single qualification primitive: returns the fully-qualified name
    /// `name` resolves under (for interning / diagnostic suppression), trying the
    /// bare/abbrev-expanded name then each active prefix; first `probe` hit wins.
    let tryQualify (scope: OpenScope) (probe: string -> bool) (name: string) : string voption =
        let rec go cs =
            match cs with
            | [] -> ValueNone
            | c :: rest -> if probe c then ValueSome c else go rest

        go (candidates scope name)

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

/// Active patterns and helpers for projecting `TypeDefn` shapes. The parser
/// emits `TypeDefn.Anon` for the bare `type C(...) = member ...` form without
/// an explicit `class`/`end`; semantically it is identical to `TypeDefn.Class`
/// throughout the front-end. Sites that need to treat both shapes uniformly
/// route through these helpers so a typo on one arm can't silently drop the
/// other.
module TypeDefnPatterns =

    /// Common fields of `TypeDefn.Class` and `TypeDefn.Anon`. Both shapes
    /// carry the same `(typeName, primaryConstr, asDefn, body)` quartet —
    /// only the lexical keyword token differs.
    [<NoEquality; NoComparison>]
    type ClassLikeDecl<'T> =
        {
            TypeName: TypeName<'T>
            PrimaryConstr: PrimaryConstrArgs<'T> voption
            AsDefn: AsDefn<'T> voption
            Body: ObjectModelBody<'T>
        }

    /// Project a `TypeDefn.Class`, `TypeDefn.Anon`, or `TypeDefn.Struct` into its
    /// primary fields. The `struct … end` shape carries the same
    /// `(typeName, primaryConstr, asDefn, body)` quartet and is handled
    /// identically through the front-end (its value-type-ness is recorded
    /// separately on `ClassTypeInfo.IsValueType`, see `isStructShape`). All other
    /// `TypeDefn` shapes yield `ValueNone`.
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

    /// `true` for the explicit `type X = struct … end` shape — a value type even
    /// without a `[<Struct>]` attribute. The attribute form lands as
    /// `Class`/`Anon`, so `registerClassTypeDefn` ORs this with the decoded
    /// attribute verdict.
    let isStructShape (td: TypeDefn<'T>) : bool =
        match td with
        | TypeDefn.Struct _ -> true
        | _ -> false

    /// `true` when the type is an interface: either the explicit `interface … end`
    /// shape, or the idiomatic all-abstract object-model form
    /// (`type IFoo = abstract member …` — every element an abstract signature, no
    /// primary ctor / `inherit` / `let`-preamble). The latter parses as
    /// `Class`/`Anon`, so `registerClassTypeDefn` consults this to stamp
    /// `ClassTypeInfo.IsInterface`, mirroring `isStructShape`. Same all-abstract
    /// predicate `Elaborate.tryInterfaceMethods` uses to project `TTypeKind.Interface`.
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

    /// Project a `TypeDefn.Union` or `TypeDefn.Record` with a single-ident name into
    /// that name `LongIdent` and the `with`-block extension elements when present (the
    /// `TypeExtensionElements` channel an `interface … with` / augmentation member rides;
    /// `ValueNone` when the type carries no `with` block). These are the two nominal kinds
    /// that — alongside a class, handled through the richer `tryClassLikeDecl` path — host
    /// `interface … with` impls; three passes (`Unification.fillNominalMembers` /
    /// `resolveInterfaceImplsForElem`, `NameResolution.walkNominalBodies`) matched this
    /// exact shape independently, each pairing it with a registry lookup
    /// (`TypeRegistry.tryNonClassMemberHost`). `ValueNone` for any other `TypeDefn` shape or
    /// a multi-ident name.
    let tryNonClassMemberHostDecl (td: TypeDefn<'T>) : struct (LongIdent<'T> * TypeDefnElements<'T> voption) voption =
        let extElems (ext: TypeExtensionElements<'T> voption) =
            match ext with
            | ValueSome(TypeExtensionElements(elements = elems)) -> ValueSome elems
            | ValueNone -> ValueNone

        match td with
        | TypeDefn.Union(typeName = TypeName(ident = nameLi); extensions = ext)
        | TypeDefn.Record(typeName = TypeName(ident = nameLi); extensions = ext) when nameLi.Idents.Length = 1 ->
            ValueSome(struct (nameLi, extElems ext))
        // An inline intrinsic-abbrev augmented with `with member …`
        // (`type X = (# … #) with member …`) hosts its members on the same path.
        // Registration files the host in `IntrinsicAbbrevHost` ONLY for an ILIntrinsic
        // RHS carrying extensions (a transparent-alias abbrev with members is rejected
        // there), so the `tryNonClassMemberHost` lookup naturally skips a rejected one.
        | TypeDefn.Abbrev(typeName = TypeName(ident = nameLi); extensions = ext & ValueSome _) when
            nameLi.Idents.Length = 1
            ->
            ValueSome(struct (nameLi, extElems ext))
        | _ -> ValueNone

    /// Project the `body` field from any object-model `TypeDefn` shape:
    /// `Class | Anon | Struct | Interface`. The four shapes share the same
    /// body type. Returns `ValueNone` for `Record | Union | Abbrev | Enum |
    /// Delegate | TypeExtension`.
    let tryObjectModelBody (td: TypeDefn<'T>) : ObjectModelBody<'T> voption =
        match td with
        | TypeDefn.Class(body = b)
        | TypeDefn.Anon(body = b)
        | TypeDefn.Struct(body = b)
        | TypeDefn.Interface(body = b) -> ValueSome b
        | _ -> ValueNone

/// The declaring containment of an element: the `namespace` group it sits in (dotted;
/// `""` for an anonymous / global / named-module file) and the `module` declarations it
/// is nested in, OUTERMOST FIRST. The two are named SEPARATELY and never flattened into
/// one dotted string — a module is a HOLDER, not a namespace segment, and folding it into
/// the namespace path is exactly the lie the segmented `NamespaceKey` exists to retire.
///
/// SYNTAX, not identity. This file compiles before `SemanticInfo`, so it cannot name
/// `ModuleHolder` — and it could not fill one anyway: a holder chain needs a module's
/// COMPILED holder name at each link, which is not a syntactic fact.
/// `NameResolutionTypeRegistration.localTypeHolder` is the one place that turns this into
/// a `TypeHolder`.
type DeclContainment<'T> =
    {
        Namespace: string
        /// Outermost first. The whole `ModuleDefn` rides along (not just its name)
        /// because the compiled holder name is a function of its ATTRIBUTES too
        /// (`[<CompilationRepresentation(ModuleSuffix)>]`).
        Modules: ModuleDefn<'T> list
    }

module DeclContainment =

    /// The containment at the top of a `namespace` group / file module — no enclosing
    /// modules yet.
    let ofNamespace (ns: string) : DeclContainment<'T> = { Namespace = ns; Modules = [] }

    /// Descend into a `module Foo = …`: the module is APPENDED to the holder chain (it is
    /// not a namespace segment, so `Namespace` is untouched).
    let enter (md: ModuleDefn<'T>) (c: DeclContainment<'T>) : DeclContainment<'T> =
        { c with Modules = c.Modules @ [ md ] }

    /// The innermost enclosing module, `ValueNone` at the namespace/file top level — the
    /// module a `let` compiles onto.
    let innermost (c: DeclContainment<'T>) : ModuleDefn<'T> voption =
        match List.tryLast c.Modules with
        | Some md -> ValueSome md
        | None -> ValueNone

    /// The declaring namespace in the `TTypeDecl.Namespace` shape: `None` for the global
    /// namespace / file module.
    let namespaceOpt (c: DeclContainment<'T>) : string option =
        if c.Namespace = "" then None else Some c.Namespace

/// One flattened leaf element of a module tree, paired with the ambient facts a pass needs
/// at that position: the `open` scope active there, the declaring containment a local
/// `SymbolKey` is minted from, and the enclosing `rec` scope that widens what a declaration
/// written here is visible from.
type WalkedElem<'T> =
    {
        Elem: ModuleElem<'T>
        Scope: OpenScope
        Containment: DeclContainment<'T>
        /// Source offset of the `module` / `namespace` keyword of the INNERMOST enclosing
        /// `rec` scope, `ValueNone` outside one. `rec` is exactly the statement that a
        /// declaration here is visible from the TOP of that scope rather than from where
        /// it is written, so this is the offset a claim minted here records as its
        /// `VisibleFrom` — the one place the rec-ness of a module enters the visibility
        /// rule, which therefore needs no branch of its own.
        RecScopeOffset: int voption
    }

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

    /// Identity walker: visits every node, changes no environment. Compose with
    /// `with` to override just the hook(s) a consumer needs — the
    /// `identityTypeIter` precedent for the expression walk.
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

    /// `Expr.LetOrUse(body = ValueNone)` is `use fixed` — pinning a managed
    /// value to a pointer. Not supported in the current subset; this helper is
    /// the single rejection point both Unification's `inferLet` and Elaborate's
    /// `translateLet` call, so the failwith demotion once `use fixed` lands
    /// lifts in one place. (Regions handles `ValueNone` distinctly — `use
    /// fixed`'s region story is independent of typing — so it doesn't route
    /// through here.)
    let requireLetBody (body: Expr<SyntaxToken> voption) : Expr<SyntaxToken> =
        match body with
        | ValueSome b -> b
        | ValueNone -> failwith "Expr.LetOrUse with no body (UseFixed) not supported"

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

        | Expr.Object(baseCall = baseCall; members = members; interfaceImpls = interfaceImpls) ->
            iterObjectExpr walker env baseCall members interfaceImpls

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

    // Walk the expression-bearing children of an object expression's member
    // list. Member shapes that carry no expression today (`Value`,
    // `AbstractSignature`) are leaves; shapes we haven't designed object-expr
    // semantics for yet (`AdditionalConstructor`) fail loud so an in-progress
    // file surfaces the gap instead of silently no-oping.
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
                // Secondary ctors in an object expression are not a legal F#
                // shape; surface loudly if the parser ever surfaces one here.
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
        // `interface Foo with …` head — no constructor argument expression.
        | ObjectConstruction.InterfaceConstruction _ -> ()

        let (ObjectMembers(memberDefns = memberDefns)) = members
        iterObjectMembers walker env memberDefns

        for InterfaceImpl.InterfaceImpl(objectMembers = objMembers) in interfaceImpls do
            match objMembers with
            | ValueSome(ObjectMembers(memberDefns = intfDefns)) -> iterObjectMembers walker env intfDefns
            | ValueNone -> ()

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

    /// Every `Type` (and member-signature) node syntactically embedded in ONE
    /// expression node. Fires `onType` / `onMemberSig` for the node's OWN embedded
    /// types only: recursion into child *expressions* is `iterExpr`'s job, so
    /// calling this once per visited node reaches every expression-embedded type
    /// exactly once. Pattern annotations (`fun` / `match` / `for` binders and a
    /// binding's `headPat` / argument pats) are a pattern-walk concern and are NOT
    /// visited here; a binding contributes only its return-type annotation.
    ///
    /// Exhaustive over `Expr` with no catch-all — a new parser case fails the
    /// incomplete-match check HERE, beside `iterExpr`'s, instead of silently going
    /// unstamped in a consumer whose read side deliberately has no by-name
    /// fallback (`Translate.tryResolveExternalTypeStamped`).
    let iterExprEmbeddedTypes
        (onType: Type<SyntaxToken> -> unit)
        (onMemberSig: MemberSig<SyntaxToken> -> unit)
        (e: Expr<SyntaxToken>)
        : unit =
        let bindingSig (b: Binding<SyntaxToken>) : unit =
            match b.returnType with
            | ValueSome(ReturnType(typ = t)) -> onType t
            | ValueNone -> ()

        // An object-expression member's *signature* types: a method/property
        // binding's return annotation, an auto-property's type, an abstract
        // signature's member sig. Bodies are child expressions (the walker's job);
        // argument patterns are the pattern walk's.
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
        // No directly-embedded `Type`: leaves, and shapes whose children are
        // expressions/patterns only.
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
        // slot (`type('T)`) carries raw tokens, not a `Type` node — nothing to
        // visit there.
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
        | Expr.LibraryOnlyStaticOptimization(constraints = cs) ->
            for c in cs do
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

    /// The CST-`Type` analogue of `iterExpr` — the single point where a written
    /// `Type` node's recursion shape is enumerated. `VisitType` fires on every
    /// `Type` node before its children; returning `false` skips the default child
    /// recursion (the visitor descended, or wants to skip, them itself), `true`
    /// continues. Mirrors `TastWalk.Iter`'s visit-only shape (the API precedent).
    ///
    /// Case coverage is the exhaustive `AstTraversal.walkType` template (no
    /// `| _ -> ()` catch-all), so a new `Type` case fails the incomplete-match
    /// check here rather than silently no-oping in a consumer. The recursion
    /// descends *every* nested `Type`, including those inside `when`-constraints
    /// (`WhenConstrainedType`) and member-trait signatures (`MemberTrait`), so a
    /// consumer's hook reaches every written type head reachable from `ty`.
    ///
    /// Types introduce no lexical binders, so — unlike `iterExpr` — this needs no
    /// environment threading; a consumer keeps its own state in the `VisitType`
    /// closure.
    [<NoEquality; NoComparison>]
    type TypeIter =
        {
            VisitType: TypeIter -> Type<SyntaxToken> -> bool
        }

    /// Identity iter: visits every node and recurses with no extra work. Compose
    /// with `with` to override the one hook.
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
                    // A measure arg carries no `Type` node; a consumer that needs
                    // the measure descends it itself (measures are opaque here).
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
            // Leaves: no nested `Type`. `VarType`/`NamedType` heads and the
            // measure/intrinsic/null forms bottom out here.
            | Type.VarType _
            | Type.NamedType _
            | Type.Null _
            | Type.MeasureType _
            | Type.ILIntrinsic _
            | Type.Missing
            | Type.SkipsTokens _ -> ()

    and iterTypeConstraints (it: TypeIter) (cs: TyparConstraints<SyntaxToken>) : unit =
        let (TyparConstraints(constraints = constraints)) = cs

        for c in constraints do
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

    /// An uncurried signature — a `DelegateSig`, or a GADT-syntax union case's
    /// `Name : arg * arg -> ret`: every argument type, then the return type.
    and iterTypeUncurriedSig (it: TypeIter) (sign: UncurriedSig<SyntaxToken>) : unit =
        let (UncurriedSig(args = ArgsSpec.ArgsSpec(args = args); returnType = ret)) = sign

        for (ArgSpec(typ = t)) in args do
            iterType it t

        iterType it ret

    /// THE enumeration of the type positions a `type` definition's DECLARED STRUCTURE
    /// writes: record/union field types, member value/signature types, interface
    /// specs/impls, an abbreviation's RHS, a delegate signature, and the header's `when`
    /// constraints. Every consumer of that surface — external-head stamping, the
    /// file-order scope check — walks it through here, so no consumer can miss a position
    /// another covers. Member *bodies* are not structure and are not reached.
    ///
    /// Two positions are handed OUT rather than iterated, because each has a consumer
    /// that treats it specially: constructor-parameter annotations are PATTERN-embedded
    /// (`onPat` owns the pattern walk, which carries more than type heads), and the
    /// `inherit` clause is resolved by its own registrar (`onInherit`).
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

        // A `[static] let` in a class preamble is a BODY, not declared structure — only its
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

        // A type header's typar-definition `when` clause (`type M<'F when 'F :> …>`)
        // lives on its `TypeName` — either the `TyparDefns`' trailing constraint list or
        // the separate `postfixConstraints`. Neither is reachable from any field / member
        // / param position, so both are enumerated here.
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

    /// The CST-`Pat` analogue of `iterType` — the single point where a pattern's
    /// recursion shape is enumerated. `VisitPat` fires on every `Pat` node before
    /// its children; returning `false` skips the default child recursion.
    ///
    /// Case coverage is exhaustive (no `| _ -> ()` catch-all), so a new `Pat` case
    /// fails the incomplete-match check here rather than silently no-oping in a
    /// consumer — the same discipline as `iterExpr` / `iterType`. Patterns
    /// introduce binders but no scopes, so no environment threading is needed.
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
            // Leaves: no sub-pattern. `TypeTest` carries a written type but no
            // inner pattern (a type-reading consumer takes its `typ` in the
            // visitor); `Pat.Expr` embeds an *expression*, not walked here
            // (mirroring `iterExpr`'s `Expr.Pat` leaf).
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
    /// arm. (Proper module nesting / qualification is a later rung.)
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

    /// Scope-preserving sibling of `implFileElems`: yields the same flattened leaf
    /// elements, but pairs each with the `OpenScope` active at its position. Where
    /// `implFileElems` erases the module boundaries open-scoping needs, this
    /// recomputes the running open-accumulator per element so a consumer can resolve
    /// a short name against the `open`s actually in scope there.
    ///
    /// `nameOf` reads a token's source text (the pass's `ctx.NameOf`); `ambient` is
    /// the seed prefix set (empty today, the referenced-contract prelude later, §6).
    /// `onScope` fires once per module/namespace body entered, paired with the
    /// *propagated* rec flag (`true` if this scope or any enclosing scope is
    /// `module rec` / `namespace rec`). FS3200's "opens must come first" rule
    /// rides this propagated flag (§3.2/§9 — each module under a rec group is
    /// independently an opens-first scope), distinct from the per-scope rec
    /// flag that drives open-resolution's constant-prelude behavior below.
    ///
    /// Scope semantics (§3): a non-recursive module/namespace is a *running
    /// accumulator* — an `open` is visible only to elements after it; a
    /// `module rec` / `namespace rec` is a *constant prelude* — every `open` in the
    /// scope applies to the whole body. A nested module inherits its enclosing
    /// accumulator. Module abbrevs (`module R = A.B.C`) fold into `Abbrevs`.
    let walkModuleTreeWith
        (nameOf: SyntaxToken -> string)
        (ambient: OpenScope)
        (onScope: ModuleElems<SyntaxToken> -> bool -> unit)
        (file: ImplementationFile<SyntaxToken>)
        : WalkedElem<SyntaxToken> list =
        // Each surfaced element carries its `DeclContainment`: the enclosing `namespace`
        // group plus every `module` it is nested in. The walk FLATTENS the module tree
        // (the wrapper element is dropped), so this is the only record of where the
        // element was declared — and it is what a local `SymbolKey` is minted from
        // (`NameResolutionTypeRegistration.stampLocalTypeKey`).
        let out = ResizeArray<WalkedElem<SyntaxToken>>()

        let longIdentText (li: LongIdent<SyntaxToken>) : string =
            li.Idents |> Seq.map nameOf |> String.concat "."

        let addOpen (scope: OpenScope) (li: LongIdent<SyntaxToken>) : OpenScope =
            let prefix = longIdentText li

            if prefix.Length = 0 then
                scope
            else
                { scope with
                    Prefixes = prefix :: scope.Prefixes
                }

        let addAbbrev (scope: OpenScope) (alias: string) (target: string) : OpenScope =
            if alias.Length = 0 || target.Length = 0 then
                scope
            else
                { scope with
                    Abbrevs = Map.add alias target scope.Abbrevs
                }

        // Apply one element's own contribution (an `open` / module-abbrev) to the
        // running accumulator. `open type` is deferred (a member channel, not a
        // namespace prefix — §6), so only `ImportDecl.ImportDecl` contributes.
        let accumulate (scope: OpenScope) (e: ModuleElem<SyntaxToken>) : OpenScope =
            match e with
            | ModuleElem.Import(ImportDecl.ImportDecl(longIdent = li)) -> addOpen scope li
            | ModuleElem.ModuleAbbrev(ModuleAbbrev.ModuleAbbrev(ident = id; longIdent = li)) ->
                addAbbrev scope (nameOf id) (longIdentText li)
            | _ -> scope

        // The rec scope a `module`/`namespace` body sits in: its OWN keyword when it is
        // itself `rec` (the innermost rec scope wins), else whatever it inherited.
        let innerRecScope (keyword: SyntaxToken) (isRec: SyntaxToken voption) (inherited: int voption) : int voption =
            if isRec.IsSome then
                ValueSome keyword.StartIndex
            else
                inherited

        // `isRec` is the scope's own rec flag (drives open-resolution's
        // constant-prelude shape). `recScope` is the *propagated* one: the innermost
        // enclosing rec scope's keyword offset, overwritten only where a scope is itself
        // rec, so it names the innermost rec ancestor and is `ValueNone` outside one. Its
        // presence is the propagated rec BOOLEAN that FS3200 wants (it fires in a non-rec
        // submodule of a rec namespace too), so the flag and the offset cannot disagree.
        let rec processElems
            (elems: ModuleElems<SyntaxToken>)
            (start: OpenScope)
            (isRec: bool)
            (recScope: int voption)
            (containment: DeclContainment<SyntaxToken>)
            : unit =
            onScope elems recScope.IsSome

            if isRec then
                // Constant prelude: all opens/abbrevs in this scope apply to the
                // whole body, regardless of position (§3.2, FS3200).
                let constScope = (start, elems) ||> Seq.fold accumulate

                for e in elems do
                    emit e constScope recScope containment
            else
                let mutable s = start

                for e in elems do
                    emit e s recScope containment
                    s <- accumulate s e

        and emit
            (e: ModuleElem<SyntaxToken>)
            (scope: OpenScope)
            (recScope: int voption)
            (containment: DeclContainment<SyntaxToken>)
            : unit =
            match e with
            | ModuleElem.Module((ModuleDefn.ModuleDefn(
                moduleToken = kw; isRec = innerRec; body = ModuleDefnBody(elements = inner))) as md) ->
                // The wrapper is dropped (as in `implFileElems`); the body is walked with
                // the enclosing scope inherited as its seed. A module is a *holder*, not a
                // namespace segment, so it EXTENDS the containment's holder chain and
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
                    // The namespace's own name is an implicit prefix for its body, and the
                    // declaring namespace at the root of its elements' containment.
                    processElems
                        elems
                        (addOpen ambient nsLi)
                        isRec.IsSome
                        (innerRecScope kw isRec ValueNone)
                        (DeclContainment.ofNamespace (longIdentText nsLi))
                | NamespaceDeclGroup.Global(elements = elems) -> processElems elems ambient false ValueNone top

        List.ofSeq out

    /// Scope-preserving walk with no per-scope hook — see `walkModuleTreeWith`.
    /// Most callers want this form; only per-scope diagnostics
    /// (`Validation.checkRecOpenPlacement`) thread an `onScope` callback.
    let walkModuleTree
        (nameOf: SyntaxToken -> string)
        (ambient: OpenScope)
        (file: ImplementationFile<SyntaxToken>)
        : (ModuleElem<SyntaxToken> * OpenScope) list =
        // Drop the containment — consumers that don't mint local `SymbolKey`s
        // (Unification's walkElems, VesperLib) keep the pair shape.
        walkModuleTreeWith nameOf ambient (fun _ _ -> ()) file
        |> List.map (fun w -> w.Elem, w.Scope)

    /// The signature elements a pass walks for a signature (`.fsi`) file — the
    /// `.fsi` analogue of `implFileElems`. A `namespace`-headed file contributes
    /// every group's elements in source order, and a nested `module Foo = …` body
    /// is spliced into the enclosing list (recursing through arbitrary nesting),
    /// for the same reasons as `implFileElems` (v1 has no namespace- or
    /// module-scoped types). Used by the sig/impl `Conformance` check.
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
