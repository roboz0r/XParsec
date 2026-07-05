namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Immutable
open XParsec.FSharp.Parser

/// The active namespace prefixes in a lexical scope, most-recent-first (so a
/// later `open` shadows an earlier one on a name collision — F# semantics).
/// Drives short-name resolution: a bare `EqualityComparer` (under
/// `open System.Collections.Generic`) becomes the qualified
/// `System.Collections.Generic.EqualityComparer` before a provider probe.
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

    /// Resolve `name` to a value via `lookup`, trying each candidate (§ `candidates`)
    /// in priority order; first hit wins. The value-returning sibling of
    /// `tryQualify`, for the typing sites that need the resolved descriptor, not
    /// just its name.
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

    /// `Expr.LetOrUse(body = ValueNone)` is `use fixed` — pinning a managed
    /// value to a pointer. Not supported in the current subset; this helper is
    /// the single rejection point both Unification's `inferLet` and Freeze's
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
        : (ModuleElem<SyntaxToken> * OpenScope * string) list =
        // The third tuple slot is the *declaring namespace* of each surfaced element
        // — the enclosing `namespace` group's
        // longident, "" for an anonymous/global/named-module file. It mirrors
        // `Freeze.run`'s `ns` exactly (a nested `module` is a holder, not a namespace
        // segment, so it passes the enclosing ns through unchanged), so a local
        // `SymbolKey` minted from it equals the type's emitted `TDecl.Namespace`.
        let out = ResizeArray<ModuleElem<SyntaxToken> * OpenScope * string>()

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

        // `isRec` is the scope's own rec flag (drives open-resolution's
        // constant-prelude shape); `inRec` is the *propagated* flag (true if
        // this scope or any enclosing scope is rec) — drives FS3200, which
        // fires in non-rec submodules of a rec namespace too.
        let rec processElems
            (elems: ModuleElems<SyntaxToken>)
            (start: OpenScope)
            (isRec: bool)
            (inRec: bool)
            (declNs: string)
            : unit =
            onScope elems inRec

            if isRec then
                // Constant prelude: all opens/abbrevs in this scope apply to the
                // whole body, regardless of position (§3.2, FS3200).
                let constScope = (start, elems) ||> Seq.fold accumulate

                for e in elems do
                    emit e constScope inRec declNs
            else
                let mutable s = start

                for e in elems do
                    emit e s inRec declNs
                    s <- accumulate s e

        and emit (e: ModuleElem<SyntaxToken>) (scope: OpenScope) (inRec: bool) (declNs: string) : unit =
            match e with
            | ModuleElem.Module(ModuleDefn.ModuleDefn(isRec = innerRec; body = ModuleDefnBody(elements = inner))) ->
                // The wrapper is dropped (as in `implFileElems`); the body is walked
                // with the enclosing scope inherited as its seed. A module is a *holder*,
                // not a namespace segment, so `declNs` passes through unchanged — this is
                // the rule `Freeze.run` applies (the module name becomes the let-holder,
                // never part of a nested type's `Namespace`).
                match inner with
                | ValueSome innerElems ->
                    processElems innerElems scope innerRec.IsSome (inRec || innerRec.IsSome) declNs
                | ValueNone -> ()
            | _ -> out.Add(e, scope, declNs)

        match file with
        | ImplementationFile.AnonymousModule elems -> processElems elems ambient false false ""
        | ImplementationFile.NamedModule(NamedModule.NamedModule(isRec = isRec; elements = elems)) ->
            processElems elems ambient isRec.IsSome isRec.IsSome ""
        | ImplementationFile.Namespaces groups ->
            for g in groups do
                match g with
                | NamespaceDeclGroup.Named(isRec = isRec; longIdent = nsLi; elements = elems) ->
                    // The namespace's own name is an implicit prefix for its body, and the
                    // declaring namespace its types are emitted into (`Freeze.run`).
                    processElems elems (addOpen ambient nsLi) isRec.IsSome isRec.IsSome (longIdentText nsLi)
                | NamespaceDeclGroup.Global(elements = elems) -> processElems elems ambient false false ""

        List.ofSeq out

    /// Scope-preserving walk with no per-scope hook — see `walkModuleTreeWith`.
    /// Most callers want this form; only per-scope diagnostics
    /// (`Validation.checkRecOpenPlacement`) thread an `onScope` callback.
    let walkModuleTree
        (nameOf: SyntaxToken -> string)
        (ambient: OpenScope)
        (file: ImplementationFile<SyntaxToken>)
        : (ModuleElem<SyntaxToken> * OpenScope) list =
        // Drop the declaring-namespace slot — consumers that don't mint local
        // `SymbolKey`s (Unification's walkElems, VesperLib) keep the pair shape.
        walkModuleTreeWith nameOf ambient (fun _ _ -> ()) file
        |> List.map (fun (e, s, _) -> e, s)

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
