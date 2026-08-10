namespace XParsec.FSharp.SemanticAnalysis.Passes

open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// Pre:  every prior side table populated.
// Post: ctx.Diagnostics has any semantic violations. Read-only.
// Anonymous-union match coverage is instead co-computed with per-arm bound variable narrowing.

module Validation =

    /// True if `t` has any reachable TyVar whose union-find root carries no `Link` and is
    /// not in `quantified`. `let f (state: 'State) = let mutable acc = state` is legal because
    /// `acc`'s root is unpinned but owned by `f`'s scheme; `let mutable r = []` is not.
    let rec private hasFreeTyVar
        (store: TypeStore)
        (quantified: System.Collections.Generic.HashSet<TyVarId>)
        (t: SemType)
        : bool =
        match t with
        | TyVar tv ->
            let root = UnionFind.find store tv

            match store.Link root with
            | ValueSome target -> hasFreeTyVar store quantified target
            | ValueNone -> not (quantified.Contains root.Id)
        // A compound holds a free var iff any child does; leaves hold none.
        | t -> SemType.existsChild (hasFreeTyVar store quantified) t

    /// `lhs <- rhs` with a single-name `lhs` whose `ResolvedBinding` says
    /// `IsMutable = false` is an error. An array-slot LHS is out of scope because it
    /// routes through different mutability rules.
    let private checkAssignment (ctx: PassContext) (l: Expr<SyntaxToken>) : unit =
        // Peel `(x)` and `(x : T)` wrappers because neither changes mutability.
        let rec unwrap e =
            match e with
            | Expr.EnclosedBlock(expr = inner)
            | Expr.TypeAnnotation(expr = inner) -> unwrap inner
            | _ -> e

        let core = unwrap l
        // The assignment TARGET as a whole, where the "not mutable" arms below point,
        // except the dotted arm, which blames the FIELD segment it is talking about.
        let coreTok = CstKeys.diagTokenOfExpr core

        let isMultiSegLocalChain =
            match core with
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
                li.Idents.Length > 1
                && ctx.Bindings.Binding.ContainsKey(NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent)
                ->
                true
            | _ -> false

        match core with
        | _ when isMultiSegLocalChain ->
            // `r.X <- v`, parsed as one multi-segment LongIdent: the penultimate
            // object argument's type drives the last segment's mutability. Only the 2-segment
            // form reports; `r.A.X <- v` would need the intermediate field types.
            let li =
                match core with
                | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) -> li
                | _ -> failwith "unreachable"

            if li.Idents.Length = 2 then
                let anchorKey = NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent

                match ctx.Bindings.Binding.TryGetValue anchorKey with
                | ValueSome rb ->
                    match ctx.Bindings.TypeVar.TryGetValue rb.BindingSite with
                    | ValueSome tv ->
                        match Unification.zonk ctx.Store (TyVar tv) with
                        | TyRecord(recKey, _) ->
                            let fieldName = ctx.NameOf li.Idents.[1]

                            match TypeRegistry.tryRecordByKey ctx.Types recKey with
                            | ValueSome info ->
                                match info.Fields |> Array.tryFind (fun f -> f.Name = fieldName) with
                                | Some field when not field.IsMutable ->
                                    ctx.Report(li.Idents.[1], Kind.ImmutableFieldAssignment fieldName)
                                | _ -> ()
                            | ValueNone -> ()
                        | _ -> ()
                    | ValueNone -> ()
                | ValueNone -> ()
        | Expr.Ident _
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent _) ->
            let lhsKey = CstKeys.ofExpr core

            match ctx.Bindings.Binding.TryGetValue lhsKey with
            | ValueSome rb when not rb.IsMutable -> ctx.Report(coreTok, Kind.Message "assignment to immutable binding")
            | _ -> ()
        | Expr.DotLookup(expr = r; longIdentOrOp = LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
            // A free TyVar object argument (unresolved record) skips silently; the
            // deferred-field-access check surfaces those.
            let rKey = CstKeys.ofExpr r

            match ctx.Bindings.TypeVar.TryGetValue rKey with
            | ValueSome tv ->
                match Unification.zonk ctx.Store (TyVar tv) with
                | TyRecord(recKey, _) ->
                    let fieldName = ctx.NameOf li.Idents.[0]

                    match TypeRegistry.tryRecordByKey ctx.Types recKey with
                    | ValueSome info ->
                        match info.Fields |> Array.tryFind (fun f -> f.Name = fieldName) with
                        | Some field when not field.IsMutable ->
                            ctx.Report(li.Idents.[0], Kind.ImmutableFieldAssignment fieldName)
                        | _ -> ()
                    | ValueNone -> ()
                | _ -> ()
            | ValueNone -> ()
        | _ -> ()

    let private checkUnresolvedDotAccesses (ctx: PassContext) : unit =
        let byVar = ctx.Bindings.TypeVar.AsDictionary()
        // Distinct roots are bounded by the entry count, so presize once rather than let an
        // un-presized set resize log2(N) times (each a fresh backing array + rehash).
        let seenRoots = System.Collections.Generic.HashSet<TyVarId>(byVar.Count)

        for kv in byVar do
            let root = UnionFind.find ctx.Store kv.Value

            let pending = ctx.Store.Pda.Live root

            if seenRoots.Add(root.Id) && not (List.isEmpty pending) then
                for d in pending do
                    ctx.Report(
                        d.Use.Tok,
                        Kind.Message(
                            sprintf
                                "Cannot resolve member '%s': object-argument type was never constrained to a record or class type"
                                d.MemberName
                        )
                    )

    let private checkValueRestriction (ctx: PassContext) : unit =
        // Collect every scheme-quantified root up front so `hasFreeTyVar` can exclude
        // them: only a free root no scheme owns is the value-restriction hole.
        let quantified = System.Collections.Generic.HashSet<TyVarId>()

        for kv in ctx.Bindings.Scheme.AsDictionary() do
            for q in kv.Value.Quantified do
                quantified.Add((UnionFind.find ctx.Store q).Id) |> ignore

        // The table holds one self-entry per binding AND one per use site, so the
        // `kv.Key = rb.BindingSite` filter is what stops this firing once per use.
        for kv in ctx.Bindings.Binding.AsDictionary() do
            let rb = kv.Value

            if rb.IsMutable && kv.Key = rb.BindingSite then
                match ctx.Bindings.TypeVar.TryGetValue rb.BindingSite with
                | ValueSome tv when hasFreeTyVar ctx.Store quantified (TyVar tv) ->
                    // This pass walks the binding TABLE, not the tree, and `BoundVarNames`, the only
                    // record of where a bound variable was written, is not filled for an ordinary
                    // `let mutable` until Elaborate, which runs after. So: nowhere.
                    ctx.Report(
                        Site.Nowhere,
                        Kind.Message
                            "value restriction: mutable binding has unresolved type variable(s); \
                             add a type annotation or constrain via a use site"
                    )
                | _ -> ()

    /// FS3200: inside a `module rec` / `namespace rec`, the first non-`open` element of a
    /// scope closes the opens-first region and any later `open` there is rejected. Without
    /// it an interspersed open would silently get whole-scope, position-insensitive opens.
    let private checkRecOpenPlacement (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
        let checkScope (elems: ModuleElems<SyntaxToken>) =
            let mutable seenNonImport = false

            for e in elems do
                match e with
                | ModuleElem.Import(ImportDecl.ImportDecl(openToken = openTok))
                | ModuleElem.Import(ImportDecl.ImportDeclType(openToken = openTok)) ->
                    if seenNonImport then
                        ctx.Report(
                            openTok,
                            Kind.Message
                                "In a recursive declaration group, 'open' declarations must come first in each module."
                        )
                | _ -> seenNonImport <- true

        let onScope (elems: ModuleElems<SyntaxToken>) (isRec: bool) : unit =
            if isRec then
                checkScope elems

        CstWalk.walkModuleTreeWith ctx.NameOf OpenScope.empty onScope file |> ignore

    /// A `use` binding requires a simple variable pattern: `use x = e`, `use x : T = e`,
    /// `use (x) = e`, `use _ = e`. A destructuring pattern is rejected because the bound
    /// value itself is what gets disposed, and a decomposition leaves no single such value.
    let rec private isSimpleUsePat (p: Pat<SyntaxToken>) : bool =
        match p with
        | Pat.NamedSimple _
        | Pat.Wildcard _ -> true
        | Pat.Typed(pat = inner)
        | Pat.EnclosedBlock(pat = inner) -> isSimpleUsePat inner
        | _ -> false

    let private checkUseBindings (ctx: PassContext) (bindings: XParsec.FSharp.ImArr<Binding<SyntaxToken>>) : unit =
        for b in bindings do
            if not (isSimpleUsePat b.pattern) then
                ctx.Report(
                    CstKeys.firstTokenOfPat b.pattern,
                    Kind.Message "Only simple variable patterns can be bound in 'use' expressions"
                )

    let private mkWalker (ctx: PassContext) : CstWalk.ExprWalker<unit> =
        {
            Visit =
                fun () e ->
                    match e with
                    | Expr.Assignment(leftExpr = l) -> checkAssignment ctx l
                    | Expr.LetOrUse(keyword = (LetOrUseKeyword.Use _ | LetOrUseKeyword.UseBang _); bindings = bindings) ->
                        checkUseBindings ctx bindings
                    | _ -> ()
            EnterFun = fun () _ -> ()
            EnterBindingRhs = fun () _ _ _ -> ()
            EnterLetBody = fun () _ -> ()
            EnterForTo = fun () _ -> ()
            EnterForIn = fun () _ -> ()
            EnterMatchArm = fun () _ -> ()
        }

    let private walkModuleElem
        (ctx: PassContext)
        (walker: CstWalk.ExprWalker<unit>)
        (m: ModuleElem<SyntaxToken>)
        : unit =

        match m with
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bindings)) ->
            for b in bindings do
                CstWalk.iterExpr walker () b.expr
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Do(expr = e)) -> CstWalk.iterExpr walker () e
        | ModuleElem.Expression e -> CstWalk.iterExpr walker () e
        | ModuleElem.Type defs ->
            // Records / DUs / abbreviations have no expression bodies that
            // affect Validation; only class-like member bodies are walked.
            for td in defs do
                match TypeDefnPatterns.tryObjectModelBody td with
                | ValueSome body ->
                    for el in body.elements do
                        match el with
                        | TypeDefnElement.Member(MemberDefn.Member(defn = d)) ->
                            match d with
                            | MethodOrPropDefn.AutoProperty(expr = e) -> CstWalk.iterExpr walker () e
                            | d ->
                                for b in CstWalk.memberBindings d do
                                    CstWalk.iterExpr walker () b.expr
                        | _ -> ()
                | ValueNone -> ()
        // Report rather than crash: one unhandled element shouldn't halt validation
        // of the rest of the file.
        | ModuleElem.Exception defn ->
            let tok =
                match defn with
                | ExceptionDefn.Full(exceptionToken = t)
                | ExceptionDefn.Abbreviation(exceptionToken = t) -> t

            ctx.Report(tok, Kind.NotYetSupported "validation of `exception` declarations")
        // A nested module's body is flattened into the element list before this walk,
        // so a surviving `ModuleElem.Module` means the flattening missed a construct:
        // a bug in THIS compiler, not in the source, and the diagnostic says so.
        | ModuleElem.Module(ModuleDefn.ModuleDefn(moduleToken = tok)) ->
            ctx.Report(tok, Kind.Internal(InternalBreak.UnflattenedModule "Validation"))
        // `open` / `module R = …` are consumed by open-resolution; no expression here.
        | ModuleElem.ModuleAbbrev _ -> ()
        | ModuleElem.Import _ -> ()
        | ModuleElem.CompilerDirective(CompilerDirectiveDecl(hash = tok)) ->
            ctx.Report(tok, Kind.NotYetSupported "validation of compiler directives")
        // Parse-recovery nodes. The parser already reported each (`MissingModuleElem` for
        // the hole, `UnexpectedTopLevel` for the skipped run) and those reach the consumer,
        // so a second verdict here would say one mistake twice.
        | ModuleElem.Missing
        | ModuleElem.SkipsTokens _ -> ()

    let private walkElems
        (ctx: PassContext)
        (walker: CstWalk.ExprWalker<unit>)
        (elems: ModuleElems<SyntaxToken>)
        : unit =
        for m in elems do
            walkModuleElem ctx walker m

    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
        let walker = mkWalker ctx
        walkElems ctx walker (CstWalk.implFileElems file)

        checkUnresolvedDotAccesses ctx
        checkValueRestriction ctx
        checkRecOpenPlacement ctx file
