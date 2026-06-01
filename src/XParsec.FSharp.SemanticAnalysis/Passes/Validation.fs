namespace XParsec.FSharp.SemanticAnalysis.Passes

open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// Pre:  every prior side table populated.
// Post: ctx.Diagnostics has any semantic violations.
//
// Read-only. Checks: pattern-match exhaustiveness (TODO), value restriction
// on mutable bindings, immutability enforcement (per docs/passes.md §4.5
// and docs/mutable-plan.md).

module Validation =

    /// True if `t` has any reachable TyVar whose union-find root carries no
    /// `Link`. Mirrors the resolve semantics of `Unification.zonk`: follow a
    /// pinned root through its `Link`, return `true` at any unpinned root.
    /// Used by the mutable-binding value-restriction check, which fires at
    /// end of analysis — by then every use site has had a chance to pin
    /// free TyVars via the unification of LHS and RHS types.
    let rec private hasFreeTyVar (t: SemType) : bool =
        match t with
        | TyVar tv ->
            let root = UnionFind.find tv

            match root.Link with
            | ValueSome target -> hasFreeTyVar target
            | ValueNone -> true
        | TyConst(_, args) -> args |> EqArray.exists hasFreeTyVar
        | TyFun(a, r) -> hasFreeTyVar a || hasFreeTyVar r
        | TyTuple items -> items |> EqArray.exists hasFreeTyVar
        | TyRecord(_, args) -> args |> EqArray.exists hasFreeTyVar
        | TyUnion(_, args) -> args |> EqArray.exists hasFreeTyVar
        | TyClass(_, args) -> args |> EqArray.exists hasFreeTyVar
        | TyUnknown _ -> false

    /// `lhs <- rhs` with a single-name `lhs` whose `ResolvedBinding` says
    /// `IsMutable = false` is an error. Non-Ident LHSes (record field,
    /// array slot, dotted access) are out of scope for v1 — they route
    /// through different mutability rules that land with records / arrays.
    let private checkAssignment (ctx: PassContext) (l: Expr<SyntaxToken>) : unit =
        // Peel `(x)` and `(x : T)` wrappers — they don't change mutability.
        let rec unwrap e =
            match e with
            | Expr.EnclosedBlock(expr = inner)
            | Expr.TypeAnnotation(expr = inner) -> unwrap inner
            | _ -> e

        let core = unwrap l

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
            // `r.X <- v` (parsed as a single multi-segment LongIdent).
            // The penultimate-receiver's type drives mutability of the
            // last segment. v1: only 2-segment forms (`r.X <- v`) emit a
            // diagnostic; deeper chains (`r.A.X <- v`) need to walk the
            // intermediate field types — defer to a follow-up when typing
            // those chains lands.
            let li =
                match core with
                | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) -> li
                | _ -> failwith "unreachable"

            if li.Idents.Length = 2 then
                let headKey = NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent

                match ctx.Bindings.Binding.TryGetValue headKey with
                | ValueSome rb ->
                    match ctx.Bindings.TypeVar.TryGetValue rb.BindingSite with
                    | ValueSome tv ->
                        match Unification.zonk (TyVar tv) with
                        | TyRecord(recKey, _) ->
                            let fieldName = ctx.NameOf li.Idents.[1]

                            match TypeRegistry.tryRecordByKey ctx.Types recKey with
                            | ValueSome info ->
                                match info.Fields |> Array.tryFind (fun f -> f.Name = fieldName) with
                                | Some field when not field.IsMutable ->
                                    ctx.Diagnostics.Add
                                        {
                                            Key = CstKeys.ofExpr core
                                            Message = sprintf "Cannot assign to immutable field '%s'" fieldName
                                            Code = ""
                                            Severity = Error
                                        }
                                | _ -> ()
                            | ValueNone -> ()
                        | _ -> ()
                    | ValueNone -> ()
                | ValueNone -> ()
        | Expr.Ident _
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent _) ->
            let lhsKey = CstKeys.ofExpr core

            match ctx.Bindings.Binding.TryGetValue lhsKey with
            | ValueSome rb when not rb.IsMutable ->
                ctx.Diagnostics.Add
                    {
                        Key = lhsKey
                        Message = "assignment to immutable binding"
                        Code = ""
                        Severity = Error
                    }
            | _ -> ()
        | Expr.DotLookup(expr = r; longIdentOrOp = LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
            // Free TyVar receivers (unresolved record) skip silently; the
            // deferred-field-access check surfaces those.
            let rKey = CstKeys.ofExpr r

            match ctx.Bindings.TypeVar.TryGetValue rKey with
            | ValueSome tv ->
                match Unification.zonk (TyVar tv) with
                | TyRecord(recKey, _) ->
                    let fieldName = ctx.NameOf li.Idents.[0]

                    match TypeRegistry.tryRecordByKey ctx.Types recKey with
                    | ValueSome info ->
                        match info.Fields |> Array.tryFind (fun f -> f.Name = fieldName) with
                        | Some field when not field.IsMutable ->
                            ctx.Diagnostics.Add
                                {
                                    Key = CstKeys.ofExpr core
                                    Message = sprintf "Cannot assign to immutable field '%s'" fieldName
                                    Code = ""
                                    Severity = Error
                                }
                        | _ -> ()
                    | ValueNone -> ()
                | _ -> ()
            | ValueNone -> ()
        | _ -> ()

    let private checkUnresolvedDotAccesses (ctx: PassContext) : unit =
        let seenRoots = System.Collections.Generic.HashSet<TypeVar>(HashIdentity.Reference)

        for kv in ctx.Bindings.TypeVar.AsDictionary() do
            let root = UnionFind.find kv.Value

            if seenRoots.Add(root) && not (List.isEmpty root.PendingDotAccess) then
                for d in root.PendingDotAccess do
                    ctx.Diagnostics.Add
                        {
                            Key = d.UseKey
                            Message =
                                sprintf
                                    "Cannot resolve member '%s': receiver type was never constrained to a record or class type"
                                    d.MemberName
                            Code = ""
                            Severity = Error
                        }

    // A scheme-level "Constraint not resolved" tail check is reserved
    // for a future revision: with v1's drainConstraints firing at every
    // use site, every meaningful unresolved-constraint case already
    // surfaces a diagnostic there. A true tail check would require
    // tracking whether each scheme is ever instantiated and whether
    // every quantified TyVar's constraint was discharged at at least
    // one instantiation — the bookkeeping isn't worth it for v1.

    let private checkValueRestriction (ctx: PassContext) : unit =
        // Iterate every binding-site self-entry (kv.Key = rb.BindingSite)
        // whose binding is mutable. NameResolution writes one self-entry
        // per binding *and* one entry per use-site; filtering on
        // `kv.Key = rb.BindingSite` keeps us from firing once per use.
        for kv in ctx.Bindings.Binding.AsDictionary() do
            let rb = kv.Value

            if rb.IsMutable && kv.Key = rb.BindingSite then
                match ctx.Bindings.TypeVar.TryGetValue rb.BindingSite with
                | ValueSome tv when hasFreeTyVar (TyVar tv) ->
                    ctx.Diagnostics.Add
                        {
                            Key = rb.BindingSite
                            Message =
                                "value restriction: mutable binding has unresolved type variable(s); \
                                 add a type annotation or constrain via a use site"
                            Code = ""
                            Severity = Error
                        }
                | _ -> ()

    /// FS3200: in a recursive declaration group, `open` declarations must come
    /// first in each module / namespace scope. Once inside a `module rec` /
    /// `namespace rec`, the first non-`open` element in a scope closes the
    /// opens-first region; any later `open` there is rejected. This makes the
    /// constant-prelude shape the rec branch of `CstWalk.walkModuleTree` assumes
    /// actually hold — without it, an interspersed open in a rec group would
    /// silently get whole-scope (position-insensitive) semantics that fsc
    /// rejects (symbol-resolution-handoff.md, open-resolution).
    ///
    /// Rides on `CstWalk.walkModuleTreeWith`'s per-scope hook so the rec-flag
    /// propagation lives in one place. The hook receives the *propagated*
    /// `inRec` (true for the rec scope itself and every module nested under it
    /// — each is an independent opens-first scope per §3.2/§9), which is the
    /// flag FS3200 fires on. The separate per-scope rec flag (whether *this*
    /// scope was declared `rec`) drives open-resolution's constant-prelude
    /// shape and stays inside `walkModuleTree`'s `processElems`.
    let private checkRecOpenPlacement (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
        let checkScope (elems: ModuleElems<SyntaxToken>) =
            let mutable seenNonImport = false

            for e in elems do
                match e with
                | ModuleElem.Import(ImportDecl.ImportDecl(openToken = openTok))
                | ModuleElem.Import(ImportDecl.ImportDeclType(openToken = openTok)) ->
                    if seenNonImport then
                        ctx.Diagnostics.Add
                            {
                                Key = NodeKey.ofToken openTok NodeKind.DeclOpen
                                Message =
                                    "In a recursive declaration group, 'open' declarations must come first in each module."
                                Code = ""
                                Severity = Error
                            }
                | _ -> seenNonImport <- true

        let onScope (elems: ModuleElems<SyntaxToken>) (isRec: bool) : unit =
            if isRec then
                checkScope elems

        CstWalk.walkModuleTreeWith ctx.NameOf OpenScope.empty onScope file |> ignore

    let private mkWalker (ctx: PassContext) : CstWalk.ExprWalker<unit> =
        {
            Visit =
                fun () e ->
                    match e with
                    | Expr.Assignment(leftExpr = l) -> checkAssignment ctx l
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
        let notYetSupported (spawningOffset: int) (msg: string) =
            ctx.Diagnostics.Add
                {
                    Key = NodeKey.ofSynthetic spawningOffset NodeKind.SynthUnsupportedDecl
                    Message = msg
                    Code = ""
                    Severity = Error
                }

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
                            | MethodOrPropDefn.Method(defn = b)
                            | MethodOrPropDefn.Property(defn = b) -> CstWalk.iterExpr walker () b.expr
                            | MethodOrPropDefn.AutoProperty(expr = e) -> CstWalk.iterExpr walker () e
                            | _ -> ()
                        | _ -> ()
                | ValueNone -> ()
        // Emit a diagnostic rather than crashing — a single unhandled element
        // shouldn't halt validation of the rest of the file. Grow real arms
        // as features land. `Missing` and `SkipsTokens` in particular are
        // produced by parse-recovery and reachable in any in-progress file.
        | ModuleElem.Exception defn ->
            let tok =
                match defn with
                | ExceptionDefn.Full(exceptionToken = t)
                | ExceptionDefn.Abbreviation(exceptionToken = t) -> t

            notYetSupported tok.StartIndex "`exception` declarations are not yet validated"
        // `CstWalk.implFileElems` flattens a nested module's body into the
        // element list before `walkElems` runs, so a `ModuleElem.Module` should
        // never reach here. If one does, the flattening invariant has drifted
        // (e.g. a new module-level construct slipped past `implFileElems`); fire
        // a diagnostic so the regression surfaces instead of vanishing into a
        // silent skip.
        | ModuleElem.Module(ModuleDefn.ModuleDefn(moduleToken = tok)) ->
            notYetSupported
                tok.StartIndex
                "Nested `module` reached Validation; `implFileElems` flattening invariant drifted"
        // `open` / `module R = …` are declaration-level nodes consumed by
        // open-resolution (NameResolution/Unification build the `OpenScope` from
        // them, symbol-resolution-handoff.md, open-resolution); they carry no expression to validate.
        | ModuleElem.ModuleAbbrev _ -> ()
        | ModuleElem.Import _ -> ()
        | ModuleElem.CompilerDirective(CompilerDirectiveDecl(hash = tok)) ->
            notYetSupported tok.StartIndex "Compiler directives are not yet validated"
        | ModuleElem.Missing -> notYetSupported 0 "Missing module element (parse recovery)"
        | ModuleElem.SkipsTokens skipped ->
            let off = if skipped.Length > 0 then skipped.[0].StartIndex else 0

            notYetSupported off "Skipped tokens at module level (parse recovery)"

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
