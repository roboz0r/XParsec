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
        | TyConst _ -> false
        | TyFun(a, r) -> hasFreeTyVar a || hasFreeTyVar r
        | TyTuple items -> items |> List.exists hasFreeTyVar
        | TyRecord(_, args) -> args |> List.exists hasFreeTyVar
        | TyUnion(_, args) -> args |> List.exists hasFreeTyVar
        | TyClass(_, args) -> args |> List.exists hasFreeTyVar

    /// `lhs <- rhs` with a single-name `lhs` whose `ResolvedBinding` says
    /// `IsMutable = false` is an error. Non-Ident LHSes (record field,
    /// array slot, dotted access) are out of scope for v1 — they route
    /// through different mutability rules that land with records / arrays.
    let private checkAssignment (ctx: PassContext) (l: Expr<SyntaxToken>) : unit =
        // Peel `(x)` and `(x : T)` wrappers — they don't change the LHS's
        // mutability story.
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
                && ctx.Binding.ContainsKey(NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent)
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

                match ctx.Binding.TryGetValue headKey with
                | ValueSome rb ->
                    match ctx.TypeVar.TryGetValue rb.BindingSite with
                    | ValueSome tv ->
                        match Unification.zonk (TyVar tv) with
                        | TyRecord(recName, _) ->
                            let fieldName = ctx.NameOf li.Idents.[1]

                            match ctx.RecordTypes.TryGetValue recName with
                            | true, info ->
                                match info.Fields |> Array.tryFind (fun f -> f.Name = fieldName) with
                                | Some field when not field.IsMutable ->
                                    ctx.Diagnostics.Add
                                        {
                                            Key = CstKeys.ofExpr core
                                            Message = sprintf "Cannot assign to immutable field '%s'" fieldName
                                            Severity = Error
                                        }
                                | _ -> ()
                            | false, _ -> ()
                        | _ -> ()
                    | ValueNone -> ()
                | ValueNone -> ()
        | Expr.Ident _
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent _) ->
            let lhsKey = CstKeys.ofExpr core

            match ctx.Binding.TryGetValue lhsKey with
            | ValueSome rb when not rb.IsMutable ->
                ctx.Diagnostics.Add
                    {
                        Key = lhsKey
                        Message = "assignment to immutable binding"
                        Severity = Error
                    }
            | _ -> ()
        | Expr.DotLookup(expr = r; longIdentOrOp = LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
            // `r.X <- v` — diagnose if X is declared immutable on r's
            // resolved record type. Free TyVar receivers (unresolved
            // record) skip silently; the deferred-field-access check
            // surfaces those.
            let rKey = CstKeys.ofExpr r

            match ctx.TypeVar.TryGetValue rKey with
            | ValueSome tv ->
                match Unification.zonk (TyVar tv) with
                | TyRecord(recName, _) ->
                    let fieldName = ctx.NameOf li.Idents.[0]

                    match ctx.RecordTypes.TryGetValue recName with
                    | true, info ->
                        match info.Fields |> Array.tryFind (fun f -> f.Name = fieldName) with
                        | Some field when not field.IsMutable ->
                            ctx.Diagnostics.Add
                                {
                                    Key = CstKeys.ofExpr core
                                    Message = sprintf "Cannot assign to immutable field '%s'" fieldName
                                    Severity = Error
                                }
                        | _ -> ()
                    | false, _ -> ()
                | _ -> ()
            | ValueNone -> ()
        | _ -> ()

    let private checkUnresolvedDotAccesses (ctx: PassContext) : unit =
        let seenRoots = System.Collections.Generic.HashSet<TypeVar>(HashIdentity.Reference)

        for kv in ctx.TypeVar.AsDictionary() do
            let root = UnionFind.find kv.Value

            if seenRoots.Add(root) && not (List.isEmpty root.PendingDotAccess) then
                for (memberName, useKey, _) in root.PendingDotAccess do
                    ctx.Diagnostics.Add
                        {
                            Key = useKey
                            Message =
                                sprintf
                                    "Cannot resolve member '%s': receiver type was never constrained to a record or class type"
                                    memberName
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
        for kv in ctx.Binding.AsDictionary() do
            let rb = kv.Value

            if rb.IsMutable && kv.Key = rb.BindingSite then
                match ctx.TypeVar.TryGetValue rb.BindingSite with
                | ValueSome tv when hasFreeTyVar (TyVar tv) ->
                    ctx.Diagnostics.Add
                        {
                            Key = rb.BindingSite
                            Message =
                                "value restriction: mutable binding has unresolved type variable(s); \
                                 add a type annotation or constrain via a use site"
                            Severity = Error
                        }
                | _ -> ()

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

    let private walkModuleElem (walker: CstWalk.ExprWalker<unit>) (m: ModuleElem<SyntaxToken>) : unit =
        match m with
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bindings)) ->
            for b in bindings do
                CstWalk.iterExpr walker () b.expr
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Do(expr = e)) -> CstWalk.iterExpr walker () e
        | ModuleElem.Expression e -> CstWalk.iterExpr walker () e
        | ModuleElem.Type defs ->
            // Walk class / anon-class member bodies for assignment
            // checks etc. Records / DUs / abbreviations have no
            // expression bodies that affect Validation.
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
        // Surface unhandled module elements rather than silently skipping
        // them — Validation needs to grow new arms as the subset expands.
        | ModuleElem.Exception _ -> failwith "Validation: ModuleElem.Exception not implemented"
        // `CstWalk.implFileElems` flattens a nested module's body into the
        // element list before `walkElems` runs, so a `ModuleElem.Module` never
        // reaches here — its contents are walked as ordinary top-level elements.
        | ModuleElem.Module _ -> ()
        | ModuleElem.ModuleAbbrev _ -> failwith "Validation: ModuleElem.ModuleAbbrev not implemented"
        | ModuleElem.Import _ -> failwith "Validation: ModuleElem.Import not implemented"
        | ModuleElem.CompilerDirective _ -> failwith "Validation: ModuleElem.CompilerDirective not implemented"
        | ModuleElem.Missing -> failwith "Validation: ModuleElem.Missing not implemented"
        | ModuleElem.SkipsTokens _ -> failwith "Validation: ModuleElem.SkipsTokens not implemented"

    let private walkElems (walker: CstWalk.ExprWalker<unit>) (elems: ModuleElems<SyntaxToken>) : unit =
        for m in elems do
            walkModuleElem walker m

    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
        let walker = mkWalker ctx
        walkElems walker (CstWalk.implFileElems file)

        checkUnresolvedDotAccesses ctx
        checkValueRestriction ctx
