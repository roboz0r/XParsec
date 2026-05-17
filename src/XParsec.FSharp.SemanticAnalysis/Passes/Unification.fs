namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// Pre:  ctx.Desugared and ctx.Binding populated.
// Post: ctx.TypeVar populated; every TypeVar's Link reaches its solved type
//       via UnionFind.find.
//
// Algorithm J: fresh TypeVar per AST node, constraints generated and
// unified on the fly. No deferred constraint set.
//
// Tiny-subset omissions (TODO):
//   - Generalisation. `let id = fun x -> x` types as `'a -> 'a` where 'a
//     stays unsolved. With multiple uses at different types we'd hit
//     monomorphism errors — add scheme + instantiate when needed.
//   - Occurs check. Only matters once recursive bindings (`let rec`) exist.
//   - SRTP / IWSAM bound resolution. The on-unified callbacks per
//     docs/typevar.md aren't wired yet.

module Unification =

    /// Walk TyVar links to the equivalence-class representative; if the rep
    /// has a Link, return its target (one level deep — call recursively for
    /// full resolution).
    let private resolveStep (t: SemType) : SemType =
        match t with
        | TyVar tv ->
            let root = UnionFind.find tv

            match root.Link with
            | ValueSome t' -> t'
            | ValueNone -> TyVar root
        | _ -> t

    /// Fully resolve a SemType: walk all TyVar chains AND recurse into TyFun
    /// arms. Used by Freeze (and tests) to materialise the final inferred
    /// type for a node.
    let rec zonk (t: SemType) : SemType =
        match t with
        | TyVar tv ->
            let root = UnionFind.find tv

            match root.Link with
            | ValueSome t' -> zonk t'
            | ValueNone -> TyVar root
        | TyConst _ -> t
        | TyFun(a, r) -> TyFun(zonk a, zonk r)

    let rec private unify (ctx: PassContext) (key: NodeKey) (a: SemType) (b: SemType) =
        let a = resolveStep a
        let b = resolveStep b

        match a, b with
        | TyConst n1, TyConst n2 when n1 = n2 -> ()
        | TyFun(a1, r1), TyFun(a2, r2) ->
            unify ctx key a1 a2
            unify ctx key r1 r2
        | TyVar tv1, TyVar tv2 when System.Object.ReferenceEquals(tv1, tv2) -> ()
        | TyVar tv1, TyVar tv2 -> UnionFind.union tv1 tv2
        | TyVar tv, other
        | other, TyVar tv ->
            let root = UnionFind.find tv
            // TODO: occurs check (matters once `let rec` lands).
            root.Link <- ValueSome other
        | _ ->
            ctx.Diagnostics.Add
                {
                    Key = key
                    Message = sprintf "Type mismatch: %A vs %A" (zonk a) (zonk b)
                    Severity = Error
                }

    /// Allocate a fresh TypeVar for `key` and store it in ctx.TypeVar.
    let private freshTv (ctx: PassContext) (key: NodeKey) : TypeVar =
        let tv = TypeVar()
        ctx.TypeVar.Set(key, tv)
        tv

    /// Look up the TypeVar previously stored for a node. Fresh-allocates if
    /// missing — happens for binding-site patterns that haven't been visited
    /// yet by inferPat.
    let private tvOf (ctx: PassContext) (key: NodeKey) : TypeVar =
        match ctx.TypeVar.TryGetValue key with
        | ValueSome tv -> tv
        | ValueNone -> freshTv ctx key

    let private inferConst (c: Constant<SyntaxToken>) : SemType =
        // TODO: dispatch on every numeric Token kind (NumFloat, NumInt64, …).
        // Tiny subset: KWTrue/KWFalse -> bool, everything else -> int.
        match c with
        | Constant.Literal t ->
            match t.Token with
            | Token.KWTrue
            | Token.KWFalse -> MockBuiltins.tyBool
            | _ -> MockBuiltins.tyInt
        | Constant.MeasuredLiteral _ -> MockBuiltins.tyInt

    let private inferPat (ctx: PassContext) (p: Pat<SyntaxToken>) : SemType =
        // Tiny subset: every pattern that introduces a name gets a fresh
        // TypeVar. Wildcard does the same (no name to bind, but the value
        // still has a type).
        let key = CstKeys.ofPat p
        TyVar(freshTv ctx key)

    let rec private infer (ctx: PassContext) (e: Expr<SyntaxToken>) : SemType =
        let key = CstKeys.ofExpr e
        let nodeTv = freshTv ctx key

        let inferredTy =
            match e with
            | Expr.Const c -> inferConst c
            | Expr.Ident _ -> inferIdent ctx e key
            | Expr.LongIdentOrOp _ -> inferIdent ctx e key
            | Expr.App(fn, args) -> inferApp ctx key fn args
            | Expr.InfixApp(left, _, right) -> inferInfix ctx key left right
            | Expr.Fun(argumentPats = argPats; expr = body) -> inferFun ctx argPats body
            | Expr.LetOrUse(bindings = bindings; body = body) -> inferLet ctx key bindings body
            | Expr.EnclosedBlock(expr = inner) -> infer ctx inner
            | _ ->
                // TODO: other expression kinds.
                TyVar(TypeVar())

        nodeTv.Link <- ValueSome inferredTy
        inferredTy

    and private inferIdent (ctx: PassContext) (e: Expr<SyntaxToken>) (key: NodeKey) : SemType =
        match ctx.Binding.TryGetValue key with
        | ValueSome rb ->
            // Local binding — the BindingSite is the headPat / lambda-param
            // NodeKey. Its TypeVar was minted by inferPat.
            TyVar(tvOf ctx rb.BindingSite)
        | ValueNone ->
            // External symbol (or unresolved — NameRes will already have
            // emitted a diagnostic in that case). Re-query the provider.
            let name = ctx.NameOf(CstKeys.firstTokenOfExpr e)

            match ctx.Provider.TryLookup name with
            | ValueSome sym ->
                // TODO: instantiate polymorphic schemes here when external
                // symbols carry SRTPs / forall-bound type vars.
                sym.Type
            | ValueNone -> TyVar(TypeVar())

    and private inferApp
        (ctx: PassContext)
        (key: NodeKey)
        (fn: Expr<SyntaxToken>)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        : SemType =
        let mutable currTy = infer ctx fn

        for a in args do
            let argTy = infer ctx a
            let resultTy = TyVar(TypeVar())
            unify ctx key currTy (TyFun(argTy, resultTy))
            currTy <- resultTy

        currTy

    and private inferInfix
        (ctx: PassContext)
        (key: NodeKey)
        (left: Expr<SyntaxToken>)
        (right: Expr<SyntaxToken>)
        : SemType =
        let leftTy = infer ctx left
        let rightTy = infer ctx right

        match ctx.Desugared.TryGetValue key with
        | ValueSome(DesugaredForm.OpName name) ->
            match ctx.Provider.TryLookup name with
            | ValueSome sym ->
                let resultTy = TyVar(TypeVar())
                unify ctx key sym.Type (TyFun(leftTy, TyFun(rightTy, resultTy)))
                resultTy
            | ValueNone ->
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message = sprintf "Unknown operator symbol: %s" name
                        Severity = Error
                    }

                TyVar(TypeVar())
        | ValueNone ->
            // Desugar didn't recognise the operator token; leave the result
            // as a free TypeVar (the unknown operator is a deficiency in
            // Desugar's lookup table, not a user error here).
            TyVar(TypeVar())

    and private inferFun
        (ctx: PassContext)
        (argPats: ImmutableArray<Pat<SyntaxToken>>)
        (body: Expr<SyntaxToken>)
        : SemType =
        let argTypes = [ for p in argPats -> inferPat ctx p ]
        let bodyTy = infer ctx body
        List.foldBack (fun a r -> TyFun(a, r)) argTypes bodyTy

    and private inferLet
        (ctx: PassContext)
        (key: NodeKey)
        (bindings: ImmutableArray<Binding<SyntaxToken>>)
        (body: Expr<SyntaxToken> voption)
        : SemType =
        for b in bindings do
            inferBinding ctx b

        match body with
        | ValueSome bodyExpr -> infer ctx bodyExpr
        | ValueNone ->
            // Module-level let has no `in` body; the let "expression"
            // doesn't have a meaningful type.
            TyConst "unit"

    and private inferBinding (ctx: PassContext) (b: Binding<SyntaxToken>) : unit =
        let patTy = inferPat ctx b.headPat

        let rhsTy =
            if b.argumentPats.IsEmpty then
                infer ctx b.expr
            else
                // `let f x y = body` is `let f = fun x y -> body`.
                let argTypes = [ for p in b.argumentPats -> inferPat ctx p ]
                let bodyTy = infer ctx b.expr
                List.foldBack (fun a r -> TyFun(a, r)) argTypes bodyTy

        unify ctx (CstKeys.ofBinding b) patTy rhsTy

    let private walkModuleElem (ctx: PassContext) (m: ModuleElem<SyntaxToken>) =
        match m with
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bindings)) ->
            for b in bindings do
                inferBinding ctx b
        | ModuleElem.Expression e -> infer ctx e |> ignore
        | _ -> ()

    let private walkElems (ctx: PassContext) (elems: ModuleElems<SyntaxToken>) =
        for m in elems do
            walkModuleElem ctx m

    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
        match file with
        | ImplementationFile.AnonymousModule elems -> walkElems ctx elems
        | ImplementationFile.NamedModule(NamedModule.NamedModule(elements = elems)) -> walkElems ctx elems
        | ImplementationFile.Namespaces _ -> ()
