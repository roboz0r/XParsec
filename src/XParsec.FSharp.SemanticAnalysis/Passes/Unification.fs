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
//   - SRTP / IWSAM bound resolution. The on-unified callbacks per
//     docs/typevar.md aren't wired yet.
//   - Binding-level return-type annotations (`let f x : int = ...`). Only
//     Expr.TypeAnnotation (`(e : t)`) is handled today.

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
        | TyTuple items -> TyTuple(List.map zonk items)

    /// Move pending deferred-constraint state from `source` onto `target`.
    /// Called whenever a TyVar is no longer the equivalence-class
    /// representative (either after union-find collapse, or when its Link is
    /// set). Bounds attached to a non-representative would otherwise never
    /// fire their on-unified callbacks. Lists are empty in the current
    /// subset, so this is a no-op at runtime — but the contract has to be
    /// honoured before SRTPs / IWSAMs come online.
    let private migrateBounds (target: TypeVar) (source: TypeVar) : unit =
        if not (System.Object.ReferenceEquals(target, source)) then
            if not (List.isEmpty source.IfaceBounds) then
                target.IfaceBounds <- source.IfaceBounds @ target.IfaceBounds
                source.IfaceBounds <- []

            if not (List.isEmpty source.SrtpBounds) then
                target.SrtpBounds <- source.SrtpBounds @ target.SrtpBounds
                source.SrtpBounds <- []
    // TODO: fire on-unified callbacks for newly-stable bounds once
    // the SRTP / IWSAM resolution machinery exists. Until then,
    // appending is enough to preserve them through unification.

    /// Does `target` (already a union-find root) appear anywhere inside `t`?
    /// Stops the `let rec f x = f` / `let rec g = g g` family from cycling
    /// Link pointers and making zonk loop. Resolves through Links and
    /// recurses into compound shapes.
    let rec private occurs (target: TypeVar) (t: SemType) : bool =
        match resolveStep t with
        | TyVar tv -> System.Object.ReferenceEquals(UnionFind.find tv, target)
        | TyConst _ -> false
        | TyFun(a, r) -> occurs target a || occurs target r
        | TyTuple items -> List.exists (occurs target) items

    let rec private unify (ctx: PassContext) (key: NodeKey) (a: SemType) (b: SemType) =
        let a = resolveStep a
        let b = resolveStep b

        match a, b with
        | TyConst n1, TyConst n2 when n1 = n2 -> ()
        | TyFun(a1, r1), TyFun(a2, r2) ->
            unify ctx key a1 a2
            unify ctx key r1 r2
        | TyTuple xs, TyTuple ys when xs.Length = ys.Length -> List.iter2 (unify ctx key) xs ys
        | TyVar tv1, TyVar tv2 when System.Object.ReferenceEquals(tv1, tv2) -> ()
        | TyVar tv1, TyVar tv2 ->
            let r1 = UnionFind.find tv1
            let r2 = UnionFind.find tv2
            UnionFind.union r1 r2
            // After union, exactly one of r1/r2 still has Parent = ValueNone.
            let newRoot = UnionFind.find r1

            let merged =
                if System.Object.ReferenceEquals(newRoot, r1) then
                    r2
                else
                    r1

            migrateBounds newRoot merged
        | TyVar tv, other
        | other, TyVar tv ->
            let root = UnionFind.find tv

            if occurs root other then
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message =
                            sprintf
                                "Occurs check: cannot construct infinite type %A = %A"
                                (zonk (TyVar root))
                                (zonk other)
                        Severity = Error
                    }
            else
                root.Link <- ValueSome other
        // No bound migration needed here: `root` keeps its bounds, and
        // setting Link is the trigger for on-unified callbacks to fire
        // once they exist.
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
            | Expr.PrefixApp(_, operand) -> inferPrefix ctx key operand
            | Expr.Fun(argumentPats = argPats; expr = body) -> inferFun ctx argPats body
            | Expr.LetOrUse(bindings = bindings; body = body) -> inferLet ctx key bindings body
            | Expr.EnclosedBlock(expr = inner) -> infer ctx inner
            | Expr.IfThenElse(condition = cond; thenExpr = thenE; elifBranches = elifs; elseBranch = elseB) ->
                inferIfThenElse ctx key cond thenE elifs elseB
            | Expr.Tuple(exprs = items) -> inferTuple ctx items
            | Expr.Sequential(exprs = items) -> inferSequential ctx key items
            | Expr.TypeAnnotation(expr = inner; typ = t) -> inferTypeAnnotation ctx key inner t
            | Expr.EmptyBlock _ -> MockBuiltins.tyUnit
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

    and private inferPrefix (ctx: PassContext) (key: NodeKey) (operand: Expr<SyntaxToken>) : SemType =
        let operandTy = infer ctx operand

        match ctx.Desugared.TryGetValue key with
        | ValueSome(DesugaredForm.OpName name) ->
            match ctx.Provider.TryLookup name with
            | ValueSome sym ->
                let resultTy = TyVar(TypeVar())
                unify ctx key sym.Type (TyFun(operandTy, resultTy))
                resultTy
            | ValueNone ->
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message = sprintf "Unknown prefix operator: %s" name
                        Severity = Error
                    }

                TyVar(TypeVar())
        | ValueNone -> TyVar(TypeVar())

    and private inferIfThenElse
        (ctx: PassContext)
        (key: NodeKey)
        (cond: Expr<SyntaxToken>)
        (thenE: Expr<SyntaxToken>)
        (elifs: ImmutableArray<ElifBranch<SyntaxToken>>)
        (elseB: ElseBranch<SyntaxToken> voption)
        : SemType =
        let condTy = infer ctx cond
        unify ctx key condTy MockBuiltins.tyBool

        let thenTy = infer ctx thenE

        for elif_ in elifs do
            let elifCond, elifExpr =
                match elif_ with
                | ElifBranch.Elif(condition = c; expr = e)
                | ElifBranch.ElseIf(condition = c; expr = e) -> c, e

            let elifCondTy = infer ctx elifCond
            unify ctx key elifCondTy MockBuiltins.tyBool
            let elifTy = infer ctx elifExpr
            unify ctx key thenTy elifTy

        match elseB with
        | ValueSome(ElseBranch(expr = elseExpr)) ->
            let elseTy = infer ctx elseExpr
            unify ctx key thenTy elseTy
            thenTy
        | ValueNone ->
            // `if c then e` (no else) requires e : unit. Tiny subset
            // doesn't have unit yet — surface as a diagnostic.
            ctx.Diagnostics.Add
                {
                    Key = key
                    Message = "if-then without else not yet supported"
                    Severity = Error
                }

            thenTy

    and private inferFun
        (ctx: PassContext)
        (argPats: ImmutableArray<Pat<SyntaxToken>>)
        (body: Expr<SyntaxToken>)
        : SemType =
        let argTypes = [ for p in argPats -> inferPat ctx p ]
        let bodyTy = infer ctx body
        List.foldBack (fun a r -> TyFun(a, r)) argTypes bodyTy

    and private inferTuple (ctx: PassContext) (items: ImmutableArray<Expr<SyntaxToken>>) : SemType =
        TyTuple [ for e in items -> infer ctx e ]

    and private inferSequential (ctx: PassContext) (key: NodeKey) (items: ImmutableArray<Expr<SyntaxToken>>) : SemType =
        // `e1; e2; …; en` — all but the last must be unit, result is the
        // last's type. A `Sequential` with fewer than two items shouldn't
        // come from the parser, but if it does, fall through harmlessly.
        if items.Length = 0 then
            MockBuiltins.tyUnit
        else
            for i = 0 to items.Length - 2 do
                let ty = infer ctx items.[i]
                unify ctx key ty MockBuiltins.tyUnit

            infer ctx items.[items.Length - 1]

    /// Translate a syntactic `Type<SyntaxToken>` into a `SemType`. The tiny
    /// subset only recognises the primitive built-ins (`int`, `bool`,
    /// `unit`) by name; anything else turns into a `TyConst <name>` whose
    /// unification will succeed only against an identical `TyConst`. Typars
    /// (`'a`) and generic types are TODO — they need typar-scoping plumbing
    /// we don't have yet.
    and private translateType (ctx: PassContext) (t: Type<SyntaxToken>) : SemType =
        match t with
        | Type.ParenType(typ = inner) -> translateType ctx inner
        | Type.NamedType li when li.Idents.Length = 1 ->
            let name = ctx.NameOf li.Idents.[0]

            match name with
            | "int" -> MockBuiltins.tyInt
            | "bool" -> MockBuiltins.tyBool
            | "unit" -> MockBuiltins.tyUnit
            | _ -> TyConst name
        | Type.FunctionType(fromType = from; toType = into) -> TyFun(translateType ctx from, translateType ctx into)
        | Type.TupleType(types = types) -> TyTuple [ for t in types -> translateType ctx t ]
        | _ ->
            // TODO: VarType (typars), GenericType, etc. Free variable until
            // we model them properly — unification will pin it via context.
            TyVar(TypeVar())

    and private inferTypeAnnotation
        (ctx: PassContext)
        (key: NodeKey)
        (inner: Expr<SyntaxToken>)
        (t: Type<SyntaxToken>)
        : SemType =
        let innerTy = infer ctx inner
        let annTy = translateType ctx t
        unify ctx key innerTy annTy
        annTy

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
            // `Expr.LetOrUse(body = ValueNone)` is `use fixed` (module-level
            // lets are ModuleElems, not Expr.LetOrUse). Tiny subset doesn't
            // support pinning — surface loudly so Freeze doesn't see a
            // best-effort type for an unsupported construct.
            failwith "Unification: Expr.LetOrUse with no body (UseFixed) not supported"

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
