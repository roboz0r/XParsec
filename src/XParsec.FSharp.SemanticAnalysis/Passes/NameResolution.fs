namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Immutable
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// Pre:  ctx.Desugared populated.
// Post: ctx.Binding populated for every ident-use site that resolves to a
//       local binding. Unresolved names that the provider also doesn't know
//       become Error diagnostics.
//
// Recursion is delegated to CstWalk.iterExpr; this pass supplies a Visit
// hook plus the three scope-introducing hooks (EnterFun, EnterBindingRhs,
// EnterLetBody). The walker thread-restores scope automatically at each
// recursive boundary, so the caller's scope is never polluted by a
// lambda/let body's locals.
//
// Notes for the tiny subset:
//   - Operators inside InfixApp / PrefixApp are NOT resolved here. Desugar
//     records them as DesugaredForm.OpName, and Unification consults the
//     provider directly when typing the application.
//   - External-symbol resolution writes NO entry to ctx.Binding; Unification
//     re-queries the provider when it sees a missing binding entry.
//   - IsInline / IsMutable are always false for the tiny subset. They'll get
//     real values when inline / mutable keywords are handled.

module NameResolution =

    type private Scope = Map<string, NodeKey>

    let private resolveIdent (ctx: PassContext) (scope: Scope list) (tok: SyntaxToken) (useKey: NodeKey) =
        let name = ctx.NameOf tok

        let rec lookup (s: Scope list) =
            match s with
            | [] -> ValueNone
            | head :: rest ->
                match Map.tryFind name head with
                | Some bs -> ValueSome bs
                | None -> lookup rest

        match lookup scope with
        | ValueSome bindingSite ->
            ctx.Binding.Set(
                useKey,
                {
                    BindingSite = bindingSite
                    IsInline = false
                    IsMutable = false
                }
            )
        | ValueNone ->
            match ctx.Provider.TryLookup name with
            | ValueSome _ -> ()
            | ValueNone ->
                ctx.Diagnostics.Add
                    {
                        Key = useKey
                        Message = sprintf "Unresolved identifier: %s" name
                        Severity = Error
                    }

    /// Every (name, NodeKey) pair introduced by a pattern. Recurses through
    /// parens, tuples, as-bindings, and type annotations; returns [] for
    /// patterns that bind nothing (Wildcard, Const).
    let rec private bindingsOfPat (ctx: PassContext) (p: Pat<SyntaxToken>) : (string * NodeKey) list =
        match p with
        | Pat.NamedSimple t -> [ ctx.NameOf t, CstKeys.ofPat p ]
        | Pat.Wildcard _
        | Pat.Const _
        | Pat.EmptyBlock _ -> []
        | Pat.EnclosedBlock(pat = inner) -> bindingsOfPat ctx inner
        | Pat.Tuple(patterns = pats) -> [ for sub in pats -> bindingsOfPat ctx sub ] |> List.concat
        | Pat.Typed(pat = inner) -> bindingsOfPat ctx inner
        | Pat.As(pat = inner; ident = ident) -> (ctx.NameOf ident, CstKeys.ofPat p) :: bindingsOfPat ctx inner
        | _ -> []

    let private extendScope (ctx: PassContext) (pats: ImmutableArray<Pat<SyntaxToken>>) (acc: Scope) : Scope =
        let mutable s = acc

        for p in pats do
            for n, k in bindingsOfPat ctx p do
                s <- Map.add n k s

        s

    let private bindingsToScope (ctx: PassContext) (bindings: ImmutableArray<Binding<SyntaxToken>>) : Scope =
        let mutable s = Map.empty

        for b in bindings do
            for n, k in bindingsOfPat ctx b.headPat do
                s <- Map.add n k s

        s

    let private visit (ctx: PassContext) (scope: Scope list) (e: Expr<SyntaxToken>) : unit =
        match e with
        | Expr.Ident tok -> resolveIdent ctx scope tok (CstKeys.ofExpr e)
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
            resolveIdent ctx scope li.Idents.[0] (CstKeys.ofExpr e)
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) ->
            // Multi-segment qualified name (Module.value, Type.Member, …).
            // No local scope can introduce a dotted name, so go straight to
            // the provider with the joined form.
            let qualName = li.Idents |> Seq.map ctx.NameOf |> String.concat "."

            match ctx.Provider.TryLookup qualName with
            | ValueSome _ -> ()
            | ValueNone ->
                ctx.Diagnostics.Add
                    {
                        Key = CstKeys.ofExpr e
                        Message = sprintf "Unresolved qualified name: %s" qualName
                        Severity = Error
                    }
        | Expr.LongIdentOrOp lio ->
            // Operator-form long idents (`A.(+)`, `(*)`) still need their
            // own resolution story. Surface the gap rather than silently
            // skipping.
            let firstTok = CstKeys.firstTokenOfLongIdentOrOp lio
            let displayName = ctx.NameOf firstTok

            ctx.Diagnostics.Add
                {
                    Key = CstKeys.ofExpr e
                    Message = sprintf "Operator-form qualified names not yet resolved (starting at '%s')" displayName
                    Severity = Error
                }
        | _ -> ()

    let private mkWalker (ctx: PassContext) : CstWalk.ExprWalker<Scope list> =
        {
            Visit = visit ctx
            EnterFun = fun scope argPats -> extendScope ctx argPats Map.empty :: scope
            EnterBindingRhs =
                fun scope isRec siblings b ->
                    // `let rec`: sibling names (including this binding's own name,
                    // so recursive self-reference resolves) are in scope for the RHS.
                    // Function-form: push parameter names on top of that.
                    let mutable s = scope

                    if isRec then
                        s <- bindingsToScope ctx siblings :: s

                    if not b.argumentPats.IsEmpty then
                        s <- extendScope ctx b.argumentPats Map.empty :: s

                    s
            EnterLetBody = fun scope bindings -> bindingsToScope ctx bindings :: scope
            EnterForTo =
                fun scope ident ->
                    let name = ctx.NameOf ident
                    let key = CstKeys.ofForToVar ident
                    Map.ofList [ name, key ] :: scope
            EnterForIn =
                fun scope pat ->
                    let scopeMap = bindingsOfPat ctx pat |> Map.ofList
                    scopeMap :: scope
            EnterMatchArm =
                fun scope pat ->
                    let scopeMap = bindingsOfPat ctx pat |> Map.ofList
                    scopeMap :: scope
        }

    let private walkModuleElem
        (ctx: PassContext)
        (walker: CstWalk.ExprWalker<Scope list>)
        (scope: Scope list)
        (m: ModuleElem<SyntaxToken>)
        : Scope list =
        match m with
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(isRec = isRec; bindings = bindings)) ->
            let isRecursive = isRec.IsSome

            for b in bindings do
                let rhsScope = walker.EnterBindingRhs scope isRecursive bindings b
                CstWalk.iterExpr walker rhsScope b.expr
            // Extend the topmost scope so later module elements can see these bindings.
            match scope with
            | [] -> [ bindingsToScope ctx bindings ]
            | top :: rest ->
                let merged =
                    bindings
                    |> Seq.fold
                        (fun acc b -> (acc, bindingsOfPat ctx b.headPat) ||> List.fold (fun a (n, k) -> Map.add n k a))
                        top

                merged :: rest
        | ModuleElem.Expression e ->
            CstWalk.iterExpr walker scope e
            scope
        | _ -> scope

    let private walkElems
        (ctx: PassContext)
        (walker: CstWalk.ExprWalker<Scope list>)
        (elems: ModuleElems<SyntaxToken>)
        =
        let mutable scope = [ Map.empty ]

        for m in elems do
            scope <- walkModuleElem ctx walker scope m

    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
        let walker = mkWalker ctx

        match file with
        | ImplementationFile.AnonymousModule elems -> walkElems ctx walker elems
        | ImplementationFile.NamedModule(NamedModule.NamedModule(elements = elems)) -> walkElems ctx walker elems
        | ImplementationFile.Namespaces _ -> ()
