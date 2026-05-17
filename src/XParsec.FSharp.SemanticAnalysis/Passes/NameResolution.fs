namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Immutable
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// Pre:  ctx.Desugared populated.
// Post: ctx.Binding populated for every ident-use site that resolves to a
//       local binding. Unresolved names that the provider also doesn't know
//       become Error diagnostics.
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

    /// Extract the (name, binding-site NodeKey) that a pattern introduces.
    /// Returns ValueNone for patterns that don't bind a single name
    /// (Wildcard, Tuple decomposition, etc. — tiny subset only handles
    /// NamedSimple).
    let private patNameAndKey (ctx: PassContext) (p: Pat<SyntaxToken>) : (string * NodeKey) voption =
        match p with
        | Pat.NamedSimple t -> ValueSome(ctx.NameOf t, CstKeys.ofPat p)
        | _ -> ValueNone

    let private extendScope (ctx: PassContext) (pats: ImmutableArray<Pat<SyntaxToken>>) (acc: Scope) : Scope =
        let mutable s = acc

        for p in pats do
            match patNameAndKey ctx p with
            | ValueSome(n, k) -> s <- Map.add n k s
            | ValueNone -> ()

        s

    let private bindingsToScope (ctx: PassContext) (bindings: ImmutableArray<Binding<SyntaxToken>>) : Scope =
        let mutable s = Map.empty

        for b in bindings do
            match patNameAndKey ctx b.headPat with
            | ValueSome(n, k) -> s <- Map.add n k s
            | ValueNone -> ()

        s

    let rec private walkExpr (ctx: PassContext) (scope: Scope list) (e: Expr<SyntaxToken>) =
        match e with
        | Expr.Const _ -> ()
        | Expr.Ident tok -> resolveIdent ctx scope tok (CstKeys.ofExpr e)
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
            resolveIdent ctx scope li.Idents.[0] (CstKeys.ofExpr e)
        | Expr.LongIdentOrOp _ -> ()
        | Expr.App(fn, args) ->
            walkExpr ctx scope fn

            for a in args do
                walkExpr ctx scope a
        | Expr.InfixApp(left, _, right) ->
            walkExpr ctx scope left
            walkExpr ctx scope right
        | Expr.Fun(argumentPats = argPats; expr = body) ->
            let bodyScope = extendScope ctx argPats Map.empty :: scope
            walkExpr ctx bodyScope body
        | Expr.LetOrUse(bindings = bindings; body = body) ->
            walkBindings ctx scope bindings

            match body with
            | ValueSome b ->
                let bodyScope = bindingsToScope ctx bindings :: scope
                walkExpr ctx bodyScope b
            | ValueNone -> ()
        | Expr.EnclosedBlock(expr = inner) -> walkExpr ctx scope inner
        | _ -> ()

    and private walkBindings (ctx: PassContext) (scope: Scope list) (bindings: ImmutableArray<Binding<SyntaxToken>>) =
        for b in bindings do
            // Function-form: `let f x y = ...` — push parameter names before walking RHS.
            let rhsScope =
                if b.argumentPats.IsEmpty then
                    scope
                else
                    extendScope ctx b.argumentPats Map.empty :: scope

            walkExpr ctx rhsScope b.expr

    let private walkModuleElem (ctx: PassContext) (scope: Scope list) (m: ModuleElem<SyntaxToken>) : Scope list =
        match m with
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bindings)) ->
            walkBindings ctx scope bindings
            // Extend the topmost scope so later module elements can see these bindings.
            match scope with
            | [] -> [ bindingsToScope ctx bindings ]
            | top :: rest ->
                let merged =
                    bindings
                    |> Seq.fold
                        (fun acc b ->
                            match patNameAndKey ctx b.headPat with
                            | ValueSome(n, k) -> Map.add n k acc
                            | ValueNone -> acc
                        )
                        top

                merged :: rest
        | ModuleElem.Expression e ->
            walkExpr ctx scope e
            scope
        | _ -> scope

    let private walkElems (ctx: PassContext) (elems: ModuleElems<SyntaxToken>) =
        let mutable scope = [ Map.empty ]

        for m in elems do
            scope <- walkModuleElem ctx scope m

    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
        match file with
        | ImplementationFile.AnonymousModule elems -> walkElems ctx elems
        | ImplementationFile.NamedModule(NamedModule.NamedModule(elements = elems)) -> walkElems ctx elems
        | ImplementationFile.Namespaces _ -> ()
