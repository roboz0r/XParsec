namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic

open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes

// The final pipeline step: one O(n) `SemType -> FrozenType` rebuild of the tree.

[<RequireQualifiedAccess>]
module Freeze =

    /// Attribute each typar root to the body-local scheme that quantified it, and to its index
    /// there. A non-generalized binding (the value restriction) keeps residual roots but has
    /// no scheme, so it contributes none.
    let private schemeBoundVars (ctx: PassContext) : Dictionary<TyVarId, struct (SchemeId * int)> =
        let map = Dictionary<TyVarId, struct (SchemeId * int)>()

        ctx.Bindings.Scheme.AsDictionary()
        |> Seq.sortBy (fun (KeyValue(boundVar, _)) -> boundVar.Raw)
        |> Seq.iteri (fun schemeIndex (KeyValue(_, scheme)) ->
            scheme.Quantified
            |> Seq.iteri (fun i tv -> map.[(UnionFind.find ctx.Store tv).Id] <- struct (SchemeId schemeIndex, i))
        )

        map

    /// A residual unlinked `TyVar` is TOLERATED here: in `let f () = let g = fun x -> x in
    /// (g, g)`, `g`'s own root occurs nowhere in `f`'s type, so nothing ever remapped it.
    let private freezeTy
        (store: TypeStore)
        (schemes: Dictionary<TyVarId, struct (SchemeId * int)>)
        (t: SemType)
        : FrozenType =
        let onVar (v: SemType) : FrozenType =
            match v with
            | TyVar tv ->
                // Key on the union-find ROOT: two `TyVar` nodes in the same class are
                // the same typar and must land on the same `FTLocalTypar`.
                match schemes.TryGetValue((UnionFind.find store tv).Id) with
                | true, struct (scheme, index) -> FTLocalTypar(scheme, index)
                | _ -> FTUnknown UnknownReason.UnresolvedTypar
            | _ -> failwithf "Freeze.freezeTy: `toFrozenWith` invoked the TyVar policy on a non-TyVar: %A" v

        // Deep-`zonk` first: elaboration leaves fields holding a `TyVar` root linked to a
        // concrete type, so only a genuinely UNLINKED root reaches `onVar`.
        Unification.zonk store t |> FrozenTypeBridge.toFrozenWith onVar

    /// Is a use of this decl spliced rather than called? Two shapes: an explicit `let inline`,
    /// and a `let` value whose body is a single zero-operand intrinsic (`let emptyDocs =
    /// (# "[]" #)`), which carries no `inline` keyword but splices at every cross-file use.
    let private isInlineVocabulary (d: TDecl) : bool =
        match d with
        | TDecl.Let(TPat.NamedSimple _, _, true, _) -> true
        | TDecl.Let(TPat.NamedSimple _, _, false, _) -> (Inline.nullaryIntrinsicValueBody d).IsSome
        | _ -> false

    /// Rewrite `Var` -> `External` + `SymbolKey` for every module-level sibling: a `Var` references
    /// a bound variable that exists only in this file's tree, so a consumer could not resolve it.
    let private rewriteSiblingRefs (siblings: Map<NodeKey, ModuleBindingInfo>) (d: TDecl) : TDecl =
        let mapper: TastWalk.Mapper =
            { TastWalk.identityMapper with
                OverrideExpr =
                    fun _ e ->
                        match e with
                        | TExpr.Var(k, ty, tok) ->
                            match Map.tryFind k siblings with
                            | Some info -> ValueSome(TExpr.External(info.Name, ValueSome info.Key, ty, tok))
                            | None -> ValueNone
                        | _ -> ValueNone
            }

        match d with
        | TDecl.Let(pat, value, isInline, ty) -> TDecl.Let(pat, TastWalk.mapExpr mapper value, isInline, ty)
        | other -> other

    /// Every `Var` in the rewritten body referencing a bound variable the splice does not re-create,
    /// in practice a module-level `let (a, b) = p`, which binds several names at once and so has
    /// no key. Paired with the FIRST reference's token, which spells what the user wrote.
    let private freeVarsOfBody (d: TDecl) : (NodeKey * SyntaxToken) list =
        match d with
        // The decl's own bound variable is in scope in its body (a template may be recursive), so it
        // seeds the bound set.
        | TDecl.Let(pat, value, _, _) ->
            let free = TastWalk.freeVars (TastWalk.boundVarsOfTPat pat) value
            let seen = HashSet<NodeKey>(HashIdentity.Structural)
            let sites = ResizeArray<NodeKey * SyntaxToken>()

            let it =
                { TastWalk.identityIter with
                    VisitExpr =
                        fun _ e ->
                            match e with
                            | TExpr.Var(k, _, tok) when free.Contains k && seen.Add k -> sites.Add(k, tok)
                            | _ -> ()

                            true
                }

            TastWalk.iterExpr it value
            List.ofSeq sites
        | _ -> []

    /// `true` ⇒ publish. A template whose rewritten body still has a free `Var` is reported
    /// and dropped from `InlineBodies`; it still splices correctly within this file.
    let private publishable (ctx: PassContext) (declTok: SyntaxToken) (rewritten: TDecl) : bool =
        match freeVarsOfBody rewritten with
        | [] -> true
        | free ->
            // A virtual token spells nothing, so fall back to the key, which at least says
            // where the reference came from.
            let name (k: NodeKey, tok: SyntaxToken) =
                match ctx.NameOf tok with
                | "" -> string k
                | spelled -> spelled

            ctx.Report(
                declTok,
                Kind.Message(
                    sprintf
                        "This inline binding cannot be published: its body references %s, which has no exportable identity (a module-level binding that binds several names at once has no single symbol key a consumer could resolve). Bind it with its own `let`."
                        (free |> List.map (fun site -> sprintf "'%s'" (name site)) |> String.concat ", ")
                )
            )

            false

    /// The frozen file in DU form, the shape the node-for-node tree map consumes.
    let private toFrozenFile (ctx: PassContext) (tast: TastFile) : Frozen.TastFile =
        // Publication is ADDITIVE: `Decls` keeps the binding. A bound variable with no
        // `ModuleBindingInfo` (a destructuring `let` pattern) has no key, so it publishes nowhere.
        let inlineBodies = ResizeArray<TInlineValue>()

        // Widened to the REFERENCE domain: the sibling rewrite is driven by a body's
        // `TExpr.Var`s, which identify their bound variable by `NodeKey`.
        let siblingsByRef = BoundVarKey.widenMap tast.ModuleMembers

        let publishedInfo (pattern: TPat) =
            match BoundVarKey.ofPat pattern with
            | ValueNone -> ValueNone
            | ValueSome boundVar ->
                match Map.tryFind boundVar tast.ModuleMembers with
                | Some info -> ValueSome(struct (boundVar, info))
                | None -> ValueNone

        for d in tast.Decls do
            match d with
            | TDecl.Let(pattern, _, _, _) when isInlineVocabulary d ->
                match publishedInfo pattern with
                | ValueSome(boundVar, info) ->
                    let k = BoundVarKey.identity boundVar
                    // The stashed TEMPLATE, not `d`: `d` is the emitted ordinary function,
                    // already walked with its static-opt clauses and trait calls resolved
                    // against the unground definition site. A nullary alias has no stash.
                    let template =
                        match ctx.InlineTemplates.TryGetValue k with
                        | true, t -> t
                        | _ -> d

                    let rewritten = rewriteSiblingRefs siblingsByRef template

                    if publishable ctx (TastWalk.patTok pattern) rewritten then
                        inlineBodies.Add
                            {
                                TInlineValue.Key = info.Key
                                Body =
                                    {
                                        Decl = rewritten
                                        ParamAttrs =
                                            match ctx.InlineParamAttrs.TryGetValue k with
                                            | true, a -> a
                                            | _ -> EqArray.empty
                                    }
                            }
                | ValueNone -> ()
            | _ -> ()

        let frozen =
            { tast with
                InlineBodies = EqArray.ofList (List.ofSeq inlineBodies)
                // Re-snapshot: the tree's `Diagnostics` were taken BEFORE the freeze, so a
                // publish failure raised above would otherwise reach `ctx` and no one else.
                Diagnostics = List.ofSeq ctx.Diagnostics
            }

        TastConvert.file (freezeTy ctx.Store (schemeBoundVars ctx)) id frozen

    /// The assembly's output: the frozen file as struct-of-arrays pools.
    let run (ctx: PassContext) (tast: TastFile) : FrozenPools =
        // No record means no source spells the bound variable: a class's `this`/`base` and a
        // spliced bound variable are both minted.
        let identOf (b: BoundVarKey) =
            match ctx.BoundVarNames.TryGetValue b with
            | ValueSome ident -> ident
            | ValueNone -> BoundVarIdent.unnamed

        toFrozenFile ctx tast |> TastPools.toPools ctx.Origin identOf
