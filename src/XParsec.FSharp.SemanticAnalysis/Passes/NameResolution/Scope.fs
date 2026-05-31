namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Immutable
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// Scope tracking and ident-use resolution for NameResolution.
//
// resolveIdent writes ctx.Bindings.Binding for every use site resolving to a
// local; names the provider knows become ctx.Resolution.ExternalValue entries
// (no Binding entry — Unification re-queries the provider); the rest become
// Error diagnostics. Operators in InfixApp/PrefixApp are NOT resolved here;
// Desugar records them as DesugaredForm.OpName and Unification consults the
// provider when typing the application.
//
// IsMutable mirrors the binding's mutableToken, propagated to every use-site
// so Validation's assignment check can read ctx.Bindings.Binding[lhsKey]
// directly. Each binding site also gets a self-entry (BindingSite = key) so
// Validation's value-restriction loop can iterate mutable bindings.

module NameResolutionScope =

    type Scope = Map<string, NodeKey * bool>

    /// True if `name` (possibly dotted) resolves as an external *type* at some
    /// small arity — i.e. a static-member-access receiver
    /// (`EqualityComparer<int>.Default`), not an unresolved value. The receiver's
    /// arity lives on the enclosing `Expr.TypeApp`, which this name's own visit
    /// can't see, so probe a bounded arity range (provider-cached). `tryQualify`
    /// applies the in-scope `open` prefixes. symbol-resolution-plan §7.2, P3.
    let private resolvesAsExternalType (ctx: PassContext) (name: string) : bool =
        let probe n =
            (ctx.Provider.TryLookupType n |> ValueOption.isSome)
            || [ 1; 2; 3; 4 ]
               |> List.exists (fun a -> ctx.Provider.TryLookupType(sprintf "%s`%d" n a) |> ValueOption.isSome)

        OpenScope.tryQualify ctx.Resolution.OpenScope probe name |> ValueOption.isSome

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
        | ValueSome(bindingSite, isMutable) ->
            ctx.Bindings.Binding.Set(
                useKey,
                {
                    BindingSite = bindingSite
                    IsInline = false
                    IsMutable = isMutable
                }
            )
        | ValueNone ->
            // `tryResolve` returns the ExternalSymbol, so its SymbolKey.ValueKey
            // is captured for Freeze to stamp onto TExpr.External (M1).
            match OpenScope.tryResolve ctx.Resolution.OpenScope ctx.Provider.TryLookup name with
            | ValueSome sym -> ctx.Resolution.ExternalValue.Set(useKey, sym.Key)
            | ValueNone ->
                // DU ctors resolve via ctx.Types.CtorIndex in Unification; class
                // names used as ctor-functions live in ctx.Types.Class; external
                // type names used as static-access receivers resolve via the
                // provider in Unification; an external union *case* (`Some` /
                // `None`) resolves via the provider's reverse case index in
                // Unification/Freeze (vesper-lib-test-plan Gap 2 Layer B), so it is
                // suppressed here the same way (it is ambient, like the `option`
                // abbreviation, rather than open-gated in v1). Suppress the
                // unresolved diagnostic for all four.
                if
                    ctx.Types.CtorIndex.ContainsKey name
                    || ctx.Types.Class.ContainsKey name
                    || resolvesAsExternalType ctx name
                    || (ctx.Provider.TryLookupUnionCase name).IsSome
                then
                    ()
                else
                    ctx.Diagnostics.Add
                        {
                            Key = useKey
                            Message = sprintf "Unresolved identifier: %s" name
                            Code = ""
                            Severity = Error
                        }

    /// True if `name` is a ctor reference in pattern position. F# spec treats
    /// uppercase-leading pattern idents as ctor references; we additionally
    /// require a registry hit so unrelated uppercase binders still bind. A local
    /// union case lives in `ctx.Types.CtorIndex`; an *external* (referenced-
    /// package) one resolves through the provider's reverse case index, so a
    /// cross-package `Some x` pattern treats `Some` as a ctor head (binding
    /// nothing) and its sub-patterns as binders, not the whole thing as a binder
    /// (vesper-lib-test-plan Gap 2 Layer C). Empty strings (virtual tokens) never
    /// match.
    let private isCtorName (ctx: PassContext) (name: string) : bool =
        name.Length > 0
        && System.Char.IsUpper name.[0]
        && (ctx.Types.CtorIndex.ContainsKey name
            || (ctx.Provider.TryLookupUnionCase name).IsSome)

    /// Every (name, NodeKey) pair introduced by a pattern; [] for patterns that
    /// bind nothing (Wildcard, Const, nullary ctors).
    let rec bindingsOfPat (ctx: PassContext) (p: Pat<SyntaxToken>) : (string * NodeKey) list =
        match p with
        | Pat.NamedSimple t when isCtorName ctx (ctx.NameOf t) ->
            // Uppercase-leading ident matching a known nullary ctor — a ctor
            // pattern, binds nothing.
            []
        | Pat.NamedSimple t -> [ ctx.NameOf t, CstKeys.ofPat p ]
        | Pat.Wildcard _
        | Pat.Const _
        | Pat.EmptyBlock _ -> []
        | Pat.EnclosedBlock(pat = inner) -> bindingsOfPat ctx inner
        | Pat.Tuple(patterns = pats) -> [ for sub in pats -> bindingsOfPat ctx sub ] |> List.concat
        | Pat.Typed(pat = inner) -> bindingsOfPat ctx inner
        | Pat.As(pat = inner; ident = ident) -> (ctx.NameOf ident, CstKeys.ofPat p) :: bindingsOfPat ctx inner
        | Pat.Record(fieldPats = fieldPats) ->
            [ for FieldPat(pat = sub) in fieldPats -> bindingsOfPat ctx sub ] |> List.concat
        | Pat.Named(longIdent = li; argumentPats = args) when
            li.Idents.Length >= 1
            && isCtorName ctx (ctx.NameOf li.Idents.[li.Idents.Length - 1])
            ->
            // Ctor pattern (`Circle r`, `Result1.Ok x`): head binds nothing,
            // sub-patterns introduce binders.
            [
                for sub in args do
                    yield! bindingsOfPat ctx sub
            ]
        | Pat.Op io ->
            // Operator-named binding head (`let (=) x y = …`): bind the compiled
            // name (`op_Equality`). Use sites resolve through Desugar→External,
            // but Validation's per-binding loop expects a self-entry.
            match Desugar.opPatCompiledName ctx.NameOf io with
            | ValueSome n -> [ n, CstKeys.ofPat p ]
            | ValueNone -> []
        | _ -> []

    /// Lambda args / for-in / match-arm patterns can't carry `mutable`, so every
    /// binder they introduce is immutable.
    let extendScope (ctx: PassContext) (pats: ImmutableArray<Pat<SyntaxToken>>) (acc: Scope) : Scope =
        let mutable s = acc

        for p in pats do
            for n, k in bindingsOfPat ctx p do
                s <- Map.add n (k, false) s

        s

    /// Build the scope additions for a let-group, and write a binding-site
    /// self-entry to ctx.Bindings.Binding for every binder — Validation's
    /// value-restriction loop filters ctx.Bindings.Binding by `kv.Key = rb.BindingSite`.
    let bindingsToScope (ctx: PassContext) (bindings: ImmutableArray<Binding<SyntaxToken>>) : Scope =
        let mutable s = Map.empty

        for b in bindings do
            let isMut = b.mutableToken.IsSome

            for n, k in bindingsOfPat ctx b.headPat do
                s <- Map.add n (k, isMut) s

                ctx.Bindings.Binding.Set(
                    k,
                    {
                        BindingSite = k
                        IsInline = b.inlineToken.IsSome
                        IsMutable = isMut
                    }
                )

        s

    let private visit (ctx: PassContext) (scope: Scope list) (e: Expr<SyntaxToken>) : unit =
        match e with
        | Expr.Ident tok -> resolveIdent ctx scope tok (CstKeys.ofExpr e)
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
            resolveIdent ctx scope li.Idents.[0] (CstKeys.ofExpr e)
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) ->
            // Multi-segment: either a chained field access (`r.X.Y`, head local)
            // or a qualified name (`Math.PI`, provider). Try the local head first;
            // record-field resolution happens in Unification once the head's type is known.
            let head = li.Idents.[0]
            let headName = ctx.NameOf head

            let rec lookup (s: Scope list) =
                match s with
                | [] -> ValueNone
                | top :: rest ->
                    match Map.tryFind headName top with
                    | Some bs -> ValueSome bs
                    | None -> lookup rest

            match lookup scope with
            | ValueSome(bindingSite, isMutable) ->
                // Key the head's binding entry under ExprIdent on the head token
                // so later passes look up the receiver's type by the same key.
                ctx.Bindings.Binding.Set(
                    NodeKey.ofToken head NodeKind.ExprIdent,
                    {
                        BindingSite = bindingSite
                        IsInline = false
                        IsMutable = isMutable
                    }
                )
            | ValueNone ->
                let qualName = li.Idents |> Seq.map ctx.NameOf |> String.concat "."

                match OpenScope.tryResolve ctx.Resolution.OpenScope ctx.Provider.TryLookup qualName with
                | ValueSome sym -> ctx.Resolution.ExternalValue.Set(CstKeys.ofExpr e, sym.Key)
                | ValueNone ->
                    // `Result2.Ok` — two-segment qualified ctor; resolves through
                    // ctx.Types.Union, suppress so Unification picks it up.
                    let isQualifiedCtor =
                        li.Idents.Length = 2
                        && ctx.Types.Union.ContainsKey(ctx.NameOf li.Idents.[0])
                        && (let info = ctx.Types.Union.[ctx.NameOf li.Idents.[0]]
                            let caseName = ctx.NameOf li.Idents.[1]
                            info.Cases |> Array.exists (fun c -> c.Name = caseName))

                    // `Math.Pi` / `Box.Empty` — two-segment qualified static member,
                    // incl. union augmentation statics (P3d.3). Same suppression.
                    let isQualifiedStatic =
                        li.Idents.Length = 2
                        && (let typeName = ctx.NameOf li.Idents.[0]
                            let memberName = ctx.NameOf li.Idents.[1]

                            let staticIn (members: TypeMemberInfo[]) =
                                members |> Array.exists (fun m -> m.IsStatic && m.Name = memberName)

                            (ctx.Types.Class.ContainsKey typeName
                             && staticIn ctx.Types.Class.[typeName].Members)
                            || (ctx.Types.Union.ContainsKey typeName
                                && staticIn ctx.Types.Union.[typeName].Members))

                    // A non-generic external static member folds into one LongIdent
                    // (`System.Console.Out`), so the receiver type is the *prefix*
                    // (all but the last segment). If that resolves as an external
                    // type, leave the member to Unification's tryExternalStaticLongIdent
                    // (which falls through silently when the tail isn't accessible, so
                    // suppression here doesn't manufacture a member that isn't there).
                    let isExternalStaticMember =
                        li.Idents.Length >= 2
                        && (let prefix =
                                seq { for i in 0 .. li.Idents.Length - 2 -> ctx.NameOf li.Idents.[i] }
                                |> String.concat "."

                            resolvesAsExternalType ctx prefix)

                    if
                        isQualifiedCtor
                        || isQualifiedStatic
                        || resolvesAsExternalType ctx qualName
                        || isExternalStaticMember
                    then
                        ()
                    else
                        ctx.Diagnostics.Add
                            {
                                Key = CstKeys.ofExpr e
                                Message = sprintf "Unresolved qualified name: %s" qualName
                                Code = ""
                                Severity = Error
                            }
        | Expr.LongIdentOrOp(LongIdentOrOp.Op(IdentOrOp.ParenOp(opName = OpName.SymbolicOp op))) when
            (Desugar.symbolicOpCompiledName op.Token |> ValueOption.isSome)
            ->
            // `(+)` and friends used as a value resolve through the provider in
            // Unification (no local binding), so not an unresolved-name error.
            ()
        | Expr.LongIdentOrOp lio ->
            // TODO: operator-form long idents (`A.(+)`, `(*)`) need their own
            // resolution story. Surface the gap rather than silently skipping.
            let firstTok = CstKeys.firstTokenOfLongIdentOrOp lio
            let displayName = ctx.NameOf firstTok

            ctx.Diagnostics.Add
                {
                    Key = CstKeys.ofExpr e
                    Message = sprintf "Operator-form qualified names not yet resolved (starting at '%s')" displayName
                    Code = ""
                    Severity = Error
                }
        | _ -> ()

    let mkWalker (ctx: PassContext) : CstWalk.ExprWalker<Scope list> =
        {
            Visit = visit ctx
            EnterFun = fun scope argPats -> extendScope ctx argPats Map.empty :: scope
            EnterBindingRhs =
                fun scope isRec siblings b ->
                    // `let rec`: sibling names (including this binding's own name,
                    // for recursive self-reference) are in scope for the RHS.
                    // Function-form: push parameter names on top.
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
                    Map.ofList [ name, (key, false) ] :: scope
            EnterForIn =
                fun scope pat ->
                    let scopeMap =
                        bindingsOfPat ctx pat |> List.map (fun (n, k) -> n, (k, false)) |> Map.ofList

                    scopeMap :: scope
            EnterMatchArm =
                fun scope pat ->
                    let scopeMap =
                        bindingsOfPat ctx pat |> List.map (fun (n, k) -> n, (k, false)) |> Map.ofList

                    scopeMap :: scope
        }
