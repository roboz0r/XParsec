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

    /// Resolve `name` (possibly dotted) as an external *type* at exactly `arity` —
    /// the receiver's type-arg count, supplied by the enclosing `Expr.TypeApp`
    /// (0 for a non-generic static-access receiver like `System.Console`). Applies
    /// the in-scope `open` prefixes (`tryResolve`'s candidate order: bare/abbrev-
    /// expanded then each prefix); per qualified candidate it probes the arity-
    /// suffixed compiled name first (metadata keys a generic `Name`arity`; the
    /// contract layer keys it bare). Returns the use-site `SymbolKey` minted from
    /// the matched shape's origin + compiled name. This replaces the former bounded
    /// `[1;2;3;4]` arity scan: the arity is now exact because the TypeApp visit
    /// resolves receiver+arity together.
    let tryResolveExternalTypeKey (ctx: PassContext) (name: string) (arity: int) : SymbolKey voption =
        let keysFor (n: string) =
            if arity = 0 then
                [ n ]
            else
                [ SymbolKeyOps.arityName n arity; n ]

        let shapeArity (shape: ExternalTypeShape) =
            match shape with
            | ExternalTypeShape.Class info -> info.Arity
            | ExternalTypeShape.Intrinsic _ -> 0
            | ExternalTypeShape.Record(arity = a)
            | ExternalTypeShape.Union(arity = a)
            | ExternalTypeShape.Abbrev(arity = a)
            | ExternalTypeShape.Opaque(arity = a) -> a

        // Mint from the matched shape's origin where one exists (Class/Union/Record
        // carry the home assembly + namespace); the origin-less shapes fall back to
        // splitting the qualified compiled name. Mirrors `Translate`'s nominal mint.
        let keyOf (compiled: string) (shape: ExternalTypeShape) =
            match shape with
            | ExternalTypeShape.Class info -> SymbolKeyOps.externalTypeKey info.Origin compiled arity
            | ExternalTypeShape.Record(origin = o)
            | ExternalTypeShape.Union(origin = o) -> SymbolKeyOps.externalTypeKey o compiled arity
            | ExternalTypeShape.Abbrev _
            | ExternalTypeShape.Intrinsic _
            | ExternalTypeShape.Opaque _ -> SymbolKeyOps.qualifiedTypeKey compiled arity

        let lookup (candidate: string) : SymbolKey voption =
            let rec go keys =
                match keys with
                | [] -> ValueNone
                | key :: rest ->
                    match ctx.Provider.TryLookupType key with
                    | ValueSome shape when shapeArity shape = arity -> ValueSome(keyOf key shape)
                    | _ -> go rest

            go (keysFor candidate)

        OpenScope.tryResolve ctx.Resolution.OpenScope lookup name

    /// True if `name` (possibly dotted) resolves as a *non-generic* external type —
    /// the arity-0 static-member-access receiver (`System.Console.Out`,
    /// `Math.Pi`). Generic receivers (`EqualityComparer<int>.Default`) are no longer
    /// probed here: the TypeApp visit resolves them at exact arity and stamps the
    /// receiver's `ResolvedType`, which the suppression sites check directly.
    let private resolvesAsExternalType (ctx: PassContext) (name: string) : bool =
        (tryResolveExternalTypeKey ctx name 0).IsSome

    /// True if the *bare* (unqualified) `name` resolves to an external union case
    /// whose declaring union is NOT `[<RequireQualifiedAccess>]`. An RQA union's
    /// cases are reachable only through the qualified form (`Color.Red`), so a bare
    /// hit on one is rejected here, matching F#. The
    /// qualified paths (`isExternalQualifiedCase`, `tryExternalCtorType` with a
    /// qualifier) resolve RQA cases unchanged — this guard is bare-name only.
    let private resolvesAsBareExternalCase (ctx: PassContext) (name: string) : bool =
        ctx.Provider.TryLookupUnionCase name
        |> ValueOption.exists (fun uc -> uc.ResolvesWith ValueNone)

    /// The dotted receiver name of an `Expr.TypeApp`, when it is an identifier /
    /// long-identifier the provider could know as a type. `ValueNone` for receiver
    /// shapes that are never an external type name (e.g. an applied expression).
    let private typeAppReceiverName (ctx: PassContext) (receiver: Expr<SyntaxToken>) : string voption =
        match receiver with
        | Expr.Ident tok -> ValueSome(ctx.NameOf tok)
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) ->
            ValueSome(li.Idents |> Seq.map ctx.NameOf |> String.concat ".")
        | _ -> ValueNone

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
                // Unification/Freeze, so it is
                // suppressed here the same way (it is ambient, like the `option`
                // abbreviation, rather than open-gated in v1). Suppress the
                // unresolved diagnostic for all four.
                if
                    ctx.Types.CtorIndex.ContainsKey name
                    || ctx.Types.Class.ContainsKey name
                    // A generic external-type receiver (`EqualityComparer<int>`) was
                    // resolved at exact arity by the enclosing TypeApp visit, which
                    // stamped this use site's `ResolvedType`; a non-generic one falls
                    // to the arity-0 `resolvesAsExternalType`.
                    || ctx.Resolution.ResolvedType.ContainsKey useKey
                    || resolvesAsExternalType ctx name
                    // A bare external union case resolves only when its union is NOT
                    // `[<RequireQualifiedAccess>]` — F# rejects the short `Red` form
                    // for an RQA `Color`.
                    || resolvesAsBareExternalCase ctx name
                    // The printf family (`printf`/`printfn`/`sprintf`/`eprintf`/
                    // `fprintf`/…) is a front-end intrinsic: `InferApp` types it via
                    // `PrintfSpec`, not a provider symbol. So a family member resolves
                    // even when the contract doesn't declare it (the writer families
                    // are not in the `Vesper.Printf` contract); don't flag it unbound.
                    || (PrintfSpec.tryFamily name |> ValueOption.isSome)
                then
                    ()
                else
                    ctx.Error(useKey, sprintf "Unresolved identifier: %s" name)

    /// True if `name` is a ctor reference in pattern position. F# spec treats
    /// uppercase-leading pattern idents as ctor references; we additionally
    /// require a registry hit so unrelated uppercase binders still bind. A local
    /// union case lives in `ctx.Types.CtorIndex`; an *external* (referenced-
    /// package) one resolves through the provider's reverse case index, so a
    /// cross-package `Some x` pattern treats `Some` as a ctor head (binding
    /// nothing) and its sub-patterns as binders, not the whole thing as a binder
    /// Empty strings (virtual tokens) never
    /// match.
    let private isCtorName (ctx: PassContext) (name: string) : bool =
        name.Length > 0
        && System.Char.IsUpper name.[0]
        && (ctx.Types.CtorIndex.ContainsKey name
            // A bare RQA external case is not a ctor head in pattern position either
            // — only its qualified form is.
            || resolvesAsBareExternalCase ctx name)

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
        | Pat.Attributed(pat = inner) -> bindingsOfPat ctx inner
        | Pat.As(pat = inner; ident = ident) -> (ctx.NameOf ident, CstKeys.ofPat p) :: bindingsOfPat ctx inner
        | Pat.TypeTestAs(pat = inner) ->
            // `:? T as x` — the inner pattern (an ident) is the binder. The test
            // type isn't a binding site; recurse into the inner pattern only.
            bindingsOfPat ctx inner
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
        | Pat.Cons(head = h; tail = t) ->
            // `h :: t`: the `::` head binds nothing; both sub-patterns introduce
            // binders.
            bindingsOfPat ctx h @ bindingsOfPat ctx t
        | Pat.Elems(pats = pats) ->
            // `[a; b; c]` list-literal pattern (the multi-element form, wrapped in
            // `EnclosedBlock(List, …)`): each element introduces binders.
            [ for sub in pats -> bindingsOfPat ctx sub ] |> List.concat
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
                // G15: `Module.member` where `Module` is a *local* (in-file) module
                // and `member` one of its `let`-bound values/functions. The module
                // tree is flattened before this walk, so the sibling is unresolvable
                // by the provider (which only knows dependency packages) and by the
                // local head lookup (a module name is not a value binding). Resolve
                // it against the pre-pass `LocalModules` registry and record a
                // use-site `Binding` entry pointing at the member's binding site —
                // exactly the shape a single-ident local resolves to, so Unification
                // (`inferIdentDefault` → `instantiateBinding`) and Freeze
                // (`translateIdent` → `TExpr.Var`) treat it as an ordinary local
                // reference. The module name is the second-to-last segment (handles
                // the 2-segment `SetTree.add`).
                let tryLocalModuleMember () : bool =
                    if li.Idents.Length >= 2 then
                        let moduleName = ctx.NameOf li.Idents.[li.Idents.Length - 2]
                        let memberName = ctx.NameOf li.Idents.[li.Idents.Length - 1]

                        match ctx.Resolution.LocalModules.TryGetValue moduleName with
                        | true, members ->
                            match members.TryGetValue memberName with
                            | true, bindingKey ->
                                ctx.Bindings.Binding.Set(
                                    CstKeys.ofExpr e,
                                    {
                                        BindingSite = bindingKey
                                        IsInline = false
                                        IsMutable = false
                                    }
                                )

                                true
                            | false, _ -> false
                        | false, _ -> false
                    else
                        false

                // Everything below G15: the qualified name names something outside
                // the local-module registry — an external value, a local qualified
                // ctor/static, an external union case/static, or genuinely unresolved.
                let resolveQualifiedExternal () =
                    let qualName = li.Idents |> Seq.map ctx.NameOf |> String.concat "."

                    match OpenScope.tryResolve ctx.Resolution.OpenScope ctx.Provider.TryLookup qualName with
                    | ValueSome sym -> ctx.Resolution.ExternalValue.Set(CstKeys.ofExpr e, sym.Key)
                    | ValueNone ->
                        // `Result2.Ok` — two-segment qualified ctor; resolves through
                        // ctx.Types.Union, suppress so Unification picks it up.
                        let isQualifiedCtor =
                            li.Idents.Length = 2
                            && TypeRegistry.localQualifiedCase
                                ctx.Types
                                (ctx.NameOf li.Idents.[0])
                                (ctx.NameOf li.Idents.[1])

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

                        // `E.C1` — a two-segment enum-case access. The head names a
                        // project-local enum, so suppress (Unification's `InferIdentExpr`
                        // enum arm resolves a valid case to `TyEnum` and emits the precise
                        // "Enum 'E' has no case 'C'" for a bad tail — one diagnostic, not
                        // a redundant "unresolved qualified name" on top). Mirrors the
                        // `isQualifiedCtor` / `isQualifiedStatic` suppressions.
                        let isEnumCase =
                            li.Idents.Length = 2
                            && (TypeRegistry.tryEnum ctx.Types (ctx.NameOf li.Idents.[0])).IsSome

                        // `Result.Ok` / `Option.Some` — a qualified *external* union case.
                        // The declaring union may be generic (`Result\`2`), but it is
                        // written without type args, so there is no `Expr.TypeApp` to
                        // recover the arity from. The case name is globally unique in the
                        // provider's reverse index, so resolve it arity-free instead of
                        // probing the union type at a guessed arity — this is what the old
                        // `isExternalStaticMember` prefix arity-scan was doing for these by
                        // accident. Mirrors the single-ident `TryLookupUnionCase`
                        // suppression and the local `isQualifiedCtor` arm; Unification /
                        // Freeze resolve the case.
                        let isExternalQualifiedCase =
                            li.Idents.Length >= 2
                            && (ctx.Provider.TryLookupUnionCase(ctx.NameOf li.Idents.[li.Idents.Length - 1])).IsSome

                        // A non-generic external static member folds into one LongIdent
                        // (`System.Console.Out`), so the receiver type is the *prefix*
                        // (all but the last segment). If that resolves as an external
                        // type, leave the member to Unification's tryExternalStaticLongIdent
                        // (which falls through silently when the tail isn't accessible, so
                        // suppression here doesn't manufacture a member that isn't there).
                        // A *generic* receiver requires explicit type args (a TypeApp,
                        // handled by `ResolvedType` above), so the prefix probe is arity-0.
                        let isExternalStaticMember =
                            li.Idents.Length >= 2
                            && (let prefix =
                                    seq { for i in 0 .. li.Idents.Length - 2 -> ctx.NameOf li.Idents.[i] }
                                    |> String.concat "."

                                resolvesAsExternalType ctx prefix)

                        if
                            isQualifiedCtor
                            || isQualifiedStatic
                            || isEnumCase
                            || isExternalQualifiedCase
                            // A generic external-type receiver written qualified
                            // (`System.Collections.Generic.List<int>.Empty`) was resolved
                            // at exact arity by the enclosing TypeApp visit, stamping this
                            // LongIdent's `ResolvedType`.
                            || ctx.Resolution.ResolvedType.ContainsKey(CstKeys.ofExpr e)
                            || resolvesAsExternalType ctx qualName
                            || isExternalStaticMember
                        then
                            ()
                        else
                            ctx.Error(CstKeys.ofExpr e, sprintf "Unresolved qualified name: %s" qualName)

                if not (tryLocalModuleMember ()) then
                    resolveQualifiedExternal ()
        | Expr.LongIdentOrOp(LongIdentOrOp.Op(IdentOrOp.ParenOp(opName = OpName.SymbolicOp op))) when
            (Desugar.symbolicOpCompiledName op.Token |> ValueOption.isSome)
            ->
            // `(+)` and friends used as a value resolve through the provider in
            // Unification (no local binding), so not an unresolved-name error.
            ()
        | Expr.LongIdentOrOp(LongIdentOrOp.QualifiedOp(longIdent = li; op = idOp)) ->
            // `A.B.(+)` — a qualified operator reference. Translate the operator
            // segment to its compiled name (`(+)` → `op_Addition`) and route the
            // resulting `A.B.op_Addition` through the same `tryResolve` machinery a
            // value long-ident uses; the resolved key is stamped for Freeze, exactly
            // as the multi-segment `LongIdent` arm does.
            // The bare-operator form already resolves via the prelude; only the
            // qualified form needs this translation.
            match OperatorNames.qualifiedOpName ctx.NameOf li idOp with
            | ValueSome qualName ->
                match OpenScope.tryResolve ctx.Resolution.OpenScope ctx.Provider.TryLookup qualName with
                | ValueSome sym -> ctx.Resolution.ExternalValue.Set(CstKeys.ofExpr e, sym.Key)
                | ValueNone -> ctx.Error(CstKeys.ofExpr e, sprintf "Unresolved qualified name: %s" qualName)
            | ValueNone ->
                // A non-symbolic op segment (active-pattern / nil / range) has no
                // `op_` member to qualify — keep surfacing the gap.
                ctx.Error(
                    CstKeys.ofExpr e,
                    sprintf
                        "Operator-form qualified names not yet resolved (starting at '%s')"
                        (ctx.NameOf li.Idents.[0])
                )
        | Expr.LongIdentOrOp lio ->
            // TODO: remaining operator-form long idents (a bare non-symbolic
            // `LongIdentOrOp.Op`, e.g. an active-pattern or nil op-name used as a
            // value) need their own resolution story. Surface the gap rather than
            // silently skipping.
            let firstTok = CstKeys.firstTokenOfLongIdentOrOp lio
            let displayName = ctx.NameOf firstTok

            ctx.Error(
                CstKeys.ofExpr e,
                sprintf "Operator-form qualified names not yet resolved (starting at '%s')" displayName
            )
        | Expr.TypeApp(expr = receiver; types = types) ->
            // The receiver's arity (its type-arg count) lives on this node, not on
            // the receiver's own visit. Resolve receiver+arity together so a generic
            // external-type receiver (`EqualityComparer<int>.Default`) resolves at the
            // *exact* arity, mint the use-site key, and stamp `ResolvedType` on the
            // receiver — the receiver's `resolveIdent` / the multi-segment LongIdent
            // arm then suppress the unresolved diagnostic by a key hit instead of the
            // old bounded `[1;2;3;4]` arity scan.
            match typeAppReceiverName ctx receiver with
            | ValueSome name ->
                match tryResolveExternalTypeKey ctx name types.Length with
                | ValueSome key -> ctx.Resolution.ResolvedType.Set(CstKeys.ofExpr receiver, key)
                | ValueNone -> ()
            | ValueNone -> ()
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
