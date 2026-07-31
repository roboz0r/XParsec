namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Immutable
open XParsec.FSharp
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open NameResolutionTypeHeadStamp

// Scope tracking and ident-use resolution for NameResolution — the resolve-once
// layer's value/expression half. Every spelling→identity result is STAMPED into
// the `ctx.Resolution` side tables, keyed by the use-site `NodeKey`, and
// downstream passes read the stamp by key:
//
// resolveIdent writes ctx.Bindings.Binding for every use site resolving to a
// local; names the provider knows are stamped (`ExternalValue` +
// `ExternalSymbolStamp`, so Unification instantiates the scheme by key rather
// than re-resolving); external union/enum cases, ctor-sugar heads, static
// receivers, and union/record qualifiers get their dedicated stamps in the
// `visit` arms below; the rest become Error diagnostics. Operators in
// InfixApp/PrefixApp are resolved here too (`stampDesugaredOperator`, off the
// compiled name Desugar recorded) — Unification reads the stamped symbol when
// typing the application.
//
// IsMutable mirrors the binding's mutableToken, propagated to every use-site
// so Validation's assignment check can read ctx.Bindings.Binding[lhsKey]
// directly. Each binding site also gets a self-entry (BindingSite = key) so
// Validation's value-restriction loop can iterate mutable bindings.

module NameResolutionScope =

    type Scope = Map<string, NodeKey * bool>

    /// F# keeps NO global reverse index for union cases: a *bare* (unqualified) case
    /// resolves only when its declaring union's module/namespace is opened or
    /// auto-opened (F#'s `AddPartsOfTyconRefToNameEnv` adds cases to the per-scope
    /// unqualified/pattern tables solely on `open`). Mirror that — a bare case is
    /// visible only when the current `OpenScope` qualifies the union's short name
    /// back to its own fully-qualified name; i.e. the declaring namespace is one of
    /// the active `open` / ambient-prelude prefixes (or the union lives in the
    /// root/global namespace, whose bare candidate always matches). RQA is a
    /// separate axis, handled by `ExternalUnionCase.ResolvesWith`.
    let private bareCaseNamespaceOpen (scope: OpenScope) (uc: ExternalUnionCase) : bool =
        // Derive the declaring namespace from `UnionName` (authoritative — the
        // reverse-index case stamps the blanket PACKAGE origin, whose namespace can
        // differ from a sub-namespace union's own). `bareName` keeps the namespace
        // and strips the `` `N `` arity suffix so `Vesper.Choice`2`'s qualified name
        // is `Vesper.Choice`, matching `short` under the `Vesper` prefix.
        let qualified = SymbolKeyOps.bareName uc.UnionName
        let short = SymbolKeyOps.shortName uc.UnionName
        (OpenScope.tryQualify scope (fun c -> c = qualified) short).IsSome

    /// The external union case a reference resolves to, applying the RQA + qualifier
    /// discipline (`ExternalUnionCase.ResolvesWith`) the downstream consumers use.
    /// `qualifier` is the written declaring type (`Option.Some` ⇒ `ValueSome "Option"`),
    /// `ValueNone` for a bare reference: a bare hit on an `[<RequireQualifiedAccess>]`
    /// union's case is rejected (F# requires `Color.Red`, not `Red`), a qualified one is
    /// accepted only when the qualifier is the union's short name. A bare hit is
    /// ADDITIONALLY gated on the declaring namespace being open (`bareCaseNamespaceOpen`):
    /// F# has no global reverse case index, so `Some`/`Ok`/`Red` resolve unqualified only
    /// once their union's namespace is opened/auto-opened. A qualified reference is NOT so
    /// gated (F# resolves `Union.Case` without the namespace opened). NameResolution — the
    /// one resolve-once layer — recognises the case HERE and stamps the resolved
    /// identity; Unification and Elaborate read the stamp rather than re-recognising from a
    /// spelling.
    let private tryExternalCase
        (ctx: PassContext)
        (qualifier: string voption)
        (caseName: string)
        : ExternalUnionCase voption =
        ctx.Resolver.TryLookupUnionCase caseName
        |> ValueOption.filter (fun uc -> uc.ResolvesWith qualifier)
        |> ValueOption.filter (fun uc ->
            match qualifier with
            | ValueSome _ -> true
            | ValueNone -> bareCaseNamespaceOpen ctx.Resolution.OpenScope uc
        )

    /// Resolve an external VALUE reference and stamp both channels — the `SymbolKey`
    /// (`ExternalValue`, which Freeze reads to key the `TExpr.External`) and the whole
    /// symbol (`ExternalSymbolStamp`, which Unification reads to instantiate the scheme
    /// by key). Both or neither: a value ref carrying only the symbol freezes to a
    /// keyless `External`, which `InlineExpansion` cannot reach — it addresses contract
    /// bodies by key — so the value silently loses its inline body. `false` on a miss;
    /// each caller decides whether that is an error or a deferral to a later pass.
    let private tryStampExternalValue (ctx: PassContext) (key: NodeKey) (name: string) : bool =
        match OpenScope.tryResolve ctx.Resolution.OpenScope ctx.Resolver.TryLookup name with
        | ValueSome sym ->
            ctx.Resolution.ExternalValue.Set(key, SymbolKey.Binding sym.Key)
            ctx.Resolution.ExternalSymbolStamp.Set(key, sym)
            true
        | ValueNone -> false

    /// True if the *bare* (unqualified) `name` resolves to an external union case
    /// whose declaring union is NOT `[<RequireQualifiedAccess>]`. An RQA union's
    /// cases are reachable only through the qualified form (`Color.Red`), so a bare
    /// hit on one is rejected here, matching F#. The
    /// qualified paths (`isExternalQualifiedCase`, `tryExternalCtorType` with a
    /// qualifier) resolve RQA cases unchanged — this guard is bare-name only.
    let private resolvesAsBareExternalCase (ctx: PassContext) (name: string) : bool =
        (tryExternalCase ctx ValueNone name).IsSome

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
            if not (tryStampExternalValue ctx useKey name) then
                // A bare external union case (`None` / `Some`) used in expression
                // position: stamp the resolved identity so Unification's
                // `tryExternalCtorType` and Elaborate's `tryCtorRef` read it by key
                // instead of re-recognising `name` through the provider.
                let bareCase = tryExternalCase ctx ValueNone name

                match bareCase with
                | ValueSome uc -> ctx.Resolution.ExternalUnionCaseStamp.Set(useKey, uc)
                | ValueNone -> ()

                // Classify the written name against the external universe ONCE, the
                // same commitment `resolveQualifiedExternal` makes for a dotted name:
                // the first hit in candidate order IS what the name names, and every
                // verdict below (the ctor stamp, the diagnostic suppression) derives
                // from that one hit. Two differently-filtered scans could each keep
                // probing past a hit they don't like and settle on DIFFERENT entities
                // for one spelling — which is the disagreement the resolve-once layer
                // exists to make impossible.
                let bareHit = tryClassifyExternalType ctx (arityProbes 0) name

                // A single-ident external CLASS name in expression position — a
                // ctor-sugar head (`InvalidOperationException "x"`). Stamp its key so
                // Unification's `tryInferExternalCtorApp` constructs it by key rather
                // than re-running the spelling lookup at inference time. Class-only: an
                // intrinsic scalar used as a conversion function (`float x`) names a
                // real external type but is not constructible.
                if not (ctx.Resolution.ResolvedType.ContainsKey useKey) then
                    match bareHit with
                    | ValueSome hit ->
                        match hit.Shape with
                        | ExternalTypeShape.Class info when info.TyparArity = 0 ->
                            ctx.Resolution.ResolvedType.Set(
                                useKey,
                                SymbolKeyOps.externalTypeKeyOf info.Origin hit.Compiled 0
                            )
                        | _ -> ()
                    | ValueNone -> ()
                // DU ctors resolve via ctx.Types.CtorIndex in Unification; class
                // names used as ctor-functions live in ctx.Types.Class; external
                // type names used as static-access receivers resolve via the
                // provider in Unification; an external union *case* (`Some` /
                // `None`) resolves via the provider's reverse case index in
                // Unification/Elaborate, so it is
                // suppressed here the same way (it is ambient, like the `option`
                // abbreviation, rather than open-gated in v1). Suppress the
                // unresolved diagnostic for all four.
                // Both local reads answer AS SEEN FROM this use (`useKey`): a class or a
                // union declared BELOW it is not in scope, so it neither suppresses the
                // diagnostic nor binds. One verdict, not two — the suppression fires
                // *because* the lookup hit, so when the lookup misses here, it misses in
                // Unification too and the diagnostic is the resolution failure itself.
                if
                    TypeRegistry.isCaseName ctx.Types (ctx.UseSiteAt useKey) name
                    || (TypeRegistry.tryClass ctx.Types (ctx.UseSiteAt useKey) name).IsSome
                    // A generic external-type receiver (`EqualityComparer<int>`) was
                    // resolved at exact arity by the enclosing TypeApp visit, which
                    // stamped this use site's `ResolvedType`; a non-generic one
                    // (`Math.Pi`'s `Math`) is the arity-0 hit classified above.
                    || ctx.Resolution.ResolvedType.ContainsKey useKey
                    || (
                        match bareHit with
                        | ValueSome hit -> hit.Shape.TyparArity = 0
                        | ValueNone -> false
                    )
                    // A bare external union case resolves only when its union is NOT
                    // `[<RequireQualifiedAccess>]` — F# rejects the short `Red` form
                    // for an RQA `Color`. Stamped just above so downstream reads it.
                    || bareCase.IsSome
                    // The printf family (`printf`/`printfn`/`sprintf`/`eprintf`/
                    // `fprintf`/…) is a front-end intrinsic: `InferApp` types it via
                    // `PrintfSpec`, not a provider symbol. So a family member resolves
                    // even when the contract doesn't declare it (the writer families
                    // are not in the `Vesper.Printf` contract); don't flag it unbound.
                    || (PrintfSpec.tryFamily name |> ValueOption.isSome)
                then
                    ()
                else
                    ctx.Report(tok, Kind.Message(sprintf "Unresolved identifier: %s" name))

    /// True if `name` is a ctor reference in pattern position. F# spec treats
    /// uppercase-leading pattern idents as ctor references; we additionally
    /// require a registry hit so unrelated uppercase binders still bind. A local
    /// union case lives in `ctx.Types.CtorIndex`; an *external* (referenced-
    /// package) one resolves through the provider's reverse case index, so a
    /// cross-package `Some x` pattern treats `Some` as a ctor head (binding
    /// nothing) and its sub-patterns as binders, not the whole thing as a binder
    /// Empty strings (virtual tokens) never
    /// match.
    let private isCtorName (ctx: PassContext) (useSite: UseSite) (name: string) : bool =
        name.Length > 0
        && System.Char.IsUpper name.[0]
        && (TypeRegistry.isCaseName ctx.Types useSite name
            // A bare RQA external case is not a ctor head in pattern position either
            // — only its qualified form is.
            || resolvesAsBareExternalCase ctx name)

    /// True if the `Pat.Named` head `li` is a ctor reference (the head binds
    /// nothing; its sub-patterns are the binders). Covers a bare local/external
    /// case (`isCtorName` on the last segment) AND the two-segment qualified-
    /// EXTERNAL case (`Color.Red`, `Result.Ok`) — the leg `isCtorName` alone
    /// misses, since a bare probe of an RQA case's short name is (correctly)
    /// rejected. Mirrors the recognition InferPat / Elaborate/Patterns apply, so a
    /// qualified external case's sub-patterns bind identically.
    let private isPatNamedCtorHead (ctx: PassContext) (useSite: UseSite) (li: LongIdent<SyntaxToken>) : bool =
        li.Idents.Length >= 1
        && (isCtorName ctx useSite (ctx.NameOf li.Idents.[li.Idents.Length - 1])
            || (li.Idents.Length = 2
                && (tryExternalCase ctx (ValueSome(ctx.NameOf li.Idents.[0])) (ctx.NameOf li.Idents.[1])).IsSome))

    /// Every (name, NodeKey) pair introduced by a pattern; [] for patterns that
    /// bind nothing (Wildcard, Const, nullary ctors).
    let rec bindingsOfPat (ctx: PassContext) (p: Pat<SyntaxToken>) : (string * NodeKey) list =
        match p with
        | Pat.NamedSimple t when isCtorName ctx (ctx.UseSiteAt(CstKeys.ofPat p)) (ctx.NameOf t) ->
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
        | Pat.Named(longIdent = li; argumentPats = args) when isPatNamedCtorHead ctx (ctx.UseSiteAt(CstKeys.ofPat p)) li ->
            // Ctor pattern (`Circle r`, `Result1.Ok x`, `Color.Red x`): head binds
            // nothing, sub-patterns introduce binders.
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
        // Explicitly binder-less, so a NEW binder-carrying `Pat` case fails the
        // incomplete-match check here instead of silently binding nothing:
        // a non-ctor-head `Pat.Named` / operator head with args (`Pat.OpNamed`) is
        // a function-definition head — its name is bound by the binding machinery,
        // its args by `EnterBindingRhs`; or/and alternatives (their shared binders
        // are not collected into scope today — F# requires both sides bind the
        // same names, unmodelled yet); struct tuples, named union-field pats,
        // `:? T` tests, and token leaves.
        | Pat.Named _
        | Pat.OpNamed _
        | Pat.Or _
        | Pat.And _
        | Pat.StructTuple _
        | Pat.NamedFieldPats _
        | Pat.Optional _
        | Pat.TypeTest _
        | Pat.Null _
        | Pat.String _
        | Pat.Expr _
        | Pat.Missing
        | Pat.SkipsTokens _ -> []

    /// Stamp every external union-case ctor head reachable in `p` with the resolved
    /// `ExternalUnionCase`, keyed by the head pattern's `CstKeys.ofPat` — the key
    /// Unification's `InferPat` and Elaborate's `translatePat` read. The traversal is
    /// `CstWalk.iterPat` (exhaustive, so a new `Pat` case is loud there), which
    /// reaches EVERY sub-pattern, including the positions `bindingsOfPat` skips
    /// because they bind nothing (or-pattern alternatives `Some 1 | Some 2`, cons
    /// tails, type-tests): recognition is done ONCE here (the resolve-once layer),
    /// so a consumer that reads the stamp instead of re-recognising would mis-lower
    /// any position left unstamped. A two-segment head applies the written qualifier
    /// (`Result.Ok`, `Color.Red`), a single-segment head the bare form — mirroring
    /// the 1-/2-ident external cases the consumers recognise (a 3+-segment head is
    /// not a consumer-recognised ctor ref, so it is not stamped). Pattern type
    /// annotations (`(x: T)`, `:? T`, `:? T as x`) carry a written type head —
    /// stamped here too: `stampPatCases` runs at every pattern-scope site (lambda
    /// args, match arms, for-in, let head/args, member args), so this is the single
    /// point that covers every pattern-embedded annotation for the resolve-once
    /// boundary.
    ///
    /// `typeIter` is the visitor those annotations are walked with. It is a parameter
    /// because a pattern in a type definition's DECLARED SURFACE (a ctor parameter, a
    /// member's argument annotation) is walked with the classifying visitor that also
    /// DIAGNOSES a head naming nothing, whereas a pattern in a body is walked with the
    /// plain stamping one — same enumeration of positions, one extra verdict.
    let stampPatCasesWith (ctx: PassContext) (typeIter: CstWalk.TypeIter) (p: Pat<SyntaxToken>) : unit =
        let visit (pat: Pat<SyntaxToken>) : unit =
            match pat with
            | Pat.NamedSimple t ->
                match tryExternalCase ctx ValueNone (ctx.NameOf t) with
                | ValueSome uc -> ctx.Resolution.ExternalUnionCaseStamp.Set(CstKeys.ofPat pat, uc)
                | ValueNone -> ()
            | Pat.Named(longIdent = li) ->
                if li.Idents.Length = 1 || li.Idents.Length = 2 then
                    let caseName = ctx.NameOf li.Idents.[li.Idents.Length - 1]

                    let qualifier =
                        if li.Idents.Length = 2 then
                            ValueSome(ctx.NameOf li.Idents.[0])
                        else
                            ValueNone

                    match tryExternalCase ctx qualifier caseName with
                    | ValueSome uc -> ctx.Resolution.ExternalUnionCaseStamp.Set(CstKeys.ofPat pat, uc)
                    | ValueNone -> ()

                // `| E.C1` — a two-segment external enum-case pattern (a named constant,
                // binds nothing). Stamp the enum's nominal key for `InferPat`'s enum arm,
                // mirroring the expression-position stamp; an enum name is never a union,
                // so this and the union-case stamp above are mutually exclusive.
                if li.Idents.Length = 2 then
                    match tryExternalEnumCaseKey ctx (ctx.NameOf li.Idents.[0]) (ctx.NameOf li.Idents.[1]) with
                    | ValueSome k -> ctx.Resolution.ExternalEnumCaseStamp.Set(CstKeys.ofPat pat, k)
                    | ValueNone -> ()
            | Pat.Typed(typ = t)
            | Pat.TypeTestAs(typ = t)
            | Pat.TypeTest(typ = t) -> CstWalk.iterType typeIter t
            | _ -> ()

        CstWalk.iterPat
            {
                VisitPat =
                    fun _ pat ->
                        visit pat
                        true
            }
            p

    /// `stampPatCasesWith` under the plain stamping visitor — the body / value-position
    /// form, where an unresolved head is not an error.
    let stampPatCases (ctx: PassContext) (p: Pat<SyntaxToken>) : unit =
        stampPatCasesWith ctx (stampTypeIter ctx) p

    /// Lambda args / for-in / match-arm patterns can't carry `mutable`, so every
    /// binder they introduce is immutable.
    let extendScope (ctx: PassContext) (pats: ImmutableArray<Pat<SyntaxToken>>) (acc: Scope) : Scope =
        let mutable s = acc

        for p in pats do
            stampPatCases ctx p

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
            stampPatCases ctx b.headPat

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

    /// Resolve an operator/value spelling through the opens-aware resolver view and,
    /// on a hit, stamp the full `ExternalSymbol` so Unification instantiates its
    /// scheme by key (`ExternalSymbolStamp`) rather than re-resolving from the
    /// spelling. A miss leaves the node unstamped — the same signal the old
    /// inference-time `tryResolve` miss produced.
    let private stampExternalSymbol (ctx: PassContext) (key: NodeKey) (name: string) : unit =
        match OpenScope.tryResolve ctx.Resolution.OpenScope ctx.Resolver.TryLookup name with
        | ValueSome sym -> ctx.Resolution.ExternalSymbolStamp.Set(key, sym)
        | ValueNone -> ()

    /// A desugared `InfixApp` / `PrefixApp` head: recover the compiled operator name
    /// Desugar recorded (`ctx.Desugared`) and stamp its resolved symbol. A `ConsExpr`
    /// (`::`, built directly, not a provider operator) and `op_AddressOf` (the byref
    /// intrinsic, no provider symbol) resolve to no stamp — the former because it is
    /// not an `OpName`, the latter because the provider surfaces no symbol — matching
    /// the arms Unification handles without the provider.
    let private stampDesugaredOperator (ctx: PassContext) (e: Expr<SyntaxToken>) : unit =
        match ctx.Desugared.TryGetValue(CstKeys.ofExpr e) with
        | ValueSome(DesugaredForm.OpName name) -> stampExternalSymbol ctx (CstKeys.ofExpr e) name
        | _ -> ()

    let private visit (ctx: PassContext) (scope: Scope list) (e: Expr<SyntaxToken>) : unit =
        // Type-annotation boundary: stamp every external type head embedded in this
        // node (`ResolvedTypeHead`) so `Translate.tryResolveExternalType` reads the
        // stamp rather than re-resolving the spelling. Runs for every visited node;
        // recursion into child expressions is the walker's, so each embedded type is
        // stamped once. Additive to — and independent of — the expression-position
        // `ResolvedType` stamping the arms below still perform.
        stampExprEmbeddedTypes ctx e

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
                // `Module.member` where `Module` is a *local* (in-file) module
                // and `member` one of its `let`-bound values/functions. The module
                // tree is flattened before this walk, so the sibling is unresolvable
                // by the provider (which only knows dependency packages) and by the
                // local head lookup (a module name is not a value binding). Resolve
                // it against the pre-pass `LocalModules` registry and record a
                // use-site `Binding` entry pointing at the member's binding site —
                // exactly the shape a single-ident local resolves to, so Unification
                // (`inferIdentDefault` → `instantiateBinding`) and Elaborate
                // (`translateIdent` → `TExpr.Var`) treat it as an ordinary local
                // reference. The module name is the second-to-last segment (handles
                // the 2-segment `SetTree.add`).
                //
                // Answered AS SEEN FROM this expression: `LocalModules` is a whole-file
                // registry, so a member whose binding sits below the use answers for
                // nothing here (F# FS0039) — unless the binding is inside a `rec` scope
                // that also contains the use, in which case its `VisibleFrom` is that
                // scope's keyword and it resolves.
                let tryLocalModuleMember () : bool =
                    if li.Idents.Length >= 2 then
                        let moduleName = ctx.NameOf li.Idents.[li.Idents.Length - 2]
                        let memberName = ctx.NameOf li.Idents.[li.Idents.Length - 1]

                        match ctx.Resolution.LocalModules.TryGetValue moduleName with
                        | true, members ->
                            match members.TryGetValue memberName with
                            | true, m when m.VisibleFrom <= (ctx.UseSiteAt(CstKeys.ofExpr e)).Offset ->
                                ctx.Bindings.Binding.Set(
                                    CstKeys.ofExpr e,
                                    {
                                        BindingSite = m.BindingSite
                                        IsInline = false
                                        IsMutable = false
                                    }
                                )

                                true
                            | _ -> false
                        | false, _ -> false
                    else
                        false

                // Below the local-module case: the qualified name names something outside
                // the local-module registry — an external value, a local qualified
                // ctor/static, an external union case/static, or genuinely unresolved.
                let resolveQualifiedExternal () =
                    let qualName = li.Idents |> Seq.map ctx.NameOf |> String.concat "."
                    // Every local read below answers AS SEEN FROM this expression: a type
                    // declared under it cannot answer for the qualifier, so the suppression
                    // and the binding Unification makes fail together.
                    let useSite = ctx.UseSiteAt(CstKeys.ofExpr e)

                    if not (tryStampExternalValue ctx (CstKeys.ofExpr e) qualName) then
                        // `Result2.Ok` — two-segment qualified ctor; resolves through
                        // ctx.Types.Union, suppress so Unification picks it up.
                        let isQualifiedCtor =
                            li.Idents.Length = 2
                            && TypeRegistry.localQualifiedCase
                                ctx.Types
                                useSite
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

                                (match TypeRegistry.tryClass ctx.Types useSite typeName with
                                 | ValueSome info -> staticIn info.Members
                                 | ValueNone -> false)
                                || (
                                    match TypeRegistry.tryUnionBare ctx.Types useSite typeName with
                                    | ValueSome info -> staticIn info.Members
                                    | ValueNone -> false
                                ))

                        // `A.T` — the whole name is a project-local TYPE named through the
                        // module holding it, so the reference is a ctor / static head, not a
                        // value. Unification resolves it through the type registry
                        // (`tryWrittenClassCtorAsFunction`), so suppress here exactly as the
                        // bare form is suppressed by never reaching this path at all.
                        let isLocalQualifiedType =
                            TypeRegistry.isWrittenTypeNameInScope ctx.Types useSite (ctx.WrittenTypeNameOf li)

                        // `E.C1` — a two-segment enum-case access. The head names a
                        // project-local enum, so suppress (Unification's `InferIdentExpr`
                        // enum arm resolves a valid case to `TyEnum` and emits the precise
                        // "Enum 'E' has no case 'C'" for a bad tail — one diagnostic, not
                        // a redundant "unresolved qualified name" on top). Mirrors the
                        // `isQualifiedCtor` / `isQualifiedStatic` suppressions.
                        let isEnumCase =
                            li.Idents.Length = 2
                            && (TypeRegistry.tryEnum ctx.Types useSite (ctx.NameOf li.Idents.[0])).IsSome

                        // `Result.Ok` / `Option.Some` — a qualified *external* union case.
                        // The declaring union may be generic (`Result\`2`), but it is
                        // written without type args, so there is no `Expr.TypeApp` to
                        // recover the arity from. The case name is globally unique in the
                        // provider's reverse index, so resolve it arity-free instead of
                        // probing the union type at a guessed arity. Mirrors the
                        // single-ident `TryLookupUnionCase` suppression and the local
                        // `isQualifiedCtor` arm; Unification / Elaborate resolve the case.
                        let isExternalQualifiedCase =
                            li.Idents.Length >= 2
                            && (ctx.Resolver.TryLookupUnionCase(ctx.NameOf li.Idents.[li.Idents.Length - 1])).IsSome

                        // Stamp the resolved case identity for the expression-position
                        // consumers (`tryExternalCtorType`, `tryCtorRef`), which recognise
                        // only the two-segment qualified form (`Option.Some`) and require
                        // the qualifier to match the union's short name — tighter than the
                        // `.IsSome` diagnostic suppression above, so a `WrongType.Some`
                        // suppresses the error yet stamps nothing (downstream then falls to
                        // its default path, as before).
                        if li.Idents.Length = 2 then
                            match
                                tryExternalCase ctx (ValueSome(ctx.NameOf li.Idents.[0])) (ctx.NameOf li.Idents.[1])
                            with
                            | ValueSome uc -> ctx.Resolution.ExternalUnionCaseStamp.Set(CstKeys.ofExpr e, uc)
                            | ValueNone -> ()

                        // Classify the whole name and its qualifier prefix each exactly
                        // ONCE against the external type universe; every stamp and every
                        // suppression verdict below derives from these two committed
                        // hits. The whole name is written without type args (arity-0
                        // probe); the prefix's arity is unrecoverable, so it takes the
                        // bounded qualifier scan.
                        let qualHit = tryClassifyExternalType ctx (arityProbes 0) qualName

                        let prefix =
                            seq { for i in 0 .. li.Idents.Length - 2 -> ctx.NameOf li.Idents.[i] }
                            |> String.concat "."

                        let prefixHit = tryClassifyExternalType ctx qualifierProbes prefix

                        // `E.C1` — a two-segment external enum-case access whose enum
                        // declares the case. Stamp the enum's nominal key so Unification's
                        // `InferIdentExpr` enum arm types the node `TyEnum key` by node-key
                        // read, not by re-recognising the spelling. (An enum prefix also
                        // suppresses the unresolved-name diagnostic below — it is an
                        // external type at the bare probe.)
                        if li.Idents.Length = 2 then
                            match prefixHit with
                            | ValueSome {
                                            Compiled = compiled
                                            Shape = ExternalTypeShape.Enum(cases, origin)
                                        } when
                                (let caseName = ctx.NameOf li.Idents.[1]
                                 cases |> Array.exists (fun c -> c.Name = caseName))
                                ->
                                ctx.Resolution.ExternalEnumCaseStamp.Set(
                                    CstKeys.ofExpr e,
                                    SymbolKeyOps.externalTypeKeyOf origin compiled 0
                                )
                            | _ -> ()

                        // Stamp the resolved external CLASS key so the key-addressed
                        // consumers read identity by key instead of re-resolving through
                        // opens at inference/freeze time. Two DISTINCT meanings for this
                        // same node, into two tables (a ctor-app consumer must never
                        // mistake a static-member's receiver prefix for a constructible
                        // head): the whole name as an external class (a ctor-sugar head
                        // `System.InvalidOperationException "x"`, or a bare class ref) →
                        // `ResolvedType`; else the folded static-member receiver PREFIX
                        // (`System.Console` in `System.Console.Out`, `N` in `N.pickName`)
                        // → `ExternalStaticReceiver`. Class-only, at arity 0 (a *generic*
                        // receiver requires explicit type args — a TypeApp, whose visit
                        // stamps `ResolvedType` at exact arity, hence the guard on an
                        // existing entry); additive to the suppression verdicts below.
                        if not (ctx.Resolution.ResolvedType.ContainsKey(CstKeys.ofExpr e)) then
                            match qualHit with
                            | ValueSome {
                                            Compiled = compiled
                                            Shape = ExternalTypeShape.Class info
                                        } when info.TyparArity = 0 ->
                                ctx.Resolution.ResolvedType.Set(
                                    CstKeys.ofExpr e,
                                    SymbolKeyOps.externalTypeKeyOf info.Origin compiled 0
                                )
                            | _ ->
                                match prefixHit with
                                | ValueSome {
                                                Compiled = compiled
                                                ProbedTyparArity = 0
                                                Shape = ExternalTypeShape.Class info
                                            } when info.TyparArity = 0 ->
                                    ctx.Resolution.ExternalStaticReceiver.Set(
                                        CstKeys.ofExpr e,
                                        SymbolKeyOps.externalTypeKey info.Origin compiled 0
                                    )
                                | _ -> ()

                        // A qualifier naming an external UNION or RECORD has no static
                        // fields, so an unresolved tail is a genuine member miss — stamp
                        // the qualifier's key so Unification's
                        // `tryQualifiedExternalMemberMiss` diagnoses it by node-key read.
                        // Present-but-unread when the tail resolves (a valid case / value /
                        // static never reaches the miss path). A class qualifier is NOT
                        // stamped: its unmodelled-static silence stays a fresh TyVar.
                        match prefixHit with
                        | ValueSome {
                                        Compiled = compiled
                                        ProbedTyparArity = a
                                        Shape = ExternalTypeShape.Union(origin = origin)
                                    }
                        | ValueSome {
                                        Compiled = compiled
                                        ProbedTyparArity = a
                                        Shape = ExternalTypeShape.Record(origin = origin)
                                    } ->
                            ctx.Resolution.ExternalUnionRecordQualifier.Set(
                                CstKeys.ofExpr e,
                                SymbolKeyOps.externalTypeKey origin compiled a
                            )
                        | _ -> ()

                        // The whole name / the prefix resolves as an external type at
                        // arity 0 (`Shape.TyparArity` must agree with the bare probe: a
                        // bare-keyed generic union hit is NOT an arity-0 type). A
                        // whole-name hit is a bare type ref; a prefix hit is a folded
                        // static-member access (`System.Console.Out`), whose member is
                        // left to Unification's tryExternalStaticLongIdent (which falls
                        // through silently when the tail isn't accessible, so suppression
                        // here doesn't manufacture a member that isn't there).
                        let qualIsExternalType =
                            match qualHit with
                            | ValueSome hit -> hit.Shape.TyparArity = 0
                            | ValueNone -> false

                        let isExternalStaticMember =
                            match prefixHit with
                            | ValueSome hit -> hit.ProbedTyparArity = 0 && hit.Shape.TyparArity = 0
                            | ValueNone -> false

                        if
                            isQualifiedCtor
                            || isQualifiedStatic
                            || isLocalQualifiedType
                            || isEnumCase
                            || isExternalQualifiedCase
                            // A generic external-type receiver written qualified
                            // (`System.Collections.Generic.List<int>.Empty`) was resolved
                            // at exact arity by the enclosing TypeApp visit, stamping this
                            // LongIdent's `ResolvedType`.
                            || ctx.Resolution.ResolvedType.ContainsKey(CstKeys.ofExpr e)
                            || qualIsExternalType
                            || isExternalStaticMember
                        then
                            ()
                        else
                            ctx.Report(CstKeys.firstTokenOfExpr e, Kind.UnresolvedQualifiedName qualName)

                if not (tryLocalModuleMember ()) then
                    resolveQualifiedExternal ()
        | Expr.LongIdentOrOp(LongIdentOrOp.Op(IdentOrOp.ParenOp(opName = OpName.SymbolicOp op))) when
            (Desugar.symbolicOpCompiledName op.Token |> ValueOption.isSome)
            ->
            // `(+)` and friends used as a value: an operator VALUE is a resolved
            // external value ref like any other, so it stamps through the same channel
            // pair. A miss is not an unresolved-name error at this layer (Unification's
            // operator-value arm reports it), so no diagnostic — just no stamp.
            match Desugar.symbolicOpCompiledName op.Token with
            | ValueSome name -> tryStampExternalValue ctx (CstKeys.ofExpr e) name |> ignore
            | ValueNone -> ()
        | Expr.LongIdentOrOp(LongIdentOrOp.QualifiedOp(longIdent = li; op = idOp)) ->
            // `A.B.(+)` — a qualified operator reference. Translate the operator
            // segment to its compiled name (`(+)` → `op_Addition`) and route the
            // resulting `A.B.op_Addition` through the same `tryResolve` machinery a
            // value long-ident uses; the resolved key is stamped for Elaborate, exactly
            // as the multi-segment `LongIdent` arm does.
            // The bare-operator form already resolves via the prelude; only the
            // qualified form needs this translation.
            match OperatorNames.qualifiedOpName ctx.NameOf li idOp with
            | ValueSome qualName ->
                if not (tryStampExternalValue ctx (CstKeys.ofExpr e) qualName) then
                    ctx.Report(CstKeys.firstTokenOfExpr e, Kind.UnresolvedQualifiedName qualName)
            | ValueNone ->
                // A non-symbolic op segment (active-pattern / nil / range) has no
                // `op_` member to qualify — keep surfacing the gap.
                ctx.Report(CstKeys.firstTokenOfExpr e, Kind.OperatorFormQualifiedName(ctx.NameOf li.Idents.[0]))
        | Expr.LongIdentOrOp lio ->
            // TODO: remaining operator-form long idents (a bare non-symbolic
            // `LongIdentOrOp.Op`, e.g. an active-pattern or nil op-name used as a
            // value) need their own resolution story. Surface the gap rather than
            // silently skipping.
            let firstTok = CstKeys.firstTokenOfLongIdentOrOp lio
            let displayName = ctx.NameOf firstTok

            ctx.Report(CstKeys.firstTokenOfExpr e, Kind.OperatorFormQualifiedName displayName)
        | Expr.TypeApp(expr = receiver; types = types) ->
            // The receiver's arity (its type-arg count) lives on this node, not on
            // the receiver's own visit. Classify receiver+arity together — ONCE — so
            // a generic external-type receiver (`EqualityComparer<int>.Default`)
            // resolves at the *exact* arity. The use-site key stamps `ResolvedType`
            // (any shape: it drives unresolved-name suppression, the generic ctor
            // path — where an abbreviation like `ResizeArray` stamps its OWN key and
            // expands on read — and Elaborate's class-ref verdict); a genuine CLASS
            // receiver additionally stamps the class-guaranteed
            // `ExternalStaticReceiver`, so the static-member reader
            // (`tryExternalTypeReceiver`) dispatches without a shape re-query at
            // inference time.
            match typeAppReceiverName ctx receiver with
            | ValueSome name ->
                match tryClassifyExternalType ctx (arityProbes types.Length) name with
                | ValueSome hit when hit.Shape.TyparArity = types.Length ->
                    let key = useSiteTypeKey hit
                    ctx.Resolution.ResolvedType.Set(CstKeys.ofExpr receiver, key)

                    match hit.Shape with
                    | ExternalTypeShape.Class _ ->
                        ctx.Resolution.ExternalStaticReceiver.Set(CstKeys.ofExpr receiver, SymbolKey.Type key)
                    | _ -> ()
                | _ -> ()
            | ValueNone -> ()
        // Operator/intrinsic symbol resolution, moved upstream from `InferApp`:
        // stamp the resolved operator `ExternalSymbol` so Unification instantiates
        // its scheme (and threads `sym.Key` into `IntrinsicKey`) by reading the
        // stamp, rather than re-running `OpenScope.tryResolve … TryLookup` at
        // inference time. Keyed by the operator node itself (the same
        // `CstKeys.ofExpr` key `inferInfix`/`inferPrefix`/`inferDynamic*` read).
        | Expr.InfixApp _
        | Expr.PrefixApp _ -> stampDesugaredOperator ctx e
        // `recv?name` — resolve `op_Dynamic` at the `DynamicLookup` node. The
        // dynamic-SET form (`recv?name <- v`) parses as `Assignment(DynamicLookup,
        // v)`; its inner `DynamicLookup` is also visited and stamps `op_Dynamic`,
        // but `inferDynamicSet` reads the `op_DynamicAssignment` stamp on the
        // enclosing `Assignment` node (below), so the inner stamp is inert.
        | Expr.DynamicLookup _ -> stampExternalSymbol ctx (CstKeys.ofExpr e) OperatorData.OpDynamic
        // `recv?name <- value` — the dynamic setter; stamp `op_DynamicAssignment`
        // on the enclosing `Assignment` (the key `inferDynamicSet` reads).
        | Expr.Assignment(leftExpr = Expr.DynamicLookup _) ->
            stampExternalSymbol ctx (CstKeys.ofExpr e) OperatorData.OpDynamicAssignment
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
                    stampPatCases ctx pat

                    let scopeMap =
                        bindingsOfPat ctx pat |> List.map (fun (n, k) -> n, (k, false)) |> Map.ofList

                    scopeMap :: scope
            EnterMatchArm =
                fun scope pat ->
                    stampPatCases ctx pat

                    let scopeMap =
                        bindingsOfPat ctx pat |> List.map (fun (n, k) -> n, (k, false)) |> Map.ofList

                    scopeMap :: scope
        }
