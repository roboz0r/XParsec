namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Immutable
open XParsec.FSharp
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open NameResolutionTypeRefStamp

// Scope tracking and ident-use resolution for NameResolution. Every
// spelling→identity result is stamped into a `ctx.Resolution` side table keyed by
// the use-site `NodeKey`; later passes read the stamp, never the spelling again.

module NameResolutionScope =

    type Scope = Map<string, NodeKey * bool>

    /// F# keeps no global reverse index for union cases: a bare `Some`/`Red` resolves
    /// only when its declaring union's namespace is opened or auto-opened. Mirror that,
    /// so a union in the root namespace always matches, its bare candidate being itself.
    let private bareCaseNamespaceOpen (scope: OpenScope) (uc: ExternalUnionCase) : bool =
        // `UnionName`, not the case's own origin: the reverse index stamps the blanket
        // PACKAGE origin, whose namespace can differ. `bareName` strips the arity
        // suffix, so ``Vesper.Choice`2`` qualifies as `Vesper.Choice`.
        let qualified = SymbolKeyOps.bareName uc.UnionName
        let short = SymbolKeyOps.shortName uc.UnionName
        (OpenScope.tryQualify scope (fun c -> c = qualified) short).IsSome

    /// The external union case a reference resolves to. `qualifier` is the written
    /// declaring type (`Option.Some` ⇒ `ValueSome "Option"`), `ValueNone` for a bare
    /// reference, which alone is gated on the declaring namespace being open.
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

    /// Resolve an external VALUE reference and stamp both channels: the `SymbolKey` and
    /// the whole symbol. Both or neither: with only the symbol the ref freezes to a
    /// KEYLESS `External`, and inline bodies are spliced by key, so the body is lost.
    let private tryStampExternalValue (ctx: PassContext) (key: NodeKey) (name: string) : bool =
        match OpenScope.tryResolve ctx.Resolution.OpenScope ctx.Resolver.TryLookup name with
        | ValueSome sym ->
            ctx.Resolution.ExternalValue.Set(key, SymbolKey.Binding sym.Key)
            ctx.Resolution.ExternalSymbolStamp.Set(key, sym)
            true
        | ValueNone -> false

    /// True if `name` resolves to an external union case WITHOUT a qualifier.
    let private resolvesAsBareExternalCase (ctx: PassContext) (name: string) : bool =
        (tryExternalCase ctx ValueNone name).IsSome

    /// The written type name an `Expr.TypeApp`'s applied expression spells, when it could
    /// name a type; `ValueNone` for shapes that never can (e.g. an applied expression).
    let private typeAppTypeName (ctx: PassContext) (expr: Expr<SyntaxToken>) : WrittenTypeName voption =
        match expr with
        | Expr.Ident tok -> ValueSome(WrittenTypeName.bare (ctx.NameOf tok))
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) -> ValueSome(ctx.WrittenTypeNameOf li)
        | _ -> ValueNone

    let private resolveIdent (ctx: PassContext) (scope: Scope list) (tok: SyntaxToken) (useKey: NodeKey) =
        let name = ctx.NameOf tok

        let rec lookup (s: Scope list) =
            match s with
            | [] -> ValueNone
            | innermost :: enclosing ->
                match Map.tryFind name innermost with
                | Some bs -> ValueSome bs
                | None -> lookup enclosing

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
                let bareCase = tryExternalCase ctx ValueNone name

                match bareCase with
                | ValueSome uc -> ctx.Resolution.ExternalUnionCaseStamp.Set(useKey, uc)
                | ValueNone -> ()

                // The first hit in candidate order IS what the name names.
                let bareHit = tryClassifyExternalType ctx (arityProbes 0) name

                // A single-ident external CLASS in expression position is a ctor-sugar
                // application (`InvalidOperationException "x"`). Class-only: `float x`
                // names a real external type but is not constructible.
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
                // Each disjunct suppresses the unresolved diagnostic for a name a later
                // pass does resolve. The local reads answer AS SEEN FROM `useKey`: a
                // class or union declared below the use neither suppresses nor binds.
                if
                    TypeRegistry.isCaseName ctx.Types (ctx.UseSiteAt useKey) name
                    || (TypeRegistry.tryClass ctx.Types (ctx.UseSiteAt useKey) name).IsSome
                    // A generic prefix (`EqualityComparer<int>`) had `ResolvedType`
                    // stamped at exact arity by the enclosing TypeApp visit.
                    || ctx.Resolution.ResolvedType.ContainsKey useKey
                    || (
                        match bareHit with
                        | ValueSome hit -> hit.Shape.TyparArity = 0
                        | ValueNone -> false
                    )
                    // Stamped just above, so downstream reads the identity by key.
                    || bareCase.IsSome
                    // The printf family is a front-end intrinsic, typed from its format
                    // string rather than a provider symbol, so no contract declares it.
                    || (PrintfSpec.tryFamily name |> ValueOption.isSome)
                then
                    ()
                else
                    ctx.Report(tok, Kind.Message(sprintf "Unresolved identifier: %s" name))

    /// True if `name` is a ctor reference in pattern position: uppercase-leading (per
    /// the F# spec) AND a case-registry hit, so an unrelated uppercase bound variable still
    /// binds. Empty strings (virtual tokens) never match.
    let private isCtorName (ctx: PassContext) (useSite: UseSite) (name: string) : bool =
        name.Length > 0
        && System.Char.IsUpper name.[0]
        && (TypeRegistry.isCaseName ctx.Types useSite name
            || resolvesAsBareExternalCase ctx name)

    /// Every (name, NodeKey) pair introduced by a pattern; [] for patterns that
    /// bind nothing (Wildcard, Const, nullary ctors).
    let rec bindingsOfPat (ctx: PassContext) (p: Pat<SyntaxToken>) : (string * NodeKey) list =
        match p with
        | Pat.NamedSimple t when isCtorName ctx (ctx.UseSiteAt(CstKeys.ofPat p)) (ctx.NameOf t) ->
            // A known nullary ctor pattern; binds nothing.
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
            // `:? T as x` — the inner pattern is the bound variable; the test type is not.
            bindingsOfPat ctx inner
        | Pat.Record(fieldPats = fieldPats) ->
            [ for FieldPat(pat = sub) in fieldPats -> bindingsOfPat ctx sub ] |> List.concat
        | Pat.Named(argumentPats = args) ->
            // `Circle r`, `Color.Red`: the head is a discriminator, never a bound variable, because
            // a bound variable is a lone ident, which parses as `Pat.NamedSimple`. So the sub-patterns
            // bind whether or not the head resolves; Unification reports one that does not resolve.
            [
                for sub in args do
                    yield! bindingsOfPat ctx sub
            ]
        | Pat.Cons(head = h; tail = t) ->
            // `h :: t`: the `::` ctor binds nothing; both sub-patterns introduce bound variables.
            bindingsOfPat ctx h @ bindingsOfPat ctx t
        // `|` spells the same names on both sides, `&` binds both. Lowering rejects an
        // or-pattern that binds, so nothing has to choose between the two occurrences.
        | Pat.Or(left = l; right = r)
        | Pat.And(left = l; right = r) -> bindingsOfPat ctx l @ bindingsOfPat ctx r
        | Pat.Elems(pats = pats) ->
            // `[a; b; c]` list-literal pattern (the multi-element form, wrapped in
            // `EnclosedBlock(List, …)`): each element introduces bound variables.
            [ for sub in pats -> bindingsOfPat ctx sub ] |> List.concat
        | Pat.Op io ->
            // Operator-named binding (`let (=) x y = …`) binds the compiled name
            // `op_Equality`; use sites resolve through the desugared form instead.
            match OperatorNames.ofPatOp ctx.NameOf io with
            | ValueSome n -> [ n, CstKeys.ofPat p ]
            | ValueNone -> []
        | Pat.OpNamed _
        | Pat.StructTuple _
        | Pat.NamedFieldPats _
        | Pat.Optional _
        | Pat.TypeTest _
        | Pat.Null _
        | Pat.String _
        | Pat.Expr _
        | Pat.Missing
        | Pat.SkipsTokens _ -> []

    /// Stamp every external union-case ctor in `p` (1- and 2-segment names only), including
    /// the alternatives and sub-patterns that introduce no bound variable. Embedded type
    /// names go through `typeIter`, which may also diagnose an unknown one.
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
                // binds nothing). An enum name is never a union.
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

    /// `stampPatCasesWith` under the plain stamping visitor: the body / value-position
    /// form, where an unresolved name is not an error.
    let stampPatCases (ctx: PassContext) (p: Pat<SyntaxToken>) : unit =
        stampPatCasesWith ctx (stampTypeIter ctx) p

    /// Lambda args / for-in / match-arm patterns can't carry `mutable`, so every
    /// bound variable they introduce is immutable.
    let extendScope (ctx: PassContext) (pats: ImmutableArray<Pat<SyntaxToken>>) (acc: Scope) : Scope =
        let mutable s = acc

        for p in pats do
            stampPatCases ctx p

            for n, k in bindingsOfPat ctx p do
                s <- Map.add n (k, false) s

        s

    /// Build the scope additions for a let-group, writing a self-entry
    /// (`BindingSite = key`) for every bound variable.
    let bindingsToScope (ctx: PassContext) (bindings: ImmutableArray<Binding<SyntaxToken>>) : Scope =
        let mutable s = Map.empty

        for b in bindings do
            let isMut = b.mutableToken.IsSome
            stampPatCases ctx b.pattern

            for n, k in bindingsOfPat ctx b.pattern do
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

    /// Resolve an operator/value spelling through the opens-aware resolver view and, on
    /// a hit, stamp the full symbol so its scheme is instantiated by key later.
    let private stampExternalSymbol (ctx: PassContext) (key: NodeKey) (name: string) : unit =
        match OpenScope.tryResolve ctx.Resolution.OpenScope ctx.Resolver.TryLookup name with
        | ValueSome sym -> ctx.Resolution.ExternalSymbolStamp.Set(key, sym)
        | ValueNone -> ()

    /// A desugared `InfixApp` / `PrefixApp` operator: stamp the symbol for the compiled
    /// operator name the desugaring recorded. `::` is not an `OpName` and `op_AddressOf`
    /// has no provider symbol, so both take the no-stamp arm.
    let private stampDesugaredOperator (ctx: PassContext) (e: Expr<SyntaxToken>) : unit =
        match ctx.Desugared.TryGetValue(CstKeys.ofExpr e) with
        | ValueSome(DesugaredForm.OpName name) -> stampExternalSymbol ctx (CstKeys.ofExpr e) name
        | _ -> ()

    let private visit (ctx: PassContext) (scope: Scope list) (e: Expr<SyntaxToken>) : unit =
        // Stamp every external type name embedded in this node's annotations. Recursion
        // into child expressions is the walker's, so each name is stamped once.
        stampExprEmbeddedTypes ctx e

        match e with
        | Expr.Ident tok -> resolveIdent ctx scope tok (CstKeys.ofExpr e)
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
            resolveIdent ctx scope li.Idents.[0] (CstKeys.ofExpr e)
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) ->
            // Multi-segment: a chained field access (`r.X.Y`, anchor local) or a qualified
            // name (`Math.PI`, provider). Field resolution waits for the anchor's type.
            let anchorIdent = li.Idents.[0]
            let anchorName = ctx.NameOf anchorIdent

            let rec lookup (s: Scope list) =
                match s with
                | [] -> ValueNone
                | innermost :: enclosing ->
                    match Map.tryFind anchorName innermost with
                    | Some bs -> ValueSome bs
                    | None -> lookup enclosing

            match lookup scope with
            | ValueSome(bindingSite, isMutable) ->
                // Key the anchor's binding entry under ExprIdent on the anchor token
                // so later passes look up the anchor's type by the same key.
                ctx.Bindings.Binding.Set(
                    NodeKey.ofToken anchorIdent NodeKind.ExprIdent,
                    {
                        BindingSite = bindingSite
                        IsInline = false
                        IsMutable = isMutable
                    }
                )
            | ValueNone ->
                // `Module.member` on a *local* (in-file) module: the module tree is
                // flattened before this walk, so neither the provider nor the anchor lookup
                // can see it. Resolving here gives it the shape a plain local ident takes.
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

                let resolveQualifiedExternal () =
                    let qualName = li.Idents |> Seq.map ctx.NameOf |> String.concat "."
                    // Every local read below answers AS SEEN FROM this expression: a type
                    // declared under it cannot answer for the qualifier.
                    let useSite = ctx.UseSiteAt(CstKeys.ofExpr e)

                    if not (tryStampExternalValue ctx (CstKeys.ofExpr e) qualName) then
                        // `Result2.Ok` — a two-segment qualified LOCAL ctor; suppress, a
                        // later pass resolves it through the type registry.
                        let isQualifiedCtor =
                            li.Idents.Length = 2
                            && TypeRegistry.localQualifiedCase
                                ctx.Types
                                useSite
                                (ctx.NameOf li.Idents.[0])
                                (ctx.NameOf li.Idents.[1])

                        // `Math.Pi` / `Box.Empty` — two-segment qualified static member,
                        // incl. union augmentation statics. Same suppression.
                        let isQualifiedStatic =
                            li.Idents.Length = 2
                            && (let typeName = ctx.NameOf li.Idents.[0]
                                let memberName = ctx.NameOf li.Idents.[1]

                                let declares name =
                                    (TypeRegistry.tryStaticMember ctx.Types useSite typeName name).IsSome

                                // A WRITE-ONLY property is declared under `set_P` alone, so the
                                // name still resolves here; Unification says it cannot be read.
                                declares memberName || declares (AccessorNames.setterName memberName))

                        // `A.T` — a project-local TYPE named through its module, so the
                        // reference is a ctor / static qualifier, not a value. Suppress.
                        let isLocalQualifiedType =
                            TypeRegistry.isWrittenTypeNameInScope ctx.Types useSite (ctx.WrittenTypeNameOf li)

                        // `E.C1` — the anchor names a project-local enum. Suppress, so a bad
                        // last segment gets the precise "Enum 'E' has no case 'C'" instead of a
                        // redundant unresolved-qualified-name on top of it.
                        let isEnumCase =
                            li.Idents.Length = 2
                            && (TypeRegistry.tryEnum ctx.Types useSite (ctx.NameOf li.Idents.[0])).IsSome

                        // `Result.Ok` — a qualified *external* union case, written without
                        // type args, so a generic union offers no arity to probe. The case
                        // name is unique in the reverse index: resolve it arity-free.
                        let isExternalQualifiedCase =
                            li.Idents.Length >= 2
                            && (ctx.Resolver.TryLookupUnionCase(ctx.NameOf li.Idents.[li.Idents.Length - 1])).IsSome

                        // Stamp the resolved case identity. Tighter than the `.IsSome`
                        // suppression above because the qualifier must match the union's short
                        // name, so `WrongType.Some` suppresses the error yet stamps nothing.
                        if li.Idents.Length = 2 then
                            match
                                tryExternalCase ctx (ValueSome(ctx.NameOf li.Idents.[0])) (ctx.NameOf li.Idents.[1])
                            with
                            | ValueSome uc -> ctx.Resolution.ExternalUnionCaseStamp.Set(CstKeys.ofExpr e, uc)
                            | ValueNone -> ()

                        // Classify the whole name and the qualifier prefix. The whole name is
                        // written without type args (arity-0); the prefix's arity is not.
                        let qualHit = tryClassifyExternalType ctx (arityProbes 0) qualName

                        let prefix =
                            seq { for i in 0 .. li.Idents.Length - 2 -> ctx.NameOf li.Idents.[i] }
                            |> String.concat "."

                        let prefixHit = tryClassifyExternalType ctx qualifierProbes prefix

                        // `E.C1` — an external enum whose prefix declares the case. Stamp the
                        // enum's nominal key, so the node types as `TyEnum key` by key read.
                        if li.Idents.Length = 2 then
                            match prefixHit with
                            | ValueSome {
                                            Compiled = compiled
                                            Shape = ExternalTypeShape.Enum(cases, origin)
                                        } when
                                (let caseName = ctx.NameOf li.Idents.[1]
                                 cases |> EqArray.exists (fun c -> c.Name = caseName))
                                ->
                                ctx.Resolution.ExternalEnumCaseStamp.Set(
                                    CstKeys.ofExpr e,
                                    SymbolKeyOps.externalTypeKeyOf origin compiled 0
                                )
                            | _ -> ()

                        // The whole name as an external class (`System.Exception "x"`, a
                        // ctor-sugar application) → `ResolvedType`; else the folded prefix
                        // (`System.Console` in `System.Console.Out`, class or intrinsic).
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
                                    ctx.Resolution.ExternalStaticQualifier.Set(
                                        CstKeys.ofExpr e,
                                        SymbolKeyOps.externalTypeKey info.Origin compiled 0
                                    )
                                | ValueSome {
                                                ProbedTyparArity = 0
                                                Shape = ExternalTypeShape.Intrinsic { Id = { Canon = canon } }
                                            } when canon.TyparArity = 0 ->
                                    ctx.Resolution.ExternalStaticQualifier.Set(CstKeys.ofExpr e, SymbolKey.Type canon)
                                | _ -> ()

                        // An external UNION or RECORD qualifier has no static fields, so an
                        // unresolved last segment is a genuine member miss, so stamp its key to be
                        // diagnosed. A class qualifier is not: it stays a fresh TyVar.
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

                        // A whole-name hit is a bare type ref; a prefix hit a folded static
                        // access (`System.Console.Out`). `Shape.TyparArity` must agree with
                        // the probe: a bare-keyed generic union is NOT an arity-0 type.
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
                            // A generic prefix (`…List<int>.Empty`) was stamped by TypeApp.
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
            // `(+)` used as a value is an ordinary external value ref, so it stamps
            // through the same channel pair. A miss is not diagnosed here.
            match Desugar.symbolicOpCompiledName op.Token with
            | ValueSome name -> tryStampExternalValue ctx (CstKeys.ofExpr e) name |> ignore
            | ValueNone -> ()
        | Expr.LongIdentOrOp(LongIdentOrOp.QualifiedOp(longIdent = li; op = idOp)) ->
            // `A.B.(+)` — translate the operator segment to its compiled name
            // (`op_Addition`) and route `A.B.op_Addition` through the value long-ident
            // machinery. Only the qualified form needs this; bare ops come from the prelude.
            match OperatorNames.qualifiedOpName ctx.NameOf li idOp with
            | ValueSome qualName ->
                if not (tryStampExternalValue ctx (CstKeys.ofExpr e) qualName) then
                    ctx.Report(CstKeys.firstTokenOfExpr e, Kind.UnresolvedQualifiedName qualName)
            | ValueNone ->
                // A non-symbolic op segment (active-pattern / nil / range) has no
                // `op_` member to qualify, so keep surfacing the gap.
                ctx.Report(CstKeys.firstTokenOfExpr e, Kind.OperatorFormQualifiedName(ctx.NameOf li.Idents.[0]))
        | Expr.LongIdentOrOp lio ->
            // TODO: a bare non-symbolic `LongIdentOrOp.Op` (an active-pattern or nil
            // op-name used as a value) needs its own resolution story. Surface the gap
            // rather than silently skipping.
            let firstTok = CstKeys.firstTokenOfLongIdentOrOp lio
            let displayName = ctx.NameOf firstTok

            ctx.Report(CstKeys.firstTokenOfExpr e, Kind.OperatorFormQualifiedName displayName)
        | Expr.TypeApp(expr = expr; types = types) ->
            // The type-arg count lives on THIS node, so the applied name and its arity are
            // classified together, so `EqualityComparer<int>.Default` resolves at the exact
            // arity. A local claim wins: `T<'a>(…)` in `T`'s own file means `T`'s decl.
            match typeAppTypeName ctx expr with
            | ValueSome written when
                not (TypeRegistry.isWrittenTypeNameInScope ctx.Types (ctx.UseSiteAt(CstKeys.ofExpr expr)) written)
                ->
                match tryClassifyExternalType ctx (arityProbes types.Length) written.Written with
                | ValueSome hit when hit.Shape.TyparArity = types.Length ->
                    let key = useSiteTypeKey hit
                    ctx.Resolution.ResolvedType.Set(CstKeys.ofExpr expr, key)

                    match hit.Shape with
                    | ExternalTypeShape.Class _ ->
                        ctx.Resolution.ExternalStaticQualifier.Set(CstKeys.ofExpr expr, SymbolKey.Type key)
                    | _ -> ()
                | _ -> ()
            | _ -> ()
        | Expr.InfixApp _
        | Expr.PrefixApp _ -> stampDesugaredOperator ctx e
        // `x?name` — stamp `op_Dynamic`. The SET form (`x?name <- v`) parses as
        // `Assignment(DynamicLookup, v)`, whose inner `DynamicLookup` is visited and
        // stamped too, but the setter reads the enclosing node, so that stamp is inert.
        | Expr.DynamicLookup _ -> stampExternalSymbol ctx (CstKeys.ofExpr e) OperatorData.OpDynamic
        // `x?name <- value` — stamp `op_DynamicAssignment` on the enclosing
        // `Assignment`, the node the setter is typed at.
        | Expr.Assignment(leftExpr = Expr.DynamicLookup _) ->
            stampExternalSymbol ctx (CstKeys.ofExpr e) OperatorData.OpDynamicAssignment
        | _ -> ()

    let mkWalker (ctx: PassContext) : CstWalk.ExprWalker<Scope list> =
        {
            Visit = visit ctx
            EnterFun = fun scope argPats -> extendScope ctx argPats Map.empty :: scope
            EnterBindingRhs =
                fun scope isRec siblings b ->
                    // `let rec`: sibling names, including this binding's own, are in
                    // scope for the RHS. Function-form pushes parameter names on top.
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
