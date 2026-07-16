namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationEngine
open UnificationTranslate

module internal UnificationInferResolve =

    /// v1 only supports single-segment (`X`) and two-segment qualified
    /// (`R.X`) forms. Multi-segment qualifiers (`A.B.X`) fall through as
    /// ValueNone for the qualifier and the last segment for the field name.
    let fieldNameAndQualifier (ctx: PassContext) (li: LongIdent<SyntaxToken>) : string voption * string =
        let idents = li.Idents
        let last = ctx.NameOf idents.[idents.Length - 1]

        if idents.Length = 1 then
            ValueNone, last
        elif idents.Length = 2 then
            ValueSome(ctx.NameOf idents.[0]), last
        else
            ValueNone, last

    let freshNamedInstance
        (ctx: PassContext)
        (typeParams: EqArray<string * TypeVar>)
        : EqArray<SemType> * Dictionary<TypeVar, SemType> =
        let subst = Dictionary<TypeVar, SemType>(HashIdentity.Reference)
        let acc = ResizeArray<SemType>(typeParams.Length)

        for (_, tp) in typeParams do
            let fresh = TypeVar()
            fresh.Level <- ctx.CurrentLevel
            let protoRoot = UnionFind.find tp
            // Copy prototype constraints onto the fresh instance so
            // every use site re-evaluates satisfaction independently
            // (a `Set<int>` and a `Set<int -> int>` each get their own
            // copy of `'a : comparison`).
            fresh.Constraints <- protoRoot.Constraints
            let asTy = TyVar fresh
            subst.[protoRoot] <- asTy
            acc.Add asTy

        EqArray.ofResizeArray acc, subst

    /// Function value whose argument shape matches the primary constructor
    /// and whose result is the constructed `TyClass`. Routes bare
    /// `Point(3, 4)` calls (no `new`) — and `A.Point(3, 4)`, the class named through the
    /// module holding it — through the function-application machinery. `ValueNone` if the
    /// written name isn't a class in scope AT `useSite`: a class declared below the call names
    /// nothing there, so the call resolves to nothing and NameResolution's "unresolved
    /// identifier" (which fires off the same miss) is the whole verdict.
    let tryWrittenClassCtorAsFunction
        (ctx: PassContext)
        (useSite: UseSite)
        (written: WrittenTypeName)
        : SemType voption =
        match TypeRegistry.tryWrittenClass ctx.Types useSite written with
        | ValueSome info ->
            let args, subst = freshNamedInstance ctx info.TypeParams
            let receiverTy = TyClass(info.TypeKey, args)

            let arg =
                info.CtorParams
                |> Array.map (fun p -> substituteWith subst p.Type)
                |> Array.toList
                |> tupleOrSingle ctx

            ValueSome(TyFun(arg, receiverTy))
        | ValueNone -> ValueNone

    /// `tryWrittenClassCtorAsFunction` for a class named bare.
    let tryClassCtorAsFunction (ctx: PassContext) (useSite: UseSite) (name: string) : SemType voption =
        tryWrittenClassCtorAsFunction ctx useSite (WrittenTypeName.bare name)

    let classCtorAsFunction (ctx: PassContext) (useSite: UseSite) (name: string) : SemType =
        match tryClassCtorAsFunction ctx useSite name with
        | ValueSome t -> t
        | ValueNone -> TyVar(freshTyVar ctx)

    /// Function-shaped type for a DU ctor reference. Multi-field cases bundle
    /// the fields into a tuple — F# DUs take a tuple as their single argument.
    /// The receiver union's typars are instantiated fresh so two independent
    /// uses of `Some` don't share a `'a`.
    let ctorType (ctx: PassContext) (info: UnionCaseInfo) : SemType =
        let unionInfo = TypeRegistry.unionOfCase ctx.Types info
        let args, subst = freshNamedInstance ctx unionInfo.TypeParams
        let unionTy = TyUnion(unionInfo.TypeKey, args)

        let walkedFields = info.Fields |> Array.map (substituteWith subst)

        match walkedFields.Length with
        | 0 -> unionTy
        | 1 -> TyFun(walkedFields.[0], unionTy)
        | _ -> TyFun(TyTuple(EqArray.ofArray walkedFields), unionTy)

    /// The union + per-field types of a resolved external case (the payload
    /// NameResolution stamped in `ExternalUnionCaseStamp`), freshly instantiating
    /// the declaring union's typars (one TyVar per declared arity, so two uses of
    /// `Some` don't share a `'a`) — the union to return as the pattern's type, and
    /// the field types to unify the sub-patterns against. TOTAL: the opens / RQA /
    /// qualifier discipline lives upstream in the stamp; a caller that holds the
    /// payload already holds the verdict.
    let externalCasePattern (ctx: PassContext) (uc: ExternalUnionCase) : SemType * SemType[] =
        let freshArgs = Array.init uc.TyparArity (fun _ -> TyVar(freshTyVar ctx))

        let unionTy =
            TyUnion(SymbolKeyOps.externalTypeKeyOf uc.Origin uc.UnionName uc.TyparArity, EqArray.ofArray freshArgs)

        let fields = ExternalSymbols.instantiateCaseFieldTypes uc.Case freshArgs
        unionTy, fields

    /// External (referenced-package) analogue of `ctorType`: build the case
    /// ctor's function/value type `field… → TyUnion(union, freshArgs)` from the
    /// case NameResolution stamped at the expression head `key`.
    let tryExternalCtorType (ctx: PassContext) (key: NodeKey) : SemType voption =
        match ctx.Resolution.ExternalUnionCaseStamp.TryGetValue key with
        | ValueNone -> ValueNone
        | ValueSome uc ->
            let unionTy, fields = externalCasePattern ctx uc

            match fields.Length with
            | 0 -> ValueSome unionTy
            | 1 -> ValueSome(TyFun(fields.[0], unionTy))
            | _ -> ValueSome(TyFun(TyTuple(EqArray.ofArray fields), unionTy))

    /// The union case `name` refers to at `useSite`. ValueNone with `count = 0` means "no
    /// such ctor *here*" — either no union declares it, or the one that does is declared
    /// below the use; `count >= 2` means ambiguous. The caller emits the appropriate
    /// diagnostic.
    let resolveCtorName (ctx: PassContext) (useSite: UseSite) (name: string) : UnionCaseInfo voption * int =
        match TypeRegistry.casesNamed ctx.Types useSite name with
        | [||] -> ValueNone, 0
        | [| only |] -> ValueSome only, 1
        | infos -> ValueNone, infos.Length

    let resolveQualifiedCtor
        (ctx: PassContext)
        (useSite: UseSite)
        (typeName: string)
        (caseName: string)
        : UnionCaseInfo voption =
        // Case names are globally unique (even across arity-overloaded unions like
        // `Choice\`2`…`Choice\`7`), so resolve through the reverse case index and let
        // the written qualifier select which union short name the case belongs to.
        // Avoids a bare `Union.[typeName]` lookup, which can't see an arity-overloaded
        // union (it does not resolve by bare name).
        match
            TypeRegistry.casesNamed ctx.Types useSite caseName
            |> Array.tryFind (fun c -> c.UnionName = typeName)
        with
        | Some c -> ValueSome c
        | None -> ValueNone

    /// The local|external record identity a field set resolves to. `ExternalRecord` is UNUSED
    /// in R4a — the local-only verdict core (`recordFieldSetVerdict`) never mints one; it is
    /// present now so the verdict type is stable across R4a and R4b, where provider candidates
    /// join the core and `ExternalRecord`s first appear.
    type ResolvedRecord =
        | LocalRecord of RecordTypeInfo
        | ExternalRecord of ExternalRecordCandidate

    /// The nominal identity a candidate dedups by — and, for construction, the identity the
    /// `TyRecord` node carries so cross-file it re-homes to the local `TypeDef` and its
    /// field shapes resolve by key. `LocalRecord` carries the `TypeKey` `stampLocalTypeKey`
    /// minted; `ExternalRecord` carries the producer's REAL key verbatim (`c.TypeKey`) —
    /// NOT a key re-minted from the compiled name, which would re-cut a module-held record's
    /// `+`-mangled segment as an `InType` class holder and yield a same-named but UNEQUAL key
    /// that misses both the by-key shape store and codegen's local record re-home.
    let resolvedRecordTypeKey (r: ResolvedRecord) : TypeKey =
        match r with
        | LocalRecord info -> info.TypeKey
        | ExternalRecord c -> c.TypeKey

    /// The candidate's declared field-name set — the axis BOTH the intersection (declared ⊇
    /// typed set) and the exact-match test (declared = typed set) compare over.
    let resolvedRecordFieldNames (r: ResolvedRecord) : Set<string> =
        match r with
        | LocalRecord info -> info.Fields |> Array.map (fun f -> f.Name) |> Set.ofArray
        | ExternalRecord c -> Set.ofArray c.FieldNames

    /// A pure field-set verdict, no diagnostics — the checker wrapper below owns every
    /// `ctx.Error`. `PartialMatches` = every record whose declared field set ⊇ the typed set
    /// (the per-field candidate intersection, deduped by `TypeKey`) — F#'s `BuildFieldMap`
    /// intersection and the future LSP-completion set; it is R4b/Phase-2-facing and the R4a
    /// wrapper does not read it. `ExactMatch` = the UNIQUE `PartialMatch` whose field set EQUALS
    /// the typed set (a superset of equal size ⟺ an equal set, so this subsumes F#'s field-count
    /// tie-break).
    type RecordFieldSetVerdict =
        {
            ExactMatch: ResolvedRecord voption
            PartialMatches: ResolvedRecord list
        }

    /// The verdict for the typed field set `names` at `useSite`, LOCAL candidates only (R4b
    /// unions provider `TryRecordsWithField` candidates in here). A record survives to
    /// `PartialMatches` iff it declares EVERY typed field — i.e. `typed ⊆ declared`, a subset
    /// filter, which is why only the FIRST field's candidates need fetching: any record that
    /// declares all typed fields declares the FIRST one, so it is already in
    /// `recordsWithField … first`. Fetching every field and intersecting (as R4a did) is
    /// therefore redundant — the first-field set is a SUPERSET of the answer and the subset test
    /// in `classifyRecordCandidates` prunes it exactly. `recordsWithField` visibility-scopes that
    /// set to `useSite`, so the verdict stays visibility-correct (a record whose declaring site
    /// sits below the use names nothing). The pure classifier does the dedup-by-key / subset /
    /// exact-match work; here we only wrap `LocalRecord`s, key them, and map the verdict back —
    /// so the combinatorial logic is testable in the open (`RecordFieldClassifier`).
    let recordFieldSetVerdict (ctx: PassContext) (useSite: UseSite) (names: string list) : RecordFieldSetVerdict =
        match names with
        | [] ->
            {
                ExactMatch = ValueNone
                PartialMatches = []
            }
        | first :: _ ->
            let candidates =
                [
                    for info in TypeRegistry.recordsWithField ctx.Types useSite first -> LocalRecord info
                    // Provider candidates join LOCAL-FIRST: on a `TypeKey` collision the
                    // local record wins (local is authoritative for the compiling unit) —
                    // the classifier's first-occurrence dedup and the `byKey` fill below
                    // both keep the earlier (local) entry, so ordering local-first suffices.
                    //
                    // The ambient-scope / `open` filter on provider candidates is DEFERRED:
                    // Phase 1 ships "any provider record with the field" — over-permissive
                    // (a cross-unit record in an unopened namespace is wrongly resolvable
                    // bare), never a miscompile — until the ambient-scope gate lands. See the
                    // plan's "Visibility / open scoping" decision.
                    // The field-name reverse index is a genuine spelling reach with no
                    // stampable node: a bare `{ X = … }` field set has no written record
                    // identity to resolve in NameResolution — the verdict IS the field-set
                    // intersection computed here at inference. So it reads the resolver face
                    // (allowlisted in `ResolverAllowlistTests`), the sibling of the
                    // `TryLookupUnionCase` bare reverse index NameResolution reads.
                    for cand in ctx.Resolver.TryRecordsWithField first -> ExternalRecord cand
                ]

            // Keyed by `TypeKey`, which supports equality but NOT comparison (so a
            // `Dictionary`, not a `Map`), to map the classifier's key verdict back to the
            // `ResolvedRecord`s. Keep the FIRST occurrence per key (guarded add) so a key
            // present both locally and via a provider maps back to the LOCAL record — the
            // same first-occurrence dedup the classifier applies.
            let byKey = Dictionary<TypeKey, ResolvedRecord>()

            for r in candidates do
                let key = resolvedRecordTypeKey r

                if not (byKey.ContainsKey key) then
                    byKey.[key] <- r

            let classification =
                RecordFieldClassifier.classifyRecordCandidates
                    [ for r in candidates -> resolvedRecordTypeKey r, resolvedRecordFieldNames r ]
                    (Set.ofList names)

            {
                ExactMatch = classification.ExactKey |> ValueOption.map (fun key -> byKey.[key])
                PartialMatches = classification.PartialKeys |> List.map (fun key -> byKey.[key])
            }

    /// `Circle(r)` parses as `Circle (EnclosedBlock r)`; `Rectangle(w, h)`
    /// as `Circle (EnclosedBlock (Tuple [w; h]))`. v1 supports the
    /// tuple-argument form and a bare single arg — both are what the F# DU
    /// ctor application convention emits.
    let unwrapCtorArgPattern (p: Pat<SyntaxToken>) : Pat<SyntaxToken> list =
        match p with
        | Pat.EnclosedBlock(pat = Pat.Tuple(patterns = pats)) -> List.ofSeq pats
        | Pat.EnclosedBlock(pat = inner) -> [ inner ]
        | Pat.Tuple(patterns = pats) -> List.ofSeq pats
        | _ -> [ p ]

    /// A 2+-segment qualified reference `Q.member` whose qualifier `Q` (every
    /// segment but the last) names a known external union/record, but whose
    /// `member` resolved to neither a value (module function) nor a case nor a
    /// static member. Those are the only valid forms under such a qualifier and
    /// every one is tried ahead of this probe, so the reference is an unresolved
    /// member — returns `(qualifier, member)` to diagnose. `ValueNone` when the
    /// head isn't such a qualifier (a class qualifier or an unknown one stays a
    /// fresh TyVar: see `tryExternalStaticLongIdent`'s intentional class silence
    /// and the `NameResolution` "Unresolved qualified name" path respectively).
    ///
    /// NameResolution — the resolve-once layer — classified the qualifier
    /// (opens-aware) and stamped its key in `ExternalUnionRecordQualifier`; this
    /// reads that stamp by node key instead of re-resolving the qualifier through
    /// the resolver-face `TryLookupType(string)` at inference time. The qualifier
    /// in the message is the stamp's RESOLVED identity (`Vesper.Option`), not the
    /// re-joined written spelling — the diagnostic names the type the reference
    /// actually hit; the member name is the written last segment.
    let tryQualifiedExternalMemberMiss (ctx: PassContext) (e: Expr<SyntaxToken>) : (string * string) voption =
        match e with
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when li.Idents.Length >= 2 ->
            match ctx.Resolution.ExternalUnionRecordQualifier.TryGetValue(CstKeys.ofExpr e) with
            | ValueSome qualifierKey ->
                let memberName = ctx.NameOf li.Idents.[li.Idents.Length - 1]
                // `bareName` drops the arity suffix a generic union's key carries
                // (`Vesper.Option`1` reads as `Vesper.Option` in a user diagnostic).
                ValueSome(SymbolKeyOps.bareName (SymbolKeyOps.qualifiedName qualifierKey), memberName)
            | ValueNone -> ValueNone
        | _ -> ValueNone

    /// Split a folded static-member LongIdent (`System.Console.Out`) into
    /// `(declTypeKey, lastTok)` where `declTypeKey` is the receiver PREFIX's resolved
    /// class identity and `lastTok` the trailing member segment. NameResolution
    /// resolved the prefix (opens-aware, class-only) and stamped its key in
    /// `ExternalStaticReceiver` under the whole LongIdent node's `key`; this reads it
    /// instead of re-running `OpenScope.tryQualify` at inference time. The DEDICATED
    /// receiver-prefix table (not `ResolvedType`) keeps a static member's receiver
    /// prefix from being mistaken for a constructible whole-name head. `ValueNone`
    /// when the prefix did not resolve to an external class.
    let splitExternalClassPrefix
        (ctx: PassContext)
        (key: NodeKey)
        (li: LongIdent<SyntaxToken>)
        : (SymbolKey * SyntaxToken) voption =
        let lastTok = li.Idents.[li.Idents.Length - 1]

        match ctx.Resolution.ExternalStaticReceiver.TryGetValue key with
        | ValueSome declTypeKey -> ValueSome(declTypeKey, lastTok)
        | ValueNone -> ValueNone

    /// Static member access on an external type, recording the resolved member's
    /// interned `SymbolKey` so Elaborate stamps a `TExpr.ExternalMember`.
    /// `typeArgs` instantiate the declaring type's
    /// typars, so `EqualityComparer<int>.Default` types as `EqualityComparer<int>`.
    /// `declTypeKey` is the declaring type's identity NameResolution resolved
    /// (opens-aware) and stamped in `ResolvedType`; member selection is the post-dot
    /// (non-opens-sensitive) step, a key-addressed `TryLookupMember` here.
    let inferExternalStaticMember
        (ctx: PassContext)
        (key: NodeKey)
        (declTypeKey: SymbolKey)
        (typeArgs: SemType list)
        (memberTok: SyntaxToken)
        : SemType =
        let memberName = ctx.NameOf memberTok

        match ctx.Provider.TryLookupMember(declTypeKey, memberName) with
        | ValueSome m ->
            let memberSig = ExternalSymbols.openSignature m (List.toArray typeArgs)

            ctx.Resolution.ExternalAccess.Set(
                key,
                {
                    Key = SymbolKey.Member m.Key
                    IsStatic = m.IsStatic
                    Storage = m.Storage
                    Signature = memberSig
                    OptionalDefaults = m.OptionalDefaults
                }
            )

            memberSig
        | ValueNone ->
            errorTy
                ctx
                key
                (sprintf "Type '%s' has no accessible member '%s'" (SymbolKeyOps.qualifiedName declTypeKey) memberName)

    /// If `recv` is an *external generic type name* used as a static-access
    /// receiver (`EqualityComparer<int>` in `EqualityComparer<int>.Default`),
    /// return the declaring type's stamped `SymbolKey` and the raw CST type args
    /// (translation is deferred to the caller). NameResolution's `Expr.TypeApp`
    /// visit already resolves the receiver at its exact arity (opens-aware) and,
    /// for a genuine CLASS, stamps the key in `ExternalStaticReceiver`, keyed by
    /// the receiver *head* expr — class-guaranteed at the writer, so this reads
    /// the stamp with no shape re-query. A generic union/record/abbrev receiver
    /// carries no entry and keeps its own path. v1 handles the `TypeApp` form
    /// only; non-generic external static access (`System.Console.Out`) folds into
    /// a single LongIdent (see `tryExternalStaticLongIdent`).
    let tryExternalTypeReceiver
        (ctx: PassContext)
        (recv: Expr<SyntaxToken>)
        : (SymbolKey * Type<SyntaxToken> list) voption =
        // The receiver head as written: a single-segment name parses as `Expr.Ident`
        // (`EqualityComparer<int>`), a dotted one as a `LongIdent`
        // (`System.Collections.Generic.EqualityComparer<int>`).
        match recv with
        | Expr.TypeApp(expr = head; types = typeArgs) ->
            match head with
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent _)
            | Expr.Ident _ ->
                match ctx.Resolution.ExternalStaticReceiver.TryGetValue(CstKeys.ofExpr head) with
                | ValueSome declTypeKey -> ValueSome(declTypeKey, List.ofSeq typeArgs)
                | ValueNone -> ValueNone
            | _ -> ValueNone
        | _ -> ValueNone

    /// `System.Console.Out` / `Console.Out` (under `open System`): the non-generic
    /// analogue of the `EqualityComparer<int>.Default` DotLookup arm. There the
    /// `<int>` keeps the type receiver a separate `Expr.TypeApp`, but a non-generic
    /// type folds into a single LongIdent (the parser merges consecutive `.ident`),
    /// so the prefix/member split is recovered here. Always static — an instance
    /// receiver is either a local binding (the field-chain arm) or a `DotLookup`.
    /// A resolved prefix whose last segment is *not* an accessible static member
    /// (e.g. a const field, not modelled yet) falls through silently rather than
    /// diagnosing — it's valid F#, just unsupported (static fields are a later
    /// phase).
    let tryExternalStaticLongIdent (ctx: PassContext) (key: NodeKey) (e: Expr<SyntaxToken>) : SemType voption =
        match e with
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when li.Idents.Length >= 2 ->
            match splitExternalClassPrefix ctx key li with
            | ValueSome(declTypeKey, lastTok) ->
                // Claim it only if the member actually resolves; otherwise leave
                // the node to the ctor/TyVar fallback without a spurious error.
                match ctx.Provider.TryLookupMember(declTypeKey, ctx.NameOf lastTok) with
                | ValueSome _ -> ValueSome(inferExternalStaticMember ctx key declTypeKey [] lastTok)
                | ValueNone -> ValueNone
            | ValueNone -> ValueNone
        | _ -> ValueNone

    /// Resolve a folded-LongIdent external *static* member reference
    /// (`System.String.Concat`) to its declaring type's stamped `SymbolKey` + member
    /// token. The head being a local binding — a `r.X.Y` field chain — is excluded.
    let tryResolveExternalStaticMemberRef
        (ctx: PassContext)
        (e: Expr<SyntaxToken>)
        : (SymbolKey * SyntaxToken) voption =
        match e with
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
            li.Idents.Length >= 2
            && not (ctx.Bindings.Binding.ContainsKey(NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent))
            ->
            match splitExternalClassPrefix ctx (CstKeys.ofExpr e) li with
            | ValueSome(declTypeKey, lastTok) when
                (ctx.Provider.TryLookupMembers(declTypeKey, ctx.NameOf lastTok)).Length > 0
                ->
                ValueSome(declTypeKey, lastTok)
            | _ -> ValueNone
        | _ -> ValueNone
