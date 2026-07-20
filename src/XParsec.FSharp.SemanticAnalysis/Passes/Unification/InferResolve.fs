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
        (typeParams: EqArray<string * TyVarId>)
        : EqArray<SemType> * Dictionary<TyVarId, SemType> =
        let subst = Dictionary<TyVarId, SemType>()
        let acc = ResizeArray<SemType>(typeParams.Length)

        for (_, tp) in typeParams do
            let fresh = ctx.NewTypeVar()
            let freshRoot = UnionFind.find ctx.Store fresh
            ctx.Store.SetLevel(freshRoot, ctx.CurrentLevel)
            let protoRoot = UnionFind.find ctx.Store tp
            // Copy prototype constraints onto the fresh instance so
            // every use site re-evaluates satisfaction independently
            // (a `Set<int>` and a `Set<int -> int>` each get their own
            // copy of `'a : comparison`).
            ctx.Store.Constraints.Set(freshRoot, ctx.Store.Constraints.Items protoRoot)
            let asTy = TyVar fresh
            subst.[protoRoot.Id] <- asTy
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
                |> Array.map (fun p -> substituteWith ctx.Store subst p.Type)
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

        let walkedFields = info.Fields |> Array.map (substituteWith ctx.Store subst)

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

    /// The local|external record identity a field set resolves to. `recordFieldSetVerdict`
    /// mints a `LocalRecord` per project-local candidate and an `ExternalRecord` per provider
    /// candidate; both flow through the shared resolver / construction shape below.
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
    /// intersection and the LSP-completion set. `ExactMatch` = the UNIQUE `PartialMatch` whose
    /// field set EQUALS the typed set (a superset of equal size ⟺ an equal set, so this subsumes
    /// F#'s field-count tie-break); `ExactCount` is how many matched exactly, so the wrapper
    /// splits "no match" (`0`) from "ambiguous" (`>1`) without re-deriving it from `PartialMatches`.
    type RecordFieldSetVerdict =
        {
            ExactMatch: ResolvedRecord voption
            ExactCount: int
            PartialMatches: ResolvedRecord list
        }

    /// Whether a provider (cross-unit) record candidate belongs to the UNQUALIFIED
    /// field-set index a bare `{ X = … }` literal reads — F#'s `eFieldLabels`. Two
    /// exclusions, BOTH bare-only (the qualified `{ R.X = … }` path resolves `R` by
    /// name and is unaffected by either — F# indexes qualified construction through
    /// the module path, not `eFieldLabels`):
    ///   * `[<RequireQualifiedAccess>]` — F#'s `isILOrRequiredQualifiedAccess` guard
    ///     keeps an RQA record out of the unqualified index (the qualifier is mandatory).
    ///   * open scope — `eFieldLabels` holds only records brought into scope, so the
    ///     candidate's declaring module / namespace must be reachable UNQUALIFIED at the
    ///     use site: an active `open`, the ambient prelude, or the enclosing namespace's
    ///     implicit open — exactly the reach `OpenScope.tryQualify` answers for a written
    ///     name (the record's simple name must qualify, under the opens in force, to the
    ///     candidate's own dotted spelling). A type NESTED in a type is never
    ///     bare-reachable cross-unit. `ctx.Resolution.OpenScope` is the live per-element
    ///     scope (`Unification.walkElems` sets it via `EnterElement`, in lockstep with the
    ///     walk), so it names the opens in force at this literal.
    let private admitsBareExternalRecord (ctx: PassContext) (cand: ExternalRecordCandidate) : bool =
        if cand.IsRequireQualifiedAccess then
            false
        else
            let key = cand.TypeKey

            let holder =
                match key.Holder with
                | TypeHolder.InNamespace ns -> ValueSome ns.Dotted
                | TypeHolder.InModule m -> ValueSome(SymbolKeyOps.moduleFullName m)
                | TypeHolder.InType _ -> ValueNone

            match holder with
            | ValueNone -> false
            | ValueSome h ->
                let (DisplayName simple) = SymbolKeyOps.typeSimpleName key
                let dotted = if h = "" then simple else h + "." + simple

                OpenScope.tryQualify ctx.Resolution.OpenScope (fun c -> c = dotted) simple
                |> ValueOption.isSome

    /// The verdict for the typed field set `names` at `useSite`, unioning LOCAL candidates with
    /// provider `TryRecordsWithField` ones. A record survives to `PartialMatches` iff it declares
    /// EVERY typed field — i.e. `typed ⊆ declared`, a subset filter, which is why only the FIRST
    /// field's candidates need fetching: any record that declares all typed fields declares the
    /// FIRST one, so it is already in `recordsWithField … first`. Fetching every field and
    /// intersecting would be redundant — the first-field set is a SUPERSET of the answer and the
    /// subset test in `classifyRecordCandidates` prunes it exactly. `recordsWithField`
    /// visibility-scopes the local set to `useSite`, so the verdict stays visibility-correct (a
    /// record whose declaring site sits below the use names nothing). The pure classifier does the
    /// dedup-by-key / subset / exact-match work over the `ResolvedRecord`s directly — so the
    /// combinatorial logic is testable in the open (`RecordFieldClassifier`) and no key→candidate
    /// map-back is needed here.
    ///
    /// `bareIndex` marks the UNQUALIFIED (`{ X = … }`) caller: only then are provider
    /// candidates gated by `admitsBareExternalRecord` (RQA + open scope), mirroring F#'s
    /// `eFieldLabels`. The qualified (`{ R.X = … }`) caller passes `false` and sees every
    /// provider record with the field, since it resolves `R` by name downstream.
    let recordFieldSetVerdict
        (ctx: PassContext)
        (useSite: UseSite)
        (bareIndex: bool)
        (names: string list)
        : RecordFieldSetVerdict =
        match names with
        | [] ->
            {
                ExactMatch = ValueNone
                ExactCount = 0
                PartialMatches = []
            }
        | first :: _ ->
            // Provider candidates join LOCAL-FIRST: on a `TypeKey` collision the local
            // record wins (local is authoritative for the compiling unit) — the
            // classifier's first-occurrence dedup keeps the earlier (local) entry, so
            // ordering local-first suffices. The field-name reverse index is a genuine
            // spelling reach with no stampable node: a bare `{ X = … }` field set has no
            // written record identity to resolve in NameResolution — the verdict IS the
            // field-set intersection computed here at inference. So it reads the resolver
            // face (allowlisted in `ResolverAllowlistTests`), the sibling of the
            // `TryLookupUnionCase` bare reverse index NameResolution reads. On the bare
            // path those provider records are scope/RQA-gated (`admitsBareExternalRecord`).
            let providerRecords =
                ctx.Resolver.TryRecordsWithField first
                |> Array.filter (fun cand -> not bareIndex || admitsBareExternalRecord ctx cand)

            let candidates =
                [
                    for info in TypeRegistry.recordsWithField ctx.Types useSite first -> LocalRecord info
                    for cand in providerRecords -> ExternalRecord cand
                ]

            // The classifier dedups by `TypeKey` (first-wins) and returns the surviving
            // `ResolvedRecord`s directly, so the local-first ordering above is what pins a
            // key present both locally and via a provider to its LOCAL record.
            let classification =
                RecordFieldClassifier.classifyRecordCandidates
                    resolvedRecordTypeKey
                    resolvedRecordFieldNames
                    candidates
                    (Set.ofList names)

            {
                ExactMatch = classification.Exact
                ExactCount = classification.ExactCount
                PartialMatches = classification.Partial
            }

    /// The record's own simple (segment) name — `info.Name` locally, the innermost type
    /// segment for an external record. It drives BOTH the "Type '%s' has no field '%s'"
    /// diagnostic and the qualified-literal qualifier match (`R` in `{ R.X = … }`). Taken
    /// off the real `TypeKey` via `typeSimpleName`, NOT `shortName` of the compiled meta
    /// name: a module-held record's metadata name is `+`-mangled (`Test.A.M+R`) and
    /// `shortName` strips only the arity suffix, so it would yield `M+R` and never match
    /// the written `R`.
    let resolvedRecordDisplayName (r: ResolvedRecord) : string =
        match r with
        | LocalRecord info -> info.Name
        | ExternalRecord candidate ->
            let (DisplayName shown) = SymbolKeyOps.typeSimpleName candidate.TypeKey
            shown

    /// The record a literal / pattern resolves to, OR `ValueNone` with the appropriate
    /// diagnostic emitted — the ONE resolver both `inferRecord` (expr) and the record arm
    /// of `inferPat` route through, so the local|external and qualified|bare branching is
    /// not duplicated across the two call sites. All diagnostics live here; the message
    /// text is IDENTICAL to the historical local-only path so local behaviour stays
    /// byte-identical.
    ///
    /// CONSTRUCTION resolves ONLY on `ExactMatch` — a superset-only or ambiguous field set
    /// is an ERROR, exactly as the old exact-set-equality rejected supersets. Vesper has no
    /// missing-field check, so resolving a superset would silently build a record with unset
    /// fields; `PartialMatches` is consumed here only to filter by a qualifier (LSP is the
    /// other, future reader) and `ExactCount` splits the no-match vs ambiguous diagnostic.
    let resolveRecordFor
        (ctx: PassContext)
        (diagKey: NodeKey)
        (useSite: UseSite)
        (qualifier: string option)
        (names: string list)
        : ResolvedRecord voption =
        match qualifier with
        | Some typeName ->
            match TypeRegistry.tryRecord ctx.Types useSite typeName with
            | ValueSome info -> ValueSome(LocalRecord info)
            | ValueNone ->
                // Local miss on a qualified record: filter the field-set candidates by
                // short name == qualifier (mirrors `ExternalUnionCase` qualifier matching).
                // A unique survivor is the external record named; otherwise the historical
                // "Unknown record type qualifier" error, unchanged for the local case.
                match
                    (recordFieldSetVerdict ctx useSite false names).PartialMatches
                    |> List.filter (fun r -> resolvedRecordDisplayName r = typeName)
                with
                | [ only ] -> ValueSome only
                | _ ->
                    ctx.Error(diagKey, sprintf "Unknown record type qualifier: %s" typeName)
                    ValueNone
        | None ->
            let verdict = recordFieldSetVerdict ctx useSite true names

            match verdict.ExactMatch with
            | ValueSome r -> ValueSome r
            | ValueNone ->
                // `ExactCount` is the number of records whose field set EQUALS the typed set
                // (what the old exact-set-equality loop counted), disambiguating "no match"
                // (0) from "ambiguous" (>1) — NOT `PartialMatches.Length`, which also holds
                // supersets.
                if verdict.ExactCount = 0 then
                    ctx.Error(diagKey, sprintf "No record type matches the field set: %s" (String.concat ", " names))
                else
                    ctx.Error(
                        diagKey,
                        sprintf
                            "Field set is ambiguous (%d candidate record types); add a qualifier or annotation"
                            verdict.ExactCount
                    )

                ValueNone

    /// The construction shape of a resolved record — its `TyRecord` key, the fresh type
    /// args to instantiate it at, and a per-field-name type resolver — with the
    /// local|external field-type branch factored into ONE place (both call sites unify
    /// each initialiser / sub-pattern against `fieldTypeOf name`).
    ///
    /// Local records instantiate their `RecordTypeInfo` typars fresh and substitute; an
    /// external record reads its frozen field shapes by key and instantiates the template
    /// with the receiver's args. DEFERRED (consistent with external unions): an external
    /// record with an explicitly `obj`-typed field skips `wrapObjArg` boxing because
    /// `translateRecord`/`recordFieldTy` (`Resolve.fs`) is `LocalRecord`-only — the exact
    /// existing limitation for external union cases (`unionCaseFieldTys`). Reified generics
    /// and concrete fields are unaffected.
    let recordConstructionOf
        (ctx: PassContext)
        (r: ResolvedRecord)
        : struct (TypeKey * EqArray<SemType> * (string -> SemType voption)) =
        match r with
        | LocalRecord info ->
            let args, subst = freshNamedInstance ctx info.TypeParams

            let fieldTypeOf (name: string) =
                match info.Fields |> Array.tryFind (fun f -> f.Name = name) with
                | Some f -> ValueSome(substituteWith ctx.Store subst f.Type)
                | None -> ValueNone

            struct (info.TypeKey, args, fieldTypeOf)
        | ExternalRecord candidate ->
            let key = candidate.TypeKey

            let fieldShapes =
                match ctx.Provider.TryLookupType(SymbolKey.Type key) with
                | ValueSome(ExternalTypeShape.Record(_, fs, _)) -> fs
                | _ -> [||]

            // Precompute the args array once (not per field): one fresh TyVar per declared
            // typar slot, instantiating each field's `FTTypar(Declaring,i)` template.
            let args =
                EqArray.ofArray [| for _ in 1 .. candidate.TyparArity -> TyVar(freshTyVar ctx) |]

            let argsArr = args.AsSpan().ToArray()

            let fieldTypeOf (name: string) =
                match fieldShapes |> Array.tryFind (fun f -> f.Name = name) with
                | Some f -> ValueSome(FrozenTypeBridge.instantiateDeclaring f.Frozen argsArr)
                | None -> ValueNone

            struct (key, args, fieldTypeOf)

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
