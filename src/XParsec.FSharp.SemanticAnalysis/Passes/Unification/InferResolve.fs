namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationEngine
open UnificationTranslate

module internal UnificationInferResolve =

    /// Single-segment (`X`) and two-segment (`R.X`) forms only: a multi-segment `A.B.X`
    /// yields `ValueNone` for the qualifier and its last segment as the field name.
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
            // Copy prototype constraints onto the fresh instance so each use site
            // re-evaluates satisfaction independently: `Set<int>` and `Set<int -> int>`
            // each get their own copy of `'a : comparison`.
            ctx.Store.Constraints.Set(freshRoot, ctx.Store.Constraints.Items protoRoot)
            let asTy = TyVar fresh
            subst.[protoRoot.Id] <- asTy
            acc.Add asTy

        EqArray.ofResizeArray acc, subst

    /// A member's signature at FRESH args for its declaring type's typars, paired with those
    /// args.
    let freshMemberInstanceArgs (ctx: PassContext) (hit: TypeRegistry.NominalMember) : EqArray<SemType> * SemType =
        let args, subst = freshNamedInstance ctx hit.Decl.TypeParams
        args, substituteWith ctx.Store subst hit.Member.Type

    /// Unify a qualifier's written `<'args>` into the member's fresh declaring args, so
    /// `C<string>.M` instantiates the member at `<string>`. An arity mismatch is diagnosed and
    /// the args left fresh; an empty `writtenTys` (the bare `C.M`) unifies nothing.
    let unifyWrittenDeclArgs
        (ctx: PassContext)
        (classTok: SyntaxToken)
        (writtenTys: ImArr<Type<SyntaxToken>>)
        (declArgs: EqArray<SemType>)
        : unit =
        if writtenTys.IsEmpty then
            ()
        elif writtenTys.Length <> declArgs.Length then
            ctx.Report(classTok, Kind.TypeArgArity(ctx.NameOf classTok, declArgs.Length, writtenTys.Length))
        else
            for i in 0 .. declArgs.Length - 1 do
                unify ctx classTok (translateType ctx writtenTys.[i]) declArgs.[i]

    /// Function value whose argument shape matches the primary constructor and whose result
    /// is the constructed `TyClass`, routing `Point(3, 4)` / `A.Point(3, 4)` (no `new`)
    /// through function application. `ValueNone` if `written` does not resolve to a class
    /// at `useSite`.
    let tryWrittenClassCtorAsFunction
        (ctx: PassContext)
        (useSite: UseSite)
        (written: WrittenTypeName)
        : SemType voption =
        match TypeRegistry.tryWrittenClass ctx.Types useSite written with
        | ValueSome info ->
            let args, subst = freshNamedInstance ctx info.TypeParams
            let ctorTy = TyClass(info.TypeKey, args)

            let arg =
                info.CtorParams
                |> Array.map (fun p -> substituteWith ctx.Store subst p.Type)
                |> Array.toList
                |> tupleOrSingle ctx.Intrinsics

            ValueSome(TyFun(arg, ctorTy))
        | ValueNone -> ValueNone

    let tryClassCtorAsFunction (ctx: PassContext) (useSite: UseSite) (name: string) : SemType voption =
        tryWrittenClassCtorAsFunction ctx useSite (WrittenTypeName.bare name)

    let classCtorAsFunction (ctx: PassContext) (useSite: UseSite) (name: string) : SemType =
        match tryClassCtorAsFunction ctx useSite name with
        | ValueSome t -> t
        | ValueNone -> TyVar(freshTyVar ctx)

    /// Function-shaped type for a DU ctor reference. Multi-field cases bundle the fields
    /// into a tuple, because an F# DU takes a tuple as its single argument. The union's typars
    /// are instantiated fresh, so two independent uses of `Some` don't share a `'a`.
    let ctorType (ctx: PassContext) (info: UnionCaseInfo) : SemType =
        let unionInfo = TypeRegistry.unionOfCase ctx.Types info
        let args, subst = freshNamedInstance ctx unionInfo.TypeParams
        let unionTy = TyUnion(unionInfo.TypeKey, args)

        let walkedFields = info.Fields |> Array.map (substituteWith ctx.Store subst)

        match walkedFields.Length with
        | 0 -> unionTy
        | 1 -> TyFun(walkedFields.[0], unionTy)
        | _ -> TyFun(TyTuple(EqArray.ofArray walkedFields), unionTy)

    /// The union type and per-field types of a resolved external case, instantiating the
    /// declaring union's typars fresh (one TyVar per declared arity). The union is the
    /// pattern's own type; the field types are what its sub-patterns unify against.
    let externalCasePattern (ctx: PassContext) (uc: ExternalUnionCase) : SemType * SemType[] =
        let freshArgs = Array.init uc.UnionKey.TyparArity (fun _ -> TyVar(freshTyVar ctx))
        let unionTy = TyUnion(uc.UnionKey, EqArray.ofArray freshArgs)
        let fields = ExternalSymbols.instantiateCaseFieldTypes uc.Case freshArgs
        unionTy, fields

    /// The external (referenced-package) case ctor's function/value type
    /// `field… → TyUnion(union, freshArgs)`, built from the case NameResolution
    /// stamped at the applied function's `key`.
    let tryExternalCtorType (ctx: PassContext) (key: NodeKey) : SemType voption =
        match ResolvedStamps.tryExternalUnionCase ctx.Resolution.Resolved key with
        | ValueNone -> ValueNone
        | ValueSome uc ->
            let unionTy, fields = externalCasePattern ctx uc

            match fields.Length with
            | 0 -> ValueSome unionTy
            | 1 -> ValueSome(TyFun(fields.[0], unionTy))
            | _ -> ValueSome(TyFun(TyTuple(EqArray.ofArray fields), unionTy))

    /// The local|external record identity a field set resolves to.
    type ResolvedRecord =
        | LocalRecord of RecordTypeInfo
        | ExternalRecord of ExternalRecordCandidate

    /// The nominal identity a candidate dedups by, and the identity the `TyRecord` node
    /// carries so cross-file it re-homes to the local `TypeDef`. `ExternalRecord` carries
    /// the producer's REAL key, never one re-minted from the `+`-mangled compiled name.
    let resolvedRecordTypeKey (r: ResolvedRecord) : TypeKey =
        match r with
        | LocalRecord info -> info.TypeKey
        | ExternalRecord c -> c.TypeKey

    let resolvedRecordFieldNames (r: ResolvedRecord) : Set<string> =
        match r with
        | LocalRecord info -> info.Fields |> Array.map (fun f -> f.Name) |> Set.ofArray
        | ExternalRecord c -> Set.ofSeq c.FieldNames

    /// A pure field-set verdict, no diagnostics. `PartialMatches` = every record whose
    /// declared field set ⊇ the typed set, deduped by `TypeKey`. `Exact` = the partials whose
    /// set EQUALS it.
    type RecordFieldSetVerdict =
        {
            Exact: RecordFieldClassifier.ExactMatch<ResolvedRecord>
            PartialMatches: ResolvedRecord list
        }

    /// How a record literal or pattern names its type: `{ X = … }` against the unqualified
    /// field-set index, or `{ R.X = … }` against the records whose simple name is `R`.
    type RecordLookup =
        | Bare
        | Qualified of typeName: string

    /// The record's own simple (segment) name, driving both the "has no field" diagnostic
    /// and the qualified-literal qualifier match (`R` in `{ R.X = … }`). Taken off the
    /// `TypeKey`, never the `+`-mangled compiled meta name (`Test.A.M+R` → `M+R`).
    let resolvedRecordDisplayName (r: ResolvedRecord) : string =
        match r with
        | LocalRecord info -> info.Name
        | ExternalRecord candidate ->
            let (DisplayName shown) = SymbolKeyOps.typeSimpleName candidate.TypeKey
            shown

    /// Whether a provider (cross-file) record candidate belongs to the UNQUALIFIED field-set
    /// index a bare `{ X = … }` literal reads: not `[<RequireQualifiedAccess>]`, and its
    /// declaring module/namespace reachable unqualified under the opens in force here.
    let private admitsBareExternalRecord (ctx: PassContext) (cand: ExternalRecordCandidate) : bool =
        if cand.IsRequireQualifiedAccess then
            false
        else
            let key = cand.TypeKey

            match SymbolKeyOps.tryModuleContainerOf key.Container with
            | ValueNone -> false
            | ValueSome c when ctx.ImplicitOpens |> List.exists (fun o -> o.Container = c) -> true
            | ValueSome c ->
                let h = SymbolKeyOps.containerFullName c
                let (DisplayName simple) = SymbolKeyOps.typeSimpleName key
                let dotted = if h = "" then simple else h + "." + simple

                OpenScope.tryQualify ctx.Resolution.OpenScope (fun c -> c = dotted) simple
                |> ValueOption.isSome

    /// The verdict for the typed field set `names` at `useSite`, unioning LOCAL and provider
    /// candidates; only the FIRST field's candidates need fetching, since a record declaring
    /// every typed field declares the first. `Bare` scope/RQA-gates the provider half;
    /// `Qualified` keeps the candidates whose simple name is the qualifier.
    let recordFieldSetVerdict
        (ctx: PassContext)
        (useSite: UseSite)
        (lookup: RecordLookup)
        (names: string list)
        : RecordFieldSetVerdict =
        match names with
        | [] ->
            {
                Exact = RecordFieldClassifier.ExactMatch.NoMatch
                PartialMatches = []
            }
        | first :: _ ->
            let providerRecords =
                ctx.TryRecordsWithField first
                |> EqArray.filter (fun cand ->
                    match lookup with
                    | Bare -> admitsBareExternalRecord ctx cand
                    | Qualified _ -> true
                )

            let withFirstField =
                [
                    for info in TypeRegistry.recordsWithField ctx.Types useSite first -> LocalRecord info
                    for cand in providerRecords -> ExternalRecord cand
                ]

            let candidates =
                match lookup with
                | Bare -> withFirstField
                | Qualified typeName -> withFirstField |> List.filter (fun r -> resolvedRecordDisplayName r = typeName)

            // The classifier's dedup is first-wins, so listing local candidates first is
            // what pins a `TypeKey` present both locally and via a provider to the LOCAL
            // record, which is the authoritative one for the compiling file.
            let classification =
                RecordFieldClassifier.classifyRecordCandidates
                    resolvedRecordTypeKey
                    resolvedRecordFieldNames
                    candidates
                    (Set.ofList names)

            {
                Exact = classification.Exact
                PartialMatches = classification.Partial
            }

    /// The record a literal / pattern resolves to, or `ValueNone` with the diagnostic already
    /// emitted. CONSTRUCTION resolves ONLY on `ExactMatch`: there is no missing-field check,
    /// so accepting a superset would silently build a record with unset fields.
    let resolveRecordFor
        (ctx: PassContext)
        (diagTok: SyntaxToken)
        (useSite: UseSite)
        (qualifier: string option)
        (names: string list)
        : ResolvedRecord voption =
        match qualifier with
        | Some typeName ->
            match TypeRegistry.tryRecord ctx.Types useSite typeName with
            | ValueSome info -> ValueSome(LocalRecord info)
            | ValueNone ->
                // Local miss on a qualified record: a unique survivor among the field-set
                // candidates named `typeName` is the external record named.
                match (recordFieldSetVerdict ctx useSite (Qualified typeName) names).PartialMatches with
                | [ only ] -> ValueSome only
                | _ ->
                    ctx.Report(diagTok, Kind.Message(sprintf "Unknown record type qualifier: %s" typeName))
                    ValueNone
        | None ->
            let verdict = recordFieldSetVerdict ctx useSite Bare names

            match verdict.Exact with
            | RecordFieldClassifier.ExactMatch.Unique r -> ValueSome r
            | RecordFieldClassifier.ExactMatch.NoMatch ->
                ctx.Report(
                    diagTok,
                    Kind.Message(sprintf "No record type matches the field set: %s" (String.concat ", " names))
                )

                ValueNone
            | RecordFieldClassifier.ExactMatch.Ambiguous count ->
                ctx.Report(
                    diagTok,
                    Kind.Message(
                        sprintf
                            "Field set is ambiguous (%d candidate record types); add a qualifier or annotation"
                            count
                    )
                )

                ValueNone

    /// The construction shape of a resolved record: its `TyRecord` key, the fresh type args
    /// to instantiate it at, and a per-field-name type resolver. A local record substitutes
    /// its own typars; an external one instantiates its frozen field-shape templates.
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
                match ctx.Provider.TryLookupType key with
                | ValueSome(ExternalTypeShape.Record(fields = fs)) -> fs
                | _ -> EqArray.empty

            // Precompute the args array once (not per field): one fresh TyVar per declared
            // typar slot, instantiating each field's `FTTypar(Declaring,i)` template.
            let args =
                EqArray.ofArray [| for _ in 1 .. candidate.TyparArity -> TyVar(freshTyVar ctx) |]

            let argsArr = args.AsSpan().ToArray()

            let fieldTypeOf (name: string) =
                match fieldShapes |> EqArray.tryFind (fun f -> f.Name = name) with
                | ValueSome f -> ValueSome(FrozenTypeBridge.instantiateDeclaring f.Frozen argsArr)
                | ValueNone -> ValueNone

            struct (key, args, fieldTypeOf)

    /// `Circle(r)` parses as `Circle (EnclosedBlock r)`; `Rectangle(w, h)` as
    /// `Rectangle (EnclosedBlock (Tuple [w; h]))`. Both forms, plus a bare single arg, are
    /// what the F# DU ctor application convention emits.
    let unwrapCtorArgPattern (p: Pat<SyntaxToken>) : Pat<SyntaxToken> list =
        match p with
        | Pat.EnclosedBlock(pat = Pat.Tuple(patterns = pats)) -> List.ofSeq pats
        | Pat.EnclosedBlock(pat = inner) -> [ inner ]
        | Pat.Tuple(patterns = pats) -> List.ofSeq pats
        | _ -> [ p ]

    /// The two names a `NoMember` diagnostic reads. Both are for DISPLAY: the qualifier is
    /// the resolved identity with its arity suffix already dropped, not a lookup key.
    type QualifiedMemberMiss =
        {
            Qualifier: string
            MemberName: string
        }

    /// For a 2+-segment `Q.member` whose qualifier resolves to a known external union/record but
    /// whose last segment resolved to no value, case or static member, the names to diagnose.
    let tryQualifiedExternalMemberMiss (ctx: PassContext) (e: Expr<SyntaxToken>) : QualifiedMemberMiss voption =
        match e with
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when li.Idents.Length >= 2 ->
            match ResolvedStamps.tryUnionRecordQualifier ctx.Resolution.Resolved (CstKeys.ofExpr e) with
            | ValueSome qualifierKey ->
                // `bareName` drops the arity suffix a generic union's key carries
                // (`Vesper.Option`1` reads as `Vesper.Option` in a user diagnostic).
                ValueSome
                    {
                        Qualifier = SymbolKeyOps.bareName (SymbolKeyOps.typeMetaName qualifierKey)
                        MemberName = ctx.NameOf li.Idents.[li.Idents.Length - 1]
                    }
            | ValueNone -> ValueNone
        | _ -> ValueNone

    /// Split a folded static-member LongIdent (`System.Console.Out`) into the qualifier
    /// PREFIX's resolved type identity and the trailing member token, reading the qualifier
    /// NameResolution stamped rather than re-resolving here.
    let splitExternalStaticPrefix
        (ctx: PassContext)
        (key: NodeKey)
        (li: LongIdent<SyntaxToken>)
        : (TypeKey * SyntaxToken) voption =
        let lastTok = li.Idents.[li.Idents.Length - 1]

        match ResolvedStamps.tryStaticQualifier ctx.Resolution.Resolved key with
        | ValueSome declTypeKey -> ValueSome(declTypeKey, lastTok)
        | ValueNone -> ValueNone

    /// Static member access on an external type, recorded in `ExternalAccess` for Elaborate to
    /// emit as a keyed `TExpr.ExternalMember`. `typeArgs` instantiate the declaring type's
    /// typars (`EqualityComparer<int>.Default` types at `<int>`), and are fresh vars when empty.
    let inferExternalStaticMember
        (ctx: PassContext)
        (key: NodeKey)
        (declTypeKey: TypeKey)
        (typeArgs: SemType list)
        (memberTok: SyntaxToken)
        : SemType =
        let memberName = ctx.NameOf memberTok

        let declaringArgs =
            match typeArgs with
            | [] -> Array.init declTypeKey.TyparArity (fun _ -> TyVar(freshTyVar ctx))
            | written -> List.toArray written

        match ctx.Provider.TryLookupMember(declTypeKey, memberName) with
        | ValueSome m ->
            let memberSig = ExternalSymbols.openSignature m declaringArgs

            ctx.Resolution.ExternalAccess.Set(key, ResolvedExternalMember.OfMember(m, memberSig))

            memberSig
        | ValueNone ->
            errorTy
                ctx
                memberTok
                (Kind.NoMember(SymbolKeyOps.typeMetaName declTypeKey, MemberNoun.AccessibleMember, memberName))

    /// If `qualifier` is an external generic type name used as a static-access qualifier
    /// (`EqualityComparer<int>` in `EqualityComparer<int>.Default`), its declaring type's
    /// stamped key and the raw CST type args, left for the caller to translate.
    let tryExternalTypeQualifier
        (ctx: PassContext)
        (qualifier: Expr<SyntaxToken>)
        : (TypeKey * Type<SyntaxToken> list) voption =
        // The qualifier as written: a single-segment name parses as `Expr.Ident`
        // (`EqualityComparer<int>`), a dotted one as a `LongIdent`.
        match qualifier with
        | Expr.TypeApp(expr = fn; types = typeArgs) ->
            match fn with
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent _)
            | Expr.Ident _ ->
                // The `TypeApp` visit stamped the applied name at its exact arity.
                match ctx.Resolution.Resolved.TryGetValue(CstKeys.ofExpr fn) with
                | ValueSome(ResolvedItem.Type(ResolvedTypeRef.External(declTypeKey, ExternalTypeShape.Class _))) ->
                    ValueSome(declTypeKey, List.ofSeq typeArgs)
                | _ -> ValueNone
            | _ -> ValueNone
        | _ -> ValueNone

    /// `System.Console.Out` / `Console.Out` (under `open System`): a non-generic type folds
    /// into ONE LongIdent (the parser merges consecutive `.ident`), so the prefix/member
    /// split is recovered here. A last segment that is no static member falls through silently.
    let tryExternalStaticLongIdent (ctx: PassContext) (key: NodeKey) (e: Expr<SyntaxToken>) : SemType voption =
        match e with
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when li.Idents.Length >= 2 ->
            match splitExternalStaticPrefix ctx key li with
            | ValueSome(declTypeKey, lastTok) ->
                // Claim it only if the member actually resolves; otherwise leave
                // the node to the ctor/TyVar fallback without a spurious error.
                match ctx.Provider.TryLookupMember(declTypeKey, ctx.NameOf lastTok) with
                | ValueSome _ -> ValueSome(inferExternalStaticMember ctx key declTypeKey [] lastTok)
                | ValueNone -> ValueNone
            | ValueNone -> ValueNone
        | _ -> ValueNone

    /// A folded-LongIdent external STATIC member reference (`System.String.Concat`), as its
    /// declaring type's stamped `SymbolKey` + member token. An anchor that is a local
    /// binding (an `r.X.Y` field chain) is excluded.
    let tryResolveExternalStaticMemberRef (ctx: PassContext) (e: Expr<SyntaxToken>) : (TypeKey * SyntaxToken) voption =
        match e with
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
            li.Idents.Length >= 2
            && not (ctx.Bindings.Binding.ContainsKey(NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent))
            ->
            match splitExternalStaticPrefix ctx (CstKeys.ofExpr e) li with
            | ValueSome(declTypeKey, lastTok) when
                (ctx.Provider.TryLookupMembers(declTypeKey, ctx.NameOf lastTok)).Length > 0
                ->
                ValueSome(declTypeKey, lastTok)
            | _ -> ValueNone
        | _ -> ValueNone
