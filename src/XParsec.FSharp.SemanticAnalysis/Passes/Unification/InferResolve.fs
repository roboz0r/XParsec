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
    /// `Point(3, 4)` calls (no `new`) through the function-application
    /// machinery. `ValueNone` if `name` isn't in `ctx.Types.Class`.
    let tryClassCtorAsFunction (ctx: PassContext) (name: string) : SemType voption =
        match ctx.Types.Class.TryGetValue name with
        | true, info ->
            let args, subst = freshNamedInstance ctx info.TypeParams
            let receiverTy = TyClass(info.Key, args)

            let arg =
                info.CtorParams
                |> Array.map (fun p -> substituteWith subst p.Type)
                |> Array.toList
                |> tupleOrSingle ctx

            ValueSome(TyFun(arg, receiverTy))
        | false, _ -> ValueNone

    let classCtorAsFunction (ctx: PassContext) (name: string) : SemType =
        match tryClassCtorAsFunction ctx name with
        | ValueSome t -> t
        | ValueNone -> TyVar(freshTyVar ctx)

    /// Function-shaped type for a DU ctor reference. Multi-field cases bundle
    /// the fields into a tuple — F# DUs take a tuple as their single argument.
    /// The receiver union's typars are instantiated fresh so two independent
    /// uses of `Some` don't share a `'a`.
    let ctorType (ctx: PassContext) (info: UnionCaseInfo) : SemType =
        let unionInfo = TypeRegistry.unionOfCase ctx.Types info
        let args, subst = freshNamedInstance ctx unionInfo.TypeParams
        let unionTy = TyUnion(unionInfo.Key, args)

        let walkedFields = info.Fields |> Array.map (substituteWith subst)

        match walkedFields.Length with
        | 0 -> unionTy
        | 1 -> TyFun(walkedFields.[0], unionTy)
        | _ -> TyFun(TyTuple(EqArray.ofArray walkedFields), unionTy)

    /// The external union case NameResolution stamped at `key` (a pattern head
    /// `CstKeys.ofPat` or an expression head `CstKeys.ofExpr`), freshly instantiating
    /// the declaring union's typars (one TyVar per declared arity, so two uses of `Some`
    /// don't share a `'a`) and returning the union type plus the per-field types in that
    /// instantiation — the union to return as the pattern's type, and the field types to
    /// unify the sub-patterns against. `ValueNone` when NameResolution recognised no
    /// external case at `key` (the opens / RQA / qualifier discipline lives upstream in
    /// the stamp, not here — this reads its verdict).
    let tryExternalCasePattern (ctx: PassContext) (key: NodeKey) : (SemType * SemType[]) voption =
        match ctx.Resolution.ExternalUnionCaseStamp.TryGetValue key with
        | ValueSome uc ->
            let freshArgs = Array.init uc.Arity (fun _ -> TyVar(freshTyVar ctx))

            let unionTy =
                TyUnion(SymbolKeyOps.externalTypeKey uc.Origin uc.UnionName uc.Arity, EqArray.ofArray freshArgs)

            let fields = ExternalSymbols.instantiateCaseFieldTypes uc.Case freshArgs
            ValueSome(unionTy, fields)
        | ValueNone -> ValueNone

    /// External (referenced-package) analogue of `ctorType`: build the case
    /// ctor's function/value type `field… → TyUnion(union, freshArgs)` from
    /// `tryExternalCasePattern`'s union + field types, reading the case NameResolution
    /// stamped at the expression head `key`.
    let tryExternalCtorType (ctx: PassContext) (key: NodeKey) : SemType voption =
        match tryExternalCasePattern ctx key with
        | ValueNone -> ValueNone
        | ValueSome(unionTy, fields) ->
            match fields.Length with
            | 0 -> ValueSome unionTy
            | 1 -> ValueSome(TyFun(fields.[0], unionTy))
            | _ -> ValueSome(TyFun(TyTuple(EqArray.ofArray fields), unionTy))

    /// ValueNone with `count = 0` means "no such ctor"; `count >= 2` means
    /// ambiguous — the caller emits the appropriate diagnostic.
    let resolveCtorName (ctx: PassContext) (name: string) : UnionCaseInfo voption * int =
        match ctx.Types.CtorIndex.TryGetValue name with
        | false, _ -> ValueNone, 0
        | true, infos when infos.Length = 1 -> ValueSome infos.[0], 1
        | true, infos -> ValueNone, infos.Length

    let resolveQualifiedCtor (ctx: PassContext) (typeName: string) (caseName: string) : UnionCaseInfo voption =
        // Case names are globally unique (even across arity-overloaded unions like
        // `Choice\`2`…`Choice\`7`), so resolve through the reverse case index and let
        // the written qualifier select which union short name the case belongs to.
        // Avoids a bare `Union.[typeName]` lookup, which can't see an arity-overloaded
        // union (its bare alias is withdrawn).
        match ctx.Types.CtorIndex.TryGetValue caseName with
        | false, _ -> ValueNone
        | true, infos -> infos |> EqArray.tryFind (fun c -> c.UnionName = typeName)

    /// Field set match is order-insensitive. candidateCount disambiguates the
    /// "no match" vs "ambiguous" diagnostic paths.
    let findUniqueRecordByFieldSet (ctx: PassContext) (names: string list) : RecordTypeInfo voption * int =
        match names with
        | [] -> ValueNone, 0
        | first :: _ ->
            match ctx.Types.FieldIndex.TryGetValue first with
            | false, _ -> ValueNone, 0
            | true, candidates ->
                let nameSet = Set.ofList names
                let mutable firstHit = Unchecked.defaultof<RecordTypeInfo>
                let mutable count = 0

                for info in candidates do
                    let declared = info.Fields |> Array.map (fun f -> f.Name) |> Set.ofArray

                    if declared = nameSet then
                        if count = 0 then
                            firstHit <- info

                        count <- count + 1

                if count = 1 then
                    ValueSome firstHit, 1
                else
                    ValueNone, count

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

    /// Does `n` (a metadata name) name an external *class* the provider knows?
    /// The probe shared by every external-static-access path.
    let isExternalClass (ctx: PassContext) (n: string) : bool =
        match ctx.Provider.TryLookupType n with
        | ValueSome(ExternalTypeShape.Class _) -> true
        | _ -> false

    /// Does `n` resolve — through the active `open`s, at any small arity — to an
    /// external *union* or *record* type? Unlike a class, a union/record exposes
    /// no static fields: the only valid `n.tail` forms are a module function (a
    /// value) and a union case (a ctor), both tried *before* this probe is
    /// consulted. So an unresolved `tail` under such a qualifier is a genuine
    /// missing-member reference, not the unmodelled-static-field silence a class
    /// qualifier warrants.
    let private resolvesAsExternalUnionOrRecord (ctx: PassContext) (n: string) : bool =
        let isUnionOrRecord (name: string) =
            match ctx.Provider.TryLookupType name with
            | ValueSome(ExternalTypeShape.Union _)
            | ValueSome(ExternalTypeShape.Record _) -> true
            | _ -> false

        let probe (name: string) =
            isUnionOrRecord name
            || [ 1; 2; 3; 4 ]
               |> List.exists (fun a -> isUnionOrRecord (SymbolKeyOps.arityName name a))

        OpenScope.tryQualify ctx.Resolution.OpenScope probe n |> ValueOption.isSome

    /// A 2+-segment qualified reference `Q.member` whose qualifier `Q` (every
    /// segment but the last) names a known external union/record, but whose
    /// `member` resolved to neither a value (module function) nor a case nor a
    /// static member. Those are the only valid forms under such a qualifier and
    /// every one is tried ahead of this probe, so the reference is an unresolved
    /// member — returns `(qualifier, member)` to diagnose. `ValueNone` when the
    /// head isn't such a qualifier (a class qualifier or an unknown one stays a
    /// fresh TyVar: see `tryExternalStaticLongIdent`'s intentional class silence
    /// and the `NameResolution` "Unresolved qualified name" path respectively).
    let tryQualifiedExternalMemberMiss (ctx: PassContext) (e: Expr<SyntaxToken>) : (string * string) voption =
        match e with
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when li.Idents.Length >= 2 ->
            let qualifier =
                seq { for i in 0 .. li.Idents.Length - 2 -> ctx.NameOf li.Idents.[i] }
                |> String.concat "."

            let memberName = ctx.NameOf li.Idents.[li.Idents.Length - 1]

            if resolvesAsExternalUnionOrRecord ctx qualifier then
                ValueSome(qualifier, memberName)
            else
                ValueNone
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
    /// interned `SymbolKey` so Freeze stamps a `TExpr.ExternalMember`.
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
                    Key = m.Key
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
    /// visit already resolves the receiver at its exact arity (opens-aware) and
    /// stamps the type key in `ResolvedType`, keyed by the receiver *head* expr; this
    /// reads that stamp instead of re-running `OpenScope.tryQualify` at inference
    /// time. v1 handles the `TypeApp` form only; non-generic external static access
    /// (`System.Console.Out`) folds into a single LongIdent (see
    /// `tryExternalStaticLongIdent`).
    let tryExternalTypeReceiver
        (ctx: PassContext)
        (recv: Expr<SyntaxToken>)
        : (SymbolKey * Type<SyntaxToken> list) voption =
        // The receiver head as written: a single-segment name parses as `Expr.Ident`
        // (`EqualityComparer<int>`), a dotted one as a `LongIdent`
        // (`System.Collections.Generic.EqualityComparer<int>`). NameResolution stamps
        // `ResolvedType` at the head expr's `NodeKey`.
        match recv with
        | Expr.TypeApp(expr = head; types = typeArgs) ->
            match head with
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent _)
            | Expr.Ident _ ->
                match ctx.Resolution.ResolvedType.TryGetValue(CstKeys.ofExpr head) with
                // The `ResolvedType` stamp is broad (any external type, for the
                // arity-based diagnostic suppression it also drives); a static-member
                // receiver dispatches only on a genuine CLASS (as the former
                // `isExternalClass` filter did), so confirm the shape by key. A generic
                // union/record/abbrev receiver declines here and keeps its own path.
                | ValueSome declTypeKey ->
                    match ctx.Provider.TryLookupType declTypeKey with
                    | ValueSome(ExternalTypeShape.Class _) -> ValueSome(declTypeKey, List.ofSeq typeArgs)
                    | _ -> ValueNone
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
