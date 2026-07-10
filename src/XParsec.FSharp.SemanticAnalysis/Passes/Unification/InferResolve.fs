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

    /// Resolve a union case in *pattern* position through the provider's reverse
    /// case index, freshly instantiating the declaring union's typars (one TyVar
    /// per declared arity, so two uses of `Some` don't share a `'a`) and
    /// returning the union type plus the per-field types in that instantiation —
    /// the union to return as the pattern's type, and the field types to unify
    /// the sub-patterns against. `qualifier` is the optionally-written declaring type (`Option.Some` ⇒ `ValueSome
    /// "Option"`): when present, the case is accepted only if the resolved
    /// union's short name matches it; the bare form (`ValueNone`) skips that
    /// guard. `ValueNone` when no external union declares `caseName`.
    let tryExternalCasePattern
        (ctx: PassContext)
        (qualifier: string voption)
        (caseName: string)
        : (SemType * SemType[]) voption =
        match ctx.Provider.TryLookupUnionCase caseName with
        // A bare (unqualified) reference to an `[<RequireQualifiedAccess>]` union's
        // case never resolves — F# requires `Color.Red`, not `Red`
        // `ResolvesWith` also enforces the qualifier
        // match for the qualified leg.
        | ValueSome uc when uc.ResolvesWith qualifier ->
            let freshArgs = Array.init uc.Arity (fun _ -> TyVar(freshTyVar ctx))

            let unionTy =
                TyUnion(SymbolKeyOps.externalTypeKey uc.Origin uc.UnionName uc.Arity, EqArray.ofArray freshArgs)

            let fields = ExternalSymbols.instantiateCaseFieldTypes uc.Case freshArgs
            ValueSome(unionTy, fields)
        | _ -> ValueNone

    /// External (referenced-package) analogue of `ctorType`: build the case
    /// ctor's function/value type `field… → TyUnion(union, freshArgs)` from
    /// `tryExternalCasePattern`'s union + field types. `qualifier` carries an
    /// optional written declaring type (`Option.Some`); see `tryExternalCasePattern`.
    let tryExternalCtorType (ctx: PassContext) (qualifier: string voption) (caseName: string) : SemType voption =
        match tryExternalCasePattern ctx qualifier caseName with
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

    /// Resolve an external enum case access `E.C1` (`headName` = `E`, `caseName` =
    /// `C1`): if `headName` resolves — through the active `open`s — to an
    /// `ExternalTypeShape.Enum` that declares `caseName`, return the enum's frozen
    /// `SymbolKey` (no args — enums are never generic). The enum analogue of
    /// `tryExternalCtorType`'s reverse union-case lookup, but an enum case is a named
    /// constant on a closed set, so the access types as the enum NOMINAL itself
    /// (`TyEnum key`), not a ctor arrow. The key matches the one
    /// `Translate.tryResolveExternalType` mints for an `(x: E)` annotation, so the
    /// access and the annotation unify. `ValueNone` when no external enum named
    /// `headName` declares `caseName`.
    let tryExternalEnumCase (ctx: PassContext) (headName: string) (caseName: string) : SymbolKey voption =
        let asEnum (n: string) =
            match ctx.Provider.TryLookupType n with
            | ValueSome(ExternalTypeShape.Enum(cases, origin)) -> ValueSome(cases, origin)
            | _ -> ValueNone

        match OpenScope.tryQualify ctx.Resolution.OpenScope (fun n -> (asEnum n).IsSome) headName with
        | ValueSome resolved ->
            match asEnum resolved with
            | ValueSome(cases, origin) when cases |> Array.exists (fun (c: ExternalEnumCaseShape) -> c.Name = caseName) ->
                ValueSome(SymbolKeyOps.externalTypeKey origin resolved 0)
            | _ -> ValueNone
        | ValueNone -> ValueNone

    /// `tryExternalEnumCase` as a pattern over the `E.C1` `LongIdent` (shared by the
    /// `Expr.LongIdentOrOp` and `Pat.Named` enum-case arms): binds the resolved enum
    /// key once, replacing the guard-then-`.Value` re-lookup (each lookup runs a
    /// provider type probe + an `open`-scope qualify, so evaluating it twice is real
    /// work). Two-segment only — single/multi-segment heads can't be enum cases.
    [<return: Struct>]
    let (|ExternalEnumCaseLi|_|) (ctx: PassContext) (li: LongIdent<SyntaxToken>) : SymbolKey voption =
        if li.Idents.Length = 2 then
            tryExternalEnumCase ctx (ctx.NameOf li.Idents.[0]) (ctx.NameOf li.Idents.[1])
        else
            ValueNone

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

    /// Split a multi-segment LongIdent into `(resolvedPrefix, lastTok)` where the
    /// prefix (every segment but the last) resolves — through the active `open`s —
    /// to an external class. `ValueNone` if it doesn't. The folded-LongIdent
    /// scaffolding shared by the external static-member / static-value paths.
    let splitExternalClassPrefix (ctx: PassContext) (li: LongIdent<SyntaxToken>) : (string * SyntaxToken) voption =
        let lastTok = li.Idents.[li.Idents.Length - 1]

        let prefixName =
            seq { for i in 0 .. li.Idents.Length - 2 -> ctx.NameOf li.Idents.[i] }
            |> String.concat "."

        match OpenScope.tryQualify ctx.Resolution.OpenScope (isExternalClass ctx) prefixName with
        | ValueSome resolved -> ValueSome(resolved, lastTok)
        | ValueNone -> ValueNone

    /// Static member access on an external type, recording the resolved member's
    /// interned `SymbolKey` so Freeze stamps a `TExpr.ExternalMember`.
    /// `typeArgs` instantiate the declaring type's
    /// typars, so `EqualityComparer<int>.Default` types as `EqualityComparer<int>`.
    let inferExternalStaticMember
        (ctx: PassContext)
        (key: NodeKey)
        (metaName: string)
        (typeArgs: SemType list)
        (memberTok: SyntaxToken)
        : SemType =
        let memberName = ctx.NameOf memberTok

        // Stage 3 holdout (bucket 3b): `metaName` is an opens-resolved spelling, not a
        // stamped key — mint an asm-blind key transitionally so this goes through the
        // key-addressed store face. Replaced by a NameResolution stamp when 3b lands.
        match ctx.Provider.TryLookupMember(SymbolKeyOps.qualifiedTypeKey metaName 0, memberName) with
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
        | ValueNone -> errorTy ctx key (sprintf "Type '%s' has no accessible member '%s'" metaName memberName)

    /// If `recv` is an *external generic type name* used as a static-access
    /// receiver (`EqualityComparer<int>` in `EqualityComparer<int>.Default`),
    /// return its metadata name (`` …EqualityComparer`1 ``) and the raw CST type
    /// args. Translation is deferred to the caller so the guard stays side-effect
    /// free — it only probes the provider. v1 handles the `TypeApp` form only;
    /// non-generic external static access (`System.Console.Out`) is a follow-up.
    let tryExternalTypeReceiver
        (ctx: PassContext)
        (recv: Expr<SyntaxToken>)
        : (string * Type<SyntaxToken> list) voption =
        // The receiver type name as written: a single-segment name parses as
        // `Expr.Ident` (`EqualityComparer<int>`), a dotted one as a `LongIdent`
        // (`System.Collections.Generic.EqualityComparer<int>`).
        let nameAndArgs =
            match recv with
            | Expr.TypeApp(expr = Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li); types = typeArgs) ->
                ValueSome(li.Idents |> Seq.map ctx.NameOf |> String.concat ".", typeArgs)
            | Expr.TypeApp(expr = Expr.Ident tok; types = typeArgs) -> ValueSome(ctx.NameOf tok, typeArgs)
            | _ -> ValueNone

        match nameAndArgs with
        | ValueNone -> ValueNone
        | ValueSome(qualName, typeArgs) ->
            let arity = typeArgs.Length
            // The arity-suffixed metadata name for a candidate (`EqualityComparer`1`).
            let metaNameOf (n: string) = SymbolKeyOps.arityName n arity

            // `tryQualify` applies the `open` prefixes, so a short
            // `EqualityComparer<int>` receiver resolves to its qualified metadata
            // name.
            match
                OpenScope.tryQualify ctx.Resolution.OpenScope (fun n -> isExternalClass ctx (metaNameOf n)) qualName
            with
            | ValueSome resolved -> ValueSome(metaNameOf resolved, List.ofSeq typeArgs)
            | ValueNone -> ValueNone

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
            match splitExternalClassPrefix ctx li with
            | ValueSome(resolved, lastTok) ->
                // Claim it only if the member actually resolves; otherwise leave
                // the node to the ctor/TyVar fallback without a spurious error.
                // Stage 3 holdout (bucket 3b): `resolved` is an opens-resolved spelling.
                match ctx.Provider.TryLookupMember(SymbolKeyOps.qualifiedTypeKey resolved 0, ctx.NameOf lastTok) with
                | ValueSome _ -> ValueSome(inferExternalStaticMember ctx key resolved [] lastTok)
                | ValueNone -> ValueNone
            | ValueNone -> ValueNone
        | _ -> ValueNone

    /// Resolve a folded-LongIdent external *static* member reference
    /// (`System.String.Concat`) to its declaring type's metadata name + member
    /// token. The head being a local binding — a `r.X.Y` field chain — is excluded.
    let tryResolveExternalStaticMemberRef (ctx: PassContext) (e: Expr<SyntaxToken>) : (string * SyntaxToken) voption =
        match e with
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
            li.Idents.Length >= 2
            && not (ctx.Bindings.Binding.ContainsKey(NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent))
            ->
            match splitExternalClassPrefix ctx li with
            | ValueSome(resolved, lastTok) when
                // Stage 3 holdout (bucket 3b): `resolved` is an opens-resolved spelling.
                (ctx.Provider.TryLookupMembers(SymbolKeyOps.qualifiedTypeKey resolved 0, ctx.NameOf lastTok)).Length > 0
                ->
                ValueSome(resolved, lastTok)
            | _ -> ValueNone
        | _ -> ValueNone
