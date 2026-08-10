namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationEngine
open UnificationTranslate
open UnificationInferGeneralize
open UnificationInferLiterals
open UnificationInferResolve
open UnificationInferPat
open UnificationInferOverload
open UnificationInferForwardSchemes
open UnificationInferDispatch

module internal UnificationInferRecordAccess =

    /// `memberName` on an intrinsic object argument (`TyConst`), declared either on the
    /// intrinsic's own contract or on the BCL type its `(# "…" #)` binding canonicalises
    /// to (both surfaces, contract first). Declines when neither declares it.
    [<return: Struct>]
    let private (|IntrinsicBclMember|_|)
        (ctx: PassContext)
        (memberName: string)
        (ty: SemType)
        : struct (SymbolKey * EqArray<SemType> * ExternalMember) voption =
        let onSurface (struct (declKey, args)) =
            match ctx.Provider.TryLookupMember(declKey, memberName) with
            | ValueSome m -> Some(struct (declKey, args, m))
            | ValueNone -> None

        match externalSurfaceKeys ctx ty |> List.tryPick onSurface with
        | Some hit -> ValueSome hit
        | None -> ValueNone

    let rec inferRecord
        (infer: Infer)
        (ctx: PassContext)
        (node: NodeSite)
        (inits: ImmutableArray<FieldInitializer<SyntaxToken>>)
        : SemType =
        let pairs =
            [
                for FieldInitializer(longIdent = li; expr = e) in inits ->
                    let q, n = fieldNameAndQualifier ctx li
                    q, n, e
            ]

        let qualifier =
            pairs
            |> List.tryPick (fun (q, _, _) ->
                match q with
                | ValueSome q -> Some q
                | _ -> None
            )

        let names = pairs |> List.map (fun (_, n, _) -> n)

        match resolveRecordFor ctx node.Tok (ctx.UseSiteAt node.Key) qualifier names with
        | ValueNone ->
            for _, _, e in pairs do
                infer ctx e |> ignore

            TyVar(freshTyVar ctx)
        | ValueSome r ->
            // A field is an assignment target, so each initialiser COERCES into the field
            // type (`unifyArg`, argument position) rather than unifying symmetrically: a
            // value boxes into an `obj` field, while a free `'a` field pins to the value.
            let struct (recKey, args, fieldTypeOf) = recordConstructionOf ctx r

            for _, fieldName, e in pairs do
                let eTy = infer ctx e

                match fieldTypeOf fieldName with
                | ValueSome fieldTy -> unifyArg ctx (CstKeys.firstTokenOfExpr e) eTy fieldTy
                | ValueNone ->
                    ctx.Report(
                        CstKeys.firstTokenOfExpr e,
                        Kind.NoMember(resolvedRecordDisplayName r, MemberNoun.Field, fieldName)
                    )

            TyRecord(recKey, args)

    and inferRecordClone
        (infer: Infer)
        (ctx: PassContext)
        (node: NodeSite)
        (src: Expr<SyntaxToken>)
        (inits: ImmutableArray<FieldInitializer<SyntaxToken>>)
        : SemType =
        let srcTy = infer ctx src

        match resolveStep ctx.Store srcTy with
        | TyRecord(recKey, srcArgs) ->
            match TypeRegistry.tryRecordByKey ctx.Types recKey with
            | ValueSome info ->
                // Clone preserves the source's arg list, so overrides unify
                // against the substituted field type (`'a` → source's arg).
                let subst = mkNamedTypeSubst ctx.Store info.TypeParams srcArgs

                for FieldInitializer(longIdent = li; expr = e) in inits do
                    let _, fieldName = fieldNameAndQualifier ctx li
                    let eTy = infer ctx e

                    match info.Fields |> Array.tryFind (fun f -> f.Name = fieldName) with
                    | Some field ->
                        unify ctx (CstKeys.firstTokenOfExpr e) eTy (substituteWith ctx.Store subst field.Type)
                    | None ->
                        ctx.Report(CstKeys.firstTokenOfExpr e, Kind.NoMember(info.Name, MemberNoun.Field, fieldName))

                TyRecord(recKey, srcArgs)
            | ValueNone ->
                let (DisplayName shown) = SymbolKeyOps.typeSimpleName recKey
                ctx.Report(node.Tok, Kind.UnknownNominalType(NominalKind.Record, shown))

                for FieldInitializer(expr = e) in inits do
                    infer ctx e |> ignore

                TyRecord(recKey, srcArgs)
        | _ ->
            ctx.Report(node.Tok, Kind.Message "Record clone requires the source expression to be a record")

            for FieldInitializer(expr = e) in inits do
                infer ctx e |> ignore

            TyVar(freshTyVar ctx)

    /// An instance member on a project-local class/union/record, instantiated at the
    /// object argument's `args`. A name that exists but is STATIC gets an "access it via
    /// 'Type.Member'" hint rather than a bare no-such-member diagnostic.
    and resolveLocalInstanceMember
        (ctx: PassContext)
        (diagTok: SyntaxToken)
        (typeName: string)
        (typeParams: EqArray<string * TyVarId>)
        (args: EqArray<SemType>)
        (members: TypeMemberInfo[])
        (memberName: string)
        : SemType =
        match members |> Array.tryFind (fun m -> m.Name = memberName && not m.IsStatic) with
        | Some m -> instantiateMemberCall ctx (typeParams, args) m.EffectiveMethodTypars m.Type
        | None ->
            if members |> Array.exists (fun m -> m.Name = memberName && m.IsStatic) then
                errorTy
                    ctx
                    diagTok
                    (Kind.Message(
                        sprintf
                            "Member '%s' on type '%s' is static; access it via '%s.%s'"
                            memberName
                            typeName
                            typeName
                            memberName
                    ))
            else
                errorTy ctx diagTok (Kind.NoMember(typeName, MemberNoun.InstanceMember, memberName))

    /// `memberName` on a typar object argument, through an interface the typar is coerced to
    /// (`'T :> IFace`). On a hit, stamps the interface key in `TyparInterfaceCall` under
    /// `diagKey` so Elaborate dispatches `CallVia.Interface`; `ValueNone` leaves it parked.
    and tryTyparInterfaceMember
        (ctx: PassContext)
        (diagKey: NodeKey)
        (root: Rep)
        (memberName: string)
        : SemType voption =
        let rec scan (cs: SemanticConstraint list) : SemType voption =
            match cs with
            | [] -> ValueNone
            | c :: rest ->
                match c.Kind with
                | SemanticConstraintKind.Coercion target ->
                    match resolveStep ctx.Store target with
                    | TyClass(ifaceKey, ifaceArgs) ->
                        match TypeRegistry.tryClassByKey ctx.Types ifaceKey with
                        | ValueSome info when info.IsInterface ->
                            match tryClassChainMember ctx ifaceKey ifaceArgs memberName with
                            | ValueSome mty ->
                                ctx.Resolution.TyparInterfaceCall.Set(diagKey, (ifaceKey, ifaceArgs))
                                ValueSome mty
                            | ValueNone -> scan rest
                        | _ ->
                            // The interface is external, so the member resolves through the
                            // provider. The object argument stays a typar (it never grounds to the
                            // interface), so this stamps `TyparInterfaceCall`, not `ExternalAccess`.
                            match ctx.Provider.TryLookupMember(SymbolKey.Type ifaceKey, memberName) with
                            | ValueSome m when not m.IsStatic ->
                                ctx.Resolution.TyparInterfaceCall.Set(diagKey, (ifaceKey, ifaceArgs))
                                ValueSome(ExternalSymbols.openSignature m (ifaceArgs.AsSpan().ToArray()))
                            | _ -> scan rest
                    | _ -> scan rest
                | _ -> scan rest

        scan (ctx.Store.Constraints.Items root)

    /// `access` is the whole access EXPRESSION, whose key files the resolved member for
    /// Elaborate. `memberTok` is the SEGMENT: a folded `r.X.Y` is one expression with one
    /// key whose intermediate segments have no node of their own, so diagnostics use the token.
    and resolveFieldStep (ctx: PassContext) (access: NodeSite) (memberTok: SyntaxToken) (rTy: SemType) : SemType =
        let memberName = ctx.NameOf memberTok
        // Commit a single-candidate external instance member; `memberArgs` instantiate ITS
        // declaring type's typars (the object argument's own, or a supertype's as-reached for an
        // INHERITED one). Method typars freshen per site, because `openSignature` shares them across sites.
        let commitExternalMember (m: ExternalMember) (memberArgs: EqArray<SemType>) : SemType =
            let memberSig =
                ExternalSymbols.instantiateSignature ctx.Store m (memberArgs.AsSpan().ToArray()) ctx.CurrentLevel

            ctx.Resolution.ExternalAccess.Set(
                access.Key,
                {
                    Key = SymbolKey.Member m.Key
                    IsStatic = false
                    Storage = m.Storage
                    Signature = memberSig
                    OptionalDefaults = m.OptionalDefaults
                }
            )

            memberSig

        match resolveStep ctx.Store rTy with
        | TyRecord(recKey, args) ->
            match TypeRegistry.tryRecordByKey ctx.Types recKey with
            | ValueSome info ->
                match info.Fields |> Array.tryFind (fun f -> f.Name = memberName) with
                | Some field -> instantiateMember ctx.Store (info.TypeParams, args) field.Type
                // A record also carries augmentation members, so `r.Bar` takes the
                // class/union path rather than reporting a field miss.
                | None ->
                    resolveLocalInstanceMember ctx memberTok info.Name info.TypeParams args info.Members memberName
            | ValueNone ->
                // Not project-local, so the record is the provider's (a prior file, or a referenced package).
                let recQual = SymbolKeyOps.typeMetaName recKey

                match ctx.Provider.TryLookupType(SymbolKey.Type recKey) with
                | ValueSome(ExternalTypeShape.Record(_, fieldShapes, _)) ->
                    match fieldShapes |> EqArray.tryFind (fun f -> f.Name = memberName) with
                    | ValueSome fieldShape ->
                        // A field read must NOT stamp `ExternalAccess`: Elaborate tries the
                        // stamped-member arm BEFORE the dot-lookup arm, so a stamped field
                        // lowers to a property call instead of `TExpr.FieldGet` / `ldfld`.
                        FrozenTypeBridge.instantiateDeclaring fieldShape.Frozen (args.AsSpan().ToArray())
                    | ValueNone ->
                        // An external record also carries augmentation members. This IS a
                        // member, so stamping `ExternalAccess` is correct here.
                        match ctx.Provider.TryLookupMember(SymbolKey.Type recKey, memberName) with
                        | ValueSome m when not m.IsStatic -> commitExternalMember m args
                        | _ -> errorTy ctx memberTok (Kind.NoMember(recQual, MemberNoun.FieldOrMember, memberName))
                | _ ->
                    let (DisplayName name) = SymbolKeyOps.typeSimpleName recKey
                    errorTy ctx memberTok (Kind.UnknownNominalType(NominalKind.Record, name))
        | TyClass(clsKey, args) ->
            // Lookups below go by the arity-qualified key: an arity-overloaded object argument
            // (`Fun\`2`/`Fun\`3`) does not resolve by bare name. `clsSimple` is diagnostics-only.
            let (DisplayName clsSimple) = SymbolKeyOps.typeSimpleName clsKey

            match TypeRegistry.tryClassByKey ctx.Types clsKey with
            | ValueSome info ->
                // Derived members shadow inherited ones. On a total miss the diagnostic
                // names the object argument's own class rather than some ancestor.
                match tryClassChainMember ctx clsKey args memberName with
                | ValueSome ty -> ty
                | ValueNone ->
                    // An explicit `val x: T` instance field (struct enumerator state),
                    // instantiated at the object argument's type args as a member would be.
                    match info.InstanceFields |> Array.tryFind (fun f -> f.Name = memberName) with
                    | Some fld -> instantiateMember ctx.Store (info.TypeParams, args) fld.Type
                    | None ->
                        resolveLocalInstanceMember ctx memberTok clsSimple info.TypeParams args info.Members memberName
            | ValueNone ->
                // Not project-local, so the class is the provider's (a BCL
                // `TyClass("…EqualityComparer\`1", [int])` from a prior static access).
                let clsQual = SymbolKeyOps.typeMetaName clsKey

                match ctx.Provider.TryLookupMember(SymbolKey.Type clsKey, memberName) with
                | ValueSome m when not m.IsStatic -> commitExternalMember m args
                | _ ->

                    // The TS-manifest provider stores heritage un-flattened (the CLR metadata
                    // layer already flattens), so an own-member miss may still resolve on a
                    // supertype. That commits at the SUPERTYPE's args, so `Base<int>.value` types as `int`.
                    match tryExternalInheritedMember ctx rTy memberName with
                    | ValueSome(struct (m, memberArgs)) -> commitExternalMember m memberArgs
                    | ValueNone ->

                        // An object argument typed as a CAPABILITY (`enumerator<'T>`, `seq<'T>`) is an
                        // `IntrinsicInterface`: it names a platform type but carries no member
                        // table, so retry there. A non-capability key comes back unchanged.
                        let platformKey = capabilityPlatformKey ctx (SymbolKey.Type clsKey)

                        match
                            (if platformKey = SymbolKey.Type clsKey then
                                 ValueNone
                             else
                                 ctx.Provider.TryLookupMember(platformKey, memberName))
                        with
                        | ValueSome m when not m.IsStatic -> commitExternalMember m args
                        | _ ->
                            // The provider stack has NO shape for this key at all, because the
                            // package that minted it never had its HOME manifest stacked. Name
                            // the NAMESPACE; the owning package is a fact of a shape, and none resolved.
                            let clsNs = clsKey.Namespace.Dotted

                            match ctx.Provider.TryLookupType(SymbolKey.Type clsKey), clsNs with
                            | ValueNone, ns when ns <> "" ->
                                errorTy
                                    ctx
                                    memberTok
                                    (Kind.Message(
                                        sprintf
                                            "type '%s' is referenced from namespace '%s' but no package in the compilation declares it"
                                            clsSimple
                                            ns
                                    ))
                            | _ -> errorTy ctx memberTok (Kind.UnknownNominalType(NominalKind.Class, clsQual))
        | TyUnion(unionKey, args) ->
            match TypeRegistry.tryUnionByKey ctx.Types unionKey with
            | ValueSome info ->
                let (DisplayName shown) = SymbolKeyOps.typeSimpleName unionKey

                resolveLocalInstanceMember ctx memberTok shown info.TypeParams args info.Members memberName
            | ValueNone ->
                // Not project-local, so the union is the provider's (a referenced `Vesper.Option`, whose
                // `IsSome`/`IsNone`/`Value` augmentation members the contract provider publishes).
                let unionQual = SymbolKeyOps.typeMetaName unionKey

                match ctx.Provider.TryLookupMember(SymbolKey.Type unionKey, memberName) with
                | ValueSome m when not m.IsStatic -> commitExternalMember m args
                | _ ->
                    // The provider knows the union but not this member → a real
                    // member miss; otherwise the type itself is unknown.
                    match ctx.Provider.TryLookupType(SymbolKey.Type unionKey) with
                    | ValueSome(ExternalTypeShape.Union _) ->
                        errorTy ctx memberTok (Kind.NoMember(unionQual, MemberNoun.InstanceMember, memberName))
                    | _ -> errorTy ctx memberTok (Kind.UnknownNominalType(NominalKind.Union, unionQual))
        | TyVar tv ->
            let root = UnionFind.find ctx.Store tv

            // A typar object argument never grounds to a nominal, so the `Pda` park below would
            // never discharge (and the binding would not generalise). Resolve the member
            // now, through an interface the typar is coerced to (`'T :> IFace`).
            match tryTyparInterfaceMember ctx access.Key root memberName with
            | ValueSome ty -> ty
            | ValueNone ->
                let resultTv = freshTyVar ctx

                let access =
                    {
                        MemberName = memberName
                        Use = access
                        ResultTv = resultTv
                    }

                ctx.Store.Pda.Prepend(root, access)
                TyVar resultTv
        // An instance member on an intrinsic object argument whose `(# "…" #)` binding maps it to
        // a BCL type (`"hello".TryCopyTo(span)`), resolved through the provider under the
        // canonical BCL name. Single-pick: an arg-overloaded name needs the arg-aware path.
        | IntrinsicBclMember ctx memberName (declKey, args, m) ->
            if not m.IsStatic then
                let memberSig = ExternalSymbols.openSignature m (args.AsSpan().ToArray())

                ctx.Resolution.ExternalAccess.Set(
                    access.Key,
                    {
                        Key = SymbolKey.Member m.Key
                        IsStatic = false
                        Storage = m.Storage
                        Signature = memberSig
                        OptionalDefaults = m.OptionalDefaults
                    }
                )

                memberSig
            else
                errorTy
                    ctx
                    memberTok
                    (Kind.NoMember(SymbolKeyOps.qualifiedName declKey, MemberNoun.InstanceMember, memberName))
        // The intrinsic `'T[]` carries no member metadata, so `arr.Length` resolves to the
        // core `GetArrayLength` inline function (scheme `'T[] -> int`).
        | TyArray _ when memberName = "Length" ->
            match ctx.CoreAccess.Value.GetArrayLength with
            | ValueSome sym ->
                // Thread the resolved identity through so `InlineExpansion` can splice
                // the `ldlen` body by KEY.
                ctx.Resolution.IntrinsicKey.Set(access.Key, SymbolKey.Binding sym.Key)
                let resultTy = TyVar(freshTyVar ctx)

                unify
                    ctx
                    memberTok
                    (ExternalSymbols.instantiateSymbol ctx.Store sym ctx.CurrentLevel)
                    (TyFun(rTy, resultTy))

                resultTy
            | ValueNone -> errorTy ctx memberTok (Kind.IntrinsicNotInScope "Array 'Length' intrinsic 'GetArrayLength'")
        | _ ->
            errorTy
                ctx
                memberTok
                (Kind.Message(sprintf "Cannot read member '%s' from non-record non-class type" memberName))

    and inferFieldAccess
        (infer: Infer)
        (ctx: PassContext)
        (node: NodeSite)
        (objArg: Expr<SyntaxToken>)
        (fieldTok: SyntaxToken)
        : SemType =
        let rTy = infer ctx objArg
        resolveFieldStep ctx node fieldTok rTy

    /// `arr.[i]` — a rank-1 array `'T[]` object argument, an `int` index, the element type as
    /// result. The element stays a fresh var unified against it, so a bare `[]`
    /// object argument is pinned from context exactly as an array literal is.
    and inferIndexedLookup
        (infer: Infer)
        (ctx: PassContext)
        (node: NodeSite)
        (objArg: Expr<SyntaxToken>)
        (index: Expr<SyntaxToken>)
        : SemType =
        resolveIndexedStep ctx node (infer ctx objArg) (infer ctx index)

    /// The element type `x.[i]` reads, from the ALREADY-INFERRED operand types. An assignment
    /// LHS infers its operands itself, so the choice of accessor / intrinsic lives here rather
    /// than inside the inferring wrapper.
    and resolveIndexedStep (ctx: PassContext) (node: NodeSite) (objArgTy: SemType) (idxTy: SemType) : SemType =
        // `arr.[i]` resolves to the core `GetArray` inline function: instantiate its scheme
        // (`'T[] -> int -> 'T`) and unify against `arr -> idx -> result`. That pins element,
        // index and result, and grounds them so `InlineExpansion` splices the `ldelem`.
        let getArrayIndex () =
            match ctx.CoreAccess.Value.GetArray with
            | ValueSome sym ->
                // Thread the resolved `GetArray` identity through so the `ldelem` body
                // splices by KEY, under this same `IndexedLookup` node key.
                ctx.Resolution.IntrinsicKey.Set(node.Key, SymbolKey.Binding sym.Key)
                let resultTy = TyVar(freshTyVar ctx)

                unify
                    ctx
                    node.Tok
                    (ExternalSymbols.instantiateSymbol ctx.Store sym ctx.CurrentLevel)
                    (TyFun(objArgTy, TyFun(idxTy, resultTy)))

                resultTy
            | ValueNone -> errorTy ctx node.Tok (Kind.IntrinsicNotInScope "Array indexing intrinsic 'GetArray'")

        // String indexing (`s.[i]`) where the target has no BCL `string` metadata (JS) and
        // `get_Chars` does not resolve: `GetString` (scheme `string -> int -> char`) unifies
        // the object argument against `string`, not `'T[]` as the `GetArray` fallback would.
        let getStringIndex () =
            match ctx.CoreAccess.Value.GetString with
            | ValueSome sym ->
                // Thread the resolved `GetString` identity (see `getArrayIndex`).
                ctx.Resolution.IntrinsicKey.Set(node.Key, SymbolKey.Binding sym.Key)
                let resultTy = TyVar(freshTyVar ctx)

                unify
                    ctx
                    node.Tok
                    (ExternalSymbols.instantiateSymbol ctx.Store sym ctx.CurrentLevel)
                    (TyFun(objArgTy, TyFun(idxTy, resultTy)))

                resultTy
            | ValueNone -> getArrayIndex ()

        let stringOrArrayIndex () =
            match resolveStep ctx.Store objArgTy with
            | TyString -> getStringIndex ()
            | _ -> getArrayIndex ()

        // An indexer on an external object argument is its BCL `get_Item` accessor (`get_Chars`
        // for a `string` intrinsic), never `GetArray`/`ldelem`: resolve it through the
        // provider, record `ExternalAccess`, and return the ELEMENT type.
        let resolveExternalIndexer (declKey: SymbolKey) (clsArgs: SemType[]) (accessorName: string) : SemType voption =
            match ctx.Provider.TryLookupMember(declKey, accessorName) with
            | ValueSome m when not m.IsStatic ->
                let memberSig = ExternalSymbols.openSignature m clsArgs

                ctx.Resolution.ExternalAccess.Set(
                    node.Key,
                    {
                        Key = SymbolKey.Member m.Key
                        IsStatic = false
                        Storage = MemberStorage.Method
                        Signature = memberSig
                        // An indexer's accessor takes no omittable optionals.
                        OptionalDefaults = []
                    }
                )

                // The accessor is `idx -> ret`, and `ret` is by-ref (`Span<char>.get_Item :
                // T&`) or by-value (`string.get_Chars : char`), so the unify RHS must match.
                let retIsByref =
                    match memberSig with
                    | TyFun(_, TyByref _) -> true
                    | _ -> false

                let resultTy = TyVar(freshTyVar ctx)

                let rhsRet =
                    if retIsByref then
                        TyConst(RuntimeNames.byrefKey, EqArray.singleton resultTy)
                    else
                        resultTy

                unify ctx node.Tok memberSig (TyFun(idxTy, rhsRet))
                ValueSome resultTy
            | _ -> ValueNone

        // An index-signature object argument (`{ [k: K]: V }`) reads through the `GetIndex`
        // intrinsic, whose `$0[$1]` bracket form IS the accessor, so no `get_Item` exists.
        // `GetIndex`'s scheme `'T -> 'K -> 'V` has three INDEPENDENT typars.
        let tryIndexSignature (declKey: SymbolKey) (clsArgs: SemType[]) : SemType voption =
            match ctx.Provider.TryLookupIndexSignature declKey with
            | [] -> ValueNone
            | entries ->
                // Realise each entry's key/value template against the object argument's args
                // (`Dict<number>`'s value `'V` → `number`).
                let realised =
                    entries
                    |> List.map (fun (kF, vF) ->
                        FrozenTypeBridge.instantiateDeclaring kF clsArgs,
                        FrozenTypeBridge.instantiateDeclaring vF clsArgs
                    )

                // Select the entry whose key type matches the index expression's type. A
                // single entry is used as-is; an unresolved / non-matching index takes the first.
                let keyTy, valTy =
                    match realised with
                    | [ single ] -> single
                    | _ ->
                        let matched =
                            match resolveStep ctx.Store idxTy with
                            | TyConst(idxKey, _) ->
                                realised
                                |> List.tryFind (fun (k, _) ->
                                    match resolveStep ctx.Store k with
                                    | TyConst(kKey, _) -> kKey = idxKey
                                    | _ -> false
                                )
                            | _ -> None

                        match matched with
                        | Some e -> e
                        | None -> List.head realised

                match ctx.CoreAccess.Value.GetIndex with
                | ValueSome sym ->
                    // Thread the resolved `GetIndex` identity through so the `$0[$1]` body
                    // splices by KEY, under this same `IndexedLookup` node key.
                    ctx.Resolution.IntrinsicKey.Set(node.Key, SymbolKey.Binding sym.Key)
                    let resultTy = TyVar(freshTyVar ctx)

                    unify
                        ctx
                        node.Tok
                        (ExternalSymbols.instantiateSymbol ctx.Store sym ctx.CurrentLevel)
                        (TyFun(objArgTy, TyFun(idxTy, resultTy)))

                    // Pin `'K`/`'V` (which the generic scheme leaves free) to the declared
                    // key/value, so `x.[k]` reads the declared element type (`string |
                    // undefined`), not a fresh var.
                    unify ctx node.Tok idxTy keyTy
                    unify ctx node.Tok resultTy valTy
                    ValueSome resultTy
                | ValueNone ->
                    ValueSome(errorTy ctx node.Tok (Kind.IntrinsicNotInScope "Index-signature intrinsic 'GetIndex'"))

        // A declared `get_Item` read is a method call, which stamps no intrinsic key.
        match tryLocalInstanceMember ctx objArgTy AccessorNames.itemGetter with
        | ValueSome accessorTy ->
            let resultTy = TyVar(freshTyVar ctx)
            unify ctx node.Tok accessorTy (TyFun(idxTy, resultTy))
            resultTy
        | ValueNone ->
            match resolveStep ctx.Store objArgTy with
            | TyClass(clsKey, clsArgs) when (TypeRegistry.tryClassByKey ctx.Types clsKey).IsNone ->
                let clsArgsArr = clsArgs.AsSpan().ToArray()

                match tryIndexSignature (SymbolKey.Type clsKey) clsArgsArr with
                | ValueSome resultTy -> resultTy
                | ValueNone ->
                    match resolveExternalIndexer (SymbolKey.Type clsKey) clsArgsArr AccessorNames.itemGetter with
                    | ValueSome resultTy -> resultTy
                    | ValueNone -> getArrayIndex ()
            // A rank-1 array reads through the intrinsic array type's declared `get_Item`, the
            // member form of the free `GetArray`.
            | TyArray elem ->
                match
                    resolveExternalIndexer
                        (SymbolKeyOps.qualifiedTypeKey RuntimeNames.arrayContractName 0)
                        [| elem |]
                        AccessorNames.itemGetter
                with
                | ValueSome resultTy -> resultTy
                | ValueNone -> getArrayIndex ()
            | _ ->
                // An intrinsic object argument mapped to a BCL type, namely `string` (`s.[i]`),
                // whose indexer is `System.String.get_Chars(int) : char`. On JS no surface
                // publishes it, since `string`'s platform repr is the bare `"string"`.
                let charsIndexer (struct (declKey, clsArgs: EqArray<SemType>)) =
                    match resolveExternalIndexer declKey (clsArgs.AsSpan().ToArray()) "get_Chars" with
                    | ValueSome resultTy -> Some resultTy
                    | ValueNone -> None

                match externalSurfaceKeys ctx objArgTy |> List.tryPick charsIndexer with
                | Some resultTy -> resultTy
                | None -> stringOrArrayIndex ()

    /// `r.X.Y…` parsed as a single multi-segment `Expr.LongIdentOrOp`, whose
    /// anchor segment NameResolution resolved as a local binding; the remaining
    /// segments are a field-access chain.
    and inferLongIdentFieldChain (ctx: PassContext) (node: NodeSite) (li: LongIdent<SyntaxToken>) : SemType =
        let anchorKey = NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent

        let anchorTy =
            match ctx.Bindings.Binding.TryGetValue anchorKey with
            | ValueSome rb -> instantiateBinding ctx rb
            | ValueNone -> TyVar(freshTyVar ctx)

        let mutable currTy = anchorTy

        for i = 1 to li.Idents.Length - 1 do
            currTy <- resolveFieldStep ctx node li.Idents.[i] currTy

        currTy

    /// The type of a folded field chain MINUS its last segment, namely the object argument of a
    /// folded-LongIdent instance method call (`w.Write(arg)` parses with
    /// `fn = LongIdent [w; Write]`), so the last segment can be resolved arg-aware.
    and inferLongIdentPrefix (ctx: PassContext) (node: NodeSite) (li: LongIdent<SyntaxToken>) : SemType =
        let anchorKey = NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent

        let anchorTy =
            match ctx.Bindings.Binding.TryGetValue anchorKey with
            | ValueSome rb -> instantiateBinding ctx rb
            | ValueNone -> TyVar(freshTyVar ctx)

        let mutable currTy = anchorTy

        for i = 1 to li.Idents.Length - 2 do
            currTy <- resolveFieldStep ctx node li.Idents.[i] currTy

        currTy
