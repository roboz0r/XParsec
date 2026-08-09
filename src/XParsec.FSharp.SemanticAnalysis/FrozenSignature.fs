namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic

// In-memory projection of a FROZEN implementation file to an `IExternalSymbolProvider`,
// a file's *implicit signature*, so file N+1 resolves file N's exports by NAME with no DLL
// emitted. Keeps INTERNAL-or-better, where the `.fsi` contract extractor keeps public-only.

module FrozenSignature =

    /// Re-axis a binding's frozen `ValRepr` onto the `Declaring` axis its sibling scheme is
    /// remapped to. The grouping is untouched, since only typar leaves move axis.
    let private valReprToDeclaring
        (source: PoolBuilder)
        (pats: PoolBuilder)
        (vr: PooledValRepr)
        : TastAccessor.ValRepr =
        // The re-axised copy is a DERIVED tree belonging to no file, so a tuple group's
        // pattern is copied into the provider's own pool and a simple group's bound variable re-minted.
        TastConvert.valRepr
            ConformanceTypars.toDeclaringAxis
            (fun id ->
                {
                    Pool = pats
                    Id = TastPoolBuilder.copyPatTreeInto pats ConformanceTypars.toDeclaringAxis source id
                }
            )
            (fun _ -> TastPoolBuilder.mintBoundVar pats)
            vr

    /// Project a frozen implementation file's INTERNAL-or-better signature to a provider
    /// view. `producer` is the file `frozen` was analysed FROM: every anchor in every
    /// published template indexes that file's `Lexed`.
    let toProvider (producer: OriginSource) (frozen: FrozenPools) : IExternalSymbolProvider =
        let originIn (ns: NamespaceKey) : SymbolOrigin =
            {
                Home = Origin.InFile producer.File.Path
                Namespace = ns
            }

        // Internal-or-better: keep `Public` + `Internal`, drop `Private`. A key ABSENT from
        // the table is `Public` (an unmarked decl), so it is exported.
        let exported (key: SymbolKey) : bool =
            match frozen.Residue.Accessibility.TryGetValue key with
            | true, Accessibility.Private -> false
            | _ -> true

        // Type channels are addressed by the minted IDENTITY (a module-held type's `InModule`
        // key is a chain no source name spells); the by-name index is rendered from it.
        let shapesByKey = Dictionary<SymbolKey, ExternalTypeShape>()
        let membersByKey = Dictionary<SymbolKey, ResizeArray<ExternalMember>>()
        let typesByName = Dictionary<string, TypeKey>(System.StringComparer.Ordinal)

        let unionCaseIndex =
            Dictionary<string, ExternalUnionCase>(System.StringComparer.Ordinal)

        // A `field-name -> [records declaring it]` MULTIMAP, NOT first-wins: a field name is
        // deliberately shared across records, so each record ADDS its candidate to the bucket.
        let recordFieldIndex =
            Dictionary<string, ResizeArray<ExternalRecordCandidate>>(System.StringComparer.Ordinal)

        let symbols = Dictionary<string, ExternalSymbol>(System.StringComparer.Ordinal)

        // The DOTTED path of a module this file declares -> the `TypeContainer` a type it holds
        // sits in, populated from each registered type's containment chain: it is what makes
        // a written `Test.A.M.T` reach the type whose canonical name is `Test.A.M+T`.
        let moduleContainers =
            Dictionary<string, TypeContainer>(System.StringComparer.Ordinal)

        let rec registerModuleContainer (m: ModuleKey) =
            let path = SymbolKeyOps.moduleFullName m

            if not (moduleContainers.ContainsKey path) then
                moduleContainers.[path] <- TypeContainer.InModule m

            match m.Container with
            | ModuleContainer.InModule parent -> registerModuleContainer parent
            | ModuleContainer.InNamespace _ -> ()

        // --- member projection --------------------------------------------------------
        // A member's frozen `Params` / `ReturnTy` already carry the declaring type's typars as
        // `FTTypar(Declaring,i)` and its own as `FTTypar(Method,j)`, the axis convention here.
        let memberFromParts
            (declKey: TypeKey)
            (declArity: int)
            (name: string)
            (isValueMember: bool)
            (isStatic: bool)
            (methodArity: int)
            (parameters: FrozenType)
            (returnTy: FrozenType)
            : ExternalMember =
            let kind =
                if isValueMember then
                    MemberKind.Property
                else
                    MemberKind.Method

            let signature =
                ExternalSignature.make (declArity, methodArity, parameters, returnTy)

            // A value member interns an EMPTY argSig (no value parameters); a method interns
            // its tupled parameter signature, the structural overload identity.
            let argSig =
                if isValueMember then
                    EqArray.empty
                else
                    ExternalSymbols.argSigOfParameters parameters

            { ExternalMember.OfKey(SymbolKeyOps.memberKeyOf declKey name argSig methodArity kind) with
                IsStatic = isStatic
                Storage =
                    (if isValueMember then
                         MemberStorage.Property
                     else
                         MemberStorage.Method)
                Signature = signature
                MethodTyparArity = methodArity
                Origin = originIn declKey.Namespace
            }

        let memberOf (declKey: TypeKey) (declArity: int) (m: TastAccessor.TypeMember) : ExternalMember =
            let isValueMember = (m.Kind = TMemberKind.Property)
            let methodArity = m.MethodTypeParams.Length

            let parameters =
                if isValueMember then
                    ExternalSymbols.unitFrozen
                else
                    ExternalSymbols.tupledParams [| for (_, ty) in m.Params -> ty |]

            memberFromParts declKey declArity m.Name isValueMember m.IsStatic methodArity parameters m.ReturnTy

        // An interface's abstract method carries a single CURRIED `Signature`; a concrete
        // member carries decurried `Params` / `ReturnTy`. Peel ONE `->` to the `.NET`-tupled
        // domain (`unit` domain / no `->` ⇒ none); a property's `Signature` IS its value type.
        let abstractMemberOf (declKey: TypeKey) (declArity: int) (am: Frozen.TAbstractMethod) : ExternalMember =
            let methodArity = am.MethodTypeParams.Length

            let parameters, returnTy =
                if am.IsProperty then
                    ExternalSymbols.unitFrozen, am.Signature
                else
                    match am.Signature with
                    | FTFun(domain, codomain) -> domain, codomain
                    | other -> ExternalSymbols.unitFrozen, other

            memberFromParts declKey declArity am.Name am.IsProperty false methodArity parameters returnTy

        let membersOf
            (declKey: TypeKey)
            (declArity: int)
            (ms: EqArray<TastAccessor.TypeMember>)
            : ResizeArray<ExternalMember> =
            let acc = ResizeArray<ExternalMember>()

            // Member-level accessibility, on the same internal-or-better threshold: a
            // `member private` is not visible to another file, so it is dropped.
            for m in ms do
                if m.Accessibility <> Accessibility.Private then
                    acc.Add(memberOf declKey declArity m)

            acc

        // --- union case shape ---------------------------------------------------------
        let caseShapeOf (c: Frozen.TUnionCase) : ExternalCaseShape =
            {
                Name = c.Name
                FieldNames = [| for (n, _) in c.Fields -> n |]
                FrozenFieldTypes = [| for (_, ty) in c.Fields -> ty |]
            }

        // --- declarations -------------------------------------------------------------
        // The file's trees as columns; the projection never descends into a value position,
        // only decl SIGNATURES and the opaque type-declaration shape.
        let pool = TastPoolBuilder.openOver frozen

        let (|ExportedTypeDecl|_|) (d: TastAccessor.DeclId) : TastAccessor.TypeDecl option =
            match TastAccessor.declKind d with
            | DeclShape.Type ->
                let td = TastAccessor.declType d
                if exported td.Key then Some td else None
            | _ -> None

        for decl in TastAccessor.roots pool do
            match decl with
            | ExportedTypeDecl td ->
                let typeKey = td.TypeKey
                let key = SymbolKey.Type typeKey
                let arity = td.TypeParams.Length
                let origin = originIn typeKey.Namespace

                // Index the enclosing module chain so a written `A.M.T` for this type resolves
                // through containment. An `InType`-nested type contributes no module container.
                match typeKey.Container with
                | TypeContainer.InModule m -> registerModuleContainer m
                | TypeContainer.InNamespace _
                | TypeContainer.InType _ -> ()

                let register (shape: ExternalTypeShape) (members: ResizeArray<ExternalMember> voption) =
                    shapesByKey.[key] <- shape
                    // First declaration wins on a compiled-name collision.
                    let name = SymbolKeyOps.typeMetaName typeKey

                    if not (typesByName.ContainsKey name) then
                        typesByName.[name] <- typeKey

                    match members with
                    | ValueSome ms when ms.Count > 0 -> membersByKey.[key] <- ms
                    | _ -> ()

                let registerCases (cases: Frozen.TUnionCase seq) (caseShapes: ExternalCaseShape[]) =
                    for c, shape in Seq.zip cases caseShapes do
                        // First declaration wins on a bare case-name collision. An RQA union's
                        // cases carry the flag so a consumer's bare `Red` is rejected.
                        if not (unionCaseIndex.ContainsKey c.Name) then
                            unionCaseIndex.[c.Name] <-
                                {
                                    UnionName = SymbolKeyOps.typeMetaName typeKey
                                    TyparArity = arity
                                    Origin = origin
                                    Case = shape
                                    IsRequireQualifiedAccess = td.IsRequireQualifiedAccess
                                }

                match td.Kind with
                | TTypeKindG.Record(fields, members, _, _) ->
                    let fieldShapes =
                        [|
                            for f in fields ->
                                {
                                    Name = f.Name
                                    IsMutable = f.IsMutable
                                    Frozen = f.Type
                                }
                                : ExternalFieldShape
                        |]

                    register
                        (ExternalTypeShape.Record(arity, fieldShapes, origin))
                        (ValueSome(membersOf typeKey arity members))

                    // One candidate per record, appended to EVERY field's bucket, so a shared
                    // field name keeps both records live. An RQA record carries the flag so a
                    // consumer's bare `{ X = … }` literal excludes it from the field-set index.
                    let candidate: ExternalRecordCandidate =
                        {
                            TypeKey = typeKey
                            TyparArity = arity
                            Origin = origin
                            FieldNames = [| for f in fields -> f.Name |]
                            IsRequireQualifiedAccess = td.IsRequireQualifiedAccess
                        }

                    for f in fields do
                        match recordFieldIndex.TryGetValue f.Name with
                        | true, buf -> buf.Add candidate
                        | _ ->
                            let buf = ResizeArray<ExternalRecordCandidate>()
                            buf.Add candidate
                            recordFieldIndex.[f.Name] <- buf

                | TTypeKindG.Union(cases, members, _) ->
                    let caseArr = [| for c in cases -> c |]
                    let caseShapes = caseArr |> Array.map caseShapeOf

                    register
                        (ExternalTypeShape.Union(arity, caseShapes, [||], origin))
                        (ValueSome(membersOf typeKey arity members))

                    registerCases caseArr caseShapes

                | TTypeKindG.Class c ->
                    let members = membersOf typeKey arity c.Members

                    let shape: ExternalClassShape =
                        {
                            TyparArity = arity
                            IsInterface = false
                            Members = members.ToArray()
                            FrozenInterfaces =
                                [|
                                    for (ity, _) in c.Interfaces do
                                        match FrozenInterface.TryOfFrozen ity with
                                        | ValueSome i -> i
                                        | ValueNone -> ()
                                |]
                            FrozenBaseType = c.BaseType
                            Flags =
                                { ExternalClassFlags.Default with
                                    IsSealed = c.IsSealed
                                    IsValueType = (c.ValueKind <> ClassValueKind.RefType)
                                }
                            Origin = origin
                        }

                    register (ExternalTypeShape.Class shape) (ValueSome members)

                | TTypeKindG.Interface methods ->
                    // Uncurry each abstract method to an `ExternalMember` under the interface
                    // key, so a cross-file `interface F with member …` check resolves the slot.
                    let members = ResizeArray<ExternalMember>()

                    for am in methods do
                        members.Add(abstractMemberOf typeKey arity am)

                    let shape: ExternalClassShape =
                        {
                            TyparArity = arity
                            IsInterface = true
                            Members = members.ToArray()
                            FrozenInterfaces = [||]
                            FrozenBaseType = ValueNone
                            Flags = ExternalClassFlags.Default
                            Origin = origin
                        }

                    register (ExternalTypeShape.Class shape) (ValueSome members)

                | TTypeKindG.Enum cases ->
                    // Project the closed case→literal table to the shape a later file resolves
                    // `(x: E)` / `E.Ci` against; its nominal identity IS the registered `key`,
                    // so no case index is needed. A case with no literal is DROPPED.
                    let caseShapes =
                        [|
                            for c in cases do
                                match c.Value with
                                | ValueSome(TEnumLiteral.Int v) ->
                                    {
                                        Name = c.Name
                                        Value = ExternalEnumCaseValue.IntVal(snd (TEnumCases.integralValue v))
                                    }
                                    : ExternalEnumCaseShape
                                | ValueSome(TEnumLiteral.String s) ->
                                    {
                                        Name = c.Name
                                        Value = ExternalEnumCaseValue.StringVal s
                                    }
                                | ValueNone -> ()
                        |]

                    register (ExternalTypeShape.Enum(caseShapes, origin)) ValueNone

            | _ -> ()

        // --- module values + inline values --------------------------------------------
        // A pool of this provider's own, for the re-axised tuple-group patterns it hands out.
        let valReprPats = TastPoolBuilder.openEmpty ()

        // Every binding fact below is read at the bound variable ID the decl's own pattern carries.
        let bindingValReprs = DenseTable.index frozen.BindingValReprs
        let moduleMembers = DenseTable.index frozen.ModuleMembers

        let bindingValRepr (boundVar: BoundVarId) : TastAccessor.ValRepr voption =
            match bindingValReprs.TryGetValue boundVar with
            // A value has no lambda groups, so it publishes no `ValRepr`.
            | true, vr when not (List.isEmpty vr.Groups) -> ValueSome(valReprToDeclaring pool valReprPats vr)
            | _ -> ValueNone

        let addValue (bindingKey: BindingKey) (boundVar: BoundVarId) (ty: FrozenType) (inlineBody: InlineBody voption) =
            let scheme = ConformanceTypars.toDeclaringAxis ty

            let sym =
                { ExternalSymbols.scheme
                      bindingKey.Decl
                      bindingKey.Name
                      scheme
                      (FrozenPools.typarArity frozen boundVar)
                      [] with
                    Origin = originIn bindingKey.Decl.Namespace
                    ValRepr = bindingValRepr boundVar
                    InlineBody = inlineBody
                }

            symbols.[sym.Name] <- sym

        // The inline VOCABULARY, indexed by the bound variable it is published FOR: a template is a
        // second tree over the SAME source bound variable as the ordinary function.
        let inlineBodyOf =
            let d = Dictionary<BoundVarId, InlineBody>(frozen.InlineTemplates.Length)

            for iv in frozen.InlineTemplates do
                match { Pool = pool; Id = iv.Decl } with
                | TastAccessor.DLet {
                                        Pattern = TastAccessor.PNamed boundVar
                                    } ->
                    d.[boundVar] <- InlineBody.anchoredIn producer (TastPoolBuilder.declTree pool iv.Decl) iv.ParamAttrs
                | _ -> ()

            d

        // EVERY module binding rides `Decls`, `inline` ones included, and its identity is in
        // `ModuleMembers`, a TOP-LEVEL binding's too, keyed in the file's namespace so it
        // exports bare. A template is not a second entry here, but the binding's `InlineBody`.
        for decl in TastAccessor.roots pool do
            match decl with
            | TastAccessor.DLet {
                                    Pattern = TastAccessor.PNamed boundVar
                                    Ty = ty
                                } ->
                match moduleMembers.TryGetValue boundVar with
                | true, info ->
                    match info.Key with
                    | SymbolKey.Binding bindingKey when exported info.Key ->
                        let inlineBody =
                            match inlineBodyOf.TryGetValue boundVar with
                            | true, body -> ValueSome body
                            | _ -> ValueNone

                        addValue bindingKey boundVar ty inlineBody
                    | _ -> ()
                | _ -> ()
            | _ -> ()

        // --- intrinsic / primitive type shapes ----------------------------------------
        // An intrinsic-repr primitive (`type int = (# "System.Int32" #)`) is kept OUT of
        // `Decls`, so the loop above never sees it: it is published from `IntrinsicReprKeys`.
        for KeyValue(key, repr) in frozen.Residue.IntrinsicReprKeys do
            match key with
            | SymbolKey.Type typeKey ->
                // A HERITABLE `(# class … #)` primitive (`obj` / `exn`) also carries a class
                // surface, so a later file's `inherit` resolves it. No `BaseType`: the impl
                // `.fs` binds only the repr, never the parent nominal.
                let shape =
                    if repr.Heritable then
                        ExternalTypeShape.Intrinsic
                            {
                                Id =
                                    {
                                        Canon = typeKey
                                        TyparArity = typeKey.TyparArity
                                        Platform = IntrinsicPlatform.Repr repr.Platform
                                    }
                                Class = ValueSome { BaseType = ValueNone; Members = [||] }
                            }
                    else
                        ExternalTypeShape.Intrinsic(
                            IntrinsicShape.Scalar(typeKey, typeKey.TyparArity, IntrinsicPlatform.Repr repr.Platform)
                        )

                shapesByKey.[key] <- shape

                let name = SymbolKeyOps.typeMetaName typeKey

                if not (typesByName.ContainsKey name) then
                    typesByName.[name] <- typeKey
            | _ -> ()

        // --- intrinsic axes -----------------------------------------------------------
        // FORWARD is `{ canon -> platform-repr }`; REVERSE is its inversion, skipping a
        // degenerate self-map (a primitive with no distinct `.fs` repr).
        let intrinsicForward =
            let d = Dictionary<SymbolKey, string>(frozen.Residue.IntrinsicReprKeys.Count)

            for KeyValue(key, repr) in frozen.Residue.IntrinsicReprKeys do
                d.[key] <- repr.Platform

            d :> IReadOnlyDictionary<_, _>

        let intrinsicReverse =
            frozen.Residue.IntrinsicReprKeys
            |> Seq.choose (fun kv ->
                if kv.Value.Platform <> SymbolKeyOps.intrinsicName kv.Key then
                    Some(kv.Value.Platform, kv.Key)
                else
                    None
            )
            |> Seq.groupBy fst
            |> Seq.map (fun (platform, xs) -> platform, xs |> Seq.map snd |> Seq.distinct |> List.ofSeq)
            |> Map.ofSeq

        // --- provider assembly --------------------------------------------------------

        // Written type name -> the REGISTERED identity key. `typesByName` holds the canonical
        // rendering (`Test.A.M+T` for a module-held type); the fallback adds the DOTTED source
        // spelling (`Test.A.M.T`), resolved through the declared module containers.
        let resolveNameToKey (name: string) : TypeKey voption =
            let exact (probe: string) =
                match typesByName.TryGetValue probe with
                | true, key -> ValueSome key
                | _ -> ValueNone

            let moduleContainer (path: string) =
                match moduleContainers.TryGetValue path with
                | true, container -> ValueSome container
                | _ -> ValueNone

            SymbolKeyOps.tryDottedInModule exact moduleContainer name

        ExternalSymbolProviders.ofKeyedLeaf (
            ExternalSymbolProviders.KeyedLeaf.ofKeyIndexes
                { ExternalSymbolProviders.KeyIndexedLeaf.empty with
                    ShapesByKey = shapesByKey
                    MembersByKey = membersByKey
                    ResolveTypeName = resolveNameToKey
                    TryLookup =
                        fun name ->
                            match symbols.TryGetValue name with
                            | true, sym -> ValueSome sym
                            | _ -> ValueNone
                    TryLookupUnionCase =
                        fun caseName ->
                            match unionCaseIndex.TryGetValue caseName with
                            | true, hit -> ValueSome hit
                            | _ -> ValueNone
                    TryRecordsWithField =
                        fun fieldName ->
                            match recordFieldIndex.TryGetValue fieldName with
                            | true, buf -> buf.ToArray()
                            | _ -> [||]
                    // A frozen impl file publishes no `[<AutoOpen>]` surface: a later file in
                    // the SAME namespace reaches these types through its own header, not here.
                    AmbientOpenPrefixes = []
                    IntrinsicReverseCanon = intrinsicReverse
                    IntrinsicForwardRepr = intrinsicForward
                }
        )
