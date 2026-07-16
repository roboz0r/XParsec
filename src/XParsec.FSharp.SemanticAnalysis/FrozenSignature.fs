namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic

// In-memory projection of a FROZEN implementation file to an
// `IExternalSymbolProvider` — the compiler computing a file's *implicit signature*
// so file N+1 resolves file N's exports by NAME, with no DLL emitted. This is the
// enabling piece for multi-file compilation units (see
// `docs/multi-file-compilation-units-plan.md`).
//
// It is the intra-assembly twin of the two existing signature projections into the
// SAME provider surface: the `.fsi` contract extractor (`VesperLib.toProvider`, from
// signature SYNTAX) and the DLL metadata reader (`MetadataSymbols`, from emitted IL).
// It reuses the SAME provider backing (`ExternalSymbolProviders.ofKeyedLeaf`) and
// emits the SAME entry types, but reads a `Frozen.TastFile` and keeps
// INTERNAL-or-better accessibility (same-assembly visible) where the `.fsi` extractor
// keeps public-only — two thresholds over the ONE honestly-stored `Accessibility`
// fact.
//
// Home origin: a file-N entity is the SAME assembly as N+1, so every entry stamps
// `Assembly = Some assemblyName` (home) rather than a foreign `SymbolOrigin`. The
// compose-WITHOUT-re-origin layering (`stack ValueNone` / `composite`) that lets N+1
// mint the identical key is the ORCHESTRATOR's concern; this projector just stamps
// home origin on what it produces.

module FrozenSignature =

    /// Re-axis a binding's frozen `ValRepr` onto the `Declaring` axis, so its
    /// parameter / result types share the axis the sibling `Scheme` is remapped to
    /// (`toDeclaringAxis`) rather than the frozen decl's `Method` axis. This keeps the
    /// one symbol's two views consistent, and matches the `Declaring`-axis `ValRepr`
    /// the `.fsi` extractor mints (`TastLower.externalValRepr`). The grouping is
    /// untouched — only the embedded typar leaves move axis.
    let private valReprToDeclaring (vr: Frozen.ValRepr) : Frozen.ValRepr =
        { vr with
            Groups =
                vr.Groups
                |> List.map (
                    function
                    | ArgGroupG.GUnit ty -> ArgGroupG.GUnit(ConformanceTypars.toDeclaringAxis ty)
                    | ArgGroupG.GSimple(slot, ty) -> ArgGroupG.GSimple(slot, ConformanceTypars.toDeclaringAxis ty)
                    | ArgGroupG.GTuple pat -> ArgGroupG.GTuple(TastConvert.pat ConformanceTypars.toDeclaringAxis pat)
                )
            ResultTy = ConformanceTypars.toDeclaringAxis vr.ResultTy
        }

    /// Project a frozen implementation file's INTERNAL-or-better signature to a
    /// provider view. `assemblyName` is this unit's home assembly — a file-N entity is
    /// the same assembly as N+1, so it rides every entry's `Origin`.
    let toProvider (assemblyName: string) (frozen: Frozen.TastFile) : IExternalSymbolProvider =
        // Home origin for an entity in namespace `ns`: this frozen signature's assembly.
        let originIn (ns: NamespaceKey) : SymbolOrigin =
            {
                Home = Origin.InAssembly(AssemblyName assemblyName)
                Namespace = ns
            }

        // The internal-or-better threshold: keep `Public` + `Internal`, drop `Private`.
        // A key ABSENT from the honestly-stored table is `Public` (an unmarked decl),
        // so it is exported. The `.fsi` extractor thresholds the SAME fact public-only.
        let exported (key: SymbolKey) : bool =
            match frozen.Accessibility.TryGetValue key with
            | true, Accessibility.Private -> false
            | _ -> true

        // Type channels are addressed by the IDENTITY the front end minted (a module-
        // held type's `InModule` key is a chain no source name spells), with a by-name
        // index rendered from it for the resolver face — exactly the extractor's shape.
        let shapesByKey = Dictionary<SymbolKey, ExternalTypeShape>()
        let membersByKey = Dictionary<SymbolKey, ResizeArray<ExternalMember>>()
        let typesByName = Dictionary<string, SymbolKey>(System.StringComparer.Ordinal)

        let unionCaseIndex =
            Dictionary<string, ExternalUnionCase>(System.StringComparer.Ordinal)

        // The record analogue of `unionCaseIndex`: a `field-name -> [records declaring
        // it]` MULTIMAP (F#'s `eFieldLabels`). Unlike the union-case index this is NOT
        // first-wins — a field name is deliberately shared across records, so each record
        // ADDS its candidate to the bucket (the unqualified record-literal resolver
        // intersects the per-field buckets to pin the type).
        let recordFieldIndex =
            Dictionary<string, ResizeArray<ExternalRecordCandidate>>(System.StringComparer.Ordinal)

        let symbols = Dictionary<string, ExternalSymbol>(System.StringComparer.Ordinal)

        // --- member projection --------------------------------------------------------
        // A type member's frozen `Params` / `ReturnTy` already carry the declaring
        // type's typars as `FTTypar(Declaring,i)` and its own as `FTTypar(Method,j)`
        // (`Elaborate.freezeKind` / `remapMemberTypes`), which is the exact axis
        // convention `ExternalSignature` speaks — no remap here.
        let memberOf (declKey: TypeKey) (declArity: int) (m: Frozen.TTypeMember) : ExternalMember =
            let paramTys = [| for (_, ty) in m.Params -> ty |]

            let kind =
                match m.Kind with
                | TMemberKind.Method -> MemberKind.Method
                | TMemberKind.Property -> MemberKind.Property

            let isValueMember = (m.Kind = TMemberKind.Property)
            let methodArity = GeneralizedTypars.count m.MethodTypeParams

            let parameters =
                if isValueMember then
                    ExternalSymbols.unitFrozen
                else
                    ExternalSymbols.tupledParams paramTys

            let signature =
                ExternalSignature.make (declArity, methodArity, parameters, m.ReturnTy)

            // A value member interns an EMPTY argSig (no value parameters); a method
            // interns its tupled parameter signature — the same structural overload
            // identity `ExternalSymbols.argSigOfParameters` mints for every producer.
            let argSig =
                if isValueMember then
                    EqArray.empty
                else
                    ExternalSymbols.argSigOfParameters parameters

            { ExternalMember.OfKey(SymbolKeyOps.memberKeyOf declKey m.Name argSig methodArity kind) with
                IsStatic = m.IsStatic
                Storage =
                    (if isValueMember then
                         MemberStorage.Property
                     else
                         MemberStorage.Method)
                Signature = signature
                MethodTyparArity = methodArity
                Origin = originIn declKey.Namespace
            }

        let membersOf
            (declKey: TypeKey)
            (declArity: int)
            (ms: EqArray<Frozen.TTypeMember>)
            : ResizeArray<ExternalMember> =
            let acc = ResizeArray<ExternalMember>()

            for m in ms do
                acc.Add(memberOf declKey declArity m)

            acc

        // --- union case shape ---------------------------------------------------------
        let caseShapeOf (c: Frozen.TUnionCase) : ExternalCaseShape =
            {
                Name = c.Name
                FieldNames = [| for (n, _) in c.Fields -> n |]
                FrozenFieldTypes = [| for (_, ty) in c.Fields -> ty |]
            }

        // --- type declarations --------------------------------------------------------
        for decl in frozen.Decls do
            match decl with
            | Frozen.TDecl.Type td when exported td.Key ->
                let typeKey = td.TypeKey
                let key = SymbolKey.Type typeKey
                let arity = td.TypeParams.Length
                let origin = originIn typeKey.Namespace

                let register (shape: ExternalTypeShape) (members: ResizeArray<ExternalMember> voption) =
                    shapesByKey.[key] <- shape
                    // First declaration wins on a compiled-name collision (same rule as
                    // the extractor's short-name index).
                    let name = SymbolKeyOps.typeMetaName typeKey

                    if not (typesByName.ContainsKey name) then
                        typesByName.[name] <- key

                    match members with
                    | ValueSome ms when ms.Count > 0 -> membersByKey.[key] <- ms
                    | _ -> ()

                let registerCases (cases: Frozen.TUnionCase seq) (caseShapes: ExternalCaseShape[]) =
                    for c, shape in Seq.zip cases caseShapes do
                        // First declaration wins on a bare case-name collision; RQA is
                        // not modelled in the frozen tree, so a case resolves bare.
                        if not (unionCaseIndex.ContainsKey c.Name) then
                            unionCaseIndex.[c.Name] <-
                                {
                                    UnionName = SymbolKeyOps.typeMetaName typeKey
                                    TyparArity = arity
                                    Origin = origin
                                    Case = shape
                                    IsRequireQualifiedAccess = false
                                }

                match td.Kind with
                | Frozen.TTypeKind.Record(fields, members, _, _) ->
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

                    // One candidate per record, appended to EVERY field's bucket (the
                    // multimap append — a shared field name keeps both records live).
                    // Only internal-or-better exported records reach here (`when exported
                    // td.Key`), so accessibility is already filtered. RQA is hardcoded
                    // `false`: the frozen tree does not model record RQA (same gap as the
                    // union-case index above), so a cross-unit `[<RequireQualifiedAccess>]`
                    // record is wrongly constructible bare until RQA is threaded through
                    // freeze (plan R6).
                    let candidate: ExternalRecordCandidate =
                        {
                            RecordName = SymbolKeyOps.typeMetaName typeKey
                            TyparArity = arity
                            Origin = origin
                            FieldNames = [| for f in fields -> f.Name |]
                            IsRequireQualifiedAccess = false
                        }

                    for f in fields do
                        match recordFieldIndex.TryGetValue f.Name with
                        | true, buf -> buf.Add candidate
                        | _ ->
                            let buf = ResizeArray<ExternalRecordCandidate>()
                            buf.Add candidate
                            recordFieldIndex.[f.Name] <- buf

                | Frozen.TTypeKind.Union(cases, members, _) ->
                    let caseArr = [| for c in cases -> c |]
                    let caseShapes = caseArr |> Array.map caseShapeOf

                    register
                        (ExternalTypeShape.Union(arity, caseShapes, [||], origin))
                        (ValueSome(membersOf typeKey arity members))

                    registerCases caseArr caseShapes

                | Frozen.TTypeKind.Class c ->
                    // Directly-implemented interfaces as `(compiled-name, type-args)`
                    // pairs — the frozen interface type is a nominal head whose key
                    // renders the name and whose args carry the declaring typars.
                    let ifaceOf (ity: FrozenType) : (string * FrozenType[]) option =
                        match ity with
                        | FTClass(k, args)
                        | FTUnion(k, args)
                        | FTRecord(k, args) -> Some(SymbolKeyOps.typeMetaName k, [| for a in args -> a |])
                        | FTConst(k, args) -> Some(SymbolKeyOps.qualifiedName k, [| for a in args -> a |])
                        | _ -> None

                    let members = membersOf typeKey arity c.Members

                    let shape: ExternalClassShape =
                        {
                            TyparArity = arity
                            IsInterface = false
                            Members = members.ToArray()
                            FrozenInterfaces =
                                [|
                                    for (ity, _) in c.Interfaces do
                                        match ifaceOf ity with
                                        | Some p -> p
                                        | None -> ()
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

                | Frozen.TTypeKind.Interface _ ->
                    // Abstract-method surfaces (single curried `Signature` FrozenTypes)
                    // are not decurried here yet — the interface's name + arity + kind
                    // are published so a reference resolves, its member set deferred.
                    register (ExternalTypeShape.Class(ExternalClassShape.basic (arity, true, origin))) ValueNone

                | Frozen.TTypeKind.Enum _ ->
                    // Enum-case literal projection (`TConstValue` → `ExternalEnumCaseValue`)
                    // is deferred; an enum type is left unregistered (a consumer falls back
                    // to nominal `TyConst`) rather than published as a body-less residue.
                    ()

            | _ -> ()

        // --- module values + inline values --------------------------------------------
        // The scheme axis: a module binding has no enclosing generic type, so its frozen
        // typars are the single `Method` axis — remap to `Declaring` (the axis
        // `ExternalSymbol.Scheme` / `instantiateDeclaring` require) via the shared
        // `ConformanceTypars.toDeclaringAxis`.
        let bindingValRepr (k: NodeKey) : Frozen.ValRepr voption =
            match Map.tryFind k frozen.BindingValReprs with
            // A value has no lambda groups — `ValueNone`, exactly as the extractor
            // leaves `ValRepr` on a non-function `val`.
            | Some vr when not (List.isEmpty vr.Groups) -> ValueSome(valReprToDeclaring vr)
            | _ -> ValueNone

        let bindingArity (k: NodeKey) : int =
            match Map.tryFind k frozen.BindingTyparArities with
            | Some n -> n
            | None -> 0

        let addValue (bindingKey: BindingKey) (k: NodeKey) (ty: FrozenType) (inlineBody: InlineBody voption) =
            let scheme = ConformanceTypars.toDeclaringAxis ty

            let sym =
                { ExternalSymbols.scheme bindingKey.Decl bindingKey.Name scheme (bindingArity k) [] with
                    Origin = originIn bindingKey.Decl.Namespace
                    ValRepr = bindingValRepr k
                    InlineBody = inlineBody
                }

            symbols.[sym.Name] <- sym

        // Non-inline module functions / values survive in `Decls`; their identity is in
        // `ModuleMembers` (a top-level binding has none, and is never exported).
        for decl in frozen.Decls do
            match decl with
            | Frozen.TDecl.Let(Frozen.TPat.NamedSimple(k, _, _), _, _, ty) ->
                match Map.tryFind k frozen.ModuleMembers with
                | Some info ->
                    match info.Key with
                    | SymbolKey.Binding bindingKey when exported info.Key -> addValue bindingKey k ty ValueNone
                    | _ -> ()
                | None -> ()
            | _ -> ()

        // Inline vocabulary rides `InlineBodies` (Freeze partitioned it out of `Decls`).
        // The body is the frozen, sibling-rewritten template `Inline.thawBody` splices.
        for iv in frozen.InlineBodies do
            match iv.Key with
            | SymbolKey.Binding bindingKey when exported iv.Key ->
                match iv.Body.Decl with
                | Frozen.TDecl.Let(Frozen.TPat.NamedSimple(k, _, _), _, _, ty) ->
                    addValue bindingKey k ty (ValueSome iv.Body)
                | _ -> ()
            | _ -> ()

        // --- intrinsic axes -----------------------------------------------------------
        // The FORWARD `{ canon -> platform-repr }` axis IS this unit's own
        // `IntrinsicReprKeys` (identity-keyed, the frozen face). The REVERSE
        // `{ platform-repr -> [canon] }` is its inversion; a degenerate self-map (a
        // primitive with no distinct `.fs` repr) is skipped, mirroring the extractor.
        let intrinsicForward = frozen.IntrinsicReprKeys

        let intrinsicReverse =
            frozen.IntrinsicReprKeys
            |> Seq.choose (fun kv ->
                if kv.Value <> SymbolKeyOps.intrinsicName kv.Key then
                    Some(kv.Value, kv.Key)
                else
                    None
            )
            |> Seq.groupBy fst
            |> Seq.map (fun (platform, xs) -> platform, xs |> Seq.map snd |> Seq.distinct |> List.ofSeq)
            |> Map.ofSeq

        // --- provider assembly --------------------------------------------------------
        let typeShapeByKey (key: SymbolKey) : ExternalTypeShape voption =
            match shapesByKey.TryGetValue key with
            | true, s -> ValueSome s
            | _ -> ValueNone

        let typeMembersByKey (key: SymbolKey) : ResizeArray<ExternalMember> voption =
            match membersByKey.TryGetValue key with
            | true, ms -> ValueSome ms
            | _ -> ValueNone

        let typeShapeByName (name: string) : ExternalTypeShape voption =
            match typesByName.TryGetValue name with
            | true, key -> typeShapeByKey key
            | _ -> ValueNone

        ExternalSymbolProviders.ofKeyedLeaf (
            ExternalSymbolProviders.KeyedLeaf.ofNamedWithMembers
                { ExternalSymbolProviders.NamedLeaf.empty with
                    TryLookup =
                        fun name ->
                            match symbols.TryGetValue name with
                            | true, sym -> ValueSome sym
                            | _ -> ValueNone
                    TryLookupType = typeShapeByName
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
                    // A frozen impl unit publishes no `[<AutoOpen>]` surface (yet).
                    AmbientOpenPrefixes = []
                    IntrinsicReverseCanon = intrinsicReverse
                    IntrinsicForwardRepr = intrinsicForward
                }
                typeShapeByKey
                typeMembersByKey
        )
