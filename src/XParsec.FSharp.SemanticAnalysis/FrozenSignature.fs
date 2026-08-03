namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic

// In-memory projection of a FROZEN implementation file to an
// `IExternalSymbolProvider` — the compiler computing a file's *implicit signature*
// so file N+1 resolves file N's exports by NAME, with no DLL emitted. This is what
// makes a multi-file assembly compile as one.
//
// It is the intra-assembly twin of the two existing signature projections into the
// SAME provider surface: the `.fsi` contract extractor (`VesperLib.toProvider`, from
// signature SYNTAX) and the DLL metadata reader (`MetadataSymbols`, from emitted IL).
// It reuses the SAME provider backing (`ExternalSymbolProviders.ofKeyedLeaf`) and
// emits the SAME entry types, but reads a frozen file's `FrozenPools` and keeps
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
    let private valReprToDeclaring
        (source: PoolBuilder)
        (pats: PoolBuilder)
        (vr: PooledValRepr)
        : TastAccessor.ValRepr =
        // The `ValRepr` traversal is `TastConvert`'s — the same one the freeze and the
        // pool build run — at the axis re-map for both the embedded types and the tuple
        // groups' pattern trees, so the grouping cannot drift from the shape it maps.
        // A tuple group's pattern is a node of the SOURCE file's pool, and the re-axised
        // copy is a DERIVED tree belonging to no file, so it is copied column-to-column
        // into the provider's own pool, exactly where an `.fsi`-minted pattern lands. A
        // simple group's slot lands there too: it named this file's binder pool, which the
        // consumer cannot index, so it is re-minted exactly as the extractor mints one.
        TastConvert.valRepr
            ConformanceTypars.toDeclaringAxis
            (fun id ->
                {
                    Pool = pats
                    Id = TastPoolBuilder.copyPatTreeInto pats ConformanceTypars.toDeclaringAxis source id
                }
            )
            (fun _ -> TastPoolBuilder.mintBinder pats)
            vr

    /// Project a frozen implementation file's INTERNAL-or-better signature to a
    /// provider view. `assemblyName` is this file's home assembly — a file-N entity is
    /// the same assembly as N+1, so it rides every entry's `Origin`.
    ///
    /// `producer` is the file `frozen` was analysed FROM, retained. Every anchor in every
    /// template published below is an index into that file's `Lexed`, so taking it as an
    /// argument is what makes the pairing a fact of the call rather than something a caller
    /// has to remember while it still has the parse in hand.
    let toProvider (assemblyName: string) (producer: OriginSource) (frozen: FrozenPools) : IExternalSymbolProvider =
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
            match frozen.Residue.Accessibility.TryGetValue key with
            | true, Accessibility.Private -> false
            | _ -> true

        // Type channels are addressed by the IDENTITY the front end minted (a module-
        // held type's `InModule` key is a chain no source name spells), with a by-name
        // index rendered from it for the resolver view — exactly the extractor's shape.
        let shapesByKey = Dictionary<SymbolKey, ExternalTypeShape>()
        let membersByKey = Dictionary<SymbolKey, ResizeArray<ExternalMember>>()
        let typesByName = Dictionary<string, TypeKey>(System.StringComparer.Ordinal)

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

        // The frozen twin of `ExtractCtx.ModuleHolders`: the DOTTED path of a module this
        // file declares -> the `TypeHolder` a type it holds sits in. A frozen impl carries
        // no standalone module decls (a module is implicit in its types' holder keys), so
        // this is populated from the containment chain of each registered type. It exists so
        // a WRITTEN dotted name for a module-held type (`Test.A.M.T`, whose canonical
        // `typeMetaName` spells `Test.A.M+T`) resolves through the declared containment via
        // the shared `SymbolKeyOps.tryDottedModuleHeld` — the same canonicalizer the `.fsi`
        // provider's `tryTypeKey` uses — instead of missing the dotted-vs-`+` divide.
        //
        // A module's index path is its COMPILED chain (`moduleFullName`), which equals the
        // SOURCE spelling for an un-suffixed module — what a use site writes. A
        // `[<CompilationRepresentation(ModuleSuffix)>]`/name-collision `…Module` module would
        // diverge (the `.fsi` extractor keys the source path because it still holds the
        // source names; the frozen tree has only the compiled chain), the same extension
        // point as an `InType`-nested written name below.
        let moduleHolders = Dictionary<string, TypeHolder>(System.StringComparer.Ordinal)

        let rec registerModuleHolder (m: ModuleKey) =
            let path = SymbolKeyOps.moduleFullName m

            if not (moduleHolders.ContainsKey path) then
                moduleHolders.[path] <- TypeHolder.InModule m

            match m.Holder with
            | ModuleHolder.InModule parent -> registerModuleHolder parent
            | ModuleHolder.InNamespace _ -> ()

        // --- member projection --------------------------------------------------------
        // A type member's frozen `Params` / `ReturnTy` already carry the declaring
        // type's typars as `FTTypar(Declaring,i)` and its own as `FTTypar(Method,j)`
        // (`Elaborate.freezeTypars`), which is the exact axis
        // convention `ExternalSignature` speaks — no remap here.
        // The shared `ExternalMember` mint: an already-`.NET`-tupled `parameters` form +
        // return, folded into the overload identity + signature every producer speaks. A
        // concrete type member (`memberOf`) and an interface's abstract method
        // (`abstractMemberOf`) both reduce to these parts, so a slot and the class impl
        // that satisfies it mint the SAME `argSig` from ONE uncurry/axis home.
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

            // A value member interns an EMPTY argSig (no value parameters); a method
            // interns its tupled parameter signature — the same structural overload
            // identity `ExternalSymbols.argSigOfParameters` mints for every producer.
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
        // member carries decurried `Params` / `ReturnTy`. The typar cut already landed the
        // signature's leaves on the Declaring / Method axis at freeze
        // (`Elaborate.freezeTypars` runs the decl's cut over each `Signature`), so no re-axis
        // is needed — only the `->` is peeled. Peel ONE `->` to the `.NET`-tupled domain
        // (`argSigOfParameters` re-flattens a tuple domain to one arg per element, so a
        // 2-arg `a * b -> c` folds to the same slot a `member _.M(a, b)` impl mints; a
        // `unit` domain / no `->` ⇒ no value parameters). A property's `Signature` IS its
        // value type.
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

            // Honour member-level accessibility on the SAME internal-or-better
            // threshold `exported` applies to top-level entities: a `member private`
            // is not visible to another file, so it is dropped from the projection (an
            // `internal` / public member stays — same-assembly visible). Without this a
            // cross-file `receiver.PrivateMember` would wrongly resolve (OVER-PERMISSIVE).
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
        // The file's trees as columns. Nothing here mints, so the overlay stays empty and
        // this is purely the read seam; the projection never descends into a value
        // position, only decl HEADS and the opaque type-declaration shape.
        let pool = TastPoolBuilder.openOver frozen

        /// A `Type` root whose declaration passes the export threshold — the entire input
        /// of the type-shape projection, so the threshold is applied in one place rather
        /// than as a guard on each arm.
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

                // Index the enclosing module chain so a written `A.M.T` for this type
                // resolves through containment. An `InType`-nested type contributes no module
                // holder — a written `Outer.Inner` cross-file name is the extension point.
                match typeKey.Holder with
                | TypeHolder.InModule m -> registerModuleHolder m
                | TypeHolder.InNamespace _
                | TypeHolder.InType _ -> ()

                let register (shape: ExternalTypeShape) (members: ResizeArray<ExternalMember> voption) =
                    shapesByKey.[key] <- shape
                    // First declaration wins on a compiled-name collision (same rule as
                    // the extractor's short-name index).
                    let name = SymbolKeyOps.typeMetaName typeKey

                    if not (typesByName.ContainsKey name) then
                        typesByName.[name] <- typeKey

                    match members with
                    | ValueSome ms when ms.Count > 0 -> membersByKey.[key] <- ms
                    | _ -> ()

                let registerCases (cases: Frozen.TUnionCase seq) (caseShapes: ExternalCaseShape[]) =
                    for c, shape in Seq.zip cases caseShapes do
                        // First declaration wins on a bare case-name collision. An RQA
                        // union's cases carry the flag so a consumer's bare `Red` is
                        // rejected (`ExternalUnionCase.ResolvesWith`).
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

                    // One candidate per record, appended to EVERY field's bucket (the
                    // multimap append — a shared field name keeps both records live).
                    // Only internal-or-better exported records reach here (`when exported
                    // td.Key`), so accessibility is already filtered. An RQA record
                    // carries the flag so a consumer's bare `{ X = … }` literal excludes
                    // it from the field-set index (`InferResolve.admitsBareExternalRecord`).
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

                | TTypeKindG.Interface methods ->
                    // Uncurry each abstract method to an `ExternalMember` under the
                    // interface key (mirroring the `Class` arm's `membersOf`, via the
                    // shared `memberFromParts`), so a cross-file `interface F with member
                    // …` conformance check and a `receiver.M` dispatch both resolve the
                    // slot. An interface inherits no base and carries no `interface`
                    // clause on the frozen tree, so base / interfaces stay empty.
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
                    // Project the closed case→literal table to an `ExternalTypeShape.Enum`,
                    // the shape a later file resolves `(x: E)` / `E.Ci` against — its nominal
                    // identity IS the registered `key`, so no case index is needed (the
                    // enum-case resolver scans shapes: `TypeHeadStamp.tryExternalEnumCaseKey`).
                    // A case whose literal failed to resolve (`ValueNone`) is DROPPED — it has
                    // no value to be referenced by, mirroring the TS-manifest arm's computed-
                    // member drop and `TEnumCases.classify`'s treatment of an unresolved case
                    // as absent. The integral WIDTH is intentionally not carried:
                    // `ExternalEnumCaseValue` has none (external enums are a JS-target feature,
                    // never CLR codegen; the underlying integral type is the frozen-layer
                    // default), so a numeric case reduces to its `int64` value.
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

                    // An enum carries no augmentation members (`TTypeKindG.members` is empty
                    // for it), so it registers with a shape only.
                    register (ExternalTypeShape.Enum(caseShapes, origin)) ValueNone

            | _ -> ()

        // --- module values + inline values --------------------------------------------
        // The scheme axis: a module binding has no enclosing generic type, so its frozen
        // typars are the single `Method` axis — remap to `Declaring` (the axis
        // `ExternalSymbol.Scheme` / `instantiateDeclaring` require) via the shared
        // `ConformanceTypars.toDeclaringAxis`.
        // The re-axised tuple-group patterns this provider hands out, in one pool it
        // owns: they are derived from the frozen file's, not nodes of it.
        let valReprPats = TastPoolBuilder.openEmpty ()

        // Every binding fact below is read at the binder ID the decl's own head pattern
        // carries — a template and the ordinary function it was snapshotted from are two
        // trees over the SAME source binder, so the one id serves both loops below.
        let bindingValReprs = DenseTable.index frozen.BindingValReprs
        let moduleMembers = DenseTable.index frozen.ModuleMembers

        let bindingValRepr (binder: BinderId) : TastAccessor.ValRepr voption =
            match bindingValReprs.TryGetValue binder with
            // A value has no lambda groups — `ValueNone`, exactly as the extractor
            // leaves `ValRepr` on a non-function `val`.
            | true, vr when not (List.isEmpty vr.Groups) -> ValueSome(valReprToDeclaring pool valReprPats vr)
            | _ -> ValueNone

        let addValue (bindingKey: BindingKey) (binder: BinderId) (ty: FrozenType) (inlineBody: InlineBody voption) =
            let scheme = ConformanceTypars.toDeclaringAxis ty

            let sym =
                { ExternalSymbols.scheme
                      bindingKey.Decl
                      bindingKey.Name
                      scheme
                      (FrozenPools.typarArity frozen binder)
                      [] with
                    Origin = originIn bindingKey.Decl.Namespace
                    ValRepr = bindingValRepr binder
                    InlineBody = inlineBody
                }

            symbols.[sym.Name] <- sym

        // The inline VOCABULARY, indexed by the binder it is published FOR — the join key
        // for the one registration loop below.
        //
        // A template rides its OWN pool root: a second, independent tree, not a projection
        // of the emitted function of the same name (it is snapshotted ahead of the
        // expansion walk, so its static-opt clauses and trait calls resolve against a CALL
        // SITE's operand types). But it is a tree over the SAME source binder, which is
        // what lets the two meet here rather than as two passes over one dictionary where
        // the later write happened to win. What ships is the DU unpool of the root:
        // `InlineBody` is the cross-file wire (`InlineThaw.bodyAtOrigin` realises it in
        // another file), and a pool id is meaningless outside the file that issued it.
        let inlineBodyOf =
            let d = Dictionary<BinderId, InlineBody>(frozen.InlineTemplates.Length)

            for iv in frozen.InlineTemplates do
                match { Pool = pool; Id = iv.Decl } with
                | TastAccessor.DLet { Binding = TastAccessor.PNamed binder } ->
                    d.[binder] <- InlineBody.anchoredIn producer (TastPoolBuilder.declTree pool iv.Decl) iv.ParamAttrs
                | _ -> ()

            d

        // EVERY module binding rides `Decls`, `inline` ones included (an inline binding is
        // emitted as an ordinary module function as well as published as a template), and
        // its identity is in `ModuleMembers` — including a TOP-LEVEL binding's, which is
        // keyed in the file's namespace and so exports under its bare name. A binding with
        // no entry there binds no single name (a destructuring `let`) and has nothing to
        // export. So ONE loop registers them all, and a template is not a second entry that
        // overwrites the first: it is the binding's `InlineBody`, looked up at the binder
        // the decl already carries.
        //
        // That also means ONE export threshold, applied to `ModuleBindingInfo.Key`. The
        // template's own `PooledInlineValue.Key` is minted from the same place
        // (`Freeze.toFrozenFile`), so consulting it separately only created somewhere for
        // the two to disagree.
        for decl in TastAccessor.roots pool do
            match decl with
            | TastAccessor.DLet {
                                    Binding = TastAccessor.PNamed binder
                                    Ty = ty
                                } ->
                match moduleMembers.TryGetValue binder with
                | true, info ->
                    match info.Key with
                    | SymbolKey.Binding bindingKey when exported info.Key ->
                        let inlineBody =
                            match inlineBodyOf.TryGetValue binder with
                            | true, body -> ValueSome body
                            | _ -> ValueNone

                        addValue bindingKey binder ty inlineBody
                    | _ -> ()
                | _ -> ()
            | _ -> ()

        // --- intrinsic / primitive type shapes ----------------------------------------
        // An intrinsic-repr primitive (`type int = (# "System.Int32" #)`) is an
        // `ILIntrinsic` abbrev, kept OUT of `Decls` — the type-decl loop above never
        // sees it. Its identity + repr ride `IntrinsicReprKeys` (a home file's key IS its
        // contract-stamped canon). Publish each as an `ExternalTypeShape.Intrinsic` — the
        // SAME nominal shape the `.fsi` extractor mints (`VesperLib.registerIntrinsic`) —
        // so a later file's `unit` / `int` / `obj` annotation resolves the name and its
        // canon reconciles through `TryLookupType key` (`EngineCore.canonKey` tier 2).
        // `platform` is always `Some`: a home file holds its own `(# … #)` repr. A
        // HERITABLE `(# class … #)` primitive (`obj` / `exn` / `Attribute`) additionally
        // carries a class surface so a later file's `inherit` resolves it
        // (`resolveInheritParent`'s `Class = ValueSome` probe). Its `BaseType` is
        // `ValueNone` — the impl `.fs` binds only the repr, never the parent nominal
        // (`obj`), and codegen chains the base-`.ctor` off the canon's own repr, not this
        // field; its ctor set is empty (the impl declares none, and the inherit path reads
        // only the `Class` marker). A scalar primitive projects with `Class = ValueNone`.
        for KeyValue(key, repr) in frozen.Residue.IntrinsicReprKeys do
            match key with
            | SymbolKey.Type typeKey ->
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
        // The FORWARD `{ canon -> platform-repr }` axis is just this file's own frozen,
        // identity-keyed `IntrinsicReprKeys` in repr form; heritability rides the published
        // `Class` surface above, not this axis. The REVERSE `{ platform-repr -> [canon] }` is
        // its inversion; a degenerate self-map (a primitive with no distinct `.fs` repr) is
        // skipped, mirroring the extractor.
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

        // Written type name -> the REGISTERED identity key, through the ONE shared
        // containment canonicalizer. `typesByName` holds the canonical `typeMetaName`
        // rendering (`Test.A.M+T` for a module-held type); `tryDottedModuleHeld` adds the
        // fallback for the spelling that is NOT that rendering — the DOTTED source form
        // (`Test.A.M.T`), resolved through the declared module holders. It is this leaf's
        // ONE name->key seam: `ofKeyIndexes` derives the whole by-name type lookup from it,
        // so a use site stamps the producer's own key rather than a flattened re-cut — which
        // is why the store view reads the key-addressed index directly (no re-cut key ever
        // arrives).
        let resolveNameToKey (name: string) : TypeKey voption =
            let exact (probe: string) =
                match typesByName.TryGetValue probe with
                | true, key -> ValueSome key
                | _ -> ValueNone

            let moduleHolder (path: string) =
                match moduleHolders.TryGetValue path with
                | true, holder -> ValueSome holder
                | _ -> ValueNone

            SymbolKeyOps.tryDottedModuleHeld exact moduleHolder name

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
                    // A frozen impl file publishes no `[<AutoOpen>]` surface, so it contributes
                    // no ambient. `AmbientOpenPrefixes` means the implicit PRELUDE — a
                    // package's `[<AutoOpen>]` modules plus its manifest namespace
                    // (`ReferencedProject.wrap`), i.e. `open Vesper.ArithmeticOperators; open
                    // Vesper` — and a producer never gets to say "open me" beyond that.
                    //
                    // A later file in the SAME namespace reaches this file's namespace-direct
                    // types by BARE name through its OWN scope, not through anything published
                    // here: its `namespace N` header implicitly opens `N`
                    // (`CstWalk.addNamespacePrefix`, F#'s `ImplicitlyOpenOwnNamespace` —
                    // `CheckDeclarations.fs:355`). Publishing this file's declared namespaces
                    // instead would hand every LATER file an implicit `open` of them whatever
                    // namespace it declares, which is not F#: there, a prior file contributes
                    // only its root NAME (`AddLocalRootModuleOrNamespace`), never its contents.
                    AmbientOpenPrefixes = []
                    IntrinsicReverseCanon = intrinsicReverse
                    IntrinsicForwardRepr = intrinsicForward
                }
        )
