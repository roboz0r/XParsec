namespace XParsec.FSharp.SemanticAnalysis

// In-memory projection of a FROZEN implementation file to the surface it publishes, a file's
// *implicit signature*, so file N+1 resolves file N's exports by NAME with no DLL emitted.
// Keeps INTERNAL-or-better, as a resolved `.fsi` does: the cross-assembly public-only cut
// belongs to the consumer, not to publication.
//
// SIGNATURES only: the splice templates are collected separately and layered on, so a `.fsi`
// can replace what a file publishes without taking its inline bodies with it.

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

    /// Project a frozen implementation file's INTERNAL-or-better signature to the surface it
    /// publishes. `declaredIn` is the file `frozen` was analysed FROM: every anchor in every
    /// published `ValRepr` indexes that file's `Lexed`.
    let toSurface (declaredIn: LexedFile) (frozen: FrozenPools) : PublishedSurface =
        let originIn (ns: NamespaceKey) : SymbolOrigin =
            {
                Home = SymbolHome.InFile declaredIn.Path
                Namespace = ns
            }

        // Internal-or-better: keep `Public` + `Internal`, drop `Private`. A key ABSENT from
        // the table is `Public` (an unmarked decl), so it is exported.
        let exported (key: SymbolKey) : bool =
            match EqDict.tryFind key frozen.Residue.Accessibility with
            | ValueSome Accessibility.Private -> false
            | _ -> true

        let surface = PublishedSurfaceBuilder.create ()

        // --- member projection --------------------------------------------------------
        // A member's frozen `Params` / `ReturnTy` already carry the declaring type's typars as
        // `FTTypar(Declaring,i)` and its own as `FTTypar(Method,j)`, the axis convention here.

        /// A member with no argument group is a value member, so `Storage` reads off the
        /// signature rather than being passed alongside it and risking disagreement.
        let memberFromParts
            (declKey: TypeKey)
            (declArity: int)
            (name: string)
            (isStatic: bool)
            (methodArity: int)
            (signature: ExternalSignature)
            : ExternalMember =
            let kind, storage =
                match signature.ArgGroups.Length with
                | 0 -> MemberKind.Property, MemberStorage.Property
                | _ -> MemberKind.Method, MemberStorage.Method

            let argSig = ExternalSignature.argSigOf signature

            { ExternalMember.OfKey(SymbolKeyOps.memberKeyOf declKey name argSig methodArity kind) with
                IsStatic = isStatic
                Storage = storage
                Signature = signature
                Origin = originIn declKey.Namespace
            }

        let memberOf (declKey: TypeKey) (declArity: int) (m: TastAccessor.TypeMember) : ExternalMember =
            let isValueMember = (m.Kind = TMemberKind.Property)
            let methodArity = m.MethodTypeParams.Length

            let signature =
                if isValueMember then
                    ExternalSignature.value (declArity, methodArity, m.ReturnTy)
                else
                    ExternalSignature.make (
                        declArity,
                        methodArity,
                        ExternalSignature.tupledParams (EqArray.ofSeq [ for (_, ty) in m.Params -> ty ]),
                        m.ReturnTy
                    )

            memberFromParts declKey declArity m.Name m.IsStatic methodArity signature

        // An interface's abstract method carries a single CURRIED `Signature`; a concrete
        // member carries decurried `Params` / `ReturnTy`. Peel ONE `->` to the `.NET`-tupled
        // domain (`unit` domain / no `->` ⇒ none); a property's `Signature` IS its value type.
        let abstractMemberOf (declKey: TypeKey) (declArity: int) (am: Frozen.TAbstractMethod) : ExternalMember =
            let methodArity = am.MethodTypeParams.Length

            let signature =
                if am.Kind = TMemberKind.Property then
                    ExternalSignature.value (declArity, methodArity, am.Signature)
                else
                    let parameters, returnTy =
                        match am.Signature with
                        | FTFun(domain, codomain) -> domain, codomain
                        | other -> ExternalSignature.unitFrozen, other

                    ExternalSignature.make (declArity, methodArity, parameters, returnTy)

            memberFromParts declKey declArity am.Name am.IsStatic methodArity signature

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

        /// A class's `.ctor` overloads: the primary constructor when the class declares one,
        /// then each `new(…)` secondary in declaration order.
        let ctorsOf (declKey: TypeKey) (declArity: int) (c: TastAccessor.Class) : ResizeArray<ExternalMember> =
            let acc = ResizeArray<ExternalMember>()

            let selfTy =
                FTClass(declKey, EqArray.ofSeq [ for i in 0 .. declArity - 1 -> FTTypar(TyparAxis.Declaring, i) ])

            let addCtor (paramTys: EqArray<FrozenType>) =
                let parameters = ExternalSignature.tupledParams paramTys

                acc.Add(
                    ExternalMember.ctor
                        declKey
                        (ExternalSignature.make (declArity, 0, parameters, selfTy))
                        (ExternalSignature.argSigOfParameters parameters)
                        (originIn declKey.Namespace)
                        []
                )

            if c.HasPrimaryCtor then
                addCtor (EqArray.ofSeq [ for p in c.CtorParams -> p.Type ])

            for sc in c.SecondaryCtors do
                addCtor (EqArray.ofSeq [ for (_, ty) in sc.Params -> ty ])

            acc

        // --- union case shape ---------------------------------------------------------
        let caseShapeOf (c: Frozen.TUnionCase) : ExternalCaseShape =
            {
                Name = c.Name
                FieldNames = EqArray.ofSeq [ for (n, _) in c.Fields -> n ]
                FrozenFieldTypes = EqArray.ofSeq [ for (_, ty) in c.Fields -> ty ]
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
                let typars = TTypeParam.kinds td.TypeParams
                let arity = typars.Length
                let origin = originIn typeKey.Namespace

                let register (shape: ExternalTypeShape) (members: ResizeArray<ExternalMember> voption) =
                    let members =
                        match members with
                        | ValueSome ms -> ms :> seq<ExternalMember>
                        | ValueNone -> Seq.empty

                    PublishedSurfaceBuilder.addTypeWith surface typeKey shape members

                match td.Kind with
                | TTypeKindG.Record {
                                        Fields = fields
                                        Members = members
                                        ValueKind = valueKind
                                    } ->
                    let fieldShapes =
                        EqArray.ofSeq
                            [
                                for f in fields ->
                                    {
                                        Name = f.Name
                                        IsMutable = f.IsMutable
                                        Frozen = f.Type
                                    }
                                    : ExternalFieldShape
                            ]

                    register
                        (ExternalTypeShape.Record
                            {
                                Typars = typars
                                Fields = fieldShapes
                                Origin = origin
                                IsValueType = valueKind.IsValueType
                                RequiresQualifiedAccess = td.IsRequireQualifiedAccess
                            })
                        (ValueSome(membersOf typeKey arity members))

                | TTypeKindG.Union {
                                       Cases = cases
                                       Members = members
                                       ValueKind = valueKind
                                   } ->
                    let caseShapes = EqArray.ofSeq [ for c in cases -> caseShapeOf c ]

                    register
                        (ExternalTypeShape.Union
                            {
                                Typars = typars
                                Cases = caseShapes
                                Interfaces = EqArray.empty
                                Origin = origin
                                IsValueType = valueKind.IsValueType
                                RequiresQualifiedAccess = td.IsRequireQualifiedAccess
                            })
                        (ValueSome(membersOf typeKey arity members))

                | TTypeKindG.Class c ->
                    let members = membersOf typeKey arity c.Members
                    members.AddRange(ctorsOf typeKey arity c)

                    let shape: ExternalClassShape =
                        {
                            Typars = typars
                            Commitment = ClassCommitment.Class
                            Members = EqArray.ofResizeArray members
                            // Neither clause is DROPPED: inference stamps one only once it
                            // resolves, reporting every other spelling. A drop would lose an
                            // impl, or re-parent the class to `obj`.
                            FrozenInterfaces =
                                EqArray.ofSeq
                                    [
                                        for (ity, _) in c.Interfaces ->
                                            FrozenNominal.ofFrozen "an `interface` clause" ity
                                    ]
                            FrozenBaseType = c.Base |> ValueOption.map (fun b -> b.Parent.Nominal)
                            Flags =
                                { ExternalClassFlags.Default with
                                    Declared = c.Declared
                                    IsValueType = c.ValueKind.IsValueType
                                }
                            Attributes = td.Attributes
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
                            Typars = typars
                            Commitment = ClassCommitment.Interface
                            Members = EqArray.ofResizeArray members
                            FrozenInterfaces = EqArray.empty
                            FrozenBaseType = ValueNone
                            Flags = ExternalClassFlags.Default
                            Attributes = td.Attributes
                            Origin = origin
                        }

                    register (ExternalTypeShape.Class shape) (ValueSome members)

                | TTypeKindG.Enum cases ->
                    // Project the closed case→literal table to the shape a later file resolves
                    // `(x: E)` / `E.Ci` against; its nominal identity IS the registered `key`,
                    // so no case index is needed. A case with no literal is DROPPED.
                    let caseShapes =
                        EqArray.ofSeq
                            [
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
                            ]

                    register (ExternalTypeShape.Enum(caseShapes, origin)) ValueNone

                // The frozen RHS already carries the declaring typars on the `Declaring`
                // axis, which is the axis a use site instantiates against.
                | TTypeKindG.Abbrev body -> register (ExternalTypeShape.Abbrev(typars, body)) ValueNone

            | _ -> ()

        // --- module values + inline values --------------------------------------------
        // A pool of this provider's own, for the re-axised tuple-group patterns it hands out.
        let valReprPats = TastPoolBuilder.openEmpty ()

        // Every binding fact below is read at the bound variable ID the decl's own pattern carries.
        let bindingValReprs = DenseTable.index frozen.BindingValReprs

        let bindingValRepr (boundVar: BoundVarId) : TastAccessor.ValRepr voption =
            match bindingValReprs.TryGetValue boundVar with
            // A value has no lambda groups, so it publishes no `ValRepr`.
            | true, vr when not (List.isEmpty vr.Groups) -> ValueSome(valReprToDeclaring pool valReprPats vr)
            | _ -> ValueNone

        let addValue (info: ModuleBindingInfo) (boundVar: BoundVarId) (ty: FrozenType) =
            let scheme = ConformanceTypars.toDeclaringAxis ty

            let sym =
                { ExternalSymbols.scheme info.Container info.Name scheme (FrozenPools.typarArity frozen boundVar) [] with
                    Origin = originIn info.Container.Namespace
                    CompiledName = info.CompiledName
                    ValRepr = bindingValRepr boundVar
                    Attributes = info.Attributes
                }

            PublishedSurfaceBuilder.addValue surface sym

        // EVERY module binding is a `Decls` entry, `inline` ones included, and its identity is in
        // `ModuleMembers`, a TOP-LEVEL binding's too, keyed in the file's namespace so it
        // exports bare. An `inline` one publishes this declaration and nothing more: its
        // template is a separate object, keyed by this same binding key.
        for decl in TastAccessor.roots pool do
            match decl with
            | TastAccessor.DLet {
                                    Pattern = TastAccessor.PNamed boundVar
                                    Ty = ty
                                } ->
                let info = TastPoolBuilder.moduleMemberOf pool boundVar

                if exported info.Key then
                    addValue info boundVar ty
            | _ -> ()

        // --- intrinsic / primitive type shapes ----------------------------------------
        // An intrinsic primitive (`type int = (# "System.Int32" #)`) is kept OUT of
        // `Decls`, so the loop above never sees it: it is published from `IntrinsicBindings`.
        for KeyValue(typeKey, binding) in frozen.Residue.IntrinsicBindings do
            // A HERITABLE `(# class … #)` primitive (`obj` / `exn`) also carries a class
            // surface, so a later file's `inherit` resolves it. The surface is EMPTY here:
            // the impl `.fs` binds the type id and declares no parent and no interfaces.
            let shape =
                if binding.Heritable then
                    ExternalTypeShape.Intrinsic
                        {
                            Id =
                                {
                                    Canon = typeKey
                                    // An intrinsic BINDING is `type x = (# "…" #)`, whose typars
                                    // are the structural constructors' (`'T []`, `byref`).
                                    Typars = TyparKinds.typeOnly typeKey.TyparArity
                                    Platform = IntrinsicPlatform.Bound binding.TypeId
                                }
                            Class =
                                ValueSome
                                    {
                                        Heritable = true
                                        BaseType = ValueNone
                                        Interfaces = EqArray.empty
                                        Members = EqArray.empty
                                    }
                        }
                else
                    ExternalTypeShape.Intrinsic(
                        IntrinsicShape.Scalar(
                            typeKey,
                            TyparKinds.typeOnly typeKey.TyparArity,
                            IntrinsicPlatform.Bound binding.TypeId
                        )
                    )

            PublishedSurfaceBuilder.addType surface typeKey shape

        for KeyValue(m, facts) in frozen.Residue.Modules do
            PublishedSurfaceBuilder.addModule
                surface
                m
                {
                    Home = SymbolHome.InFile declaredIn.Path
                    Facts = facts
                }

        PublishedSurface.ofBuilder surface

    /// `toSurface` as a provider view.
    let toSignatures (declaredIn: LexedFile) (frozen: FrozenPools) : IExternalSymbolProvider =
        PublishedSurface.toProvider (toSurface declaredIn frozen)
