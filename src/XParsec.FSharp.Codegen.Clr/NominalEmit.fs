namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection.Metadata
open XParsec.FSharp.SemanticAnalysis
open AssemblerScaffold
open NominalShared

/// The per-type bound variable/preparer shared by unions, records, and classes. `register`
/// fills the `EmitContext` registries with layout-derived handles only, so any prepared
/// body can reference any type's ctor, factory, field, or member. The union half lives in
/// `UnionEmit`; this module orchestrates and holds the record and class halves.
module internal NominalEmit =

    /// The resolved `inherit` parent of a nominal class: the `extends` column and the
    /// primary-ctor chain target both read off it.
    type private BaseShape =
        /// No `inherit` clause: `extends Object`, or `System.ValueType` for a struct.
        | NoBase
        /// A non-generic FOREIGN base (`inherit exn` → `System.Exception`): `tref` is the
        /// `extends` token, and `key` is the PLATFORM key the chained base `.ctor` is minted
        /// against.
        | ExternalBase of key: TypeKey * tref: EntityHandle
        /// A non-generic base this compilation emits: `handle` is its `TypeDefinition` token,
        /// and `key` reaches its ctor through the emitted `classes` registry.
        | LocalMono of key: TypeKey * handle: EntityHandle
        /// A generic parent (`Box<int>`), or any non-`FTClass` base: `extends` a
        /// `GENERICINST` `TypeSpec` encoded against this class's typars.
        | Generic of ft: FrozenType

    let register
        (asm: Assembler)
        (input: NominalEmissionInput)
        (td: TastAccessor.TypeDecl)
        (members: TastAccessor.TypeMember list)
        : unit =
        let icodegen = asm.Icodegen
        let isGeneric = not td.TypeParams.IsEmpty
        let typarMarkers = typarMarkersOf td

        // Every member's handle is its layout row, resolvable before any body
        // is built, so a member body can reference a sibling (`this.Length`) or
        // a case factory (`static member Empty = []`).
        let emittedMembers = Dictionary<string, EqArray<Emit.EmittedMember>>()

        // Name → its overloads in declaration order. Own members lead and interface-impl
        // members trail, so a same-signature pair (`Set.Add : Set<'T>` vs
        // `ICollection<'T>.Add : unit`) resolves to the class's own member on a tie.
        (members @ NominalMembers.flattenIfaceMembers input.Interfaces)
        |> List.iteri (fun i (mem: TastAccessor.TypeMember) ->
            let em: Emit.EmittedMember =
                {
                    Handle = toEntity (asm.MethodDef(MethodKey.Member(td.Key, i)))
                    IsStatic = mem.IsStatic
                    ParamArity = mem.Params.Length
                    MetaName = memberMetaName mem.Name mem.Kind
                    ParamTys = [ for (_, t) in mem.Params -> t ]
                    RetTy = mem.ReturnTy
                    MethodTyparCount = mem.MethodTypeParams.Length
                }

            let prior =
                match emittedMembers.TryGetValue mem.Name with
                | true, ms -> ms
                | false, _ -> EqArray.empty

            emittedMembers.[mem.Name] <- EqArray.append prior (EqArray.singleton em)
        )

        match input with
        | NominalEmissionInput.Union ud -> UnionEmit.register asm ud emittedMembers

        | NominalEmissionInput.Record rd ->
            asm.Records.[td.TypeKey] <-
                {
                    Name = td.Name
                    Typars = EqArray.toList (TTypeParam.names td.TypeParams)
                    Fields =
                        [
                            for f in rd.Fields ->
                                {
                                    Name = f.Name
                                    Field = toEntity (asm.FieldDef(FieldKey.RecordField(td.Key, f.Name)))
                                    Ty = f.Type
                                    Accessors =
                                        EmitTypes.RecordFieldAccessorRefs.create
                                            f.IsMutable
                                            (fun role ->
                                                toEntity (
                                                    asm.MethodDef(MethodKey.RecordFieldAccessor(td.Key, f.Name, role))
                                                )
                                            )
                                }
                        ]
                    IsValueType = rd.ValueKind.IsValueType
                    Ctor = toEntity (asm.MethodDef(MethodKey.NominalCtor td.Key))
                    Members = emittedMembers
                }

        | NominalEmissionInput.Class cd ->
            let instanceFields = cd.Fields
            let ctorParams = cd.CtorParams
            let staticLets = TPreambleEntryG.lets cd.StaticPreamble
            let instanceLets = TPreambleEntryG.lets cd.InstancePreamble
            let secondaryCtors = cd.SecondaryCtors
            let isStruct = cd.ValueKind.IsValueType

            // The handle every `ldsfld`/`stsfld` references: a generic class reaches
            // its own `static let` field through a `MemberRef` on the open
            // self-`TypeSpec` (`Set\`1<!0>::empty`), a mono class through the `Def` token.
            let staticFieldsDict = Dictionary<string, EntityHandle>()

            for sl in staticLets do
                staticFieldsDict.[sl.Name] <-
                    if isGeneric then
                        icodegen.UserGenericMemberRef(
                            td.TypeKey,
                            typarMarkers,
                            UserMemberKind.ClassMember(ClassMember.Field sl.Name)
                        )
                    else
                        toEntity (asm.FieldDef(FieldKey.ClassStaticField(td.Key, sl.Name)))

            // `(arity, paramTys, handle)` lets a `New` call site pick the matching
            // overload; the param types carry declaring-typar markers so a generic
            // site can mint a `MemberRef` on the instantiated `TypeSpec`.
            let secondaryCtorHandles =
                secondaryCtors
                |> List.mapi (fun i (sc: TastAccessor.SecondaryCtor) ->
                    sc.Params.Length,
                    [ for (_, t) in sc.Params -> t ],
                    toEntity (asm.MethodDef(MethodKey.SecondaryCtor(td.Key, i)))
                )

            // The val-field reference form (no primary ctor) declares no `NominalCtor`
            // row, so don't reserve its handle. `Ctor` aliases the first secondary.
            // Structs and the no-secondary fallback keep the synthesised primary.
            let emitPrimaryCtor =
                isStruct || cd.HasPrimaryCtor || List.isEmpty secondaryCtorHandles

            let ctorHandle =
                if emitPrimaryCtor then
                    toEntity (asm.MethodDef(MethodKey.NominalCtor td.Key))
                else
                    let (_, _, h) = List.head secondaryCtorHandles
                    h

            asm.Classes.[td.TypeKey] <-
                {
                    Name = td.Name
                    Typars = EqArray.toList (TTypeParam.names td.TypeParams)
                    Fields =
                        [
                            for p in ctorParams ->
                                p.Name, toEntity (asm.FieldDef(FieldKey.ClassCtorParamField(td.Key, p.Name))), p.Type
                        ]
                    // A declared `val` field and an instance-`let` backing field are one
                    // thing at a use site: `this.x` resolves by NAME against this list.
                    InstanceFields =
                        [
                            for f in instanceFields ->
                                f.Name, toEntity (asm.FieldDef(FieldKey.ClassInstanceField(td.Key, f.Name))), f.Type
                            for l in instanceLets ->
                                l.Name, toEntity (asm.FieldDef(FieldKey.ClassLetField(td.Key, l.Name))), l.Type
                        ]
                    IsValueType = isStruct
                    Ctor = ctorHandle
                    HasPrimaryCtor = emitPrimaryCtor
                    Members = emittedMembers
                    StaticFields = staticFieldsDict
                    SecondaryCtors = secondaryCtorHandles
                    // The implemented interfaces over this class's declaring typars; the impl
                    // member bodies are not needed here.
                    Interfaces = [ for (iface, _) in cd.Interfaces -> iface ]
                }

    /// The record `.ctor`: one parameter per field, stored in declaration order, and the
    /// accessor pair behind each field's property.
    let private prepareRecord
        (asm: Assembler)
        (td: TastAccessor.TypeDecl)
        (recordIsStruct: bool)
        (fields: Frozen.TRecordField list)
        : unit =
        let provider = asm.Provider

        let fieldHandles =
            [
                for f in fields -> toEntity (asm.FieldDef(FieldKey.RecordField(td.Key, f.Name)))
            ]

        // A raw `FieldDefinition` token in `ldfld` / `stfld` resolves to the wrong slot for
        // a field at index >= 1 of a generic type, so the `.ctor` and the accessors alike
        // reach a field through its `MemberRef` on the open self-`TypeSpec` (`R\`1<!0>::Y`).
        let fieldRefs =
            List.map2
                (fun (f: Frozen.TRecordField) handle ->
                    selfMemberRef asm td (UserMemberKind.RecordMember(RecordMember.Field f.Name)) handle
                )
                fields
                fieldHandles

        // `System.ValueType` has no accessible ctor and value types do not chain,
        // so a struct record's `.ctor` only stores fields; a reference record
        // chains `Object::.ctor`.
        let ctorBody =
            if recordIsStruct then
                Emit.buildStructCtor fieldRefs
            else
                Emit.buildChainedCtor provider.ObjectCtorRef [] fieldRefs

        let ctorMethodBody = bodyOf asm ctorBody

        asm.AddPrepared(
            MethodKey.NominalCtor td.Key,
            {
                Signature = provider.RecordCtorSignature [ for f in fields -> f.Type ]
                Body = ctorMethodBody
                ParamNames = [ for f in fields -> f.Name ]
                MethodTypars = []
            }
        )

        // A struct record's `ldarg.0` is a byref, which `ldfld` and `stfld` accept.
        for (f, fieldRef) in List.zip fields fieldRefs do
            for role in RecordFieldAccessors.rolesOf f do
                let body, paramNames =
                    match role with
                    | TAccessorRole.Getter -> Emit.buildFieldGetter fieldRef, []
                    | TAccessorRole.Setter -> Emit.buildFieldSetter fieldRef, [ "value" ]

                let prepared: PreparedMethod =
                    {
                        Signature = provider.RecordAccessorSignature(role, f.Type)
                        Body = bodyOf asm body
                        ParamNames = paramNames
                        MethodTypars = []
                    }

                asm.AddPrepared(MethodKey.RecordFieldAccessor(td.Key, f.Name, role), prepared)

    let private classBaseShapeOf (asm: Assembler) (td: TastAccessor.TypeDecl) (cd: ClassDecl) : BaseShape =
        let icodegen = asm.Icodegen

        match cd.Base with
        | ValueNone -> BaseShape.NoBase
        // A parent carrying type ARGUMENTS resolves through a `TypeSpec` whatever its
        // flavour, so only an argless one is worth classifying further.
        | ValueSome b when not b.Parent.Args.IsEmpty -> BaseShape.Generic(FrozenNominal.ty b.Parent.Nominal)
        | ValueSome b ->
            match b.Parent with
            | BaseParentG.Class n ->
                match icodegen.ClassOrigin n.Key with
                | ClassOrigin.Foreign tref -> BaseShape.ExternalBase(n.Key, tref)
                | ClassOrigin.Local handle -> BaseShape.LocalMono(n.Key, handle)
                | ClassOrigin.Unresolved ->
                    failwithf "Emit: class '%s' inherits %A, which resolves to no class" td.Name n.Key
            // An intrinsic-class parent (`inherit exn`) is inherited by CANON, so
            // resolve it to its platform class (`System.Exception`).
            | BaseParentG.PrimitiveCanon n ->
                match icodegen.IntrinsicClassBase n.Key with
                | ValueSome(platformKey, tref) -> BaseShape.ExternalBase(platformKey, tref)
                | ValueNone -> BaseShape.Generic(FrozenNominal.ty n)

    /// The primary `.ctor`'s chain target and its argument expressions.
    let private classCtorChain
        (asm: Assembler)
        (td: TastAccessor.TypeDecl)
        (isStruct: bool)
        (baseShape: BaseShape)
        (baseCtorCall: TastAccessor.BaseCtorCall voption)
        : Emit.CtorChain =
        let provider = asm.Provider
        let icodegen = asm.Icodegen
        let classes = asm.Classes

        // An external base's `.ctor` is minted BY KEY from the `ChosenCtor` identity
        // the front end recorded, falling back to arity; the parameterless one is
        // minted off the `TypeRef`, since a protected ctor is not in the member set.
        match baseShape, baseCtorCall with
        | BaseShape.ExternalBase(baseKey, _), ValueSome bcc when not bcc.Args.IsEmpty ->
            let argTypes = [ for a in bcc.Args -> TastAccessor.exprTy a ]

            match icodegen.TryEmitCtor(baseKey, bcc.ChosenCtor, [], argTypes) with
            | ValueSome recipe -> Emit.CtorChain.Base(recipe.Handle, EqArray.toList bcc.Args)
            | ValueNone ->
                failwithf
                    "Emit: class '%s' inherits external base %A but no '.ctor' overload matches its %d base-ctor argument(s)"
                    td.Name
                    baseKey
                    bcc.Args.Length
        | BaseShape.ExternalBase(baseKey, _), _ ->
            match icodegen.ExternalParameterlessBaseCtor baseKey with
            | ValueSome extCtor -> Emit.CtorChain.Base(extCtor, [])
            | ValueNone ->
                failwithf
                    "Emit: class '%s' inherits external base %A but its parameterless '.ctor()' could not be minted"
                    td.Name
                    baseKey
        | _, ValueSome bcc ->
            let baseKey, baseArgs =
                match baseShape with
                | BaseShape.LocalMono(k, _) -> k, []
                | BaseShape.Generic(FTClass(n, xs)) -> n, EqArray.toList xs
                | _ -> failwithf "Emit: class '%s' has a base-ctor call but no class base type" td.Name

            // `inherit Base(args)` reaches any of the base's ctors, so the chain target
            // is picked on the same two axes a `TExpr.New` is.
            let baseCtorHandle =
                match classes.TryGetValue baseKey with
                | true, bc ->
                    let argTypes = [ for a in bcc.Args -> TastAccessor.exprTy a ]
                    let kind, handle = EmitResolve.pickLocalCtor td.Name bc baseArgs argTypes

                    if List.isEmpty bc.Typars then
                        handle
                    else
                        icodegen.UserGenericMemberRef(baseKey, baseArgs, kind)
                | false, _ ->
                    failwithf "Emit: base class '%A' of '%s' is not an emitted project-local class" baseKey td.Name

            Emit.CtorChain.Base(baseCtorHandle, EqArray.toList bcc.Args)
        | _, ValueNone when isStruct -> Emit.CtorChain.None
        | _, ValueNone -> Emit.CtorChain.Base(provider.ObjectCtorRef, [])

    // The `.cctor` runs stores and effects interleaved, in declaration order,
    // which is load-bearing:
    // `static let a = f()` / `static do g a` / `static let b = h()`.
    let private prepareCctor
        (asm: Assembler)
        (emitCtx: Emit.EmitContext)
        (td: TastAccessor.TypeDecl)
        (staticPreamble: TastAccessor.PreambleEntry list)
        : unit =
        if not (List.isEmpty staticPreamble) then
            let staticFields = asm.Classes.[td.TypeKey].StaticFields

            let cctorSteps =
                [
                    for entry in staticPreamble ->
                        match entry with
                        | TPreambleEntryG.Let sl -> Emit.PreambleStep.Store(staticFields.[sl.Name], sl.Init)
                        | TPreambleEntryG.Do e -> Emit.PreambleStep.Run e
                ]

            let cctorBody = bodyOf asm (Emit.buildStaticCctor emitCtx cctorSteps)

            asm.AddPrepared(
                MethodKey.NominalCctor td.Key,
                {
                    Signature = asm.Provider.CctorSignature()
                    Body = cctorBody
                    ParamNames = []
                    MethodTypars = []
                }
            )

    // Each secondary is a `.ctor` overload whose body runs its `let`-preamble,
    // then either chains the primary `.ctor` or stores explicit field inits.
    let private prepareSecondaryCtors
        (asm: Assembler)
        (emitCtx: Emit.EmitContext)
        (td: TastAccessor.TypeDecl)
        (cd: ClassDecl)
        (classCtor: EntityHandle)
        : unit =
        let icodegen = asm.Icodegen
        let isGeneric = not td.TypeParams.IsEmpty
        let typarMarkers = typarMarkersOf td
        let instanceFields = cd.Fields
        let ctorParams = cd.CtorParams
        let secondaryCtors = cd.SecondaryCtors

        if not (List.isEmpty secondaryCtors) then
            let primaryCtorRef =
                selfMemberRef asm td (UserMemberKind.ClassMember ClassMember.Ctor) classCtor

            secondaryCtors
            |> List.iteri (fun i sc ->
                let paramTys = [ for (_, t) in sc.Params -> t ]

                let lets = EqArray.toList sc.Lets

                let ctorIr =
                    match sc.Body with
                    | TSecondaryCtorBodyG.ExplicitFieldInit inits ->
                        // Both ctor-param backing fields and explicit `val` fields
                        // are eligible.
                        let fieldHandleOf name =
                            if isGeneric then
                                icodegen.UserGenericMemberRef(
                                    td.TypeKey,
                                    typarMarkers,
                                    UserMemberKind.ClassMember(ClassMember.Field name)
                                )
                            elif ctorParams |> List.exists (fun (p: Frozen.TRecordField) -> p.Name = name) then
                                toEntity (asm.FieldDef(FieldKey.ClassCtorParamField(td.Key, name)))
                            elif instanceFields |> List.exists (fun (f: Frozen.TRecordField) -> f.Name = name) then
                                toEntity (asm.FieldDef(FieldKey.ClassInstanceField(td.Key, name)))
                            else
                                failwithf "Emit: class '%s' secondary ctor inits unknown field '%s'" td.Name name

                        let fieldInits = [ for fi in inits -> fieldHandleOf fi.Field, fi.Init ]

                        Emit.buildSecondaryCtorFieldInit emitCtx sc.Params lets fieldInits
                    | TSecondaryCtorBodyG.Chain primaryArgs ->
                        Emit.buildSecondaryCtor emitCtx sc.Params lets primaryCtorRef (EqArray.toList primaryArgs)

                let scBody = bodyOf asm ctorIr

                asm.AddPrepared(
                    MethodKey.SecondaryCtor(td.Key, i),
                    {
                        Signature = asm.Provider.RecordCtorSignature paramTys
                        Body = scBody
                        ParamNames = argNames sc.Params.Length
                        MethodTypars = []
                    }
                )
            )

    /// The class's ctors and `.cctor`; returns the `extends` handle for its
    /// `TypeDefinition` row.
    let private prepareClass
        (asm: Assembler)
        (emitCtx: Emit.EmitContext)
        (td: TastAccessor.TypeDecl)
        (defaultBase: EntityHandle)
        (cd: ClassDecl)
        : EntityHandle =
        let provider = asm.Provider
        let icodegen = asm.Icodegen
        let ctorParams = cd.CtorParams
        let secondaryCtors = cd.SecondaryCtors
        let baseCtorCall = cd.Base |> ValueOption.bind (fun b -> b.Ctor)
        let isStruct = cd.ValueKind.IsValueType

        let baseShape = classBaseShapeOf asm td cd

        // A non-generic parent is its token directly because the `extends` column
        // rejects a `TypeSpec` that merely wraps a plain class.
        let baseTypeHandle =
            match baseShape with
            | BaseShape.NoBase -> if isStruct then provider.ValueTypeBase else defaultBase
            | BaseShape.ExternalBase(_, handle)
            | BaseShape.LocalMono(_, handle) -> handle
            | BaseShape.Generic bt -> icodegen.TypeToken bt

        let emitPrimaryCtor = isStruct || cd.HasPrimaryCtor || List.isEmpty secondaryCtors

        // The chain target for a secondary that chains to the primary. In the
        // suppressed val-field form it aliases the first secondary, so
        // `primaryCtorRef` resolves to a real token, not an absent `NominalCtor`.
        let classCtor =
            if emitPrimaryCtor then
                toEntity (asm.MethodDef(MethodKey.NominalCtor td.Key))
            else
                toEntity (asm.MethodDef(MethodKey.SecondaryCtor(td.Key, 0)))

        // A generic class's ctor `stfld` sequence reaches each field through a
        // `MemberRef` on the open self-`TypeSpec` (`Box\`1<!0>::n`): the raw
        // `FieldDefinition` token resolves to the wrong slot at index >= 1.
        let ctorFieldRefs =
            [
                for p in ctorParams ->
                    selfMemberRef
                        asm
                        td
                        (UserMemberKind.ClassMember(ClassMember.Field p.Name))
                        (toEntity (asm.FieldDef(FieldKey.ClassCtorParamField(td.Key, p.Name))))
            ]

        let ctorChain = classCtorChain asm td isStruct baseShape baseCtorCall

        // Base args are the only ctor expressions that reference a primary-ctor param
        // directly (`this` does not exist yet; a preamble entry reaches one through its
        // backing field), so this is empty for every other chain shape.
        let ctorParamArgs =
            match baseCtorCall with
            | ValueSome bcc -> EqArray.toList bcc.CtorParams
            | ValueNone -> []

        // The instance preamble, resolved through the same self-`MemberRef` shape as
        // the ctor-param stores.
        let instanceSteps =
            [
                for entry in cd.InstancePreamble ->
                    match entry with
                    | TPreambleEntryG.Let l ->
                        Emit.PreambleStep.Store(
                            selfMemberRef
                                asm
                                td
                                (UserMemberKind.ClassMember(ClassMember.Field l.Name))
                                (toEntity (asm.FieldDef(FieldKey.ClassLetField(td.Key, l.Name)))),
                            l.Init
                        )
                    | TPreambleEntryG.Do e -> Emit.PreambleStep.Run e
            ]

        let ctorBody =
            Emit.buildClassPrimaryCtor emitCtx ctorChain cd.ThisKey ctorParamArgs ctorFieldRefs instanceSteps

        if emitPrimaryCtor then
            let ctorMethodBody = bodyOf asm ctorBody

            asm.AddPrepared(
                MethodKey.NominalCtor td.Key,
                {
                    Signature = provider.RecordCtorSignature [ for p in ctorParams -> p.Type ]
                    Body = ctorMethodBody
                    ParamNames = [ for p in ctorParams -> p.Name ]
                    MethodTypars = []
                }
            )

        prepareCctor asm emitCtx td cd.StaticPreamble
        prepareSecondaryCtors asm emitCtx td cd classCtor

        baseTypeHandle

    /// One authored member's body and signature row. `selfValueTy` is the declaring type,
    /// instantiated at its own typars, when it is a VALUE type.
    let private prepareTypeMember
        (asm: Assembler)
        (emitCtx: Emit.EmitContext)
        (td: TastAccessor.TypeDecl)
        (selfValueTy: FrozenType voption)
        (index: int)
        (mem: TastAccessor.TypeMember)
        : unit =
        let provider = asm.Provider

        // `(name, ty)[]` in ABI order — position IS the typar index. Feeds the
        // `GENERIC` header arity and the `GenericParam` rows.
        let methodTypars = mem.MethodTypeParams
        let isGenericMethod = methodTypars.Length > 0

        // A `unit`-returning member, static or instance, encodes as genuine CLR
        // `void`. Emitting the `unit`-as-`ValueTuple` return instead breaks
        // cross-assembly binding: a consumer's void member-ref misses it.
        let returnsVoid =
            match mem.ReturnTy with
            | FTUnit -> true
            | _ -> false

        // A static member has no `this` to deref-copy.
        let selfValueTy = if mem.IsStatic then ValueNone else selfValueTy

        let memberBody =
            try
                bodyOf
                    asm
                    (Emit.buildMember emitCtx selfValueTy mem.ThisKey mem.BaseKey mem.Params returnsVoid mem.Body)
            with ex ->
                raise (
                    System.Exception(sprintf "While lowering body of member '%A.%s'\n%s" td.Key mem.Name ex.Message, ex)
                )

        let paramTys = [ for (_, t) in mem.Params -> t ]

        // A generic method needs the `GENERIC` calling-convention header count; its
        // own typars appear as `FTTypar(Method, i)` nodes, encoded `!!i`.
        let signature =
            try
                if returnsVoid && isGenericMethod then
                    provider.GenericMethodOnTypeSignatureVoid(methodTypars.Length, paramTys, not mem.IsStatic)
                elif returnsVoid && mem.IsStatic then
                    provider.StaticMethodSignatureVoid paramTys
                elif returnsVoid then
                    provider.InstanceMethodSignatureVoid paramTys
                elif isGenericMethod then
                    provider.GenericMethodOnTypeSignature(methodTypars.Length, paramTys, mem.ReturnTy, not mem.IsStatic)
                elif mem.IsStatic then
                    provider.StaticMethodSignature(paramTys, mem.ReturnTy)
                else
                    provider.InstanceMethodSignature(paramTys, mem.ReturnTy)
            with ex ->
                // A leaked metavar / unresolved type constructor surfaces here as an anonymous
                // encoder failure; identify the member and keep the original as
                // `InnerException`, whose stack pinpoints the encode site.
                raise (System.Exception(sprintf "While encoding signature of member '%A.%s'" td.Key mem.Name, ex))

        asm.AddPrepared(
            MethodKey.Member(td.Key, index),
            {
                Signature = signature
                Body = memberBody
                ParamNames = argNames mem.Params.Length
                // The metadata name drops the F# leading quote: `'T` → `T`.
                MethodTypars = [ for (n, _) in methodTypars -> n.TrimStart('\'') ]
            }
        )

    /// Every body behind a record's synthesised structural rows. One pass mints the field
    /// refs that equality, comparison and `%A` share.
    let private prepareRecordStructural
        (asm: Assembler)
        (td: TastAccessor.TypeDecl)
        (self: StructuralSelf)
        (recordIsStruct: bool)
        : unit =
        let provider = asm.Provider
        let handles = asm.Structural

        // Minted on first use and shared from there: a record that synthesises no
        // structural body at all adds no `MemberRef` row.
        let named = lazy (recordFieldRefs asm td)

        // One shape, so nothing precedes the field walk.
        let walk: Lazy<EmitStructural.StructuralWalk> =
            lazy (EmitStructural.StructuralWalk.Flat(ValueNone, [ for (_, f) in named.Value -> f ]))

        if self.Members.Equality then
            prepareEqualityTriple
                asm
                td
                self.SelfTy
                (ValueSome(EmitStructural.buildGetHashCode handles walk.Value))
                (EmitStructural.buildEqualsObj
                    recordIsStruct
                    self.SelfType
                    self.SelfTy
                    (EmitStructural.TypedEntry.Direct(equalsTyped asm td self)))
                (ValueSome(EmitStructural.buildEqualsTyped handles recordIsStruct walk.Value))

        if self.Members.Comparison then

            prepareComparisonPair
                asm
                td
                self.SelfTy
                (ValueSome(EmitStructural.buildCompareTo handles recordIsStruct walk.Value))
                (EmitStructural.buildCompareToObj
                    handles
                    recordIsStruct
                    self.SelfType
                    self.SelfTy
                    (EmitStructural.TypedEntry.Direct(compareToTyped asm td self)))

        if self.Members.Format then
            asm.AddPrepared(
                MethodKey.FmtFormat td.Key,
                {
                    Signature = provider.StructuralFormatSignature()
                    Body = bodyOf asm (EmitStructuralFormat.buildRecordFormat handles named.Value)
                    ParamNames = [ "sink" ]
                    MethodTypars = []
                }
            )

    // The BCL members a capability's platform interface INHERITS but never declared:
    // unsynthesised, the CLR refuses to load the type. Only the non-generic slots
    // need it, because a generic slot binds implicitly by the authored member's signature.
    let private prepareCoSlots
        (asm: Assembler)
        (td: TastAccessor.TypeDecl)
        (members: TastAccessor.TypeMember list)
        (userInterfaces: (FrozenNominal * TastAccessor.TypeMember list) list)
        : unit =
        let provider = asm.Provider
        let icodegen = asm.Icodegen

        let coSlots =
            CapabilityCoSlots.required asm.Symbols [ for (iface, _) in userInterfaces -> iface ]

        // The authored capability member a shim forwards to, as a handle callable from
        // inside this type. Scoped to the impl block of the capability that DEMANDED the
        // slot, so a like-named member of another interface can never be picked up.
        let capabilityMember (ifaceTy: FrozenNominal) (slot: CoSlot) : EntityHandle * FrozenType =
            let name =
                match CapabilityCoSlots.forwardsTo slot with
                | ValueSome n -> n
                | ValueNone -> failwithf "Emit: co-slot '%A' forwards to no capability member" slot

            let hit =
                NominalMembers.ofInterface members userInterfaces ifaceTy
                |> List.tryFind (fun (_, m) -> m.Name = name)

            match hit with
            | None ->
                failwithf
                    "Emit: type '%A' implements capability '%A', whose co-slot forwards to member '%s', but that impl block declares no such member"
                    td.Key
                    ifaceTy.Key
                    name
            | Some(i, mem) ->
                let kind =
                    UserMemberKind.Member(
                        memberMetaName mem.Name mem.Kind,
                        false,
                        0,
                        [ for (_, t) in mem.Params -> t ],
                        mem.ReturnTy
                    )

                selfMemberRef asm td kind (toEntity (asm.MethodDef(MethodKey.Member(td.Key, i)))), mem.ReturnTy

        for (ifaceTy, slot) in coSlots do
            let signature, body =
                match slot with
                | CoSlot.EnumerableGetEnumerator ->
                    let getEnumerator, _ = capabilityMember ifaceTy slot

                    provider.InstanceMethodSignature(
                        [],
                        FTClass(SymbolKeyOps.typeKeyOf "System.Collections" "IEnumerator", EqArray.empty)
                    ),
                    Emit.buildEnumerableGetEnumeratorCoSlot getEnumerator
                | CoSlot.EnumeratorCurrent ->
                    let current, elemTy = capabilityMember ifaceTy slot

                    provider.InstanceMethodSignature([], RuntimeNames.objTy),
                    Emit.buildEnumeratorCurrentCoSlot current (icodegen.TypeToken elemTy)
                | CoSlot.EnumeratorReset ->
                    provider.InstanceMethodSignatureVoid [],
                    Emit.buildEnumeratorResetCoSlot provider.NotSupportedExceptionCtor

            asm.AddPrepared(
                MethodKey.CapCoSlot(td.Key, slot),
                {
                    Signature = signature
                    Body = bodyOf asm body
                    ParamNames = []
                    MethodTypars = []
                }
            )

    let prepare
        (asm: Assembler)
        (emitCtx: Emit.EmitContext)
        (input: NominalEmissionInput)
        (td: TastAccessor.TypeDecl)
        (members: TastAccessor.TypeMember list)
        : unit =
        let provider = asm.Provider
        let typarMarkers = typarMarkersOf td

        // A `[<Struct>]` nominal: `this` (`ldarg.0`) is a managed pointer, so a member body
        // deref-copies a value use of it, and a union's or record's synthesised
        // equality/comparison bodies take value-type shape. A struct class emits no triple.
        let isStruct = input.IsValueType

        // The `extends` column for this `TypeDefinition`. A `[<Struct>]` union or record
        // extends `System.ValueType`; the reference forms keep the `Object` default, and
        // the class arm picks its own base.
        let defaultBase = provider.ObjectType
        let structuralBase = if isStruct then provider.ValueTypeBase else defaultBase

        // The per-kind rows that precede this type's members: its `.ctor` / factories /
        // fields, and the `extends` column its `TypeDefinition` takes.
        let baseTypeHandle =
            match input with
            | NominalEmissionInput.Union ud ->
                UnionEmit.prepareUnion asm ud
                structuralBase
            | NominalEmissionInput.Record rd ->
                prepareRecord asm td isStruct rd.Fields
                structuralBase
            | NominalEmissionInput.Class cd -> prepareClass asm emitCtx td defaultBase cd

        // Interface-impl member bodies emit as virtual methods the runtime binds to the
        // `InterfaceImpl` row by name + signature. The synthesised eq/comparison/format
        // impls use disjoint `MethodKey`s, so the two never collide on a method row.
        let userInterfaces = input.Interfaces
        let selfTyMarkers = selfTyOf input td typarMarkers

        let selfValueTy = if isStruct then ValueSome selfTyMarkers else ValueNone

        for (index, _, mem) in NominalMembers.indexed members userInterfaces do
            prepareTypeMember asm emitCtx td selfValueTy index mem

        let structural = StructuralMembers.ofInput input

        let structuralSelf =
            {
                SelfType = selfTypeHandleOf asm input td
                SelfTy = selfTyMarkers
                Members = structural
            }

        match input with
        | NominalEmissionInput.Union ud -> UnionEmit.prepareStructural asm ud structuralSelf
        | NominalEmissionInput.Record _ -> prepareRecordStructural asm td structuralSelf isStruct
        | NominalEmissionInput.Class _ -> ()

        prepareCoSlots asm td members userInterfaces

        // One `InterfaceImpl` handle per implemented interface. A generic interface arg
        // (`IEnumerable<'T>`) carries its `'T` as `FTTypar(Declaring, i)`, encoded `!i`.
        let interfaces =
            [
                if structural.Equality then
                    provider.EquatableInterfaceSpec selfTyMarkers
                if structural.Comparison then
                    provider.ComparableInterfaceSpec selfTyMarkers
                    provider.IComparableType
                if structural.Format then
                    provider.StructuralFormattableInterface
                for (iface, _) in userInterfaces do
                    provider.InterfaceHandleOf(FrozenNominal.ty iface)
            ]

        asm.AddTypeRowExtras(
            TypeSlotKey.Nominal td.Key,
            {
                Interfaces = interfaces
                BaseType = baseTypeHandle
            }
        )
