namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection.Metadata
open XParsec.FSharp.SemanticAnalysis
open AssemblerScaffold

/// The per-type binder/preparer shared by unions, records, and classes.
///
/// `register` (the Bind phase) fills the `EmitContext` registries
/// (`Unions`/`Records`/`Classes`) with layout-derived handles — pure data, no
/// bodies — so that *any* prepared body can reference *any* type's ctor,
/// factory, field, or member with no emission-order discipline.
///
/// `prepare` builds every signature/body for the type's method rows against
/// those resolved handles (`Assembler.AddPrepared`) and records the type's
/// `TypeRowExtras` (the pre-minted `InterfaceImpl` / `BaseType` handles).
/// The writer adds the actual rows in layout order.
module internal NominalEmit =

    /// The declaring type's own typars as self-describing open-typar nodes
    /// (`!i`), `i` = position in `TypeParams`. The codegen encoders resolve a
    /// `FTTypar(Declaring, i)` straight off the node, so these need no ambient
    /// typar window.
    let private typarMarkersOf (td: Frozen.TTypeDecl) : FrozenType list =
        [ for i in 0 .. td.TypeParams.Length - 1 -> FTTypar(TyparAxis.Declaring, i) ]

    let private ifaceMembersOf (input: NominalEmissionInput) : Frozen.TTypeMember list =
        match input with
        | NominalEmissionInput.Class(_, _, _, _, _, _, _, interfaces, _) ->
            [
                for (_, ms) in interfaces do
                    yield! ms
            ]
        | _ -> []

    let register
        (asm: Assembler)
        (input: NominalEmissionInput)
        (td: Frozen.TTypeDecl)
        (members: Frozen.TTypeMember list)
        : unit =
        let icodegen = asm.Icodegen
        let isGeneric = not td.TypeParams.IsEmpty
        let typarMarkers = typarMarkersOf td

        // Every member's handle is its layout row, resolvable before any body
        // is built, so a member body can reference a sibling (`this.Length`) or
        // a case factory (`Empty = Nil`). The class's own members lead,
        // interface-impl members trail (same indexing as the layout).
        let emittedMembers = Dictionary<string, Emit.EmittedMember>()

        (members @ ifaceMembersOf input)
        |> List.iteri (fun i (mem: Frozen.TTypeMember) ->
            emittedMembers.[mem.Name] <-
                {
                    Handle = toEntity (asm.MethodDef(MethodKey.Member(td.Key, i)))
                    IsStatic = mem.IsStatic
                    Arity = mem.Params.Length
                    MetaName = memberMetaName mem
                    ParamTys = [ for (_, t) in mem.Params -> t ]
                    RetTy = mem.ReturnTy
                }
        )

        match input with
        | NominalEmissionInput.Union cases ->
            let emittedCases = Dictionary<string, Emit.EmittedCase>()

            cases
            |> List.iteri (fun tag c ->
                emittedCases.[c.Name] <-
                    {
                        Tag = tag
                        Factory = toEntity (asm.MethodDef(MethodKey.UnionFactory(td.Key, c.Name)))
                        Fields =
                            [
                                for fi in 0 .. c.Fields.Length - 1 ->
                                    toEntity (asm.FieldDef(FieldKey.UnionCaseField(td.Key, c.Name, fi)))
                            ]
                    }
            )

            asm.Unions.[td.Key] <-
                {
                    Name = td.Name
                    Typars = EqArray.toList td.TypeParams
                    TagField = toEntity (asm.FieldDef(FieldKey.UnionTag td.Key))
                    Cases = emittedCases
                    Members = emittedMembers
                }

        | NominalEmissionInput.Record fields ->
            asm.Records.[td.Key] <-
                {
                    Name = td.Name
                    Typars = EqArray.toList td.TypeParams
                    Fields =
                        [
                            for f in fields ->
                                f.Name, toEntity (asm.FieldDef(FieldKey.RecordField(td.Key, f.Name))), f.Type
                        ]
                    Ctor = toEntity (asm.MethodDef(MethodKey.NominalCtor td.Key))
                }

        | NominalEmissionInput.Class(instanceFields, ctorParams, _, _, staticLets, secondaryCtors, _, _, isStruct) ->
            // The handle every `ldsfld`/`stsfld` *references*. A generic class
            // reaches its own `static let` field through a `MemberRef` on the
            // open self-`TypeSpec` (`Set\`1<!0>::empty`), the static analogue of
            // the ctor-field `MemberRef`s (G13); a mono class uses the `Def`
            // token directly.
            let staticFieldsDict = Dictionary<string, EntityHandle>()

            for sl in staticLets do
                staticFieldsDict.[sl.Name] <-
                    if isGeneric then
                        icodegen.UserGenericMemberRef(
                            td.Key,
                            typarMarkers,
                            UserMemberKind.ClassMember(ClassMember.Field sl.Name)
                        )
                    else
                        toEntity (asm.FieldDef(FieldKey.ClassStaticField(td.Key, sl.Name)))

            // The `(arity, paramTys, handle)` list lets a `TExprG.New` call site
            // pick the matching overload; the declared param types carry
            // declaring-typar markers so a generic call site can mint a
            // `MemberRef` on the instantiated `TypeSpec`.
            let secondaryCtorHandles =
                secondaryCtors
                |> List.mapi (fun i (sc: Frozen.TSecondaryCtor) ->
                    sc.Params.Length,
                    [ for (_, t) in sc.Params -> t ],
                    toEntity (asm.MethodDef(MethodKey.SecondaryCtor(td.Key, i)))
                )

            asm.Classes.[td.Key] <-
                {
                    Name = td.Name
                    Typars = EqArray.toList td.TypeParams
                    Fields =
                        [
                            for p in ctorParams ->
                                p.Name, toEntity (asm.FieldDef(FieldKey.ClassCtorParamField(td.Key, p.Name))), p.Type
                        ]
                    InstanceFields =
                        [
                            for f in instanceFields ->
                                f.Name, toEntity (asm.FieldDef(FieldKey.ClassInstanceField(td.Key, f.Name))), f.Type
                        ]
                    IsValueType = isStruct
                    Ctor = toEntity (asm.MethodDef(MethodKey.NominalCtor td.Key))
                    Members = emittedMembers
                    StaticFields = staticFieldsDict
                    SecondaryCtors = secondaryCtorHandles
                }

    let prepare
        (asm: Assembler)
        (input: NominalEmissionInput)
        (td: Frozen.TTypeDecl)
        (members: Frozen.TTypeMember list)
        : unit =
        let provider = asm.Provider
        let icodegen = asm.Icodegen
        let ctx = asm.Ctx
        let bodyStream = asm.BodyStream
        let encodeLocals = asm.EncodeLocals
        let emitCtx = asm.EmitCtx
        let unions = asm.Unions
        let records = asm.Records
        let classes = asm.Classes

        let isGeneric = not td.TypeParams.IsEmpty
        let typarMarkers = typarMarkersOf td

        // A reference to one of *this* type's own members (field / tag / ctor).
        // A generic type reaches it through a `MemberRef` on the open
        // self-`TypeSpec` (`Box\`1<!0>::n`, G13 / vesper-set-sprint-plan §1.11);
        // a monomorphic type uses the resolved `Def` token. The mono handle is a
        // cheap registry/layout lookup, so eager evaluation in the generic branch
        // is free.
        let selfMemberRef (kind: UserMemberKind) (monoHandle: EntityHandle) : EntityHandle =
            if isGeneric then
                icodegen.UserGenericMemberRef(td.Key, typarMarkers, kind)
            else
                monoHandle

        // A member (or secondary-ctor) body local of a generic type carries its
        // declaring typars as `FTTypar(Declaring, i)` nodes the encoder resolves
        // to `!i` directly, so the generic and monomorphic paths are identical —
        // the one `encodeLocals` covers both.

        // The IL base type for this `TypeDefinition`. Defaults to `Object`; the
        // class arm overwrites it with the parent's `TypeSpec` for an `inherit`
        // clause (B-4 Step 2.5). Resolved here (not in `Finalise`) so a generic
        // parent encodes against this class's typars while they are ambient.
        let mutable baseTypeHandle = provider.ObjectType

        match input with
        | NominalEmissionInput.Union cases ->
            let tagField = toEntity (asm.FieldDef(FieldKey.UnionTag td.Key))
            let unionCtor = toEntity (asm.MethodDef(MethodKey.NominalCtor td.Key))

            let ctorBodyOffset =
                Cil.buildBody encodeLocals bodyStream (IlIr.lower (Emit.buildClosureCtor provider.ObjectCtorRef []))

            asm.AddPrepared(
                MethodKey.NominalCtor td.Key,
                {
                    Signature = provider.NullaryCtorSignature()
                    BodyOffset = ctorBodyOffset
                    ParamNames = []
                    MethodTypars = []
                }
            )

            cases
            |> List.iteri (fun tag c ->
                let fieldHandles =
                    [
                        for fi in 0 .. c.Fields.Length - 1 ->
                            toEntity (asm.FieldDef(FieldKey.UnionCaseField(td.Key, c.Name, fi)))
                    ]

                let ctorRef = selfMemberRef (UserMemberKind.UnionMember UnionMember.Ctor) unionCtor
                let tagRef = selfMemberRef (UserMemberKind.UnionMember UnionMember.Tag) tagField

                let fieldRefs =
                    [
                        for fi in 0 .. List.length fieldHandles - 1 ->
                            selfMemberRef (UserMemberKind.UnionMember(UnionMember.Field(c.Name, fi))) fieldHandles.[fi]
                    ]

                let factoryBody =
                    Cil.buildBody
                        encodeLocals
                        bodyStream
                        (IlIr.lower (Emit.buildUnionFactory ctorRef tag tagRef fieldRefs))

                let paramTys = [ for (_, t) in c.Fields -> t ]

                // A mono union's `typarMarkers` is empty, so the self return type is
                // `FTUnion(td.Key, [])` — one path covers both.
                let factorySig =
                    provider.StaticMethodSignature(paramTys, FTUnion(td.Key, EqArray.ofList typarMarkers))

                asm.AddPrepared(
                    MethodKey.UnionFactory(td.Key, c.Name),
                    {
                        Signature = factorySig
                        BodyOffset = factoryBody
                        ParamNames = argNames (List.length paramTys)
                        MethodTypars = []
                    }
                )
            )

        | NominalEmissionInput.Record fields ->
            let fieldHandles =
                [
                    for f in fields -> toEntity (asm.FieldDef(FieldKey.RecordField(td.Key, f.Name)))
                ]

            // Generic records share the §1.11 / B-1 ctor-store hazard with
            // classes: a raw `FieldDefinition` token in `stfld` resolves to the
            // wrong slot for a field at index >= 1 of a generic type. `selfMemberRef`
            // routes each generic store through the field's `MemberRef` on the open
            // self-`TypeSpec` (`R\`1<!0>::Y`); monomorphic records keep the `Def`
            // token.
            let ctorFieldRefs =
                [
                    for i, f in List.indexed fields ->
                        selfMemberRef (UserMemberKind.RecordMember(RecordMember.Field f.Name)) fieldHandles.[i]
                ]

            let ctorBodyOffset =
                Cil.buildBody
                    encodeLocals
                    bodyStream
                    (IlIr.lower (Emit.buildRecordCtor provider.ObjectCtorRef ctorFieldRefs))

            asm.AddPrepared(
                MethodKey.NominalCtor td.Key,
                {
                    Signature = provider.RecordCtorSignature [ for f in fields -> f.Type ]
                    BodyOffset = ctorBodyOffset
                    ParamNames = [ for f in fields -> f.Name ]
                    MethodTypars = []
                }
            )

        | NominalEmissionInput.Class(instanceFields,
                                     ctorParams,
                                     baseType,
                                     _isSealed,
                                     staticLets,
                                     secondaryCtors,
                                     baseCtorCall,
                                     _interfaces,
                                     isStruct) ->
            // Resolve the parent handle for the IL `TypeDefinition.BaseType`
            // (B-4 Step 2.5). A non-generic parent (`Shape`) is the parent's
            // `TypeDefinition` token directly — the base-type column rejects a
            // `TypeSpec` that merely wraps a plain class. An instantiated generic
            // parent (`Box<int>` / `SetTree\`1<!0>`) needs a `GENERICINST`
            // `TypeSpec`, encoded with this class's typars ambient so an open
            // parent arg resolves to `!i`. Parent-less ⇒ `Object` (the default).
            match baseType with
            | ValueSome(FTClass(baseKey, baseArgs)) when baseArgs.IsEmpty ->
                baseTypeHandle <- provider.UserTypeHandle baseKey
            | ValueSome bt ->
                // A generic parent's open args ride `FTTypar(Declaring, i)` nodes
                // (Freeze remaps `info.BaseType`), encoded `!i` directly — no window.
                baseTypeHandle <- icodegen.TypeToken bt
            | ValueNone ->
                // A `[<Struct>]` value type extends `System.ValueType`; a plain
                // parent-less class keeps the `Object` default. v1 structs never
                // carry an `inherit`, so this is the only struct base path.
                if isStruct then
                    baseTypeHandle <- provider.ValueTypeBase

            let classCtor = toEntity (asm.MethodDef(MethodKey.NominalCtor td.Key))

            // A *generic* class's ctor `stfld` sequence must reference each
            // field through a `MemberRef` on the open self-`TypeSpec`
            // (`Box\`1<!0>::n`), not the raw `FieldDefinition` token — the
            // generic-union/record field paths already do this. The single-
            // field case happened to work with the raw `Def` token because
            // field 0 aliases the type's first slot, but a field at index >= 1
            // resolves to the wrong slot at runtime (vesper-set-sprint-plan
            // §1.11 / B-1). Member-body `FieldGet`/`FieldSet` already route
            // through `EmitResolve.resolveRecordField`'s `MemberRef`; `selfMemberRef`
            // closes the matching gap on the ctor stores.
            let ctorFieldRefs =
                [
                    for p in ctorParams ->
                        selfMemberRef
                            (UserMemberKind.ClassMember(ClassMember.Field p.Name))
                            (toEntity (asm.FieldDef(FieldKey.ClassCtorParamField(td.Key, p.Name))))
                ]

            // The primary `.ctor` body. Without an `inherit` clause it chains to
            // `Object` (the record/closure recipe). With one (B-4 Step 2.5) it
            // chains to the parent's `.ctor` with the `inherit Base(args)` args
            // before the field stores; the parent's primary `.ctor` is its `Def`
            // token (mono parent) or a `MemberRef` on the parent's `TypeSpec`
            // (generic parent). v1 inherits only from a project-local class —
            // every class is already in the registry (Bind pre-fills it).
            let ctorBody =
                match baseCtorCall with
                | ValueSome bcc ->
                    let baseKey, baseArgs =
                        match baseType with
                        | ValueSome(FTClass(n, xs)) -> n, EqArray.toList xs
                        | _ -> failwithf "Emit: class '%s' has a base-ctor call but no class base type" td.Name

                    let baseCtorHandle =
                        match classes.TryGetValue baseKey with
                        | true, bc when List.isEmpty bc.Typars -> bc.Ctor
                        | true, _ ->
                            icodegen.UserGenericMemberRef(
                                baseKey,
                                baseArgs,
                                UserMemberKind.ClassMember ClassMember.Ctor
                            )
                        | false, _ ->
                            failwithf
                                "Emit: base class '%A' of '%s' is not an emitted project-local class"
                                baseKey
                                td.Name

                    Emit.buildClassBaseCtor
                        emitCtx
                        baseCtorHandle
                        (EqArray.toList bcc.Args)
                        (EqArray.toList bcc.CtorParams)
                        ctorFieldRefs
                | ValueNone when isStruct ->
                    // A value-type ctor stores its params and returns — no
                    // `System.ValueType::.ctor` chain (value types don't chain).
                    Emit.buildStructCtor ctorFieldRefs
                | ValueNone -> Emit.buildRecordCtor provider.ObjectCtorRef ctorFieldRefs

            let ctorBodyOffset = Cil.buildBody encodeLocals bodyStream (IlIr.lower ctorBody)

            asm.AddPrepared(
                MethodKey.NominalCtor td.Key,
                {
                    Signature = provider.RecordCtorSignature [ for p in ctorParams -> p.Type ]
                    BodyOffset = ctorBodyOffset
                    ParamNames = [ for p in ctorParams -> p.Name ]
                    MethodTypars = []
                }
            )

            // The synthesised `.cctor` initialises the `static let` backing
            // fields in declaration order. An initialiser referencing an earlier
            // `static let` (lowered to `StaticFieldGet`) resolves through the
            // registry `register` filled.
            if not (List.isEmpty staticLets) then
                let staticFields = classes.[td.Key].StaticFields

                let cctorInits =
                    [
                        for sl in staticLets ->
                            // Inline splicing ran pre-freeze (Passes.InlineExpansion);
                            // codegen only collapses the residual saturated built-in ops.
                            staticFields.[sl.Name], Emit.expandBuiltinOps sl.Init
                    ]

                let cctorBody =
                    Cil.buildBody encodeLocals bodyStream (IlIr.lower (Emit.buildStaticCctor emitCtx cctorInits))

                asm.AddPrepared(
                    MethodKey.NominalCctor td.Key,
                    {
                        Signature = provider.CctorSignature()
                        BodyOffset = cctorBody
                        ParamNames = []
                        MethodTypars = []
                    }
                )

            // Secondary constructors (B-11). Each is a `.ctor` overload whose
            // body runs its `let`-preamble then chains to the primary `.ctor`
            // (or stores explicit field inits); the chain target is the primary
            // ctor's `Def` token (monomorphic) or a `MemberRef` on the open
            // self-`TypeSpec` (generic).
            if not (List.isEmpty secondaryCtors) then
                let primaryCtorRef =
                    selfMemberRef (UserMemberKind.ClassMember ClassMember.Ctor) classCtor

                secondaryCtors
                |> List.iteri (fun i sc ->
                    let paramTys = [ for (_, t) in sc.Params -> t ]

                    // Inline splicing ran pre-freeze (Passes.InlineExpansion);
                    // codegen only collapses the residual saturated built-in ops.
                    let prep (e: Frozen.TExpr) = Emit.expandBuiltinOps e

                    let lets = [ for l in sc.Lets -> { l with Init = prep l.Init } ]

                    // A secondary ctor takes one of two forms (Tast
                    // `TSecondaryCtorG`): the explicit field-init form stores
                    // into declared fields and skips the primary chain;
                    // otherwise it chains to the primary `.ctor`.
                    let ctorIr =
                        if not sc.FieldInits.IsEmpty then
                            // Resolve each named field to its handle — a generic
                            // class routes through a `MemberRef` on the open
                            // self-`TypeSpec` (as `ctorFieldRefs` does); a mono
                            // class uses the raw `Def` token. Both ctor-param
                            // backing fields and explicit `val` fields are eligible.
                            let fieldHandleOf name =
                                if isGeneric then
                                    icodegen.UserGenericMemberRef(
                                        td.Key,
                                        typarMarkers,
                                        UserMemberKind.ClassMember(ClassMember.Field name)
                                    )
                                elif ctorParams |> List.exists (fun (p: Frozen.TRecordField) -> p.Name = name) then
                                    toEntity (asm.FieldDef(FieldKey.ClassCtorParamField(td.Key, name)))
                                elif instanceFields |> List.exists (fun (f: Frozen.TRecordField) -> f.Name = name) then
                                    toEntity (asm.FieldDef(FieldKey.ClassInstanceField(td.Key, name)))
                                else
                                    failwithf "Emit: class '%s' secondary ctor inits unknown field '%s'" td.Name name

                            let fieldInits = [ for fi in sc.FieldInits -> fieldHandleOf fi.Field, prep fi.Init ]

                            Emit.buildSecondaryCtorFieldInit emitCtx sc.Params lets fieldInits
                        else
                            let primaryArgs = [ for a in sc.PrimaryArgs -> prep a ]
                            Emit.buildSecondaryCtor emitCtx sc.Params lets primaryCtorRef primaryArgs

                    let scBody = Cil.buildBody encodeLocals bodyStream (IlIr.lower ctorIr)

                    asm.AddPrepared(
                        MethodKey.SecondaryCtor(td.Key, i),
                        {
                            Signature = provider.RecordCtorSignature paramTys
                            BodyOffset = scBody
                            ParamNames = argNames sc.Params.Length
                            MethodTypars = []
                        }
                    )
                )

        // Interface implementations (B-2, §5.3): each `(ifaceTy, members)` entry's
        // member bodies are already-typed `Frozen.TTypeMember`s, flattened here. They
        // emit as virtual methods (`ifaceEqualsAttrs` — a new slot, `Final` since
        // classes are sealed) that the runtime binds to the `InterfaceImpl` row by
        // name + signature. The class's own members lead, interface-impl members
        // trail — the same indexing the layout's `MethodKey.Member` rows use.
        let classInterfaces =
            match input with
            | NominalEmissionInput.Class(_, _, _, _, _, _, _, interfaces, _) -> interfaces
            | _ -> []

        let ifaceMembers = ifaceMembersOf input

        // `isIfaceImpl` selects the `void`-return conformance below; the row's
        // attrs were fixed by the layout's enumeration.
        let prepareMember (index: int) (isIfaceImpl: bool) (mem: Frozen.TTypeMember) =
            // A *generic* member (B-12). Both its declaring-type typars and its own
            // method typars now ride self-describing `TyTypar` nodes in the signature /
            // locals / body (Freeze.remapMemberTypes remaps both axes), so no ambient
            // typar window is installed; the encoder resolves them by
            // index. `methodTypars` still feeds the `GENERIC` header arity and the
            // `GenericParam` rows.
            let methodTypars = mem.MethodTypeParams
            let isGenericMethod = not methodTypars.IsEmpty

            // An interface-impl member conforming to a `void` BCL slot
            // (`IDisposable.Dispose` / `IEnumerator.Reset`): its `unit` return must
            // encode as genuine `void` to bind to the slot, and its body must `ret`
            // empty-stacked (pop the residual `unit`-as-value). Only interface
            // impls get this — a class's own `unit`-returning method keeps the
            // `unit`-as-`ValueTuple` convention its callers expect.
            let returnsVoid =
                isIfaceImpl
                && (
                    match mem.ReturnTy with
                    | FTConst("unit", _) -> true
                    | _ -> false
                )

            let bodyOffset =
                Cil.buildBody
                    encodeLocals
                    bodyStream
                    (IlIr.lower (
                        Emit.buildMember
                            emitCtx
                            mem.ThisKey
                            mem.BaseKey
                            mem.Params
                            returnsVoid
                            // Inline splicing ran pre-freeze (Passes.InlineExpansion);
                            // codegen only collapses the residual saturated built-in ops.
                            (Emit.expandBuiltinOps mem.Body)
                    ))

            let paramTys = [ for (_, t) in mem.Params -> t ]

            // The declaring type's typars (if any) ride `FTTypar(Declaring, i)` nodes
            // the encoder resolves to `!i` directly, so a generic and a monomorphic
            // type share one signature builder. A generic *method* (B-12) additionally
            // needs the `GENERIC` calling-convention header count; its own typars ride
            // `FTTypar(Method, i)` nodes the encoder resolves to `!!i` (no window).
            let signature =
                try
                    if returnsVoid then
                        // Interface-impl member conforming to a `void` slot — `void`
                        // return, not the `unit`-as-`ValueTuple` the general path emits.
                        provider.InstanceMethodSignatureVoid paramTys
                    elif isGenericMethod then
                        provider.GenericMethodOnTypeSignature(
                            methodTypars.Length,
                            paramTys,
                            mem.ReturnTy,
                            not mem.IsStatic
                        )
                    elif mem.IsStatic then
                        provider.StaticMethodSignature(paramTys, mem.ReturnTy)
                    else
                        provider.InstanceMethodSignature(paramTys, mem.ReturnTy)
                with ex ->
                    // A leaked metavar / unresolved head in a member signature surfaces
                    // here as a generic encoder failure; name the member + declaring
                    // type so the front-end grounding gap is pinpointable rather than
                    // anonymous (vesper-set Phase 9 contract-extraction wall).
                    failwithf "%s (while encoding signature of member '%A.%s')" ex.Message td.Key mem.Name

            asm.AddPrepared(
                MethodKey.Member(td.Key, index),
                {
                    Signature = signature
                    BodyOffset = bodyOffset
                    ParamNames = argNames mem.Params.Length
                    // The method's own typars are owned by this `MethodDef` (the
                    // metadata name drops the F# leading quote, like every other
                    // generic-param row).
                    MethodTypars = [ for (n, _) in methodTypars -> n.TrimStart('\'') ]
                }
            )

        members |> List.iteri (fun i mem -> prepareMember i false mem)

        ifaceMembers
        |> List.iteri (fun i mem -> prepareMember (List.length members + i) true mem)

        // This type as a `FrozenType`, parameterised over its declaring typars.
        // A mono type's `typarMarkers` is empty, so `selfTyMarkers` collapses to
        // `FT…(td.Key, [])` — one path covers both. Shared by the equality
        // triple, the comparison pair, and the synthesised interface specs.
        let selfTy (ts: FrozenType list) : FrozenType =
            match input with
            | NominalEmissionInput.Union _ -> FTUnion(td.Key, EqArray.ofList ts)
            | NominalEmissionInput.Record _ -> FTRecord(td.Key, EqArray.ofList ts)
            | NominalEmissionInput.Class _ -> FTClass(td.Key, EqArray.ofList ts)

        let selfTyMarkers = selfTy typarMarkers

        // The metadata handle the equality/comparison bodies `box`/`unbox` /
        // `call` against: a generic type's open self-`TypeSpec`, a mono type's
        // `TypeDef`.
        let selfTypeHandle =
            if not isGeneric then
                provider.UserTypeHandle td.Key
            else
                match input with
                | NominalEmissionInput.Union _ -> provider.GenericUnionSelfSpec td.Key
                | NominalEmissionInput.Record _ -> provider.GenericRecordSelfSpec td.Key
                | NominalEmissionInput.Class _ -> provider.UserTypeHandle td.Key

        // The structural field set as `(handle, type)`, flat across a union's
        // cases in declaration order — sound because inactive-case fields are
        // always default (see `Emit.UnionEqualitySupport`). `selfMemberRef`
        // routes a generic type's field through its self-`TypeSpec` `MemberRef`.
        // Equality and comparison consume the identical set.
        let structuralFields () : (EntityHandle * FrozenType) list =
            match input with
            | NominalEmissionInput.Union cases ->
                let emitted = unions.[td.Key]

                [
                    for c in cases do
                        let caseFields = emitted.Cases.[c.Name].Fields

                        for fi in 0 .. c.Fields.Length - 1 ->
                            selfMemberRef (UserMemberKind.UnionMember(UnionMember.Field(c.Name, fi))) caseFields.[fi],
                            snd c.Fields.[fi]
                ]
            | NominalEmissionInput.Record _ ->
                [
                    for (name, h, fty) in records.[td.Key].Fields ->
                        selfMemberRef (UserMemberKind.RecordMember(RecordMember.Field name)) h, fty
                ]
            | NominalEmissionInput.Class _ -> []

        // A union's `_tag` field reference (generic ⇒ self-`TypeSpec` `MemberRef`).
        let tagFieldRef () =
            selfMemberRef (UserMemberKind.UnionMember UnionMember.Tag) unions.[td.Key].TagField

        let bodyOf ir =
            Cil.buildBody encodeLocals bodyStream (IlIr.lower ir)

        // The equality triple in `NominalEmit` emission order: `GetHashCode` +
        // `Equals(object)` override + typed `Equals(Self)` interface impl. Union
        // and record differ only in the `support` shape + body builders; the row
        // signatures / param names are identical.
        let prepareEqualityTriple getHashCodeIr equalsObjIr equalsTypedIr =
            asm.AddPrepared(
                MethodKey.EqGetHashCode td.Key,
                {
                    Signature = provider.GetHashCodeOverrideSignature()
                    BodyOffset = bodyOf getHashCodeIr
                    ParamNames = []
                    MethodTypars = []
                }
            )

            asm.AddPrepared(
                MethodKey.EqEqualsObj td.Key,
                {
                    Signature = provider.EqualsOverrideSignature()
                    BodyOffset = bodyOf equalsObjIr
                    ParamNames = [ "obj" ]
                    MethodTypars = []
                }
            )

            asm.AddPrepared(
                MethodKey.EqEqualsTyped td.Key,
                {
                    Signature = provider.EqualsTypedSignature selfTyMarkers
                    BodyOffset = bodyOf equalsTypedIr
                    ParamNames = [ "other" ]
                    MethodTypars = []
                }
            )

        // The comparison pair: typed `CompareTo(Self)` first (its handle feeds
        // `CompareTo(object)`'s body), then the `CompareTo(object)` override.
        let prepareComparisonPair compareToTypedIr compareToObjIr =
            asm.AddPrepared(
                MethodKey.CmpCompareToTyped td.Key,
                {
                    Signature = provider.CompareToTypedSignature selfTyMarkers
                    BodyOffset = bodyOf compareToTypedIr
                    ParamNames = [ "other" ]
                    MethodTypars = []
                }
            )

            asm.AddPrepared(
                MethodKey.CmpCompareToObj td.Key,
                {
                    Signature = provider.CompareToOverrideSignature()
                    BodyOffset = bodyOf compareToObjIr
                    ParamNames = [ "obj" ]
                    MethodTypars = []
                }
            )

        let emitsEqualityTriple = td.EqualitySupport = EqualityVerdict.Structural

        if emitsEqualityTriple then
            match input with
            | NominalEmissionInput.Union _ ->
                let support: Emit.UnionEqualitySupport =
                    {
                        SelfType = selfTypeHandle
                        SelfTy = selfTyMarkers
                        TagField = tagFieldRef ()
                        Fields = structuralFields ()
                        IntType = FTConst("int", EqArray.empty)
                        ComparerDefault = fun t -> provider.EqualityComparerDefault t
                        ComparerEquals = fun t -> provider.EqualityComparerEquals t
                        HashCodeLocal = provider.HashCodeType
                        HashCodeAdd = fun t -> provider.HashCodeAdd t
                        HashCodeToHashCode = provider.HashCodeToHashCode
                    }

                prepareEqualityTriple
                    (Emit.buildUnionGetHashCode support)
                    (Emit.buildUnionEquals support)
                    (Emit.buildUnionEqualsTyped support)
            | NominalEmissionInput.Record _ ->
                let support: Emit.RecordEqualitySupport =
                    {
                        SelfType = selfTypeHandle
                        SelfTy = selfTyMarkers
                        Fields = structuralFields ()
                        ComparerDefault = fun t -> provider.EqualityComparerDefault t
                        ComparerEquals = fun t -> provider.EqualityComparerEquals t
                        HashCodeLocal = provider.HashCodeType
                        HashCodeAdd = fun t -> provider.HashCodeAdd t
                        HashCodeToHashCode = provider.HashCodeToHashCode
                    }

                prepareEqualityTriple
                    (Emit.buildRecordGetHashCode support)
                    (Emit.buildRecordEquals support)
                    (Emit.buildRecordEqualsTyped support)
            | NominalEmissionInput.Class _ -> ()

        let emitsComparisonPair = td.ComparisonSupport = ComparisonVerdict.Structural

        if emitsComparisonPair then
            // The typed `CompareTo(Self)` handle feeds `CompareTo(object)`'s
            // body — a within-type forward reference that is an ordinary
            // layout lookup.
            let typedCompareTo = toEntity (asm.MethodDef(MethodKey.CmpCompareToTyped td.Key))

            match input with
            | NominalEmissionInput.Union _ ->
                let cmpSupport: Emit.UnionComparisonSupport =
                    {
                        SelfType = selfTypeHandle
                        SelfTy = selfTyMarkers
                        TagField = tagFieldRef ()
                        Fields = structuralFields ()
                        ComparerDefault = fun t -> provider.ComparerDefault t
                        ComparerCompare = fun t -> provider.ComparerCompare t
                        ArgumentExceptionCtor = provider.ArgumentExceptionCtor
                        MismatchMessage = ctx.UserString "Object type mismatch"
                    }

                prepareComparisonPair
                    (Emit.buildUnionCompareTo cmpSupport)
                    (Emit.buildUnionCompareToObj cmpSupport typedCompareTo)
            | NominalEmissionInput.Record _ ->
                let cmpSupport: Emit.RecordComparisonSupport =
                    {
                        SelfType = selfTypeHandle
                        SelfTy = selfTyMarkers
                        Fields = structuralFields ()
                        ComparerDefault = fun t -> provider.ComparerDefault t
                        ComparerCompare = fun t -> provider.ComparerCompare t
                        ArgumentExceptionCtor = provider.ArgumentExceptionCtor
                        MismatchMessage = ctx.UserString "Object type mismatch"
                    }

                prepareComparisonPair
                    (Emit.buildRecordCompareTo cmpSupport)
                    (Emit.buildRecordCompareToObj cmpSupport typedCompareTo)
            | NominalEmissionInput.Class _ -> ()

        // One `InterfaceImpl` entity handle per implemented interface — the
        // synthesised structural-equality / comparison interfaces (unions /
        // records) and the user-declared `interface … with` impls (B-2, §5.3,
        // classes). A generic interface arg (`IEnumerable<'T>`) carries its `'T`
        // as a `FTTypar(Declaring, i)` (emitted by Freeze; `selfTyMarkers` for
        // the synthesised interfaces), encoded `!i` straight off the node — no
        // ambient window. `TypeSpecOf` mints the user interfaces' handles.
        let interfaces =
            if emitsEqualityTriple || emitsComparisonPair || not (List.isEmpty classInterfaces) then
                [
                    if emitsEqualityTriple then
                        provider.EquatableInterfaceSpec selfTyMarkers
                    if emitsComparisonPair then
                        provider.ComparableInterfaceSpec selfTyMarkers
                        provider.IComparableType
                    for (ifaceTy, _) in classInterfaces do
                        provider.InterfaceHandleOf ifaceTy
                ]
            else
                []

        asm.AddTypeRowExtras(
            TypeKey.Nominal td.Key,
            {
                Interfaces = interfaces
                BaseType = baseTypeHandle
            }
        )
