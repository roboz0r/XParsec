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

    /// The resolved `inherit` parent of a nominal class, classified ONCE so the
    /// `extends` column and the primary-ctor chain target both read off a single
    /// decision instead of each re-destructuring `baseType`.
    type private BaseShape =
        /// No `inherit` clause (a plain class chains to `Object`; a struct extends
        /// `System.ValueType` — handled at the `extends` site, not here).
        | NoBase
        /// A non-generic HERITABLE external base — an external `FTClass`
        /// (`inherit Attribute`, where `Attribute = (# class "System.Attribute" #)`)
        /// or an intrinsic-class canon resolved to its platform class
        /// (`inherit exn` → `System.Exception`): `extends` its raw external
        /// `TypeRef`; the primary ctor chains to its `.ctor` — the overload picked
        /// from the `inherit` args, or the parameterless `.ctor()` when there are
        /// none. `key` (the PLATFORM key) mints that base ctor; `tref` is the
        /// resolved `extends` token.
        | ExternalBase of key: SymbolKey * tref: EntityHandle
        /// A non-generic project-local base: `extends` its `TypeDefinition` token.
        | LocalMono of key: SymbolKey
        /// A generic parent (`Box<int>`) — or any non-`FTClass` base type:
        /// `extends` a `GENERICINST` `TypeSpec`, encoded with this class's typars
        /// ambient so an open parent arg resolves to `!i`.
        | Generic of ft: FrozenType

    /// The user `interface … with` impls (interface type + member bodies) a
    /// nominal carries. Classes, unions, and records all carry them.
    let private userInterfacesOf (input: NominalEmissionInput) : (FrozenType * Frozen.TTypeMember list) list =
        match input with
        | NominalEmissionInput.Class(_, _, _, _, _, _, _, interfaces, _, _) -> interfaces
        | NominalEmissionInput.Union(_, interfaces) -> interfaces
        | NominalEmissionInput.Record(_, interfaces) -> interfaces

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
        // a case factory (`Empty = Nil`).
        let emittedMembers = Dictionary<string, Emit.EmittedMember list>()

        // The class's own members lead, interface-impl members trail (same
        // indexing as the layout's `MethodKey.Member` rows). Every member still
        // gets its own indexed method row; this name→members map drives only
        // *name-based* resolution (`resolveInstanceMember`/`resolveStaticMember`
        // for a `this.Member` / `Set<'T>.Member` access on the class receiver).
        //
        // A name maps to a *list* of overloads in declaration order, so an
        // overloaded member (`AppendFormatted(value:'T)` / `(value:'T,
        // alignment:int)` / `(value:'T, alignment:int, format:string)`) keeps every
        // signature; the call site picks by argument types (ECMA-335 §I.10.2 — CLS
        // overloading is by number + types of parameters; `EmitResolve.pickOverload`).
        //
        // Declaration order also resolves the *interface-impl name collision*: a
        // class member and an interface-impl member can share a name AND signature
        // (`Set` has both its own `Add : Set<'T>` and `ICollection<'T>.Add : unit`,
        // both arity 1, param `'T`). The class's own member must win — `set.Add
        // value` resolves to it in the front end, and the interface slot is only
        // ever reached through an interface-typed receiver (the external dispatch
        // path), never this table. Own members lead the list and `pickOverload`
        // prefers the first equally-good match, so the own member wins on a tie.
        (members @ NominalMembers.flattenIfaceMembers (userInterfacesOf input))
        |> List.iteri (fun i (mem: Frozen.TTypeMember) ->
            let em: Emit.EmittedMember =
                {
                    Handle = toEntity (asm.MethodDef(MethodKey.Member(td.Key, i)))
                    IsStatic = mem.IsStatic
                    Arity = mem.Params.Length
                    MetaName = memberMetaName mem
                    ParamTys = [ for (_, t) in mem.Params -> t ]
                    RetTy = mem.ReturnTy
                    MethodTyparCount = GeneralizedTypars.count mem.MethodTypeParams
                }

            let prior =
                match emittedMembers.TryGetValue mem.Name with
                | true, ms -> ms
                | false, _ -> []

            emittedMembers.[mem.Name] <- prior @ [ em ]
        )

        match input with
        | NominalEmissionInput.Union(cases, _) ->
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

        | NominalEmissionInput.Record(fields, _) ->
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

        | NominalEmissionInput.Class(instanceFields,
                                     ctorParams,
                                     _,
                                     _,
                                     staticLets,
                                     secondaryCtors,
                                     _,
                                     interfaces,
                                     isStruct,
                                     hasPrimaryCtor) ->
            // The handle every `ldsfld`/`stsfld` *references*. A generic class
            // reaches its own `static let` field through a `MemberRef` on the
            // open self-`TypeSpec` (`Set\`1<!0>::empty`), the static analogue of
            // the ctor-field `MemberRef`s; a mono class uses the `Def`
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

            // The val-field *reference* form (no primary ctor) emits no `NominalCtor`
            // row (see `Layout`), so don't reserve its handle — `Ctor` aliases the
            // first secondary (never dereferenced as a primary: construction resolves
            // to a secondary by arity, and only a class WITH a primary has chaining
            // secondaries that read `Ctor`). Structs always keep their primary, as
            // does the no-secondary fallback — matching `Layout`'s `emitPrimaryCtor`.
            let emitPrimaryCtor =
                isStruct || hasPrimaryCtor || List.isEmpty secondaryCtorHandles

            let ctorHandle =
                if emitPrimaryCtor then
                    toEntity (asm.MethodDef(MethodKey.NominalCtor td.Key))
                else
                    let (_, _, h) = List.head secondaryCtorHandles
                    h

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
                    Ctor = ctorHandle
                    HasPrimaryCtor = emitPrimaryCtor
                    Members = emittedMembers
                    StaticFields = staticFieldsDict
                    SecondaryCtors = secondaryCtorHandles
                    // The implemented-interface templates over this class's declaring
                    // typars (the `fst` of each impl pair — the member bodies are not
                    // needed for the witness walk). Same source the definition emission
                    // reads at `classInterfaces`.
                    Interfaces = [ for (ifaceTy, _) in interfaces -> ifaceTy ]
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
        // self-`TypeSpec` (`Box\`1<!0>::n`);
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
        // clause. Resolved here (not in `Finalise`) so a generic
        // parent encodes against this class's typars while they are ambient.
        let mutable baseTypeHandle = provider.ObjectType

        match input with
        | NominalEmissionInput.Union(cases, _) ->
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

        | NominalEmissionInput.Record(fields, _) ->
            let fieldHandles =
                [
                    for f in fields -> toEntity (asm.FieldDef(FieldKey.RecordField(td.Key, f.Name)))
                ]

            // Generic records share the ctor-store hazard with
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
                                     isStruct,
                                     hasPrimaryCtor) ->
            // Classify the `inherit` parent once. A non-generic external base
            // (`inherit Attribute`) resolves to its raw external `TypeRef`; a
            // non-generic project-local base to its `TypeDefinition` token. External
            // detection keys off `ExternalClassTypeRef` returning a token — a
            // project-local key is never in the provider's external table.
            let baseShape =
                match baseType with
                | ValueNone -> BaseShape.NoBase
                | ValueSome(FTClass(baseKey, baseArgs)) when baseArgs.IsEmpty ->
                    match icodegen.ExternalClassTypeRef baseKey with
                    | ValueSome tref -> BaseShape.ExternalBase(baseKey, tref)
                    | ValueNone -> BaseShape.LocalMono baseKey
                // An intrinsic-class parent (`inherit exn`) arrives as the canon
                // `FTConst`, not an `FTClass` — resolve it to its platform external
                // class (`System.Exception`), whose key mints the base-ctor and whose
                // raw `TypeRef` is the `extends` token.
                | ValueSome(FTConst(canonKey, args) as bt) when args.IsEmpty ->
                    match icodegen.IntrinsicClassBase canonKey with
                    | ValueSome(platformKey, tref) -> BaseShape.ExternalBase(platformKey, tref)
                    | ValueNone -> BaseShape.Generic bt
                | ValueSome bt -> BaseShape.Generic bt

            // The IL `TypeDefinition.BaseType` (`extends`) column. A non-generic
            // parent is its token directly — the column rejects a `TypeSpec` that
            // merely wraps a plain class. A generic parent (`Box<int>` /
            // `SetTree\`1<!0>`) needs a `GENERICINST` `TypeSpec`. Parent-less ⇒
            // `Object` (the default already in `baseTypeHandle`), except a struct,
            // which extends `System.ValueType`. v1 structs never carry an `inherit`,
            // so the struct base is only reachable through `NoBase`.
            match baseShape with
            | BaseShape.NoBase ->
                if isStruct then
                    baseTypeHandle <- provider.ValueTypeBase
            | BaseShape.ExternalBase(_, tref) -> baseTypeHandle <- tref
            | BaseShape.LocalMono baseKey -> baseTypeHandle <- provider.UserTypeHandle baseKey
            | BaseShape.Generic bt -> baseTypeHandle <- icodegen.TypeToken bt

            // The val-field *reference* form (no primary ctor) emits no primary
            // `.ctor` — its secondaries are the only ctors (matches `Layout`'s
            // `emitPrimaryCtor` and the `register` handle reservation). Structs and
            // the no-secondary fallback keep the synthesised primary.
            let emitPrimaryCtor = isStruct || hasPrimaryCtor || List.isEmpty secondaryCtors

            // `classCtor` is the chain target for a secondary ctor that chains to the
            // primary; those occur only when a primary exists. For the suppressed
            // val-field form it aliases the first secondary's `.ctor` so the (unused)
            // `primaryCtorRef` resolves to a real token rather than reserving an
            // absent `NominalCtor` handle.
            let classCtor =
                if emitPrimaryCtor then
                    toEntity (asm.MethodDef(MethodKey.NominalCtor td.Key))
                else
                    toEntity (asm.MethodDef(MethodKey.SecondaryCtor(td.Key, 0)))

            // A *generic* class's ctor `stfld` sequence must reference each
            // field through a `MemberRef` on the open self-`TypeSpec`
            // (`Box\`1<!0>::n`), not the raw `FieldDefinition` token — the
            // generic-union/record field paths already do this. The single-
            // field case happened to work with the raw `Def` token because
            // field 0 aliases the type's first slot, but a field at index >= 1
            // resolves to the wrong slot at runtime. Member-body `FieldGet`/`FieldSet` already route
            // through `EmitResolve.resolveRecordField`'s `MemberRef`; `selfMemberRef`
            // closes the matching gap on the ctor stores.
            let ctorFieldRefs =
                [
                    for p in ctorParams ->
                        selfMemberRef
                            (UserMemberKind.ClassMember(ClassMember.Field p.Name))
                            (toEntity (asm.FieldDef(FieldKey.ClassCtorParamField(td.Key, p.Name))))
                ]

            // The primary `.ctor` body, one arm per base species:
            //  * `ExternalBase` with `inherit Base(args)` args (`inherit exn(msg)`): the
            //    base is external, so its `.ctor` overload is re-picked from the
            //    call-site arg types through the same external-ctor resolution a
            //    `new System.Exception(...)` uses (`TryEmitCtor`), then chained with
            //    the `inherit` args before the field stores.
            //  * `ExternalBase`, no args (`inherit Attribute`, `Attribute = (# class … #)`):
            //    chain to its parameterless `.ctor()` (minted directly off the external
            //    `TypeRef` — a protected base ctor need not be in the member harvest)
            //    instead of `System.Object::.ctor`.
            //  * `inherit Base(args)` on a project-local base (`baseCtorCall`): chain to
            //    the parent's `.ctor` with the `inherit` args before the field stores —
            //    its `Def` token (mono parent) or a `MemberRef` on the parent's
            //    `TypeSpec` (generic parent). Bind pre-fills every local class.
            //  * struct: store params and return — value types don't chain a base ctor.
            //  * otherwise: chain to `System.Object::.ctor` (the record/closure recipe).
            let ctorBody =
                match baseShape, baseCtorCall with
                | BaseShape.ExternalBase(baseKey, _), ValueSome bcc when not bcc.Args.IsEmpty ->
                    let argTypes = [ for a in bcc.Args -> TastLower.typeOfExpr a ]

                    match icodegen.TryEmitCtor(baseKey, [], argTypes) with
                    | ValueSome recipe ->
                        Emit.buildClassBaseCtor
                            emitCtx
                            recipe.Handle
                            (EqArray.toList bcc.Args)
                            (EqArray.toList bcc.CtorParams)
                            ctorFieldRefs
                    | ValueNone ->
                        failwithf
                            "Emit: class '%s' inherits external base %A but no '.ctor' overload matches its %d base-ctor argument(s)"
                            td.Name
                            baseKey
                            bcc.Args.Length
                | BaseShape.ExternalBase(baseKey, _), _ ->
                    match icodegen.ExternalParameterlessBaseCtor baseKey with
                    | ValueSome extCtor -> Emit.buildClassBaseCtor emitCtx extCtor [] [] ctorFieldRefs
                    | ValueNone ->
                        failwithf
                            "Emit: class '%s' inherits external base %A but its parameterless '.ctor()' could not be minted"
                            td.Name
                            baseKey
                | _, ValueSome bcc ->
                    let baseKey, baseArgs =
                        match baseShape with
                        | BaseShape.LocalMono k -> k, []
                        | BaseShape.Generic(FTClass(n, xs)) -> n, EqArray.toList xs
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
                | _, ValueNone when isStruct -> Emit.buildStructCtor ctorFieldRefs
                | _, ValueNone -> Emit.buildRecordCtor provider.ObjectCtorRef ctorFieldRefs

            if emitPrimaryCtor then
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

            // Secondary constructors. Each is a `.ctor` overload whose
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

        // Interface implementations: each `(ifaceTy, members)` entry's
        // member bodies are already-typed `Frozen.TTypeMember`s, flattened here. They
        // emit as virtual methods (`ifaceEqualsAttrs` — a new slot, `Final` since
        // classes, unions, and records are all sealed) that the runtime binds to the
        // `InterfaceImpl` row by name + signature. The type's own members lead,
        // interface-impl members trail — the shared `NominalMembers.indexed` contract
        // the layout's `MethodKey.Member` rows use. Classes, unions, and records all
        // carry user impls; unions/records additionally synthesise eq/comp/format
        // interfaces (below), which use disjoint `MethodKey`s, so the two never collide
        // on a method row.
        let userInterfaces = userInterfacesOf input

        // `isIfaceImpl` selects the `void`-return conformance below; the row's
        // attrs were fixed by the layout's enumeration.
        let prepareMember (index: int) (isIfaceImpl: bool) (mem: Frozen.TTypeMember) =
            // A *generic* member. Both its declaring-type typars and its own
            // method typars now ride self-describing `TyTypar` nodes in the signature /
            // locals / body (Freeze.remapMemberTypes remaps both axes), so no ambient
            // typar window is installed; the encoder resolves them by
            // index. `methodTypars` still feeds the `GENERIC` header arity and the
            // `GenericParam` rows.
            // Canonical ABI order as a `(name, root)[]`; position IS the typar index.
            let methodTypars = GeneralizedTypars.toArray mem.MethodTypeParams
            let isGenericMethod = methodTypars.Length > 0

            // A `unit`-returning INSTANCE method (incl. an interface-impl member
            // conforming to a `void` BCL slot, `IDisposable.Dispose` etc.) encodes as
            // genuine `void`, and its body `ret`s empty-stacked (pop the residual
            // `unit`-as-value). This matches the universal *consumer* convention: both
            // the external-call path (`EmitCall`, `unit → void` member-refs) and the
            // local instance-call path (`EmitMember.buildMethodCall`) treat a
            // `unit`-returning instance call as void + a reified `unit`. Emitting the
            // `unit`-as-`ValueTuple` return instead breaks cross-assembly binding — a
            // consumer's void member-ref misses the `ValueTuple`-returning method
            // (`MissingMethodException`), which is exactly what bit the Vesper-compiled
            // `Vesper.Formatter` (its `AppendFormatted`/`AppendStructured` are
            // generic `unit`-returning instance methods the printf recipe calls void).
            // A STATIC `unit` member now also encodes `void`
            // ("void everywhere": the static asymmetry is
            // removed). The flip is safe because the re-read invariant
            // (`MetadataSymbols.frozenParams` maps a parameterless `void` back to
            // `unit -> unit`) round-trips it, and every call site already treats a
            // `unit`-returning call as void + a reified `unit`.
            let returnsVoid =
                match mem.ReturnTy with
                | FTConst(key, _) when SymbolKeyOps.simpleName key = "unit" -> true
                | _ -> false

            let bodyOffset =
                try
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
                                // The body is already `expandBuiltinOps`-expanded in
                                // `Layout.build` (once, so closure discovery and this
                                // walk share node identity); no re-expansion here.
                                mem.Body
                        ))
                with ex ->
                    raise (
                        System.Exception(
                            sprintf "While lowering body of member '%A.%s'\n%s" td.Key mem.Name ex.Message,
                            ex
                        )
                    )

            let paramTys = [ for (_, t) in mem.Params -> t ]

            // The declaring type's typars (if any) ride `FTTypar(Declaring, i)` nodes
            // the encoder resolves to `!i` directly, so a generic and a monomorphic
            // type share one signature builder. A generic *method* additionally
            // needs the `GENERIC` calling-convention header count; its own typars ride
            // `FTTypar(Method, i)` nodes the encoder resolves to `!!i` (no window).
            let signature =
                try
                    if returnsVoid && isGenericMethod then
                        // A generic `unit`-returning instance method (e.g.
                        // `Formatter.AppendFormatted<'T>`): `void` return + the `GENERIC`
                        // header, so a consumer's generic void member-ref binds.
                        provider.GenericMethodOnTypeSignatureVoid(methodTypars.Length, paramTys, not mem.IsStatic)
                    elif returnsVoid && mem.IsStatic then
                        // A `unit`-returning static member — `void` return.
                        provider.StaticMethodSignatureVoid paramTys
                    elif returnsVoid then
                        // A `unit`-returning instance method — `void` return, not the
                        // `unit`-as-`ValueTuple` the general path emits.
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
                    // anonymous. Wrap so
                    // the original encoder exception rides as `InnerException` — its
                    // stack pinpoints the actual encode failure.
                    raise (System.Exception(sprintf "While encoding signature of member '%A.%s'" td.Key mem.Name, ex))

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

        // Own members then interface-impl members, indexed by the shared
        // `NominalMembers.indexed` contract — the same `MethodKey.Member` index space
        // `Layout` declared the rows under, so bodies bind to the right rows.
        for (index, isIfaceImpl, mem) in NominalMembers.indexed members userInterfaces do
            prepareMember index isIfaceImpl mem

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
            | NominalEmissionInput.Union(cases, _) ->
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
                        IntType = FTConst(BuiltinTypes.intrinsicKey "int", EqArray.empty)
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

        // The synthesised `IStructuralFormattable.Format(IFormatSink)` (`%A`) —
        // emitted for *every* record / union (orthogonal to the equality /
        // comparison verdicts). The body is straight-line `callvirt`s on the `sink`
        // arg, threading the type's fields through `EmitStructuralFormat.buildRecordFormat` /
        // `buildUnionFormat` (the same field-handle resolution `structuralFields`
        // uses, so a generic type routes through its self-`TypeSpec` `MemberRef`s).
        let emitsStructuralFormat =
            // Suppressed in the assembly that *defines* the interfaces (Vesper.Core):
            // its own records would otherwise reference `IStructuralFormattable`
            // through an external `AssemblyRef` to Core itself (see
            // `Assembler.DefinesStructuralFormatInterfaces`).
            not asm.DefinesStructuralFormatInterfaces
            && match input with
               | NominalEmissionInput.Union _
               | NominalEmissionInput.Record _ -> true
               | NominalEmissionInput.Class _ -> false

        if emitsStructuralFormat then
            let formatIr =
                match input with
                | NominalEmissionInput.Union(cases, _) ->
                    let emitted = unions.[td.Key]

                    let formatCases =
                        [
                            for c in cases ->
                                let caseFields = emitted.Cases.[c.Name].Fields

                                {
                                    EmitStructuralFormat.UnionFormatCase.Name = c.Name
                                    EmitStructuralFormat.UnionFormatCase.Fields =
                                        [
                                            for fi in 0 .. c.Fields.Length - 1 ->
                                                selfMemberRef
                                                    (UserMemberKind.UnionMember(UnionMember.Field(c.Name, fi)))
                                                    caseFields.[fi],
                                                snd c.Fields.[fi]
                                        ]
                                }
                        ]

                    let support: EmitStructuralFormat.UnionFormatSupport =
                        {
                            Sink = provider.FormatSinkHandles
                            MkString = ctx.UserString
                            BoxToken = icodegen.TypeToken
                            TagField = tagFieldRef ()
                            Cases = formatCases
                        }

                    EmitStructuralFormat.buildUnionFormat support
                | NominalEmissionInput.Record _ ->
                    let support: EmitStructuralFormat.RecordFormatSupport =
                        {
                            Sink = provider.FormatSinkHandles
                            MkString = ctx.UserString
                            BoxToken = icodegen.TypeToken
                            Fields =
                                [
                                    for (name, h, fty) in records.[td.Key].Fields ->
                                        name,
                                        selfMemberRef (UserMemberKind.RecordMember(RecordMember.Field name)) h,
                                        fty
                                ]
                        }

                    EmitStructuralFormat.buildRecordFormat support
                | NominalEmissionInput.Class _ -> failwith "unreachable: class has no structural Format"

            asm.AddPrepared(
                MethodKey.FmtFormat td.Key,
                {
                    Signature = provider.StructuralFormatSignature()
                    BodyOffset = bodyOf formatIr
                    ParamNames = [ "sink" ]
                    MethodTypars = []
                }
            )

        // One `InterfaceImpl` entity handle per implemented interface — the
        // synthesised structural-equality / comparison interfaces (unions /
        // records) and the user-declared `interface … with` impls,
        // classes). A generic interface arg (`IEnumerable<'T>`) carries its `'T`
        // as a `FTTypar(Declaring, i)` (emitted by Freeze; `selfTyMarkers` for
        // the synthesised interfaces), encoded `!i` straight off the node — no
        // ambient window. `TypeSpecOf` mints the user interfaces' handles.
        let interfaces =
            if
                emitsEqualityTriple
                || emitsComparisonPair
                || emitsStructuralFormat
                || not (List.isEmpty userInterfaces)
            then
                [
                    if emitsEqualityTriple then
                        provider.EquatableInterfaceSpec selfTyMarkers
                    if emitsComparisonPair then
                        provider.ComparableInterfaceSpec selfTyMarkers
                        provider.IComparableType
                    if emitsStructuralFormat then
                        provider.StructuralFormattableInterface
                    for (ifaceTy, _) in userInterfaces do
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
