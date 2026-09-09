namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open Vesper
open XParsec.FSharp.SemanticAnalysis

module internal LayoutNodes =

    /// The TAST's `interface … with` blocks, narrowed once here. Inference stamps a block only
    /// once it resolves to an interface, so a non-nominal is a bug, not a shape to drop.
    let private ifaceBlocks
        (interfaces: Block<FrozenType * Block<TastAccessor.TypeMember>>)
        : (FrozenNominal * TastAccessor.TypeMember list) list =
        [
            for (ifaceTy, ms) in interfaces -> FrozenNominal.ofFrozen "an `interface` clause" ifaceTy, Block.toList ms
        ]

    let partitionTypeDecls (symbols: ICodegenSymbols) (decls: TastAccessor.DeclId list) : PartitionedTypeDecls =
        let interfaces = ResizeArray()
        let unions = ResizeArray()
        let records = ResizeArray()
        let classes = ResizeArray()
        let enums = ResizeArray()
        let structEnums = ResizeArray()

        for d in decls do
            match TastAccessor.declKind d with
            | DeclShape.Type ->
                let td = TastAccessor.declType d

                match td.Kind with
                | TTypeKindG.Interface methods -> interfaces.Add(td, Block.toList methods)
                | TTypeKindG.Union u ->
                    let cases = Block.toList u.Cases

                    let regime =
                        UnionRegime.classify
                            u.ValueKind
                            u.Cases.Length
                            (u.Cases |> Block.exists (fun c -> not c.Fields.IsEmpty))

                    unions.Add
                        {
                            Decl = td
                            Cases = cases
                            Members = Block.toList u.Members
                            Interfaces = ifaceBlocks u.Interfaces
                            ValueKind = u.ValueKind
                            Regime = regime
                            Placements = FlatUnionPlacements.ofCases symbols td.TypeKey regime cases
                        }
                | TTypeKindG.Record r ->
                    records.Add
                        {
                            Decl = td
                            Fields = Block.toList r.Fields
                            Members = Block.toList r.Members
                            Interfaces = ifaceBlocks r.Interfaces
                            ValueKind = r.ValueKind
                        }
                // A numeric enum emits a real `System.Enum` subclass.
                | TTypeKindG.Enum cases ->
                    match TEnumCases.classify cases with
                    | ValueSome TEnumVariant.Numeric ->
                        let underlying = TEnumCases.numericUnderlyingTypeKey cases

                        let numericCases =
                            [
                                for c in cases do
                                    match c.Value with
                                    | ValueSome(TEnumLiteral.Int v) -> c.Name, v
                                    | _ -> ()
                            ]

                        enums.Add
                            {
                                Decl = td
                                Underlying = underlying
                                Cases = numericCases
                            }
                    // A string / mixed enum emits a `[<Struct>]` wrapper; `IsMixed`
                    // drives the `obj`-vs-`string` backing field in the emitter.
                    | ValueSome(TEnumVariant.String | TEnumVariant.Mixed as variant) ->
                        let structCases =
                            [
                                for c in cases do
                                    match c.Value with
                                    | ValueSome lit -> c.Name, lit
                                    | ValueNone -> ()
                            ]

                        structEnums.Add
                            {
                                Decl = td
                                IsMixed = (variant = TEnumVariant.Mixed)
                                Cases = structCases
                            }
                    // No case resolved to a legal literal, so there is nothing to emit.
                    | ValueNone -> ()
                | TTypeKindG.Class c ->
                    classes.Add
                        {
                            Decl = td
                            Fields = Block.toList c.Fields
                            CtorParams = Block.toList c.CtorParams
                            Members = Block.toList c.Members
                            Base = c.Base
                            Interfaces = ifaceBlocks c.Interfaces
                            IsSealed = c.Declared.IsSealed
                            StaticPreamble = Block.toList c.StaticPreamble
                            InstancePreamble = Block.toList c.InstancePreamble
                            ThisKey = c.ThisKey
                            SecondaryCtors = Block.toList c.SecondaryCtors
                            ValueKind = c.ValueKind
                            HasPrimaryCtor = c.HasPrimaryCtor
                        }
                // A transparent alias: every use site already expanded to the body, so no
                // type is emitted for the name.
                | TTypeKindG.Abbrev _
                | TTypeKindG.Measure _ -> ()
            | _ -> ()

        {
            Interfaces = List.ofSeq interfaces
            Unions = List.ofSeq unions
            Records = List.ofSeq records
            Classes = List.ofSeq classes
            Enums = List.ofSeq enums
            StructEnums = List.ofSeq structEnums
        }

    /// An augmentation member's method row: an interface-impl member forces the
    /// virtual/new-slot/final attrs so the runtime binds it to the `InterfaceImpl`
    /// row; a type's own member keeps its natural attrs.
    let private memberRow (key: SymbolKey) (index: int) (isIfaceImpl: bool) (mem: TastAccessor.TypeMember) : MethodRow =
        {
            Key = MethodKey.Member(key, index)
            Name = memberMetaName mem.Name mem.Kind
            Attrs =
                accessorAttrs
                    mem.Kind
                    (if isIfaceImpl then ifaceEqualsAttrs
                     elif mem.IsStatic then staticMethodAttrs
                     // An `override` reuses the base slot: `Public Virtual HideBySig`, no
                     // `NewSlot`. A plain `member` stays non-virtual.
                     elif mem.IsOverride then overrideMethodAttrs
                     else instanceMethodAttrs)
        }

    /// A nominal type's own augmentation members followed by its user `interface …
    /// with` impl members, as `MethodKey.Member` rows.
    let ownAndIfaceMemberRows
        (key: SymbolKey)
        (members: TastAccessor.TypeMember list)
        (interfaces: (FrozenNominal * TastAccessor.TypeMember list) list)
        : MethodRow list =
        NominalMembers.indexed members interfaces
        |> List.map (fun (i, isIfaceImpl, m) -> memberRow key i isIfaceImpl m)

    /// The `Property` rows a nominal type's members declare, over the same indexed member
    /// list `ownAndIfaceMemberRows` walks, so an accessor's row and its property agree on
    /// which `MethodDef` the `MethodSemantics` row binds.
    let ownAndIfaceProperties
        (key: SymbolKey)
        (members: TastAccessor.TypeMember list)
        (interfaces: (FrozenNominal * TastAccessor.TypeMember list) list)
        : PropertySlot list =
        NominalMembers.indexed members interfaces
        |> List.map (fun (i, _, m) ->
            {
                Method = MethodKey.Member(key, i)
                Name = m.Name
                Kind = m.Kind
                IsStatic = m.IsStatic
                ParamTys = m.Params |> Block.map snd
                RetTy = m.ReturnTy
            }
        )
        |> PropertySlot.ofAccessors key

    /// A nominal's synthesised structural rows. `attrs` decides whether the typed
    /// `Equals(Self)` / `CompareTo(Self)` / `Format` entries carry a body; `Equals(object)`
    /// and `CompareTo(object)` carry one in every regime.
    let structuralRows (attrs: StructuralRowAttrs) (s: StructuralMembers) (td: TastAccessor.TypeDecl) : MethodRow list =
        [
            if s.Equality then
                {
                    Key = MethodKey.EqGetHashCode td.Key
                    Name = "GetHashCode"
                    Attrs = attrs.ObjectSlot
                }

                {
                    Key = MethodKey.EqEqualsObj td.Key
                    Name = "Equals"
                    Attrs = overrideMethodAttrs
                }

                {
                    Key = MethodKey.EqEqualsTyped td.Key
                    Name = "Equals"
                    Attrs = attrs.InterfaceSlot
                }

            if s.Comparison then
                {
                    Key = MethodKey.CmpCompareToTyped td.Key
                    Name = "CompareTo"
                    Attrs = attrs.InterfaceSlot
                }

                {
                    Key = MethodKey.CmpCompareToObj td.Key
                    Name = "CompareTo"
                    Attrs = ifaceEqualsAttrs
                }

            if s.Format then
                {
                    Key = MethodKey.FmtFormat td.Key
                    Name = "Format"
                    Attrs = attrs.InterfaceSlot
                }
        ]

    /// The capability co-slot rows: each a new virtual slot the runtime binds to the
    /// inherited BCL interface method by name + signature, like the typed `Equals(Self)`.
    let coSlotRows
        (symbols: ICodegenSymbols)
        (td: TastAccessor.TypeDecl)
        (interfaces: (FrozenNominal * TastAccessor.TypeMember list) list)
        : MethodRow list =
        [
            for (_, slot) in CapabilityCoSlots.required symbols [ for (iface, _) in interfaces -> iface ] ->
                {
                    Key = MethodKey.CapCoSlot(td.Key, slot)
                    Name = CapabilityCoSlots.metaName slot
                    Attrs = ifaceEqualsAttrs
                }
        ]

    /// The module a declaration's key says holds it, or `ValueNone` for one declared
    /// straight in a namespace.
    let private declaringModule (td: TastAccessor.TypeDecl) : ModuleKey voption =
        match td.Key with
        | SymbolKey.Type t ->
            match t.Container with
            | TypeContainer.InModule m -> ValueSome m
            | TypeContainer.InNamespace _ -> ValueNone
            // `InType` is the EXTERNAL nesting of a bare-IL type; source cannot declare
            // a nested type, and a local one would need an enclosing SLOT this backend
            // has no way to reference.
            | TypeContainer.InType outer ->
                failwithf "Layout: local type '%s' claims a CLR-nested container '%s'" td.Name outer.Name
        | k -> failwithf "Layout: type declaration '%s' carries a non-type key %A" td.Name k

    /// Where a declaration's `TypeDef` row sits: `(namespace column, enclosing slot)`. A
    /// type held by a module has an empty namespace column and a `NestedClass` row under
    /// the module class; one declared straight in a namespace is a root of that namespace.
    let containerPlacement (td: TastAccessor.TypeDecl) : string * TypeSlotKey voption =
        match declaringModule td with
        | ValueSome m -> "", ValueSome(TypeSlotKey.ModuleClass m)
        | ValueNone -> SymbolKeyOps.typeNs td.TypeKey, ValueNone

    let nominalNode
        (kind: TypeSlotKind)
        (td: TastAccessor.TypeDecl)
        (fields: FieldSlot list)
        (methods: MethodRow list)
        (properties: PropertySlot list)
        : TypeNode =
        let ns, enclosing = containerPlacement td

        {
            Slot =
                {
                    Key = TypeSlotKey.Nominal td.Key
                    Kind = kind
                    Namespace = ns
                    MetaName = SymbolKeyOps.arityName td.Name (int td.TypeParams.TypeArity)
                    Typars = GenericParamRow.ofTypars td.TypeParams
                }
            Enclosing = enclosing
            Fields = fields
            Methods = methods
            Properties = properties
            Nested = []
        }

    // ---- Per-kind node builders ------------------------------------------------------

    let buildInterfaceNodes (interfaces: (TastAccessor.TypeDecl * Frozen.TAbstractMethod list) list) : TypeNode list =
        [
            for (td, methods) in interfaces ->
                let rows =
                    methods
                    |> List.mapi (fun i m ->
                        let key = MethodKey.InterfaceMethod(td.Key, i)

                        let methodRow: MethodRow =
                            {
                                Key = key
                                // An abstract property emits as its `get_<Name>` getter
                                // slot; a method keeps its bare name.
                                Name = memberMetaName m.Name m.Kind
                                Attrs = accessorAttrs m.Kind abstractMethodAttrs
                            }

                        let accessor: AccessorRow =
                            {
                                Method = key
                                Name = m.Name
                                Kind = m.Kind
                                IsStatic = m.IsStatic
                                ParamTys = abstractMethodParams m |> Block.map snd
                                RetTy = snd (uncurry m.Signature)
                            }

                        methodRow, accessor
                    )

                let properties = rows |> List.map snd |> PropertySlot.ofAccessors td.Key
                nominalNode TypeSlotKind.Interface td [] (List.map fst rows) properties
        ]

    let buildRecordNodes (symbols: ICodegenSymbols) (records: RecordDecl list) : TypeNode list =
        [
            for rd in records ->
                let td = rd.Decl

                let fields =
                    [
                        for f in rd.Fields ->
                            {
                                Key = FieldKey.RecordField(td.Key, f.Name)
                                Name = RecordBackingField.metaName f.Name
                                Attrs = instanceFieldAttrs FieldReach.OwnType (writesOf f.IsMutable)
                                Ty = f.Type
                                ClosureScope = ValueNone
                            }
                    ]

                let methodRows =
                    [
                        yield
                            {
                                Key = MethodKey.NominalCtor td.Key
                                Name = ".ctor"
                                Attrs = ctorAttrs
                            }

                        for f in rd.Fields do
                            for role in RecordFieldAccessors.rolesOf f ->
                                {
                                    Key = MethodKey.RecordFieldAccessor(td.Key, f.Name, role)
                                    Name = TAccessorRole.methodName role f.Name
                                    Attrs = synthAccessorAttrs
                                }

                        yield! ownAndIfaceMemberRows td.Key rd.Members rd.Interfaces

                        yield! structuralRows concreteStructuralAttrs (StructuralMembers.ofRecord rd) td
                        yield! coSlotRows symbols td rd.Interfaces
                    ]

                let properties =
                    [
                        for f in rd.Fields ->
                            let accessorOf (role: TAccessorRole) : MethodKey voption =
                                if List.contains role (RecordFieldAccessors.rolesOf f) then
                                    ValueSome(MethodKey.RecordFieldAccessor(td.Key, f.Name, role))
                                else
                                    ValueNone

                            {
                                Key = PropertyKey.RecordField(td.Key, f.Name)
                                Name = f.Name
                                IsInstance = true
                                IndexTys = Block.empty
                                ValueTy = f.Type
                                Getter = accessorOf TAccessorRole.Getter
                                Setter = accessorOf TAccessorRole.Setter
                            }

                        yield! ownAndIfaceProperties td.Key rd.Members rd.Interfaces
                    ]

                nominalNode (TypeSlotKind.Record rd.ValueKind) td fields methodRows properties
        ]

    /// Per class: ctor-param backing fields (`initonly`), `val` fields (immutable ⇒ `initonly`),
    /// then instance-`let` and `static let` fields. Methods: primary `.ctor`,
    /// [`.cctor`], [secondary `.ctor`s], own members, interface-impl members.
    let buildClassNodes (symbols: ICodegenSymbols) (classes: ClassDecl list) : TypeNode list =
        [
            for cd in classes ->
                let td = cd.Decl

                let fields =
                    [
                        for p in cd.CtorParams ->
                            {
                                Key = FieldKey.ClassCtorParamField(td.Key, p.Name)
                                Name = p.Name
                                // A ctor parameter has no `mutable` form.
                                Attrs = instanceFieldAttrs FieldReach.Assembly FieldWrites.ByCtor
                                Ty = p.Type
                                ClosureScope = ValueNone
                            }
                        for f in cd.Fields ->
                            {
                                Key = FieldKey.ClassInstanceField(td.Key, f.Name)
                                Name = f.Name
                                Attrs = instanceFieldAttrs FieldReach.Public (writesOf f.IsMutable)
                                Ty = f.Type
                                ClosureScope = ValueNone
                            }
                        // A preamble `let` is written by the initialiser its binding sits in —
                        // the primary `.ctor` for an instance one, the `.cctor` for a
                        // `static let` — so `mutable` is what decides `initonly` for both.
                        for l in TPreambleEntryG.lets cd.InstancePreamble ->
                            {
                                Key = FieldKey.ClassLetField(td.Key, l.Name)
                                Name = l.Name
                                Attrs = instanceFieldAttrs FieldReach.Assembly (writesOf l.IsMutable)
                                Ty = l.Type
                                ClosureScope = ValueNone
                            }
                        for sl in TPreambleEntryG.lets cd.StaticPreamble ->
                            {
                                Key = FieldKey.ClassStaticField(td.Key, sl.Name)
                                Name = sl.Name
                                Attrs = staticFieldAttrs FieldReach.Assembly (writesOf sl.IsMutable)
                                Ty = sl.Type
                                ClosureScope = ValueNone
                            }
                    ]

                // A `val`-field reference type (`type T = val …; new(…) = …`) has no
                // primary ctor, because a synthesised parameterless one would collide with a
                // `new()`. A struct keeps its primary: F# forbids `new()` there.
                let emitPrimaryCtor =
                    cd.ValueKind.IsValueType || cd.HasPrimaryCtor || List.isEmpty cd.SecondaryCtors

                let methodRows =
                    [
                        if emitPrimaryCtor then
                            yield
                                {
                                    Key = MethodKey.NominalCtor td.Key
                                    Name = ".ctor"
                                    Attrs = ctorAttrs
                                }

                        // The `.cctor` runs the WHOLE static sequence, so a class whose
                        // static preamble is only `static do` still needs one.
                        if not (List.isEmpty cd.StaticPreamble) then
                            yield
                                {
                                    Key = MethodKey.NominalCctor td.Key
                                    Name = ".cctor"
                                    Attrs = cctorAttrs
                                }

                        yield!
                            cd.SecondaryCtors
                            |> List.mapi (fun i _ ->
                                {
                                    Key = MethodKey.SecondaryCtor(td.Key, i)
                                    Name = ".ctor"
                                    Attrs = ctorAttrs
                                }
                            )

                        yield! ownAndIfaceMemberRows td.Key cd.Members cd.Interfaces
                        yield! coSlotRows symbols td cd.Interfaces
                    ]

                nominalNode
                    (TypeSlotKind.Class(cd.IsSealed, cd.ValueKind))
                    td
                    fields
                    methodRows
                    (ownAndIfaceProperties td.Key cd.Members cd.Interfaces)
        ]

    /// Per numeric enum: the special-name `value__` field the CLR reads for
    /// `Enum.GetUnderlyingType`, then one `public static literal` field per case. No
    /// methods, because equality / hashing / compare all come from the `System.Enum` base.
    let buildEnumNodes (enums: EnumDecl list) : TypeNode list =
        [
            for ed in enums ->
                let td = ed.Decl

                let fields =
                    [
                        yield
                            {
                                Key = FieldKey.EnumValueField td.Key
                                Name = "value__"
                                Attrs = enumUnderlyingFieldAttrs
                                Ty = FTConst(ed.Underlying, Block.empty)
                                ClosureScope = ValueNone
                            }
                        for (caseName, _) in ed.Cases ->
                            {
                                Key = FieldKey.EnumCaseField(td.Key, caseName)
                                Name = caseName
                                Attrs = enumLiteralFieldAttrs
                                Ty = FTEnum td.TypeKey
                                ClosureScope = ValueNone
                            }
                    ]

                nominalNode TypeSlotKind.Enum td fields [] []
        ]

    /// Per string/mixed enum, a `[<Struct>]` wrapper: one backing field (`string`, or
    /// `obj` when mixed) holding the case value, one `public static initonly` field per
    /// case singleton, and `.ctor(value)` + the `.cctor` that constructs each case.
    let buildStructEnumNodes (structEnums: StructEnumDecl list) : TypeNode list =
        [
            for sed in structEnums ->
                let td = sed.Decl

                let fieldTy =
                    FTConst(
                        (if sed.IsMixed then
                             RuntimeNames.objKey
                         else
                             RuntimeNames.stringKey),
                        Block.empty
                    )

                let fields =
                    [
                        yield
                            {
                                Key = FieldKey.EnumBackingField td.Key
                                Name = "value"
                                Attrs = instanceFieldAttrs FieldReach.Public FieldWrites.ByCtor
                                Ty = fieldTy
                                ClosureScope = ValueNone
                            }
                        for (caseName, _) in sed.Cases ->
                            {
                                Key = FieldKey.EnumCaseField(td.Key, caseName)
                                Name = caseName
                                // The case singletons are `.cctor`-initialised: only a
                                // primitive field can be `literal`.
                                Attrs = staticFieldAttrs FieldReach.Public FieldWrites.ByCtor
                                Ty = FTEnum td.TypeKey
                                ClosureScope = ValueNone
                            }
                    ]

                let methodRows =
                    [
                        {
                            Key = MethodKey.NominalCtor td.Key
                            Name = ".ctor"
                            Attrs = ctorAttrs
                        }
                        {
                            Key = MethodKey.NominalCctor td.Key
                            Name = ".cctor"
                            Attrs = cctorAttrs
                        }
                    ]

                nominalNode TypeSlotKind.StructEnum td fields methodRows []
        ]

    /// Per closure: capture fields; `.ctor` + `Invoke`. Closures synthesise their typar
    /// names (`T0`, …), because only the count survives to codegen. A closure stays a ROOT even
    /// though it was lifted out of a module: its name is already globally unique.
    let buildClosureNodes (closures: EmitTypes.Closure list) : TypeNode list =
        [
            for c in closures ->
                let isGeneric = c.TypeArity > 0<_>
                let cached = Emit.closureIsCached c

                let captureFields =
                    c.Captures
                    |> Block.mapi (fun i cap ->
                        // A capture is read by `Invoke`. A captured `let mutable` arrives
                        // as a `Vesper.Ref`, so the field holds the cell. A back-patched
                        // capture is stored from another type and outside a `.ctor`.
                        let attrs =
                            match cap.Fill with
                            | EmitTypes.CaptureFill.ByCtor -> instanceFieldAttrs FieldReach.OwnType FieldWrites.ByCtor
                            | EmitTypes.CaptureFill.BackPatched ->
                                instanceFieldAttrs FieldReach.Assembly FieldWrites.Anywhere

                        {
                            Key = FieldKey.ClosureCapture(c.Name, i)
                            Name = sprintf "capture%d" i
                            Attrs = attrs
                            Ty = cap.Ty
                            ClosureScope = (if isGeneric then ValueSome c.Frame else ValueNone)
                        }
                    )
                    |> Block.toList

                // The singleton field: `static readonly` of the closure's own type. Its
                // `Ty` is unused, because the writer mints the self-type signature from the
                // closure's TypeDef handle (a closure type has no `FrozenType`).
                let cachedFields =
                    [
                        if cached then
                            {
                                Key = FieldKey.ClosureCached c.Name
                                Name = "instance"
                                Attrs = staticFieldAttrs FieldReach.Public FieldWrites.ByCtor
                                Ty = c.ResultTy
                                ClosureScope = ValueNone
                            }
                    ]

                let fields = captureFields @ cachedFields

                let methodRows =
                    [
                        {
                            Key = MethodKey.ClosureCtor c.Name
                            Name = ".ctor"
                            Attrs = ctorAttrs
                        }
                        {
                            Key = MethodKey.ClosureInvoke c.Name
                            Name = "Invoke"
                            Attrs = invokeAttrs
                        }
                        if cached then
                            {
                                Key = MethodKey.ClosureCctor c.Name
                                Name = ".cctor"
                                Attrs = cctorAttrs
                            }
                    ]

                {
                    Slot =
                        {
                            Key = TypeSlotKey.Closure c.Name
                            Kind = TypeSlotKind.Closure
                            Namespace = ""
                            MetaName = SymbolKeyOps.arityName c.Name (int c.TypeArity)
                            Typars = GenericParamRow.ofTypars (TyparList.positional c.TypeArity)
                        }
                    Enclosing = ValueNone
                    Fields = fields
                    Methods = methodRows
                    Properties = []
                    Nested = []
                }
        ]
