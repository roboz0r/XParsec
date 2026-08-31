namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

module internal LayoutNodes =

    /// The TAST's `interface … with` blocks, narrowed once here. Inference stamps a block only
    /// once it resolves to an interface, so a non-nominal is a bug, not a shape to drop.
    let private ifaceBlocks
        (interfaces: EqArray<FrozenType * EqArray<TastAccessor.TypeMember>>)
        : (FrozenNominal * TastAccessor.TypeMember list) list =
        [
            for (ifaceTy, ms) in interfaces -> FrozenNominal.ofFrozen "an `interface` clause" ifaceTy, EqArray.toList ms
        ]

    let partitionTypeDecls (decls: TastAccessor.DeclId list) : PartitionedTypeDecls =
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
                | TTypeKindG.Interface methods -> interfaces.Add(td, EqArray.toList methods)
                | TTypeKindG.Union u ->
                    unions.Add
                        {
                            Decl = td
                            Cases = EqArray.toList u.Cases
                            Members = EqArray.toList u.Members
                            Interfaces = ifaceBlocks u.Interfaces
                            ValueKind = u.ValueKind
                            Regime =
                                UnionRegime.classify
                                    u.ValueKind
                                    u.Cases.Length
                                    (u.Cases |> EqArray.exists (fun c -> not c.Fields.IsEmpty))
                        }
                | TTypeKindG.Record r ->
                    records.Add
                        {
                            Decl = td
                            Fields = EqArray.toList r.Fields
                            Members = EqArray.toList r.Members
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
                            Fields = EqArray.toList c.Fields
                            CtorParams = EqArray.toList c.CtorParams
                            Members = EqArray.toList c.Members
                            Base = c.Base
                            Interfaces = ifaceBlocks c.Interfaces
                            IsSealed = c.Declared.IsSealed
                            StaticPreamble = EqArray.toList c.StaticPreamble
                            InstancePreamble = EqArray.toList c.InstancePreamble
                            ThisKey = c.ThisKey
                            SecondaryCtors = EqArray.toList c.SecondaryCtors
                            ValueKind = c.ValueKind
                            HasPrimaryCtor = c.HasPrimaryCtor
                        }
                // A transparent alias: every use site already expanded to the body, so no
                // type is emitted for the name.
                | TTypeKindG.Abbrev _ -> ()
            | _ -> ()

        {
            Interfaces = List.ofSeq interfaces
            Unions = List.ofSeq unions
            Records = List.ofSeq records
            Classes = List.ofSeq classes
            Enums = List.ofSeq enums
            StructEnums = List.ofSeq structEnums
        }

    /// Metadata typar names: `'T` → `T`.
    let private typarNames (typeParams: EqArray<string>) : string list =
        [ for n in typeParams -> n.TrimStart('\'') ]

    /// An augmentation member's method row: an interface-impl member forces the
    /// virtual/new-slot/final attrs so the runtime binds it to the `InterfaceImpl`
    /// row; a type's own member keeps its natural attrs.
    let private memberRow (key: SymbolKey) (index: int) (isIfaceImpl: bool) (mem: TastAccessor.TypeMember) : MethodRow =
        {
            Key = MethodKey.Member(key, index)
            Name = memberMetaName mem
            Attrs =
                if isIfaceImpl then ifaceEqualsAttrs
                elif mem.IsStatic then staticMethodAttrs
                // An `override` reuses the base slot: `Public Virtual HideBySig`, no
                // `NewSlot`. A plain `member` stays non-virtual.
                elif mem.IsOverride then overrideMethodAttrs
                else instanceMethodAttrs
        }

    /// A nominal type's own augmentation members followed by its user `interface …
    /// with` impl members, as `MethodKey.Member` rows.
    let private ownAndIfaceMemberRows
        (key: SymbolKey)
        (members: TastAccessor.TypeMember list)
        (interfaces: (FrozenNominal * TastAccessor.TypeMember list) list)
        : MethodRow list =
        NominalMembers.indexed members interfaces
        |> List.map (fun (i, isIfaceImpl, m) -> memberRow key i isIfaceImpl m)

    /// A nominal's synthesised structural rows, in row order: the equality triple
    /// (`GetHashCode`, `Equals(object)`, the typed `Equals(Self)`), the comparison pair
    /// (the typed `CompareTo(Self)`, `CompareTo(object)`), then `Format`.
    ///
    /// `attrs` decides whether the typed entries carry a body. On a hierarchy union's base
    /// they are abstract and each case type implements them, so `EqualityComparer<Self>`
    /// and `Comparer<Self>` dispatch straight to the case. `Equals(object)` and
    /// `CompareTo(object)` stay concrete in every regime: they cast and hand over to the
    /// typed slot, so a case type declares neither.
    let private structuralRows
        (attrs: StructuralRowAttrs)
        (s: StructuralMembers)
        (td: TastAccessor.TypeDecl)
        : MethodRow list =
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
    let private coSlotRows
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

    /// A nominal type's node. Its `TypeDef` sits in its declaring module's class
    /// when it has one, with an empty namespace column and a `NestedClass` row, and at the
    /// root of its namespace otherwise.
    let private nominalNode
        (kind: TypeSlotKind)
        (td: TastAccessor.TypeDecl)
        (fields: FieldSlot list)
        (methods: MethodRow list)
        : TypeNode =
        let ns, enclosing =
            match declaringModule td with
            | ValueSome m -> "", ValueSome(TypeSlotKey.ModuleClass m)
            | ValueNone -> SymbolKeyOps.typeNs td.TypeKey, ValueNone

        {
            Slot =
                {
                    Key = TypeSlotKey.Nominal td.Key
                    Kind = kind
                    Namespace = ns
                    MetaName = SymbolKeyOps.arityName td.Name td.TypeParams.Length
                    Typars = typarNames td.TypeParams
                }
            Enclosing = enclosing
            Fields = fields
            Methods = methods
            Nested = []
        }

    // Backing storage for ctor params and `let` bindings is `assembly`, like FSC: a
    // lambda in a member body is lifted into a closure class nested in the enclosing
    // MODULE, so `private` would fault its read at JIT with `FieldAccessException`.
    let private compilerGeneratedStorage = FieldAttributes.Assembly

    // ---- Per-kind node builders ------------------------------------------------------

    let buildInterfaceNodes (interfaces: (TastAccessor.TypeDecl * Frozen.TAbstractMethod list) list) : TypeNode list =
        [
            for (td, methods) in interfaces ->
                let methodRows =
                    methods
                    |> List.mapi (fun i m ->
                        {
                            Key = MethodKey.InterfaceMethod(td.Key, i)
                            // An abstract property emits as its `get_<Name>` getter
                            // slot; a method keeps its bare name.
                            Name = if m.IsProperty then "get_" + m.Name else m.Name
                            Attrs = abstractMethodAttrs
                        }
                    )

                nominalNode TypeSlotKind.Interface td [] methodRows
        ]

    /// The row one structural slot takes on a hierarchy union's case type. The `Union`-typed
    /// entries and `GetHashCode` override the slots the base declares; the `Case`-typed pair
    /// declares no slot of its own and binds by `call`.
    let private unionCaseSlotRow (td: TastAccessor.TypeDecl) (caseName: string) (slot: UnionCaseSlot) : MethodRow =
        let attrs =
            match slot with
            | UnionCaseSlot.GetHashCode
            | UnionCaseSlot.EqualsUnion
            | UnionCaseSlot.CompareToUnion
            | UnionCaseSlot.Format -> overrideMethodAttrs
            | UnionCaseSlot.EqualsCase
            | UnionCaseSlot.CompareToCase -> instanceMethodAttrs

        {
            Key = MethodKey.UnionCaseStructural(td.Key, caseName, slot)
            Name = UnionCaseSlot.metaName slot
            Attrs = attrs
        }

    /// One case's nested `TypeDef` in a hierarchy union: its own payload fields, its
    /// `.ctor`, and the structural bodies the base declares abstract. A generic union's
    /// case redeclares the union's typars, so its `extends` instantiates the base over
    /// them; the metadata name carries no arity suffix, because a nested type's own arity
    /// counts only the typars it adds.
    let private unionCaseNode (ud: UnionDecl) (structural: StructuralMembers) (c: Frozen.TUnionCase) : TypeNode =
        let td = ud.Decl

        let fields =
            List.zip (ud.FieldNames c) (EqArray.toList c.Fields)
            |> List.mapi (fun fi (name, (_, fty)) ->
                {
                    Key = FieldKey.UnionCaseField(td.Key, c.Name, fi)
                    Name = name
                    // Written only by the case's own `.ctor`, which is what `initonly`
                    // permits now that no factory stores after construction.
                    Attrs = FieldAttributes.Public ||| FieldAttributes.InitOnly
                    Ty = fty
                    ClosureScope = ValueNone
                }
            )

        let methodRows =
            [
                {
                    Key = MethodKey.UnionCaseCtor(td.Key, c.Name)
                    Name = ".ctor"
                    Attrs = ctorAttrs
                }
                yield! UnionCaseSlot.required structural |> List.map (unionCaseSlotRow td c.Name)
            ]

        {
            Slot =
                {
                    Key = TypeSlotKey.UnionCase(td.Key, c.Name)
                    Kind = TypeSlotKind.UnionCase
                    Namespace = ""
                    MetaName = c.Name
                    Typars = typarNames td.TypeParams
                }
            Enclosing = ValueSome(TypeSlotKey.Nominal td.Key)
            Fields = fields
            Methods = methodRows
            Nested = []
        }

    /// Per union: `_tag`, a singleton field per nullary case, the case payloads a flat
    /// regime holds co-resident, `.ctor` (`UnionCtorShape.ofRegime`), case factories,
    /// members, [equality triple], [comparison pair]. A hierarchy union additionally nests
    /// a `TypeDef` per case.
    let buildUnionNodes (symbols: ICodegenSymbols) (unions: UnionDecl list) : TypeNode list =
        [
            for ud in unions ->
                let td = ud.Decl
                let isStruct = ud.ValueKind.IsValueType
                let isHierarchy = ud.IsHierarchy
                let structural = StructuralMembers.ofUnion ud
                let singletonCases = ud.SingletonCases

                let selfTy =
                    FTUnion(td.TypeKey, EqArray.ofList (declaringMarkers td.TypeParams.Length))

                let fields =
                    [
                        // A single-case union's sole case needs no discriminant, and its
                        // FSC-spelled payload may itself claim the name `_tag`
                        // (`C of tag: int`). A `TypeTested` base leaves the row out too.
                        // `get_Tag` fronts it for every reader outside the union and its
                        // case types.
                        if ud.HasTag then
                            yield
                                {
                                    Key = FieldKey.UnionTag td.Key
                                    Name = "_tag"
                                    Attrs = FieldAttributes.Private ||| FieldAttributes.InitOnly
                                    Ty = FTConst(RuntimeNames.intKey, EqArray.empty)
                                    ClosureScope = ValueNone
                                }

                        // The `<Case>` factory is the singleton's public accessor.
                        for (_, c) in singletonCases ->
                            {
                                Key = FieldKey.UnionCaseSingleton(td.Key, c.Name)
                                Name = "_unique_" + c.Name
                                Attrs = FieldAttributes.Private ||| FieldAttributes.Static ||| FieldAttributes.InitOnly
                                Ty = selfTy
                                ClosureScope = ValueNone
                            }

                        // A hierarchy case's payload lands on the case's own `TypeDef`.
                        // Written by the `.ctor` declaring it (`UnionCtorShape`), hence
                        // `initonly`.
                        if not isHierarchy then
                            for c in ud.Cases do
                                for (fi, name) in List.indexed (ud.FieldNames c) ->
                                    {
                                        Key = FieldKey.UnionCaseField(td.Key, c.Name, fi)
                                        Name = name
                                        Attrs = FieldAttributes.Public ||| FieldAttributes.InitOnly
                                        Ty = snd c.Fields.[fi]
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

                        // The `.cctor` constructs each nullary case's singleton once, so a
                        // nullary construction site stops allocating.
                        if not (List.isEmpty singletonCases) then
                            yield
                                {
                                    Key = MethodKey.NominalCctor td.Key
                                    Name = ".cctor"
                                    Attrs = cctorAttrs
                                }

                        // The public accessor for the private `_tag`, and the one channel a
                        // match arm outside the union reads the discriminant through.
                        if ud.HasTag then
                            yield
                                {
                                    Key = MethodKey.UnionGetTag td.Key
                                    Name = "get_Tag"
                                    Attrs = tagGetterAttrs
                                }

                        for c in ud.Cases do
                            yield
                                {
                                    Key = MethodKey.UnionFactory(td.Key, c.Name)
                                    Name = c.Name
                                    Attrs = staticFactoryAttrs
                                }

                        yield! ownAndIfaceMemberRows td.Key ud.Members ud.Interfaces

                        let attrs =
                            if isHierarchy then
                                abstractStructuralAttrs
                            else
                                concreteStructuralAttrs

                        yield! structuralRows attrs structural td
                        yield! coSlotRows symbols td ud.Interfaces
                    ]

                let node =
                    nominalNode (TypeSlotKind.Union(ud.ValueKind, ud.Regime)) td fields methodRows

                if isHierarchy then
                    { node with
                        Nested = [ for c in ud.Cases -> unionCaseNode ud structural c ]
                    }
                else
                    node
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
                                Name = f.Name
                                Attrs = FieldAttributes.Public
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

                        yield! ownAndIfaceMemberRows td.Key rd.Members rd.Interfaces

                        yield! structuralRows concreteStructuralAttrs (StructuralMembers.ofRecord rd) td
                        yield! coSlotRows symbols td rd.Interfaces
                    ]

                nominalNode (TypeSlotKind.Record rd.ValueKind) td fields methodRows
        ]

    /// Per class: ctor-param backing fields, `val` fields (immutable ⇒ `initonly`),
    /// then instance-`let` and `static let` storage. Methods: primary `.ctor`,
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
                                Attrs = compilerGeneratedStorage
                                Ty = p.Type
                                ClosureScope = ValueNone
                            }
                        for f in cd.Fields ->
                            {
                                Key = FieldKey.ClassInstanceField(td.Key, f.Name)
                                Name = f.Name
                                Attrs =
                                    if f.IsMutable then
                                        FieldAttributes.Public
                                    else
                                        FieldAttributes.Public ||| FieldAttributes.InitOnly
                                Ty = f.Type
                                ClosureScope = ValueNone
                            }
                        // An immutable instance `let` is written exactly once, by the
                        // primary `.ctor`, which is what `initonly` permits, so a
                        // `let mutable` is the only preamble bound variable that stays writable.
                        for l in TPreambleEntryG.lets cd.InstancePreamble ->
                            {
                                Key = FieldKey.ClassLetField(td.Key, l.Name)
                                Name = l.Name
                                Attrs =
                                    if l.IsMutable then
                                        compilerGeneratedStorage
                                    else
                                        compilerGeneratedStorage ||| FieldAttributes.InitOnly
                                Ty = l.Type
                                ClosureScope = ValueNone
                            }
                        for sl in TPreambleEntryG.lets cd.StaticPreamble ->
                            {
                                Key = FieldKey.ClassStaticField(td.Key, sl.Name)
                                Name = sl.Name
                                Attrs = compilerGeneratedStorage ||| FieldAttributes.Static
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

                nominalNode (TypeSlotKind.Class(cd.IsSealed, cd.ValueKind)) td fields methodRows
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
                                Attrs =
                                    FieldAttributes.Public
                                    ||| FieldAttributes.SpecialName
                                    ||| FieldAttributes.RTSpecialName
                                Ty = FTConst(ed.Underlying, EqArray.empty)
                                ClosureScope = ValueNone
                            }
                        for (caseName, _) in ed.Cases ->
                            {
                                Key = FieldKey.EnumCaseField(td.Key, caseName)
                                Name = caseName
                                // A `public static literal` field typed as the enum
                                // itself; `HasDefault` flags its `Constant` row.
                                Attrs =
                                    FieldAttributes.Public
                                    ||| FieldAttributes.Static
                                    ||| FieldAttributes.Literal
                                    ||| FieldAttributes.HasDefault
                                Ty = FTEnum td.TypeKey
                                ClosureScope = ValueNone
                            }
                    ]

                nominalNode TypeSlotKind.Enum td fields []
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
                        EqArray.empty
                    )

                let fields =
                    [
                        yield
                            {
                                Key = FieldKey.EnumBackingField td.Key
                                Name = "value"
                                // Written once by the `.ctor`, which is what `initonly`
                                // permits.
                                Attrs = FieldAttributes.Public ||| FieldAttributes.InitOnly
                                Ty = fieldTy
                                ClosureScope = ValueNone
                            }
                        for (caseName, _) in sed.Cases ->
                            {
                                Key = FieldKey.EnumCaseField(td.Key, caseName)
                                Name = caseName
                                // `public static initonly E` — the case singletons,
                                // `.cctor`-initialised (only a primitive field can be
                                // `literal`).
                                Attrs = FieldAttributes.Public ||| FieldAttributes.Static ||| FieldAttributes.InitOnly
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

                nominalNode TypeSlotKind.StructEnum td fields methodRows
        ]

    /// Per closure: capture fields; `.ctor` + `Invoke`. Closures synthesise their typar
    /// names (`T0`, …), because only the count survives to codegen. A closure stays a ROOT even
    /// though it was lifted out of a module: its name is already globally unique.
    let buildClosureNodes (closures: EmitTypes.Closure list) : TypeNode list =
        [
            for c in closures ->
                let isGeneric = c.Typars > 0
                let cached = Emit.closureIsCached c

                let captureFields =
                    [
                        for i in 0 .. List.length c.Captures - 1 ->
                            {
                                Key = FieldKey.ClosureCapture(c.Name, i)
                                Name = sprintf "capture%d" i
                                Attrs = FieldAttributes.Public
                                Ty = snd c.Captures.[i]
                                ClosureScope = (if isGeneric then ValueSome c.DeclaringTypars else ValueNone)
                            }
                    ]

                // The singleton field: `static readonly` of the closure's own type. Its
                // `Ty` is unused, because the writer mints the self-type signature from the
                // closure's TypeDef handle (a closure type has no `FrozenType`).
                let cachedFields =
                    [
                        if cached then
                            {
                                Key = FieldKey.ClosureCached c.Name
                                Name = "instance"
                                Attrs = FieldAttributes.Public ||| FieldAttributes.Static ||| FieldAttributes.InitOnly
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
                            MetaName = SymbolKeyOps.arityName c.Name c.Typars
                            Typars = [ for i in 0 .. c.Typars - 1 -> sprintf "T%d" i ]
                        }
                    Enclosing = ValueNone
                    Fields = fields
                    Methods = methodRows
                    Nested = []
                }
        ]
