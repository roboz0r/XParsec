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
            for (ifaceTy, ms) in interfaces -> FrozenNominal.OfFrozen "an `interface` clause" ifaceTy, EqArray.toList ms
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
                | TTypeKindG.Union(cases, members, interfaces) ->
                    unions.Add
                        {
                            Decl = td
                            Cases = EqArray.toList cases
                            Members = EqArray.toList members
                            Interfaces = ifaceBlocks interfaces
                        }
                | TTypeKindG.Record(fields, members, interfaces, valueKind) ->
                    records.Add
                        {
                            Decl = td
                            Fields = EqArray.toList fields
                            Members = EqArray.toList members
                            Interfaces = ifaceBlocks interfaces
                            ValueKind = valueKind
                        }
                // A numeric enum emits a real `System.Enum` subclass.
                | TTypeKindG.Enum cases ->
                    match TEnumCases.classify cases with
                    | ValueSome TEnumVariant.Numeric ->
                        let underlying =
                            match TEnumCases.underlyingTypeKey cases with
                            | ValueSome w -> w
                            | ValueNone -> RuntimeNames.intKey

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
                            BaseType = c.BaseType |> ValueOption.map (FrozenNominal.OfFrozen "an `inherit` clause")
                            Interfaces = ifaceBlocks c.Interfaces
                            IsSealed = c.Declared.IsSealed
                            StaticPreamble = EqArray.toList c.StaticPreamble
                            InstancePreamble = EqArray.toList c.InstancePreamble
                            ThisKey = c.ThisKey
                            SecondaryCtors = EqArray.toList c.SecondaryCtors
                            BaseCtorCall = c.BaseCtorCall
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

    /// `GetHashCode`, `Equals(obj)`, `Equals(Self)`.
    let private equalityRows (td: TastAccessor.TypeDecl) : MethodRow list =
        match td.EqualitySupport with
        | EqualityVerdict.Structural ->
            [
                {
                    Key = MethodKey.EqGetHashCode td.Key
                    Name = "GetHashCode"
                    Attrs = overrideMethodAttrs
                }
                {
                    Key = MethodKey.EqEqualsObj td.Key
                    Name = "Equals"
                    Attrs = overrideMethodAttrs
                }
                {
                    Key = MethodKey.EqEqualsTyped td.Key
                    Name = "Equals"
                    Attrs = ifaceEqualsAttrs
                }
            ]
        | _ -> []

    /// `CompareTo(Self)` and `CompareTo(obj)`.
    let private comparisonRows (td: TastAccessor.TypeDecl) : MethodRow list =
        match td.ComparisonSupport with
        | ComparisonVerdict.Structural ->
            [
                {
                    Key = MethodKey.CmpCompareToTyped td.Key
                    Name = "CompareTo"
                    Attrs = ifaceEqualsAttrs
                }
                {
                    Key = MethodKey.CmpCompareToObj td.Key
                    Name = "CompareTo"
                    Attrs = ifaceEqualsAttrs
                }
            ]
        | _ -> []

    /// The synthesised `IStructuralFormattable.Format` row (`%A`), for every record / union
    /// that does not declare the interface itself: `%A` renders a value's structure and so
    /// never depends on whether the type supports `=` / `<`.
    let private formatRows
        (td: TastAccessor.TypeDecl)
        (interfaces: (FrozenNominal * TastAccessor.TypeMember list) list)
        : MethodRow list =
        if NominalMembers.declaresStructuralFormat interfaces then
            []
        else
            [
                {
                    Key = MethodKey.FmtFormat td.Key
                    Name = "Format"
                    Attrs = ifaceEqualsAttrs
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
            | ValueNone -> defaultArg td.Namespace "", ValueNone

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

    /// Per union: `_tag` + every case's payload fields; nullary `.ctor`,
    /// case factories, members, [equality triple], [comparison pair].
    let buildUnionNodes (symbols: ICodegenSymbols) (unions: UnionDecl list) : TypeNode list =
        [
            for ud in unions ->
                let td = ud.Decl

                let fields =
                    [
                        yield
                            {
                                Key = FieldKey.UnionTag td.Key
                                Name = "_tag"
                                Attrs = FieldAttributes.Public
                                Ty = FTConst(RuntimeNames.intKey, EqArray.empty)
                                ClosureScope = ValueNone
                            }
                        for c in ud.Cases do
                            for fi in 0 .. c.Fields.Length - 1 ->
                                {
                                    Key = FieldKey.UnionCaseField(td.Key, c.Name, fi)
                                    Name = sprintf "%s_%d" c.Name fi
                                    Attrs = FieldAttributes.Public
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

                        for c in ud.Cases do
                            yield
                                {
                                    Key = MethodKey.UnionFactory(td.Key, c.Name)
                                    Name = c.Name
                                    Attrs = staticFactoryAttrs
                                }

                        yield! ownAndIfaceMemberRows td.Key ud.Members ud.Interfaces

                        yield! equalityRows td
                        yield! comparisonRows td
                        yield! formatRows td ud.Interfaces
                        yield! coSlotRows symbols td ud.Interfaces
                    ]

                nominalNode TypeSlotKind.Union td fields methodRows
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

                        yield! equalityRows td
                        yield! comparisonRows td
                        yield! formatRows td rd.Interfaces
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
                    cd.ValueKind <> ClassValueKind.RefType
                    || cd.HasPrimaryCtor
                    || List.isEmpty cd.SecondaryCtors

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

                nominalNode (TypeSlotKind.StructEnum sed.IsMixed) td fields methodRows
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
