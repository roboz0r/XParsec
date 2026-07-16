namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

/// The per-kind layout node builders: `partitionTypeDecls` + the private row helpers +
/// the `buildXNodes` that map each partition slice to its `TypeNode` rows. Factored out
/// of `Layout` so `Layout.fs` keeps only the unit orchestration (`buildUnit`), the
/// assembly `combine`, and `deriveHandles`. Consumes the layout data model in `LayoutModel`
/// and is consumed by `Layout.buildUnit`.
module internal LayoutNodes =

    /// Single-walk partition of `tast.Decls` by `TTypeKind`.
    let partitionTypeDecls (decls: EqArray<Frozen.TDecl>) : PartitionedTypeDecls =
        let interfaces = ResizeArray()
        let unions = ResizeArray()
        let records = ResizeArray()
        let classes = ResizeArray()
        let enums = ResizeArray()
        let structEnums = ResizeArray()

        for d in decls do
            match d with
            | TDeclG.Type td ->
                match td.Kind with
                | TTypeKindG.Interface methods -> interfaces.Add(td, EqArray.toList methods)
                | TTypeKindG.Union(cases, members, interfaces) ->
                    unions.Add
                        {
                            Decl = td
                            Cases = EqArray.toList cases
                            Members = EqArray.toList members
                            Interfaces = [ for (ifaceTy, ms) in interfaces -> ifaceTy, EqArray.toList ms ]
                        }
                | TTypeKindG.Record(fields, members, interfaces, valueKind) ->
                    records.Add
                        {
                            Decl = td
                            Fields = EqArray.toList fields
                            Members = EqArray.toList members
                            Interfaces = [ for (ifaceTy, ms) in interfaces -> ifaceTy, EqArray.toList ms ]
                            ValueKind = valueKind
                        }
                // NUMERIC enum emission: a real `System.Enum` subclass.
                // Only all-integer enums are partitioned here — string/mixed enums
                // and all-illegal enums are dropped (their use sites fail
                // loudly in `ClrEncoder`). The resolved case literals + the derived
                // underlying width are read once here off the single source of truth
                // (`TEnumCases`), so the emitter never re-derives them.
                | TTypeKindG.Enum cases ->
                    match TEnumCases.classify cases with
                    | ValueSome TEnumVariant.Numeric ->
                        let underlying =
                            match TEnumCases.underlyingTypeName cases with
                            | ValueSome w -> w
                            | ValueNone -> "int"

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
                    // STRING / MIXED enum emission: a `[<Struct>]` wrapper.
                    // The resolved case literals (string text, or the int/string of a
                    // mixed case) are read once here off `c.Value`; `IsMixed` drives
                    // the `obj`-vs-`string` field + boxed construction in the emitter.
                    // An all-illegal enum (`classify` = `ValueNone`) is still dropped.
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
                    | ValueNone -> ()
                | TTypeKindG.Class c ->
                    classes.Add
                        {
                            Decl = td
                            Fields = EqArray.toList c.Fields
                            CtorParams = EqArray.toList c.CtorParams
                            Members = EqArray.toList c.Members
                            BaseType = c.BaseType
                            Interfaces = [ for (ifaceTy, ms) in c.Interfaces -> ifaceTy, EqArray.toList ms ]
                            IsSealed = c.IsSealed
                            StaticPreamble = EqArray.toList c.StaticPreamble
                            InstancePreamble = EqArray.toList c.InstancePreamble
                            ThisKey = c.ThisKey
                            SecondaryCtors = EqArray.toList c.SecondaryCtors
                            BaseCtorCall = c.BaseCtorCall
                            ValueKind = c.ValueKind
                            HasPrimaryCtor = c.HasPrimaryCtor
                        }
            | _ -> ()

        {
            Interfaces = List.ofSeq interfaces
            Unions = List.ofSeq unions
            Records = List.ofSeq records
            Classes = List.ofSeq classes
            Enums = List.ofSeq enums
            StructEnums = List.ofSeq structEnums
        }

    /// Metadata-layer typar names: the leading F# quote dropped, once, here
    /// (so every consumer — `GenericParam` rows, the writer's row checks —
    /// compares like with like).
    let private typarNames (typeParams: EqArray<string>) : string list =
        [ for n in typeParams -> n.TrimStart('\'') ]

    /// An augmentation member's method row: interface-impl members force the
    /// virtual/new-slot/final attrs so the runtime binds them to the
    /// `InterfaceImpl`; a type's own members keep their natural attrs.
    let private memberRow (key: SymbolKey) (index: int) (isIfaceImpl: bool) (mem: Frozen.TTypeMember) : MethodRow =
        {
            Key = MethodKey.Member(key, index)
            Name = memberMetaName mem
            Attrs =
                if isIfaceImpl then ifaceEqualsAttrs
                elif mem.IsStatic then staticMethodAttrs
                // An `override` of a base virtual (Object's `Equals`/`GetHashCode`/
                // `ToString` for an `inherit`-less class) reuses the base slot —
                // `Public Virtual HideBySig`, no `NewSlot` — so the runtime binds it
                // over the inherited method. A plain `member` stays non-virtual.
                elif mem.IsOverride then overrideMethodAttrs
                else instanceMethodAttrs
        }

    /// A nominal type's own augmentation members followed by its user `interface …
    /// with` impl members, as `MethodKey.Member` rows. Indexing follows the shared
    /// `NominalMembers.indexed` contract (own at `[0..n)`, impl at `[n..)`) that
    /// `NominalEmit` binds bodies against. Impl members are forced to `ifaceEqualsAttrs`
    /// (virtual/new-slot/final) so the runtime binds each to its `InterfaceImpl` row.
    /// Shared by the union, record, and class arms.
    let private ownAndIfaceMemberRows
        (key: SymbolKey)
        (members: Frozen.TTypeMember list)
        (interfaces: (FrozenType * Frozen.TTypeMember list) list)
        : MethodRow list =
        NominalMembers.indexed members interfaces
        |> List.map (fun (i, isIfaceImpl, m) -> memberRow key i isIfaceImpl m)

    /// The equality triple's rows, in `NominalEmit` emission order.
    let private equalityRows (td: Frozen.TTypeDecl) : MethodRow list =
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

    /// The comparison pair's rows: the typed `CompareTo(Self)` first (its
    /// handle feeds `CompareTo(object)`'s body).
    let private comparisonRows (td: Frozen.TTypeDecl) : MethodRow list =
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

    /// The synthesised `IStructuralFormattable.Format` row (`%A`). Emitted for
    /// *every* record / union, unconditionally — `%A` is orthogonal to the equality /
    /// comparison verdicts (it renders a value's structure, never depending on whether the
    /// type supports `=` / `<`), and the interface resolves local-or-external like any
    /// nominal (`ClrEnv.coreInterfaceEntity`), so even `Vesper.Core`'s own records get a
    /// row. A new virtual slot bound to the `InterfaceImpl` by name + signature, like the
    /// typed `Equals(Self)`.
    let private formatRows (td: Frozen.TTypeDecl) : MethodRow list =
        [
            {
                Key = MethodKey.FmtFormat td.Key
                Name = "Format"
                Attrs = ifaceEqualsAttrs
            }
        ]

    /// The capability co-slot rows, in `NominalEmit` emission order. `CapabilityCoSlots.required`
    /// is a pure function of the type's implemented interfaces, and `NominalEmit` prepares
    /// the bodies by calling it on the SAME interfaces — so a reserved row can never go
    /// un-prepared, without a shared side table to keep in step. Each is a new virtual slot
    /// the runtime binds to the inherited BCL interface method by name + signature, like the
    /// typed `Equals(Self)`.
    let private coSlotRows
        (symbols: ICodegenSymbols)
        (td: Frozen.TTypeDecl)
        (interfaces: (FrozenType * Frozen.TTypeMember list) list)
        : MethodRow list =
        [
            for (_, slot) in CapabilityCoSlots.required symbols [ for (ifaceTy, _) in interfaces -> ifaceTy ] ->
                {
                    Key = MethodKey.CapCoSlot(td.Key, slot)
                    Name = CapabilityCoSlots.metaName slot
                    Attrs = ifaceEqualsAttrs
                }
        ]

    /// The module a declaration's key says holds it, or `ValueNone` for one declared
    /// straight in a namespace. The key is the ONE place the containment lives —
    /// `ModuleRules` builds it — so nothing here re-derives it from a name.
    let private declaringModule (td: Frozen.TTypeDecl) : ModuleKey voption =
        match td.Key with
        | SymbolKey.Type t ->
            match t.Holder with
            | TypeHolder.InModule m -> ValueSome m
            | TypeHolder.InNamespace _ -> ValueNone
            // `InType` is the EXTERNAL nesting of a bare-IL type. Vesper source cannot
            // declare a nested type, so a project-local decl never carries one; if one
            // ever arrives it needs an enclosing SLOT, which this backend has no way to
            // name — fail rather than emit it as a root under a truncated name.
            | TypeHolder.InType outer ->
                failwithf "Layout: local type '%s' claims a CLR-nested holder '%s'" td.Name outer.Name
        | k -> failwithf "Layout: type declaration '%s' carries a non-type key %A" td.Name k

    /// A nominal type's node. Its `TypeDef` sits in its declaring module's holder class
    /// when it has one — empty namespace column, a `NestedClass` row — and at the root
    /// of its namespace otherwise.
    let private nominalNode
        (kind: TypeSlotKind)
        (td: Frozen.TTypeDecl)
        (fields: FieldSlot list)
        (methods: MethodRow list)
        : TypeNode =
        let ns, enclosing =
            match declaringModule td with
            | ValueSome m -> "", ValueSome(TypeSlotKey.Holder m)
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

    // COMPILER-GENERATED backing storage (ctor-param, instance-`let` and `static let`
    // fields) is `assembly`, matching FSC: a lambda in a member body — or in a
    // preamble initialiser — is lifted into a closure class nested in the enclosing
    // MODULE, not in the class, so it reads the class's storage as a *different
    // type* — reachable only assembly-wide. `private` would make that read fault at
    // JIT time with `FieldAccessException`, and `public` would leak non-API storage.
    // A declared `val` field is the user's own surface and stays `public`.
    let private compilerGeneratedStorage = FieldAttributes.Assembly

    // ---- Per-kind node builders ------------------------------------------------------
    //
    // One `TypeNode list` per partition slice (plus the discovered closures). Each maps a
    // slice to its by-kind rows and is a pure function of that slice + the one ambient fact
    // a nominal row needs (`symbols`, for co-slots). `buildUnit` calls them in the by-kind
    // order the `TypeDef` table has always used; nothing here reads unit-wide or
    // later-derived state.

    let buildInterfaceNodes (interfaces: (Frozen.TTypeDecl * Frozen.TAbstractMethod list) list) : TypeNode list =
        [
            for (td, methods) in interfaces ->
                let methodRows =
                    methods
                    |> List.mapi (fun i m ->
                        {
                            Key = MethodKey.InterfaceMethod(td.Key, i)
                            // An abstract property emits as its `get_<Name>` getter
                            // slot (matching the impl's getter); a method keeps its
                            // bare name.
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
                        yield! formatRows td
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
                        yield! formatRows td
                        yield! coSlotRows symbols td rd.Interfaces
                    ]

                nominalNode (TypeSlotKind.Record rd.ValueKind) td fields methodRows
        ]

    /// Per class: ctor-param backing fields, then explicit `val [mutable]`
    /// instance fields (mutable ⇒ plain writable; immutable ⇒ `initonly`),
    /// then the instance-`let` and `static let` backing fields. Methods: primary
    /// `.ctor`, [`.cctor` when a static preamble exists], [secondary `.ctor`s], own
    /// members, interface-impl members.
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
                        // primary `.ctor` — which is what `initonly` permits — so a
                        // `let mutable` is the only preamble binder that stays writable.
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

                // The `val`-field *reference* form (`type T = val …; new(…) = …`)
                // has no primary ctor — its secondaries are the only `.ctor`s, so
                // a synthesised parameterless primary would collide with a
                // parameterless `new()`. Suppress it there. Structs always keep
                // their synthesised primary (a value type's other emission paths
                // reference its `NominalCtor`, and F# forbids a struct
                // parameterless ctor, so there is no collision). The no-secondary
                // fallback keeps the primary so a ctor-less type still has one.
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

    /// Per numeric enum: the special-name `value__` instance field
    /// (the underlying integral storage the CLR reads for `Enum.GetUnderlyingType`)
    /// then one `public static literal` field per case (its constant integer is
    /// attached as a `Constant` row in the writer's field pass). No methods —
    /// equality/hashing/compare all come from the `System.Enum` base.
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
                                Ty = FTConst(RuntimeNames.primitiveKey ed.Underlying, EqArray.empty)
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

    /// Per string/mixed enum: a `[<Struct>]` wrapper. One instance
    /// backing field (`string`, or `obj` when mixed) holding the case value, then
    /// one `public static initonly` field per case (the constructed singleton, set
    /// in the `.cctor`). Two methods: the `.ctor(field)` that stores the backing
    /// field, and the `.cctor` that constructs each case. Methods are bound in
    /// `Assembler.PrepareStructEnums`.
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
                                // Immutable: written once by the `.ctor` (`stfld`
                                // through the `newobj` temp address is legal on an
                                // `initonly` instance field from within `.ctor`).
                                Attrs = FieldAttributes.Public ||| FieldAttributes.InitOnly
                                Ty = fieldTy
                                ClosureScope = ValueNone
                            }
                        for (caseName, _) in sed.Cases ->
                            {
                                Key = FieldKey.EnumCaseField(td.Key, caseName)
                                Name = caseName
                                // `public static initonly E` — the closed set of
                                // case singletons, `.cctor`-initialised (a struct
                                // field cannot be `literal`; only a primitive can).
                                // DEFERRED: the design wanted PRIVATE fields exposed
                                // via public get-only properties (so construction is
                                // not public API — the closed-set guarantee for
                                // EXTERNAL consumers). The metadata writer has no
                                // Property/MethodSemantics table, and within-assembly
                                // access is `ldsfld` of the field directly, so the
                                // fields are public for now. Revisit (add property
                                // emission + private fields/ctor) if/when an external
                                // consumer needs the encapsulated closed set.
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

    /// Per closure: capture fields; `.ctor` + `Invoke`. Closures synthesise
    /// their typar names (`T0`, …) — only the count survives to codegen.
    ///
    /// A closure stays a ROOT even though it was lifted out of a module: its
    /// `TypeSlotKey.Closure name` is its ONLY address and that name is already
    /// globally unique, so nesting it would change its name / namespace / visibility
    /// and add a `NestedClass` row for a type nothing resolves.
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

                // The singleton field: `static readonly` of the closure's
                // own type. Its `Ty` is unused — the writer mints the self-type
                // signature from the closure's TypeDef handle, not from `Ty` (a
                // closure type has no `FrozenType` the encoder resolves).
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
