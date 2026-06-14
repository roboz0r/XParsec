namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

// Every ranged-table row enumerated as data; handle = position in the layout.
// One source of truth for row order; the writer walks it mechanically and
// forward-handle arithmetic derives from prefix sums over these lists only.

/// The method attribute sets, shared by the layout's method enumeration and
/// the `prepare` phase (so the writer's predicted-vs-actual row check compares
/// like with like).
[<AutoOpen>]
module internal MethodAttrSets =

    /// An abstract interface method (no body).
    let abstractMethodAttrs =
        MethodAttributes.Public
        ||| MethodAttributes.Abstract
        ||| MethodAttributes.Virtual
        ||| MethodAttributes.HideBySig
        ||| MethodAttributes.NewSlot

    let staticFactoryAttrs =
        MethodAttributes.Public
        ||| MethodAttributes.Static
        ||| MethodAttributes.HideBySig

    let staticMethodAttrs = staticFactoryAttrs

    // Non-virtual `public hidebysig` instance method (the union/record/class is
    // sealed, so `call` dispatch is correct).
    let instanceMethodAttrs = MethodAttributes.Public ||| MethodAttributes.HideBySig

    // A synthesised `Object.Equals`/`GetHashCode` override: reuse the base
    // virtual slot (no `NewSlot`) so name + signature matching makes it the
    // override.
    let overrideMethodAttrs =
        MethodAttributes.Public
        ||| MethodAttributes.Virtual
        ||| MethodAttributes.HideBySig

    // Typed `IEquatable<Self>::Equals(Self)` / `IComparable<Self>::CompareTo`
    // and interface-impl members: a *new* virtual slot (`Object` has none to
    // reuse), `Final` since sealed. The runtime binds it to the
    // `InterfaceImpl` by name + signature.
    let ifaceEqualsAttrs =
        MethodAttributes.Public
        ||| MethodAttributes.Virtual
        ||| MethodAttributes.HideBySig
        ||| MethodAttributes.NewSlot
        ||| MethodAttributes.Final

    let ctorAttrs =
        MethodAttributes.Public
        ||| MethodAttributes.HideBySig
        ||| MethodAttributes.SpecialName
        ||| MethodAttributes.RTSpecialName

    let cctorAttrs =
        MethodAttributes.Private
        ||| MethodAttributes.Static
        ||| MethodAttributes.HideBySig
        ||| MethodAttributes.SpecialName
        ||| MethodAttributes.RTSpecialName

    // A closure derives from `System.Object` and *implements* the
    // `Vesper.Fun\`2::Invoke` interface slot (R1) by name + signature; `Final`
    // because a sealed closure has no further overrides.
    let invokeAttrs =
        MethodAttributes.Public
        ||| MethodAttributes.Virtual
        ||| MethodAttributes.HideBySig
        ||| MethodAttributes.NewSlot
        ||| MethodAttributes.Final

    /// A property is emitted (and referenced) as `get_<name>`; a method keeps
    /// its name.
    let memberMetaName (mem: Frozen.TTypeMember) : string =
        match mem.Kind with
        | TMemberKind.Property -> "get_" + mem.Name
        | TMemberKind.Method -> mem.Name

/// Identity of one `TypeDefinition` row in the layout. Reuses existing
/// identities (§3.1): nominal types by `SymbolKey`, closures by their
/// synthesized unique name, holders by `Emit.HolderKey`. Structural equality;
/// a collision is a bug that should fail loudly (dictionary add throws).
[<RequireQualifiedAccess>]
type internal TypeKey =
    | ModulePseudo
    | Nominal of SymbolKey
    | Closure of name: string
    | Holder of Emit.HolderKey
    | Program

/// Which `Add*` recipe the writer uses for a `TypeSlot` — the only decision
/// left at write time.
[<RequireQualifiedAccess>]
type internal TypeSlotKind =
    | ModulePseudo
    | Interface
    | Union
    | Record
    /// `isSealed` reflects `[<Sealed>]`; a `[<Struct>]` value type flips
    /// sequential layout + `Sealed` + the `ValueType` base.
    | Class of isSealed: bool * isValueType: bool
    | Closure
    /// A named module holder; `HasCctor` ⇔ it owns module values (drops
    /// `BeforeFieldInit`).
    | Holder of hasCctor: bool
    /// The anonymous "Program" holder (holder-less fns + `Main`).
    | Program

/// Identity of one `Field` row in the layout (§3.1) — who resolves this
/// handle at `Bind` time. Structural; a collision fails loudly.
[<RequireQualifiedAccess>]
type internal FieldKey =
    | UnionTag of SymbolKey
    | UnionCaseField of SymbolKey * case: string * index: int
    | RecordField of SymbolKey * name: string
    /// A class primary-ctor parameter's backing field.
    | ClassCtorParamField of SymbolKey * name: string
    /// An explicit `val [mutable] x: T` instance field.
    | ClassInstanceField of SymbolKey * name: string
    /// A `static let` backing field.
    | ClassStaticField of SymbolKey * name: string
    | ClosureCapture of closure: string * index: int
    /// A module-level value's `public static` holder field.
    | ModuleValue of NodeKey

/// One `Field` row: the i-th entry of `AssemblyLayout.Fields` is table row
/// i+1. The layout stores only def-table rows; whether a *use site* routes
/// through a `MemberRef` on an open self-`TypeSpec` (generic types/closures,
/// G13) stays a `Bind`-time policy.
type internal FieldSlot =
    {
        Key: FieldKey
        Name: string
        Attrs: FieldAttributes
        Ty: FrozenType
        /// A *generic* closure's capture field encodes its signature inside the
        /// ambient closure-typar scope (the body's typars re-project onto the
        /// closure class's slots) — the writer brackets the `AddField` call in
        /// `EnterClosureTyparScope`/`ExitClosureTyparScope`. `ValueSome d` carries
        /// the closure's declaring-typar offset; `ValueNone` ⇒ no closure scope.
        ClosureScope: int voption
    }

/// Identity of one `MethodDef` row in the layout (§3.1). Indexed cases
/// (`Member`, `SecondaryCtor`, `InterfaceMethod`) use the position in the
/// declaring type's own list so same-named overloads can't collide; a class's
/// interface-impl members continue the `Member` index past its own members.
[<RequireQualifiedAccess>]
type internal MethodKey =
    | InterfaceMethod of SymbolKey * index: int
    /// A nominal type's first `.ctor` (union nullary / record / class primary).
    | NominalCtor of SymbolKey
    /// A class's synthesised `static let` `.cctor`.
    | NominalCctor of SymbolKey
    | SecondaryCtor of SymbolKey * index: int
    | UnionFactory of SymbolKey * case: string
    /// An augmentation member — index over `members @ ifaceMembers`.
    | Member of SymbolKey * index: int
    | EqGetHashCode of SymbolKey
    | EqEqualsObj of SymbolKey
    | EqEqualsTyped of SymbolKey
    | CmpCompareToTyped of SymbolKey
    | CmpCompareToObj of SymbolKey
    /// The synthesised `IStructuralFormattable.Format(IFormatSink)` (`%A`, P3).
    | FmtFormat of SymbolKey
    | ClosureCtor of closure: string
    | ClosureInvoke of closure: string
    | HolderCctor of Emit.HolderKey
    | StaticFn of NodeKey
    | Main

/// One `MethodDef` row: the i-th entry of `AssemblyLayout.Methods` is table
/// row i+1. Carries the row identity (name + attrs); the signature / body /
/// params are bound late (`PreparedMethod`), against resolved handles.
type internal MethodRow =
    {
        Key: MethodKey
        Name: string
        Attrs: MethodAttributes
    }

/// A bound method row ready to write: signature and body built at the Bind /
/// Prepare phase against resolved handles (body-stream order is free — only
/// the `MethodDef` row order matters, and the writer takes that from
/// `AssemblyLayout.Methods`).
type internal PreparedMethod =
    {
        Signature: BlobBuilder
        /// `-1` ⇒ abstract (no body).
        BodyOffset: int
        ParamNames: string list
        /// `GenericParam` rows owned by this method (metadata names, quote
        /// already dropped), added by the writer once the real handle exists.
        MethodTypars: string list
    }

/// The Prepare-minted handles a `TypeDefinition` row needs at write time,
/// keyed by `TypeKey`. Pre-minted because a generic parent's / interface's
/// `TypeSpec` encoding can depend on ambient state only live during the
/// type's Prepare window (e.g. the closure-typar scope).
type internal TypeRowExtras =
    {
        /// One `InterfaceImpl` entity handle per implemented interface.
        Interfaces: EntityHandle list
        /// The IL `TypeDefinition.BaseType` handle: `Object` for unions /
        /// records / closures and parent-less classes, the parent's resolved
        /// handle for an `inherit` clause, `System.ValueType` for a struct.
        BaseType: EntityHandle
    }

/// One `TypeDefinition` row. The i-th entry of `AssemblyLayout.Types` is table
/// row i+1 (index 0 = `<Module>` = row 1). `FieldCount`/`MethodCount` are this
/// type's contiguous ranged-table rows; first-field/first-method handles are
/// prefix sums over the list (see `Layout.deriveHandles`).
type internal TypeSlot =
    {
        Key: TypeKey
        Kind: TypeSlotKind
        Namespace: string
        /// Metadata name, already arity-suffixed (`SymbolKeyOps.arityName`).
        MetaName: string
        /// Metadata-layer typar names (leading F# quote dropped).
        Typars: string list
        FieldCount: int
        MethodCount: int
    }

/// The planned assembly: the ranged-table rows as data, plus the lowering
/// products the plan was computed from (computed once here, consumed by the
/// emission passes — they must never re-derive them; see
/// `feedback_walkelems_order_ctor_params`).
type internal AssemblyLayout =
    {
        /// Index 0 = `<Module>`; the i-th entry is TypeDef row i+1.
        Types: TypeSlot list
        /// The full `Field` table in row order (§2): union `_tag` + case
        /// fields → record fields → class ctor-param / `val` / static-let
        /// fields → closure captures → module-value fields (holder order).
        Fields: FieldSlot list
        /// The full `MethodDef` table in row order (§2): interface abstract
        /// methods → per union/record/class: ctor(s) + factories + members +
        /// [equality triple] + [comparison pair] → per closure: `.ctor` +
        /// `Invoke` → holder `.cctor`s + static fns (`HolderPlan.MethodPlan`
        /// order) → [`Main`].
        Methods: MethodRow list
        // ---- Reused lowering products ----
        Lowered: Frozen.TDecl list
        Plan: HolderPlan
        Closures: EmitTypes.Closure list
        ClosureByNode: Dictionary<Frozen.TExpr, EmitTypes.Closure>
        Partitioned: PartitionedTypeDecls
        /// The Program slot's presence is a layout decision: exe (`Main`) or
        /// holder-less fns.
        EmitEntryPoint: bool
        /// True iff *this* compilation defines the `%A` structural-format interfaces
        /// (it is `Vesper.Core`). Computed once here from `Partitioned.Interfaces`;
        /// the single source `formatRows` (suppress the `Format` row) and
        /// `Assembler.DefinesStructuralFormatInterfaces` / `NominalEmit` (suppress the
        /// `Format` body) all read, so the row reservation and the body emission can
        /// never disagree.
        DefinesStructuralFormatInterfaces: bool
    }

/// The resolved handle lookup derived from the layout once: `TypeKey` →
/// `TypeDefinitionHandle` (position), plus each type's first-field /
/// first-method handle from prefix-summing `FieldCount`/`MethodCount`.
/// **No handle arithmetic exists outside this derivation.**
type internal LayoutHandles =
    {
        TypeDefs: Dictionary<TypeKey, TypeDefinitionHandle>
        FirstFields: Dictionary<TypeKey, FieldDefinitionHandle>
        FirstMethods: Dictionary<TypeKey, MethodDefinitionHandle>
        MethodDefs: Dictionary<MethodKey, MethodDefinitionHandle>
        /// Total ranged-table rows the layout owns — writer-level row-count
        /// checks compare the real builder counts against these.
        TotalFields: int
        TotalMethods: int
    }

    member this.TypeDefOf(key: TypeKey) : TypeDefinitionHandle = this.TypeDefs.[key]
    member this.FirstFieldOf(key: TypeKey) : FieldDefinitionHandle = this.FirstFields.[key]
    member this.FirstMethodOf(key: TypeKey) : MethodDefinitionHandle = this.FirstMethods.[key]
    member this.MethodDefOf(key: MethodKey) : MethodDefinitionHandle = this.MethodDefs.[key]

module internal Layout =

    /// Single-walk partition of `tast.Decls` by `TTypeKind`.
    let partitionTypeDecls (decls: EqArray<Frozen.TDecl>) : PartitionedTypeDecls =
        let interfaces = ResizeArray()
        let unions = ResizeArray()
        let records = ResizeArray()
        let classes = ResizeArray()

        for d in decls do
            match d with
            | TDeclG.Type td ->
                match td.Kind with
                | TTypeKindG.Interface methods -> interfaces.Add(td, EqArray.toList methods)
                | TTypeKindG.Union(cases, members) ->
                    unions.Add
                        {
                            Decl = td
                            Cases = EqArray.toList cases
                            Members = EqArray.toList members
                        }
                | TTypeKindG.Record(fields, members) ->
                    records.Add
                        {
                            Decl = td
                            Fields = EqArray.toList fields
                            Members = EqArray.toList members
                        }
                | TTypeKindG.Class(fields,
                                   ctorParams,
                                   members,
                                   baseType,
                                   ifaces,
                                   isSealed,
                                   staticLets,
                                   secondaryCtors,
                                   baseCtorCall,
                                   isStruct) ->
                    classes.Add
                        {
                            Decl = td
                            Fields = EqArray.toList fields
                            CtorParams = EqArray.toList ctorParams
                            Members = EqArray.toList members
                            BaseType = baseType
                            Interfaces = [ for (ifaceTy, ms) in ifaces -> ifaceTy, EqArray.toList ms ]
                            IsSealed = isSealed
                            StaticLets = EqArray.toList staticLets
                            SecondaryCtors = EqArray.toList secondaryCtors
                            BaseCtorCall = baseCtorCall
                            IsStruct = isStruct
                        }
            | _ -> ()

        {
            Interfaces = List.ofSeq interfaces
            Unions = List.ofSeq unions
            Records = List.ofSeq records
            Classes = List.ofSeq classes
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

    /// The synthesised `IStructuralFormattable.Format` row (`%A`, P3). Emitted for
    /// *every* record / union — `%A` is orthogonal to the equality / comparison
    /// verdicts (it renders a value's structure, never depending on whether the type
    /// supports `=` / `<`). A new virtual slot bound to the `InterfaceImpl` by name +
    /// signature, like the typed `Equals(Self)`.
    let private formatRows (definesInterfaces: bool) (td: Frozen.TTypeDecl) : MethodRow list =
        if definesInterfaces then
            []
        else
            [
                {
                    Key = MethodKey.FmtFormat td.Key
                    Name = "Format"
                    Attrs = ifaceEqualsAttrs
                }
            ]

    let private nominalSlot
        (kind: TypeSlotKind)
        (td: Frozen.TTypeDecl)
        (fieldCount: int)
        (methodCount: int)
        : TypeSlot =
        {
            Key = TypeKey.Nominal td.Key
            Kind = kind
            Namespace = defaultArg td.Namespace ""
            MetaName = SymbolKeyOps.arityName td.Name td.TypeParams.Length
            Typars = typarNames td.TypeParams
            FieldCount = fieldCount
            MethodCount = methodCount
        }

    /// Enumerate the `TypeDefinition` rows in the canonical order (§2):
    /// `<Module>` → interfaces → unions → records → classes → closures →
    /// named holders → the anonymous "Program" holder (present only when an
    /// exe or holder-less fns exist). Reuses the existing lowering/discovery
    /// passes unchanged and carries their products.
    let build (project: ProjectInfo) (tast: Frozen.TastFile) : AssemblyLayout =
        let lowered = Emit.lower tast.Decls
        let plan = HolderPlan.create tast.ModuleMembers lowered

        // Member bodies never pass through `Emit.lower`; they only need the
        // closing `expandBuiltinOps` pass (`NominalEmit` used to apply it per
        // member). We run it **once here** so the expanded body is the single
        // object both closure discovery and `buildMember` walk — closure node
        // identity (`HashIdentity.Reference`) demands they be the same nodes.
        // The expanded partition is stored as `Partitioned`; `NominalEmit` reads
        // the already-expanded bodies.
        let expandMember (m: Frozen.TTypeMember) : Frozen.TTypeMember =
            { m with
                Body = Emit.expandBuiltinOps m.Body
            }

        let rawPartitioned = partitionTypeDecls tast.Decls

        let partitioned =
            { rawPartitioned with
                Unions =
                    [
                        for ud in rawPartitioned.Unions ->
                            { ud with
                                Members = List.map expandMember ud.Members
                            }
                    ]
                Records =
                    [
                        for rd in rawPartitioned.Records ->
                            { rd with
                                Members = List.map expandMember rd.Members
                            }
                    ]
                Classes =
                    [
                        for cd in rawPartitioned.Classes ->
                            { cd with
                                Members = List.map expandMember cd.Members
                                Interfaces = [ for (ty, ms) in cd.Interfaces -> ty, List.map expandMember ms ]
                            }
                    ]
            }

        // The assembly that *defines* the `%A` structural-format interfaces
        // (`Vesper.Core`) does not get the per-type `Format` row / body — its own
        // records would otherwise reference `IStructuralFormattable` through an
        // external `AssemblyRef` to Core itself. Computed once here and published on
        // `AssemblyLayout`: this is the *single* source the row reservation
        // (`formatRows`, below) and the body emission (`NominalEmit`, via
        // `Assembler.DefinesStructuralFormatInterfaces`) both read — so a reserved
        // `Format` row can never go un-prepared (the failure mode if the two drifted).
        let definesStructuralFormatInterfaces =
            partitioned.Interfaces
            |> List.exists (fun (td, _) -> RuntimeNames.isStructuralFormattableKey td.Key)

        // Closure-discovery roots from every (expanded) member body, each tagged
        // with its declaring type's typar count (0 ⇒ monomorphic).
        let memberRoots =
            [
                let root (td: Frozen.TTypeDecl) (m: Frozen.TTypeMember) : EmitClosures.MemberClosureRoot =
                    {
                        DeclaringTypars = td.TypeParams.Length
                        MethodTypars = m.MethodTypeParams.Length
                        Body = m.Body
                    }

                for ud in partitioned.Unions do
                    for m in ud.Members -> root ud.Decl m

                for rd in partitioned.Records do
                    for m in rd.Members -> root rd.Decl m

                for cd in partitioned.Classes do
                    for m in cd.Members -> root cd.Decl m

                    for (_, ms) in cd.Interfaces do
                        for m in ms -> root cd.Decl m
            ]

        let closures, closureByNode =
            Emit.discoverClosures
                plan.StaticFnKeys
                plan.ModuleValueKeys
                plan.StaticFnTypars
                tast.ClosureReprs
                lowered
                memberRoots

        let emitEntryPoint =
            match project.OutputKind with
            | Exe -> true
            | Library -> false

        let moduleSlot =
            {
                Key = TypeKey.ModulePseudo
                Kind = TypeSlotKind.ModulePseudo
                Namespace = ""
                MetaName = "<Module>"
                Typars = []
                FieldCount = 0
                MethodCount = 0
            }

        // Each type's slot, its field rows, and its method rows are built
        // together so `FieldCount` / `MethodCount` can never drift from the
        // enumerations that define the rows: the slot's counts are the lengths
        // of the very lists the writer walks.

        let interfaceParts =
            [
                for (td, methods) in partitioned.Interfaces ->
                    let methodRows =
                        methods
                        |> List.mapi (fun i m ->
                            {
                                Key = MethodKey.InterfaceMethod(td.Key, i)
                                Name = m.Name
                                Attrs = abstractMethodAttrs
                            }
                        )

                    nominalSlot TypeSlotKind.Interface td 0 (List.length methodRows), ([]: FieldSlot list), methodRows
            ]

        // Per union: `_tag` + every case's payload fields; nullary `.ctor`,
        // case factories, members, [equality triple], [comparison pair].
        let unionParts =
            [
                for ud in partitioned.Unions ->
                    let td = ud.Decl

                    let fields =
                        [
                            yield
                                {
                                    Key = FieldKey.UnionTag td.Key
                                    Name = "_tag"
                                    Attrs = FieldAttributes.Public
                                    Ty = FTConst("int", EqArray.empty)
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

                            yield! ud.Members |> List.mapi (fun i m -> memberRow td.Key i false m)
                            yield! equalityRows td
                            yield! comparisonRows td
                            yield! formatRows definesStructuralFormatInterfaces td
                        ]

                    nominalSlot TypeSlotKind.Union td (List.length fields) (List.length methodRows), fields, methodRows
            ]

        let recordParts =
            [
                for rd in partitioned.Records ->
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

                            yield! rd.Members |> List.mapi (fun i m -> memberRow td.Key i false m)
                            yield! equalityRows td
                            yield! comparisonRows td
                            yield! formatRows definesStructuralFormatInterfaces td
                        ]

                    nominalSlot TypeSlotKind.Record td (List.length fields) (List.length methodRows), fields, methodRows
            ]

        // Per class: ctor-param backing fields, then explicit `val [mutable]`
        // instance fields (mutable ⇒ plain writable; immutable ⇒ `initonly`),
        // then `static let` backing fields. Methods: primary `.ctor`,
        // [`.cctor` when static lets], [secondary `.ctor`s], own members,
        // interface-impl members.
        let classParts =
            [
                for cd in partitioned.Classes ->
                    let td = cd.Decl

                    let fields =
                        [
                            for p in cd.CtorParams ->
                                {
                                    Key = FieldKey.ClassCtorParamField(td.Key, p.Name)
                                    Name = p.Name
                                    Attrs = FieldAttributes.Public
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
                            for sl in cd.StaticLets ->
                                {
                                    Key = FieldKey.ClassStaticField(td.Key, sl.Name)
                                    Name = sl.Name
                                    Attrs = FieldAttributes.Private ||| FieldAttributes.Static
                                    Ty = sl.Type
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

                            if not (List.isEmpty cd.StaticLets) then
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

                            let ownCount = List.length cd.Members
                            yield! cd.Members |> List.mapi (fun i m -> memberRow td.Key i false m)

                            yield!
                                [
                                    for (_, ms) in cd.Interfaces do
                                        yield! ms
                                ]
                                |> List.mapi (fun i m -> memberRow td.Key (ownCount + i) true m)
                        ]

                    nominalSlot
                        (TypeSlotKind.Class(cd.IsSealed, cd.IsStruct))
                        td
                        (List.length fields)
                        (List.length methodRows),
                    fields,
                    methodRows
            ]

        // Per closure: capture fields; `.ctor` + `Invoke`. Closures synthesise
        // their typar names (`T0`, …) — only the count survives to codegen.
        let closureParts =
            [
                for c in closures ->
                    let isGeneric = c.Typars > 0

                    let fields =
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
                        ]

                    let slot =
                        {
                            Key = TypeKey.Closure c.Name
                            Kind = TypeSlotKind.Closure
                            Namespace = ""
                            MetaName = SymbolKeyOps.arityName c.Name c.Typars
                            Typars = [ for i in 0 .. c.Typars - 1 -> sprintf "T%d" i ]
                            FieldCount = List.length fields
                            MethodCount = List.length methodRows
                        }

                    slot, fields, methodRows
            ]

        // Named holders in plan order: module-value fields (immutable ⇒
        // `initonly`, set only in the holder `.cctor`); [`.cctor` when it has
        // values] + its fns (`HolderPlan.MethodPlan` slot order).
        let holderParts =
            [
                for h in plan.OrderedNamedHolders ->
                    let ns, holderName = h
                    let values = HolderPlan.holderValues plan h

                    let fields =
                        [
                            for mv in values ->
                                {
                                    Key = FieldKey.ModuleValue mv.Key
                                    Name = mv.Name
                                    Attrs =
                                        FieldAttributes.Public ||| FieldAttributes.Static ||| FieldAttributes.InitOnly
                                    Ty = mv.Ty
                                    ClosureScope = ValueNone
                                }
                        ]

                    let fnCount =
                        plan.StaticFns |> List.filter (fun fn -> fn.Holder = Some h) |> List.length

                    let hasCctor = not (List.isEmpty values)

                    let slot =
                        {
                            Key = TypeKey.Holder h
                            Kind = TypeSlotKind.Holder hasCctor
                            Namespace = defaultArg ns ""
                            MetaName = holderName
                            Typars = []
                            FieldCount = List.length fields
                            MethodCount = (if hasCctor then 1 else 0) + fnCount
                        }

                    slot, fields
            ]

        let slotOf (slot, _, _) = slot
        let fieldsOf (_, fields, _) = fields
        let methodsOf (_, _, methods) = methods

        let interfaceSlots = List.map slotOf interfaceParts
        let unionSlots = List.map slotOf unionParts
        let recordSlots = List.map slotOf recordParts
        let classSlots = List.map slotOf classParts
        let closureSlots = List.map slotOf closureParts
        let holderSlots = List.map fst holderParts

        // The full `MethodDef` table in §2 row order. Each nominal type's rows
        // are the very list its slot's `MethodCount` counted, so the two can't
        // disagree; the holder `.cctor`s + static fns follow in
        // `HolderPlan.MethodPlan` order, then `Main`. `Assembler.WriteMethods`
        // walks this list once, asserting each actual `MethodDef` handle matches
        // the prefix-sum prediction in `LayoutHandles`.
        let methods =
            [
                yield! interfaceParts |> List.collect methodsOf
                yield! unionParts |> List.collect methodsOf
                yield! recordParts |> List.collect methodsOf
                yield! classParts |> List.collect methodsOf
                yield! closureParts |> List.collect methodsOf

                for slot in plan.MethodPlan do
                    match slot with
                    | HolderCctor h ->
                        yield
                            {
                                Key = MethodKey.HolderCctor h
                                Name = ".cctor"
                                Attrs = cctorAttrs
                            }
                    | HolderFn fn ->
                        yield
                            {
                                Key = MethodKey.StaticFn fn.Key
                                Name = fn.Name
                                Attrs = staticMethodAttrs
                            }

                if emitEntryPoint then
                    yield
                        {
                            Key = MethodKey.Main
                            Name = "Main"
                            Attrs = staticMethodAttrs
                        }
            ]

        let programSlots =
            if emitEntryPoint || not (List.isEmpty plan.HolderlessFns) then
                [
                    {
                        Key = TypeKey.Program
                        Kind = TypeSlotKind.Program
                        Namespace = ""
                        MetaName = project.ModuleName
                        Typars = []
                        FieldCount = 0
                        MethodCount = List.length plan.HolderlessFns + (if emitEntryPoint then 1 else 0)
                    }
                ]
            else
                []

        let types =
            moduleSlot :: interfaceSlots
            @ unionSlots
            @ recordSlots
            @ classSlots
            @ closureSlots
            @ holderSlots
            @ programSlots

        // Every nominal slot's `MethodCount` is the length of the very row list
        // it contributes, so they can't drift. This guard still earns its keep
        // over the holder / Program tail, whose counts come from `MethodPlan`
        // arithmetic rather than the rows: a mismatch there means a missed
        // conditional row — fail here, not at serialize.
        let slotMethodTotal = types |> List.sumBy (fun s -> s.MethodCount)

        if slotMethodTotal <> List.length methods then
            failwithf
                "Layout: type slots claim %d method rows but the method enumeration has %d"
                slotMethodTotal
                (List.length methods)

        {
            Types = types
            Fields =
                List.collect fieldsOf unionParts
                @ List.collect fieldsOf recordParts
                @ List.collect fieldsOf classParts
                @ List.collect fieldsOf closureParts
                @ List.collect snd holderParts
            Methods = methods
            Lowered = lowered
            Plan = plan
            Closures = closures
            ClosureByNode = closureByNode
            Partitioned = partitioned
            EmitEntryPoint = emitEntryPoint
            DefinesStructuralFormatInterfaces = definesStructuralFormatInterfaces
        }

    /// Derive every handle from the layout once: TypeDef handle = list position
    /// + 1; first-field / first-method handles by prefix-summing the slots'
    /// `FieldCount` / `MethodCount` (an empty range naturally points past the
    /// end of the previous owner's range).
    let deriveHandles (layout: AssemblyLayout) : LayoutHandles =
        let typeDefs = Dictionary<TypeKey, TypeDefinitionHandle>()
        let firstFields = Dictionary<TypeKey, FieldDefinitionHandle>()
        let firstMethods = Dictionary<TypeKey, MethodDefinitionHandle>()
        let methodDefs = Dictionary<MethodKey, MethodDefinitionHandle>()
        let mutable fieldCursor = 0
        let mutable methodCursor = 0

        layout.Types
        |> List.iteri (fun i slot ->
            typeDefs.Add(slot.Key, MetadataTokens.TypeDefinitionHandle(i + 1))
            firstFields.Add(slot.Key, MetadataTokens.FieldDefinitionHandle(fieldCursor + 1))
            firstMethods.Add(slot.Key, MetadataTokens.MethodDefinitionHandle(methodCursor + 1))
            fieldCursor <- fieldCursor + slot.FieldCount
            methodCursor <- methodCursor + slot.MethodCount
        )

        layout.Methods
        |> List.iteri (fun i row -> methodDefs.Add(row.Key, MetadataTokens.MethodDefinitionHandle(i + 1)))

        {
            TypeDefs = typeDefs
            FirstFields = firstFields
            FirstMethods = firstMethods
            MethodDefs = methodDefs
            TotalFields = fieldCursor
            TotalMethods = methodCursor
        }
