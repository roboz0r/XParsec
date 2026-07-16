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
    // `Vesper.Fun\`2::Invoke` interface slot by name + signature; `Final`
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
/// identities: nominal types by `SymbolKey`, closures by their
/// synthesized unique name, holders by `Emit.HolderKey`. Structural equality;
/// a collision is a bug that should fail loudly (dictionary add throws).
[<RequireQualifiedAccess>]
type internal TypeSlotKey =
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
    /// `valueKind` selects reference vs `[<Struct>]` value type (flips the
    /// `System.ValueType` base) — a record is always sealed and never byref-like.
    | Record of valueKind: ClassValueKind
    /// `isSealed` reflects `[<Sealed>]`; `valueKind` selects reference vs
    /// `[<Struct>]` value type (flips sequential layout + `Sealed` + the
    /// `ValueType` base) vs `[<IsByRefLike>]` byref-like (additionally stamps the
    /// `IsByRefLikeAttribute` custom attribute).
    | Class of isSealed: bool * valueKind: ClassValueKind
    | Closure
    /// A numeric enum: a sealed `System.Enum` subclass — no methods, a
    /// special-name `value__` instance field, and one `static literal` field per case.
    | Enum
    /// A string / mixed enum: a sealed `[<Struct>]` value type over a
    /// single field (`string`, or `obj` when `isMixed`), with a `.ctor` setting it,
    /// per-case `static initonly` fields, and a `.cctor` constructing them. `isMixed`
    /// is carried only for documentation symmetry with the writer; the base is
    /// always `System.ValueType`.
    | StructEnum of isMixed: bool
    /// A named module holder; `HasCctor` ⇔ it owns module values (drops
    /// `BeforeFieldInit`).
    | Holder of hasCctor: bool
    /// The anonymous "Program" holder (holder-less fns + `Main` + the top-level value fields). `hasCctor` ⇔ it owns leading-prefix values (drops
    /// `BeforeFieldInit`, its `.cctor` runs before `Main`).
    | Program of hasCctor: bool

/// Identity of one `Field` row in the layout — who resolves this
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
    /// An instance-`let` binder's backing field (a class-preamble `let`, stored by the
    /// primary `.ctor`). Distinct from `ClassInstanceField` — the `val` form is the
    /// user's own surface, this is compiler-generated storage — but both resolve by
    /// name at a `this.x` use site.
    | ClassLetField of SymbolKey * name: string
    /// A `static let` backing field.
    | ClassStaticField of SymbolKey * name: string
    /// A numeric enum's special-name `value__` instance field (its underlying
    /// integral storage).
    | EnumValueField of SymbolKey
    /// A numeric enum's `static literal` case field (`E::A`); also a string/mixed
    /// enum's `public static initonly` case field (`.cctor`-initialised, holding the
    /// constructed wrapper).
    | EnumCaseField of SymbolKey * case: string
    /// A string/mixed enum's single instance field (the wrapped `string` / `obj`).
    | EnumBackingField of SymbolKey
    | ClosureCapture of closure: string * index: int
    /// A non-capturing, monomorphic closure's `static readonly` singleton field —
    /// the one cached instance every construction site `ldsfld`s.
    | ClosureCached of closure: string
    /// A module-level value's `public static` holder field, keyed by its
    /// `SymbolKey` (declaring holder + emitted name) so the ONE combined field-def
    /// map stays injective across compilation units and under entry-file shadowing —
    /// a bare per-file `NodeKey` collides on both axes (`Emit.ModuleValue.SymbolKey`).
    | ModuleValue of SymbolKey

/// One `Field` row: the i-th entry of `AssemblyLayout.Fields` is table row
/// i+1. The layout stores only def-table rows; whether a *use site* routes
/// through a `MemberRef` on an open self-`TypeSpec` (generic types/closures) stays a `Bind`-time policy.
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

/// Identity of one `MethodDef` row in the layout. Indexed cases
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
    /// The synthesised `IStructuralFormattable.Format(IFormatSink)` (`%A`).
    | FmtFormat of SymbolKey
    /// A synthesised capability co-slot (`CoSlot`) — the BCL members a capability's
    /// platform face inherits but its member surface never declared.
    | CapCoSlot of SymbolKey * CoSlot
    | ClosureCtor of closure: string
    | ClosureInvoke of closure: string
    /// A non-capturing, monomorphic closure's `.cctor` — `newobj`s the closure once
    /// and `stsfld`s it into `FieldKey.ClosureCached`. Present only
    /// for a cached closure; a capturing/generic closure has none.
    | ClosureCctor of closure: string
    | HolderCctor of Emit.HolderKey
    /// The anonymous "Program" holder's `.cctor` — initialises the
    /// leading-prefix top-level values; at most one per assembly.
    | ProgramCctor
    /// A top-level function lowered to a static method, keyed by its `SymbolKey`
    /// (declaring holder + emitted name) so the ONE combined method-def map stays
    /// injective across compilation units and under entry-file shadowing — a bare
    /// per-file `NodeKey` collides on both axes (`Emit.StaticFn.SymbolKey`).
    | StaticFn of SymbolKey
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
/// keyed by `TypeSlotKey`. Pre-minted because a generic parent's / interface's
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

/// One `TypeDefinition` row. It carries NO row counts: a type's field / method rows
/// are the lists on its `TypeNode`, and every prefix sum is taken over those — so
/// there is nothing here that could disagree with them.
type internal TypeSlot =
    {
        Key: TypeSlotKey
        Kind: TypeSlotKind
        /// The `TypeDef` namespace column. EMPTY for a nested type — a nested type's
        /// namespace is its enclosing type's, which is the CLR rule.
        Namespace: string
        /// Metadata name, already arity-suffixed (`SymbolKeyOps.arityName`). ONE
        /// segment: the holder chain lives in `TypeNode.Enclosing` (a `NestedClass`
        /// row), never in the name.
        MetaName: string
        /// Metadata-layer typar names (leading F# quote dropped).
        Typars: string list
    }

/// One node of the emitted type HIERARCHY: a `TypeDefinition` row together with the
/// ranged-table rows it owns and the types nested inside it. The `TypeDef` table IS
/// the pre-order flattening of the roots, and `AssemblyLayout`'s `Types` / `Fields` /
/// `Methods` are `List.collect`s over that flattening — so a type's range and the rows
/// in it agree BY DERIVATION, not by a count kept in step by hand.
///
/// `Enclosing` is `ValueNone` for a root (`<Module>`, a namespace-level type, a
/// closure, a root module's holder, `Program`) and `ValueSome` for a type the CLR
/// nests — exactly the types that get a `NestedClass` row and nested visibility.
type internal TypeNode =
    {
        Slot: TypeSlot
        Enclosing: TypeSlotKey voption
        Fields: FieldSlot list
        Methods: MethodRow list
        Nested: TypeNode list
    }

/// One compilation unit's contribution to the assembly, as data — everything a
/// unit produces on its own, BEFORE the single `<Module>` pseudo-type and the
/// single Program holder are minted (both belong to the assembly, not a unit, so
/// `Layout.combine` mints them once and `Layout.buildUnit` never does). A future
/// multi-unit driver builds one of these per source unit and hands the list to
/// `combine`.
type internal UnitLayout =
    {
        /// This unit's placeable ROOT nodes — its namespace-level nominals, its
        /// closures, its root-module holders (each carrying its own nested subtree)
        /// — with the `<Module>` and Program roots deliberately absent. `combine`
        /// concatenates these across units between the one `<Module>` head and the
        /// one Program tail.
        Roots: TypeNode list
        /// This unit's contribution to the completeness check's built-key set: every
        /// nominal, closure and holder key it built, independently of how they were
        /// placed in the tree. `combine` adds the `<Module>` and Program keys and asks
        /// the set question once over the whole assembly.
        BuiltKeys: TypeSlotKey list
        Lowered: Frozen.TDecl list
        Plan: HolderPlan
        Closures: EmitTypes.Closure list
        ClosureByNode: Dictionary<Frozen.TExpr, EmitTypes.Closure>
        Partitioned: PartitionedTypeDecls
        /// This unit's source-lambda value-struct closure verdicts, snapshotted from its
        /// own `tast.FunVerdicts`. The Assembler's per-unit closure-verdict rewrite reads
        /// exactly this unit's verdicts (a foreign unit's node keys mean nothing to it),
        /// which is why it rides on the unit rather than a single ctor-level table.
        FunVerdicts: Map<NodeKey, FunVerdict>
        /// Whether this unit carries the entry point (`Main`). `buildUnit` leaves it FALSE
        /// — the OutputKind decision belongs to the whole assembly, not a file — and
        /// `combine` stamps it TRUE on the single entry unit (an executable's last file)
        /// and FALSE on all others, so `PrepareMain` fires exactly once.
        EmitEntryPoint: bool
        /// True iff this unit defines the `%A` structural-format interfaces
        /// (it is `Vesper.Core`). See `AssemblyLayout.DefinesStructuralFormatInterfaces`.
        DefinesStructuralFormatInterfaces: bool
    }

/// The planned assembly: the ranged-table rows as data, plus the lowering
/// products the plan was computed from (computed once here, consumed by the
/// emission passes — they must never re-derive them; see
/// `feedback_walkelems_order_ctor_params`).
type internal AssemblyLayout =
    {
        /// The `TypeDef` table: the PRE-ORDER flattening of the type hierarchy, so each
        /// holder is immediately followed by the types it holds. Index 0 = `<Module>`;
        /// the i-th entry is TypeDef row i+1.
        Types: TypeNode list
        /// The full `Field` table in row order — the fields of `Types`, in `Types`
        /// order. Derived, never assembled a second time.
        Fields: FieldSlot list
        /// The full `MethodDef` table in row order — the methods of `Types`, in `Types`
        /// order. Derived, never assembled a second time.
        Methods: MethodRow list
        /// The Program slot's presence is a layout decision: exe (`Main`) or
        /// holder-less fns. True iff some unit carries the entry point.
        EmitEntryPoint: bool
        /// True iff *this* compilation defines the `%A` structural-format interfaces
        /// (it is `Vesper.Core`). Computed once here from `Partitioned.Interfaces`;
        /// the single source `formatRows` (suppress the `Format` row) and
        /// `Assembler.DefinesStructuralFormatInterfaces` / `NominalEmit` (suppress the
        /// `Format` body) all read, so the row reservation and the body emission can
        /// never disagree.
        DefinesStructuralFormatInterfaces: bool
        /// The per-unit products this layout was combined from — one per source file.
        /// Every per-unit datum the emission passes need (lowered decls, holder plan,
        /// closures, partition, closure verdicts, the entry flag) lives here, keyed so a
        /// unit's bodies resolve their own file-local nodes; the shared registries and the
        /// one combined row space live on the Assembler.
        Units: UnitLayout list
    }

/// The resolved handle lookup derived from the layout once: `TypeSlotKey` →
/// `TypeDefinitionHandle` (position), plus each type's first-field /
/// first-method handle from prefix-summing `FieldCount`/`MethodCount`.
/// **No handle arithmetic exists outside this derivation.**
type internal LayoutHandles =
    {
        TypeDefs: Dictionary<TypeSlotKey, TypeDefinitionHandle>
        FirstFields: Dictionary<TypeSlotKey, FieldDefinitionHandle>
        FirstMethods: Dictionary<TypeSlotKey, MethodDefinitionHandle>
        MethodDefs: Dictionary<MethodKey, MethodDefinitionHandle>
        /// Total ranged-table rows the layout owns — writer-level row-count
        /// checks compare the real builder counts against these.
        TotalFields: int
        TotalMethods: int
    }

    member this.TypeDefOf(key: TypeSlotKey) : TypeDefinitionHandle = this.TypeDefs.[key]
    member this.FirstFieldOf(key: TypeSlotKey) : FieldDefinitionHandle = this.FirstFields.[key]
    member this.FirstMethodOf(key: TypeSlotKey) : MethodDefinitionHandle = this.FirstMethods.[key]
    member this.MethodDefOf(key: MethodKey) : MethodDefinitionHandle = this.MethodDefs.[key]

module internal Layout =

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

    /// Build ONE compilation unit's contribution to the type HIERARCHY, and its slice of
    /// the ranged tables. By kind: namespace-level interfaces / unions / records / classes
    /// / enums → closures → the root modules' holders. A module's holder is immediately
    /// followed by the types it holds — in that same by-kind order — and by its nested
    /// modules' holders. The single `<Module>` pseudo-type (TypeDef row 1) and the single
    /// Program holder are NOT minted here: they belong to the assembly, so `combine` mints
    /// them once around the concatenated units. The shared `ClosureNamer` is threaded in so
    /// a multi-unit driver can keep closure TypeDef names unique assembly-wide. Reuses the
    /// existing lowering/discovery passes unchanged and carries their products.
    let buildUnit
        (closureNamer: Emit.ClosureNamer)
        (symbols: ICodegenSymbols)
        (project: ProjectInfo)
        (tast: Frozen.TastFile)
        : UnitLayout =
        let lowered0 = Emit.lower tast.Decls
        // The anonymous "Program" holder's key — a module of that name in the global
        // namespace. It owns the holder-less fns + `Main` + the top-level value fields /
        // `.cctor`. Its type slot is `TypeSlotKey.Program`, not `TypeSlotKey.Holder`, so
        // this key never names a holder class: it exists only to tag those values'
        // `Holder` field.
        let programHolder =
            SymbolKeyOps.moduleKeyOf (ModuleHolder.InNamespace NamespaceKey.Global) project.ModuleName

        // `(ns, name)` of every `[<Struct; IsByRefLike>]` type — a top-level value of
        // such a type can't be a static field; computed from `tast.Decls` since
        // `Emit.lower` strips the type decls `lowered` would carry.
        let refStructNsNames =
            tast.Decls
            |> EqArray.toList
            |> List.choose (fun d ->
                match d with
                | TDeclG.Type td ->
                    match td.Kind with
                    | TTypeKindG.Class c when c.ValueKind = ClassValueKind.RefStruct ->
                        Some(Emit.typeKeyNsName td.TypeKey)
                    | _ -> None
                | _ -> None
            )
            |> HashSet

        // `HolderPlan.create` eta-expands every non-saturated reference to a
        // static-eligible module function (`bridgeStaticFnEscapes`) — keeping the flat
        // static method and adding a wrapper closure (F#/JS model) — and publishes the
        // bridged decls as `plan.Lowered`. That single rewritten list feeds both
        // closure discovery and `buildMain`, so they see the same nodes the plan was
        // computed from.
        let plan =
            HolderPlan.create
                tast.ModuleMembers
                tast.GenericFnSchemes
                programHolder
                tast.TopLevelNames
                refStructNsNames
                lowered0

        let lowered = plan.Lowered

        // Member bodies never pass through `Emit.lower` — they need no lowering at all;
        // they arrive from the freeze ready to emit. Partitioned **once** here and
        // published as `Partitioned`, so closure discovery and `buildMember` walk the
        // same node objects — closure node identity (`HashIdentity.Reference`) demands it.
        let partitioned = partitionTypeDecls tast.Decls

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
            |> List.exists (fun (td, _) -> RuntimeNames.isStructuralFormattableKey td.TypeKey)

        // Closure-discovery roots from every (expanded) member body and class-preamble
        // expression, each tagged with its declaring type's typar count (0 ⇒
        // monomorphic). A preamble initialiser or `do` body is emitted into the `.ctor` /
        // `.cctor` from these very nodes, so a lambda in one (a function-valued `let`,
        // `let bump x = …`) is a closure exactly as a member body's is — omit it and its
        // construction site finds no discovered closure.
        let memberRoots =
            [
                let root
                    (td: Frozen.TTypeDecl)
                    (methodTypars: int)
                    (body: Frozen.TExpr)
                    : EmitClosures.MemberClosureRoot =
                    {
                        DeclaringTypars = td.TypeParams.Length
                        MethodTypars = methodTypars
                        Body = body
                    }

                let memberRoot (td: Frozen.TTypeDecl) (m: Frozen.TTypeMember) =
                    root td (GeneralizedTypars.count m.MethodTypeParams) m.Body

                let preambleRoot (td: Frozen.TTypeDecl) (entry: Frozen.TPreambleEntry) =
                    match entry with
                    | TPreambleEntryG.Let l -> root td 0 l.Init
                    | TPreambleEntryG.Do e -> root td 0 e

                for ud in partitioned.Unions do
                    for m in ud.Members -> memberRoot ud.Decl m

                    for (_, ms) in ud.Interfaces do
                        for m in ms -> memberRoot ud.Decl m

                for rd in partitioned.Records do
                    for m in rd.Members -> memberRoot rd.Decl m

                for cd in partitioned.Classes do
                    for m in cd.Members -> memberRoot cd.Decl m

                    for (_, ms) in cd.Interfaces do
                        for m in ms -> memberRoot cd.Decl m

                    for entry in cd.StaticPreamble @ cd.InstancePreamble -> preambleRoot cd.Decl entry
            ]

        let closures, closureByNode =
            Emit.discoverClosures
                closureNamer
                plan.StaticFnKeys
                plan.ModuleValueKeys
                plan.StaticFnTypars
                // The node-keyed value-struct closure verdicts,
                // snapshotted in `Pipeline` like `ClosureReprs`.
                // `discoverClosures` marks a source-lambda argument a value-struct (and
                // at what flat arity) by node membership — no structural re-derivation.
                tast.FunVerdicts
                tast.ClosureReprs
                lowered
                memberRoots

        // Each type's node carries its own field and method rows: the rows the writer
        // walks ARE the rows its range claims, since both are `List.collect`s over the
        // same flattening.

        let interfaceNodes =
            [
                for (td, methods) in partitioned.Interfaces ->
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

        // Per union: `_tag` + every case's payload fields; nullary `.ctor`,
        // case factories, members, [equality triple], [comparison pair].
        let unionNodes =
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
                            yield! formatRows definesStructuralFormatInterfaces td
                            yield! coSlotRows symbols td ud.Interfaces
                        ]

                    nominalNode TypeSlotKind.Union td fields methodRows
            ]

        let recordNodes =
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

                            yield! ownAndIfaceMemberRows td.Key rd.Members rd.Interfaces

                            yield! equalityRows td
                            yield! comparisonRows td
                            yield! formatRows definesStructuralFormatInterfaces td
                            yield! coSlotRows symbols td rd.Interfaces
                        ]

                    nominalNode (TypeSlotKind.Record rd.ValueKind) td fields methodRows
            ]

        // Per class: ctor-param backing fields, then explicit `val [mutable]`
        // instance fields (mutable ⇒ plain writable; immutable ⇒ `initonly`),
        // then the instance-`let` and `static let` backing fields. Methods: primary
        // `.ctor`, [`.cctor` when a static preamble exists], [secondary `.ctor`s], own
        // members, interface-impl members.
        //
        // COMPILER-GENERATED backing storage (ctor-param, instance-`let` and `static let`
        // fields) is `assembly`, matching FSC: a lambda in a member body — or in a
        // preamble initialiser — is lifted into a closure class nested in the enclosing
        // MODULE, not in the class, so it reads the class's storage as a *different
        // type* — reachable only assembly-wide. `private` would make that read fault at
        // JIT time with `FieldAccessException`, and `public` would leak non-API storage.
        // A declared `val` field is the user's own surface and stays `public`.
        let compilerGeneratedStorage = FieldAttributes.Assembly

        let classNodes =
            [
                for cd in partitioned.Classes ->
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

        // Per numeric enum: the special-name `value__` instance field
        // (the underlying integral storage the CLR reads for `Enum.GetUnderlyingType`)
        // then one `public static literal` field per case (its constant integer is
        // attached as a `Constant` row in the writer's field pass). No methods —
        // equality/hashing/compare all come from the `System.Enum` base.
        let enumNodes =
            [
                for ed in partitioned.Enums ->
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

        // Per string/mixed enum: a `[<Struct>]` wrapper. One instance
        // backing field (`string`, or `obj` when mixed) holding the case value, then
        // one `public static initonly` field per case (the constructed singleton, set
        // in the `.cctor`). Two methods: the `.ctor(field)` that stores the backing
        // field, and the `.cctor` that constructs each case. Methods are bound in
        // `Assembler.PrepareStructEnums`.
        let structEnumNodes =
            [
                for sed in partitioned.StructEnums ->
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
                                    Attrs =
                                        FieldAttributes.Public ||| FieldAttributes.Static ||| FieldAttributes.InitOnly
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

        // Per closure: capture fields; `.ctor` + `Invoke`. Closures synthesise
        // their typar names (`T0`, …) — only the count survives to codegen.
        //
        // A closure stays a ROOT even though it was lifted out of a module: its
        // `TypeSlotKey.Closure name` is its ONLY address and that name is already
        // globally unique, so nesting it would change its name / namespace / visibility
        // and add a `NestedClass` row for a type nothing resolves.
        let closureNodes =
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
                                    Attrs =
                                        FieldAttributes.Public ||| FieldAttributes.Static ||| FieldAttributes.InitOnly
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

        // Every nominal type, in the by-kind order the `TypeDef` table has always used.
        // That order now applies *within* each holder (and among the roots) rather than
        // globally: filtering this list by holder preserves it.
        let nominalNodes =
            interfaceNodes
            @ unionNodes
            @ recordNodes
            @ classNodes
            @ enumNodes
            @ structEnumNodes

        // ---- Holder discovery ------------------------------------------------------
        //
        // A holder class is needed for every module that HOLDS something emitted, and
        // for every module on the way down to it — a `NestedClass` row needs its
        // enclosing `TypeDef` to exist. Three sources, and all three are necessary:
        //
        //   * the plan's holders — modules with static fns / module values;
        //   * every module named in an emitted TYPE's holder chain — a module that holds
        //     only types has no binding, so the plan never names it;
        //   * their ANCESTORS — a nested module `A.B` is a class nested in `A`'s holder,
        //     which must exist even when `A` itself holds nothing directly.
        //
        // Ancestors-first, first-appearance order, deduplicated: a parent is therefore
        // always discovered before its children.
        let orderedHolders =
            let seen = HashSet<ModuleKey>()
            let acc = ResizeArray<ModuleKey>()

            let rec add (m: ModuleKey) =
                match m.Holder with
                | ModuleHolder.InModule parent -> add parent
                | ModuleHolder.InNamespace _ -> ()

                if seen.Add m then
                    acc.Add m

            for h in plan.OrderedNamedHolders do
                add h

            for node in nominalNodes do
                match node.Enclosing with
                | ValueSome(TypeSlotKey.Holder m) -> add m
                | _ -> ()

            List.ofSeq acc

        let holderMethodRows (h: Emit.HolderKey) : MethodRow list =
            [
                // The `.cctor` initialises the holder's module values; it precedes the
                // fns exactly as `HolderPlan.MethodPlan` prepares them.
                if not (List.isEmpty (HolderPlan.holderValues plan h)) then
                    yield
                        {
                            Key = MethodKey.HolderCctor h
                            Name = ".cctor"
                            Attrs = cctorAttrs
                        }

                for fn in plan.StaticFns do
                    if fn.Holder = Some h then
                        yield
                            {
                                Key = MethodKey.StaticFn fn.SymbolKey
                                Name = fn.Name
                                Attrs = staticMethodAttrs
                            }
            ]

        // A holder node: its module-value fields (immutable ⇒ `initonly`, set only in
        // the holder `.cctor`), its methods, and — nested inside it — the types it
        // holds followed by its child holders.
        let rec holderNode (h: Emit.HolderKey) : TypeNode =
            let values = HolderPlan.holderValues plan h

            let fields =
                [
                    for mv in values ->
                        {
                            Key = FieldKey.ModuleValue mv.SymbolKey
                            Name = mv.Name
                            Attrs = FieldAttributes.Public ||| FieldAttributes.Static ||| FieldAttributes.InitOnly
                            Ty = mv.Ty
                            ClosureScope = ValueNone
                        }
                ]

            let held =
                nominalNodes
                |> List.filter (fun n -> n.Enclosing = ValueSome(TypeSlotKey.Holder h))

            let children =
                orderedHolders
                |> List.filter (fun m -> m.Holder = ModuleHolder.InModule h)
                |> List.map holderNode

            {
                Slot =
                    {
                        Key = TypeSlotKey.Holder h
                        Kind = TypeSlotKind.Holder(not (List.isEmpty values))
                        // A nested module's holder is a class nested in its parent's
                        // holder, so its namespace column is empty; a root module's
                        // carries the declaring namespace.
                        Namespace =
                            match h.Holder with
                            | ModuleHolder.InNamespace ns -> ns.Dotted
                            | ModuleHolder.InModule _ -> ""
                        MetaName = h.Name
                        Typars = []
                    }
                Enclosing =
                    match h.Holder with
                    | ModuleHolder.InModule parent -> ValueSome(TypeSlotKey.Holder parent)
                    | ModuleHolder.InNamespace _ -> ValueNone
                Fields = fields
                Methods = holderMethodRows h
                Nested = held @ children
            }

        let rootHolderNodes =
            orderedHolders
            |> List.filter (fun m ->
                match m.Holder with
                | ModuleHolder.InNamespace _ -> true
                | ModuleHolder.InModule _ -> false
            )
            |> List.map holderNode

        // The single `<Module>` pseudo-type and the single Program holder are minted by
        // `combine`, not here — they belong to the assembly, not a unit. This unit hands
        // over its placeable roots (namespace-level nominals, then closures, then root
        // holders — each carrying its own subtree) and the flat key set for the
        // completeness check.
        {
            Roots =
                (nominalNodes |> List.filter (fun n -> n.Enclosing.IsNone))
                @ closureNodes
                @ rootHolderNodes
            BuiltKeys =
                [
                    for n in nominalNodes -> n.Slot.Key
                    for n in closureNodes -> n.Slot.Key
                    for h in orderedHolders -> TypeSlotKey.Holder h
                ]
            Lowered = lowered
            Plan = plan
            Closures = closures
            ClosureByNode = closureByNode
            Partitioned = partitioned
            FunVerdicts = tast.FunVerdicts
            // The entry flag is the whole-assembly OutputKind decision, made by `combine`
            // (an executable's LAST file is the entry unit); a file cannot know it alone.
            EmitEntryPoint = false
            DefinesStructuralFormatInterfaces = definesStructuralFormatInterfaces
        }

    /// Assemble the units into the whole `AssemblyLayout`: PREPEND the single `<Module>`
    /// pseudo-type (so it is TypeDef row 1 for the assembly), APPEND the single Program
    /// holder, flatten the concatenated roots into the `TypeDef` table, and run the
    /// completeness check ONCE over the combined set. The singular lowering products stay
    /// exposed for the Assembler; for a single unit they are that unit's.
    let combine (project: ProjectInfo) (units: UnitLayout list) : AssemblyLayout =
        // The entry unit carries the program entry point (`Main` + the anonymous "Program"
        // holder). For an executable it is the LAST file — F#'s rule that only the final
        // compilation unit may hold top-level expressions — and a library has none. Stamp
        // the flag onto exactly that unit (`buildUnit` left every unit FALSE, unaware of
        // the whole-assembly OutputKind decision) so `PrepareMain` fires once.
        let entryIndex =
            match project.OutputKind with
            | Exe -> List.length units - 1
            | Library -> -1

        let units =
            units
            |> List.mapi (fun i u ->
                { u with
                    EmitEntryPoint = (i = entryIndex)
                }
            )

        let entryUnit =
            match units |> List.tryFind (fun u -> u.EmitEntryPoint) with
            | Some u -> ValueSome u
            | None -> ValueNone

        // Only the entry file may carry top-level VALUE bindings — the anonymous "Program"
        // holder's static fields, written by its `.cctor` (leading prefix) or `Main`
        // (trailing). These come from a file's implicit-module top-level `let`s, which only
        // an executable's last file has; a non-entry unit with any is a front-end error. A
        // namespace-level `let` (a holder-less FN) is NOT top-level code — a library may
        // carry those on the Program holder — so it is aggregated below, not rejected here.
        units
        |> List.iteri (fun i u ->
            if not u.EmitEntryPoint then
                let p = u.Plan

                if
                    not (List.isEmpty p.ProgramCctorValues)
                    || not (List.isEmpty p.ProgramMainValues)
                then
                    failwithf
                        "Layout.combine: compilation unit %d of %d carries %d top-level value binding(s) but is not the entry file — only the last file of an executable may carry top-level code"
                        (i + 1)
                        (List.length units)
                        (List.length p.ProgramCctorValues + List.length p.ProgramMainValues)
        )

        // A holder `TypeSlotKey` contributed by two units is a same-FQN module split across
        // files — a front-end error the front end should already reject. Assert it here so a
        // duplicate holder TypeDef row can never reach `deriveHandles` (an opaque throw).
        let holderSeen = HashSet<TypeSlotKey>()

        for u in units do
            for k in u.BuiltKeys do
                match k with
                | TypeSlotKey.Holder _ ->
                    if not (holderSeen.Add k) then
                        failwithf
                            "Layout.combine: holder %A is contributed by more than one unit — a module's definition is split across files"
                            k
                | _ -> ()

        // The single `<Module>` pseudo-type is minted once here, not per unit, so it is
        // TypeDef row 1 for the whole assembly no matter how many units are combined.
        let moduleNode =
            {
                Slot =
                    {
                        Key = TypeSlotKey.ModulePseudo
                        Kind = TypeSlotKind.ModulePseudo
                        Namespace = ""
                        MetaName = "<Module>"
                        Typars = []
                    }
                Enclosing = ValueNone
                Fields = []
                Methods = []
                Nested = []
            }

        // The single Program holder, minted once here (not per unit). Its top-level value
        // FIELDS + `.cctor` + `Main` come from the ENTRY unit alone (only the last file of
        // an executable has top-level value bindings / `Main`; a library has neither):
        // leading-prefix values are `initonly` (written by the `.cctor`), values after a
        // top-level `do` are plain mutable `static` (written by `Main`). Its holder-less
        // static FNS aggregate across EVERY unit — a namespace-level `let` in any file lands
        // here — in unit order, each prepared by its owning unit's `MethodPlan`.
        //
        // `Main` belongs to this node's method list, which is what puts it inside the
        // Program type's `MethodList` range: the row and the range that claims it are
        // now the same list, so no ordering convention is left to preserve.
        let programFields =
            match entryUnit with
            | ValueNone -> []
            | ValueSome u ->
                let plan = u.Plan

                [
                    for mv in plan.ProgramCctorValues ->
                        {
                            Key = FieldKey.ModuleValue mv.SymbolKey
                            Name = mv.Name
                            Attrs = FieldAttributes.Public ||| FieldAttributes.Static ||| FieldAttributes.InitOnly
                            Ty = mv.Ty
                            ClosureScope = ValueNone
                        }
                    for mv in plan.ProgramMainValues ->
                        {
                            Key = FieldKey.ModuleValue mv.SymbolKey
                            Name = mv.Name
                            Attrs = FieldAttributes.Public ||| FieldAttributes.Static
                            Ty = mv.Ty
                            ClosureScope = ValueNone
                        }
                ]

        let hasProgramCctor =
            match entryUnit with
            | ValueSome u -> not (List.isEmpty u.Plan.ProgramCctorValues)
            | ValueNone -> false

        // Every unit's holder-less fns, in unit order, on the one Program holder.
        let holderlessFnRows =
            [
                for u in units do
                    for fn in u.Plan.HolderlessFns ->
                        {
                            Key = MethodKey.StaticFn fn.SymbolKey
                            Name = fn.Name
                            Attrs = staticMethodAttrs
                        }
            ]

        // The Program holder exists when there is any top-level code or namespace-level fn
        // to hold it: an entry point (`Main`), leading-prefix value fields, or any
        // holder-less fn across the units.
        let programNodes =
            if
                entryUnit.IsSome
                || not (List.isEmpty programFields)
                || not (List.isEmpty holderlessFnRows)
            then
                [
                    {
                        Slot =
                            {
                                Key = TypeSlotKey.Program
                                Kind = TypeSlotKind.Program hasProgramCctor
                                Namespace = ""
                                MetaName = project.ModuleName
                                Typars = []
                            }
                        Enclosing = ValueNone
                        Fields = programFields
                        Methods =
                            [
                                if hasProgramCctor then
                                    yield
                                        {
                                            Key = MethodKey.ProgramCctor
                                            Name = ".cctor"
                                            Attrs = cctorAttrs
                                        }

                                yield! holderlessFnRows

                                // `Main` is emitted iff there is an entry unit — only an
                                // executable has one, and it is what makes that unit the entry.
                                if entryUnit.IsSome then
                                    yield
                                        {
                                            Key = MethodKey.Main
                                            Name = "Main"
                                            Attrs = staticMethodAttrs
                                        }
                            ]
                        Nested = []
                    }
                ]
            else
                []

        // The roots, by kind: `<Module>` first (it must be TypeDef row 1), then the units'
        // namespace-level types / closures / root holders (each carrying its own subtree),
        // and the Program holder last.
        let roots = moduleNode :: (units |> List.collect (fun u -> u.Roots)) @ programNodes

        // The `TypeDef` table: the pre-order flattening. Every table the writer walks is
        // a projection of it, so a type's row range and the rows in that range cannot
        // disagree — there is no second enumeration to fall out of step.
        let rec flatten (n: TypeNode) : TypeNode list = n :: List.collect flatten n.Nested

        let types = List.collect flatten roots

        // The ONE invariant the derivation cannot make true by construction:
        // COMPLETENESS. Every node built above must be placed in the tree exactly once —
        // none dropped (a holder whose discovery missed it), none duplicated (a nominal
        // landing in both the roots and a module's `Nested`). Both are set questions, so
        // ask them as such — once, over the whole assembly.
        let builtKeys =
            [
                yield moduleNode.Slot.Key
                for u in units do
                    yield! u.BuiltKeys
                for n in programNodes -> n.Slot.Key
            ]

        let placedKeys = types |> List.map (fun n -> n.Slot.Key)

        if
            List.length placedKeys <> List.length builtKeys
            || not (HashSet(placedKeys).SetEquals(HashSet builtKeys))
        then
            failwithf
                "Layout: the type hierarchy places %d slots but %d were built — a slot is dropped, duplicated or invented"
                (List.length placedKeys)
                (List.length builtKeys)

        {
            Types = types
            Fields = types |> List.collect (fun n -> n.Fields)
            Methods = types |> List.collect (fun n -> n.Methods)
            // Assembly-level: does any unit carry the entry point (the PE serialises with an
            // entry point) / define the `%A` structural-format interfaces (Core suppresses
            // the per-type `Format` row + body).
            EmitEntryPoint = entryUnit.IsSome
            DefinesStructuralFormatInterfaces = units |> List.exists (fun u -> u.DefinesStructuralFormatInterfaces)
            Units = units
        }

    /// Plan the whole assembly from every tast: build one unit per file and combine them.
    /// The ONE `ClosureNamer` is created here and threaded through every `buildUnit`, so
    /// closure TypeDef names stay unique assembly-wide across files. `combine` selects the
    /// entry unit, rejects top-level code outside it, and mints the shared `<Module>` /
    /// Program roots once. Single-unit output is byte-identical to the pre-split `build`.
    let buildMany (symbols: ICodegenSymbols) (project: ProjectInfo) (tasts: Frozen.TastFile list) : AssemblyLayout =
        let closureNamer = Emit.ClosureNamer()
        let units = tasts |> List.map (buildUnit closureNamer symbols project)
        combine project units

    /// Plan the whole assembly from one tast — `buildMany` over a singleton unit list.
    let build (symbols: ICodegenSymbols) (project: ProjectInfo) (tast: Frozen.TastFile) : AssemblyLayout =
        buildMany symbols project [ tast ]

    /// Derive every handle from the layout once: TypeDef handle = position in the
    /// pre-order flattening + 1; first-field / first-method handles by prefix-summing
    /// each node's OWN row lists — the very lists `AssemblyLayout.Fields` / `.Methods`
    /// are collected from, so the prediction and the rows are the same data (an empty
    /// range naturally points past the end of the previous owner's range).
    let deriveHandles (layout: AssemblyLayout) : LayoutHandles =
        let typeDefs = Dictionary<TypeSlotKey, TypeDefinitionHandle>()
        let firstFields = Dictionary<TypeSlotKey, FieldDefinitionHandle>()
        let firstMethods = Dictionary<TypeSlotKey, MethodDefinitionHandle>()
        let methodDefs = Dictionary<MethodKey, MethodDefinitionHandle>()
        let mutable fieldCursor = 0
        let mutable methodCursor = 0

        layout.Types
        |> List.iteri (fun i node ->
            typeDefs.Add(node.Slot.Key, MetadataTokens.TypeDefinitionHandle(i + 1))
            firstFields.Add(node.Slot.Key, MetadataTokens.FieldDefinitionHandle(fieldCursor + 1))
            firstMethods.Add(node.Slot.Key, MetadataTokens.MethodDefinitionHandle(methodCursor + 1))
            fieldCursor <- fieldCursor + List.length node.Fields
            methodCursor <- methodCursor + List.length node.Methods
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
