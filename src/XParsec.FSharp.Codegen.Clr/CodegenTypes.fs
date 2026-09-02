namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common

/// A BCL method no author wrote: `interface seq<'T>` declares only `GetEnumerator`, but the
/// inherited non-generic `IEnumerable` / `IEnumerator` slots must be implemented too or the
/// type raises `TypeLoadException`. The generic slots bind to the authored members by name.
[<RequireQualifiedAccess>]
type internal CoSlot =
    /// `IEnumerable.GetEnumerator() : IEnumerator` — forwards to the capability's
    /// `GetEnumerator`, whose `IEnumerator`1<T>` return already IS an `IEnumerator`.
    | EnumerableGetEnumerator
    /// `IEnumerator.get_Current() : object` — forwards to the capability's `Current`,
    /// boxing the `'T`.
    | EnumeratorCurrent
    /// `IEnumerator.Reset() : void` — the shim throws `NotSupportedException`, since the
    /// pull protocol has no rewind and so no capability member to forward to.
    | EnumeratorReset

/// Which co-slots a nominal must synthesise, keyed off each implemented interface's
/// resolved `IntrinsicInterface` shape rather than its source name.
module internal CapabilityCoSlots =

    /// The only BCL interfaces with inherited members: every other capability
    /// (`System.IDisposable`, `IEquatable`1`, `IComparable`1`) is single-method, no bases.
    let private ofPlatformInterface (platform: string) : CoSlot list =
        match platform with
        | "System.Collections.Generic.IEnumerable`1" -> [ CoSlot.EnumerableGetEnumerator ]
        | "System.Collections.Generic.IEnumerator`1" -> [ CoSlot.EnumeratorCurrent; CoSlot.EnumeratorReset ]
        | _ -> []

    /// The co-slots the implemented `interfaces` require, in emission order, each paired
    /// with the interface that demands it, so a shim's forwarding target is looked up in
    /// THAT interface's impl block, not by name across every impl member.
    let required (symbols: ICodegenSymbols) (interfaces: FrozenNominal list) : (FrozenNominal * CoSlot) list =
        [
            for iface in interfaces do
                match symbols.TryLookupType iface.Key with
                | ValueSome(ExternalTypeShape.IntrinsicInterface { Platform = platform }) ->
                    for slot in ofPlatformInterface platform.Value -> iface, slot
                | _ -> ()
        ]

    /// The emitted method name of a co-slot — the BCL slot it implicitly binds to.
    let metaName (slot: CoSlot) : string =
        match slot with
        | CoSlot.EnumerableGetEnumerator -> "GetEnumerator"
        | CoSlot.EnumeratorCurrent -> "get_Current"
        | CoSlot.EnumeratorReset -> "Reset"

    /// The capability member a co-slot forwards to, by its SOURCE name (the member the
    /// author wrote in the `interface <capability> with` block). `ValueNone` for `Reset`,
    /// which throws instead of forwarding.
    let forwardsTo (slot: CoSlot) : string voption =
        match slot with
        | CoSlot.EnumerableGetEnumerator -> ValueSome "GetEnumerator"
        | CoSlot.EnumeratorCurrent -> ValueSome "Current"
        | CoSlot.EnumeratorReset -> ValueNone

/// A partitioned union declaration: its `TTypeDecl`, cases, and members.
type internal UnionDecl =
    {
        Decl: TastAccessor.TypeDecl
        Cases: Frozen.TUnionCase list
        Members: TastAccessor.TypeMember list
        /// User `interface … with member …` impls: each pair is an implemented interface
        /// type + its already-typed member bodies.
        Interfaces: (FrozenNominal * TastAccessor.TypeMember list) list
        /// `Struct` for a `[<Struct>]` union (`System.ValueType` base, sealed,
        /// `IsReadOnly` with `initonly` fields; the factories `newobj` the flat `.ctor` and
        /// return by value).
        ValueKind: UnionValueKind
        /// The metadata shape this union is emitted in, classified once at partition time.
        Regime: UnionRegime
        /// The physical slots, where they are declared, each case field's read path and
        /// the public `Get_<Case>_<i>` readers. `ValueSome` exactly where the regime is
        /// flat.
        Placements: FlatUnionPlacements voption
    }

    /// A nested `TypeDef` per case on an abstract base.
    member this.IsHierarchy: bool = UnionRegime.isHierarchy this.Regime

    /// A `_tag : int32` discriminant on the union type.
    member this.HasTag: bool = UnionRegime.hasTag this.Regime

    /// The parameter list this union's own `.ctor` declares.
    member this.CtorShape: UnionCtorShape = UnionCtorShape.ofRegime this.Regime

    /// The public `Get_<Case>_<i>` readers; empty for a hierarchy regime.
    member this.CaseGetters: UnionCaseGetter list =
        match this.Placements with
        | ValueSome p -> p.Getters
        | ValueNone -> []

    /// The `(tag, case)` pairs held as a `_unique_<Case>` singleton, constructed once by
    /// the union's `.cctor`. Every nullary case of a reference union qualifies, in any
    /// regime; a `[<Struct>]` union yields none. Empty ⇒ no `.cctor` row.
    member this.SingletonCases: (int * Frozen.TUnionCase) list =
        match this.ValueKind with
        | UnionValueKind.Struct -> []
        | UnionValueKind.RefType -> this.Cases |> List.indexed |> List.filter (fun (_, c) -> c.Fields.IsEmpty)

    /// One case's payload field names, in declaration order.
    member this.FieldNames(c: Frozen.TUnionCase) : string list =
        UnionCaseFields.names [ for (n, _) in c.Fields -> n ]

/// A partitioned record declaration: its `TTypeDecl`, fields, and members.
type internal RecordDecl =
    {
        Decl: TastAccessor.TypeDecl
        Fields: Frozen.TRecordField list
        Members: TastAccessor.TypeMember list
        /// User `interface … with member …` impls: each pair is an implemented interface
        /// type + its already-typed member bodies.
        Interfaces: (FrozenNominal * TastAccessor.TypeMember list) list
        /// `Struct` for a `[<Struct>]` record (`System.ValueType` base, sealed).
        ValueKind: RecordValueKind
    }

/// A partitioned class declaration. `Fields` are the explicit `val [mutable] x: T`
/// instance fields; `CtorParams` are the primary constructor's parameters, which also
/// become backing fields.
type internal ClassDecl =
    {
        Decl: TastAccessor.TypeDecl
        Fields: Frozen.TRecordField list
        CtorParams: Frozen.TRecordField list
        Members: TastAccessor.TypeMember list
        Base: TastAccessor.Base voption
        Interfaces: (FrozenNominal * TastAccessor.TypeMember list) list
        IsSealed: bool
        /// `static let` / `static do` in declaration order: the body of the synthesised
        /// `.cctor`. A `let` also takes a static backing field.
        StaticPreamble: TastAccessor.PreambleEntry list
        /// Instance `let` / `do` in declaration order: the END of the primary `.ctor`,
        /// after the base-ctor call and the ctor-param field stores (so an initialiser
        /// reads a ctor param through its already-stored field).
        InstancePreamble: TastAccessor.PreambleEntry list
        /// The class-level `this` bound variable. The instance preamble reads fields through it
        /// (`FieldGet(Var ThisKey, …)`), so the primary `.ctor` maps it to `ldarg.0`.
        ThisKey: BoundVarKeyG<BoundVarId>
        SecondaryCtors: TastAccessor.SecondaryCtor list
        ValueKind: ClassValueKind
        /// `false` for the `val`-field form (`type T = val …; new(…) = …`): the
        /// secondaries are the only ctors (no synthesised primary `.ctor`).
        HasPrimaryCtor: bool
    }

/// A partitioned NUMERIC enum — all-integer cases only (string/mixed land in
/// `StructEnumDecl`). `Underlying` is the integral primitive identity (`int` / `byte` /
/// `int64`) the `value__` field takes; `Cases` is `(name, literal)` in declaration order.
type internal EnumDecl =
    {
        Decl: TastAccessor.TypeDecl
        Underlying: TypeKey
        Cases: (string * TConstValue) list
    }

/// A partitioned STRING or MIXED enum, emitted as a `[<Struct>]` wrapper over one field
/// (`string`, or `obj` when `IsMixed`, a boxed int or string per case) plus one
/// `.cctor`-initialised `static initonly` field per case, in `Cases` declaration order.
type internal StructEnumDecl =
    {
        Decl: TastAccessor.TypeDecl
        IsMixed: bool
        Cases: (string * TEnumLiteral) list
    }

/// One disjoint walk over the file's decls: every type declaration is routed to exactly one
/// of these lists by its kind.
type internal PartitionedTypeDecls =
    {
        Interfaces: (TastAccessor.TypeDecl * Frozen.TAbstractMethod list) list
        Unions: UnionDecl list
        Records: RecordDecl list
        Classes: ClassDecl list
        Enums: EnumDecl list
        StructEnums: StructEnumDecl list
    }

/// The per-kind part of a nominal emission: what differs in field / ctor / factory
/// emission. Every arm's `interfaces` yields one `InterfaceImpl` row per entry and one
/// virtual `MethodDefinition` per member.
[<RequireQualifiedAccess>]
type internal NominalEmissionInput =
    | Union of UnionDecl
    | Record of RecordDecl
    | Class of ClassDecl

    /// User `interface … with member …` impls: each pair is an implemented interface
    /// type + its already-typed member bodies.
    member this.Interfaces: (FrozenNominal * TastAccessor.TypeMember list) list =
        match this with
        | NominalEmissionInput.Union ud -> ud.Interfaces
        | NominalEmissionInput.Record rd -> rd.Interfaces
        | NominalEmissionInput.Class cd -> cd.Interfaces

    /// A `[<Struct>]` value type: `System.ValueType` base, `IsReadOnly`, and
    /// value-type-shaped equality/comparison bodies.
    member this.IsValueType: bool =
        match this with
        | NominalEmissionInput.Union ud -> ud.ValueKind.IsValueType
        | NominalEmissionInput.Record rd -> rd.ValueKind.IsValueType
        | NominalEmissionInput.Class cd -> cd.ValueKind.IsValueType

/// Shared contract over a nominal type's own method members and its `interface … with` impls.
[<RequireQualifiedAccess>]
module internal NominalMembers =

    /// Whether the impl blocks include `Vesper.IStructuralFormattable`: the type supplies its
    /// own `%A` body, so no `Format` row, IL or `InterfaceImpl` is synthesised for it.
    let declaresStructuralFormat (interfaces: (FrozenNominal * TastAccessor.TypeMember list) list) : bool =
        interfaces
        |> List.exists (fun (iface, _) -> iface.Args.IsEmpty && iface.Key = RuntimeNames.structuralFormattableKey)

    /// The grouped `interface … with` impls as one member sequence, in declaration order,
    /// which is the order `indexed` numbers impl members in.
    let flattenIfaceMembers
        (interfaces: (FrozenNominal * TastAccessor.TypeMember list) list)
        : TastAccessor.TypeMember list =
        [
            for (_, ms) in interfaces do
                yield! ms
        ]

    /// The `(index, isIfaceImpl, member)` sequence for a nominal's method rows: its own
    /// augmentation members at `[0..n)`, then its flattened impl members at `[n..)`. This
    /// is the `MethodKey.Member` index contract row declaration and body emission share.
    let indexed
        (members: TastAccessor.TypeMember list)
        (interfaces: (FrozenNominal * TastAccessor.TypeMember list) list)
        : (int * bool * TastAccessor.TypeMember) list =
        [
            yield! members |> List.mapi (fun i m -> i, false, m)

            let ownCount = List.length members
            yield! flattenIfaceMembers interfaces |> List.mapi (fun i m -> ownCount + i, true, m)
        ]

    /// The `(index, member)` pairs of ONE interface's impl block, in `indexed`'s index
    /// space, for a consumer that knows which interface it needs and must not match a
    /// same-named member declared by another interface.
    let ofInterface
        (members: TastAccessor.TypeMember list)
        (interfaces: (FrozenNominal * TastAccessor.TypeMember list) list)
        (iface: FrozenNominal)
        : (int * TastAccessor.TypeMember) list =
        // `indexed`'s walk, carrying each block's start index instead of discarding it.
        let rec go (index: int) (rest: (FrozenNominal * TastAccessor.TypeMember list) list) =
            match rest with
            | [] -> []
            | (ifaceTy, ms) :: tail ->
                let next = index + List.length ms

                let here =
                    if ifaceTy = iface then
                        ms |> List.mapi (fun i m -> index + i, m)
                    else
                        []

                here @ go next tail

        go (List.length members) interfaces

/// Which synthesised structural members a nominal emits.
type internal StructuralMembers =
    {
        /// `GetHashCode`, `Equals(object)` and the typed `Equals(Self)`, plus the
        /// `IEquatable<Self>` row.
        Equality: bool
        /// The typed `CompareTo(Self)` and `CompareTo(object)`, plus the
        /// `IComparable<Self>` and `IComparable` rows.
        Comparison: bool
        /// `IStructuralFormattable.Format` (`%A`).
        Format: bool
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module internal StructuralMembers =

    /// The verdict of a class, which synthesises none of them.
    let none =
        {
            Equality = false
            Comparison = false
            Format = false
        }

    let private ofDataShape
        (td: TastAccessor.TypeDecl)
        (interfaces: (FrozenNominal * TastAccessor.TypeMember list) list)
        : StructuralMembers =
        {
            Equality = td.EqualitySupport = EqualityVerdict.Structural
            Comparison = td.ComparisonSupport = ComparisonVerdict.Structural
            // `%A` renders a value's structure, so it follows neither verdict; a source
            // that implements `IStructuralFormattable` itself supplies the body instead.
            Format = not (NominalMembers.declaresStructuralFormat interfaces)
        }

    let ofUnion (ud: UnionDecl) : StructuralMembers = ofDataShape ud.Decl ud.Interfaces

    let ofRecord (rd: RecordDecl) : StructuralMembers = ofDataShape rd.Decl rd.Interfaces

    let ofInput (input: NominalEmissionInput) : StructuralMembers =
        match input with
        | NominalEmissionInput.Union ud -> ofUnion ud
        | NominalEmissionInput.Record rd -> ofRecord rd
        | NominalEmissionInput.Class _ -> none

/// Why a frozen attribute emitted no `CustomAttribute` row.
[<RequireQualifiedAccess>]
type SkippedAttributeRowReason =
    /// The local attribute class declares no ctor taking this many positional arguments.
    | NoMatchingCtor of positionalArgCount: int
    /// Two or more local ctors take this many positional arguments; overload resolution is
    /// not performed at row emission.
    | AmbiguousCtor of positionalArgCount: int
    /// A generic attribute class has no encodable ctor parent.
    | GenericAttributeClass
    /// No referenced-assembly ctor with this many positional arguments resolved.
    | NoExternalCtor of positionalArgCount: int
    /// An argument value is outside the II.23.3 encodable constant domain.
    | UnencodableArgument
    /// A named argument typed by a referenced-assembly enum, whose II.23.3 SerString would
    /// need an assembly-qualified name.
    | ForeignEnumArgument of enumKey: TypeKey

/// One skipped row: the attribute, the declaration element it was written on, and why.
/// Rows are advisory because the `.fsi` contract is the Vesper→Vesper carrier, so a skip
/// does not fail the compile; it is filed on the artifact for diagnosis.
type SkippedAttributeRow =
    {
        AttributeKey: TypeKey
        /// The parent element as `Type` / `Type.Member` / `Type.Field`.
        Parent: string
        Reason: SkippedAttributeRowReason
    }

/// The in-memory assembled PE plus enough to inspect / write it.
type ClrArtifact =
    {
        /// The project this was emitted for: where it writes, the TFM its
        /// `runtimeconfig.json` names, and the paths a shipped reference is copied from.
        Project: ProjectInfo
        /// The serialised PE image.
        Pe: BlobBuilder
        /// Simple names of every assembly the emitted PE binds against (its `AssemblyRef`
        /// table), which seed the ship set when materialising a runnable app.
        ReferencedAssemblies: string list
        /// Frozen attributes that emitted no `CustomAttribute` row.
        SkippedAttributeRows: SkippedAttributeRow list
    }

    member this.AssemblyName: string = this.Project.AssemblyName

    member this.OutputPath: string option = this.Project.OutputPath
