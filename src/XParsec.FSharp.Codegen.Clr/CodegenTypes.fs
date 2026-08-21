namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open XParsec.FSharp.SemanticAnalysis

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
                match CodegenSymbols.lookupTypeByKey symbols iface.Key with
                | ValueSome(ExternalTypeShape.IntrinsicInterface { Platform = platform }) ->
                    for slot in ofPlatformInterface platform -> iface, slot
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
    }

/// A partitioned record declaration: its `TTypeDecl`, fields, and members.
type internal RecordDecl =
    {
        Decl: TastAccessor.TypeDecl
        Fields: Frozen.TRecordField list
        Members: TastAccessor.TypeMember list
        /// User `interface … with member …` impls: each pair is an implemented interface
        /// type + its already-typed member bodies.
        Interfaces: (FrozenNominal * TastAccessor.TypeMember list) list
        /// `Struct` for a `[<Struct>]` record (`System.ValueType` base, sealed) or
        /// `RefType` otherwise. Records are never `RefStruct`.
        ValueKind: ClassValueKind
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
        BaseType: FrozenNominal voption
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
        BaseCtorCall: TastAccessor.BaseCtorCall voption
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
    | Union of cases: Frozen.TUnionCase list * interfaces: (FrozenNominal * TastAccessor.TypeMember list) list
    /// `isStruct` ⇒ a `[<Struct>]` value-type record: `System.ValueType` base, a
    /// base-chain-free `.ctor`, and value-type-shaped equality/comparison bodies.
    | Record of
        fields: Frozen.TRecordField list *
        interfaces: (FrozenNominal * TastAccessor.TypeMember list) list *
        isStruct: bool
    | Class of ClassDecl

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
    }

    member this.AssemblyName: string = this.Project.AssemblyName

    member this.OutputPath: string option = this.Project.OutputPath
