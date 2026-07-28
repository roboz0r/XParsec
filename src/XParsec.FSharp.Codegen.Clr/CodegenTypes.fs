namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open XParsec.FSharp.SemanticAnalysis

/// A method the CLR demands but the *capability* contract never declared, so no author
/// ever wrote it. A capability's platform face drags in a wider BCL interface hierarchy
/// than the capability's own member surface: `interface seq<'T>` declares only
/// `GetEnumerator`, but its face `IEnumerable`1` inherits the non-generic `IEnumerable`,
/// and `enumerator`'s face `IEnumerator`1` inherits `IEnumerator`'s `object Current` and
/// `Reset`. The CLR requires EVERY method in a declared interface's transitive closure to
/// be implemented, so the backend synthesises these as forwarding shims — without them the
/// type does not load (`TypeLoadException: … does not have an implementation`), and a C#
/// consumer could not iterate a Vesper type at all.
///
/// This is the concrete meaning of "each backend lowers the abstract protocol to its
/// platform idiom". The BCL knowledge lives HERE, in the CLR backend (as it already does
/// in `EmitLoops`' `for … in` slots), never in the platform-agnostic capability contract.
/// The *generic* face slots need no synthesis: the authored members already bind to them
/// implicitly by name + signature (which is also why `MoveNext`, whose signature is
/// identical on the non-generic `IEnumerator`, needs no shim).
[<RequireQualifiedAccess>]
type internal CoSlot =
    /// `IEnumerable.GetEnumerator() : IEnumerator` — forwards to the capability's
    /// `GetEnumerator`, whose `IEnumerator`1<T>` return already IS an `IEnumerator`.
    | EnumerableGetEnumerator
    /// `IEnumerator.get_Current() : object` — forwards to the capability's `Current`,
    /// boxing the `'T`.
    | EnumeratorCurrent
    /// `IEnumerator.Reset() : void` — the pull protocol has no rewind, so there is no
    /// capability member to forward to. Throws `NotSupportedException`, exactly as a
    /// non-resettable BCL enumerator does.
    | EnumeratorReset

/// Which co-slots a nominal must synthesise, derived from the capability interfaces it
/// implements. A capability is recognised STRUCTURALLY — an `IntrinsicInterface` shape and
/// the platform face it reconciles to — never by a canonical `Vesper.Collections.seq`
/// string literal, so this stays keyed off resolution rather than a hardcoded contract name.
module internal CapabilityCoSlots =

    /// The BCL faces whose inherited members outrun their capability's member surface.
    /// Every other capability face (`System.IDisposable`, `IEquatable`1`, `IComparable`1`)
    /// is a single-method interface with no bases — hence no co-slots, and hence why this
    /// synthesis is new with the iteration cluster.
    let private ofPlatformFace (platform: string) : CoSlot list =
        match platform with
        | "System.Collections.Generic.IEnumerable`1" -> [ CoSlot.EnumerableGetEnumerator ]
        | "System.Collections.Generic.IEnumerator`1" -> [ CoSlot.EnumeratorCurrent; CoSlot.EnumeratorReset ]
        | _ -> []

    /// The co-slots the implemented `interfaces` require, in emission order, each paired
    /// with the capability interface that demands it — so the shim's forwarding target is
    /// looked up in THAT interface's impl block (`NominalMembers.ofInterface`) rather than
    /// re-guessed by name across every impl member. A pure function of the interface list:
    /// `Layout` (row reservation) and `NominalEmit` (body preparation) each call it on the
    /// same interfaces and cannot disagree.
    let required (symbols: ICodegenSymbols) (interfaces: FrozenType list) : (FrozenType * CoSlot) list =
        [
            for iface in interfaces do
                match iface with
                | FTClass(key, _) ->
                    match CodegenSymbols.lookupTypeByKey symbols (SymbolKey.Type key) with
                    | ValueSome(ExternalTypeShape.IntrinsicInterface { Platform = platform }) ->
                        for slot in ofPlatformFace platform -> iface, slot
                    | _ -> ()
                | _ -> ()
        ]

    /// The emitted method name of a co-slot — the BCL slot it implicitly binds to.
    let metaName (slot: CoSlot) : string =
        match slot with
        | CoSlot.EnumerableGetEnumerator -> "GetEnumerator"
        | CoSlot.EnumeratorCurrent -> "get_Current"
        | CoSlot.EnumeratorReset -> "Reset"

    /// The capability member a co-slot forwards to, by its SOURCE name (the member the
    /// author wrote in the `interface <capability> with` block). `ValueNone` for `Reset`:
    /// the pull protocol has no rewind, so that shim throws instead of forwarding.
    let forwardsTo (slot: CoSlot) : string voption =
        match slot with
        | CoSlot.EnumerableGetEnumerator -> ValueSome "GetEnumerator"
        | CoSlot.EnumeratorCurrent -> ValueSome "Current"
        | CoSlot.EnumeratorReset -> ValueNone

/// One disjoint walk over `tast.Decls`: every `TDecl.Type` is routed to exactly
/// one list by its `TTypeKind`. Adding a new nominal kind is one field + one
/// `match` arm in `partitionTypeDecls`.
/// A partitioned union declaration: its `TTypeDecl`, cases, and members.
type internal UnionDecl =
    {
        Decl: TastAccessor.TypeDecl
        Cases: Frozen.TUnionCase list
        Members: TastAccessor.TypeMember list
        /// User `interface … with member …` impls (same shape as `ClassDecl.Interfaces`):
        /// each pair is an implemented interface type + its already-typed member bodies.
        Interfaces: (FrozenType * TastAccessor.TypeMember list) list
    }

/// A partitioned record declaration: its `TTypeDecl`, fields, and members.
type internal RecordDecl =
    {
        Decl: TastAccessor.TypeDecl
        Fields: Frozen.TRecordField list
        Members: TastAccessor.TypeMember list
        /// User `interface … with member …` impls (same shape as `ClassDecl.Interfaces`):
        /// each pair is an implemented interface type + its already-typed member bodies.
        Interfaces: (FrozenType * TastAccessor.TypeMember list) list
        /// `Struct` for a `[<Struct>]` record (`System.ValueType` base, sealed) or
        /// `RefType` otherwise. Records are never `RefStruct`.
        ValueKind: ClassValueKind
    }

/// A partitioned class declaration. `Fields` are the explicit `val [mutable] x: T`
/// instance fields; `CtorParams` become backing fields. `BaseType`/`BaseCtorCall`
/// carry the optional `inherit`. `ValueKind` flags a reference type, a
/// `[<Struct>]` value type, or a `[<IsByRefLike>]` byref-like value type.
type internal ClassDecl =
    {
        Decl: TastAccessor.TypeDecl
        Fields: Frozen.TRecordField list
        CtorParams: Frozen.TRecordField list
        Members: TastAccessor.TypeMember list
        BaseType: FrozenType voption
        Interfaces: (FrozenType * TastAccessor.TypeMember list) list
        IsSealed: bool
        /// `static let` / `static do` in declaration order: the body of the synthesised
        /// `.cctor`. A `let` also takes a static backing field.
        StaticPreamble: TastAccessor.PreambleEntry list
        /// Instance `let` / `do` in declaration order: the tail of the primary `.ctor`,
        /// after the base-ctor call and the ctor-param field stores (so an initialiser
        /// reads a ctor param through its already-stored field). A `let` also takes an
        /// instance backing field.
        InstancePreamble: TastAccessor.PreambleEntry list
        /// The class-level `this` binder. The instance preamble reads the class's fields
        /// through it (`FieldGet(Var ThisKey, …)`), so the primary `.ctor` maps it to
        /// `ldarg.0`.
        ThisKey: BinderKey
        SecondaryCtors: TastAccessor.SecondaryCtor list
        BaseCtorCall: TastAccessor.BaseCtorCall voption
        ValueKind: ClassValueKind
        /// `false` for the `val`-field form (`type T = val …; new(…) = …`): the
        /// secondaries are the only ctors (no synthesised primary `.ctor`).
        HasPrimaryCtor: bool
    }

/// A partitioned NUMERIC enum declaration. Only all-integer enums land
/// here — `partitionTypeDecls` classifies via `TEnumCases.classify` and drops
/// string/mixed enums and any all-illegal enum. `Underlying` is the
/// derived integral primitive NAME (`int`/`byte`/`uint32`/`int64`,
/// `TEnumCases.underlyingTypeName`) the `value__` field is typed by; `Cases` is the
/// resolved `(caseName, underlying-int literal)` table in declaration order (only
/// cases whose literal resolved — a `ValueNone` errored case is skipped, matching the
/// emitted literal-field set).
type internal EnumDecl =
    {
        Decl: TastAccessor.TypeDecl
        Underlying: string
        Cases: (string * TConstValue) list
    }

/// A partitioned STRING or MIXED enum declaration. Emitted as a
/// `[<Struct>]` value type wrapping a single field — `string` for a pure-string
/// enum, `obj` (boxed int / string per case) for a mixed one — with one
/// `public static initonly` field of the enum type per case, `.cctor`-initialised
/// by constructing the wrapper from the case's literal, and equality on that
/// field. `IsMixed` selects the `obj` field + boxed-literal construction (vs the
/// `string` field + `ldstr`). `Cases` is the resolved `(caseName, literal)` table
/// in declaration order (only cases whose literal resolved — a `ValueNone` errored
/// case is skipped, matching the emitted case-field set).
type internal StructEnumDecl =
    {
        Decl: TastAccessor.TypeDecl
        IsMixed: bool
        Cases: (string * TEnumLiteral) list
    }

type internal PartitionedTypeDecls =
    {
        Interfaces: (TastAccessor.TypeDecl * Frozen.TAbstractMethod list) list
        Unions: UnionDecl list
        Records: RecordDecl list
        Classes: ClassDecl list
        Enums: EnumDecl list
        StructEnums: StructEnumDecl list
    }

/// Per-arm payload for `emitNominalType`: the part that differs in
/// field/ctor/factory emission and in the `Self` kind the equality/comparison
/// support records carry. Everything downstream is shared.
[<RequireQualifiedAccess>]
type internal NominalEmissionInput =
    /// `interfaces` pairs each user-implemented `interface … with` type with its
    /// already-typed member bodies (same shape as the class arm): codegen emits one
    /// `InterfaceImpl` row per entry and one virtual `MethodDefinition` per member,
    /// alongside any synthesised structural eq/comp/format interfaces.
    | Union of cases: Frozen.TUnionCase list * interfaces: (FrozenType * TastAccessor.TypeMember list) list
    /// `interfaces` pairs each user-implemented `interface … with` type with its
    /// already-typed member bodies (same shape as the class / union arms): codegen
    /// emits one `InterfaceImpl` row per entry and one virtual `MethodDefinition`
    /// per member.
    /// `isStruct` ⇒ a `[<Struct>]` value-type record: `System.ValueType` base,
    /// a base-chain-free `.ctor`, and value-type-shaped equality/comparison bodies.
    | Record of
        fields: Frozen.TRecordField list *
        interfaces: (FrozenType * TastAccessor.TypeMember list) list *
        isStruct: bool
    /// The partitioned declaration itself: the class arm needs so much of it
    /// (fields, ctor params, base, preambles, secondaries, value kind) that a
    /// re-projection would only be able to drift from it.
    | Class of ClassDecl

/// Shared index contract over a nominal type's method members.
[<RequireQualifiedAccess>]
module internal NominalMembers =

    /// Flatten a nominal's grouped `interface … with` impls (interface type + its member
    /// bodies) to the impl-member sequence in declaration order. The SINGLE place the
    /// load-bearing flatten order lives, so `indexed`, `Layout`, and `NominalEmit` can't
    /// drift on it (they previously each re-flattened the grouped list independently).
    let flattenIfaceMembers
        (interfaces: (FrozenType * TastAccessor.TypeMember list) list)
        : TastAccessor.TypeMember list =
        [
            for (_, ms) in interfaces do
                yield! ms
        ]

    /// The `(index, isIfaceImpl, member)` sequence for a nominal type's method rows:
    /// its own augmentation members at indices `[0..n)`, then its flattened
    /// `interface … with` impl members at `[n..)`. The SINGLE definition of the
    /// own-before-iface `MethodKey.Member` index contract that both `Layout` (row
    /// declaration) and `NominalEmit` (body emission) consume — so the two files can't
    /// drift on the arithmetic. Takes the GROUPED interfaces and flattens internally
    /// (`flattenIfaceMembers`), colocating the flatten order with the index order.
    /// Eq/comp/format rows use disjoint `MethodKey`s, so they never collide with these
    /// and their relative order is irrelevant.
    let indexed
        (members: TastAccessor.TypeMember list)
        (interfaces: (FrozenType * TastAccessor.TypeMember list) list)
        : (int * bool * TastAccessor.TypeMember) list =
        [
            yield! members |> List.mapi (fun i m -> i, false, m)

            let ownCount = List.length members
            yield! flattenIfaceMembers interfaces |> List.mapi (fun i m -> ownCount + i, true, m)
        ]

    /// The `(index, member)` pairs of ONE interface's impl block, in the same index space
    /// `indexed` defines. The lookup for a consumer that already knows WHICH interface it
    /// needs (a capability co-slot shim forwards to a member of the capability it was
    /// derived from), so it never has to scan every impl member by name and hope no other
    /// interface declares that name too.
    let ofInterface
        (members: TastAccessor.TypeMember list)
        (interfaces: (FrozenType * TastAccessor.TypeMember list) list)
        (iface: FrozenType)
        : (int * TastAccessor.TypeMember) list =
        // The own-then-impl-blocks-in-order walk `indexed` defines, carrying each block's
        // start index instead of discarding it.
        let rec go (index: int) (rest: (FrozenType * TastAccessor.TypeMember list) list) =
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
        AssemblyName: string
        OutputPath: string option
        /// The serialised PE image.
        Pe: BlobBuilder
        /// Simple names of every assembly the emitted PE binds against (its
        /// `AssemblyRef` table). Drives `materialiseApp`'s copy.
        ReferencedAssemblies: string list
        /// The distinct FSharp.Core constructs the emission referenced. **Empty
        /// ⇒ the PE has no `FSharp.Core.dll` dependency.**
        FSharpCoreDependencies: string list
    }
