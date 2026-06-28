namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open XParsec.FSharp.SemanticAnalysis

/// One disjoint walk over `tast.Decls`: every `TDecl.Type` is routed to exactly
/// one list by its `TTypeKind`. Adding a new nominal kind is one field + one
/// `match` arm in `partitionTypeDecls`.
/// A partitioned union declaration: its `TTypeDecl`, cases, and members.
type internal UnionDecl =
    {
        Decl: Frozen.TTypeDecl
        Cases: Frozen.TUnionCase list
        Members: Frozen.TTypeMember list
        /// User `interface … with member …` impls (same shape as `ClassDecl.Interfaces`):
        /// each pair is an implemented interface type + its already-typed member bodies.
        Interfaces: (FrozenType * Frozen.TTypeMember list) list
    }

/// A partitioned record declaration: its `TTypeDecl`, fields, and members.
type internal RecordDecl =
    {
        Decl: Frozen.TTypeDecl
        Fields: Frozen.TRecordField list
        Members: Frozen.TTypeMember list
        /// User `interface … with member …` impls (same shape as `ClassDecl.Interfaces`):
        /// each pair is an implemented interface type + its already-typed member bodies.
        Interfaces: (FrozenType * Frozen.TTypeMember list) list
    }

/// A partitioned class declaration. `Fields` are the explicit `val [mutable] x: T`
/// instance fields; `CtorParams` become backing fields. `BaseType`/`BaseCtorCall`
/// carry the optional `inherit`. `ValueKind` flags a reference type, a
/// `[<Struct>]` value type, or a `[<IsByRefLike>]` byref-like value type.
type internal ClassDecl =
    {
        Decl: Frozen.TTypeDecl
        Fields: Frozen.TRecordField list
        CtorParams: Frozen.TRecordField list
        Members: Frozen.TTypeMember list
        BaseType: FrozenType voption
        Interfaces: (FrozenType * Frozen.TTypeMember list) list
        IsSealed: bool
        StaticLets: Frozen.TStaticLet list
        SecondaryCtors: Frozen.TSecondaryCtor list
        BaseCtorCall: Frozen.TBaseCtorCall voption
        ValueKind: ClassValueKind
        /// `false` for the `val`-field form (`type T = val …; new(…) = …`): the
        /// secondaries are the only ctors (no synthesised primary `.ctor`).
        HasPrimaryCtor: bool
    }

type internal PartitionedTypeDecls =
    {
        Interfaces: (Frozen.TTypeDecl * Frozen.TAbstractMethod list) list
        Unions: UnionDecl list
        Records: RecordDecl list
        Classes: ClassDecl list
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
    | Union of cases: Frozen.TUnionCase list * interfaces: (FrozenType * Frozen.TTypeMember list) list
    /// `interfaces` pairs each user-implemented `interface … with` type with its
    /// already-typed member bodies (same shape as the class / union arms): codegen
    /// emits one `InterfaceImpl` row per entry and one virtual `MethodDefinition`
    /// per member.
    | Record of fields: Frozen.TRecordField list * interfaces: (FrozenType * Frozen.TTypeMember list) list
    /// `ctorParams` become backing fields; `baseType` defaults to `Object`
    /// (`ValueNone`) — or `System.ValueType` when `isStruct`. `fields` are the
    /// explicit `val [mutable] x: T` instance fields (each a `FieldDefinition`).
    /// `isSealed` reflects `[<Sealed>]` (a struct is always sealed). Each
    /// `staticLets` entry becomes a private static field + an entry in the
    /// synthesised `.cctor`. `interfaces` pairs each implemented
    /// interface type with its already-typed member bodies: codegen emits one
    /// `InterfaceImpl` row per entry and one virtual `MethodDefinition` per member
    /// (implicit impl). `isStruct` ⇒ value-type emission.
    | Class of
        fields: Frozen.TRecordField list *
        ctorParams: Frozen.TRecordField list *
        baseType: FrozenType voption *
        isSealed: bool *
        staticLets: Frozen.TStaticLet list *
        secondaryCtors: Frozen.TSecondaryCtor list *
        baseCtorCall: Frozen.TBaseCtorCall voption *
        interfaces: (FrozenType * Frozen.TTypeMember list) list *
        isStruct: bool *
        // `false` for the `val`-field form (`type T = val …; new(…) = …`): the
        // secondaries are the only ctors, so `NominalEmit` skips the synthesised
        // primary `.ctor` (it would collide with a parameterless `new()`) and
        // `EmitConstruct.buildNew` resolves every construction to a secondary.
        hasPrimaryCtor: bool

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
