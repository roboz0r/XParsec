namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open XParsec.FSharp.SemanticAnalysis

/// One deferred `TypeDefinition` row (`unionTypes` / `recordTypes` /
/// `closureTypes`). Method/field rows are all added first, then the trailing
/// pass walks these. `Interfaces` carries pre-minted `InterfaceImpl` entity
/// handles — pre-minted because their `TypeSpec` encoding needs the
/// type-typars / closure-typars ambient that is only live during this type's
/// emit window.
type internal EmittedTypeRow =
    {
        Name: string
        Namespace: string
        Typars: string list
        FirstField: FieldDefinitionHandle
        FirstMethod: MethodDefinitionHandle
        Interfaces: EntityHandle list
        /// Unions / records / closures are always sealed (rung 2 forbids
        /// inheritance); classes opt in via `[<Sealed>]`.
        IsSealed: bool
        /// The IL `TypeDefinition.BaseType` handle. `System.Object` for unions /
        /// records / closures and for a parent-less class; a class with an
        /// `inherit` clause (B-4 Step 2.5) resolves its parent's `TypeSpec` here
        /// while the declaring-type typars are ambient. Pre-resolved during emit
        /// because a generic parent's encoding needs that ambient context.
        /// A `[<Struct>]` type resolves it to `System.ValueType`.
        BaseType: EntityHandle
        /// `true` for a `[<Struct>]` value type (vesper-set-sprint-phase-6):
        /// `classAttrsOf` then flips `SequentialLayout` + `Sealed` instead of
        /// `AutoLayout`, and the base type is `System.ValueType`.
        IsValueType: bool
    }

/// Drives the offset arithmetic in `predictTypeDef`; the order here matches the
/// trailing TypeDefinition emission order (interfaces → unions → records →
/// classes → closures → holders).
[<RequireQualifiedAccess>]
type internal NominalKind =
    | Interface
    | Union
    | Record
    | Class
    | Closure

/// Count of `TypeDefinition`s that will land in the trailing emission loop,
/// grouped by `NominalKind`. Consulted by `predictTypeDef` at every
/// forward-handle site.
type internal TypeDefCounts =
    {
        Interfaces: int
        Unions: int
        Records: int
        Classes: int
    }

/// One disjoint walk over `tast.Decls`: every `TDecl.Type` is routed to exactly
/// one list by its `TTypeKind`. Adding a new nominal kind is one field + one
/// `match` arm in `partitionTypeDecls`.
/// A partitioned union declaration: its `TTypeDecl`, cases, and members.
type internal UnionDecl =
    {
        Decl: Frozen.TTypeDecl
        Cases: Frozen.TUnionCase list
        Members: Frozen.TTypeMember list
    }

/// A partitioned record declaration: its `TTypeDecl`, fields, and members.
type internal RecordDecl =
    {
        Decl: Frozen.TTypeDecl
        Fields: Frozen.TRecordField list
        Members: Frozen.TTypeMember list
    }

/// A partitioned class declaration. `Fields` are the explicit `val [mutable] x: T`
/// instance fields; `CtorParams` become backing fields. `BaseType`/`BaseCtorCall`
/// carry the optional `inherit`. `IsStruct` flags a `[<Struct>]` value type.
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
        IsStruct: bool
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
    | Union of cases: Frozen.TUnionCase list
    | Record of fields: Frozen.TRecordField list
    /// `ctorParams` become backing fields; `baseType` defaults to `Object`
    /// (`ValueNone`) — or `System.ValueType` when `isStruct`. `fields` are the
    /// explicit `val [mutable] x: T` instance fields (each a `FieldDefinition`).
    /// `isSealed` reflects `[<Sealed>]` (a struct is always sealed). Each
    /// `staticLets` entry becomes a private static field + an entry in the
    /// synthesised `.cctor`. `interfaces` (B-2, §5.3) pairs each implemented
    /// interface type with its already-typed member bodies: codegen emits one
    /// `InterfaceImpl` row per entry and one virtual `MethodDefinition` per member
    /// (implicit impl). `isStruct` ⇒ value-type emission (vesper-set-sprint-phase-6).
    | Class of
        fields: Frozen.TRecordField list *
        ctorParams: Frozen.TRecordField list *
        baseType: FrozenType voption *
        isSealed: bool *
        staticLets: Frozen.TStaticLet list *
        secondaryCtors: Frozen.TSecondaryCtor list *
        baseCtorCall: Frozen.TBaseCtorCall voption *
        interfaces: (FrozenType * Frozen.TTypeMember list) list *
        isStruct: bool

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
