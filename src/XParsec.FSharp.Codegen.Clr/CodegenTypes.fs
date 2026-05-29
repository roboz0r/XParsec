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
type internal PartitionedTypeDecls =
    {
        Interfaces: (TTypeDecl * TAbstractMethod list) list
        Unions: (TTypeDecl * TUnionCase list * TTypeMember list) list
        Records: (TTypeDecl * TRecordField list * TTypeMember list) list
        Classes:
            (TTypeDecl *
            TRecordField list *
            TRecordField list *
            TTypeMember list *
            SemType voption *
            bool *
            TStaticLet list) list
    }

/// Per-arm payload for `emitNominalType`: the part that differs in
/// field/ctor/factory emission and in the `Self` kind the equality/comparison
/// support records carry. Everything downstream is shared.
[<RequireQualifiedAccess>]
type internal NominalEmissionInput =
    | Union of cases: TUnionCase list
    | Record of fields: TRecordField list
    /// `ctorParams` become backing fields; `baseType` defaults to `Object`
    /// (`ValueNone`); the `fields` slot is reserved for future mutable instance
    /// fields. `isSealed` reflects `[<Sealed>]`. Each `staticLets` entry becomes
    /// a private static field + an entry in the synthesised `.cctor`.
    | Class of
        fields: TRecordField list *
        ctorParams: TRecordField list *
        baseType: SemType voption *
        isSealed: bool *
        staticLets: TStaticLet list

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
