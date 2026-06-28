/// Version 0 of the serialised TS-extraction manifest — the neutral IR that the
/// F#/Fable extractor PRODUCES and the F# `TsManifestProvider` CONSUMES. Shared
/// so producer and consumer cannot drift: Fable-compiled to JS for the
/// extractor, used natively on .NET by the loader.
///
/// This is the GlueAST-equivalent boundary from
/// `codegen-js-symbol-provider-plan.md` — a pure type-description grammar, never
/// live `ts.*` objects or `SemType` closures (neither serialises). The grammar
/// carries the deferred constructs (Dynamic / Structural / extra Export cases)
/// up front so the schema version need not bump when they are filled in.
module Vesper.Ts.Manifest.Schema

[<Literal>]
let SchemaVersion = 0

/// A serialisable type reference — the data-level mirror of `FrozenType`.
[<RequireQualifiedAccess>]
type TypeRef =
    /// Named/intrinsic: `int`, `string`, or `Foo<args>`. Empty args = a
    /// primitive. The name is the canonical FRONT-END identity (`int`, `string`,
    /// `bool`, `unit`) — never a BCL or JS repr; the provider maps it to the
    /// platform repr.
    | Named of name: string * args: TypeRef list
    /// Open type parameter, declaring-axis index.
    | Typar of index: int
    /// Curried function arrow.
    | Fun of args: TypeRef list * ret: TypeRef
    | Tuple of items: TypeRef list
    /// Anonymous structural union → `TyOr`. `null`/`undefined` ride in as their
    /// own members (NOT folded): `T | null | undefined → TyOr [T; null; undefined]`.
    | Union of members: TypeRef list
    /// `any` → `TyDynamic` (deferred front-end type).
    | Dynamic
    /// Structural object type, content-hashed (deferred milestone).
    | Structural of hash: string * fields: (string * TypeRef) list

[<RequireQualifiedAccess>]
type MemberKind =
    | Property
    | Method

type Param =
    {
        Name: string
        Type: TypeRef
        Optional: bool
        Rest: bool
    }

type Signature =
    {
        /// Count of the member's OWN generic type parameters (method axis).
        TypeParams: int
        Params: Param list
        Returns: TypeRef
    }

type Member =
    {
        Name: string
        Kind: MemberKind
        /// Property type (Property kind); `None` for methods (use `Signatures`).
        Type: TypeRef option
        Signatures: Signature list
        Static: bool
        Optional: bool
    }

/// How a top-level symbol is exported — selects the import intrinsic on lowering.
[<RequireQualifiedAccess>]
type ImportShape =
    | Named
    | Default
    | Namespace
    | CommonJsExport

/// A top-level export. MVP emits `Function` + `Interface`; the remaining cases
/// are declared so the grammar is stable but are not yet produced.
[<RequireQualifiedAccess>]
type Export =
    | Function of name: string * signatures: Signature list * import: ImportShape
    | Interface of name: string * typeParams: int * members: Member list * heritage: TypeRef list
    // --- declared for grammar stability; not yet emitted ---
    | Class of name: string * typeParams: int * members: Member list * heritage: TypeRef list * import: ImportShape
    | TypeAlias of name: string * typeParams: int * target: TypeRef
    | Enum of name: string * members: (string * string option) list
    | Variable of name: string * ty: TypeRef * isConst: bool * import: ImportShape
    | Namespace of name: string * exports: Export list

type PackageManifest =
    {
        SchemaVersion: int
        Package: string
        Version: string option
        Exports: Export list
    }
