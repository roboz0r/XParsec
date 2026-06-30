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

// v1: enum member values are now type-tagged (`EnumValue`) instead of a bare
// `string option`. Stringifying numerics conflated a string member `A = "42"`
// with a numeric `A = 42` (both decoded to `Some "42"`), losing the variant the
// consumer's numeric/string/mixed classification depends on.
[<Literal>]
let SchemaVersion = 1

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

/// A type-tagged enum member value — the data-level mirror of what a TS enum
/// member's constant resolves to. TS source expresses only `number` or `string`
/// (no `byte`/`int16`/`uint64` distinction), so the wire carries exactly those
/// two shapes; integral *width* is NOT here — it is assigned later at the
/// `FrozenType` layer (authored literal suffix, or the `I32` default for a
/// width-less TS import). `None` at the use site marks a computed/unresolvable
/// member.
[<RequireQualifiedAccess>]
type EnumValue =
    /// A TS numeric member, restricted to the integer subset (the extractor
    /// throws on a non-integer literal rather than widening to a float).
    | IntVal of int64
    | StringVal of string

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
    /// `members`: each case name paired with its type-tagged value; `None` = a
    /// computed/unresolvable member. The numeric/string/mixed variant falls out
    /// of the member values on the consumer side.
    | Enum of name: string * members: (string * EnumValue option) list
    | Variable of name: string * ty: TypeRef * isConst: bool * import: ImportShape
    | Namespace of name: string * exports: Export list

[<RequireQualifiedAccess>]
type Severity =
    | Warning
    | Error

type Span = { File: string; Start: int; End: int }

/// A degradation the extractor recorded instead of throwing — a structured note
/// that some TS construct could not be represented faithfully and what was emitted
/// in its place. `Span` is optional (not every degradation has a source location).
///
/// `Code` is drawn from a CLOSED, stable vocabulary — Phase 2 consumers key on
/// these exact strings, so do NOT mint ad-hoc codes:
///   - `method-axis-typar-erased`     — a member's own generic type parameters dropped
///   - `structural-object-stubbed`    — an anonymous structural object replaced by a stub
///   - `asymmetric-accessor-narrowed` — get/set with differing types narrowed to one
///   - `merged-namespace-dropped`     — a merged-declaration namespace arm discarded
///   - `any-dynamic`                  — `any` lowered to the deferred dynamic type
///   - `intersection-erased`          — a `&`-intersection type erased
///   - `literal-widened`             — a literal type widened to its base
type Diagnostic =
    {
        Severity: Severity
        Code: string
        Symbol: string
        Span: Span option
        Message: string
    }

type PackageManifest =
    {
        SchemaVersion: int
        Package: string
        Version: string option
        Exports: Export list
        Diagnostics: Diagnostic list
    }
