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

/// A type-tagged literal value — the data-level mirror of a string/number literal
/// constant. Shared by `TypeRef.Literal` (a TS literal TYPE, `"GET"`) and enum
/// member values (a TS enum member's constant). TS source expresses only `number`
/// or `string`, so the wire carries exactly those two shapes; no `bool` (design
/// §"Literal types stay structural"). `None` at an enum use site marks a
/// computed/unresolvable member.
[<RequireQualifiedAccess>]
type EnumValue =
    /// A TS numeric member/literal, restricted to the integer subset (the extractor
    /// throws on a non-integer literal rather than widening to a float).
    | IntVal of int64
    | StringVal of string

/// A serialisable type reference — the data-level mirror of `FrozenType`.
[<RequireQualifiedAccess>]
type TypeRef =
    /// Named/intrinsic: `int`, `string`, or `Foo<args>`. Empty args = a
    /// primitive. The name is the canonical FRONT-END identity (`int`, `string`,
    /// `bool`, `unit`) — never a BCL or JS repr; the provider maps it to the
    /// platform repr.
    | Named of name: string * args: TypeRef list
    /// Open type parameter, declaring-axis index (the enclosing class / interface /
    /// alias / free-function's own typars).
    | Typar of index: int
    /// Open type parameter, METHOD-axis index — a generic MEMBER's OWN type parameter
    /// (`map<U>(x: U)` → `U` is `MethodTypar 0`), distinct from the declaring axis so
    /// the provider can freshen it per call site (`FTTypar(TyparAxis.Method, i)`). Only
    /// a member-of-a-type carries this; a free function's own typars ride `Typar`.
    | MethodTypar of index: int
    /// Curried function arrow.
    | Fun of args: TypeRef list * ret: TypeRef
    | Tuple of items: TypeRef list
    /// Anonymous structural union → `TyOr`. `null`/`undefined` ride in as their
    /// own members (NOT folded): `T | null | undefined → TyOr [T; null; undefined]`.
    | Union of members: TypeRef list
    /// A TS string/number literal TYPE (`"GET"`, `42`) → `FTLiteral`. Structural,
    /// external-vocabulary only; Vesper inference NEVER mints one (design §"Literal
    /// types stay structural … the nominalism invariant"). A literal composes with
    /// `Union` — `("ping" | "pong")` is `Union [Literal "ping"; Literal "pong"]`.
    | Literal of value: EnumValue
    /// `keyof T` (a TS index-query type) → `FTKeyOf`. Carried FAITHFULLY, never
    /// evaluated in TS-land (design §"keyof … ride on top … ground-EVALUATED rather
    /// than degraded"): the front end folds it to the member-name literal union when
    /// `T` is ground. Its child references the queried type (`keyof Events` →
    /// `KeyOf (Typar 0)`).
    | KeyOf of TypeRef
    /// `T[K]` (an indexed-access type) → `FTIndexedAccess`. Carried faithfully: the
    /// front end folds it to the member's type when `objTy` is ground and `index` is
    /// a known literal (`Events[Key]` → `IndexedAccess(Typar 0, MethodTypar 0)`).
    | IndexedAccess of objTy: TypeRef * index: TypeRef
    /// `check extends extends_ ? whenTrue : whenFalse` (a conditional type) →
    /// `FTConditional`. Carried faithfully: the front end picks a branch when
    /// `check`/`extends_` are ground (mitt's `undefined extends Events[Key] ? Key :
    /// never`).
    | Conditional of check: TypeRef * extends: TypeRef * whenTrue: TypeRef * whenFalse: TypeRef
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
        /// Per-method-typar upper bound, aligned to the method axis: index `i` is the
        /// `i`-th own type parameter's constraint (`<Key extends keyof Events>` → the
        /// `keyof Events` `TypeRef`), `None` when unconstrained. Length is `TypeParams`.
        /// CARRIED, not evaluated — the front end reads it at grounding (design §"keyof
        /// … ground-EVALUATED"): step 3 solves a freshened method typar against its
        /// bound's keyof-fold. The codec OMITS the field when every entry is `None`, so
        /// a constraint-free signature stays byte-identical to a pre-slot golden.
        TypeParamBounds: TypeRef option list
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
