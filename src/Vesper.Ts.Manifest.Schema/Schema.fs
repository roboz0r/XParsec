/// The serialised TS-extraction manifest (`SchemaVersion` below is the current
/// wire version) — the neutral IR that the F#/Fable extractor PRODUCES and the
/// F# `TsManifestProvider` CONSUMES. Shared so producer and consumer cannot
/// drift: Fable-compiled to JS for the extractor, used natively on .NET by the
/// loader.
///
/// This is the GlueAST-equivalent boundary from
/// `codegen-js-symbol-provider-plan.md` — a pure type-description grammar, never
/// live `ts.*` objects or `SemType` closures (neither serialises). The grammar
/// carries the deferred constructs (Dynamic / Structural / extra Export cases)
/// up front so the schema version need not bump when they are filled in.
module Vesper.Ts.Manifest.Schema

// v1: enum member values are now type-tagged (`LiteralValue`) instead of a bare
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
type LiteralValue =
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
    | Literal of value: LiteralValue
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
    /// TS `any` → the opaque `dynamic` JS intrinsic front-end side (`FTConst "dynamic"`).
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

/// A top-level export. Every case is produced by the extractor.
[<RequireQualifiedAccess>]
type Export =
    | Function of name: string * signatures: Signature list * import: ImportShape
    | Interface of name: string * typeParams: int * members: Member list * heritage: TypeRef list
    | Class of name: string * typeParams: int * members: Member list * heritage: TypeRef list * import: ImportShape
    | TypeAlias of name: string * typeParams: int * target: TypeRef
    /// `members`: each case name paired with its type-tagged value; `None` = a
    /// computed/unresolvable member. The numeric/string/mixed variant falls out
    /// of the member values on the consumer side.
    | Enum of name: string * members: (string * LiteralValue option) list
    | Variable of name: string * ty: TypeRef * isConst: bool * import: ImportShape
    | Namespace of name: string * exports: Export list

[<RequireQualifiedAccess>]
type Severity =
    | Warning
    | Error

/// The KIND of a foreign named reference — mirrors the home manifest's export kind
/// for the same name, so the provider knows how to re-mint the reference: a
/// class/interface mints a homed `FTClass` IDENTITY; an alias/enum stays a carried
/// `FTConst` until its home manifest is stacked (v1 decision — a homed alias must
/// resolve through its home's `Abbrev`, deferred). A CLOSED set here (unlike
/// `DiagCode`, whose `Unknown` tolerates a newer producer): the provider's re-mint
/// dispatch depends on knowing the kind, so `OfWire` rejects an unrecognised one.
[<RequireQualifiedAccess>]
type RefKind =
    | Class
    | Interface
    | Alias
    | Enum

    /// The lowercase wire spelling (stable across schema versions).
    member this.Wire: string =
        match this with
        | Class -> "class"
        | Interface -> "interface"
        | Alias -> "alias"
        | Enum -> "enum"

    static member OfWire(s: string) : Result<RefKind, string> =
        match s with
        | "class" -> Ok Class
        | "interface" -> Ok Interface
        | "alias" -> Ok Alias
        | "enum" -> Ok Enum
        | other -> Error(sprintf "unknown ref kind '%s'" other)

/// One entry in the manifest-level refs table: the FOREIGN identity of a named type
/// this manifest references but does NOT declare — its home module/package, its kind,
/// and its declared generic arity. The ECMA-335 `TypeRef`/`AssemblyRef` analog:
/// IDENTITY ONLY, never the foreign type's shape or members (inlining them is the
/// staleness trap the design forbids). The provider re-mints a homed `SymbolKey`
/// identity from this; member access then resolves through the ordinary provider
/// stack IF the home manifest is part of the compilation, and fails with a
/// "package not referenced" diagnostic when it is not.
type RefEntry =
    {
        /// The referenced type's home module specifier / package — the `Assembly` of
        /// the minted `SymbolKey`.
        Home: string
        Kind: RefKind
        /// The referenced type's DECLARED generic arity: the arity law applies, so the
        /// minted key's simple name is `SymbolKeyOps.arityName name Arity`.
        Arity: int
    }

type Span = { File: string; Start: int; End: int }

/// The degradation vocabulary — one case per way the extractor lowers a TS
/// construct it cannot represent faithfully. The DU *is* the closed set (the old
/// prose "do NOT mint ad-hoc codes" rule, now unmintable): every case except
/// `Unknown` is emitted by the extractor, and the wire string lives in one place
/// (`Wire` / `OfWire`). NOT diagnosed by design (the silent, documented lowering
/// decisions): `any` → `Dynamic` (the designed mapping, not a loss) and the two
/// literal widenings (boolean literal → `bool`, non-integer numeric literal →
/// `float` — design §"string first; skip bool").
[<RequireQualifiedAccess>]
type DiagCode =
    /// A type parameter bound by NEITHER the declaring nor the method axis,
    /// erased to `obj` (defensive — an authored member typar rides the method
    /// axis faithfully and never takes this).
    | MethodAxisTyparErased
    /// An anonymous structural object replaced by a content-hashed stub.
    | StructuralObjectStubbed
    /// get/set accessor with differing types, narrowed to the getter's.
    | AsymmetricAccessorNarrowed
    /// A merged-declaration namespace arm discarded (dominant declaration kept).
    | MergedNamespaceDropped
    /// A `&`-intersection type erased to `obj`.
    | IntersectionErased
    /// A whole top-level symbol whose extraction threw was DROPPED (the per-symbol
    /// resilience backstop for the real-scale `lib.es2015` burndown): an unforeseen
    /// construct made the walk abort, so the symbol is diagnosed + skipped rather than
    /// aborting the entire extraction. Coarser than the in-place degrades above (it
    /// loses the whole export), so those are preferred where the construct is known.
    | SymbolWalkFailed
    /// An accessor (get/set) whose signature could not be resolved for the symmetry
    /// check — degraded to the property's resolved type without the check (a lib-scale
    /// analog of `AsymmetricAccessorNarrowed`, distinguished because nothing was
    /// narrowed: the check simply could not run).
    | AccessorSignatureUnresolved
    /// An `enum` member carrying a NON-INTEGER numeric value (`1.5`) or an unresolvable
    /// name — the wire only carries `IntVal of int64`, so the member's value is dropped
    /// to `None` (a computed member) rather than widening/rounding.
    | EnumMemberDegraded
    /// A class `implements`/heritage clause entry whose interface symbol could not be
    /// resolved — the single entry is dropped from the heritage list.
    | HeritageEntryUnresolved
    /// A `mapType` recursion that exceeded the depth bound — a self-recursive
    /// conditional type (`Awaited<T>`, which `Promise`'s members reference) would else
    /// blow the JS stack; the subtree is degraded to `obj` at the bound so the enclosing
    /// symbol still extracts.
    | RecursionDepthExceeded
    /// Forward tolerance: a code minted by a NEWER extractor decodes losslessly
    /// instead of failing the whole manifest.
    | Unknown of string

    /// The wire spelling (kebab-case, stable across schema versions).
    member this.Wire: string =
        match this with
        | MethodAxisTyparErased -> "method-axis-typar-erased"
        | StructuralObjectStubbed -> "structural-object-stubbed"
        | AsymmetricAccessorNarrowed -> "asymmetric-accessor-narrowed"
        | MergedNamespaceDropped -> "merged-namespace-dropped"
        | IntersectionErased -> "intersection-erased"
        | SymbolWalkFailed -> "symbol-walk-failed"
        | AccessorSignatureUnresolved -> "accessor-signature-unresolved"
        | EnumMemberDegraded -> "enum-member-degraded"
        | HeritageEntryUnresolved -> "heritage-entry-unresolved"
        | RecursionDepthExceeded -> "recursion-depth-exceeded"
        | Unknown s -> s

    static member OfWire(s: string) : DiagCode =
        match s with
        | "method-axis-typar-erased" -> MethodAxisTyparErased
        | "structural-object-stubbed" -> StructuralObjectStubbed
        | "asymmetric-accessor-narrowed" -> AsymmetricAccessorNarrowed
        | "merged-namespace-dropped" -> MergedNamespaceDropped
        | "intersection-erased" -> IntersectionErased
        | "symbol-walk-failed" -> SymbolWalkFailed
        | "accessor-signature-unresolved" -> AccessorSignatureUnresolved
        | "enum-member-degraded" -> EnumMemberDegraded
        | "heritage-entry-unresolved" -> HeritageEntryUnresolved
        | "recursion-depth-exceeded" -> RecursionDepthExceeded
        | other -> Unknown other

/// A degradation the extractor recorded instead of throwing — a structured note
/// that some TS construct could not be represented faithfully and what was emitted
/// in its place. `Span` is optional (not every degradation has a source location).
type Diagnostic =
    {
        Severity: Severity
        Code: DiagCode
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
        /// Foreign named references (the `TypeRef`/`AssemblyRef` analog), keyed by the
        /// referenced type's BARE name. A list of pairs (not a `Map`) so the wire order
        /// is stable for the golden — mirroring `Exports`. IDENTITY ONLY: the provider
        /// mints a homed identity from an entry, never the foreign type's shape. The
        /// codec OMITS the table when empty, so a ref-free manifest stays byte-identical
        /// to a pre-refs golden.
        Refs: (string * RefEntry) list
    }
