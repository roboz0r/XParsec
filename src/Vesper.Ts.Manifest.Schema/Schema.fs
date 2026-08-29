/// The serialised TS-extraction manifest — a pure type-description grammar (never live
/// `ts.*` objects or `SemType` closures, neither of which serialises), Fable-compiled to
/// JS for the extractor that produces it and used natively on .NET by `TsManifestProvider`.
module Vesper.Ts.Manifest.Schema

[<Literal>]
let SchemaVersion = 2

/// A type-tagged literal value, shared by `TypeRef.Literal` (a TS literal TYPE, `"GET"`)
/// and enum member values. TS source expresses only `number` or `string`, so the wire
/// carries exactly those two shapes — no `bool`.
[<RequireQualifiedAccess>]
type LiteralValue =
    /// Integer subset only: a non-integer TS numeric widens to `float` (as a literal type)
    /// or drops to `None` (as an enum member).
    | IntVal of int64
    | StringVal of string

/// A serialisable type reference — the data-level mirror of `FrozenType`.
[<RequireQualifiedAccess>]
type TypeRef =
    /// Named/intrinsic: `int`, `string`, or `Foo<args>`. The name is the canonical
    /// FRONT-END identity (`int`, `string`, `bool`, `unit`) — never a BCL or JS repr;
    /// the provider maps it to the platform repr.
    | Named of name: string * args: TypeRef list
    /// Open type parameter, declaring-axis index (the enclosing class / interface /
    /// alias / free-function's own typars).
    | Typar of index: int
    /// Open type parameter, METHOD-axis index — a generic MEMBER's OWN type parameter
    /// (`map<U>(x: U)` → `U` is `MethodTypar 0`). Only a member-of-a-type carries this;
    /// a free function's own typars are carried on `Typar`.
    | MethodTypar of index: int
    /// Curried function type.
    | Fun of args: TypeRef list * ret: TypeRef
    | Tuple of items: TypeRef list
    /// Anonymous structural union → `FTOr`. `null`/`undefined` arrive as their
    /// own disjuncts (NOT folded): `T | null | undefined → FTOr [T; null; undefined]`.
    | Union of disjuncts: TypeRef list
    /// A TS string/number literal TYPE (`"GET"`, `42`) → `FTLiteral`. Composes with
    /// `Union`: `("ping" | "pong")` is `Union [Literal "ping"; Literal "pong"]`.
    | Literal of value: LiteralValue
    /// `keyof T` → `FTKeyOf`. Carried, never evaluated here: the front end folds it to the
    /// member-name literal union when `T` is ground. The child is the queried type
    /// (`keyof Events` → `KeyOf (Typar 0)`).
    | KeyOf of TypeRef
    /// `T[K]` (an indexed-access type) → `FTIndexedAccess`. Carried faithfully: the
    /// front end folds it to the member's type when `objTy` is ground and `index` is
    /// a known literal (`Events[Key]` → `IndexedAccess(Typar 0, MethodTypar 0)`).
    | IndexedAccess of objTy: TypeRef * index: TypeRef
    /// `check extends extends_ ? whenTrue : whenFalse` → `FTConditional`. Carried
    /// faithfully: the front end picks a branch when `check`/`extends_` are ground
    /// (`undefined extends Events[Key] ? Key : never`).
    | Conditional of check: TypeRef * extends: TypeRef * whenTrue: TypeRef * whenFalse: TypeRef
    /// TS `any` → the opaque `dynamic` JS intrinsic front-end side (`FTConst "dynamic"`).
    | Dynamic
    /// Structural object type, content-hashed. `index` carries the TS index signatures
    /// `{ [k: string]: T }` the object bears, as `(key, value)` pairs — a type may declare
    /// BOTH a string- and a number-index signature, so all of them are carried. Empty list = none.
    | Structural of hash: string * fields: (string * TypeRef) list * index: (TypeRef * TypeRef) list

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
        /// Per-method-typar upper bound: index `i` is the `i`-th own type parameter's
        /// constraint (`<Key extends keyof Events>` → the `keyof Events` `TypeRef`),
        /// `None` when unconstrained. Length is `TypeParams`.
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

[<RequireQualifiedAccess>]
type ImportShape =
    | Named
    | Default
    | Namespace
    | CommonJsExport

[<RequireQualifiedAccess>]
type Export =
    | Function of name: string * signatures: Signature list * import: ImportShape
    /// `index`: the TS index signatures on the interface, as `TypeRef.Structural`'s. They
    /// arrive FLATTENED through heritage, so `ProcessEnv extends Dict<T>` carries the
    /// string index directly. Empty list = none.
    | Interface of
        name: string *
        typeParams: int *
        members: Member list *
        heritage: TypeRef list *
        index: (TypeRef * TypeRef) list
    /// `index`: the TS index signatures on the class, as `Interface`'s.
    | Class of
        name: string *
        typeParams: int *
        members: Member list *
        heritage: TypeRef list *
        import: ImportShape *
        index: (TypeRef * TypeRef) list
    | TypeAlias of name: string * typeParams: int * target: TypeRef
    /// `members`: each case name paired with its value; `None` = a computed, non-integer
    /// or otherwise unresolvable member.
    | Enum of name: string * members: (string * LiteralValue option) list
    | Variable of name: string * ty: TypeRef * isConst: bool * import: ImportShape
    | Namespace of name: string * exports: Export list

[<RequireQualifiedAccess>]
type Severity =
    | Warning
    | Error

/// The KIND of a foreign named reference — mirrors the home manifest's export kind for
/// the same name, so the provider knows how to re-mint it: a class/interface mints a
/// homed `FTClass` IDENTITY; an alias/enum stays a carried `FTConst`.
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

/// One entry in the manifest-level refs table: the FOREIGN identity of a named type this
/// manifest references but does NOT declare. IDENTITY ONLY — the provider re-mints a homed
/// `SymbolKey` from it and resolves members through the home manifest, never from here.
type RefEntry =
    {
        /// The referenced type's home module specifier / package — the `Assembly` of the
        /// minted `SymbolKey`.
        Home: string
        Kind: RefKind
        TyparArity: int
    }

type Span = { File: string; Start: int; End: int }

/// The degradation vocabulary — one case per way the extractor lowers a TS construct it
/// cannot represent faithfully. Three lowerings are deliberately NOT diagnosed: `any` →
/// `Dynamic`, boolean literal → `bool`, and non-integer numeric literal → `float`.
[<RequireQualifiedAccess>]
type DiagCode =
    /// A type parameter bound by NEITHER the declaring nor the method axis, erased to `obj`.
    | MethodAxisTyparErased
    /// An anonymous structural object replaced by a content-hashed stub.
    | StructuralObjectStubbed
    /// get/set accessor with differing types, narrowed to the getter's.
    | AsymmetricAccessorNarrowed
    /// A merged-declaration namespace arm discarded (dominant declaration kept).
    | MergedNamespaceDropped
    /// A `&`-intersection type erased to `obj`.
    | IntersectionErased
    /// A whole top-level symbol whose extraction threw was DROPPED — diagnosed and skipped
    /// rather than aborting the entire extraction.
    | SymbolWalkFailed
    /// An accessor (get/set) whose signature could not be resolved for the symmetry check —
    /// degraded to the property's resolved type. Unlike `AsymmetricAccessorNarrowed`,
    /// nothing was narrowed: the check simply could not run.
    | AccessorSignatureUnresolved
    /// An `enum` member carrying a NON-INTEGER numeric value (`1.5`) or an unresolvable
    /// name — the wire only carries `IntVal of int64`, so the member's value is dropped
    /// to `None` (a computed member) rather than widening/rounding.
    | EnumMemberDegraded
    /// A class `implements`/heritage clause entry whose interface symbol could not be
    /// resolved — the single entry is dropped from the heritage list.
    | HeritageEntryUnresolved
    /// Type-mapping recursion past the depth bound — a self-recursive conditional type
    /// (`Awaited<T>`) would else blow the JS stack; the subtree is degraded to `obj` at the
    /// bound so the enclosing symbol still extracts.
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

/// A degradation the extractor recorded instead of throwing: which TS construct could not
/// be represented faithfully, and what was emitted in its place.
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
        /// Foreign named references, keyed by the referenced type's BARE name. A list of
        /// pairs and not a `Map`, so the wire order is stable for the golden — as `Exports`.
        Refs: (string * RefEntry) list
    }
