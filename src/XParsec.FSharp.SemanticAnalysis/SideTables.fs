namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// Side tables hold all in-flight semantic information. CST is never mutated.

/// Project-local nominal identity for a type definition. `asm` is the type's **home assembly** — `Some <thisAsm>`
/// when the compilation knows its target assembly (`PassContext.AssemblyName`),
/// `None` only on the front-end-only / contract-scrape paths that never emit. It
/// is invariant per type: a consumer mints the SAME key for this type from its
/// `SymbolOrigin.Assembly`, so a project-local key equals the cross-package
/// reference key (the property the codegen local/external branch reads). `name`
/// is the .NET arity-qualified simple name (`Choice\`2`); `ns` is the declaring
/// namespace, threaded through registration (`stampLocalTypeKey`).
module internal LocalSymbolKey =

    /// The project-local `SymbolKey.TypeKey` for `name` at `arity`, declared in
    /// namespace `ns`, with home assembly `asm` (`Some <thisAsm>` for an emitting
    /// compilation, `None` for the front-end-only paths). The arity-name rule is
    /// `SymbolKeyOps.arityName` — the one shared definition, so the registry key
    /// (`TypeRegistry.keyFor`) and the stamped `SymbolKey` name can't drift.
    let ofType (asm: string option) (ns: string) (name: string) (arity: int) : SymbolKey =
        SymbolKey.TypeKey(asm, ns, SymbolKeyOps.arityName name arity)

    /// The project-local `SymbolKey.MemberKey` for a member `name` of `kind` on the
    /// type identified by `declKey`, with `arity` value parameters. Project-local
    /// members carry no overload set (overload resolution is a separate future
    /// feature), so `(declKey, name)` is unique and the local resolution path ignores
    /// `argSig`. But codegen's *external* member-ref param-flatten reads `argSig.Length`
    /// to decide whether a `.NET`-tupled static member (`op_Addition(Set, Set)`) mints
    /// N parameters or one `ValueTuple` — so a key that may target an external
    /// declaring type (an SRTP `+`/`-` dispatch) must carry the real `arity`. The
    /// `argSig` contents are placeholders; only the length is ever read. The local
    /// analogue of the external `MemberKey` minted by `MetadataSymbols` / `VesperLib`;
    /// carried on the local member-call TAST nodes so codegen reads the declaring type
    /// off `decl` instead of re-deriving it from a class-name string.
    let ofMember (declKey: SymbolKey) (name: string) (arity: int) (kind: MemberKind) : SymbolKey =
        SymbolKey.MemberKey(declKey, name, EqArray.ofList (List.replicate arity ""), kind)

// `ModuleMemberInfo` moved to `SideTypes.fs` (it must precede `Tast.fs`).

/// Field types start as fresh TyVars stamped by NameResolution and get linked
/// to the real translated type by Unification before any expression is typed.
[<Sealed>]
type RecordFieldInfo(name: string, ty: SemType, isMutable: bool, declKey: NodeKey) =
    member val Name = name
    member val Type = ty
    member val IsMutable = isMutable
    member val DeclKey = declKey

/// Properties in v1 are read-only (get-only); v1 `AutoProperty` also lands here
/// as `Property`.
[<RequireQualifiedAccess>]
type ClassMemberKind =
    | Method
    | Property

/// Per-member metadata for a class augmentation, union augmentation, or
/// (post-sprint) interface impl — the union-vs-class split is owned only by
/// which side table holds the array (`ClassTypeInfo.Members` vs
/// `UnionTypeInfo.Members`), not by the record itself.
///
/// Member types start as placeholder TyVars and get linked by Unification's
/// `fillTypeMembers` after the registry is populated. Forward references
/// between members in the same type therefore resolve against the placeholder.
[<Sealed>]
type TypeMemberInfo(name: string, kind: ClassMemberKind, isStatic: bool, ty: SemType, declKey: NodeKey) =
    new(name, kind, ty, declKey) = TypeMemberInfo(name, kind, false, ty, declKey)
    member val Name = name
    member val Kind = kind
    /// `true` for `static member`s. Instance members are looked up via
    /// the receiver's TyClass / TyUnion; static members are looked up by type
    /// name + member name (no `this` binding inside the body).
    member val IsStatic = isStatic
    member val Type = ty
    member val DeclKey = declKey
    /// The member's *own* generic parameters (e.g. `abstract Map<'C> : ...`),
    /// as prototype TyVars keyed by source name. Empty for a non-generic member.
    /// REGISTRATION SEED ONLY: this is the pre-inference identity prototype set read
    /// by body inference (lookups are by name / root identity, so its order is
    /// irrelevant). It is NOT the final ABI order — that lives in `Generalized`,
    /// written post-inference. `DeclaredTyparCount` still splits the explicit prefix.
    member val MethodTypeParams: EqArray<string * TypeVar> = EqArray.empty with get, set
    /// Canonical (post-inference) method typars — the ABI order, correct-by-construction.
    /// Written ONCE by `generaliseMemberTypars` (regular methods) or the abstract-signature
    /// path (abstract methods). `MethodTypeParams` above is only the pre-inference identity seed.
    member val Generalized: GeneralizedTypars = GeneralizedTypars.empty with get, set
    /// How many leading entries of `MethodTypeParams` are the member's
    /// EXPLICITLY-declared `<'C, …>` typars (source order). The remaining entries
    /// are annotation-implicit typars appended at registration. Captured here
    /// because the explicit/implicit split is otherwise unrecoverable post-
    /// registration, yet the F# ordering rule treats explicitly-declared typars
    /// (and ONLY those) as "declared-first"; `Unification.generaliseMemberTypars`
    /// passes exactly this prefix as `GeneralizedTypars.canonical`'s `declared`.
    member val DeclaredTyparCount: int = 0 with get, set

    /// The method typars to use at a member CALL site: the canonical `Generalized`
    /// once it is populated, else the registration seed (forward references within a
    /// class can call a member before it is generalised). Order-irrelevant here —
    /// `instantiateMemberCall` freshens by union-find root. Faithfully reproduces the
    /// pre-split read of `MethodTypeParams` (which was seed-then-canonical).
    member this.EffectiveMethodTypars: EqArray<string * TypeVar> =
        if GeneralizedTypars.count this.Generalized > 0 then
            EqArray.ofArray (GeneralizedTypars.toArray this.Generalized)
        else
            this.MethodTypeParams

    /// `true` when the source declares the member with `MemberKeyword.Override`
    /// or `MemberKeyword.Default`. Stamped by the `registerInheritedSlots`
    /// post-pass; consumed by Freeze/Codegen to choose `call` vs `callvirt`.
    member val IsOverride: bool = false with get, set

/// Field types start as fresh TyVar placeholders stamped by NameResolution and
/// are linked by Unification's field-fill-in pass before any expression is
/// typed. `FieldNames` carries per-field names for named fields
/// (`| Case of x: int * y: int`); positional fields have `ValueNone`.
[<Sealed>]
type UnionCaseInfo
    (name: string, unionName: string, unionArity: int, fields: SemType[], fieldNames: string voption[], declKey: NodeKey)
    =
    member val Name = name
    member val UnionName = unionName
    /// Generic arity of the declaring union (count of its type parameters). Pairs
    /// with `UnionName` to resolve the *right* union when the short name is
    /// overloaded by arity (`Choice\`2`…`Choice\`7`): `TypeRegistry.unionOfCase`
    /// keys `ctx.Types.Union` by `(UnionName, UnionArity)`.
    member val UnionArity = unionArity
    member val Fields = fields
    member val FieldNames = fieldNames
    member val DeclKey = declKey

/// A registered `interface IFace with member …` block on a class or union.
/// `InterfaceCst` is the parsed interface
/// `Type` — re-read by Unification's `fillClassMembers` (the external provider
/// isn't available at NameResolution time) to resolve + verify the target is an
/// interface, linking `Resolved`. `Members` are the impl's method / property
/// placeholders (same shape as a class augmentation member, types linked by
/// `fillTypeMembers`); their bodies live in `Elements` — each interface
/// `MemberDefn` re-wrapped as a `TypeDefnElement.Member` so the NameResolution /
/// Unification member walks consume them unchanged. `DeclKey` anchors a
/// "not an interface" diagnostic at the interface type's name token.
[<Sealed>]
type ClassInterfaceImplInfo
    (
        interfaceCst: Type<SyntaxToken>,
        members: TypeMemberInfo[],
        elements: TypeDefnElements<SyntaxToken>,
        declKey: NodeKey
    ) =
    member val InterfaceCst = interfaceCst
    member val Members = members
    member val Elements = elements
    member val DeclKey = declKey
    /// Resolved interface type, filled by Unification's `fillClassMembers` once
    /// the external provider can map `InterfaceCst`. `ValueNone` until then, and
    /// left `ValueNone` if resolution fails (the diagnostic already fired).
    member val Resolved: SemType voption = ValueNone with get, set

/// The shared surface a nominal type exposes to the interface-impl machinery —
/// implemented by both `ClassTypeInfo` and `UnionTypeInfo` so Unification's
/// `resolveInterfaceImpls` / `fillInterfaceImpls`, the `subsumes` interface
/// admission and the custom-eq/comp conformance check operate over *either* kind
/// without forking the (already kind-agnostic) logic. `MkSelfType` is the only
/// kind-dependent piece: a class yields `TyClass(Key, args)`, a union
/// `TyUnion(Key, args)`, so the `this`-type seeding inside an impl body is exact.
type IInterfaceImplHost =
    abstract member Key: SymbolKey
    abstract member DeclKey: NodeKey
    abstract member TypeParams: EqArray<string * TypeVar>
    /// Source-text name bound to `this` inside member / impl bodies (`"this"` unless
    /// an `as`-binder renamed it). Used by NameResolution to seed the body scope.
    abstract member ThisName: string
    abstract member ThisKey: NodeKey
    abstract member InterfaceImpls: ClassInterfaceImplInfo[]
    abstract member Members: TypeMemberInfo[]
    abstract member EqualitySupport: EqualityVerdict
    abstract member ComparisonSupport: ComparisonVerdict
    /// Build the host's own nominal Self type at the given type args
    /// (`TyClass` for a class, `TyUnion` for a union).
    abstract member MkSelfType: EqArray<SemType> -> SemType

/// `TypeParams` carries declared typars in declaration order, paired with
/// source-text names. Each TypeVar is a *prototype* — substituted out by
/// `instantiateRecordType` at every use site so independent instantiations get
/// independent variables. `Fields[i].Type` may reference these TyVars directly
/// (a bare `'a` field type shares identity with the corresponding `TypeParams[i]`).
[<Sealed>]
type RecordTypeInfo
    (
        name: string,
        typeParams: EqArray<string * TypeVar>,
        fields: RecordFieldInfo[],
        declKey: NodeKey,
        typarConstraints: TyparConstraints<SyntaxToken> voption,
        key: SymbolKey
    ) =
    new(name, typeParams, fields, declKey) =
        RecordTypeInfo(
            name,
            typeParams,
            fields,
            declKey,
            ValueNone,
            LocalSymbolKey.ofType None "" name typeParams.Length
        )

    member val Name = name
    /// Stable project-local nominal identity — the arity-qualified
    /// `TypeKey(asm, declNs, name\`arity)` minted by `stampLocalTypeKey` at registration
    /// (`asm`/`declNs` = the type's home assembly + declaring namespace). The
    /// convenience constructor (synthesis / test paths with no namespace in scope)
    /// defaults to the `TypeKey(None, "", name\`arity)` placeholder.
    member val Key: SymbolKey = key
    member val TypeParams = typeParams
    member val Fields = fields
    member val DeclKey = declKey
    /// `when 'a : ...` clause attached to the type's typar list, if any.
    /// `Unification.fillRecordFieldTypes` walks this and attaches each
    /// constraint to the matching prototype TyVar in `TypeParams`.
    member val TyparConstraints = typarConstraints
    /// Equality posture for this record.
    /// Filled during `NameResolution.registerRecordTypeDefn` from the type's
    /// attributes; the placeholder defaults to `Structural` so any path that
    /// overlooks the registration (mostly tests that synthesise records
    /// directly) stays equal-by-fields. `Unification.checkConstraint` reads it
    /// to short-circuit `NoEquality` types; `Freeze` projects it onto
    /// `TTypeDecl.EqualitySupport` for codegen.
    member val EqualitySupport = EqualityVerdict.Structural with get, set
    /// Comparison posture for this record. Filled during
    /// `NameResolution.registerRecordTypeDefn` from the type's attributes. Defaults
    /// to `NoComparison` (opt-in). `Unification.checkConstraint` reads it to reject
    /// `<` / `>` / `<=` / `>=` on un-annotated types; `Freeze` projects it onto
    /// `TTypeDecl.ComparisonSupport` for codegen.
    member val ComparisonSupport = ComparisonVerdict.NoComparison with get, set
    /// Augmentation members (`with member …` / `static member …`). Member types
    /// start as placeholder TyVars and are linked by Unification's
    /// `fillRecordMembers`. Empty for a plain record (mirrors `UnionTypeInfo.Members`).
    member val Members: TypeMemberInfo[] = [||] with get, set
    /// `this`-binding source name (default `"this"`; honours `as self`).
    member val ThisName = "this" with get, set
    /// Synthetic NodeKey for the `this` binder shared across every instance
    /// member body in this record. Set during registration when there are members.
    member val ThisKey = Unchecked.defaultof<NodeKey> with get, set
    /// `interface IFace with member …` blocks declared on the record.
    /// Stamped by `NameResolution.registerNominalMembers`; each impl's interface type
    /// is resolved + verified, and its member bodies typed, by Unification's
    /// `fillHostMembers` (mirroring `UnionTypeInfo.InterfaceImpls`). Empty unless
    /// the record declares an `interface … with` block. `Freeze` projects them onto
    /// `TTypeKind.Record.interfaces`.
    member val InterfaceImpls: ClassInterfaceImplInfo[] = [||] with get, set

    interface IInterfaceImplHost with
        member this.Key = this.Key
        member this.DeclKey = this.DeclKey
        member this.TypeParams = this.TypeParams
        member this.ThisName = this.ThisName
        member this.ThisKey = this.ThisKey
        member this.InterfaceImpls = this.InterfaceImpls
        member this.Members = this.Members
        member this.EqualitySupport = this.EqualitySupport
        member this.ComparisonSupport = this.ComparisonSupport
        member this.MkSelfType args = TyRecord(this.Key, args)

/// `TypeParams` mirrors `RecordTypeInfo.TypeParams`. Case field types may
/// reference these TyVars directly.
[<Sealed>]
type UnionTypeInfo
    (
        name: string,
        typeParams: EqArray<string * TypeVar>,
        cases: UnionCaseInfo[],
        declKey: NodeKey,
        typarConstraints: TyparConstraints<SyntaxToken> voption,
        key: SymbolKey
    ) =
    new(name, typeParams, cases, declKey) =
        UnionTypeInfo(name, typeParams, cases, declKey, ValueNone, LocalSymbolKey.ofType None "" name typeParams.Length)

    member val Name = name
    /// Stable project-local nominal identity — the arity-qualified
    /// `TypeKey(asm, declNs, name\`arity)` (e.g. `Choice\`2`) minted by `stampLocalTypeKey`
    /// at registration to match the emitted metadata name. The convenience constructor
    /// (synthesis / test paths with no namespace in scope) defaults to the
    /// `TypeKey(None, "", name\`arity)` placeholder.
    member val Key: SymbolKey = key
    member val TypeParams = typeParams
    member val Cases = cases
    member val DeclKey = declKey
    /// `when 'a : ...` clause attached to the type's typar list, if any.
    /// `Unification.fillUnionFieldTypes` walks this and attaches each
    /// constraint to the matching prototype TyVar in `TypeParams`.
    member val TyparConstraints = typarConstraints
    /// Augmentation members (`with member …` / `static member …`). Member types
    /// start as placeholder TyVars and are linked by Unification's
    /// `fillUnionMembers`. Empty for a plain union.
    member val Members: TypeMemberInfo[] = [||] with get, set
    /// `this`-binding source name (default `"this"`; honours `as self`).
    member val ThisName = "this" with get, set
    /// Synthetic NodeKey for the `this` binder shared across every instance
    /// member body in this union. Set during registration when there are members.
    member val ThisKey = Unchecked.defaultof<NodeKey> with get, set
    /// Equality posture for this union. Filled during
    /// `NameResolution.registerUnionTypeDefn`; defaults to `Structural`.
    /// `Unification.checkConstraint` short-circuits on `NoEquality`; `Freeze`
    /// projects it onto `TTypeDecl.EqualitySupport` for codegen.
    member val EqualitySupport = EqualityVerdict.Structural with get, set
    /// Comparison posture for this union. Filled during
    /// `NameResolution.registerUnionTypeDefn` from the type's attributes. Defaults
    /// to `NoComparison` (opt-in). `Unification.checkConstraint` reads it to reject
    /// `<` / `>` / `<=` / `>=` on un-annotated types; `Freeze` projects it onto
    /// `TTypeDecl.ComparisonSupport` for codegen.
    member val ComparisonSupport = ComparisonVerdict.NoComparison with get, set
    /// `interface IFace with member …` blocks declared on the union.
    /// Stamped by `NameResolution.registerNominalMembers`; each impl's interface type
    /// is resolved + verified, and its member bodies typed, by Unification's
    /// `fillHostMembers` (mirroring `ClassTypeInfo.InterfaceImpls`). Empty unless
    /// the union declares an `interface … with` block. `Freeze` projects them onto
    /// `TTypeKind.Union.interfaces`; codegen emission is deferred.
    member val InterfaceImpls: ClassInterfaceImplInfo[] = [||] with get, set

    interface IInterfaceImplHost with
        member this.Key = this.Key
        member this.DeclKey = this.DeclKey
        member this.TypeParams = this.TypeParams
        member this.ThisName = this.ThisName
        member this.ThisKey = this.ThisKey
        member this.InterfaceImpls = this.InterfaceImpls
        member this.Members = this.Members
        member this.EqualitySupport = this.EqualitySupport
        member this.ComparisonSupport = this.ComparisonSupport
        member this.MkSelfType args = TyUnion(this.Key, args)

/// Host side-table for an inline intrinsic-abbrev augmented with concrete
/// `(# … #)`-bodied members (`type widget = (# "object" #) with member …`).
/// The abbrev KEEPS its `TyConst` identity: it stays in `IntrinsicReprTypes` and
/// `translateType` resolves the name to `TyConst name` at every use site, so this
/// is NOT a nominal type registration — it exists ONLY to hang the augmentation
/// members off the same `IInterfaceImplHost` member-extract / fill / elaborate path
/// unions and records use. `MkSelfType` yields `TyConst name args` (the intrinsic
/// identity), so each member's `ThisTy` is the intrinsic type, never a `TyClass`.
/// Interface impls are out of scope for the intrinsic host (always empty). The
/// surfaced `TDecl.Type(Class)` is an internal artifact consumed only by the
/// member-inline harvest; it is never emitted.
[<Sealed>]
type IntrinsicAbbrevInfo
    (name: string, typeParams: EqArray<string * TypeVar>, declKey: NodeKey, key: SymbolKey, selfKey: SymbolKey) =
    member val Name = name
    /// Stable project-local nominal identity, minted by `stampLocalTypeKey` at
    /// registration to match a use-site key. Never emitted (the abbrev is intrinsic).
    member val Key: SymbolKey = key
    /// The abbrev's INTRINSIC identity key (verbatim name, contract namespace,
    /// asm-blind), resolved through `TypeRegistry.intrinsicKeyOf` at registration —
    /// the SAME key a use-site (`Translate`) resolves the abbrev name to. Distinct
    /// from `Key` (arity-suffixed local nominal, for the member-harvest host path):
    /// this is the `TyConst` key `MkSelfType` seeds onto each member's `ThisTy`, so
    /// a non-`Vesper` user intrinsic-abbrev's self-type cannot diverge from its
    /// use-site identity (`primitiveKey name` hardcoded `Vesper`, the latent split-brain).
    member val SelfKey: SymbolKey = selfKey
    member val TypeParams = typeParams
    member val DeclKey = declKey
    /// Augmentation members (`with member …`). Stamped by
    /// `NameResolution.registerNominalMembers`; types linked by Unification's
    /// `fillHostMembers`. Empty until then.
    member val Members: TypeMemberInfo[] = [||] with get, set
    /// `this`-binding source name (default `"this"`; honours `as self`).
    member val ThisName = "this" with get, set
    /// Synthetic NodeKey for the `this` binder shared across every instance member
    /// body. Set during registration when there are members.
    member val ThisKey = Unchecked.defaultof<NodeKey> with get, set
    /// `interface … with` blocks are out of scope for the intrinsic host; always empty.
    member val InterfaceImpls: ClassInterfaceImplInfo[] = [||] with get, set

    interface IInterfaceImplHost with
        member this.Key = this.Key
        member this.DeclKey = this.DeclKey
        member this.TypeParams = this.TypeParams
        member this.ThisName = this.ThisName
        member this.ThisKey = this.ThisKey
        member this.InterfaceImpls = this.InterfaceImpls
        member this.Members = this.Members
        // An intrinsic value repr is reference-neutral here; these verdicts are
        // unread for this host (it is never surfaced through the equality/comparison
        // gate — the harvest reads only its members).
        member _.EqualitySupport = EqualityVerdict.Reference
        member _.ComparisonSupport = ComparisonVerdict.NoComparison
        // The load-bearing choice: the member self-type is the abbrev's INTRINSIC
        // type (`TyConst SelfKey args`), preserving `X`'s `TyConst` identity — not a
        // `TyClass`. `translateNominalMember` stamps this onto each member's `ThisTy`.
        // `SelfKey` is the contract-resolved identity (via `intrinsicKeyOf`), so a
        // non-`Vesper` user intrinsic-abbrev's `this` type matches its use-site key.
        member this.MkSelfType args = TyConst(this.SelfKey, args)

/// An enum type declaration (`type E = | C1 = v1 | …`). Unlike unions/records,
/// an enum is non-generic and carries no member side tables: it is a closed,
/// named set of cases. Registration (`NameResolution.registerEnumTypeDefn`) needs
/// only the **case names** (for the qualified-access membership check `E.C1`) plus
/// the minted nominal `Key`; the case→literal *values* are resolved later by
/// `Elaborate.tryEnumType` (which alone has the literal readers in compile order)
/// and ride the surfaced `TTypeKind.Enum` node, the single source of truth.
/// `Key` is the arity-0 `TypeKey(asm, declNs, name)` minted by `stampLocalTypeKey`,
/// so a `(x: E)` annotation resolves to `TyEnum Key` and the surfaced decl carries
/// the identical key.
[<Sealed>]
type EnumTypeInfo
    (name: string, caseNames: string[], caseStringValues: string[] voption, declKey: NodeKey, key: SymbolKey) =
    member val Name = name
    /// Case identifiers in declaration order. The `E.C1` qualified-access path
    /// checks membership here; a name absent from it is a resolution error.
    member val CaseNames = caseNames
    /// The case VALUES when every case is a string literal (`| Auto = "auto"`,
    /// `| A = ("auto")`), in declaration order; `ValueNone` otherwise (numeric /
    /// mixed / computed). The full case→literal table is resolved later by
    /// `Elaborate.resolveEnumCaseValue`, but the literal-union admission (`subsumes`,
    /// at Unification time — BEFORE Elaborate) needs the string value SET early, so
    /// the string cases are read here through the SAME
    /// `StringLiterals.tryEnumCaseStringLiteral` projection Elaborate uses (they
    /// cannot disagree on which cases carry a string constant).
    member val CaseStringValues: string[] voption = caseStringValues
    member val DeclKey = declKey
    /// Stable project-local nominal identity — the arity-0 `TypeKey(asm, declNs,
    /// name)` minted by `stampLocalTypeKey`; matches the surfaced `TDecl.Type.Key`.
    member val Key: SymbolKey = key
    /// Is `n` one of this enum's declared cases? Drives the `E.C1` membership check.
    member this.HasCase(n: string) = Array.contains n caseNames

/// `InProgress` is set while translating the RHS so a re-entry through
/// `translateType` can detect a cycle and short-circuit. `Filled` is terminal —
/// once set, neither `Body` nor `Status` mutates again.
[<RequireQualifiedAccess>]
type AbbreviationStatus =
    | NotFilled
    | InProgress
    | Filled

/// `Body` is filled lazily by Unification's `forceFill` so order within a module
/// doesn't matter — a declaration can reference any other type in the same group.
[<Sealed>]
type AbbreviationInfo
    (
        name: string,
        typeParams: EqArray<string * TypeVar>,
        rhsCst: Type<SyntaxToken>,
        declKey: NodeKey,
        typarConstraints: TyparConstraints<SyntaxToken> voption,
        key: SymbolKey
    ) =
    new(name, typeParams, rhsCst, declKey) =
        AbbreviationInfo(
            name,
            typeParams,
            rhsCst,
            declKey,
            ValueNone,
            LocalSymbolKey.ofType None "" name typeParams.Length
        )

    member val Name = name
    /// Stable project-local nominal identity — the arity-qualified
    /// `TypeKey(asm, declNs, name\`arity)` minted by `stampLocalTypeKey` at registration.
    /// Abbreviations are transparent (never emitted), so this is for symmetry. The
    /// convenience constructor (synthesis / test paths with no namespace in scope)
    /// defaults to the `TypeKey(None, "", name\`arity)` placeholder.
    member val Key: SymbolKey = key
    member val TypeParams = typeParams
    member val RhsCst = rhsCst
    member val DeclKey = declKey
    /// `when 'a : ...` clause attached to the type's typar list, if any.
    /// `Unification.forceFill` walks this and attaches each constraint
    /// to the matching prototype TyVar in `TypeParams` before translating
    /// the RHS.
    member val TyparConstraints = typarConstraints
    member val Body: SemType voption = ValueNone with get, set
    member val Status: AbbreviationStatus = AbbreviationStatus.NotFilled with get, set

/// The declared `Type` is `ValueNone` for un-annotated arguments (a fresh TyVar
/// is used as the parameter's binding-site TyVar instead) and `ValueSome t` for
/// `(x: int)`-shaped annotations.
[<Sealed>]
type ClassCtorParamInfo(name: string, ty: SemType, declKey: NodeKey) =
    member val Name = name
    member val Type = ty
    member val DeclKey = declKey

/// An explicit instance field declared with `val [mutable] x: T`.
/// `Type` starts as a placeholder
/// TyVar stamped at registration and is linked by Unification's `fillClassMembers`
/// from `TypeCst` (the field is always annotated). `IsMutable` reflects the
/// `mutable` keyword — `Freeze` projects it onto `TTypeKind.Class.fields` so a
/// `this.x <- …` mutation in a member body type-checks and codegen emits a
/// writable `FieldDefinition`. `DeclKey` anchors the field's identity (and a
/// `this.x` `FieldGet`/`FieldSet` resolves against the class member walk, not
/// a binder, so it is currently informational).
[<Sealed>]
type ClassFieldInfo(name: string, ty: SemType, isMutable: bool, typeCst: Type<SyntaxToken>, declKey: NodeKey) =
    member val Name = name
    member val Type = ty
    member val IsMutable = isMutable
    member val TypeCst = typeCst
    member val DeclKey = declKey

/// A class-level `static let x = <init>`.
/// `Type` starts as a placeholder TyVar stamped by `NameResolution` and is linked
/// by Unification's `fillClassMembers` once the `Init` expression is inferred.
/// `Init` is the CST initialiser, re-read by Unification (to infer) and Freeze (to
/// translate into the synthesised `.cctor`). `DeclKey` is the binder's `NodeKey`
/// (the same one `bindingsOfPat` mints for the head pattern), so a `static let`-bound
/// name reference resolves to it and shares the placeholder TyVar.
[<Sealed>]
type ClassStaticLetInfo(name: string, ty: SemType, declKey: NodeKey, init: Expr<SyntaxToken>) =
    member val Name = name
    member val Type = ty
    member val DeclKey = declKey
    member val Init = init

/// A secondary constructor (`new(args) = SelfType(primaryArgs)`).
/// `Params` are the secondary ctor's own
/// parameters (their types start as placeholder TyVars, linked by Unification's
/// `fillClassMembers` from the annotations / chain-call unification, exactly like
/// `ClassCtorParamInfo`). `DeclKey` is a synthetic key minted from the `new`
/// token so each overload is distinct. `Body` is the CST `AdditionalConstrExpr`
/// re-read by Unification (to infer + unify the chain args against the primary
/// ctor) and Freeze (to translate the let-preamble + primary-ctor args).
[<Sealed>]
type ClassSecondaryCtorInfo
    (declKey: NodeKey, parms: ClassCtorParamInfo[], paramPat: Pat<SyntaxToken>, body: AdditionalConstrExpr<SyntaxToken>)
    =
    member val DeclKey = declKey
    member val Params = parms
    /// The `new(...)` parameter pattern, re-read by Unification to link each
    /// param's placeholder TyVar to its declared-type annotation (mirrors the
    /// primary ctor's `fillClassCtorParamTypes`).
    member val ParamPat = paramPat
    member val Body = body

[<Sealed>]
type ClassTypeInfo
    (
        name: string,
        typeParams: EqArray<string * TypeVar>,
        ctorParams: ClassCtorParamInfo[],
        members: TypeMemberInfo[],
        declKey: NodeKey,
        thisName: string,
        thisKey: NodeKey,
        baseKey: NodeKey,
        key: SymbolKey
    ) =
    member val Name = name
    /// Stable project-local nominal identity — the arity-qualified
    /// `TypeKey(asm, declNs, name\`arity)` minted by `stampLocalTypeKey` at registration
    /// to match the emitted metadata name.
    member val Key: SymbolKey = key
    member val TypeParams = typeParams
    member val CtorParams = ctorParams
    member val Members = members
    member val DeclKey = declKey
    /// `this`-binding source name (default `"this"`; honours `as self`).
    member val ThisName = thisName
    /// Synthetic NodeKey for the `this` binder shared across every
    /// member body in this class.
    member val ThisKey = thisKey
    /// Synthetic NodeKey for the `base` binder, mirroring `ThisKey`. Used by
    /// the inheritance plumbing (`base.M()` non-virtual dispatch +
    /// `inherit Base(args)` ctor lowering); always allocated, only read when
    /// `BaseType` is `ValueSome`.
    member val BaseKey = baseKey
    /// Parent type from `inherit Base(args)` once resolved. Stays `ValueNone`
    /// until the `registerInheritedSlots` walk fills it.
    /// `ValueNone` ⇒ codegen emits `TypeDefinition.BaseType = Object`.
    member val BaseType: SemType voption = ValueNone with get, set
    /// CST expression for the constructor arguments to the base type
    /// (`inherit Base(arg1, arg2)`'s `(arg1, arg2)` shape). `ValueNone` for
    /// classes without an `inherit` clause; the inheritance pass
    /// stamps it from `ClassInheritsDecl.expr`.
    member val BaseCtorArgs: Expr<SyntaxToken> voption = ValueNone with get, set
    /// `[<Sealed>]`. Stamped by
    /// `NameResolution.registerClassTypeDefn` from the type's attributes;
    /// `Freeze` projects it onto `TTypeKind.Class.isSealed` so codegen flips
    /// `TypeAttributes.Sealed` on the emitted `TypeDefinition`.
    member val IsSealed: bool = false with get, set
    /// Class-level `static let` bindings in
    /// declaration order. Stamped by `NameResolution.registerClassTypeDefn` from
    /// the class's `classPreamble`; types are linked by Unification's
    /// `fillClassMembers`; `Freeze` projects each onto a `TStaticLet`. Empty unless
    /// the class declares `static let`s. Generic classes reject `static let` (the
    /// per-instantiation cache lowering is deferred), so this is only populated for
    /// monomorphic classes.
    member val StaticLets: ClassStaticLetInfo[] = [||] with get, set
    /// Secondary constructors in declaration
    /// order. Stamped by `NameResolution.registerClassTypeDefn`; param types are
    /// linked by Unification's `fillClassMembers`; `Freeze` projects each onto a
    /// `TSecondaryCtor`. Empty unless the class declares `new(...)` overloads.
    member val SecondaryCtors: ClassSecondaryCtorInfo[] = [||] with get, set
    /// True when the class declares a *primary* constructor (`type T(args) =` /
    /// `type T() =`); false for the `val`-field form (`type T = val …; new(…) =`)
    /// whose only ctors are secondaries. Stamped by `registerClassTypeDefn` from the
    /// parsed `PrimaryConstrArgs` presence; `Freeze` projects it onto
    /// `TClassG.HasPrimaryCtor` so codegen suppresses the synthesised primary `.ctor`
    /// for the val-field form (else it collides with a parameterless `new()`).
    /// Defaults `true` so any path that doesn't stamp it keeps the prior behaviour.
    member val HasPrimaryCtor: bool = true with get, set
    /// `[<AllowNullLiteral>]`. Stamped
    /// by `NameResolution.registerClassTypeDefn` from the type's attributes;
    /// read only by Unification's `Expr.Null` arm so `null` unifies with the
    /// class. Never reaches codegen (no IL flag for it).
    member val AllowNullLiteral: bool = false with get, set
    /// `interface IFace with member …` blocks.
    /// Stamped by `NameResolution.registerClassTypeDefn`; each impl's interface
    /// type is resolved + verified, and its member bodies typed, by Unification's
    /// `fillClassMembers`. Empty unless the class declares an `interface … with`
    /// block. `Freeze` projects them onto `TTypeKind.Class.interfaces` for codegen
    /// (deferred).
    member val InterfaceImpls: ClassInterfaceImplInfo[] = [||] with get, set
    /// `when 'a : ...` / `when 'a :> IFace` clause attached to the class's typar
    /// list, if any. Stamped by `NameResolution.registerClassTypeDefn`; Unification's
    /// `fillClassMembers` walks it (under the class typar scope) and attaches each
    /// constraint to the matching prototype TyVar in `TypeParams` — so a member body's
    /// `this.field` access on an interface-constrained class typar (`'S :> IBox<'T>`)
    /// resolves through the interface (`CallVia.Interface`). Mirrors
    /// `RecordTypeInfo.TyparConstraints`. `ValueNone` for an unconstrained class.
    member val TyparConstraints: TyparConstraints<SyntaxToken> voption = ValueNone with get, set
    /// `[<Struct>]` (or the `type X = struct … end` shape).
    /// Stamped by `registerClassTypeDefn`; `Freeze`
    /// projects it onto `TTypeKind.Class.isStruct` so codegen emits a
    /// `System.ValueType`-based value type. A struct is implicitly sealed.
    member val IsValueType: bool = false with get, set
    /// A project-local *interface* declaration (`type IFoo = abstract member …` —
    /// all members abstract, no ctor / fields / inherit / `let`-preamble). Stamped
    /// by `registerClassTypeDefn` from the syntactic shape so the inference pass
    /// (`resolveInterfaceImpls`) and the subtype check recognise a local interface
    /// the external provider knows nothing about; mirrors how a referenced
    /// interface is recognised via `ExternalTypeShape.Class.IsInterface`.
    member val IsInterface: bool = false with get, set
    /// `[<IsByRefLike>]` — a byref-like (`ref struct`) value type. Stamped by
    /// `registerClassTypeDefn` (implies `IsValueType`); `Freeze` projects it onto
    /// `TTypeKind.Class.isByRefLike` so codegen stamps
    /// `System.Runtime.CompilerServices.IsByRefLikeAttribute`.
    member val IsByRefLike: bool = false with get, set
    /// Explicit `val [mutable] x: T` instance fields in declaration order.
    /// Stamped by `registerClassTypeDefn`; field
    /// types are linked by Unification's `fillClassMembers`; `Freeze` projects
    /// each onto a `TRecordField` in `TTypeKind.Class.fields`. Empty unless the
    /// class declares any `val` fields.
    member val InstanceFields: ClassFieldInfo[] = [||] with get, set
    /// Equality posture for this class. The field default is the reference-class
    /// posture (`Reference`); `NameResolution.registerClassTypeDefn` (a later
    /// phase) computes the kind-aware value — a `[<Struct>]` value type
    /// (`IsValueType = true`) defaults to `Structural`, and `[<CustomEquality>]`
    /// stamps `Custom`. `Unification`/`Freeze` read this like the
    /// record/union ones.
    member val EqualitySupport = EqualityVerdict.Reference with get, set
    /// Comparison posture for this class. The field default is also the resolved
    /// default: comparison is opt-in, so every class kind (struct included) stays
    /// `NoComparison` unless an attribute overrides it —
    /// `NameResolution.registerClassTypeDefn` (a later phase) stamps `Structural`
    /// for `[<StructuralComparison>]` and `Custom` for `[<CustomComparison>]`.
    /// `Unification`/`Freeze` read this like the record/union ones.
    member val ComparisonSupport = ComparisonVerdict.NoComparison with get, set

    interface IInterfaceImplHost with
        member this.Key = this.Key
        member this.DeclKey = this.DeclKey
        member this.TypeParams = this.TypeParams
        member this.ThisName = this.ThisName
        member this.ThisKey = this.ThisKey
        member this.InterfaceImpls = this.InterfaceImpls
        member this.Members = this.Members
        member this.EqualitySupport = this.EqualitySupport
        member this.ComparisonSupport = this.ComparisonSupport
        member this.MkSelfType args = TyClass(this.Key, args)

/// One entry in `PassContextTypes.ClassMemberIndex` — the declaring class
/// paired with the matching `TypeMemberInfo`. A record rather than a 2-tuple so
/// a third field (e.g. the interface the impl satisfies) extends cleanly.
[<Struct; NoEquality; NoComparison>]
type ClassMemberIndexEntry =
    {
        Class: ClassTypeInfo
        Member: TypeMemberInfo
    }

/// A member access on an *external* type that resolved through the provider
/// Recorded by `Unification` keyed by the
/// member-access node's `NodeKey`; `Freeze` reads it to mint a
/// `TExpr.ExternalMember` carrying the interned `SymbolKey`. `IsStatic`
/// distinguishes `Type.Member` from `value.Member` (drives whether Freeze keeps
/// the receiver), `Storage` a value member (field/property) from a method value
/// (and, at CLR emission, a `Field` from a `Property`).
///
/// `Signature` is the member's **declared** type in the receiver's instantiation
/// (`ExternalSymbols.openSignature` / `instantiateSignature`): for a method,
/// `TyFun(params → ret)`; for a property, the property type. Freeze reads it for
/// the implicit value→`obj` box decision — the call *node*'s SemType is the
/// *applied* shape with an `obj`-bound argument typar left un-grounded (the
/// obj-absorption rule), so the `obj` parameter slot is visible only on this
/// recorded declared signature, not the node.
[<Struct>]
type ResolvedExternalMember =
    {
        Key: SymbolKey
        IsStatic: bool
        Storage: MemberStorage
        Signature: SemType
        /// The resolved member's trailing optional-parameter defaults, carried
        /// forward verbatim from `ExternalMember.OptionalDefaults` so a later call
        /// site (`InferExternalCall.tryFillOptionalCall`) reads them off the resolved
        /// record instead of re-querying the provider and re-matching by `Key`. Empty
        /// for a member with no omittable optionals (the common case).
        OptionalDefaults: TConstValue list
    }

    /// A value member (field/property) vs an arrow `Method` — the predicate the
    /// optional-default gate and Freeze read; mirrors `ExternalMember.IsValueMember`.
    member m.IsValueMember = m.Storage.IsValueMember

/// A use of an operator as a value (`(+)` in `Seq.fold (+) …`), enqueued by
/// `Unification.inferIdent` for the post-walk `resolveOperatorValues` drain. The
/// node's binding to a *project-local* static-operator member is type-directed
/// (it depends on the operand types, ground only once the whole file is typed),
/// so the decision can't be made at the node and is deferred here. `Ty` is the
/// node's instantiated operator type at the use site; the drain zonks it and
/// scans every operand for a declaring nominal.
[<Struct>]
type OperatorValueSite =
    {
        /// The operator-value node's `NodeKey` — the drain's output key into
        /// `ResolvedOperatorValue`, and Freeze's lookup key.
        Node: NodeKey
        /// The operator's compiled name (`op_Addition`).
        Name: string
        /// The node's instantiated operator type (a curried function), zonked at
        /// drain time to read the now-ground operand types.
        Ty: SemType
    }

// `ForInEnumerator` moved to `SideTypes.fs` (it must precede `Tast.fs`).

[<Sealed>]
type SideTable<'V>() =
    let dict = Dictionary<NodeKey, 'V>(HashIdentity.Structural)

    member _.Count = dict.Count

    member _.TryGetValue(key: NodeKey) =
        match dict.TryGetValue(key) with
        | true, v -> ValueSome v
        | false, _ -> ValueNone

    member _.Set(key: NodeKey, value: 'V) = dict[key] <- value

    member _.Remove(key: NodeKey) = dict.Remove key |> ignore

    member _.ContainsKey(key: NodeKey) = dict.ContainsKey key

    /// Callers must treat the returned dictionary as read-only once Freeze starts.
    member _.AsDictionary() : IReadOnlyDictionary<NodeKey, 'V> = dict :> _

/// Type-definition side tables: the project-wide registry of records, unions,
/// classes, and abbreviations plus their reverse / member indexes. Populated by
/// `NameResolution.registerXxx`, filled in by `Unification`, read everywhere
/// downstream.
type PassContextTypes =
    {
        /// Field types are filled in by Unification after the registry is populated.
        Record: Dictionary<string, RecordTypeInfo>
        /// Case field types are filled in by Unification after the registry is populated.
        Union: Dictionary<string, UnionTypeInfo>
        /// Member types start as placeholder TyVars and get linked by Unification's
        /// `fillClassMembers` pre-pass.
        Class: Dictionary<string, ClassTypeInfo>
        /// Project-local enum type declarations, keyed by bare short name (enums
        /// are non-generic, so no arity overload). Populated by
        /// `NameResolution.registerEnumTypeDefn`; read by `translateType` (so
        /// `(x: E)` resolves to `TyEnum Key`) and the `E.C1` qualified-access path.
        Enum: Dictionary<string, EnumTypeInfo>
        /// Bodies are filled in by Unification's `fillAbbreviationBodies` pre-pass.
        /// Abbreviations expand eagerly at every `translateType` lookup, so
        /// downstream passes see the underlying type as if written longhand.
        Abbreviation: Dictionary<string, AbbreviationInfo>
        /// Reverse index: ctor name → bucket of case-info entries (each tagged with
        /// the declaring union type). Consumers iterate the bucket; order does not
        /// matter, so registration appends with `EqArray.ofResizeArray`.
        CtorIndex: Dictionary<string, EqArray<UnionCaseInfo>>
        /// Reverse index: field name → bucket of record types that declare it.
        FieldIndex: Dictionary<string, EqArray<RecordTypeInfo>>
        /// Reverse index: member name → bucket of declaring (class, member) entries.
        /// Used only for ambiguity diagnostics when a receiver's type is free and
        /// the member name occurs in multiple classes.
        ClassMemberIndex: Dictionary<string, EqArray<ClassMemberIndexEntry>>
        /// Maps the Vesper type name to its target representation (the inline-IL
        /// string), from an intrinsic-binding abbrev (`type int = (# "System.Int32" #)`).
        /// Unlike `Abbreviation`, these are NOT transparent: a use site resolves to
        /// `TyConst name`, not the RHS — the binding records *how the target
        /// represents* the type, not an alias to expand. Input to the
        /// `encodeType` rekey.
        IntrinsicReprTypes: Dictionary<string, string>
        /// The name → qualified `SymbolKey` index for this unit's own intrinsics,
        /// populated at registration from the declaring `namespace` (`Vesper`). The
        /// intrinsic's identity is CONTRACT-SOURCED: `Translate` reads the resolved key
        /// here instead of re-deriving the namespace from a hardcoded name set. The key
        /// keeps the VERBATIM intrinsic name (`"int"`, `"[]"`) with NO arity suffix — the
        /// name field IS the identity string, arity rides in the `TyConst` args. Mirror of
        /// the arity-suffixed `SymbolKey` a record/union stamps, minus the suffix.
        /// `IntrinsicReprTypes` stays a pure name → target-repr side-table; this carries
        /// identity.
        IntrinsicKeys: Dictionary<string, SymbolKey>
        /// Names of intrinsic-repr types declared as HERITABLE external reference
        /// bases (`type Attribute = (# class "System.Attribute" #)`), the `class`/
        /// `interface`-tagged subset of `IntrinsicReprTypes`. A name here may appear as
        /// an `inherit` parent: `resolveInheritParent` resolves it to the EXTERNAL type
        /// its repr names (`System.Attribute`), so codegen emits `extends` + a base-ctor
        /// call instead of treating it as an opaque (sealed, unencodable-as-base) value
        /// repr. The repr string itself stays in `IntrinsicReprTypes`.
        HeritableExternBases: HashSet<string>
        /// Host side-tables for inline intrinsic-abbrevs carrying `with member …`
        /// augmentations (`type widget = (# "object" #) with member …`), keyed by bare
        /// short name (abbrevs aren't arity-overloaded). Populated by
        /// `NameResolution.registerAbbreviationDefn` ONLY when the abbrev's RHS is
        /// `Type.ILIntrinsic` and it carries extensions; a transparent-alias abbrev with
        /// members is rejected there and never lands here. The type itself stays in
        /// `IntrinsicReprTypes` (identity preserved); this table only carries the members.
        /// Resolved as an `IInterfaceImplHost` by `tryNonClassMemberHost`, so the member
        /// bodies name-resolve, type, and elaborate on the shared host path.
        IntrinsicAbbrevHost: Dictionary<string, IntrinsicAbbrevInfo>
        /// Bookkeeping for the bare-name alias `Union` keeps for arity-overloaded
        /// unions (`Choice\`2`…`Choice\`7`). `Union` is keyed by `TypeRegistry.keyFor`
        /// (bare name for a non-generic union, ``name`N`` for arity N>0); a *single*
        /// generic arity of a name additionally registers a bare-name alias, so a
        /// generic union written without args (or read by bare name in legacy paths)
        /// still resolves exactly as before. This maps the bare short name → the arity
        /// of its current alias, or `-1` once a second arity collides and the alias is
        /// withdrawn (the name is then only resolvable by its arity-key). Internal to
        /// `TypeRegistry.registerUnion`; not read elsewhere.
        UnionBareArity: Dictionary<string, int>
        /// Bookkeeping for the bare-name alias `Class` keeps for arity-overloaded
        /// classes / interfaces (`Fun\`2` vs `Fun\`3`). Mirrors `UnionBareArity`
        /// exactly: `Class` is keyed by `TypeRegistry.keyFor` (bare name for a
        /// non-generic class, ``name`N`` for arity N>0); a *single* generic arity
        /// of a name additionally registers a bare-name alias so every existing
        /// single-arity class read by bare name still resolves. Maps the bare short
        /// name → the arity of its current alias, or `-1` once a second arity
        /// collides and the alias is withdrawn (the name is then only resolvable by
        /// its arity-key). Internal to `TypeRegistry.registerClass`.
        ClassBareArity: Dictionary<string, int>
        /// Bookkeeping for the bare-name alias `Record` keeps for arity-overloaded
        /// records (`Point\`2` vs `Point\`3`). Mirrors `UnionBareArity` /
        /// `ClassBareArity` exactly. Internal to `TypeRegistry.registerRecord`.
        RecordBareArity: Dictionary<string, int>
        /// Uniqueness witness for project-local `SymbolKey`s.
        /// Maps each minted `TypeKey(None, ns, name\`arity)` → the decl-site
        /// `NodeKey` that first minted it. Stamped through `TypeRegistry.recordKeyOrigin`
        /// as each type registers; a second *distinct* declaration minting the same key
        /// is a uniqueness violation (a missing/҂wrong `ns` in the mint, not a user
        /// duplicate, which is caught earlier and never reaches the stamp). The gate
        /// that proves a local `SymbolKey` is unique enough to become the TAST identity.
        SymbolKeyOrigins: Dictionary<SymbolKey, NodeKey>
    }

module PassContextTypes =
    let empty () : PassContextTypes =
        {
            Record = Dictionary<_, _>()
            Union = Dictionary<_, _>()
            Class = Dictionary<_, _>()
            Enum = Dictionary<_, _>()
            Abbreviation = Dictionary<_, _>()
            CtorIndex = Dictionary<_, _>()
            FieldIndex = Dictionary<_, _>()
            ClassMemberIndex = Dictionary<_, _>()
            IntrinsicReprTypes = Dictionary<_, _>()
            IntrinsicKeys = Dictionary<_, _>()
            HeritableExternBases = HashSet<_>()
            IntrinsicAbbrevHost = Dictionary<_, _>()
            UnionBareArity = Dictionary<_, _>()
            ClassBareArity = Dictionary<_, _>()
            RecordBareArity = Dictionary<_, _>()
            SymbolKeyOrigins = Dictionary<_, _>()
        }

/// Arity-aware access to the project-local *union* registry. F# (and .NET) let a
/// type name be overloaded by generic arity — `Choice<'T1,'T2>` and
/// `Choice<'T1,'T2,'T3>` are distinct types `Choice\`2` / `Choice\`3`. The bare
/// `Dictionary<string, _>` keys would collapse them onto `"Choice"`, so unions are
/// keyed by `keyFor name arity`. A *single*-arity name also keeps a bare-name alias
/// (so every existing single-arity lookup by bare name keeps working unchanged);
/// the alias is withdrawn once a second arity registers (`UnionBareArity`). Records,
/// unions, and classes are all arity-keyed this way; only abbreviations stay bare.
module TypeRegistry =

    /// The .NET-style key: the bare name for a non-generic type, ``name`N`` for
    /// arity N>0. Matches the emitted metadata type name. Delegates to
    /// `SymbolKeyOps.arityName` so the registry key and the stamped `SymbolKey`
    /// name share one rule.
    let keyFor (name: string) (arity: int) : string = SymbolKeyOps.arityName name arity

    /// The contract-sourced identity key for a locally-declared intrinsic (`int`,
    /// `[]`, a user intrinsic-abbrev): the qualified `SymbolKey` registration stamped
    /// from the declaring `namespace` (`IntrinsicKeys`), so the namespace comes from the
    /// contract rather than a hardcoded name set. The single source EVERY local-intrinsic
    /// mint routes through, so a use-site (`Translate`) and a member/self-type mint
    /// (`MemberRegistration`) of the same intrinsic cannot diverge on the namespace. Falls
    /// back to the by-name mint only for a name in `IntrinsicReprTypes` without a stamped
    /// key (defensive — registration populates both together).
    let intrinsicKeyOf (types: PassContextTypes) (name: string) : SymbolKey =
        match types.IntrinsicKeys.TryGetValue name with
        | true, k -> k
        | _ -> RuntimeNames.primitiveKey name

    // --- Arity-overload mechanism (shared by Union and Class) --------------------
    // F# / .NET overload a type *name* by generic arity (`Choice\`2`/`Choice\`3`,
    // `Fun\`2`/`Fun\`3`). Such a kind is keyed by `keyFor name arity`; a *single*
    // registered arity also keeps a bare-name alias so every legacy bare-name read
    // still resolves, and the alias is withdrawn (sentinel `-1`) once a second arity
    // collides. `table` + its `bareArity` companion are the only things that differ
    // between the union and class registries — so the mechanism lives here once.

    /// Register `info` under its arity-key, maintaining (or withdrawing) the
    /// bare-name alias. The `-1` sentinel in `bareArity` marks a demoted name that
    /// is resolvable only by its arity-key. Idempotent for a repeat `(name, arity)`.
    let private registerArityKeyed
        (table: Dictionary<string, 'T>)
        (bareArity: Dictionary<string, int>)
        (name: string)
        (arity: int)
        (info: 'T)
        : unit =
        table.[keyFor name arity] <- info

        if arity > 0 then
            match bareArity.TryGetValue name with
            | false, _ ->
                table.[name] <- info
                bareArity.[name] <- arity
            | true, a when a = arity -> table.[name] <- info // refresh the same-arity alias
            | true, -1 -> () // already demoted: only the arity-key resolves
            | true, _ ->
                // A second distinct arity for this short name: withdraw the now-
                // ambiguous bare alias; both arities resolve only by their key.
                table.Remove name |> ignore
                bareArity.[name] <- -1

    /// Resolve an arity-overloaded type by its project-local `SymbolKey`. The key's
    /// `TypeKey` `name` component *is* the registry key (both route through
    /// `SymbolKeyOps.arityName`), so this reads it VERBATIM — no `simpleName` strip.
    /// A non-`TypeKey` key never names such a type, so it misses.
    let private tryByTypeKey (table: Dictionary<string, 'T>) (key: SymbolKey) : 'T voption =
        match key with
        | SymbolKey.TypeKey(name = name) ->
            match table.TryGetValue name with
            | true, info -> ValueSome info
            | false, _ -> ValueNone
        | _ -> ValueNone

    // --- Records / classes --------------------------------------
    // Records, unions, and classes are all arity-overloadable in F# (`Point\`2`/
    // `Point\`3`, `Choice\`2`/`Choice\`3`, `Fun\`2`/`Fun\`3`), so each is keyed by
    // `keyFor name arity` with a bare-name alias (`registerArityKeyed`). Only
    // abbreviations stay bare-keyed (not arity-overloaded today).

    /// Register a record under its arity-key, keeping the bare-name alias while the
    /// short name is single-arity (`RecordBareArity`). See `registerArityKeyed`.
    let registerRecord (types: PassContextTypes) (name: string) (arity: int) (info: RecordTypeInfo) : unit =
        registerArityKeyed types.Record types.RecordBareArity name arity info

    /// True iff a record with this exact `(name, arity)` is registered (the arity-key,
    /// never the bare alias) — the duplicate-definition test (mirror `containsClass`).
    let containsRecord (types: PassContextTypes) (name: string) (arity: int) : bool =
        types.Record.ContainsKey(keyFor name arity)

    /// Resolve a record by bare short name. Single-arity records keep a bare-name
    /// alias (`registerRecord`); a name with two registered arities has its alias
    /// withdrawn and misses here (callers holding an arity-qualified key use
    /// `tryRecordByKey`; those with a use-site arity use `tryRecordArity`). The
    /// recognition-only call sites (module-vs-type tests, qualified heads) ride this alias.
    let tryRecord (types: PassContextTypes) (name: string) : RecordTypeInfo voption =
        match types.Record.TryGetValue name with
        | true, info -> ValueSome info
        | false, _ -> ValueNone

    /// Resolve a record by `(name, arity)` — exact arity-key only, so a wrong arity
    /// misses. Does NOT fall back to the bare alias (mirror `tryClassArity`/`tryUnion`).
    let tryRecordArity (types: PassContextTypes) (name: string) (arity: int) : RecordTypeInfo voption =
        match types.Record.TryGetValue(keyFor name arity) with
        | true, info -> ValueSome info
        | false, _ -> ValueNone

    /// Resolve a record by its project-local `SymbolKey` — the reader-side companion
    /// to `tryUnionByKey`/`tryClassByKey`. Reads the key's arity-qualified name
    /// VERBATIM (via `tryByTypeKey`), so a consumer holding a `TyRecord(key, _)`
    /// resolves the exact arity without a `simpleName` strip.
    let tryRecordByKey (types: PassContextTypes) (key: SymbolKey) : RecordTypeInfo voption =
        tryByTypeKey types.Record key

    /// Register a class under its arity-key, keeping the bare-name alias while the
    /// short name is single-arity (`ClassBareArity`). See `registerArityKeyed`.
    let registerClass (types: PassContextTypes) (name: string) (arity: int) (info: ClassTypeInfo) : unit =
        registerArityKeyed types.Class types.ClassBareArity name arity info

    /// True iff a class with this exact `(name, arity)` is registered (the arity-
    /// key, never the bare alias) — the duplicate-definition test (mirror
    /// `containsUnion`).
    let containsClass (types: PassContextTypes) (name: string) (arity: int) : bool =
        types.Class.ContainsKey(keyFor name arity)

    /// Resolve a class by bare short name. Single-arity classes keep a bare-name
    /// alias (`registerClass`), so this resolves them; a name with two registered
    /// arities has its alias withdrawn and misses here (callers holding an
    /// arity-qualified key use `tryClassByKey`). The recognition-only call sites
    /// (`Scope.fs`, `NameResolution.fs`, qualified-static heads) ride this alias.
    let tryClass (types: PassContextTypes) (name: string) : ClassTypeInfo voption =
        match types.Class.TryGetValue name with
        | true, info -> ValueSome info
        | false, _ -> ValueNone

    /// Resolve a class by `(name, arity)` — exact arity-key only, so a wrong arity
    /// misses. Does NOT fall back to the bare alias (mirror `tryUnion`).
    let tryClassArity (types: PassContextTypes) (name: string) (arity: int) : ClassTypeInfo voption =
        match types.Class.TryGetValue(keyFor name arity) with
        | true, info -> ValueSome info
        | false, _ -> ValueNone

    /// Resolve a class by its project-local `SymbolKey` — the class analogue of
    /// `tryUnionByKey`. See `tryByTypeKey`.
    let tryClassByKey (types: PassContextTypes) (key: SymbolKey) : ClassTypeInfo voption = tryByTypeKey types.Class key

    /// True iff a class is registered under this `SymbolKey`'s arity-qualified name —
    /// the key-based membership gate (the `tryClassByKey`-shaped mirror of
    /// `containsClass`). A caller holding a `TyClass` key uses this so an
    /// arity-overloaded class (whose bare alias is withdrawn) isn't misclassified as
    /// external.
    let containsClassKey (types: PassContextTypes) (key: SymbolKey) : bool = (tryByTypeKey types.Class key).IsSome

    /// Resolve a nominal type that may carry `interface … with` impls — a class,
    /// union, *or* record — by its arity-qualified `SymbolKey` (via `tryByTypeKey`,
    /// which reads the key's ``name`arity`` verbatim), surfaced as the shared
    /// `IInterfaceImplHost`. The `subsumes` interface-admission walk uses this so a
    /// union's/record's declared interfaces participate in subtyping exactly like a
    /// class's. Class → union → record, so a class wins a name collision (as the
    /// bare-alias reads always have). Key-based (not bare short name): an
    /// arity-overloaded host (`Foo`2`/`Foo`3`) has its bare alias withdrawn, so a
    /// bare lookup would miss the local host and mis-classify the type as external.
    let tryInterfaceImplHostByKey (types: PassContextTypes) (key: SymbolKey) : IInterfaceImplHost voption =
        match tryByTypeKey types.Class key with
        | ValueSome info -> ValueSome(info :> IInterfaceImplHost)
        | ValueNone ->
            match tryByTypeKey types.Union key with
            | ValueSome info -> ValueSome(info :> IInterfaceImplHost)
            | ValueNone ->
                match tryByTypeKey types.Record key with
                | ValueSome info -> ValueSome(info :> IInterfaceImplHost)
                | ValueNone -> ValueNone

    /// Resolve a union, record, or inline intrinsic-abbrev host (NOT a class) by bare
    /// short name as the shared `IInterfaceImplHost`. The non-class analogue of the
    /// `tryClassLikeDecl` path: the host-body passes route their
    /// `TypeDefnPatterns.tryNonClassMemberHostDecl` match through this one lookup, so they
    /// share one bare-name convention instead of skewing against the arity-key. (The
    /// subtype walk resolves the same hosts by `SymbolKey` via `tryInterfaceImplHostByKey`.)
    /// The intrinsic-abbrev host is admitted so `type X = (# … #) with member …` fills
    /// and elaborates its members on the SAME `fillHostMembers` path — its `MkSelfType`
    /// yields the abbrev's `TyConst` identity, so the members' self-type stays intrinsic.
    /// Classes are excluded — they fill and resolve through their own richer path
    /// (`fillClassMembers` / `walkClassBodies`), so admitting one here would double-fill.
    let tryNonClassMemberHost (types: PassContextTypes) (name: string) : IInterfaceImplHost voption =
        match types.Union.TryGetValue name with
        | true, info -> ValueSome(info :> IInterfaceImplHost)
        | false, _ ->
            match types.Record.TryGetValue name with
            | true, info -> ValueSome(info :> IInterfaceImplHost)
            | false, _ ->
                match types.IntrinsicAbbrevHost.TryGetValue name with
                | true, info -> ValueSome(info :> IInterfaceImplHost)
                | false, _ -> ValueNone

    /// Register an enum under its bare short name (enums are non-generic, so no
    /// arity overload — mirrors records, not unions).
    let registerEnum (types: PassContextTypes) (name: string) (info: EnumTypeInfo) : unit = types.Enum.[name] <- info

    /// True iff an enum with this name is registered — the enum half of the
    /// duplicate-definition test.
    let containsEnum (types: PassContextTypes) (name: string) : bool = types.Enum.ContainsKey name

    /// Resolve an enum by bare short name; `ValueNone` if none. Used by
    /// `translateType` (`(x: E)` → `TyEnum`) and the `E.C1` qualified-access path.
    let tryEnum (types: PassContextTypes) (name: string) : EnumTypeInfo voption =
        match types.Enum.TryGetValue name with
        | true, info -> ValueSome info
        | false, _ -> ValueNone

    let registerAbbrev (types: PassContextTypes) (name: string) (info: AbbreviationInfo) : unit =
        types.Abbreviation.[name] <- info

    let containsAbbrev (types: PassContextTypes) (name: string) : bool = types.Abbreviation.ContainsKey name

    let tryAbbrev (types: PassContextTypes) (name: string) : AbbreviationInfo voption =
        match types.Abbreviation.TryGetValue name with
        | true, info -> ValueSome info
        | false, _ -> ValueNone

    /// Register a union under its arity-key, keeping the bare-name alias while the
    /// short name is single-arity (`UnionBareArity`). See `registerArityKeyed`.
    let registerUnion (types: PassContextTypes) (name: string) (arity: int) (info: UnionTypeInfo) : unit =
        registerArityKeyed types.Union types.UnionBareArity name arity info

    /// True iff a union with this exact `(name, arity)` is registered (the arity-
    /// key, never the bare alias) — the duplicate-definition test.
    let containsUnion (types: PassContextTypes) (name: string) (arity: int) : bool =
        types.Union.ContainsKey(keyFor name arity)

    /// Resolve a union by `(name, arity)` — exact arity-key only, so a wrong arity
    /// misses (the caller diagnoses). Does NOT fall back to the bare alias.
    let tryUnion (types: PassContextTypes) (name: string) (arity: int) : UnionTypeInfo voption =
        match types.Union.TryGetValue(keyFor name arity) with
        | true, info -> ValueSome info
        | false, _ -> ValueNone

    /// Resolve a union by its project-local `SymbolKey` — the arity-qualified
    /// `TypeKey(None, _, name\`arity)` minted onto `UnionTypeInfo.Key` and stamped
    /// into `Resolution.ResolvedType`. The key's
    /// `name` component *is* the registry key: both it and `keyFor` use the one
    /// `SymbolKeyOps.arityName` rule, so this is a direct `Union` lookup with
    /// no arity re-derivation. A non-`TypeKey` key (a value / member) never names a
    /// union, so it misses. The reader-side seam for SymbolKey-first resolution.
    let tryUnionByKey (types: PassContextTypes) (key: SymbolKey) : UnionTypeInfo voption = tryByTypeKey types.Union key

    /// The declaring union of a registered case, resolved by its `(UnionName,
    /// UnionArity)`. The case came from a registered union, so this is total in
    /// practice; falls back to the bare alias defensively.
    let unionOfCase (types: PassContextTypes) (info: UnionCaseInfo) : UnionTypeInfo =
        match tryUnion types info.UnionName info.UnionArity with
        | ValueSome u -> u
        | ValueNone -> types.Union.[info.UnionName]

    /// Is `qualifier.caseName` a *local* union-case reference — the case is
    /// registered and one of its declaring unions has the short name `qualifier`?
    /// Case names are globally unique (even across `Choice\`2`…`Choice\`7`), so this
    /// is the arity-safe replacement for `Union.ContainsKey qualifier` + "has case"
    /// when recognising a qualified union-case (`Choice.Choice1Of3`).
    let localQualifiedCase (types: PassContextTypes) (qualifier: string) (caseName: string) : bool =
        match types.CtorIndex.TryGetValue caseName with
        | true, infos -> infos |> EqArray.exists (fun c -> c.UnionName = qualifier)
        | false, _ -> false

    /// Record the decl-site origin of a freshly-minted project-local `SymbolKey` and
    /// report a uniqueness violation. Returns the
    /// *prior* declaration's `NodeKey` when `key` was already minted by a **different**
    /// declaration — the caller turns that into a diagnostic. `ValueNone` on the first
    /// mint or an idempotent re-stamp of the same decl. A collision here means two
    /// distinct types collapsed onto one key, i.e. the mint dropped a distinguishing
    /// `ns` — never a user duplicate (those are rejected before the stamp). When the
    /// corpus registers clean, the local key is unique enough to carry TAST identity.
    let recordKeyOrigin (types: PassContextTypes) (declKey: NodeKey) (key: SymbolKey) : NodeKey voption =
        match types.SymbolKeyOrigins.TryGetValue key with
        | true, prior when prior <> declKey -> ValueSome prior
        | true, _ -> ValueNone
        | false, _ ->
            types.SymbolKeyOrigins.[key] <- declKey
            ValueNone

/// Per-binding side tables: the resolved binder / inferred scheme / TyVar
/// graph / escape-classification entries indexed by `NodeKey`, plus the
/// `module Foo = …` holder map that `Freeze` snapshots into the TAST.
type PassContextBindings =
    {
        Binding: SideTable<ResolvedBinding>
        /// Keyed by the binding's headPat NodeKey (which is also the `BindingSite`
        /// NameResolution records). Present only for `let`-bound names that pass
        /// `shouldGeneralise` — module-level, nested, and `let rec` single-name
        /// bindings. Compound destructuring heads and lambda parameters do NOT get
        /// schemes.
        Scheme: SideTable<TypeScheme>
        TypeVar: SideTable<TypeVar>
        Escape: SideTable<EscapeState>
        /// Axis-2 representation verdict per region (a `RegionRepr`), keyed by the
        /// same binder / anon `NodeKey` as `Escape`. Populated by `Regions.run`
        /// from the second (representation) fixpoint; folded with `Escape` into the
        /// per-closure `ClosureRepr` verdict. Orthogonal to `Escape` (lifetime): a
        /// frame-local closure held in an aggregate is `LocalStack` here yet
        /// `RequiresHeapRepr` there.
        Repr: SideTable<RegionRepr>
        /// Module-level bindings inside a named `module Foo = …`: each
        /// binding's `NodeKey` → where its emitted static method belongs (a real
        /// `Foo`/`FooModule` holder type, not the anonymous "Program" holder).
        /// Populated by `Freeze` and snapshotted into `TastFile.ModuleMembers`; the
        /// backend keys off it to name + place a module function (`ListModule::fold`).
        ModuleMembers: Dictionary<NodeKey, ModuleMemberInfo>
        /// A *top-level* (implicit-"Program"-module, `holder = None`) binding's
        /// `NodeKey` → its source name. Top-level bindings record no
        /// `ModuleMemberInfo`, so this is the only name source for a top-level value
        /// lowered to a Program-holder static field. Consulted only by the value
        /// collector, so top-level functions keep their `fn$<off>` holderless path.
        TopLevelNames: Dictionary<NodeKey, string>
        /// A `let` binding's explicitly-declared `<'b,'a>` typars, in SOURCE order,
        /// each paired with the `TypeVar` inference seeded for it. Captured by
        /// `Infer.inferBinding` while the binding's transient `TyparScope` is live
        /// (it's restored per binding, so it's gone by Elaborate). Keyed by the
        /// binding's headPat NodeKey. Elaborate's free-function method-typar minter
        /// reads this to order method typars declared-first (the F# rule); absent
        /// when the binding declared no typars.
        DeclaredTypars: SideTable<(string * TypeVar) list>
    }

module PassContextBindings =
    let empty () : PassContextBindings =
        {
            Binding = SideTable<_>()
            Scheme = SideTable<_>()
            TypeVar = SideTable<_>()
            Escape = SideTable<_>()
            Repr = SideTable<_>()
            ModuleMembers = Dictionary<_, _>()
            TopLevelNames = Dictionary<_, _>()
            DeclaredTypars = SideTable<_>()
        }

/// Name-resolution scopes: the `open` / typar / external-access state the passes
/// thread per module element. `OpenScope` / `AmbientOpenScope` / `TyparScope` /
/// `TyparScopeStrict` mutate per-element; `ExternalAccess` accumulates resolved
/// external member-access hits keyed by `NodeKey`.
type PassContextResolution =
    {
        /// The `open` / auto-open namespace prefixes active at the module element
        /// currently being analysed. Set per top-level element by the pass walk
        /// (from `CstWalk.walkModuleTree`), then read by the provider-probe sites
        /// (`tryQualify`) so a short name resolves against the opens in scope.
        /// Constant inside any one expression (`open` is a declaration-level node).
        /// Seeded to the provider's ambient prelude (below) so a pass that reads it before the
        /// walk sets a per-element scope still sees the auto-opens.
        mutable OpenScope: OpenScope
        /// The *stable* ambient prelude each pass seeds its `walkModuleTree` from —
        /// distinct from the mutable `OpenScope` (which a pass overwrites per
        /// element). Seeded from the provider's `AmbientOpenPrefixes` (the
        /// referenced-contract `[<AutoOpen>]` modules / prelude), empty when the
        /// provider surfaces none. Both NameResolution and Unification must read
        /// the *same* seed, so it can't be the per-element `OpenScope` they mutate.
        mutable AmbientOpenScope: OpenScope
        /// Per-signature type-parameter scope: each signature opens its own scope
        /// and restores the prior one on exit. Anonymous typars (`_`) never enter
        /// the scope — they're fresh per occurrence.
        mutable TyparScope: Dictionary<string, TypeVar>
        /// Prototype TyVars (keyed by source name) for the *next* binding's own
        /// `<'C, …>` typars. `inferBinding` mints a fresh scope for a binding's
        /// declared typars; when this seed is set it reuses the prototype TyVar
        /// for a matching name instead of allocating a fresh one. `fillTypeMembers`
        /// sets it from a generic member's `TypeMemberInfo.MethodTypeParams`
        /// so the typars flowing into the inferred signature are the same
        /// roots `Freeze` surfaces and codegen installs as the ambient `!!i` set;
        /// `ValueNone` for every other binding (fresh typars, the existing
        /// behaviour).
        mutable BindingTyparSeed: Dictionary<string, TypeVar> voption
        /// The enclosing type's type-parameter scope (class / union typars), kept
        /// in scope across a member-body walk. `inferBinding` mints a *fresh* scope
        /// per binding (so sibling bindings' `'a`s stay distinct); without this it
        /// would drop the class typars that `fillTypeMembers` put in scope, so a
        /// generic member's *signature* annotation (`(x: 'T)`, `: Set<'T>`) would
        /// find an empty scope and — under `TyparScopeStrict` — diagnose "Free type
        /// parameter 'T". When set, `inferBinding` seeds its fresh scope with these
        /// typars first (the binding's own `<'a>` typars seed after, shadowing on a
        /// name clash). `fillTypeMembers` / `fillSecondaryCtors` set it; `ValueNone`
        /// for every non-member binding (the existing behaviour). Persists across
        /// nested `let`s in a member body so they too see the class typars.
        mutable EnclosingTypars: Dictionary<string, TypeVar> voption
        /// When true, `translateType` rejects any `'a` not already present in
        /// `TyparScope` rather than introducing it implicitly. Used by the type-defn
        /// fill-in walk: implicit free typars in a record / DU declaration aren't
        /// legal F# (only `<'a>`-declared typars are). Binding-level scopes keep
        /// this `false`.
        mutable TyparScopeStrict: bool
        /// Keyed by a member-access node's `NodeKey` (`Expr.DotLookup`): the resolved
        /// external member (`TryLookupMember` hit) for a `<externalType>.Member` or
        /// static `Type.Member` access. Freeze reads it to mint a `TExpr.ExternalMember`
        /// stamping the resolved `SymbolKey`. Absent
        /// for project-local member access (resolved via `Types.Class` / `Types.Union`).
        ExternalAccess: SideTable<ResolvedExternalMember>
        /// Keyed by an external *method-call head*'s `NodeKey` (the same key
        /// `ExternalAccess` stores the resolved member under): the compile-time
        /// constant defaults of the trailing optional parameters this call *omitted*,
        /// in declaration order. Recorded by `Unification`'s optional-argument fill
        /// (`InferExternalCall.tryFillOptionalCall`) when a call supplies fewer
        /// arguments than the member's parameter count, relying on the member's
        /// `ExternalMember.OptionalDefaults`; read by `Freeze.translateApp`, which
        /// synthesises them as literal arguments so codegen sees the full tupled call
        /// (`ArrayPool<'T>.Return(arr)` ⇒ `Return(arr, false)`). Absent ⇒ a fully
        /// applied call (the common case), emitted unchanged.
        ExternalOptionalFill: SideTable<TConstValue list>
        /// Keyed by a member-access node's `NodeKey` (the folded `LongIdent` /
        /// `DotLookup` head of `x.M(...)`): the constraining *interface*'s
        /// `SymbolKey.TypeKey` when the receiver's type is a generic typar coerced
        /// to a project-local interface (`'T :> IFace`). Recorded by
        /// `Unification.resolveFieldStep`'s typar arm when it resolves the member
        /// through the typar's `Coercion` constraint, and read by `Freeze` to mint a
        /// `TExpr.MethodCall` with `CallVia.Interface` (the declaring type is the
        /// interface; codegen emits `constrained. <typar> callvirt`). Absent ⇒ an
        /// ordinary nominal-receiver member access. The paired `SemType list` is the
        /// interface's instantiation type arguments (`'E` in `'T :> IStructSeq<'E>`),
        /// taken from the `Coercion` constraint's target so Freeze can thread them
        /// onto `CallVia.Interface` and codegen mint the slot on the *instantiated*
        /// interface `TypeSpec`; empty for a non-generic interface.
        TyparInterfaceCall: SideTable<SymbolKey * EqArray<SemType>>
        /// Keyed by an external-value use-site's `NodeKey` (the `Expr.Ident` /
        /// `Expr.LongIdentOrOp` that resolved through `IExternalSymbolProvider.TryLookup`):
        /// the resolved value's `SymbolKey.ValueKey`. Freeze stamps it onto
        /// `TExpr.External` so codegen can do robust identity checks
        /// (e.g. "is this exactly `Vesper.Printf.printfn`?") instead of
        /// suffix-matching the source-written name.
        ExternalValue: SideTable<SymbolKey>
        /// Keyed by an external union-case ctor head's `NodeKey` — a *pattern* head
        /// (`CstKeys.ofPat`, the `Some x` / `Result.Ok x` of a `match` / binder) or an
        /// *expression* head (`CstKeys.ofExpr`, a bare `None` / qualified `Option.Some`
        /// used as a value / ctor function): the `ExternalUnionCase` NameResolution
        /// resolved that head to. NameResolution owns case recognition — it applies the
        /// opens / RQA / qualifier discipline (`ExternalUnionCase.ResolvesWith`) once and
        /// stamps the resolved identity here; Unification's `InferPat` / `InferIdentExpr`
        /// and Freeze's `translatePat` / `tryCtorRef` READ this stamp instead of handing
        /// raw source spelling back to the resolver-face `TryLookupUnionCase(string)`.
        /// Absent ⇒ the head is not an external union case (a binder, a local ctor, or a
        /// bare reference to an `[<RequireQualifiedAccess>]` case, which resolves only
        /// qualified). A missed stamp where a consumer reads is a phantom binder /
        /// mis-lowering, so the pattern-stamping walk must reach every pattern position.
        ///
        /// The `ExternalUnionCase` payload itself is stamped (not a `(union key, case
        /// name)` pair): a consumer must recover the declaring union's key AND the
        /// matched case's per-field type builders to instantiate `TyUnion(union,
        /// freshArgs)` and unify sub-patterns. Recovering field types from the union key
        /// alone would need a second, key-addressed `TryLookupType(union key)` → select
        /// case by name — but a provider that publishes only the reverse case index
        /// (several test fakes, and any minimal contract) answers `TryLookupUnionCase`
        /// yet returns `ValueNone` for the forward key lookup, so that round-trip would
        /// both change behaviour and break green. Carrying the resolved payload keeps
        /// identity resolved ONCE upstream while reproducing the previous recognisers
        /// exactly with no downstream provider call (key-semantics §4 permits this).
        ExternalUnionCaseStamp: SideTable<ExternalUnionCase>
        /// Keyed by the `NodeKey` of an expression Freeze lowers to a desugared
        /// `TExpr.External(<intrinsicName>, …)` head that splices a cross-package
        /// `let inline` body: an arithmetic/comparison/custom operator
        /// (`InfixApp`/`PrefixApp`), the dynamic-access operators
        /// (`op_Dynamic` on a `DynamicLookup`, `op_DynamicAssignment` on the
        /// enclosing `Assignment`), or a synthesised element/index/length intrinsic
        /// (`GetArray`/`GetString`/`GetIndex` on an `IndexedLookup`,
        /// `SetArray`/`SetIndex` on the enclosing `Assignment`, `GetArrayLength` on
        /// the `.Length` `DotLookup` / `LongIdent` chain). Unification resolves the
        /// intrinsic's `ExternalSymbol` through the provider while typing the node
        /// (the same `OpenScope.tryResolve` that grounds the call) and records its
        /// `SymbolKey` here; Freeze stamps it onto the minted `TExpr.External` so
        /// `InlineExpansion` splices the body by KEY. Absent ⇒ the head keeps
        /// `key = ValueNone` (a saturated builtin operator codegen emits directly via
        /// `BuiltinOps`, `op_AddressOf` / other non-provider intrinsics, or a splice
        /// target whose symbol did not resolve — a diagnostic already fired). This is
        /// the operator/intrinsic twin of `ExternalValue` (resolved *value* refs); it
        /// exists because these heads are minted fresh by Freeze rather than routed
        /// through `translateIdent`'s `ExternalValue` path.
        IntrinsicKey: SideTable<SymbolKey>
        /// Keyed by an operator-as-value node's `NodeKey` (`(+)` in `Seq.fold (+) …`):
        /// the `SymbolKey` of the *project-local* nominal whose static-operator
        /// member the value binds to. F# resolves such an operator value to the
        /// operand type's own `static member (+)`, not the built-in arithmetic
        /// operator; this is decided type-directed by `Unification.resolveOperatorValues`
        /// (which scans every operand once the file is typed) and read by
        /// `Freeze.translateIdent`, which eta-expands the value into a closure
        /// calling `<declaringType>.op_Addition`. Absent ⇒ the ordinary built-in /
        /// `External` operator-value path. The declaring type, not the member key,
        /// is stored: Freeze re-forms `LocalSymbolKey.ofMember` from it plus the
        /// node's already-known operator name.
        ResolvedOperatorValue: SideTable<SymbolKey>
        /// Keyed by a `:?` type-test expression's `NodeKey`: the resolved
        /// tested-against type (`Expr.DynamicTypeTest`'s target). The node's own
        /// inferred type is `bool` (the result), so the target type — which
        /// codegen needs for the `isinst` operand — is stashed here by
        /// Unification and read by Freeze to populate `TExpr.TypeTest.testTy`.
        TypeTestTargets: SideTable<SemType>
        /// Keyed by a `use` binding's head-pattern `NodeKey`: the `SymbolKey` of the
        /// `Dispose` member to call when the binder's type is *external* (a BCL
        /// disposable). Recorded by `Unification`'s `use`-Dispose resolution and read
        /// by `Freeze` to stamp `TExpr.Use.dispose` (`ValueSome`); absent for a
        /// project-local binder, where Freeze leaves `ValueNone` and codegen takes the
        /// duck-typed direct-call path.
        UseDispose: SideTable<SymbolKey>
        /// Keyed by a `for x in src do …` node's `NodeKey`: how the source yields
        /// its enumerator. Recorded by `Unification.inferForIn` and read by `Freeze`
        /// to stamp `TExpr.ForIn.enumerator`. Absent ⇒ `ForInEnumerator.Interface`
        /// (range sources and the interface path); present with
        /// `ForInEnumerator.Pattern` for a source exposing only a pattern-based
        /// `GetEnumerator()`.
        ForInShape: SideTable<ForInEnumerator>
        /// Keyed by a type-reference OR an expression-position type-name `NodeKey`:
        /// the `SymbolKey` that reference resolves to. Minting sites:
        /// `NameResolution.registerUnionTypeDefn` stamps the *decl* site (`DeclType`
        /// key) from the union's minted `Key`; `translateType` / `resolveNamedGeneric`
        /// stamp type-annotation *use* sites (`TypeNamed` / `TypeGeneric` keys); and
        /// NameResolution's ident/long-ident walk stamps *expression* sites — a
        /// generic external-type receiver (`EqualityComparer<int>.Default`, the
        /// `Expr.TypeApp` head), a folded static-member receiver prefix
        /// (`System.Console` in `System.Console.Out`, the whole `Expr.LongIdent`
        /// node's key), and an external ctor-sugar head (`InvalidOperationException`
        /// as an `App` head). Because a `NodeKey` carries its `NodeKind`, the
        /// expression stamps (`ExprIdent` / `ExprLongIdent`) never collide with the
        /// type-node stamps (`TypeNamed` / …) at the same source offset.
        /// Read by: the type-decl emitter (`Freeze.tryUnionType`) and the enum
        /// use-site elaborator by type key; and the expression-position store-face
        /// consumers — Unification's `tryExternalTypeReceiver` /
        /// `splitExternalClassPrefix` / `tryInferExternalCtorApp` read the stamped
        /// declaring-type key and do a key-addressed `TryLookupMember` /
        /// `TryLookupMembers(_, ".ctor")` instead of re-running an opens-aware
        /// `OpenScope.tryQualify` + string provider lookup at inference time (F#'s
        /// name-resolution/type-inference seam: the static type prefix is resolved
        /// here, opens-aware, ONCE; the post-dot member name stays a string, a
        /// non-opens-sensitive post-selector).
        ResolvedType: SideTable<SymbolKey>
        /// Keyed by a folded static-member `Expr.LongIdent` node (`System.Console.Out`,
        /// `N.pickName`): the resolved `SymbolKey` of the receiver PREFIX (every
        /// segment but the last) when it resolves — opens-aware — to an external
        /// CLASS. This is DISTINCT from `ResolvedType`, which records the type a node
        /// names *wholly* (a ctor-sugar head, a bare type ref, a generic static
        /// receiver). The two carry incompatible meanings for the SAME folded-LongIdent
        /// node — `System.InvalidOperationException` is a whole-name class (a ctor head,
        /// `ResolvedType`) while `N.pickName` is a prefix class + trailing member (here)
        /// — so they cannot share one table: a ctor-app consumer reading `ResolvedType`
        /// must NOT see the receiver prefix of a static member and mistake it for a
        /// constructible head. Read by `splitExternalClassPrefix` (the static-member /
        /// static-value paths); `TryLookupMember(prefixKey, lastSegment)` selects the
        /// post-dot member by key. Absent when the prefix is not an external class (a
        /// namespace, a local field chain, an unknown qualifier).
        ExternalStaticReceiver: SideTable<SymbolKey>
        /// Project-local *module* member registry. Maps a local module's
        /// short name (`SetTree`) → its directly-declared `let` value/function
        /// bindings (member name → the binding-site `NodeKey` `bindingsOfPat` mints
        /// for the head pattern). Populated by `NameResolution.registerLocalModules`,
        /// a pre-pass over the *un-flattened* module tree — the flattened element
        /// walk (`CstWalk.walkModuleTreeWith`) erases module boundaries, so a
        /// sibling module's function would otherwise be unresolvable. Read by the
        /// qualified-name path (`SetTree.add` resolves to the member's binding site,
        /// recorded as a use-site `Binding` entry so Unification/Freeze treat it as
        /// an ordinary local reference) and by the nested-type body walk (an
        /// enclosing module's bindings enter the type-body scope, unqualified).
        /// The `SetTree` *module* and a same-named `SetTree<'T>` *type* coexist:
        /// this table is keyed independently of `Types.Class`.
        LocalModules: Dictionary<string, Dictionary<string, NodeKey>>
        /// Maps a local *type*'s short name (`SetIterator`) → the short name
        /// of the module it is declared inside (`SetTree`). Populated alongside
        /// `LocalModules`; consulted by the nested-type body walk to merge the
        /// enclosing module's bindings into the member-body scope. Absent for a
        /// type declared at namespace / file top level.
        TypeEnclosingModule: Dictionary<string, string>
    }

module PassContextResolution =
    let create (ambient: OpenScope) : PassContextResolution =
        {
            OpenScope = ambient
            AmbientOpenScope = ambient
            TyparScope = Dictionary<string, TypeVar>(System.StringComparer.Ordinal)
            BindingTyparSeed = ValueNone
            EnclosingTypars = ValueNone
            TyparScopeStrict = false
            ExternalAccess = SideTable<_>()
            TyparInterfaceCall = SideTable<_>()
            ExternalOptionalFill = SideTable<_>()
            ExternalValue = SideTable<_>()
            ExternalUnionCaseStamp = SideTable<_>()
            IntrinsicKey = SideTable<_>()
            ResolvedOperatorValue = SideTable<_>()
            TypeTestTargets = SideTable<_>()
            UseDispose = SideTable<_>()
            ForInShape = SideTable<_>()
            ResolvedType = SideTable<_>()
            ExternalStaticReceiver = SideTable<_>()
            LocalModules = Dictionary<_, _>()
            TypeEnclosingModule = Dictionary<_, _>()
        }

/// A `recv?name` dynamic-access site whose `^TResult` var (`Root`) may escape
/// `dynamic` to a concrete type through context (`d?foo + 1` pins it to `int`).
/// Recorded by `inferDynamicLookup`; swept post-settle by `DynamicEscape.run`,
/// which warns when `Root` zonks to a non-`dynamic` shape (the `default : dynamic`
/// did NOT fire — an unchecked assertion). `Key` is the `?` node's key, used both
/// to attribute the warning and to match a suppressing `(d?foo : T)` ascription.
type DynamicEscapeSite = { Root: TypeVar; Key: NodeKey }

/// **Thread-safety:** a `PassContext` is single-threaded — its side tables,
/// `Diagnostics` channel, and the `TypeVar` graph it owns all mutate in
/// place and are not safe to access from multiple threads. Parallelism
/// happens at file granularity by allocating one `PassContext` per file
/// and analysing them concurrently; the shared `IExternalSymbolProvider`
/// is the only object that crosses thread boundaries (and its contract
/// requires thread-safe `TryLookup`).
///
/// The bulk of the per-file state lives in three sub-records grouped by
/// concern: `Types` (project type registry), `Bindings` (per-binder side
/// tables), `Resolution` (name-resolution scopes).
[<Sealed>]
type PassContext(provider: IExternalSymbolProvider, input: string, lexed: Lexed) =
    // Seed the ambient (implicit-open) prelude from the provider's
    // `AmbientOpenPrefixes` (the referenced-contract `[<AutoOpen>]` modules /
    // FSharp.Core prelude). This is the single seam: every path that builds a
    // `PassContext` (the pipeline and the direct-construction tests alike) picks
    // it up here. Providers without an implicit prelude return `[]`, so
    // resolution is unchanged for them. The ambient sits at the tail of the
    // prefix list, so explicit `open`s the pass walk prepends are tried first.
    let ambientOpenScope =
        { OpenScope.empty with
            Prefixes = provider.AmbientOpenPrefixes
        }

    // `IntrinsicReprTypes` holds ONLY this compilation unit's own intrinsic
    // bindings (`type int = (# "System.Int32" #)`), registered by NameResolution.
    // A *referenced* package's intrinsics are no longer seeded here: they ride
    // the provider as `ExternalTypeShape.Intrinsic` shapes, read local-first /
    // provider-fallback by `subsumes.canonKey`, `translateType`, and codegen.
    let types = PassContextTypes.empty ()

    member val Provider = provider

    /// The four language-capability identities, resolved once here THROUGH THE
    /// PROVIDER (`ExternalSymbols.resolveCapabilities`) from their canonical Vesper
    /// contract names — the single carrier the `for-in`/`use` lowering and the FS0378
    /// custom-eq/comp check read. A capability the provider does not name is
    /// `ValueNone` (resolve-on-use, §5.4); the passes carry zero hardcoded BCL
    /// identities.
    member val CapabilityIds = ExternalSymbols.resolveCapabilities provider with get

    member val Input = input
    member val Lexed = lexed
    /// The simple name of the assembly this compilation unit emits into — the
    /// **home assembly** stamped onto every locally-minted nominal `SymbolKey`
    /// (`LocalSymbolKey.ofType`), so a project-local type's key equals the key a
    /// *consumer* mints for the same type from its `SymbolOrigin` (asm = the
    /// declaring assembly, invariant per type).
    /// `""` for the front-end-only / contract-scrape paths that never emit and so
    /// have no home assembly to stamp; set by `Pipeline.analyse*For`.
    member val AssemblyName = "" with get, set
    // Fully qualified: this file `open`s `XParsec.FSharp.Parser`, which also
    // defines a `Diagnostic`; with our `Diagnostic` now declared in `SideTypes.fs`
    // (ahead of `Tast.fs`) rather than in this file, the bare name would bind to
    // the parser's. The record literals in `Error`/`Warn` below resolve by field
    // labels, so only this annotation needs the qualifier.
    member val Diagnostics = ResizeArray<XParsec.FSharp.SemanticAnalysis.Diagnostic>() with get
    member val Types = types with get

    /// The primitive-intrinsic identities (`int`/`string`/…) resolved from the
    /// `prim-types-*` contract — the `SemType` analogue of `CapabilityIds`, so the passes
    /// carry no static intrinsic `SemType`s. Each field resolves lazily/cached (see
    /// `IntrinsicSet`), reading this unit's own `IntrinsicKeys` (populated by the
    /// NameResolution pre-pass) first, then the provider via ambient `open`.
    member val Intrinsics =
        IntrinsicSet(fun name -> IntrinsicResolve.tryResolveIntrinsicType provider types.IntrinsicKeys name) with get

    /// PassContext-lifetime memo of nominal `SymbolKey` → canonical intrinsic
    /// `SymbolKey`, populated lazily by `subsumes.canonKey`. `subsumes`' recursive walk
    /// would otherwise round-trip the composite provider / MetadataLoadContext per node
    /// to read an `ExternalTypeShape.Intrinsic` canon; the set is tiny and bounded, so a
    /// per-context cache keyed by the incoming key suffices. A key that is neither a local
    /// nor a provider intrinsic caches its own identity.
    member val IntrinsicCanonCache = Dictionary<SymbolKey, SymbolKey>() with get

    /// The reverse intrinsic axis `{ platform-repr -> canon }`: a platform runtime
    /// name (`"number"`) -> the `.fsi` canon identities sharing that repr. Its sole
    /// unify-time reader is `numericFamilyOr` (the JS `number`-family contravariant
    /// widening, keyed on the MULTI-canon entries) — the single-canon BCL
    /// reconciliation (`"System.Exception"` -> `exn`) that `canonKey` used to read
    /// from here now happens eagerly at resolution (`MetadataSymbols.tryBuildType`),
    /// so no BCL name reaches the unifier. Merges the provider's
    /// `IntrinsicReverseCanon` (referenced contracts) with this unit's own
    /// self-compiled intrinsics (`IntrinsicReprTypes`, inverted). `lazy` so it is
    /// built once, on the first Unification read — AFTER NameResolution has
    /// populated `IntrinsicReprTypes`.
    member val IntrinsicReverseCanon: Lazy<Dictionary<string, SymbolKey list>> =
        lazy
            (let d = Dictionary<string, SymbolKey list>()

             for KeyValue(platform, canons) in provider.IntrinsicReverseCanon do
                 d.[platform] <- canons
             // Local self-compiled intrinsics (short `.fsi` name -> platform repr):
             // invert so a raw platform name reconciles with the short identity within
             // a `--compiling-fslib` unit. Skips a degenerate `platform = short`. A local
             // repr wins over the provider's canons for the same platform (self-compiled
             // identity is authoritative within the unit), so it replaces the entry. The
             // canon value is the contract-stamped qualified identity (`IntrinsicKeys`, keyed
             // from the declaring `namespace`), so it compares EQUAL to the forward/provider
             // canons; `intrinsicKeyOf` falls back to the by-name mint only for a repr with
             // no stamped key.
             for KeyValue(short, platform) in types.IntrinsicReprTypes do
                 if platform <> short then
                     d.[platform] <- [ TypeRegistry.intrinsicKeyOf types short ]

             d) with get

    member val Bindings = PassContextBindings.empty () with get
    member val Resolution = PassContextResolution.create ambientOpenScope with get
    member val Desugared = SideTable<DesugaredForm>() with get
    /// Keyed by an `Expr.App` NodeKey; present only for printf calls lowered
    /// inline (literal format, fully applied, a `StdOut`/`StdErr`/`StringResult`
    /// sink, every specifier `PrintfHoleForm.tryClassify` accepts). Absence keeps
    /// the existing FSharp.Core path.
    member val PrintfApp = SideTable<PrintfSpec.PrintfSink>() with get
    /// Keyed by the same `Expr.App` NodeKey as `PrintfApp`; present only for a
    /// fully-applied `%a`/`%t` call on a *writer/builder* family (a `Writer` /
    /// `Builder` sink whose scratch type the provider resolves). Carries the
    /// resolved scratch class + `ToString` key Freeze splices into the capture-first
    /// residue block. `sprintf` `%a`/`%t` has no entry — its residue is the
    /// callback's returned string; absence means "no scratch needed".
    member val PrintfCallbackScratch = SideTable<PrintfSpec.CallbackScratch>() with get
    /// Keyed by an `Expr.App` NodeKey; present only for a *fully-unapplied*
    /// lowerable printf partial (`printfn "%d"`, `printf "%d %s"`, …) — a literal
    /// format, `idx = 0`, a `StdOut`/`StdErr`/`StringResult` sink, `1..K` holes,
    /// every specifier lowerable and none `%A`/`%O` (an unapplied `%A` hole is an
    /// unpinned typar). Freeze synthesises a Vesper closure
    /// `fun h1 … hn -> Format(sink, …)` for it (heap, 4a) instead of the
    /// FSharp.Core `PrintfFormat` cold path. Mutually exclusive with `PrintfApp`
    /// (that fires only when the call is fully applied). Absence keeps the existing
    /// FSharp.Core path.
    member val PrintfPartial = SideTable<PrintfSpec.PrintfSink>() with get
    /// E1: a `let`-bound (or ascribed) format-string literal, keyed by its
    /// BINDING-SITE NodeKey (the head pattern's key — the same key a use-site
    /// `Ident` resolves to via `Bindings.Binding`). Recorded by `Infer.inferBinding`
    /// when `tryTypeFormatLiteral` types the literal against a `PrintfFormat`
    /// annotation. The printf gate (`tryInferPrintfApp`) and Freeze
    /// (`translatePrintfFormat`) both const-propagate through it: a format position
    /// holding such an `Ident` recovers the literal and lowers natively, exactly like
    /// a syntactic literal — there is no cold runtime for a format value in the
    /// self-host contract (the printf functions are inline-lowered intrinsics), so
    /// native lowering is the ONLY runnable path.
    member val PrintfFormatLiterals = SideTable<Expr<SyntaxToken>>() with get

    /// E1 const-propagation: if `argExpr` at a printf format position is an `Ident` /
    /// `LongIdent` bound to a format-string literal (recorded in
    /// `PrintfFormatLiterals` by `inferBinding` when `tryTypeFormatLiteral` typed it
    /// against a `PrintfFormat` annotation), return that underlying `Expr.String` so
    /// the gate / Freeze can treat it exactly like a syntactic literal. `ValueNone`
    /// for any other shape — a direct literal (handled by the ordinary path), or a
    /// non-format binding. Consulted by BOTH the gate (`tryInferPrintfApp`) and Freeze
    /// (`translatePrintfFormat`), so the two stay in lockstep.
    member this.TryRecoverFormatLiteral(argExpr: Expr<SyntaxToken>) : Expr<SyntaxToken> voption =
        match argExpr with
        | Expr.Ident _
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent _) ->
            match this.Bindings.Binding.TryGetValue(CstKeys.ofExpr argExpr) with
            | ValueSome rb -> this.PrintfFormatLiterals.TryGetValue rb.BindingSite
            | ValueNone -> ValueNone
        | _ -> ValueNone

    /// Type provenance: the set of nodes whose type was **written by the programmer**
    /// (a source type annotation fixed it), keyed by the annotated value / pattern /
    /// binding node. A node's type is DECLARED iff `IsTypeDeclared` — every other
    /// type-bearing node (list / array / record / DU / object-expression element,
    /// unannotated `let` / lambda parameter, …) is INFERRED *by absence*, so the two
    /// classes partition the type-bearing nodes without recording the (far larger)
    /// inferred set. Recorded by `Infer`/`InferPat`/`InferTypeOps`/… at each
    /// `translateType`-of-a-source-annotation site (ascription `(e : T)`, annotated
    /// `let`/return, typed pattern & parameter `(x : T)`, `new T(…)`, `:> T` / `:? T`
    /// / `:?> T`, `match … :? T as x`). Type *declarations* (`type …`) are always
    /// explicit and carry no per-node provenance, so they are out of scope.
    ///
    /// A DECLARED node's type can still contain INFERRED positions — a `_` wildcard
    /// (`Box<_>`: `Box` declared, the arg inferred). Those are tracked per-TyVar by
    /// `inferenceHoles`: read provenance off the node's *un-zonked* annotation type and
    /// treat a `TyVar` position as inferred iff `IsInferenceHole`, every nominal /
    /// arrow / tuple / applied-with-concrete-arg position as declared. (A *named* typar
    /// `'a` in `Box<'a>` is written, so it is NOT a hole — only the anonymous `_` is.)
    member val private declaredTypeSites = SideTable<unit>() with get

    /// The `_`-wildcard TyVars minted by `translateType` for a source `Type.VarType
    /// Typar.Anon`. Reference identity: the exact node stored at the wildcard position
    /// of a declared annotation's type. Query it as it appears in the *un-zonked* type
    /// (zonking a resolved hole to its inferred fill would erase the marker).
    member val private inferenceHoles = HashSet<TypeVar>(HashIdentity.Reference) with get

    /// Mark `key`'s type as source-declared (see `declaredTypeSites`), given the
    /// annotation's translated type `annTy`. A BARE `_` (`let x : _ = …`, `(x : _)`) is
    /// a request to *infer*, not a declaration, so it is skipped — the node stays
    /// inferred. Any written structure (`Box<_>`, `int`, `'a`) marks declared, even when
    /// it contains nested `_` holes (queryable via `HasInferenceHoleIn`). Idempotent.
    member this.MarkTypeDeclared(key: NodeKey, annTy: SemType) =
        let isBareHole =
            match annTy with
            | TyVar tv -> this.IsInferenceHole tv
            | _ -> false

        if not isBareHole then
            this.declaredTypeSites.Set(key, ())

    /// Whether `key`'s type was written in source (`true`) rather than inferred
    /// (`false` — the default for every type-bearing node with no annotation). Note a
    /// `true` node may still carry inferred `_`-wildcard positions (`IsInferenceHole`).
    member this.IsTypeDeclared(key: NodeKey) : bool = this.declaredTypeSites.ContainsKey key

    /// Mark `tv` as a `_`-wildcard inference hole (see `inferenceHoles`). Idempotent.
    member this.MarkInferenceHole(tv: TypeVar) = this.inferenceHoles.Add tv |> ignore

    /// Whether `tv` is a `_`-wildcard hole — an INFERRED position inside an otherwise
    /// declared annotation type. Check the TyVar as stored in the un-zonked type.
    member this.IsInferenceHole(tv: TypeVar) : bool = this.inferenceHoles.Contains tv

    /// Whether `ty` — read from the LIVE (pre-freeze) TyVar graph, e.g.
    /// `TyVar ctx.Bindings.TypeVar.[key]` — carries any `_`-wildcard hole. Distinguishes
    /// a fully-written `Box<int>` (`false`) from a partly-inferred `Box<_>` (`true`) at a
    /// node that `IsTypeDeclared`. Follows a non-hole var's `Link` to reach structure but
    /// STOPS at a hole (its `Link` is the *inferred fill*, not part of the written type),
    /// so a resolved `Box<_>` (`_` pinned to `int`) still reports its hole. Freeze zonks
    /// holes away, so this must run against the pre-zonk graph, not the frozen TAST.
    member this.HasInferenceHoleIn(ty: SemType) : bool =
        let seen = HashSet<TypeVar>(HashIdentity.Reference)

        let rec walk t =
            match t with
            | TyVar tv when this.IsInferenceHole tv -> true
            | TyVar tv ->
                if not (seen.Add tv) then
                    false
                else
                    match tv.Link with
                    | ValueSome inner -> walk inner
                    | ValueNone -> false
            | TyClass(_, args)
            | TyRecord(_, args)
            | TyUnion(_, args)
            | TyConst(_, args) -> args |> EqArray.exists walk
            | TyFun(a, b) -> walk a || walk b
            | TyTuple items -> items |> EqArray.exists walk
            | _ -> false

        walk ty

    /// The node-keyed value-struct closure verdict. Keyed by a
    /// SOURCE-lambda argument's NodeKey; the `FunVerdict` carries the flat `FunN`
    /// arity (always) and, for a transformer combinator, the result-typar position.
    /// Recorded in `inferApp` when an argument lambda lands on a typar parameter
    /// whose `:> Fun<a,b>`/`:> Fun<a,b,c>` coercion bound fires (the `subsumes` arm). The
    /// decision lives here (inference) as the single source of truth; the Pipeline
    /// snapshots it onto `TastFile.FunVerdicts`, codegen's `discoverClosures` reads
    /// `Arity` to size the value-struct closure's flat `Invoke`, and
    /// `ClosureVerdictRewrite` reads `ResultTyparPos` for the slot/result rewrite. A
    /// lambda with no entry is the ordinary curried closure.
    member val FunVerdicts = SideTable<FunVerdict>() with get
    /// A project-local generalised binding's
    /// `NodeKey` → its frozen typar bounds (`FrozenConstraint` list). Written by
    /// `Elaborate.translateModuleElem` at the single index-minting point (so the
    /// bounds' typar leaves carry the SAME method-axis indices the body freezes
    /// with), snapshotted by `Elaborate.run` onto `TastFile.GenericFnSchemes`. Read
    /// by the call-site phantom-typar solve (`EmitCall`).
    member val GenericFnSchemes = SideTable<FrozenConstraint list>() with get
    /// Keyed by an `Expr.LibraryOnlyStaticOptimization` NodeKey: the resolved
    /// `when ^T : …` constraints of that one clause (the `and`-joined list), with
    /// the typar / required type translated to `SemType` while the binding's typar
    /// scope is live. Freeze reads it to build each `TExpr.StaticOptimization`
    /// clause; the typar carries the inline binding's quantified root so
    /// `Inline.inlineExpand` can substitute it at the call site.
    member val StaticOpt = SideTable<EqArray<TStaticOptConstraint>>() with get
    /// Current let-depth (Rémy's levels). Push on entering a binding group's
    /// RHSes, pop after typing them; generalisation uses the pre-push value as
    /// the threshold for "which TyVars do I quantify?".
    member val CurrentLevel = 0 with get, set
    /// Bare-program list literals: each `[…]` whose container type was left
    /// *flexible* (a fresh `TypeVar`, paired with its element type) so a consumer
    /// can drive it — `List.fold`'s `Vesper.Collections.List` parameter flips it to
    /// the Vesper list, otherwise it defaults to FSharp.Core's `list`. Drained by
    /// `Unification.resolveListLiterals` after the walk: a still-free literal links
    /// to the default list, a flipped one has its element reconciled. Programs that
    /// declare their own `list` abbrev never register here (they resolve eagerly).
    member val ListLiterals = ResizeArray<TypeVar * SemType>() with get

    /// `recv?name` dynamic-access sites, enqueued by `inferDynamicLookup` and swept
    /// post-settle by `DynamicEscape.run`. A site whose `Root` zonks to a concrete
    /// non-`dynamic` type is an implicit escape (the `default : dynamic` did not fire)
    /// and warns — unless its `Key` is in `DynamicEscapeSuppressed`.
    member val DynamicEscapes = ResizeArray<DynamicEscapeSite>() with get

    /// `?` node keys whose escape warning is suppressed by an explicit ascription
    /// directly on the `?` expression (`(d?foo : int)`), recorded by
    /// `inferTypeAnnotation`. "Name the type at the escape point."
    member val DynamicEscapeSuppressed = HashSet<NodeKey>() with get

    /// Operator-as-value use sites (`(+)` in `Seq.fold (+) …`), enqueued by
    /// `inferIdent` and drained after the walk by `Unification.resolveOperatorValues`
    /// — which binds each to a project-local static-operator member by scanning its
    /// (then-ground) operand types, recording the verdict in
    /// `Resolution.ResolvedOperatorValue`. Deferred because the binding is
    /// type-directed: at the node the operand types are still flexible.
    member val OperatorValueSites = ResizeArray<OperatorValueSite>() with get

    /// Which cons-list a *bare-program* list literal/pattern (one no consumer
    /// pinned) defaults to when drained by `Unification.resolveListLiterals`.
    /// `false` (the default) keeps FSharp.Core's `list` — the form a normal
    /// FSharp.Core-referencing program prints/interops with. `true` is set by the
    /// self-host package build (`Pipeline.analyse*ForSelfHost`): a BCL-only package
    /// has no FSharp.Core, so an unpinned `[]`/`::` must land on the Vesper
    /// cons-list to emit `Vesper.List`-only — the `withCore`-vs-not distinction the
    /// front end cannot otherwise see (it lives in codegen's `ProjectInfo`).
    member val DefaultListIsVesper = false with get, set

    /// Compiler-recognised parameter attributes (`ParamAttrs`) for each
    /// module-level `let inline` binding, keyed by the binding's function-binder
    /// `NodeKey` and positionally aligned to its curried parameters. Populated by
    /// `Elaborate` (which also validates each `[<CallAtMostOnce>]` parameter's
    /// linearity) and read by `Passes.InlineExpansion` for *local* inline call
    /// sites; the cross-package twin travels in `ExternalSymbols.InlineBody`.
    /// Only bindings with at least one non-default parameter register here.
    member val InlineParamAttrs = Dictionary<NodeKey, ParamAttrs[]>() with get

    /// Source text of `token`. Empty for virtual (synthesised) tokens.
    member this.NameOf(token: SyntaxToken) : string =
        match token.Index with
        | TokenIndex.Regular iT -> this.Lexed.GetTokenString(iT, this.Input)
        | TokenIndex.Virtual -> ""

    /// Allocation-free sibling of `NameOf`: a `ReadableString` view of `token`'s
    /// source text, without copying out a substring. Empty for virtual tokens.
    member this.ReadableOf(token: SyntaxToken) : ReadableString =
        match token.Index with
        | TokenIndex.Regular iT -> this.Lexed.GetTokenReadable(iT, this.Input)
        | TokenIndex.Virtual -> ReadableString.Empty

    /// Record an `Error`-severity diagnostic at `key`. The canonical way to
    /// report — collapses the otherwise-ubiquitous inline `Diagnostic` literal
    /// (every call passed `Code = ""` / `Severity = Severity.Error`).
    member this.Error(key: NodeKey, msg: string) =
        this.Diagnostics.Add
            {
                Key = key
                Code = ""
                Message = msg
                Severity = Severity.Error
            }

    /// `Warning`-severity analogue of `Error`.
    member this.Warn(key: NodeKey, msg: string) =
        this.Diagnostics.Add
            {
                Key = key
                Code = ""
                Message = msg
                Severity = Severity.Warning
            }

// `Diagnostic` / `Severity` moved to `SideTypes.fs` (they must precede `Tast.fs`);
// `PassContext` above references them as types defined earlier in compile order.
