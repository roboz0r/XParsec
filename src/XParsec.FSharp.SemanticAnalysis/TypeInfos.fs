namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// Side tables hold all in-flight semantic information. CST is never mutated.

/// Project-local nominal identity for a type definition: the containment chain the
/// declaration sits in, the SOURCE simple name, and the declared generic arity. It carries
/// no home assembly — a key is nominal identity, and a consumer resolving the same type
/// across a package boundary therefore mints an EQUAL key without having to agree with
/// this compilation about what its own assembly is called.
module internal LocalSymbolKey =

    /// The project-local `TypeKey` for `name` at `arity`, declared in `holder` — the
    /// declaring namespace, or the enclosing module chain rooted in it
    /// (`NameResolutionTypeRegistration.localTypeHolder` is the one producer of the
    /// latter). `name` is the name AS WRITTEN and the arity is the declared typar count:
    /// the `` `N `` is a CLR metadata spelling, produced only when a metadata name is
    /// rendered (`SymbolKeyOps.typeSegmentName`), never carried in an identity. This key IS
    /// the registry key (`TypeRegistry` tables are `TypeKey`-keyed).
    ///
    /// `SymbolKeyOps.typeKeyOfHolder` is the mint — shared with the `.fsi` contract
    /// extractor, so a type declared here and the same type met across a package boundary
    /// agree on their identity, escaped names (`` ``[]`` ``, which can carry no arity)
    /// included.
    let ofType (holder: TypeHolder) (name: string) (arity: int) : TypeKey =
        SymbolKeyOps.typeKeyOfHolder holder name arity

    /// The project-local `SymbolKey.MemberKey` for a PROPERTY `name` on the type
    /// identified by `declKey`. A property's `ArgSig` is empty and its name is unique on
    /// a type (properties do not overload by argument), so `{decl, name, [], 0, Property}`
    /// is a TOTAL identity by construction — there is no placeholder here. This is the
    /// only local member-key mint: METHOD keys are minted from the RESOLVED member
    /// (`LocalMemberKeys.totalMemberKey`, which freezes the real value signature and
    /// method-typar arity), so a lossy placeholder method key is unrepresentable. The
    /// local analogue of the external property `MemberKey` minted by `MetadataSymbols` /
    /// `VesperLib`; carried on the local property-get TAST nodes so codegen reads the
    /// declaring type off `decl` instead of re-deriving it from a class-name string.
    let ofProperty (declKey: TypeKey) (name: string) : SymbolKey =
        SymbolKeyOps.memberKey declKey name EqArray.empty 0 MemberKind.Property

// `ModuleBindingInfo` moved to `SideTypes.fs` (it must precede `Tast.fs`).

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
    member val MethodTypeParams: EqArray<string * TyVarId> = EqArray.empty with get, set
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
    member this.EffectiveMethodTypars: EqArray<string * TyVarId> =
        if GeneralizedTypars.count this.Generalized > 0 then
            EqArray.ofArray (GeneralizedTypars.toArray this.Generalized)
        else
            this.MethodTypeParams

    /// `true` when the source declares the member with `MemberKeyword.Override`
    /// or `MemberKeyword.Default`. Stamped by member extraction; consumed by
    /// Elaborate/Codegen to choose `call` vs `callvirt`.
    member val IsOverride: bool = false with get, set

/// Field types start as fresh TyVar placeholders stamped by NameResolution and
/// are linked by Unification's field-fill-in pass before any expression is
/// typed. `FieldNames` carries per-field names for named fields
/// (`| Case of x: int * y: int`); positional fields have `ValueNone`.
[<Sealed>]
type UnionCaseInfo
    (
        name: string,
        unionName: string,
        unionKey: TypeKey,
        fields: SemType[],
        fieldNames: string voption[],
        declKey: NodeKey
    ) =
    member val Name = name
    /// The declaring union's short name AS WRITTEN. Only ever compared against a written
    /// QUALIFIER (`Choice.Choice1Of3`) — it is not this case's identity, and no lookup may
    /// re-resolve it: a name identifies a type only as seen from somewhere, and by the time
    /// a case is in hand its union is already resolved.
    member val UnionName = unionName
    /// The declaring union's `TypeKey`, stamped at registration from the union's own claim.
    /// "Which union declares this case" is therefore identity navigation, answered by a
    /// key-addressed read (`TypeRegistry.unionOfCase`) that asks no scoping question —
    /// there is none left to ask.
    member val UnionKey = unionKey
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
/// implemented by `ClassTypeInfo`, `UnionTypeInfo` and `RecordTypeInfo` so
/// Unification's `resolveInterfaceImpls` / `fillInterfaceImpls`, the `subsumes`
/// interface admission, the custom-eq/comp conformance check and
/// `InferControlFlow.tryLocalInterfaceEnumeratorOn` operate over *any* kind without
/// forking the (already kind-agnostic) logic. `MkSelfType` is the only kind-dependent
/// piece: a class yields `TyClass(Key, args)`, a union `TyUnion(Key, args)`, a record
/// `TyRecord(Key, args)`, so the `this`-type seeding inside an impl body is exact.
type IInterfaceImplHost =
    /// The nominal identity widened for the kind-blind sinks (the external-symbol store
    /// face, a diagnostic's `qualifiedName`).
    abstract member Key: SymbolKey
    /// The SAME identity as a `TypeKey` — what the `SemType`/`FrozenType` nominal cases
    /// carry, and what the declaring slot of a `MemberKey` / a `TypeHolder` demands. A
    /// nominal type's key can only ever be a type key, so no consumer narrows.
    abstract member TypeKey: TypeKey
    abstract member DeclKey: NodeKey
    abstract member TypeParams: EqArray<string * TyVarId>
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
        typeParams: EqArray<string * TyVarId>,
        fields: RecordFieldInfo[],
        declKey: NodeKey,
        typarConstraints: TyparConstraints<SyntaxToken> voption,
        key: TypeKey
    ) =
    new(name, typeParams, fields, declKey) =
        RecordTypeInfo(
            name,
            typeParams,
            fields,
            declKey,
            ValueNone,
            LocalSymbolKey.ofType (TypeHolder.InNamespace NamespaceKey.Global) name typeParams.Length
        )

    member val Name = name
    /// Stable project-local nominal identity — the arity-qualified `TypeKey` minted by
    /// `stampLocalTypeKey` at registration, whose holder chain is the type's declaring
    /// containment (namespace + enclosing modules) rooted in its home assembly. The
    /// convenience constructor (synthesis / test paths with no containment in scope)
    /// defaults to the global-namespace, home-less placeholder.
    member val TypeKey: TypeKey = key
    member this.Key: SymbolKey = SymbolKey.Type this.TypeKey
    member val TypeParams = typeParams
    member val Fields = fields
    member val DeclKey = declKey
    /// `when 'a : ...` clause attached to the type's typar list, if any.
    /// `NameResolution.registerRecordTypeDefn` walks this and attaches each
    /// constraint to the matching prototype TyVar in `TypeParams`.
    member val TyparConstraints = typarConstraints
    /// Equality posture for this record.
    /// Filled during `NameResolution.registerRecordTypeDefn` from the type's
    /// attributes; the placeholder defaults to `Structural` so any path that
    /// overlooks the registration (mostly tests that synthesise records
    /// directly) stays equal-by-fields. `Unification.checkConstraint` reads it
    /// to short-circuit `NoEquality` types; `Elaborate` projects it onto
    /// `TTypeDecl.EqualitySupport` for codegen.
    member val EqualitySupport = EqualityVerdict.Structural with get, set
    /// Comparison posture for this record. Filled during
    /// `NameResolution.registerRecordTypeDefn` from the type's attributes. Defaults
    /// to `NoComparison` (opt-in). `Unification.checkConstraint` reads it to reject
    /// `<` / `>` / `<=` / `>=` on un-annotated types; `Elaborate` projects it onto
    /// `TTypeDecl.ComparisonSupport` for codegen.
    member val ComparisonSupport = ComparisonVerdict.NoComparison with get, set
    /// `[<RequireQualifiedAccess>]` posture. Filled during
    /// `NameResolution.registerRecordTypeDefn` from the type's attributes; defaults
    /// to `false`. `Elaborate` projects it onto `TTypeDecl.IsRequireQualifiedAccess`,
    /// which the frozen-tree projection reads so a cross-unit RQA record is kept out
    /// of a consumer's unqualified (`{ X = … }`) field-set index.
    member val IsRequireQualifiedAccess = false with get, set
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
    /// Stamped by `NameResolution.registerNominalMember`; each impl's interface type
    /// is resolved + verified, and its member bodies typed, by Unification's
    /// `fillHostMembers` (mirroring `UnionTypeInfo.InterfaceImpls`). Empty unless
    /// the record declares an `interface … with` block. `Elaborate` projects them onto
    /// `TTypeKind.Record.interfaces`.
    member val InterfaceImpls: ClassInterfaceImplInfo[] = [||] with get, set
    /// `[<Struct>]` record. Stamped by `registerRecordTypeDefn` from the same
    /// struct predicate (`isValueTypeDefn`) the struct-field cycle check reads;
    /// `Elaborate` projects it onto `TTypeKind.Record`'s `ClassValueKind` so
    /// codegen emits a `System.ValueType`-based value type (sealed). The struct-
    /// field cycle check consulted the CST attributes directly because this flag
    /// did not exist; it can now read the registry.
    member val IsValueType: bool = false with get, set

    interface IInterfaceImplHost with
        member this.Key = this.Key
        member this.TypeKey = this.TypeKey
        member this.DeclKey = this.DeclKey
        member this.TypeParams = this.TypeParams
        member this.ThisName = this.ThisName
        member this.ThisKey = this.ThisKey
        member this.InterfaceImpls = this.InterfaceImpls
        member this.Members = this.Members
        member this.EqualitySupport = this.EqualitySupport
        member this.ComparisonSupport = this.ComparisonSupport
        member this.MkSelfType args = TyRecord(this.TypeKey, args)

/// `TypeParams` mirrors `RecordTypeInfo.TypeParams`. Case field types may
/// reference these TyVars directly.
[<Sealed>]
type UnionTypeInfo
    (
        name: string,
        typeParams: EqArray<string * TyVarId>,
        cases: UnionCaseInfo[],
        declKey: NodeKey,
        typarConstraints: TyparConstraints<SyntaxToken> voption,
        key: TypeKey
    ) =
    new(name, typeParams, cases, declKey) =
        UnionTypeInfo(
            name,
            typeParams,
            cases,
            declKey,
            ValueNone,
            LocalSymbolKey.ofType (TypeHolder.InNamespace NamespaceKey.Global) name typeParams.Length
        )

    member val Name = name
    /// Stable project-local nominal identity — the arity-qualified `TypeKey` (e.g.
    /// `Choice\`2`) minted by `stampLocalTypeKey` at registration to match the emitted
    /// metadata name. The convenience constructor (synthesis / test paths with no
    /// containment in scope) defaults to the global-namespace, home-less placeholder.
    member val TypeKey: TypeKey = key
    member this.Key: SymbolKey = SymbolKey.Type this.TypeKey
    member val TypeParams = typeParams
    member val Cases = cases
    member val DeclKey = declKey
    /// `when 'a : ...` clause attached to the type's typar list, if any.
    /// `NameResolution.registerUnionTypeDefn` walks this and attaches each
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
    /// `Unification.checkConstraint` short-circuits on `NoEquality`; `Elaborate`
    /// projects it onto `TTypeDecl.EqualitySupport` for codegen.
    member val EqualitySupport = EqualityVerdict.Structural with get, set
    /// Comparison posture for this union. Filled during
    /// `NameResolution.registerUnionTypeDefn` from the type's attributes. Defaults
    /// to `NoComparison` (opt-in). `Unification.checkConstraint` reads it to reject
    /// `<` / `>` / `<=` / `>=` on un-annotated types; `Elaborate` projects it onto
    /// `TTypeDecl.ComparisonSupport` for codegen.
    member val ComparisonSupport = ComparisonVerdict.NoComparison with get, set
    /// `[<RequireQualifiedAccess>]` posture. Filled during
    /// `NameResolution.registerUnionTypeDefn` from the type's attributes; defaults
    /// to `false`. `Elaborate` projects it onto `TTypeDecl.IsRequireQualifiedAccess`,
    /// which the frozen-tree projection reads so a cross-unit RQA union's cases are
    /// kept out of a consumer's bare case index (F# requires `Color.Red`, not `Red`).
    member val IsRequireQualifiedAccess = false with get, set
    /// `interface IFace with member …` blocks declared on the union.
    /// Stamped by `NameResolution.registerNominalMember`; each impl's interface type
    /// is resolved + verified, and its member bodies typed, by Unification's
    /// `fillHostMembers` (mirroring `ClassTypeInfo.InterfaceImpls`). Empty unless
    /// the union declares an `interface … with` block. `Elaborate` projects them onto
    /// `TTypeKind.Union.interfaces`; codegen emission is deferred.
    member val InterfaceImpls: ClassInterfaceImplInfo[] = [||] with get, set

    interface IInterfaceImplHost with
        member this.Key = this.Key
        member this.TypeKey = this.TypeKey
        member this.DeclKey = this.DeclKey
        member this.TypeParams = this.TypeParams
        member this.ThisName = this.ThisName
        member this.ThisKey = this.ThisKey
        member this.InterfaceImpls = this.InterfaceImpls
        member this.Members = this.Members
        member this.EqualitySupport = this.EqualitySupport
        member this.ComparisonSupport = this.ComparisonSupport
        member this.MkSelfType args = TyUnion(this.TypeKey, args)

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
    (name: string, typeParams: EqArray<string * TyVarId>, declKey: NodeKey, key: TypeKey, selfKey: SymbolKey) =
    member val Name = name
    /// Stable project-local nominal identity, minted by `stampLocalTypeKey` at
    /// registration to match a use-site key. Never emitted (the abbrev is intrinsic).
    member val TypeKey: TypeKey = key
    member this.Key: SymbolKey = SymbolKey.Type this.TypeKey
    /// The abbrev's INTRINSIC identity key (contract namespace, arity-suffixed),
    /// resolved through `TypeRegistry.intrinsicKeyOf` at registration —
    /// the SAME key a use-site (`Translate`) resolves the abbrev name to. Distinct
    /// from `Key` (the local nominal claim, for the member-harvest host path — it is
    /// holder-homed, where this is namespace-homed):
    /// this is the `TyConst` key `MkSelfType` seeds onto each member's `ThisTy`, so
    /// a non-`Vesper` user intrinsic-abbrev's self-type cannot diverge from its
    /// use-site identity (`primitiveKey name` hardcoded `Vesper`, the latent split-brain).
    member val SelfKey: SymbolKey = selfKey
    member val TypeParams = typeParams
    member val DeclKey = declKey
    /// Augmentation members (`with member …`). Stamped by
    /// `NameResolution.registerNominalMember`; types linked by Unification's
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
        member this.TypeKey = this.TypeKey
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
/// `Key` is the arity-0 `TypeKey` minted by `stampLocalTypeKey`,
/// so a `(x: E)` annotation resolves to `TyEnum Key` and the surfaced decl carries
/// the identical key.
[<Sealed>]
type EnumTypeInfo(name: string, caseNames: string[], caseStringValues: string[] voption, declKey: NodeKey, key: TypeKey)
    =
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
    /// Stable project-local nominal identity — the arity-0 `TypeKey` minted by
    /// `stampLocalTypeKey`; matches the surfaced `TDecl.Type.Key`.
    member val TypeKey: TypeKey = key
    member this.Key: SymbolKey = SymbolKey.Type this.TypeKey
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
        typeParams: EqArray<string * TyVarId>,
        rhsCst: Type<SyntaxToken>,
        declKey: NodeKey,
        typarConstraints: TyparConstraints<SyntaxToken> voption,
        key: TypeKey
    ) =
    new(name, typeParams, rhsCst, declKey) =
        AbbreviationInfo(
            name,
            typeParams,
            rhsCst,
            declKey,
            ValueNone,
            LocalSymbolKey.ofType (TypeHolder.InNamespace NamespaceKey.Global) name typeParams.Length
        )

    member val Name = name
    /// Stable project-local nominal identity — the arity-qualified `TypeKey` minted by
    /// `stampLocalTypeKey` at registration. Abbreviations are transparent (never
    /// emitted), so this is for symmetry. The convenience constructor (synthesis / test
    /// paths with no containment in scope) defaults to the global-namespace, home-less
    /// placeholder.
    member val TypeKey: TypeKey = key
    member this.Key: SymbolKey = SymbolKey.Type this.TypeKey
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

/// A primary- or secondary-constructor parameter. `Type` is always a `TyVar`, because it is
/// the parameter's binding-site inference cell: a member body's reference to the parameter
/// types through it, and an UNANNOTATED parameter is pinned by the construction site. An
/// ANNOTATED parameter's cell is linked to its declared type at registration
/// (`ctorParamsOfPat`), under the class's typar scope and against the types in scope there.
[<Sealed>]
type ClassCtorParamInfo(name: string, ty: SemType, declKey: NodeKey) =
    member val Name = name
    member val Type = ty
    member val DeclKey = declKey

/// An explicit instance field declared with `val [mutable] x: T`. A `val` field is always
/// annotated, so `Type` is the RESOLVED declared type, translated at registration under the
/// class's typar scope. `IsMutable` reflects the `mutable` keyword — `Elaborate` projects it
/// onto `TTypeKind.Class.fields` so a `this.x <- …` mutation in a member body type-checks
/// and codegen emits a writable `FieldDefinition`. `DeclKey` anchors the field's identity
/// (and a `this.x` `FieldGet`/`FieldSet` resolves against the class member walk, not a
/// binder, so it is currently informational).
[<Sealed>]
type ClassFieldInfo(name: string, ty: SemType, isMutable: bool, declKey: NodeKey) =
    member val Name = name
    member val Type = ty
    member val IsMutable = isMutable
    member val DeclKey = declKey

/// One `[static] let [mutable] [rec] x = <init>` of a class preamble.
/// `Type` starts as a placeholder TyVar stamped by `NameResolution` and is linked
/// by Unification's `fillClassMembers` once the binding is inferred.
/// `Binding` is the WHOLE CST binding, re-read by Unification (to infer) and Elaborate (to
/// translate) — head pattern *and* `argumentPats`: `let f x = …` binds a FUNCTION value, so
/// reading only the head pattern and taking `expr` as the initialiser would silently drop
/// the parameters. `DeclKey` is the binder's `NodeKey`
/// (the same one `bindingsOfPat` mints for the head pattern), so a preamble-bound
/// name reference resolves to it and shares the placeholder TyVar.
[<Sealed>]
type ClassLetInfo(name: string, ty: SemType, declKey: NodeKey, binding: Binding<SyntaxToken>, isRec: bool) =
    member val Name = name
    member val Type = ty
    member val DeclKey = declKey
    member val Binding = binding
    /// `let mutable`. An INSTANCE preamble binder is a mutable *field*, never a ref cell —
    /// a closure over it captures `this`.
    member val IsMutable = binding.mutableToken.IsSome
    /// `let rec` — the binder is in scope of its OWN initialiser (and only then).
    member val IsRec = isRec

/// One entry of a class preamble, in declaration order. A preamble is one ordered
/// sequence per side (static / instance) rather than parallel lists of lets and dos:
/// `static let a = f()` / `static do g a` / `static let b = h()` runs in exactly that
/// order, so the interleaving is load-bearing.
[<RequireQualifiedAccess>]
type ClassPreambleEntry =
    | Let of ClassLetInfo
    | Do of Expr<SyntaxToken>

[<RequireQualifiedAccess>]
module ClassPreamble =
    /// The `let` binders of a preamble, in declaration order — every one of which becomes a
    /// field, so this is the list the field-reference rewrites and the field layout read.
    let lets (entries: ClassPreambleEntry[]) : ClassLetInfo[] =
        entries
        |> Array.choose (
            function
            | ClassPreambleEntry.Let l -> Some l
            | ClassPreambleEntry.Do _ -> None
        )

/// A secondary constructor (`new(args) = SelfType(primaryArgs)`).
/// `Params` are the secondary ctor's own parameters, resolved exactly like the primary
/// ctor's (`ClassCtorParamInfo`). `DeclKey` is a synthetic key minted from the `new`
/// token so each overload is distinct. `Body` is the CST `AdditionalConstrExpr`
/// re-read by Unification (to infer + unify the chain args against the primary
/// ctor) and Elaborate (to translate the let-preamble + primary-ctor args).
[<Sealed>]
type ClassSecondaryCtorInfo(declKey: NodeKey, parms: ClassCtorParamInfo[], body: AdditionalConstrExpr<SyntaxToken>) =
    member val DeclKey = declKey
    member val Params = parms
    member val Body = body

[<Sealed>]
type ClassTypeInfo
    (
        name: string,
        typeParams: EqArray<string * TyVarId>,
        ctorParams: ClassCtorParamInfo[],
        members: TypeMemberInfo[],
        declKey: NodeKey,
        thisName: string,
        thisKey: NodeKey,
        baseKey: NodeKey,
        key: TypeKey
    ) =
    member val Name = name
    /// Stable project-local nominal identity — the arity-qualified `TypeKey` minted by
    /// `stampLocalTypeKey` at registration to match the emitted metadata name.
    member val TypeKey: TypeKey = key
    member this.Key: SymbolKey = SymbolKey.Type this.TypeKey
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
    /// until the group-close `registerInheritedSlot` fill.
    /// `ValueNone` ⇒ codegen emits `TypeDefinition.BaseType = Object`.
    member val BaseType: SemType voption = ValueNone with get, set
    /// CST expression for the constructor arguments to the base type
    /// (`inherit Base(arg1, arg2)`'s `(arg1, arg2)` shape). `ValueNone` for
    /// classes without an `inherit` clause; the inheritance pass
    /// stamps it from `ClassInheritsDecl.expr`.
    member val BaseCtorArgs: Expr<SyntaxToken> voption = ValueNone with get, set
    /// `[<Sealed>]`. Stamped by
    /// `NameResolution.registerClassTypeDefn` from the type's attributes;
    /// `Elaborate` projects it onto `TTypeKind.Class.isSealed` so codegen flips
    /// `TypeAttributes.Sealed` on the emitted `TypeDefinition`.
    member val IsSealed: bool = false with get, set
    /// `static let` / `static do` in declaration order — the `.cctor`'s body. Stamped by
    /// `NameResolution.registerClassTypeDefn` from the class's `classPreamble`; `let` types
    /// are linked by Unification's `fillClassMembers`; `Elaborate` projects each onto a
    /// `TPreambleEntry`. Empty unless the class declares any. A GENERIC class may declare
    /// `static let`: the field rides the open `TypeDefinition`, so each closed instantiation
    /// gets its own.
    member val StaticPreamble: ClassPreambleEntry[] = [||] with get, set
    /// Instance `let` / `do` in declaration order — the tail of the primary ctor, running
    /// after the base-ctor call. Each `let` binder is a private instance field: the same
    /// lowering a primary-ctor parameter gets, with the value coming from an initialiser
    /// rather than an argument. Empty unless the class declares any; a class with no
    /// primary ctor cannot have one at all (registration diagnoses it, as F# does).
    member val InstancePreamble: ClassPreambleEntry[] = [||] with get, set
    /// Secondary constructors in declaration
    /// order. Stamped by `NameResolution.registerClassTypeDefn`; param types are
    /// linked by Unification's `fillClassMembers`; `Elaborate` projects each onto a
    /// `TSecondaryCtor`. Empty unless the class declares `new(...)` overloads.
    member val SecondaryCtors: ClassSecondaryCtorInfo[] = [||] with get, set
    /// True when the class declares a *primary* constructor (`type T(args) =` /
    /// `type T() =`); false for the `val`-field form (`type T = val …; new(…) =`)
    /// whose only ctors are secondaries. Stamped by `registerClassTypeDefn` from the
    /// parsed `PrimaryConstrArgs` presence; `Elaborate` projects it onto
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
    /// block. `Elaborate` projects them onto `TTypeKind.Class.interfaces` for codegen
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
    /// Stamped by `registerClassTypeDefn`; `Elaborate`
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
    /// `registerClassTypeDefn` (implies `IsValueType`); `Elaborate` projects it onto
    /// `TTypeKind.Class.isByRefLike` so codegen stamps
    /// `System.Runtime.CompilerServices.IsByRefLikeAttribute`.
    member val IsByRefLike: bool = false with get, set
    /// Explicit `val [mutable] x: T` instance fields in declaration order, their declared
    /// types resolved, by `registerClassTypeDefn`; `Elaborate` projects each onto a
    /// `TRecordField` in `TTypeKind.Class.fields`. Empty unless the class declares any
    /// `val` fields.
    member val InstanceFields: ClassFieldInfo[] = [||] with get, set
    /// Equality posture for this class. The field default is the reference-class
    /// posture (`Reference`); `NameResolution.registerClassTypeDefn` (a later
    /// phase) computes the kind-aware value — a `[<Struct>]` value type
    /// (`IsValueType = true`) defaults to `Structural`, and `[<CustomEquality>]`
    /// stamps `Custom`. `Unification`/`Elaborate` read this like the
    /// record/union ones.
    member val EqualitySupport = EqualityVerdict.Reference with get, set
    /// Comparison posture for this class. The field default is also the resolved
    /// default: comparison is opt-in, so every class kind (struct included) stays
    /// `NoComparison` unless an attribute overrides it —
    /// `NameResolution.registerClassTypeDefn` (a later phase) stamps `Structural`
    /// for `[<StructuralComparison>]` and `Custom` for `[<CustomComparison>]`.
    /// `Unification`/`Elaborate` read this like the record/union ones.
    member val ComparisonSupport = ComparisonVerdict.NoComparison with get, set

    interface IInterfaceImplHost with
        member this.Key = this.Key
        member this.TypeKey = this.TypeKey
        member this.DeclKey = this.DeclKey
        member this.TypeParams = this.TypeParams
        member this.ThisName = this.ThisName
        member this.ThisKey = this.ThisKey
        member this.InterfaceImpls = this.InterfaceImpls
        member this.Members = this.Members
        member this.EqualitySupport = this.EqualitySupport
        member this.ComparisonSupport = this.ComparisonSupport
        member this.MkSelfType args = TyClass(this.TypeKey, args)

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
/// member-access node's `NodeKey`; `Elaborate` reads it to mint a
/// `TExpr.ExternalMember` carrying the interned `SymbolKey`. `IsStatic`
/// distinguishes `Type.Member` from `value.Member` (drives whether Elaborate keeps
/// the receiver), `Storage` a value member (field/property) from a method value
/// (and, at CLR emission, a `Field` from a `Property`).
///
/// `Signature` is the member's **declared** type in the receiver's instantiation
/// (`ExternalSymbols.openSignature` / `instantiateSignature`): for a method,
/// `TyFun(params → ret)`; for a property, the property type. Elaborate reads it for
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
    /// optional-default gate and Elaborate read; mirrors `ExternalMember.IsValueMember`.
    member m.IsValueMember = m.Storage.IsValueMember
