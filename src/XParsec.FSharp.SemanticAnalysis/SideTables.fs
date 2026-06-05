namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// Side tables hold all in-flight semantic information. CST is never mutated.
// See docs/architecture.md.

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
    /// type identified by `declKey`. `argSig` is
    /// always empty: project-local members carry no overload set (overload
    /// resolution is a separate future feature), so `(declKey, name)` is unique. The
    /// local analogue of the external `MemberKey` minted by `MetadataSymbols` /
    /// `VesperLib`; carried on the local member-call TAST nodes so codegen reads the
    /// declaring type off `decl` instead of re-deriving it from a class-name string.
    let ofMember (declKey: SymbolKey) (name: string) (kind: MemberKind) : SymbolKey =
        SymbolKey.MemberKey(declKey, name, EqArray.empty, kind)

// `ModuleMemberInfo` moved to `SideTypes.fs` (it must precede `Tast.fs`).

/// Field types start as fresh TyVars stamped by NameResolution and get linked
/// to the real translated type by Unification before any expression is typed.
[<Sealed>]
type RecordFieldInfo(name: string, ty: SemType, isMutable: bool, declKey: NodeKey) =
    member val Name = name
    member val Type = ty
    member val IsMutable = isMutable
    member val DeclKey = declKey

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
    /// `NameResolution.registerRecordTypeDefn` from the
    /// type's attributes. Defaults to `NoComparison` (brainstorm-comparison §9
    /// opt-in). `Unification.checkConstraint` reads it to reject `<` / `>` /
    /// `<=` / `>=` on un-annotated types; `Freeze` projects it onto
    /// `TTypeDecl.ComparisonSupport` for codegen.
    member val ComparisonSupport = ComparisonVerdict.NoComparison with get, set

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
    member val MethodTypeParams: EqArray<string * TypeVar> = EqArray.empty with get, set
    /// `true` when the source declares the member with `MemberKeyword.Override`
    /// or `MemberKeyword.Default` (the inheritance-plan §Registration flag).
    /// Inert in Phase 1 (B-1) — stamped by Phase 2's `registerInheritedSlots`
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
    /// Equality posture for this union.
    /// Filled during `NameResolution.registerUnionTypeDefn`; defaults to
    /// `Structural` (the brainstorm §8 rule for unions).
    /// `Unification.checkConstraint` short-circuits on `NoEquality`; `Freeze`
    /// projects it onto `TTypeDecl.EqualitySupport` for codegen.
    member val EqualitySupport = EqualityVerdict.Structural with get, set
    /// Comparison posture for this union. Filled during
    /// `NameResolution.registerUnionTypeDefn` from the
    /// type's attributes. Defaults to `NoComparison` (brainstorm-comparison §9
    /// opt-in). `Unification.checkConstraint` reads it to reject `<` / `>` /
    /// `<=` / `>=` on un-annotated types; `Freeze` projects it onto
    /// `TTypeDecl.ComparisonSupport` for codegen.
    member val ComparisonSupport = ComparisonVerdict.NoComparison with get, set

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

/// An explicit instance field declared with `val [mutable] x: T`
/// (vesper-set-sprint-phase-6 / structs-handoff). `Type` starts as a placeholder
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

/// A class-level `static let x = <init>` (vesper-set-sprint-plan §1.8 / B-10).
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

/// A secondary constructor (`new(args) = SelfType(primaryArgs)`,
/// vesper-set-sprint-plan §1.9 / B-11). `Params` are the secondary ctor's own
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

/// A registered `interface IFace with member …` block on a class (B-2,
/// vesper-set-sprint-phase-5 §5.1). `InterfaceCst` is the parsed interface
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
    /// Phase 2's inheritance plumbing (`base.M()` non-virtual dispatch +
    /// `inherit Base(args)` ctor lowering); always allocated, only read when
    /// `BaseType` is `ValueSome`.
    member val BaseKey = baseKey
    /// Parent type from `inherit Base(args)` once resolved. Stays `ValueNone`
    /// in Phase 1 (B-1) — Phase 2's `registerInheritedSlots` walk fills it.
    /// `ValueNone` ⇒ codegen emits `TypeDefinition.BaseType = Object`.
    member val BaseType: SemType voption = ValueNone with get, set
    /// CST expression for the constructor arguments to the base type
    /// (`inherit Base(arg1, arg2)`'s `(arg1, arg2)` shape). `ValueNone` for
    /// classes without an `inherit` clause and Phase 1 placeholders; Phase 2
    /// stamps it from `ClassInheritsDecl.expr`.
    member val BaseCtorArgs: Expr<SyntaxToken> voption = ValueNone with get, set
    /// `[<Sealed>]` (vesper-set-sprint-plan §1.6 / B-8). Stamped by
    /// `NameResolution.registerClassTypeDefn` from the type's attributes;
    /// `Freeze` projects it onto `TTypeKind.Class.isSealed` so codegen flips
    /// `TypeAttributes.Sealed` on the emitted `TypeDefinition`.
    member val IsSealed: bool = false with get, set
    /// Class-level `static let` bindings (vesper-set-sprint-plan §1.8 / B-10) in
    /// declaration order. Stamped by `NameResolution.registerClassTypeDefn` from
    /// the class's `classPreamble`; types are linked by Unification's
    /// `fillClassMembers`; `Freeze` projects each onto a `TStaticLet`. Empty unless
    /// the class declares `static let`s. Generic classes reject `static let` (the
    /// per-instantiation cache lowering is deferred), so this is only populated for
    /// monomorphic classes.
    member val StaticLets: ClassStaticLetInfo[] = [||] with get, set
    /// Secondary constructors (vesper-set-sprint-plan §1.9 / B-11) in declaration
    /// order. Stamped by `NameResolution.registerClassTypeDefn`; param types are
    /// linked by Unification's `fillClassMembers`; `Freeze` projects each onto a
    /// `TSecondaryCtor`. Empty unless the class declares `new(...)` overloads.
    member val SecondaryCtors: ClassSecondaryCtorInfo[] = [||] with get, set
    /// `[<AllowNullLiteral>]` (vesper-set-sprint-plan §1.6 / B-8). Stamped
    /// by `NameResolution.registerClassTypeDefn` from the type's attributes;
    /// read only by Unification's `Expr.Null` arm so `null` unifies with the
    /// class. Never reaches codegen (no IL flag for it).
    member val AllowNullLiteral: bool = false with get, set
    /// `interface IFace with member …` blocks (B-2, vesper-set-sprint-phase-5).
    /// Stamped by `NameResolution.registerClassTypeDefn`; each impl's interface
    /// type is resolved + verified, and its member bodies typed, by Unification's
    /// `fillClassMembers`. Empty unless the class declares an `interface … with`
    /// block. `Freeze` projects them onto `TTypeKind.Class.interfaces` for codegen
    /// (Step 5.3, deferred).
    member val InterfaceImpls: ClassInterfaceImplInfo[] = [||] with get, set
    /// `[<Struct>]` (or the `type X = struct … end` shape,
    /// vesper-set-sprint-phase-6). Stamped by `registerClassTypeDefn`; `Freeze`
    /// projects it onto `TTypeKind.Class.isStruct` so codegen emits a
    /// `System.ValueType`-based value type. A struct is implicitly sealed.
    member val IsValueType: bool = false with get, set
    /// Explicit `val [mutable] x: T` instance fields in declaration order
    /// (vesper-set-sprint-phase-6). Stamped by `registerClassTypeDefn`; field
    /// types are linked by Unification's `fillClassMembers`; `Freeze` projects
    /// each onto a `TRecordField` in `TTypeKind.Class.fields`. Empty unless the
    /// class declares any `val` fields.
    member val InstanceFields: ClassFieldInfo[] = [||] with get, set

/// One entry in `PassContextTypes.ClassMemberIndex` — the declaring class
/// paired with the matching `TypeMemberInfo`. Promoted from a 2-tuple ahead of
/// the interface-impl sprint so a third field (e.g. the interface the impl
/// satisfies) lands by extending the record rather than churning every caller.
/// See [`docs/pre-sprint-cleanup.md`](docs/pre-sprint-cleanup.md) P2.12.
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
/// the receiver), `IsProperty` a property get from a method value.
[<Struct>]
type ResolvedExternalMember =
    {
        Key: SymbolKey
        IsStatic: bool
        IsProperty: bool
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

    member _.ContainsKey(key: NodeKey) = dict.ContainsKey key

    /// Callers must treat the returned dictionary as read-only once Freeze starts.
    member _.AsDictionary() : IReadOnlyDictionary<NodeKey, 'V> = dict :> _

/// Type-definition side tables: the project-wide registry of records, unions,
/// classes, and abbreviations plus their reverse / member indexes. Populated by
/// `NameResolution.registerXxx`, filled in by `Unification`, read everywhere
/// downstream. Grouped here so the class sprint can add new tables in one
/// place. See [`docs/pre-sprint-cleanup.md`](docs/pre-sprint-cleanup.md) P2.3.
type PassContextTypes =
    {
        /// Field types are filled in by Unification after the registry is populated.
        Record: Dictionary<string, RecordTypeInfo>
        /// Case field types are filled in by Unification after the registry is populated.
        Union: Dictionary<string, UnionTypeInfo>
        /// Member types start as placeholder TyVars and get linked by Unification's
        /// `fillClassMembers` pre-pass.
        Class: Dictionary<string, ClassTypeInfo>
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
            Abbreviation = Dictionary<_, _>()
            CtorIndex = Dictionary<_, _>()
            FieldIndex = Dictionary<_, _>()
            ClassMemberIndex = Dictionary<_, _>()
            IntrinsicReprTypes = Dictionary<_, _>()
            UnionBareArity = Dictionary<_, _>()
            SymbolKeyOrigins = Dictionary<_, _>()
        }

/// Arity-aware access to the project-local *union* registry. F# (and .NET) let a
/// type name be overloaded by generic arity — `Choice<'T1,'T2>` and
/// `Choice<'T1,'T2,'T3>` are distinct types `Choice\`2` / `Choice\`3`. The bare
/// `Dictionary<string, _>` keys would collapse them onto `"Choice"`, so unions are
/// keyed by `keyFor name arity`. A *single*-arity name also keeps a bare-name alias
/// (so every existing single-arity lookup by bare name keeps working unchanged);
/// the alias is withdrawn once a second arity registers (`UnionBareArity`). Only
/// unions are arity-overloaded today (records / classes / abbreviations stay bare).
module TypeRegistry =

    /// The .NET-style key: the bare name for a non-generic type, ``name`N`` for
    /// arity N>0. Matches the emitted metadata type name. Delegates to
    /// `SymbolKeyOps.arityName` so the registry key and the stamped `SymbolKey`
    /// name share one rule.
    let keyFor (name: string) (arity: int) : string = SymbolKeyOps.arityName name arity

    // --- Records / classes / abbreviations --------------------------------------
    // These aren't arity-overloaded today (unlike unions), so the key is the bare
    // short name. The wrappers exist so project-local *identity creation* for every
    // type kind funnels through one place — the single seam an arity key (or a
    // declaring-namespace) would be threaded through if these ever overload.
    // The bare-name reads scattered downstream
    // stay direct for now, exactly as the union bare-alias reads do.

    let registerRecord (types: PassContextTypes) (name: string) (info: RecordTypeInfo) : unit =
        types.Record.[name] <- info

    let containsRecord (types: PassContextTypes) (name: string) : bool = types.Record.ContainsKey name

    let tryRecord (types: PassContextTypes) (name: string) : RecordTypeInfo voption =
        match types.Record.TryGetValue name with
        | true, info -> ValueSome info
        | false, _ -> ValueNone

    /// Resolve a record by its `SymbolKey` — the reader-side companion to
    /// `tryUnionByKey`. Records aren't arity-overloaded, so the table is keyed by the
    /// bare simple name; this projects the key's simple name internally so a consumer
    /// holding a `TyRecord(key, _)` carries the key straight through instead of
    /// re-projecting it to a string at every use site.
    let tryRecordByKey (types: PassContextTypes) (key: SymbolKey) : RecordTypeInfo voption =
        tryRecord types (SymbolKeyOps.simpleName key)

    let registerClass (types: PassContextTypes) (name: string) (info: ClassTypeInfo) : unit = types.Class.[name] <- info

    let containsClass (types: PassContextTypes) (name: string) : bool = types.Class.ContainsKey name

    let tryClass (types: PassContextTypes) (name: string) : ClassTypeInfo voption =
        match types.Class.TryGetValue name with
        | true, info -> ValueSome info
        | false, _ -> ValueNone

    /// Resolve a class by its `SymbolKey` — the class analogue of `tryRecordByKey`.
    /// Classes aren't arity-overloaded; the table is keyed by the bare simple name,
    /// projected from the key internally.
    let tryClassByKey (types: PassContextTypes) (key: SymbolKey) : ClassTypeInfo voption =
        tryClass types (SymbolKeyOps.simpleName key)

    let registerAbbrev (types: PassContextTypes) (name: string) (info: AbbreviationInfo) : unit =
        types.Abbreviation.[name] <- info

    let containsAbbrev (types: PassContextTypes) (name: string) : bool = types.Abbreviation.ContainsKey name

    let tryAbbrev (types: PassContextTypes) (name: string) : AbbreviationInfo voption =
        match types.Abbreviation.TryGetValue name with
        | true, info -> ValueSome info
        | false, _ -> ValueNone

    /// Register a union under its arity-key, maintaining the bare-name alias while
    /// the short name is single-arity and withdrawing it once a second arity
    /// collides. Idempotent for a repeat of the same `(name, arity)`.
    let registerUnion (types: PassContextTypes) (name: string) (arity: int) (info: UnionTypeInfo) : unit =
        types.Union.[keyFor name arity] <- info

        if arity > 0 then
            match types.UnionBareArity.TryGetValue name with
            | false, _ ->
                types.Union.[name] <- info
                types.UnionBareArity.[name] <- arity
            | true, a when a = arity -> types.Union.[name] <- info // refresh the same-arity alias
            | true, -1 -> () // already demoted: only the arity-key resolves
            | true, _ ->
                // A second distinct arity for this short name: withdraw the now-
                // ambiguous bare alias; both arities resolve only by their key.
                types.Union.Remove name |> ignore
                types.UnionBareArity.[name] <- -1

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
    let tryUnionByKey (types: PassContextTypes) (key: SymbolKey) : UnionTypeInfo voption =
        match key with
        | SymbolKey.TypeKey(name = name) ->
            match types.Union.TryGetValue name with
            | true, info -> ValueSome info
            | false, _ -> ValueNone
        | _ -> ValueNone

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
        /// Module-level bindings inside a named `module Foo = …` (R3 deferred): each
        /// binding's `NodeKey.Raw` → where its emitted static method belongs (a real
        /// `Foo`/`FooModule` holder type, not the anonymous "Program" holder).
        /// Populated by `Freeze` and snapshotted into `TastFile.ModuleMembers`; the
        /// backend keys off it to name + place a module function (`ListModule::fold`).
        ModuleMembers: Dictionary<uint64, ModuleMemberInfo>
    }

module PassContextBindings =
    let empty () : PassContextBindings =
        {
            Binding = SideTable<_>()
            Scheme = SideTable<_>()
            TypeVar = SideTable<_>()
            Escape = SideTable<_>()
            ModuleMembers = Dictionary<_, _>()
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
        /// the scope — they're fresh per occurrence. See docs/generics-plan.md
        /// §"Typar scope".
        mutable TyparScope: Dictionary<string, TypeVar>
        /// Prototype TyVars (keyed by source name) for the *next* binding's own
        /// `<'C, …>` typars. `inferBinding` mints a fresh scope for a binding's
        /// declared typars; when this seed is set it reuses the prototype TyVar
        /// for a matching name instead of allocating a fresh one. `fillTypeMembers`
        /// sets it from a generic member's `TypeMemberInfo.MethodTypeParams`
        /// (B-12) so the typars flowing into the inferred signature are the same
        /// roots `Freeze` surfaces and codegen installs as the ambient `!!i` set;
        /// `ValueNone` for every other binding (fresh typars, the existing
        /// behaviour). See vesper-set-sprint-plan §1.10.
        mutable BindingTyparSeed: Dictionary<string, TypeVar> voption
        /// When true, `translateType` rejects any `'a` not already present in
        /// `TyparScope` rather than introducing it implicitly. Used by the type-defn
        /// fill-in walk: implicit free typars in a record / DU declaration aren't
        /// legal F# (only `<'a>`-declared typars are). Binding-level scopes keep
        /// this `false`.
        mutable TyparScopeStrict: bool
        /// Keyed by a member-access node's `NodeKey` (`Expr.DotLookup`): the resolved
        /// external member (`TryLookupMember` hit) for a `<externalType>.Member` or
        /// static `Type.Member` access. Freeze reads it to mint a `TExpr.ExternalMember`
        /// stamping the resolved `SymbolKey` (P3). Absent
        /// for project-local member access (resolved via `Types.Class` / `Types.Union`).
        ExternalAccess: SideTable<ResolvedExternalMember>
        /// Keyed by an external-value use-site's `NodeKey` (the `Expr.Ident` /
        /// `Expr.LongIdentOrOp` that resolved through `IExternalSymbolProvider.TryLookup`):
        /// the resolved value's `SymbolKey.ValueKey`. Freeze stamps it onto
        /// `TExpr.External` so codegen can do robust identity checks
        /// (e.g. "is this exactly `Vesper.Printf.printfn`?") instead of
        /// suffix-matching the source-written name
        /// (vesper-set-sprint-plan §0.1 / M1).
        ExternalValue: SideTable<SymbolKey>
        /// Keyed by a `:?` type-test expression's `NodeKey`: the resolved
        /// tested-against type (`Expr.DynamicTypeTest`'s target). The node's own
        /// inferred type is `bool` (the result), so the target type — which
        /// codegen needs for the `isinst` operand — is stashed here by
        /// Unification and read by Freeze to populate `TExpr.TypeTest.testTy`
        /// (inheritance-plan §`:?`).
        TypeTestTargets: SideTable<SemType>
        /// Keyed by a `use` binding's head-pattern `NodeKey`: the `SymbolKey` of the
        /// `Dispose` member to call when the binder's type is *external* (a BCL
        /// disposable). Recorded by `Unification`'s `use`-Dispose resolution and read
        /// by `Freeze` to stamp `TExpr.Use.dispose` (`ValueSome`); absent for a
        /// project-local binder, where Freeze leaves `ValueNone` and codegen takes the
        /// duck-typed direct-call path (vesper-set-sprint-phase-4 §4.3).
        UseDispose: SideTable<SymbolKey>
        /// Keyed by a `for x in src do …` node's `NodeKey`: how the source yields
        /// its enumerator. Recorded by `Unification.inferForIn` and read by `Freeze`
        /// to stamp `TExpr.ForIn.enumerator`. Absent ⇒ `ForInEnumerator.Interface`
        /// (range sources and the §4.2 interface path); present with
        /// `ForInEnumerator.DuckTyped` for a source exposing only a pattern-based
        /// `GetEnumerator()` (vesper-set-sprint-phase-4 §4.4).
        ForInShape: SideTable<ForInEnumerator>
        /// Keyed by a *type-reference* `NodeKey`: the project-local `SymbolKey`
        /// that reference resolves to. Two minting
        /// sites populate it: `NameResolution.registerUnionTypeDefn` stamps the
        /// *decl* site (`DeclType` key) from the union's minted `Key`, and
        /// `translateType` / `resolveNamedGeneric` stamp *use* sites (`TypeNamed` /
        /// `TypeGeneric` keys) as a union annotation resolves. Read by the type-decl
        /// emitter (`Freeze.tryUnionType`) to recover the union by key instead of
        /// re-deriving `(name, arity)`; the use-site stamps are populate-only
        /// groundwork until a Phase 3 consumer keys off them. Unions only for now
        /// (the proven-out case); records / classes / abbrevs follow as their
        /// consumers migrate.
        ResolvedType: SideTable<SymbolKey>
    }

module PassContextResolution =
    let create (ambient: OpenScope) : PassContextResolution =
        {
            OpenScope = ambient
            AmbientOpenScope = ambient
            TyparScope = Dictionary<string, TypeVar>(System.StringComparer.Ordinal)
            BindingTyparSeed = ValueNone
            TyparScopeStrict = false
            ExternalAccess = SideTable<_>()
            ExternalValue = SideTable<_>()
            TypeTestTargets = SideTable<_>()
            UseDispose = SideTable<_>()
            ForInShape = SideTable<_>()
            ResolvedType = SideTable<_>()
        }

/// **Thread-safety:** a `PassContext` is single-threaded — its side tables,
/// `Diagnostics` channel, and the `TypeVar` graph it owns all mutate in
/// place and are not safe to access from multiple threads. Parallelism
/// happens at file granularity by allocating one `PassContext` per file
/// and analysing them concurrently; the shared `IExternalSymbolProvider`
/// is the only object that crosses thread boundaries (and its contract
/// requires thread-safe `TryLookup`). See [`docs/architecture.md`](docs/architecture.md#parallelism).
///
/// The bulk of the per-file state lives in three sub-records grouped by
/// concern: [`Types`](#Types) (project type registry), [`Bindings`](#Bindings)
/// (per-binder side tables), [`Resolution`](#Resolution) (name-resolution
/// scopes). See [`docs/pre-sprint-cleanup.md`](docs/pre-sprint-cleanup.md) P2.3.
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
    // provider-fallback by `subsumes.canonName`, `translateType`, and codegen.
    let types = PassContextTypes.empty ()

    member val Provider = provider
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
    /// PassContext-lifetime memo of intrinsic-name → canonical repr, populated
    /// lazily by `subsumes.canonName`. `subsumes`' recursive walk would otherwise
    /// round-trip the composite provider / MetadataLoadContext per node to read
    /// an `ExternalTypeShape.Intrinsic` repr; the set is tiny and bounded, so a
    /// per-context cache keyed by name suffices. A name that is neither a local
    /// nor a provider intrinsic caches its own identity.
    member val IntrinsicCanonCache = Dictionary<string, string>() with get
    member val Bindings = PassContextBindings.empty () with get
    member val Resolution = PassContextResolution.create ambientOpenScope with get
    member val Desugared = SideTable<DesugaredForm>() with get
    /// Keyed by an `Expr.App` NodeKey; present only for the printf calls P1
    /// lowers inline (literal format, fully applied, a
    /// `StdOut`/`StdErr`/`StringResult` sink, every specifier in
    /// `PrintfSpec.tryHoleFormat`). Absence keeps the existing FSharp.Core path.
    /// See docs/vesper-printf-plan.md.
    member val PrintfApp = SideTable<PrintfSpec.PrintfSink>() with get
    /// Keyed by an `Expr.LibraryOnlyStaticOptimization` NodeKey: the resolved
    /// `when ^T : …` constraints of that one clause (the `and`-joined list), with
    /// the typar / required type translated to `SemType` while the binding's typar
    /// scope is live. Freeze reads it to build each `TExpr.StaticOptimization`
    /// clause; the typar carries the inline binding's quantified root so
    /// `Inline.inlineExpand` can substitute it at the call site. See
    /// docs/operators-plan.md (prereq 3).
    member val StaticOpt = SideTable<EqArray<TStaticOptConstraint>>() with get
    /// Current let-depth (Rémy's levels). Push on entering a binding group's
    /// RHSes, pop after typing them; generalisation uses the pre-push value as
    /// the threshold for "which TyVars do I quantify?".
    member val CurrentLevel = 0 with get, set
    /// Bare-program list literals (R3): each `[…]` whose container type was left
    /// *flexible* (a fresh `TypeVar`, paired with its element type) so a consumer
    /// can drive it — `List.fold`'s `Vesper.Collections.List` parameter flips it to
    /// the Vesper list, otherwise it defaults to FSharp.Core's `list`. Drained by
    /// `Unification.resolveListLiterals` after the walk: a still-free literal links
    /// to the default list, a flipped one has its element reconciled. Programs that
    /// declare their own `list` abbrev never register here (they resolve eagerly).
    member val ListLiterals = ResizeArray<TypeVar * SemType>() with get

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
