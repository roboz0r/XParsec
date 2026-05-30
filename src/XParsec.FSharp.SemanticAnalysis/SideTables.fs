namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// Side tables hold all in-flight semantic information. CST is never mutated.
// See docs/architecture.md.

/// Where a module-level `let` should be emitted: a *named* holder type (an F#
/// module compiles to a static class) rather than the anonymous "Program" holder
/// the backend uses for top-level functions. Recorded for every binding inside a
/// `module Foo = …`; the backend keys this off the binding's `NodeKey` to give the
/// emitted static method its source `Name` on the `Holder` type in `Namespace`
/// (e.g. `Vesper.Collections.ListModule::fold`). The `Module` suffix follows the
/// F# rule that a module sharing a name with a type in its namespace compiles to
/// `<Name>Module`.
type ModuleMemberInfo =
    {
        Namespace: string option
        Holder: string
        Name: string
    }

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
        typarConstraints: TyparConstraints<SyntaxToken> voption
    ) =
    new(name, typeParams, fields, declKey) = RecordTypeInfo(name, typeParams, fields, declKey, ValueNone)
    member val Name = name
    member val TypeParams = typeParams
    member val Fields = fields
    member val DeclKey = declKey
    /// `when 'a : ...` clause attached to the type's typar list, if any.
    /// `Unification.fillRecordFieldTypes` walks this and attaches each
    /// constraint to the matching prototype TyVar in `TypeParams`.
    member val TyparConstraints = typarConstraints
    /// Equality posture per [`docs/records-plan.md`](docs/records-plan.md) §B4.
    /// Filled during `NameResolution.registerRecordTypeDefn` from the type's
    /// attributes; the placeholder defaults to `Structural` so any path that
    /// overlooks the registration (mostly tests that synthesise records
    /// directly) stays equal-by-fields. `Unification.checkConstraint` reads it
    /// to short-circuit `NoEquality` types; `Freeze` projects it onto
    /// `TTypeDecl.EqualitySupport` for codegen.
    member val EqualitySupport = EqualityVerdict.Structural with get, set
    /// Comparison posture per [`docs/records-plan.md`](docs/records-plan.md)
    /// §B6. Filled during `NameResolution.registerRecordTypeDefn` from the
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
type UnionCaseInfo(name: string, unionName: string, fields: SemType[], fieldNames: string voption[], declKey: NodeKey) =
    member val Name = name
    member val UnionName = unionName
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
        typarConstraints: TyparConstraints<SyntaxToken> voption
    ) =
    new(name, typeParams, cases, declKey) = UnionTypeInfo(name, typeParams, cases, declKey, ValueNone)
    member val Name = name
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
    /// Equality posture per [`docs/records-plan.md`](docs/records-plan.md) §B4.
    /// Filled during `NameResolution.registerUnionTypeDefn`; defaults to
    /// `Structural` (the records-plan §B4 / brainstorm §8 rule for unions).
    /// `Unification.checkConstraint` short-circuits on `NoEquality`; `Freeze`
    /// projects it onto `TTypeDecl.EqualitySupport` for codegen.
    member val EqualitySupport = EqualityVerdict.Structural with get, set
    /// Comparison posture per [`docs/records-plan.md`](docs/records-plan.md)
    /// §B6. Filled during `NameResolution.registerUnionTypeDefn` from the
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
        typarConstraints: TyparConstraints<SyntaxToken> voption
    ) =
    new(name, typeParams, rhsCst, declKey) = AbbreviationInfo(name, typeParams, rhsCst, declKey, ValueNone)
    member val Name = name
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
        baseKey: NodeKey
    ) =
    member val Name = name
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
/// (symbol-resolution-plan §7.2). Recorded by `Unification` keyed by the
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

/// How a `for x in src do …` (`TExpr.ForIn`) sources its enumerator — resolved by
/// `Unification.inferForIn` and read by `Freeze` to enrich the node, because
/// codegen can't re-derive the struct-vs-interface decision from the element type
/// alone (vesper-set-sprint-phase-4 §4.2/§4.4). Defined here (ahead of `Tast.fs`
/// in compile order) so both the side table below and the `TExpr.ForIn` field can
/// name it.
[<RequireQualifiedAccess>]
type ForInEnumerator =
    /// The §4.2 interface path: lower through the `IEnumerable<'T>` /
    /// `IEnumerator<'T>` interface slots with `callvirt`. The default for the
    /// range form and for every source whose enumerable surface is (or includes)
    /// the interface — the only path codegen emits today.
    | Interface
    /// The §4.4 duck-typed path: the source exposes a public parameterless
    /// `GetEnumerator()` returning `EnumeratorTy`, which itself exposes
    /// `MoveNext(): bool` and a `Current` property *without* the source
    /// implementing `IEnumerable<'T>` (C#'s non-boxing `foreach`). The member
    /// `SymbolKey`s are the provider-interned identities (`GetEnumerator` on the
    /// source; `MoveNext` / `Current` on `EnumeratorTy`). `IsValueType` selects
    /// value-receiver emission (`ldloca` + `constrained.`/`call`); `Dispose` is
    /// `ValueSome key` iff `EnumeratorTy : IDisposable`, else the `finally` is
    /// elided. **Scaffolding + front end only** — codegen value-type emission is
    /// deferred to a dedicated session (see the §4.4 codegen handoff).
    | DuckTyped of
        enumeratorTy: SemType *
        getEnumerator: SymbolKey *
        moveNext: SymbolKey *
        current: SymbolKey *
        isValueType: bool *
        dispose: SymbolKey voption

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
        }

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
        /// See docs/symbol-resolution-handoff.md (open-resolution). Seeded to the
        /// provider's ambient prelude (below) so a pass that reads it before the
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
        /// stamping the resolved `SymbolKey` (symbol-resolution-plan §7.2, P3). Absent
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
    // prefix list, so explicit `open`s the pass walk prepends are tried first
    // (symbol-resolution-handoff.md, open-resolution).
    let ambientOpenScope =
        { OpenScope.empty with
            Prefixes = provider.AmbientOpenPrefixes
        }

    // `IntrinsicReprTypes` holds ONLY this compilation unit's own intrinsic
    // bindings (`type int = (# "System.Int32" #)`), registered by NameResolution.
    // A *referenced* package's intrinsics are no longer seeded here: they ride
    // the provider as `ExternalTypeShape.Intrinsic` shapes, read local-first /
    // provider-fallback by `subsumes.canonName`, `translateType`, and codegen
    // (intrinsic-repr-handoff.md — first-cut teardown).
    let types = PassContextTypes.empty ()

    member val Provider = provider
    member val Input = input
    member val Lexed = lexed
    member val Diagnostics = ResizeArray<Diagnostic>() with get
    member val Types = types with get
    /// PassContext-lifetime memo of intrinsic-name → canonical repr, populated
    /// lazily by `subsumes.canonName`. `subsumes`' recursive walk would otherwise
    /// round-trip the composite provider / MetadataLoadContext per node to read
    /// an `ExternalTypeShape.Intrinsic` repr; the set is tiny and bounded, so a
    /// per-context cache keyed by name suffices. A name that is neither a local
    /// nor a provider intrinsic caches its own identity. See intrinsic-repr-handoff.md.
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
    /// (every call passed `Code = ""` / `Severity = Error`).
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

/// TODO: range + sub-severities still pending. `Code` lets the sprint group
/// related diagnostics (e.g. for tooling); existing call sites pass `""` —
/// new ones should mint a short identifier (e.g. `"V001"`).
and [<Struct>] Diagnostic =
    {
        Key: NodeKey
        Code: string
        Message: string
        Severity: Severity
    }

and [<Struct>] Severity =
    | Error
    | Warning
    | Info
