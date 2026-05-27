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
/// `<Name>Module`. See docs/selfhost-handoff.md (R3 "compile `fold` into the core").
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
        typeParams: (string * TypeVar) list,
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
    member val MethodTypeParams: (string * TypeVar) list = [] with get, set

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
        typeParams: (string * TypeVar) list,
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
        typeParams: (string * TypeVar) list,
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

[<Sealed>]
type ClassTypeInfo
    (
        name: string,
        typeParams: (string * TypeVar) list,
        ctorParams: ClassCtorParamInfo[],
        members: TypeMemberInfo[],
        declKey: NodeKey,
        thisName: string,
        thisKey: NodeKey
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
        /// Reverse index: ctor name → list of case-info entries (each tagged with
        /// the declaring union type).
        CtorIndex: Dictionary<string, UnionCaseInfo list>
        /// Reverse index: field name → list of record types that declare it.
        FieldIndex: Dictionary<string, RecordTypeInfo list>
        /// Reverse index: member name → list of declaring (class, member) entries.
        /// Used only for ambiguity diagnostics when a receiver's type is free and
        /// the member name occurs in multiple classes.
        ClassMemberIndex: Dictionary<string, ClassMemberIndexEntry list>
        /// Maps the Vesper type name to its target representation (the inline-IL
        /// string), from an intrinsic-binding abbrev (`type int = (# "System.Int32" #)`).
        /// Unlike `Abbreviation`, these are NOT transparent: a use site resolves to
        /// `TyConst name`, not the RHS — the binding records *how the target
        /// represents* the type, not an alias to expand. Input to the future
        /// `encodeType` rekey; see docs/self-host-rung1-plan.md.
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
        /// element). Seeded from the provider's `IAmbientOpenScope` (the
        /// referenced-contract `[<AutoOpen>]` modules / prelude), empty when the
        /// provider surfaces none. Both NameResolution and Unification must read
        /// the *same* seed, so it can't be the per-element `OpenScope` they mutate.
        mutable AmbientOpenScope: OpenScope
        /// Per-signature type-parameter scope: each signature opens its own scope
        /// and restores the prior one on exit. Anonymous typars (`_`) never enter
        /// the scope — they're fresh per occurrence. See docs/generics-plan.md
        /// §"Typar scope".
        mutable TyparScope: Dictionary<string, TypeVar>
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
    }

module PassContextResolution =
    let create (ambient: OpenScope) : PassContextResolution =
        {
            OpenScope = ambient
            AmbientOpenScope = ambient
            TyparScope = Dictionary<string, TypeVar>(System.StringComparer.Ordinal)
            TyparScopeStrict = false
            ExternalAccess = SideTable<_>()
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
    // Seed the ambient (implicit-open) prelude from the provider if it surfaces
    // one (`IAmbientOpenScope` — the referenced-contract `[<AutoOpen>]` modules /
    // FSharp.Core prelude). This is the single seam: every path that builds a
    // `PassContext` (the pipeline and the direct-construction tests alike) picks
    // it up here. Providers without an implicit prelude contribute the empty
    // ambient, so resolution is unchanged for them. The ambient sits at the tail
    // of the prefix list, so explicit `open`s the pass walk prepends are tried
    // first (symbol-resolution-handoff.md, open-resolution).
    let ambientOpenScope =
        match box provider with
        | :? IAmbientOpenScope as a ->
            { OpenScope.empty with
                Prefixes = a.AmbientOpenPrefixes
            }
        | _ -> OpenScope.empty

    member val Provider = provider
    member val Input = input
    member val Lexed = lexed
    member val Diagnostics = ResizeArray<Diagnostic>() with get
    member val Types = PassContextTypes.empty () with get
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
    member val StaticOpt = SideTable<TStaticOptConstraint list>() with get
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
