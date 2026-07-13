namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// The project-wide type-definition registry: records, unions, classes, enums,
// and abbreviations plus their reverse / member indexes, and the arity-keyed
// register/lookup API over them. Populated by NameResolution.registerXxx,
// filled in by Unification, read everywhere downstream.

type PassContextTypes =
    {
        /// Keyed by the type's own project-local `TypeKey` — the WHOLE containment
        /// chain, not a name: a table entry and the `SymbolKey` a consumer carries are
        /// the same value, so `tryRecordByKey` is a genuine key-addressed read.
        /// Field types are filled in by Unification after the registry is populated.
        Record: Dictionary<TypeKey, RecordTypeInfo>
        /// Keyed by `TypeKey` (see `Record`).
        /// Case field types are filled in by Unification after the registry is populated.
        Union: Dictionary<TypeKey, UnionTypeInfo>
        /// Keyed by `TypeKey` (see `Record`). Member types start as placeholder TyVars
        /// and get linked by Unification's `fillClassMembers` pre-pass.
        Class: Dictionary<TypeKey, ClassTypeInfo>
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
        /// Reverse index: record short name (NO arity suffix, as written in source) →
        /// the `TypeKey`s of every registered record claiming it. The only route from a
        /// bare name back to a type: the tables are key-addressed, so a name has a
        /// *candidate set*, not an entry. An arity-overloaded name (`Point\`2` /
        /// `Point\`3`) holds several keys; a bare-name read of it does not resolve
        /// (`tryRecord`). Same shape as `CtorIndex` / `FieldIndex`.
        RecordNames: Dictionary<string, ResizeArray<TypeKey>>
        /// Reverse index: union short name → candidate `TypeKey`s. See `RecordNames`.
        UnionNames: Dictionary<string, ResizeArray<TypeKey>>
        /// Reverse index: class / interface short name → candidate `TypeKey`s. See
        /// `RecordNames`.
        ClassNames: Dictionary<string, ResizeArray<TypeKey>>
        /// Uniqueness witness for project-local `SymbolKey`s.
        /// Maps each minted `TypeKey(None, ns, name\`arity)` → the decl-site
        /// `NodeKey` that first minted it. Stamped through `TypeRegistry.recordKeyOrigin`
        /// as each type registers; a second *distinct* declaration minting the same key
        /// is a uniqueness violation (a missing/wrong `ns` in the mint, not a user
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
            RecordNames = Dictionary<_, _>()
            UnionNames = Dictionary<_, _>()
            ClassNames = Dictionary<_, _>()
            SymbolKeyOrigins = Dictionary<_, _>()
        }

/// Access to the project-local record / union / class registries. Each is keyed by
/// the type's own `TypeKey`, so containment (and the `` `N `` arity suffix a `TypeKey`
/// carries in its `Name`) is part of the key by construction: `Choice<'T1,'T2>` and
/// `Choice<'T1,'T2,'T3>` are distinct keys, and so are same-named types in different
/// places. A *name* therefore addresses a CANDIDATE SET (`RecordNames` / `UnionNames` /
/// `ClassNames`), never an entry — the three lookup faces below are the only ways in:
///   * by `SymbolKey` (`tryRecordByKey` / …) — exact, the identity a consumer already holds;
///   * by `(name, arity)` (`tryRecordArity` / `tryUnion` / `tryClassArity`) — exact;
///   * by BARE name (`tryRecord` / `tryUnionBare` / `tryClass`) — resolves only when the
///     name is unambiguous (see `tryKeyOfBareName`).
/// Only enums and abbreviations stay bare-name-keyed (neither is arity-overloadable).
module TypeRegistry =

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

    // --- Key-addressed mechanism (shared by Record, Union and Class) --------------
    // A `table` + its short-name `index` are the only things that differ between the
    // three kinds, so the mechanism lives here once. The table is keyed by `TypeKey`;
    // the index maps the source-written short name to the keys claiming it.

    /// Register `info` under its own `TypeKey` and index that key under `name` (the
    /// short name as written, no arity suffix — the key carries the arity). Idempotent:
    /// re-registering the same key refreshes the entry and does not duplicate the
    /// index candidate.
    let private registerKeyed
        (table: Dictionary<TypeKey, 'T>)
        (index: Dictionary<string, ResizeArray<TypeKey>>)
        (name: string)
        (key: TypeKey)
        (info: 'T)
        : unit =
        table.[key] <- info

        match index.TryGetValue name with
        | true, keys ->
            if not (keys.Contains key) then
                keys.Add key
        | false, _ ->
            let keys = ResizeArray 1
            keys.Add key
            index.[name] <- keys

    /// The key a name claims at EXACTLY this arity, if any. The `TypeKey`'s `Name` is
    /// the arity-qualified name (`SymbolKeyOps.arityName` is the one rule both the mint
    /// and this read use), so the arity test is a name test on the candidates.
    let private tryKeyOfArity
        (index: Dictionary<string, ResizeArray<TypeKey>>)
        (name: string)
        (arity: int)
        : TypeKey voption =
        match index.TryGetValue name with
        | true, keys ->
            let arityName = SymbolKeyOps.arityName name arity
            let i = keys.FindIndex(fun k -> k.Name = arityName)
            if i < 0 then ValueNone else ValueSome keys.[i]
        | false, _ -> ValueNone

    /// What a BARE (arity-less) short name resolves to. A NON-GENERIC type owns its
    /// short name outright — its key's `Name` *is* the bare name — so it wins whenever
    /// one exists; failing that a lone candidate resolves; and an arity-overloaded name
    /// (`Point\`2` / `Point\`3`) is genuinely ambiguous unqualified, so it resolves to
    /// NOTHING and the caller must come with an arity or a key.
    let private tryKeyOfBareName (index: Dictionary<string, ResizeArray<TypeKey>>) (name: string) : TypeKey voption =
        match index.TryGetValue name with
        | true, keys ->
            let i = keys.FindIndex(fun k -> k.Name = name)

            if i >= 0 then ValueSome keys.[i]
            elif keys.Count = 1 then ValueSome keys.[0]
            else ValueNone
        | false, _ -> ValueNone

    let private tryOfKey (table: Dictionary<TypeKey, 'T>) (key: TypeKey voption) : 'T voption =
        match key with
        | ValueSome k ->
            match table.TryGetValue k with
            | true, info -> ValueSome info
            | false, _ -> ValueNone
        | ValueNone -> ValueNone

    /// Resolve a type by its project-local `SymbolKey` — a real key-addressed read: the
    /// `TypeKey` a consumer carries IS the table's key, containment and arity suffix
    /// included, so a same-named type declared elsewhere cannot answer for it. A
    /// non-`TypeKey` key never names a type, so it misses.
    let private tryByTypeKey (table: Dictionary<TypeKey, 'T>) (key: SymbolKey) : 'T voption =
        match key with
        | SymbolKey.Type t ->
            match table.TryGetValue t with
            | true, info -> ValueSome info
            | false, _ -> ValueNone
        | _ -> ValueNone

    // --- Records / unions / classes -----------------------------------------------
    // All three are arity-overloadable in F# (`Point\`2`/`Point\`3`,
    // `Choice\`2`/`Choice\`3`, `Fun\`2`/`Fun\`3`) and all three are keyed by their own
    // `TypeKey`, which carries the arity in its `Name`. Only abbreviations and enums
    // stay bare-name-keyed (neither is arity-overloaded).

    /// Register a record under its own `TypeKey`, indexing that key under the record's
    /// short name. The key comes off the `info` — there is no second spelling of the
    /// identity to drift from it.
    let registerRecord (types: PassContextTypes) (info: RecordTypeInfo) : unit =
        registerKeyed types.Record types.RecordNames info.Name info.TypeKey info

    /// True iff a record claims exactly `(name, arity)` — the record half of the
    /// duplicate-definition test (`containsAnyType`).
    let containsRecord (types: PassContextTypes) (name: string) (arity: int) : bool =
        (tryKeyOfArity types.RecordNames name arity).IsSome

    /// Resolve a record by BARE short name (see `tryKeyOfBareName`): the non-generic
    /// record of that name, else the lone candidate, else nothing — an arity-overloaded
    /// name does not resolve unqualified. Callers holding a key use `tryRecordByKey`;
    /// those with a use-site arity use `tryRecordArity`.
    let tryRecord (types: PassContextTypes) (name: string) : RecordTypeInfo voption =
        tryOfKey types.Record (tryKeyOfBareName types.RecordNames name)

    /// Resolve a record by `(name, arity)` — exact arity, so a wrong arity misses.
    let tryRecordArity (types: PassContextTypes) (name: string) (arity: int) : RecordTypeInfo voption =
        tryOfKey types.Record (tryKeyOfArity types.RecordNames name arity)

    /// Resolve a record by its project-local `SymbolKey` — the reader-side companion
    /// to `tryUnionByKey`/`tryClassByKey`. See `tryByTypeKey`.
    let tryRecordByKey (types: PassContextTypes) (key: SymbolKey) : RecordTypeInfo voption =
        tryByTypeKey types.Record key

    /// Register a class under its own `TypeKey`. See `registerRecord`.
    let registerClass (types: PassContextTypes) (info: ClassTypeInfo) : unit =
        registerKeyed types.Class types.ClassNames info.Name info.TypeKey info

    /// True iff a class claims exactly `(name, arity)` — the class half of the
    /// duplicate-definition test (`containsAnyType`).
    let containsClass (types: PassContextTypes) (name: string) (arity: int) : bool =
        (tryKeyOfArity types.ClassNames name arity).IsSome

    /// Resolve a class by BARE short name (see `tryKeyOfBareName`). The
    /// recognition-only call sites (`Scope.fs`, `NameResolution.fs`, qualified-static
    /// heads) read the registry this way; a caller holding a key uses `tryClassByKey`.
    let tryClass (types: PassContextTypes) (name: string) : ClassTypeInfo voption =
        tryOfKey types.Class (tryKeyOfBareName types.ClassNames name)

    /// Resolve a class by `(name, arity)` — exact arity, so a wrong arity misses.
    let tryClassArity (types: PassContextTypes) (name: string) (arity: int) : ClassTypeInfo voption =
        tryOfKey types.Class (tryKeyOfArity types.ClassNames name arity)

    /// Resolve a class by its project-local `SymbolKey` — the class analogue of
    /// `tryUnionByKey`. See `tryByTypeKey`.
    let tryClassByKey (types: PassContextTypes) (key: SymbolKey) : ClassTypeInfo voption = tryByTypeKey types.Class key

    /// True iff a class is registered under this `SymbolKey` — the key-based membership
    /// gate (the `tryClassByKey`-shaped mirror of `containsClass`). A caller holding a
    /// `TyClass` key uses this so a local class is never misclassified as external.
    let containsClassKey (types: PassContextTypes) (key: SymbolKey) : bool = (tryByTypeKey types.Class key).IsSome

    /// Resolve a nominal type that may carry `interface … with` impls — a class,
    /// union, *or* record — by its `SymbolKey`, surfaced as the shared
    /// `IInterfaceImplHost`. The `subsumes` interface-admission walk uses this so a
    /// union's/record's declared interfaces participate in subtyping exactly like a
    /// class's. Class → union → record: a key can only be registered in one of the
    /// three (the duplicate-definition test forbids a name+arity claimed twice), so the
    /// order is a search order, not a precedence.
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

    /// Register a union under its own `TypeKey`. See `registerRecord`.
    let registerUnion (types: PassContextTypes) (info: UnionTypeInfo) : unit =
        registerKeyed types.Union types.UnionNames info.Name info.TypeKey info

    /// True iff a union claims exactly `(name, arity)` — the union half of the
    /// duplicate-definition test.
    let containsUnion (types: PassContextTypes) (name: string) (arity: int) : bool =
        (tryKeyOfArity types.UnionNames name arity).IsSome

    /// Is a record / union / class of this short name registered at ANY arity? A
    /// short-name index entry exists iff some type of that kind claims the name, so the
    /// presence of the key IS the answer.
    let private containsAnyArity (types: PassContextTypes) (name: string) : bool =
        types.RecordNames.ContainsKey name
        || types.UnionNames.ContainsKey name
        || types.ClassNames.ContainsKey name

    /// THE duplicate-type-definition test. Every registration site (record, union,
    /// enum, abbreviation, class) asks this one question with its own declared
    /// `arity`, so a new kind cannot be wired into one guard and forgotten in the
    /// others — and so a duplicate is always rejected BEFORE `stampLocalTypeKey`,
    /// keeping that function's SymbolKey-collision diagnostic unreachable from user
    /// source (it is an internal-error backstop, not a user diagnostic).
    ///
    /// The rule: a declaration claims a *name at an arity*, and a claim may be held
    /// by at most one type of any kind.
    ///   * Records, unions and classes are arity-overloadable (`Foo` and ``Foo`1``
    ///     are distinct types), so each claims exactly `(name, arity)`.
    ///   * An enum is non-generic, so it claims `(name, 0)`: it collides with a
    ///     record `Foo` but NOT with a record ``Foo`1``.
    ///   * An abbreviation — and an inline-IL intrinsic repr — is keyed by BARE name in
    ///     its table and resolved by bare name at every use site, so its claim is
    ///     arity-BLIND. `containsAbbrev` below therefore ignores the arity, and the
    ///     abbreviation's OWN guard must be `containsAnyTypeBare`, not this — the claim
    ///     has to be blind in BOTH directions or it is not a claim at all.
    let containsAnyType (types: PassContextTypes) (name: string) (arity: int) : bool =
        containsRecord types name arity
        || containsUnion types name arity
        || containsClass types name arity
        || (arity = 0 && containsEnum types name)
        || containsAbbrev types name
        || types.IntrinsicReprTypes.ContainsKey name

    /// The duplicate test for a declaration whose claim is BARE — an abbreviation or an
    /// inline-IL intrinsic repr. Its table has no arity in its key and every use site
    /// resolves it by bare name, so it claims the name at EVERY arity and collides with
    /// a same-named type of any kind at any arity.
    ///
    /// This is not pedantry about a symmetry: `containsAnyType` alone would let a
    /// generic `type Foo<'a> = …` alias register alongside a non-generic record `Foo`
    /// (their arities differ, so no arity-precise check fires) — and a bare `Foo` at a
    /// use site then resolves through `Abbreviation`, which `resolveBareTypeName` checks
    /// BEFORE `Record`, yielding the generic alias applied to no arguments.
    let containsAnyTypeBare (types: PassContextTypes) (name: string) : bool =
        containsAnyArity types name
        || containsEnum types name
        || containsAbbrev types name
        || types.IntrinsicReprTypes.ContainsKey name

    /// Resolve a union by `(name, arity)` — exact arity, so a wrong arity misses (the
    /// caller diagnoses).
    let tryUnion (types: PassContextTypes) (name: string) (arity: int) : UnionTypeInfo voption =
        tryOfKey types.Union (tryKeyOfArity types.UnionNames name arity)

    /// Resolve a union by BARE short name (see `tryKeyOfBareName`) — the union sibling
    /// of `tryRecord` / `tryClass`, for the recognition-only call sites (a qualified
    /// ctor / static head, the module-vs-type name test).
    let tryUnionBare (types: PassContextTypes) (name: string) : UnionTypeInfo voption =
        tryOfKey types.Union (tryKeyOfBareName types.UnionNames name)

    /// Resolve a union by its project-local `SymbolKey` — the `TypeKey` minted onto
    /// `UnionTypeInfo.Key` and stamped into `Resolution.ResolvedType`. A non-`TypeKey`
    /// key (a value / member) never names a union, so it misses. The reader-side seam
    /// for SymbolKey-first resolution.
    let tryUnionByKey (types: PassContextTypes) (key: SymbolKey) : UnionTypeInfo voption = tryByTypeKey types.Union key

    /// Resolve a union, record, or inline intrinsic-abbrev host (NOT a class) by bare
    /// short name as the shared `IInterfaceImplHost`. The non-class analogue of the
    /// `tryClassLikeDecl` path: the host-body passes route their
    /// `TypeDefnPatterns.tryNonClassMemberHostDecl` match through this one lookup, so they
    /// share one bare-name convention. (The subtype walk resolves the same hosts by
    /// `SymbolKey` via `tryInterfaceImplHostByKey`.)
    /// The intrinsic-abbrev host is admitted so `type X = (# … #) with member …` fills
    /// and elaborates its members on the SAME `fillHostMembers` path — its `MkSelfType`
    /// yields the abbrev's `TyConst` identity, so the members' self-type stays intrinsic.
    /// Classes are excluded — they fill and resolve through their own richer path
    /// (`fillClassMembers` / `walkClassBodies`), so admitting one here would double-fill.
    let tryNonClassMemberHost (types: PassContextTypes) (name: string) : IInterfaceImplHost voption =
        match tryUnionBare types name with
        | ValueSome info -> ValueSome(info :> IInterfaceImplHost)
        | ValueNone ->
            match tryRecord types name with
            | ValueSome info -> ValueSome(info :> IInterfaceImplHost)
            | ValueNone ->
                match types.IntrinsicAbbrevHost.TryGetValue name with
                | true, info -> ValueSome(info :> IInterfaceImplHost)
                | false, _ -> ValueNone

    /// The declaring union of a registered case, resolved by its `(UnionName,
    /// UnionArity)` — the pair the case was stamped with at registration, so this is
    /// total: a case cannot exist without its union.
    let unionOfCase (types: PassContextTypes) (info: UnionCaseInfo) : UnionTypeInfo =
        match tryUnion types info.UnionName info.UnionArity with
        | ValueSome u -> u
        | ValueNone -> failwithf "Internal error: union case '%s' has no registered union '%s'" info.Name info.UnionName

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
