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
        /// still resolves exactly as before. This maps the bare short name → what owns
        /// the bare key: the arity of its current alias, `0` when a real *non-generic*
        /// union owns it (so no alias may be installed over it, and the
        /// duplicate-definition test can tell the two apart), or `-1` once a second
        /// arity collides and the alias is withdrawn (the name is then only resolvable
        /// by its arity-key). Internal to `TypeRegistry`; not read elsewhere.
        UnionBareArity: Dictionary<string, int>
        /// Bookkeeping for the bare-name alias `Class` keeps for arity-overloaded
        /// classes / interfaces (`Fun\`2` vs `Fun\`3`). Mirrors `UnionBareArity`
        /// exactly: `Class` is keyed by `TypeRegistry.keyFor` (bare name for a
        /// non-generic class, ``name`N`` for arity N>0); a *single* generic arity
        /// of a name additionally registers a bare-name alias so every existing
        /// single-arity class read by bare name still resolves. Same `alias arity` /
        /// `0` (non-generic owner) / `-1` (withdrawn) encoding as `UnionBareArity`.
        /// Internal to `TypeRegistry`.
        ClassBareArity: Dictionary<string, int>
        /// Bookkeeping for the bare-name alias `Record` keeps for arity-overloaded
        /// records (`Point\`2` vs `Point\`3`). Mirrors `UnionBareArity` /
        /// `ClassBareArity` exactly. Internal to `TypeRegistry.registerRecord`.
        RecordBareArity: Dictionary<string, int>
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
    /// bare-name alias. `bareArity` records what OWNS the bare key `name`, which
    /// `keyFor` gives both to a non-generic type and to a generic type's alias:
    ///   * `0`  — a real non-generic type; there is no alias slot to hand out.
    ///   * `N`  — the alias of the single registered arity `N`.
    ///   * `-1` — demoted: a second arity collided, the alias is withdrawn, and the
    ///            name resolves only by its arity-key.
    /// Idempotent for a repeat `(name, arity)`.
    let private registerArityKeyed
        (table: Dictionary<string, 'T>)
        (bareArity: Dictionary<string, int>)
        (name: string)
        (arity: int)
        (info: 'T)
        : unit =
        table.[keyFor name arity] <- info

        if arity = 0 then
            // The non-generic type owns the bare key outright (it IS its arity-key),
            // evicting any alias a generic namesake had installed there.
            bareArity.[name] <- 0
        else
            match bareArity.TryGetValue name with
            | false, _ ->
                table.[name] <- info
                bareArity.[name] <- arity
            | true, a when a = arity -> table.[name] <- info // refresh the same-arity alias
            | true, 0 -> () // the bare key is a non-generic type's own: no alias to give
            | true, -1 -> () // already demoted: only the arity-key resolves
            | true, _ ->
                // A second distinct arity for this short name: withdraw the now-
                // ambiguous bare alias; both arities resolve only by their key.
                table.Remove name |> ignore
                bareArity.[name] <- -1

    /// True iff EXACTLY `(name, arity)` is registered. At arity > 0 the arity-key
    /// answers it. At arity 0 the table key is the BARE name, which a generic
    /// namesake's alias also occupies — so `bareArity`'s `0` (written only by a real
    /// non-generic registration) is the witness, and a generic `Foo\`1` alias does not
    /// masquerade as a non-generic `Foo`.
    let private containsArity
        (table: Dictionary<string, 'T>)
        (bareArity: Dictionary<string, int>)
        (name: string)
        (arity: int)
        : bool =
        if arity = 0 then
            match bareArity.TryGetValue name with
            | true, 0 -> true
            | _ -> false
        else
            table.ContainsKey(keyFor name arity)

    /// Resolve an arity-overloaded type by its project-local `SymbolKey`. The key's
    /// `TypeKey` `name` component *is* the registry key (both route through
    /// `SymbolKeyOps.arityName`), so this reads it VERBATIM — no `simpleName` strip.
    /// A non-`TypeKey` key never names such a type, so it misses.
    let private tryByTypeKey (table: Dictionary<string, 'T>) (key: SymbolKey) : 'T voption =
        match key with
        | SymbolKey.Type t ->
            match table.TryGetValue t.Name with
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

    /// True iff a record with this exact `(name, arity)` is registered (never a
    /// generic namesake's bare alias) — the record half of the duplicate-definition
    /// test (`containsAnyType`).
    let containsRecord (types: PassContextTypes) (name: string) (arity: int) : bool =
        containsArity types.Record types.RecordBareArity name arity

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

    /// True iff a class with this exact `(name, arity)` is registered (never a generic
    /// namesake's bare alias) — the class half of the duplicate-definition test
    /// (`containsAnyType`).
    let containsClass (types: PassContextTypes) (name: string) (arity: int) : bool =
        containsArity types.Class types.ClassBareArity name arity

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

    /// True iff a union with this exact `(name, arity)` is registered (never a generic
    /// namesake's bare alias) — the union half of the duplicate-definition test.
    let containsUnion (types: PassContextTypes) (name: string) (arity: int) : bool =
        containsArity types.Union types.UnionBareArity name arity

    /// Is a record / union / class of this short name registered at ANY arity? The
    /// `*BareArity` witness answers it: every registration writes an entry (`0` for a
    /// non-generic, `N` for a single generic arity, `-1` once two collide), so the
    /// presence of the key IS "this kind claims this name somewhere".
    let private containsAnyArity (types: PassContextTypes) (name: string) : bool =
        types.RecordBareArity.ContainsKey name
        || types.UnionBareArity.ContainsKey name
        || types.ClassBareArity.ContainsKey name

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
