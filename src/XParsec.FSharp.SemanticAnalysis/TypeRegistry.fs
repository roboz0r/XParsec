namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// The project-wide type-definition registry: records, unions, classes, enums,
// and abbreviations plus their reverse / member indexes, and the arity-keyed
// register/lookup API over them. Populated by NameResolution.registerXxx,
// filled in by Unification, read everywhere downstream.

/// Which registry a claimed `(name, arity)` was declared into. The name table
/// (`PassContextTypes.TypeClaims`) is kind-agnostic — one claim, one owner, ANY kind —
/// so this is how a claim points back at the table holding its detail. It is also what
/// lets a use-site resolve a name by ASKING the claim rather than probing the kind
/// tables in a fixed precedence order (see `Translate.resolveBareTypeName`).
[<RequireQualifiedAccess>]
type TypeDeclKind =
    | Record
    | Union
    | Class
    | Enum
    | Abbreviation
    /// A `type int = (# "System.Int32" #)` intrinsic binding. Its declared NAME is a
    /// name-table citizen exactly like any other type's; its target-representation
    /// string is not (that is a `IntrinsicReprTypes` side-table concern).
    | IntrinsicRepr

/// The nominal identity of one type declaration — everything about it that is NOT
/// kind-specific. Established by `NameResolutionTypeRegistration.claimTypeIdentity` for
/// every member of a `type … and …` group before any of the group's per-kind registrars
/// run, so the local `SymbolKey` has exactly one mint site, duplicate detection is one
/// predicate over one table, and an intra-group reference that needs only a key and an
/// arity is answerable the moment it is seen.
type TypeIdentity =
    {
        /// The short name as written (no arity suffix — `Key.Name` carries that).
        Name: string
        /// Generic arity. `(Name, Arity)` is the CLAIM: at most one type of any kind
        /// may hold it, so `Foo` and `` Foo`1 `` are distinct claims (as in F#) and may
        /// be held by different kinds.
        Arity: int
        Kind: TypeDeclKind
        /// The declaration site — the diagnostic key and the `DeclKey` stamped onto the
        /// kind-specific `*TypeInfo`. Derived once, from the name token the claim was
        /// read off, and handed to the per-kind registrar with the rest of the identity.
        DeclKey: NodeKey
        /// The project-local `SymbolKey`, minted once by `stampLocalTypeKey`.
        Key: TypeKey
        /// The source offset from which this claim is VISIBLE: a use at offset `u` can see
        /// it iff `VisibleFrom <= u`. F# declaration scoping is file-ordered, and this is
        /// the whole of it — one field, one comparison, no special cases:
        ///
        ///   * it is the first token of the claim's `type … and …` GROUP, not of the
        ///     individual type, so every claim minted from one group shares it. A member
        ///     body inside the group is textually after that token, so it sees its own type
        ///     and its `and`-siblings by position alone — the recursive-group exception
        ///     stops being an exception;
        ///   * except inside a `module rec` / `namespace rec`, where it is the enclosing
        ///     `rec` scope's keyword. `rec` moves the offset earlier and nothing else; the
        ///     lookup does not branch on it.
        VisibleFrom: int
    }

/// An ACCEPTED type declaration: the identity its claim established, paired with the CST
/// it was claimed from. THE input to every per-kind detail registrar — a registrar is
/// HANDED its `TypeIdentity` rather than re-deriving name / arity / key from the CST, so
/// its idea of the type cannot drift from the claim's, and a REJECTED duplicate (which is
/// never claimed) can never reach a registrar at all.
[<NoEquality; NoComparison>]
type ClaimedTypeDefn =
    {
        Identity: TypeIdentity
        Defn: TypeDefn<SyntaxToken>
    }

/// A type declaration REJECTED as a duplicate: an earlier declaration already holds its
/// `(Name, Arity)` claim and keeps it. It registers no detail and mints no `SymbolKey` —
/// a key is precisely what a rejected declaration does not get, which is why this is not a
/// `TypeIdentity`. The duplicate diagnostic is raised where the claim is contested.
///
/// Retained for OBSERVABILITY ONLY (`PassContextTypes.RejectedDuplicates`): a rejected
/// declaration should be inspectable rather than vanish. Name resolution, unification and
/// codegen must NOT read it — the first claimant owns the name, and a reference to a
/// member that existed only on the rejected declaration is a member-not-found diagnostic,
/// which is the correct answer.
[<NoEquality; NoComparison>]
type RejectedTypeDefn =
    {
        Name: string
        Arity: int
        Kind: TypeDeclKind
        DeclKey: NodeKey
        Defn: TypeDefn<SyntaxToken>
    }

type PassContextTypes =
    {
        /// Keyed by the type's own project-local `TypeKey` — the WHOLE containment
        /// chain, not a name: a table entry and the `SymbolKey` a consumer carries are
        /// the same value, so `tryRecordByKey` is a genuine key-addressed read.
        /// Field types are RESOLVED at registration, against the types in scope where the
        /// record is declared.
        Record: Dictionary<TypeKey, RecordTypeInfo>
        /// Keyed by `TypeKey` (see `Record`). Case field types are resolved at registration.
        Union: Dictionary<TypeKey, UnionTypeInfo>
        /// Keyed by `TypeKey` (see `Record`). The class's declared structure (ctor-param
        /// annotations, `val` field types) is resolved at registration; MEMBER types start
        /// as placeholder TyVars and get linked by Unification's `fillClassMembers` — a
        /// member's type is inferred from its body, so it is not structure.
        Class: Dictionary<TypeKey, ClassTypeInfo>
        /// Project-local enum type declarations, keyed by bare short name (enums
        /// are non-generic, so no arity overload). Populated by
        /// `NameResolution.registerEnumTypeDefn`; read by `translateType` (so
        /// `(x: E)` resolves to `TyEnum Key`) and the `E.C1` qualified-access path.
        Enum: Dictionary<string, EnumTypeInfo>
        /// Keyed by `TypeKey` (see `Record`) — an abbreviation is arity-overloadable
        /// like every other kind (`type T = int` coexists with `type T<'a> = …`), so a
        /// bare `T` at a use site cannot reach the generic alias.
        /// A body is forced by the first thing that names the alias (`forceFill`, from
        /// `translateType`), and at the latest when its declaring group closes — an alias
        /// RHS reads its referent's registered detail, so it cannot resolve where it is
        /// written. Abbreviations expand eagerly at every `translateType` lookup, so
        /// downstream passes see the underlying type as if written longhand.
        Abbreviation: Dictionary<TypeKey, AbbreviationInfo>
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
        /// The declared NAME is a `TypeClaims` citizen like any other type's (so a record
        /// `int` collides with `type int = (# … #)`); the repr STRING is not — two types
        /// resolving to the same target repr is an identity/origin question, not a
        /// name-table one.
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
        /// short name — an intrinsic binding is resolved by bare name at every use site
        /// (`IntrinsicReprTypes` / `IntrinsicKeys` are keyed the same way). Populated by
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
        /// Reverse index: abbreviation short name → candidate `TypeKey`s. See `RecordNames`.
        AbbreviationNames: Dictionary<string, ResizeArray<TypeKey>>
        /// THE name table: short name → every `(name, arity)` claim held under it,
        /// regardless of KIND. A type declaration claims a name at an arity, and a claim
        /// may be held by at most one type of any kind — so duplicate detection is one
        /// predicate (`isTypeClaimed`) over this one table, and a kind added later cannot
        /// be wired into some guards and forgotten in others. Populated in source order by
        /// `NameResolutionTypeRegistration.claimTypeIdentity`, which is also the sole mint
        /// site of a project-local type `SymbolKey`.
        ///
        /// While the top-down registration scan is running, this table holds exactly the
        /// types IN SCOPE at the group being registered: every type declared above it, plus
        /// its own `type … and …` group (all of whose names are claimed before any of its
        /// detail registers). That is F#'s file-order type scoping, and it is why a
        /// registration-time miss against this table is a genuine "not defined": nothing
        /// below can answer for the name, so the head is either external or nothing at all
        /// (`NameResolutionTypeHeadStamp.classifyTypeHead`).
        TypeClaims: Dictionary<string, ResizeArray<TypeIdentity>>
        /// Every ACCEPTED type declaration of this unit, in SOURCE order, with the identity
        /// its claim established. Co-populated with `TypeClaims` (one write, `claimType`),
        /// so the list and the name table cannot disagree about which declarations were
        /// accepted. The accepted / rejected pair below it is the whole record of what the
        /// registration scan decided about each `type` in the file; both are observability
        /// only — the registrars are driven from the group being registered, not from here.
        ClaimedTypeDefns: ResizeArray<ClaimedTypeDefn>
        /// The type declarations rejected as duplicates, in source order. Observability
        /// only — see `RejectedTypeDefn`; nothing downstream may read it.
        RejectedDuplicates: ResizeArray<RejectedTypeDefn>
        /// The RECORD / UNION / CLASS short names this unit declares — the names a
        /// `module` of the same name collides with, and so the ONE input (with the
        /// module's own attributes) to the `…Module` suffix rule
        /// (`NameResolutionTypeRegistration.moduleHolderName`).
        ///
        /// Separate from the kind registries — and from `TypeClaims` — because the rule
        /// must give the SAME answer at both of its call sites, and those two sites sit on
        /// opposite sides of registration: the key mint runs DURING the identity pass (the
        /// kind registries do not exist yet, and a `module Foo` may textually precede the
        /// `type Foo` it collides with), the emitter's holder-name site runs long after.
        /// So the set is filled by one sweep of the whole unit BEFORE the first key is
        /// minted, from the same `tryDeclaredTypeName` the claims come from.
        NominalTypeNames: HashSet<string>
        /// Uniqueness witness for project-local `SymbolKey`s.
        /// Maps each minted type `TypeKey` → the decl-site
        /// `NodeKey` that first minted it. Stamped through `TypeRegistry.recordKeyOrigin`
        /// as each type registers; a second *distinct* declaration minting the same key
        /// is a uniqueness violation (a missing/wrong containment in the mint, not a user
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
            AbbreviationNames = Dictionary<_, _>()
            TypeClaims = Dictionary<_, _>()
            ClaimedTypeDefns = ResizeArray<_>()
            RejectedDuplicates = ResizeArray<_>()
            NominalTypeNames = HashSet<_>()
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
/// Abbreviations are keyed the same way. Only enums stay bare-name-keyed — an enum is
/// non-generic, so its claim is always `(name, 0)` and a name addresses at most one.
///
/// Above all three sits the kind-agnostic NAME TABLE (`TypeClaims`): the `(name, arity)`
/// claim each declaration holds. It is the sole duplicate-definition test
/// (`isTypeClaimed`) and the sole route from a use-site name+arity to the type that owns
/// it (`tryTypeClaim`), so cross-kind precedence is a lookup, not a hand-ordered cascade.
///
/// THE by-name / by-key split. F# declaration scoping is file-ordered, so a NAME does not
/// identify a type on its own — it identifies one only as seen FROM somewhere. Every
/// by-name face below therefore takes a `SourcePos`, and answers against the claims
/// visible there (`TypeIdentity.VisibleFrom`); a caller with nowhere to speak from passes
/// `SourcePos.unbounded` and gets the whole-unit view. The `…ByKey` faces take none, and
/// must not: a `SymbolKey` already names a resolved type, so there is no scoping question
/// left to ask. Requiring a position of exactly the by-name faces is what makes an
/// unscoped by-name read impossible to write by accident.
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
    ///
    /// With `tryKeyOfBareName` it is the funnel EVERY kind index (record / union / class /
    /// abbrev) resolves a name through, which is why the use site enters here rather than
    /// at each kind's face: one place decides what a name can see.
    let private tryKeyOfArity
        (index: Dictionary<string, ResizeArray<TypeKey>>)
        (_useSite: SourcePos)
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
    let private tryKeyOfBareName
        (index: Dictionary<string, ResizeArray<TypeKey>>)
        (_useSite: SourcePos)
        (name: string)
        : TypeKey voption =
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

    // --- The name table -------------------------------------------------------------
    // THE rule: a declaration claims a NAME at an ARITY, and a claim may be held by at
    // most one type of ANY kind. Records, unions, classes and abbreviations are
    // arity-overloadable (`Point\`2`/`Point\`3`, `type T = int` alongside
    // `type T<'a> = …`), so each claims exactly `(name, arity)`; an enum is non-generic,
    // so it claims `(name, 0)` and collides with a record `Foo` but NOT with a record
    // ``Foo`1``; an intrinsic binding (`type int = (# "System.Int32" #)`) claims the name
    // it DECLARES, at its declared arity — its target-representation string is not a
    // name-table concern.

    /// Accept a type declaration: claim `(Name, Arity)` for it in the name table AND
    /// retain it, in source order, for the per-kind detail registrars. Called once per
    /// accepted type by the file-order identity pass, which has already rejected a
    /// contested claim. THE single write to both — so "claimed" and "will be registered"
    /// are one fact, not two that can drift.
    let claimType (types: PassContextTypes) (claimed: ClaimedTypeDefn) : unit =
        let id = claimed.Identity

        match types.TypeClaims.TryGetValue id.Name with
        | true, claims -> claims.Add id
        | false, _ ->
            let claims = ResizeArray 1
            claims.Add id
            types.TypeClaims.[id.Name] <- claims

        types.ClaimedTypeDefns.Add claimed

    /// Record a declaration whose `(name, arity)` claim is already held. It registers
    /// nothing; this retains it so the rejection is inspectable. See `RejectedTypeDefn`.
    let rejectDuplicateType (types: PassContextTypes) (rejected: RejectedTypeDefn) : unit =
        types.RejectedDuplicates.Add rejected

    /// Is this claim visible from `useSite`? THE file-order rule, in one comparison — see
    /// `TypeIdentity.VisibleFrom`. A claim declared below the use answers for nothing, so a
    /// use above a declaration sees exactly what F# sees there: the external universe, or
    /// nothing at all.
    let private visibleAt (useSite: SourcePos) (claim: TypeIdentity) : bool = claim.VisibleFrom <= useSite.Offset

    /// The identity holding `(name, arity)` AS SEEN FROM `useSite`, if any. The single route
    /// from a use-site name+arity to the type that owns it — so a resolver ASKS which kind
    /// owns the name instead of probing the kind tables in a hand-ordered precedence cascade.
    ///
    /// A claim only answers when it is visible from the use (`visibleAt`): a name is not an
    /// identity on its own, it is one only as seen from somewhere. Registration's own reads
    /// were already scoped by the top-down scan (the table holds what is claimed so far); a
    /// read from a body — walked long after the whole file is registered — is scoped by this
    /// and nothing else.
    let tryTypeClaim (types: PassContextTypes) (useSite: SourcePos) (name: string) (arity: int) : TypeIdentity voption =
        match types.TypeClaims.TryGetValue name with
        | true, claims ->
            let i = claims.FindIndex(fun c -> c.Arity = arity && visibleAt useSite c)
            if i < 0 then ValueNone else ValueSome claims.[i]
        | false, _ -> ValueNone

    /// THE duplicate-type-definition test: is `(name, arity)` already claimed, by any
    /// kind? One table, one predicate — a kind added later cannot be wired into some
    /// guards and forgotten in others.
    ///
    /// UNBOUNDED on purpose, and not a use site: a duplicate is a duplicate wherever it is
    /// written. The test is scoped by the registration scan itself — the table holds
    /// exactly the claims made so far — not by a position.
    let isTypeClaimed (types: PassContextTypes) (name: string) (arity: int) : bool =
        (tryTypeClaim types SourcePos.unbounded name arity).IsSome

    /// Does any claim VISIBLE FROM `useSite` hold `name` at SOME arity — i.e. is this name a
    /// project-local type there? THE local/external precedence test: a written head whose
    /// name this answers `true` for names a project-local type and nothing else, and one it
    /// answers `false` for is external or nothing at all. Arity-blind on purpose: a head
    /// written at the wrong arity for the local type of that name is still LOCAL (an arity
    /// diagnostic), never a silent fall-through to an external type of the same name.
    let isTypeNameInScope (types: PassContextTypes) (useSite: SourcePos) (name: string) : bool =
        match types.TypeClaims.TryGetValue name with
        | true, claims -> claims.Exists(fun c -> visibleAt useSite c)
        | false, _ -> false

    /// Note a record / union / class short name (`NominalTypeNames`). Called by the
    /// pre-scan that runs ahead of the identity pass; see the field's doc.
    let noteNominalTypeName (types: PassContextTypes) (name: string) : unit =
        types.NominalTypeNames.Add name |> ignore

    /// Does this unit declare a record / union / class called `name`? THE
    /// module-name-collision test behind the `…Module` suffix — see `NominalTypeNames`.
    let isNominalTypeName (types: PassContextTypes) (name: string) : bool = types.NominalTypeNames.Contains name

    // --- Records / unions / classes / abbreviations -----------------------------------
    // All four are arity-overloadable and keyed by their own `TypeKey`, which carries the
    // arity in its `Name`. Only enums stay bare-name-keyed (an enum is never generic).

    /// Register a record under its own `TypeKey`, indexing that key under the record's
    /// short name. The key comes off the `info` — there is no second spelling of the
    /// identity to drift from it.
    let registerRecord (types: PassContextTypes) (info: RecordTypeInfo) : unit =
        registerKeyed types.Record types.RecordNames info.Name info.TypeKey info

    /// Resolve a record by BARE short name (see `tryKeyOfBareName`): the non-generic
    /// record of that name, else the lone candidate, else nothing — an arity-overloaded
    /// name does not resolve unqualified. Callers holding a key use `tryRecordByKey`;
    /// those with a use-site arity use `tryRecordArity`.
    let tryRecord (types: PassContextTypes) (useSite: SourcePos) (name: string) : RecordTypeInfo voption =
        tryOfKey types.Record (tryKeyOfBareName types.RecordNames useSite name)

    /// Resolve a record by `(name, arity)` — exact arity, so a wrong arity misses.
    let tryRecordArity
        (types: PassContextTypes)
        (useSite: SourcePos)
        (name: string)
        (arity: int)
        : RecordTypeInfo voption =
        tryOfKey types.Record (tryKeyOfArity types.RecordNames useSite name arity)

    /// Resolve a record by its project-local `SymbolKey` — the reader-side companion
    /// to `tryUnionByKey`/`tryClassByKey`. See `tryByTypeKey`.
    let tryRecordByKey (types: PassContextTypes) (key: SymbolKey) : RecordTypeInfo voption =
        tryByTypeKey types.Record key

    /// Register a class under its own `TypeKey`. See `registerRecord`.
    let registerClass (types: PassContextTypes) (info: ClassTypeInfo) : unit =
        registerKeyed types.Class types.ClassNames info.Name info.TypeKey info

    /// Resolve a class by BARE short name (see `tryKeyOfBareName`). The
    /// recognition-only call sites (`Scope.fs`, `NameResolution.fs`, qualified-static
    /// heads) read the registry this way; a caller holding a key uses `tryClassByKey`.
    let tryClass (types: PassContextTypes) (useSite: SourcePos) (name: string) : ClassTypeInfo voption =
        tryOfKey types.Class (tryKeyOfBareName types.ClassNames useSite name)

    /// Resolve a class by `(name, arity)` — exact arity, so a wrong arity misses.
    let tryClassArity
        (types: PassContextTypes)
        (useSite: SourcePos)
        (name: string)
        (arity: int)
        : ClassTypeInfo voption =
        tryOfKey types.Class (tryKeyOfArity types.ClassNames useSite name arity)

    /// Resolve a class by its project-local `SymbolKey` — the class analogue of
    /// `tryUnionByKey`. See `tryByTypeKey`.
    let tryClassByKey (types: PassContextTypes) (key: SymbolKey) : ClassTypeInfo voption = tryByTypeKey types.Class key

    /// True iff a class is registered under this `SymbolKey` — the key-based membership
    /// gate (the `tryClassByKey`-shaped mirror of `tryClassArity`). A caller holding a
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

    /// Register an enum under its bare short name — an enum is never generic, so its
    /// claim is always `(name, 0)` and the name addresses at most one.
    let registerEnum (types: PassContextTypes) (name: string) (info: EnumTypeInfo) : unit = types.Enum.[name] <- info

    /// Resolve an enum by bare short name; `ValueNone` if none. Used by
    /// `translateType` (`(x: E)` → `TyEnum`) and the `E.C1` qualified-access path.
    let tryEnum (types: PassContextTypes) (_useSite: SourcePos) (name: string) : EnumTypeInfo voption =
        match types.Enum.TryGetValue name with
        | true, info -> ValueSome info
        | false, _ -> ValueNone

    /// Register an abbreviation under its own `TypeKey`. See `registerRecord`.
    let registerAbbrev (types: PassContextTypes) (info: AbbreviationInfo) : unit =
        registerKeyed types.Abbreviation types.AbbreviationNames info.Name info.TypeKey info

    /// Resolve an abbreviation by BARE short name (see `tryKeyOfBareName`). Cross-kind
    /// precedence is NOT this function's business: a caller that must know which kind owns
    /// a name asks `tryTypeClaim` first, and reaches here only for the lenient tail (a
    /// GENERIC alias named without its arguments back-fills fresh TyVars).
    let tryAbbrev (types: PassContextTypes) (useSite: SourcePos) (name: string) : AbbreviationInfo voption =
        tryOfKey types.Abbreviation (tryKeyOfBareName types.AbbreviationNames useSite name)

    /// Resolve an abbreviation by `(name, arity)` — exact arity, so a wrong arity misses.
    let tryAbbrevArity
        (types: PassContextTypes)
        (useSite: SourcePos)
        (name: string)
        (arity: int)
        : AbbreviationInfo voption =
        tryOfKey types.Abbreviation (tryKeyOfArity types.AbbreviationNames useSite name arity)

    /// Resolve an abbreviation by its project-local `SymbolKey`. See `tryRecordByKey`.
    let tryAbbrevByKey (types: PassContextTypes) (key: SymbolKey) : AbbreviationInfo voption =
        tryByTypeKey types.Abbreviation key

    /// Register a union under its own `TypeKey`. See `registerRecord`.
    let registerUnion (types: PassContextTypes) (info: UnionTypeInfo) : unit =
        registerKeyed types.Union types.UnionNames info.Name info.TypeKey info

    /// Resolve a union by `(name, arity)` — exact arity, so a wrong arity misses (the
    /// caller diagnoses).
    let tryUnion (types: PassContextTypes) (useSite: SourcePos) (name: string) (arity: int) : UnionTypeInfo voption =
        tryOfKey types.Union (tryKeyOfArity types.UnionNames useSite name arity)

    /// Resolve a union by BARE short name (see `tryKeyOfBareName`) — the union sibling
    /// of `tryRecord` / `tryClass`, for the recognition-only call sites (a qualified
    /// ctor / static head, the module-vs-type name test).
    let tryUnionBare (types: PassContextTypes) (useSite: SourcePos) (name: string) : UnionTypeInfo voption =
        tryOfKey types.Union (tryKeyOfBareName types.UnionNames useSite name)

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
    let tryNonClassMemberHost
        (types: PassContextTypes)
        (useSite: SourcePos)
        (name: string)
        : IInterfaceImplHost voption =
        match tryUnionBare types useSite name with
        | ValueSome info -> ValueSome(info :> IInterfaceImplHost)
        | ValueNone ->
            match tryRecord types useSite name with
            | ValueSome info -> ValueSome(info :> IInterfaceImplHost)
            | ValueNone ->
                match types.IntrinsicAbbrevHost.TryGetValue name with
                | true, info -> ValueSome(info :> IInterfaceImplHost)
                | false, _ -> ValueNone

    /// The declaring union of a registered case, resolved by its `(UnionName,
    /// UnionArity)` — the pair the case was stamped with at registration, so this is
    /// total: a case cannot exist without its union.
    let unionOfCase (types: PassContextTypes) (useSite: SourcePos) (info: UnionCaseInfo) : UnionTypeInfo =
        match tryUnion types useSite info.UnionName info.UnionArity with
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
