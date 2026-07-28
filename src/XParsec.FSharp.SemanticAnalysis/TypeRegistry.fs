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
        /// Generic arity. Part of the CLAIM: `Foo` and `` Foo`1 `` are distinct claims (as
        /// in F#) and may be held by different kinds.
        TyparArity: int
        /// The module / namespace chain the declaration sits in. Part of the CLAIM, and the
        /// reason `N.A.T` and `N.B.T` are two types rather than one name contested twice: a
        /// claim is `(Holder, Name, Arity)`, and at most one type of any kind may hold it.
        /// It is also WHERE the claim answers from — a bare name reaches it only from
        /// inside this holder chain, or through an `open` naming it.
        ///
        /// `Key.Holder` is this chain read as a type's holder (`ModuleRules.typeHolderOf`);
        /// both come off the ONE chain the declaring containment yields, so they cannot
        /// disagree about where the type was declared.
        Holder: ModuleHolder
        Kind: TypeDeclKind
        /// The declaration site — the key stamped onto the kind-specific `*TypeInfo` and
        /// the name token that spells it, projected once from that one token and handed to
        /// the per-kind registrar with the rest of the identity.
        DeclSite: NodeSite
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

/// A type declaration REJECTED as a duplicate: an earlier declaration in the SAME module
/// already holds its `(Holder, Name, Arity)` claim and keeps it. It registers no detail and
/// mints no `SymbolKey` —
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
        TyparArity: int
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
        /// Keyed by `TypeKey` (see `Record`) — an enum is non-generic, so its claim is
        /// always `(holder, name, 0)`, but two sibling modules may each declare one and a
        /// name would then address both. Populated by
        /// `NameResolution.registerEnumTypeDefn`; read by `translateType` (so
        /// `(x: E)` resolves to `TyEnum Key`) and the `E.C1` qualified-access path.
        Enum: Dictionary<TypeKey, EnumTypeInfo>
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
        /// READ ONLY through `TypeRegistry.casesNamed` / `isCaseName` / `localQualifiedCase`,
        /// which take the use site: a case is visible exactly where its declaring union is.
        CtorIndex: Dictionary<string, EqArray<UnionCaseInfo>>
        /// Reverse index: field name → bucket of record types that declare it.
        /// READ ONLY through `TypeRegistry.recordsWithField`, which takes the use site: a
        /// field is visible exactly where its declaring record is.
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
        /// The same bindings as `IntrinsicReprTypes`, addressed by the intrinsic's
        /// contract-sourced `SymbolKey` (`IntrinsicKeys`) instead of its declared name —
        /// the LOCAL half of the forward `{ canon -> platform repr }` axis whose external
        /// half is `IExternalSymbolProvider.IntrinsicForwardRepr` (also key-addressed).
        /// A consumer holding a resolved key asks HERE; it must never recover the declared
        /// name from the key to ask `IntrinsicReprTypes` (`SymbolKeyOps.simpleName` is a
        /// lossy display projection — it would drop the `` `N `` a generic intrinsic's key
        /// carries and false-match any same-named type). Written with `IntrinsicReprTypes`
        /// at the one registration site, so the two cannot disagree. Carries the
        /// `class`-tag verdict alongside the repr (`IntrinsicReprInfo.Heritable`) — the
        /// frozen face `TastFile.IntrinsicReprKeys` is a straight copy of this table.
        IntrinsicReprKeys: Dictionary<SymbolKey, IntrinsicReprInfo>
        /// The name → qualified `SymbolKey` index for this unit's own intrinsics,
        /// populated at registration from the declaring `namespace` (`Vesper`). The
        /// intrinsic's identity is CONTRACT-SOURCED: `Translate` reads the resolved key
        /// here instead of re-deriving the namespace from a hardcoded name set. The key
        /// CARRIES ITS ARITY exactly like a record's/union's (`Vesper.Collections.seq` at
        /// arity 1, `Vesper.int` at 0), so a self-compiled intrinsic's key EQUALS the one its
        /// own contract publishes (`SymbolKeyOps.intrinsicCanonKey`) and the one a use site
        /// stamps (`TypeHeadStamp.useSiteTypeKey`) — the arity is part of the identity, not
        /// something a recogniser must strip. The TABLE is keyed by the bare declared name
        /// (as every use site spells it); the arity rides in the VALUE.
        /// `IntrinsicReprTypes` stays a pure name → target-repr side-table; this carries
        /// identity.
        IntrinsicKeys: Dictionary<string, SymbolKey>
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
        /// THE name table: short name → every `(holder, name, arity)` claim held under it,
        /// regardless of KIND. A type declaration claims a name at an arity in the module
        /// that holds it, and a claim may be held by at most one type of any kind — so
        /// duplicate detection is one predicate (`isTypeClaimed`) over this one table, and a
        /// kind added later cannot be wired into some guards and forgotten in others.
        /// Populated in source order by `NameResolutionTypeRegistration.claimTypeIdentity`,
        /// which is also the sole mint site of a project-local type `SymbolKey`.
        ///
        /// Several claims may sit under one short name — sibling modules each declaring `T`
        /// — so a name addresses a CANDIDATE SET here exactly as it does in the kind
        /// indexes. Which candidate a use site sees, and which wins when it sees more than
        /// one, is `TypeRegistry`'s `claimRank`.
        ///
        /// While the top-down registration scan is running, this table holds exactly the
        /// types DECLARED ABOVE the group being registered, plus its own `type … and …`
        /// group (all of whose names are claimed before any of its detail registers). That
        /// is the file-order half of F# type scoping; the module half is `claimRank`'s.
        TypeClaims: Dictionary<string, ResizeArray<TypeIdentity>>
        /// The module / namespace scopes this unit DECLARES, keyed by the dotted SOURCE
        /// path an `open` names them by (`"N"`, `"N.A"`) — the one route from an `open`'s
        /// written path to the holder it opens. Filled by `PassContext.EnterContainment` as
        /// each pass walks into a scope, from `ModuleRules.holderScopes`, which is also
        /// where the holder chain itself comes from: an `open`'s target and a declaration's
        /// holder are then literally the same value, so they cannot fail to match.
        ///
        /// Keyed by the SOURCE path because that is what an `open` writes. A `ModuleKey`
        /// carries the module's COMPILED holder name (`ListModule`), so the two spellings
        /// are not interchangeable and the table is the translation.
        LocalHolders: Dictionary<string, ModuleHolder>
        /// The INVERSE of `LocalHolders`: the dotted SOURCE path each declared scope is
        /// named by. A qualifier is written relative to a SCOPE (`A.T` inside `module N.B`
        /// means `N.A.T`), so resolving one needs the source path of the scope it is written
        /// in — and a use site carries its `ModuleHolder` chain, not a path. Written with
        /// `LocalHolders` at the one site (`noteLocalHolder`), so the two directions cannot
        /// disagree about which path names which scope.
        LocalHolderPaths: Dictionary<ModuleHolder, string>
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
            IntrinsicReprKeys = Dictionary<_, _>()
            IntrinsicKeys = Dictionary<_, _>()
            IntrinsicAbbrevHost = Dictionary<_, _>()
            RecordNames = Dictionary<_, _>()
            UnionNames = Dictionary<_, _>()
            ClassNames = Dictionary<_, _>()
            AbbreviationNames = Dictionary<_, _>()
            TypeClaims = Dictionary<_, _>()
            LocalHolders = Dictionary<_, _>()
            LocalHolderPaths = Dictionary<_, _>()
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
/// THE by-name / by-key split. A NAME does not identify a type on its own — it identifies
/// one only as seen FROM somewhere, and "somewhere" is a `UseSite`: the offset the name is
/// written at (F# declaration scoping is file-ordered) together with the module chain that
/// encloses it (a bare name resolves innermost-outward). Every by-name face below therefore
/// takes one, and answers against the claims visible there (`TypeIdentity.VisibleFrom`); a
/// caller with nowhere to speak from passes `UseSite.unbounded` and gets the whole-unit
/// view. The `…ByKey` faces take none, and must not: a `SymbolKey` already names a resolved
/// type, so there is no scoping question left to ask. Requiring a use site of exactly the
/// by-name faces is what makes an unscoped by-name read impossible to write by accident.
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

    // --- What a use site can see, and which candidate wins ----------------------------
    // F# builds the name environment by descending the module tree and ADDING, in source
    // order, each declaration and each `open`; the last thing added wins. `claimRank` is
    // that ordering, `BindingRank` is its value, and MAX is the resolution rule. Every
    // by-name face below is a `max claimRank` over a candidate set — there is no second
    // statement of precedence anywhere.

    /// The scope this unit declares under the dotted SOURCE `path`, as a path is WRITTEN
    /// INSIDE the scope whose own source path is `scope`: `scope.path` first, then each
    /// shorter prefix of `scope`, longest first — F#'s own order — and finally `path` alone
    /// (fully qualified from the root). So `open A` inside `namespace N` names `N.A` before
    /// it names a top-level `A`, and so does the qualifier of an `A.T` written there.
    ///
    /// `ValueNone` for a path this unit does not declare (`open System`, `System.Uri`): it
    /// names no local scope and so brings no local claim into reach; the external resolver
    /// answers for it instead.
    let private tryHolderOfPath (types: PassContextTypes) (scope: string) (path: string) : ModuleHolder voption =
        let rec go (scope: string) =
            let qualified = if scope.Length = 0 then path else scope + "." + path

            match types.LocalHolders.TryGetValue qualified with
            | true, h -> ValueSome h
            | false, _ ->
                if scope.Length = 0 then
                    ValueNone
                else
                    let cut = scope.LastIndexOf '.'
                    go (if cut < 0 then "" else scope.Substring(0, cut))

        go scope

    /// The module / namespace this `open` names, if THIS unit declares it — its written path
    /// resolved from the scope it is written in.
    let private openedHolder (types: PassContextTypes) (o: LocalOpen) : ModuleHolder voption =
        tryHolderOfPath types o.Scope o.Path

    /// The scope that the dotted SOURCE `path` names when it is written INSIDE the scope
    /// `enclosing` — and `enclosing` itself when the path is empty, because a BARE name
    /// qualifies by nothing and so names the scope it is written in. That identity is what
    /// lets one rule rank a bare name and a qualified one (`claimRank`).
    ///
    /// Only an EXACT descent, no walking outward: which scopes a path may be read from is
    /// `claimRank`'s question, and it asks this once per scope it is prepared to read from.
    let private holderUnder (types: PassContextTypes) (enclosing: ModuleHolder) (path: string) : ModuleHolder voption =
        if path.Length = 0 then
            ValueSome enclosing
        else
            match types.LocalHolderPaths.TryGetValue enclosing with
            | true, basePath ->
                let qualified = if basePath.Length = 0 then path else basePath + "." + path

                match types.LocalHolders.TryGetValue qualified with
                | true, h -> ValueSome h
                | false, _ -> ValueNone
            | false, _ -> ValueNone

    /// ONE way a written name REACHES a scope from a use site: the scope it reaches, and
    /// where the name it qualifies enters the name environment through it.
    [<Struct; NoComparison>]
    type private ScopeReach =
        {
            /// The scope reached — where the claim must be held for this reach to answer.
            Scope: ModuleHolder
            /// How many `module`s enclose whatever ADDED the name (the enclosing scope, or the
            /// `open`). An inner scope is entered after the scopes above it, so this is what
            /// makes resolution innermost-outward.
            Depth: int
            /// Where in that scope the name enters, when the reach fixes it — an `open`'s own
            /// offset. `ValueNone` when a SCOPE reaches the name directly (an ancestor, or the
            /// root of a fully-qualified path): a declaration enters the environment where it
            /// is WRITTEN, so the offset is the claim's own `VisibleFrom`.
            Offset: int voption
        }

    /// EVERY way the written module `path` (EMPTY for a bare name) reaches a scope of this
    /// unit from `useSite`. THE enumeration of routes a name may travel — the ranker below
    /// maximises over it, and it is the only place the routes are stated:
    ///
    ///   * an ANCESTOR scope of the use — its own module, an enclosing module, the namespace
    ///     at the root — reaches whatever `path` names UNDER it, at THAT scope's depth. For a
    ///     bare name (empty path) that is the scope itself, so a type in the use's own module
    ///     beats a same-named one in the module enclosing it, which beats one at namespace
    ///     level. For `A.T` it is the `A` that scope holds. A SIBLING module is no ancestor,
    ///     so a BARE cross-module name is simply not defined (FS0039) — but the sibling IS
    ///     reached by naming it (`A.T`), because the scope enclosing both holds it;
    ///   * an `open` reaches whatever `path` names under the scope it OPENS, at the depth of
    ///     the scope the `open` is WRITTEN in and at the `open`'s own offset. So an `open`
    ///     beats an enclosing module's declaration, the LAST of two `open`s wins, within one
    ///     scope a declaration and an `open` are ordered by nothing but the text, and an
    ///     `open N` qualifies a PARTIAL path (`A.T` ⇒ `N.A.T`);
    ///   * a FULLY-QUALIFIED path (`N.A.T`) reaches its scope from the ROOT, from anywhere —
    ///     including another namespace of the same unit — and ranks outermost, below every
    ///     scope that could name it more nearly.
    ///
    /// EMPTY for a path that names no scope of this unit (`System.Text.StringBuilder`): the
    /// external resolver answers for those, and a use site with nowhere to speak from
    /// (`UseSite.unbounded`) reaches nothing this way — it sees every claim regardless.
    let private pathReaches (types: PassContextTypes) (useSite: UseSite) (path: string) : ScopeReach list =
        match useSite.Holder with
        | ValueNone -> []
        | ValueSome here ->
            let reaches = ResizeArray()

            for h in here.SelfAndAncestors do
                match holderUnder types h path with
                | ValueSome scope ->
                    reaches.Add
                        {
                            Scope = scope
                            Depth = h.Depth
                            Offset = ValueNone
                        }
                | ValueNone -> ()

            for o in useSite.Opens do
                match openedHolder types o with
                | ValueSome opened ->
                    match holderUnder types opened path with
                    | ValueSome scope ->
                        reaches.Add
                            {
                                Scope = scope
                                Depth = o.ScopeDepth
                                Offset = ValueSome o.Offset
                            }
                    | ValueNone -> ()
                | ValueNone -> ()

            if path.Length > 0 then
                match types.LocalHolders.TryGetValue path with
                | true, scope ->
                    reaches.Add
                        {
                            Scope = scope
                            Depth = 0
                            Offset = ValueNone
                        }
                | false, _ -> ()

            List.ofSeq reaches

    /// WHERE this claim enters the name environment at `useSite`, given the ways the written
    /// name reaches a scope there — `ValueNone` if it is not in scope at all. THE whole of
    /// project-local type scoping, with `pathReaches`:
    ///
    ///   * a claim declared BELOW the use answers for nothing (`VisibleFrom`) — F# type
    ///     scoping is file-ordered, and `module rec` is not an exception to it but a
    ///     restatement (the claim's `VisibleFrom` is the `rec` keyword);
    ///   * otherwise the MAXIMUM over every reach that lands on the scope HOLDING the claim,
    ///     because F# adds each of them to ONE environment and the last one added is what the
    ///     name means.
    let private claimRank (useSite: UseSite) (reaches: ScopeReach list) (claim: TypeIdentity) : BindingRank voption =
        if claim.VisibleFrom > useSite.Offset then
            ValueNone
        else
            match useSite.Holder with
            // Nowhere to speak from: the whole-unit view (`UseSite.unbounded`). Every claim
            // is in scope, and none outranks another — so a caller that must choose still
            // takes the first, as it did before it had anywhere to speak from.
            | ValueNone -> ValueSome { Depth = 0; Offset = 0 }
            | ValueSome _ ->
                let mutable best = ValueNone

                for r in reaches do
                    if r.Scope = claim.Holder then
                        let rank =
                            {
                                Depth = r.Depth
                                Offset =
                                    match r.Offset with
                                    | ValueSome o -> o
                                    | ValueNone -> claim.VisibleFrom
                            }

                        match best with
                        | ValueSome b when b >= rank -> ()
                        | _ -> best <- ValueSome rank

                best

    /// The claim on `written` that WINS at `useSite` among those `admit`s — the max-rank
    /// candidate. THE resolution primitive: every by-name face is this with a different
    /// `admit`, so no face can invent a precedence of its own, and a qualified name is not a
    /// second resolver but the same one reading from the scope its path names.
    let private tryWinner
        (types: PassContextTypes)
        (useSite: UseSite)
        (written: WrittenTypeName)
        (admit: TypeIdentity -> bool)
        : TypeIdentity voption =
        match types.TypeClaims.TryGetValue written.Name with
        | true, claims ->
            let reaches = pathReaches types useSite written.Path
            let mutable best = ValueNone
            let mutable bestRank = ValueNone

            for c in claims do
                if admit c then
                    match claimRank useSite reaches c with
                    | ValueSome r ->
                        match bestRank with
                        | ValueSome b when b >= r -> ()
                        | _ ->
                            bestRank <- ValueSome r
                            best <- ValueSome c
                    | ValueNone -> ()

            best
        | false, _ -> ValueNone

    /// Is the type `key` (claimed under the short name `name`) visible from `useSite`?
    /// The kind indexes below map a name to KEYS, but the scoping facts live on the CLAIM —
    /// so a kind index is scoped by asking the name table about the very key it is about to
    /// answer with. A key carries holder, name and arity, so at most one claim can match.
    let private keyVisibleAt (types: PassContextTypes) (useSite: UseSite) (name: string) (key: TypeKey) : bool =
        (tryWinner types useSite (WrittenTypeName.bare name) (fun c -> c.Key = key)).IsSome

    /// The key `written` claims at EXACTLY this arity AS SEEN FROM `useSite`, if any — the
    /// winning claim's, restricted to the keys THIS kind's index holds (so a same-named type
    /// of another kind that shadows it makes the kind read MISS, which is the right answer:
    /// the name does not mean this kind here).
    ///
    /// With `tryKeyOfArglessName` it is the funnel EVERY kind index (record / union / class /
    /// abbrev) resolves a name through, which is why the use site enters here rather than
    /// at each kind's face: one place decides what a name can see.
    let private tryKeyOfArity
        (types: PassContextTypes)
        (index: Dictionary<string, ResizeArray<TypeKey>>)
        (useSite: UseSite)
        (written: WrittenTypeName)
        (arity: int)
        : TypeKey voption =
        match index.TryGetValue written.Name with
        | true, keys ->
            match tryWinner types useSite written (fun c -> c.TyparArity = arity && keys.Contains c.Key) with
            | ValueSome c -> ValueSome c.Key
            | ValueNone -> ValueNone
        | false, _ -> ValueNone

    /// What a name written WITHOUT type arguments resolves to AS SEEN FROM `useSite`. A
    /// NON-GENERIC type owns its name outright — nothing else can be written argument-less
    /// and mean it — so it wins whenever one is in scope; failing that the candidates
    /// resolve only if they agree on an arity (a generic type named without its arguments
    /// back-fills them); and a name overloaded on arity (`Point<'a,'b>` / `Point<'a,'b,'c>`)
    /// is genuinely ambiguous written this way, so it resolves to NOTHING and the caller
    /// must come with an arity or a key.
    ///
    /// Among candidates of the SAME arity the winner is the max-rank one, exactly as for an
    /// arity-qualified read: two sibling modules' `T`, both in scope through `open`s, are
    /// not ambiguous — the later `open` wins, as it does in F#.
    let private tryKeyOfArglessName
        (types: PassContextTypes)
        (index: Dictionary<string, ResizeArray<TypeKey>>)
        (useSite: UseSite)
        (written: WrittenTypeName)
        : TypeKey voption =
        match index.TryGetValue written.Name with
        | true, keys ->
            let inThisKind (c: TypeIdentity) = keys.Contains c.Key

            match tryWinner types useSite written (fun c -> c.TyparArity = 0 && inThisKind c) with
            | ValueSome c -> ValueSome c.Key
            | ValueNone ->
                // No non-generic claimant. The generic ones answer only if they agree on an
                // arity — otherwise the name genuinely does not say which type it means.
                let arities =
                    match types.TypeClaims.TryGetValue written.Name with
                    | true, claims ->
                        let reaches = pathReaches types useSite written.Path
                        let mutable seen = ValueNone
                        let mutable oneArity = true

                        for c in claims do
                            if inThisKind c && (claimRank useSite reaches c).IsSome then
                                match seen with
                                | ValueSome a when a <> c.TyparArity -> oneArity <- false
                                | _ -> seen <- ValueSome c.TyparArity

                        if oneArity then seen else ValueNone
                    | false, _ -> ValueNone

                match arities with
                | ValueNone -> ValueNone
                | ValueSome arity ->
                    match tryWinner types useSite written (fun c -> c.TyparArity = arity && inThisKind c) with
                    | ValueSome c -> ValueSome c.Key
                    | ValueNone -> ValueNone
        | false, _ -> ValueNone

    let private tryOfKey (table: Dictionary<TypeKey, 'T>) (key: TypeKey voption) : 'T voption =
        match key with
        | ValueSome k ->
            match table.TryGetValue k with
            | true, info -> ValueSome info
            | false, _ -> ValueNone
        | ValueNone -> ValueNone

    /// Resolve a type by its project-local `TypeKey` — a real key-addressed read: the
    /// `TypeKey` a consumer carries IS the table's key, containment and arity suffix
    /// included, so a same-named type declared elsewhere cannot answer for it.
    let private tryByTypeKey (table: Dictionary<TypeKey, 'T>) (key: TypeKey) : 'T voption =
        match table.TryGetValue key with
        | true, info -> ValueSome info
        | false, _ -> ValueNone

    // --- The name table -------------------------------------------------------------
    // THE rule: a declaration claims a NAME at an ARITY in the MODULE that holds it, and a
    // claim may be held by at most one type of ANY kind. Records, unions, classes and
    // abbreviations are arity-overloadable (`Point\`2`/`Point\`3`, `type T = int` alongside
    // `type T<'a> = …`), so each claims exactly `(holder, name, arity)`; an enum is
    // non-generic, so it claims `(holder, name, 0)` and collides with a record `Foo` in the
    // same module but NOT with a record ``Foo`1``, and not with a `Foo` in a sibling module;
    // an intrinsic binding (`type int = (# "System.Int32" #)`) claims the name it DECLARES,
    // at its declared arity — its target-representation string is not a name-table concern.

    /// Accept a type declaration: claim `(Holder, Name, Arity)` for it in the name table AND
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

    /// The identity the WRITTEN name `written` at `arity` MEANS at `useSite`, if any — the
    /// winning claim (`claimRank`). The single route from a use-site name+arity to the type
    /// that owns it — so a resolver ASKS which kind owns the name instead of probing the kind
    /// tables in a hand-ordered precedence cascade. A qualified head (`A.T`, `N.A.T`) is the
    /// same lookup with a non-empty path: the path says which SCOPE the name is read from.
    ///
    /// A claim only answers where it is in scope: a name is not an identity on its own, it
    /// is one only as seen from somewhere. Registration's own reads are additionally scoped
    /// by the top-down scan (the table holds only what is claimed so far); a read from a
    /// body — walked long after the whole file is registered — is scoped by this and nothing
    /// else.
    let tryWrittenTypeClaim
        (types: PassContextTypes)
        (useSite: UseSite)
        (written: WrittenTypeName)
        (arity: int)
        : TypeIdentity voption =
        tryWinner types useSite written (fun c -> c.TyparArity = arity)

    /// `tryWrittenTypeClaim` for a name written with no qualifier.
    let tryTypeClaim (types: PassContextTypes) (useSite: UseSite) (name: string) (arity: int) : TypeIdentity voption =
        tryWrittenTypeClaim types useSite (WrittenTypeName.bare name) arity

    /// THE duplicate-type-definition test: is `(holder, name, arity)` already claimed, by
    /// any kind? One table, one predicate — a kind added later cannot be wired into some
    /// guards and forgotten in others.
    ///
    /// The HOLDER is part of the claim, so sibling `N.A.T` and `N.B.T` are two legal types
    /// and only a second `T` in the SAME module is a duplicate. It takes no use site, and
    /// must not: a duplicate is a duplicate wherever it is written, and whatever is in scope
    /// where it is written. The test is scoped by the registration scan itself — the table
    /// holds exactly the claims made so far.
    let isTypeClaimed (types: PassContextTypes) (holder: ModuleHolder) (name: string) (arity: int) : bool =
        match types.TypeClaims.TryGetValue name with
        | true, claims -> claims.Exists(fun c -> c.TyparArity = arity && c.Holder = holder)
        | false, _ -> false

    /// The claim the written name reaches at `useSite` at ANY arity — THE local/external
    /// precedence test, and the type it reached. A written head this answers for names a
    /// project-local type and nothing else; one it does not is external or nothing at all.
    /// It holds for a QUALIFIED head as it does for a bare one: `A.T` names the local `A`'s
    /// `T` even where an external `A.T` is also reachable, because the scope enclosing the
    /// use holds `A` and nothing nearer can be named (probed against `dotnet fsi`).
    ///
    /// Arity-blind on purpose: a head written at the wrong arity for the local type of that
    /// name is still LOCAL (an arity diagnostic), never a silent fall-through to an external
    /// type of the same name. It answers WITH the claim, so the caller that blames the arity
    /// blames the very type the precedence rule reached rather than re-finding one of its own.
    let tryWrittenTypeClaimAnyArity
        (types: PassContextTypes)
        (useSite: UseSite)
        (written: WrittenTypeName)
        : TypeIdentity voption =
        tryWinner types useSite written (fun _ -> true)

    /// `tryWrittenTypeClaimAnyArity` for a name written with no qualifier.
    let tryTypeClaimAnyArity (types: PassContextTypes) (useSite: UseSite) (name: string) : TypeIdentity voption =
        tryWrittenTypeClaimAnyArity types useSite (WrittenTypeName.bare name)

    /// Does the written name reach a project-local type at `useSite`, at any arity? See
    /// `tryWrittenTypeClaimAnyArity`.
    let isWrittenTypeNameInScope (types: PassContextTypes) (useSite: UseSite) (written: WrittenTypeName) : bool =
        (tryWrittenTypeClaimAnyArity types useSite written).IsSome

    /// `isWrittenTypeNameInScope` for a name written with no qualifier.
    let isTypeNameInScope (types: PassContextTypes) (useSite: UseSite) (name: string) : bool =
        isWrittenTypeNameInScope types useSite (WrittenTypeName.bare name)

    /// Record a module / namespace scope this unit declares, under the dotted SOURCE path an
    /// `open` — or a qualified name — names it by, and the path it is named by. Idempotent —
    /// every pass re-walks the tree and re-enters the same scopes.
    let noteLocalHolder (types: PassContextTypes) (path: string) (holder: ModuleHolder) : unit =
        types.LocalHolders.[path] <- holder
        types.LocalHolderPaths.[holder] <- path

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

    /// Resolve a record by BARE short name (see `tryKeyOfArglessName`): the non-generic
    /// record of that name, else the lone candidate, else nothing — an arity-overloaded
    /// name does not resolve unqualified. Callers holding a key use `tryRecordByKey`;
    /// those with a use-site arity use `tryRecordArity`.
    let tryRecord (types: PassContextTypes) (useSite: UseSite) (name: string) : RecordTypeInfo voption =
        tryOfKey types.Record (tryKeyOfArglessName types types.RecordNames useSite (WrittenTypeName.bare name))

    /// Resolve a record by `(name, arity)` — exact arity, so a wrong arity misses.
    let tryRecordArity
        (types: PassContextTypes)
        (useSite: UseSite)
        (name: string)
        (arity: int)
        : RecordTypeInfo voption =
        tryOfKey types.Record (tryKeyOfArity types types.RecordNames useSite (WrittenTypeName.bare name) arity)

    /// Resolve a record by its project-local `SymbolKey` — the reader-side companion
    /// to `tryUnionByKey`/`tryClassByKey`. See `tryByTypeKey`.
    let tryRecordByKey (types: PassContextTypes) (key: TypeKey) : RecordTypeInfo voption = tryByTypeKey types.Record key

    /// Register a class under its own `TypeKey`. See `registerRecord`.
    let registerClass (types: PassContextTypes) (info: ClassTypeInfo) : unit =
        registerKeyed types.Class types.ClassNames info.Name info.TypeKey info

    /// Resolve a class by the name as WRITTEN — bare (`T`) or qualified by the module that
    /// holds it (`A.T`), which is how a body outside `A` names and constructs it. See
    /// `tryKeyOfArglessName` for what an argument-less name resolves to.
    let tryWrittenClass
        (types: PassContextTypes)
        (useSite: UseSite)
        (written: WrittenTypeName)
        : ClassTypeInfo voption =
        tryOfKey types.Class (tryKeyOfArglessName types types.ClassNames useSite written)

    /// Resolve a class by BARE short name. The recognition-only call sites (`Scope.fs`,
    /// `NameResolution.fs`, qualified-static heads) read the registry this way; a caller
    /// holding a key uses `tryClassByKey`.
    let tryClass (types: PassContextTypes) (useSite: UseSite) (name: string) : ClassTypeInfo voption =
        tryWrittenClass types useSite (WrittenTypeName.bare name)

    /// Resolve a class by `(name, arity)` — exact arity, so a wrong arity misses.
    let tryClassArity (types: PassContextTypes) (useSite: UseSite) (name: string) (arity: int) : ClassTypeInfo voption =
        tryOfKey types.Class (tryKeyOfArity types types.ClassNames useSite (WrittenTypeName.bare name) arity)

    /// Resolve a class by its project-local `SymbolKey` — the class analogue of
    /// `tryUnionByKey`. See `tryByTypeKey`.
    let tryClassByKey (types: PassContextTypes) (key: TypeKey) : ClassTypeInfo voption = tryByTypeKey types.Class key

    /// True iff a class is registered under this `SymbolKey` — the key-based membership
    /// gate (the `tryClassByKey`-shaped mirror of `tryClassArity`). A caller holding a
    /// `TyClass` key uses this so a local class is never misclassified as external.
    let containsClassKey (types: PassContextTypes) (key: TypeKey) : bool = (tryByTypeKey types.Class key).IsSome

    /// Resolve a nominal type that may carry `interface … with` impls — a class,
    /// union, *or* record — by its `SymbolKey`, surfaced as the shared
    /// `IInterfaceImplHost`. The `subsumes` interface-admission walk uses this so a
    /// union's/record's declared interfaces participate in subtyping exactly like a
    /// class's. Class → union → record: a key can only be registered in one of the
    /// three (the duplicate-definition test forbids a name+arity claimed twice), so the
    /// order is a search order, not a precedence.
    let tryInterfaceImplHostByKey (types: PassContextTypes) (key: TypeKey) : IInterfaceImplHost voption =
        match tryByTypeKey types.Class key with
        | ValueSome info -> ValueSome(info :> IInterfaceImplHost)
        | ValueNone ->
            match tryByTypeKey types.Union key with
            | ValueSome info -> ValueSome(info :> IInterfaceImplHost)
            | ValueNone ->
                match tryByTypeKey types.Record key with
                | ValueSome info -> ValueSome(info :> IInterfaceImplHost)
                | ValueNone -> ValueNone

    /// Register an enum under its own `TypeKey`. See `registerRecord` — an enum needs no
    /// short-name index of its own, because it is never generic: its claim in the name table
    /// is always `(holder, name, 0)`, and that claim carries the key.
    let registerEnum (types: PassContextTypes) (info: EnumTypeInfo) : unit = types.Enum.[info.TypeKey] <- info

    /// Resolve an enum by bare short name AS SEEN FROM `useSite`; `ValueNone` if none. Used
    /// by `translateType` (`(x: E)` → `TyEnum`) and the `E.C1` qualified-access path.
    /// An enum is never generic, so the `(name, 0)` claim winning at the use IS the whole of
    /// the resolution — there is no arity to disambiguate.
    let tryEnum (types: PassContextTypes) (useSite: UseSite) (name: string) : EnumTypeInfo voption =
        match tryTypeClaim types useSite name 0 with
        | ValueNone -> ValueNone
        | ValueSome claim -> tryOfKey types.Enum (ValueSome claim.Key)

    /// Resolve an enum by its project-local `SymbolKey`. See `tryRecordByKey`.
    let tryEnumByKey (types: PassContextTypes) (key: TypeKey) : EnumTypeInfo voption = tryByTypeKey types.Enum key

    /// Register an abbreviation under its own `TypeKey`. See `registerRecord`.
    let registerAbbrev (types: PassContextTypes) (info: AbbreviationInfo) : unit =
        registerKeyed types.Abbreviation types.AbbreviationNames info.Name info.TypeKey info

    /// Resolve an abbreviation by BARE short name (see `tryKeyOfArglessName`). Cross-kind
    /// precedence is NOT this function's business: a caller that must know which kind owns
    /// a name asks `tryTypeClaim` first, and reaches here only for the lenient tail (a
    /// GENERIC alias named without its arguments back-fills fresh TyVars).
    let tryAbbrev (types: PassContextTypes) (useSite: UseSite) (name: string) : AbbreviationInfo voption =
        tryOfKey
            types.Abbreviation
            (tryKeyOfArglessName types types.AbbreviationNames useSite (WrittenTypeName.bare name))

    /// Resolve an abbreviation by `(name, arity)` — exact arity, so a wrong arity misses.
    let tryAbbrevArity
        (types: PassContextTypes)
        (useSite: UseSite)
        (name: string)
        (arity: int)
        : AbbreviationInfo voption =
        tryOfKey
            types.Abbreviation
            (tryKeyOfArity types types.AbbreviationNames useSite (WrittenTypeName.bare name) arity)

    /// Resolve an abbreviation by its project-local `SymbolKey`. See `tryRecordByKey`.
    let tryAbbrevByKey (types: PassContextTypes) (key: TypeKey) : AbbreviationInfo voption =
        tryByTypeKey types.Abbreviation key

    /// Register a union under its own `TypeKey`. See `registerRecord`.
    let registerUnion (types: PassContextTypes) (info: UnionTypeInfo) : unit =
        registerKeyed types.Union types.UnionNames info.Name info.TypeKey info

    /// Resolve a union by `(name, arity)` — exact arity, so a wrong arity misses (the
    /// caller diagnoses).
    let tryUnion (types: PassContextTypes) (useSite: UseSite) (name: string) (arity: int) : UnionTypeInfo voption =
        tryOfKey types.Union (tryKeyOfArity types types.UnionNames useSite (WrittenTypeName.bare name) arity)

    /// Resolve a union by BARE short name (see `tryKeyOfArglessName`) — the union sibling
    /// of `tryRecord` / `tryClass`, for the recognition-only call sites (a qualified
    /// ctor / static head, the module-vs-type name test).
    let tryUnionBare (types: PassContextTypes) (useSite: UseSite) (name: string) : UnionTypeInfo voption =
        tryOfKey types.Union (tryKeyOfArglessName types types.UnionNames useSite (WrittenTypeName.bare name))

    /// Resolve a union by its project-local `SymbolKey` — the `TypeKey` minted onto
    /// `UnionTypeInfo.Key` and stamped into `Resolution.ResolvedType`. A non-`TypeKey`
    /// key (a value / member) never names a union, so it misses. The reader-side seam
    /// for SymbolKey-first resolution.
    let tryUnionByKey (types: PassContextTypes) (key: TypeKey) : UnionTypeInfo voption = tryByTypeKey types.Union key

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
    let tryNonClassMemberHost (types: PassContextTypes) (useSite: UseSite) (name: string) : IInterfaceImplHost voption =
        match tryUnionBare types useSite name with
        | ValueSome info -> ValueSome(info :> IInterfaceImplHost)
        | ValueNone ->
            match tryRecord types useSite name with
            | ValueSome info -> ValueSome(info :> IInterfaceImplHost)
            | ValueNone ->
                match types.IntrinsicAbbrevHost.TryGetValue name with
                | true, info -> ValueSome(info :> IInterfaceImplHost)
                | false, _ -> ValueNone

    /// The union / record / inline intrinsic-abbrev host a DECLARATION names — the
    /// key-addressed face of `tryNonClassMemberHost`, for the passes that are walking the
    /// declaration itself rather than a reference to it. A declaration knows exactly which
    /// type it is, and two sibling modules may each declare `T`, so it must not re-find
    /// itself by name.
    ///
    /// The nominal kinds answer by KEY. An intrinsic binding answers by NAME, and can only:
    /// it is a primitive declared at namespace level and spelled bare at every face
    /// (`IntrinsicReprTypes` / `IntrinsicKeys` / `IntrinsicAbbrevHost` are all bare-name
    /// tables, because a primitive's name is its identity).
    let tryNonClassMemberHostByKey
        (types: PassContextTypes)
        (key: TypeKey)
        (name: string)
        : IInterfaceImplHost voption =
        match tryByTypeKey types.Union key with
        | ValueSome info -> ValueSome(info :> IInterfaceImplHost)
        | ValueNone ->
            match tryByTypeKey types.Record key with
            | ValueSome info -> ValueSome(info :> IInterfaceImplHost)
            | ValueNone ->
                match types.IntrinsicAbbrevHost.TryGetValue name with
                | true, info -> ValueSome(info :> IInterfaceImplHost)
                | false, _ -> ValueNone

    /// The declaring union of a registered case — IDENTITY NAVIGATION, not a name lookup:
    /// the case carries its union's `TypeKey` (stamped from the union's own claim at
    /// registration), so this is a key-addressed read like every other `…ByKey` face and
    /// takes no use site. There is no scoping question here: a caller holding a case got it
    /// from a scoped read, so its union is visible by construction, and re-resolving the
    /// union's NAME could only ever disagree with the case in hand.
    let unionOfCase (types: PassContextTypes) (info: UnionCaseInfo) : UnionTypeInfo =
        match tryOfKey types.Union (ValueSome info.UnionKey) with
        | ValueSome u -> u
        | ValueNone -> failwithf "Internal error: union case '%s' has no registered union '%s'" info.Name info.UnionName

    // --- The reverse name indexes: fields and union cases -----------------------------
    // A field name / case name is not a TYPE name, so it holds no claim of its own and
    // cannot go through the key funnel. It is scoped by its OWNER's claim instead — the
    // record that declares the field, the union that declares the case — which is exactly
    // as file-ordered as the owner is. The three faces below are the ONLY reads of
    // `FieldIndex` / `CtorIndex`: a raw dictionary read has no signature to demand a
    // position, so leaving one in place would leave a by-name read that is unscoped by
    // construction, which is the hole the by-name/by-key split exists to close.

    /// Is the union declaring `case` visible from `useSite`? The case names its union by
    /// KEY, so this asks the name table about that very key — the same `keyVisibleAt` test
    /// the kind indexes are scoped by, and for the same reason: the file-order fact lives on
    /// the CLAIM.
    let private caseVisibleAt (types: PassContextTypes) (useSite: UseSite) (case: UnionCaseInfo) : bool =
        keyVisibleAt types useSite case.UnionName case.UnionKey

    /// The records declaring a field called `name` that are VISIBLE from `useSite` — the
    /// candidate set a record literal / record pattern intersects over. A record declared
    /// below the use is not a candidate, so `{ a = 1 }` above `type R = { a: int }` matches
    /// no record at all: F#'s verdict (the record label is not defined) falls out of
    /// resolution failing, not out of a separate check.
    let recordsWithField (types: PassContextTypes) (useSite: UseSite) (name: string) : RecordTypeInfo[] =
        match types.FieldIndex.TryGetValue name with
        | true, infos ->
            let hits = ResizeArray infos.Length

            for info in infos do
                if keyVisibleAt types useSite info.Name info.TypeKey then
                    hits.Add info

            hits.ToArray()
        | false, _ -> Array.empty

    /// The union cases named `name` VISIBLE from `useSite`. More than one means the bare
    /// name is ambiguous and needs a qualifier; none means it names no case here, so an
    /// uppercase ident is an ordinary binder in pattern position and unresolved in
    /// expression position.
    let casesNamed (types: PassContextTypes) (useSite: UseSite) (name: string) : UnionCaseInfo[] =
        match types.CtorIndex.TryGetValue name with
        | true, infos ->
            let hits = ResizeArray infos.Length

            for case in infos do
                if caseVisibleAt types useSite case then
                    hits.Add case

            hits.ToArray()
        | false, _ -> Array.empty

    /// Does `name` name any union case visible from `useSite`? The recognition test behind
    /// the ctor-vs-binder decision in pattern position and the ctor-reference suppression in
    /// expression position.
    let isCaseName (types: PassContextTypes) (useSite: UseSite) (name: string) : bool =
        match types.CtorIndex.TryGetValue name with
        | true, infos -> infos |> EqArray.exists (caseVisibleAt types useSite)
        | false, _ -> false

    /// Is `qualifier.caseName` a *local* union-case reference visible from `useSite` — the
    /// case is registered, one of its declaring unions has the short name `qualifier`, and
    /// that union is in scope here? Case names are globally unique (even across
    /// `Choice\`2`…`Choice\`7`), so this is the arity-safe replacement for
    /// `Union.ContainsKey qualifier` + "has case" when recognising a qualified union-case
    /// (`Choice.Choice1Of3`).
    let localQualifiedCase (types: PassContextTypes) (useSite: UseSite) (qualifier: string) (caseName: string) : bool =
        match types.CtorIndex.TryGetValue caseName with
        | true, infos ->
            infos
            |> EqArray.exists (fun c -> c.UnionName = qualifier && caseVisibleAt types useSite c)
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
