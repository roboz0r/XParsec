namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// Which registry a claimed `(name, arity)` was declared into: the name table is
/// kind-agnostic, so one claim has one owner whatever its kind.
[<RequireQualifiedAccess>]
type TypeDeclKind =
    | Record
    | Union
    | Class
    | Enum
    | Abbreviation
    /// A `type int = (# "System.Int32" #)` intrinsic binding. Its declared NAME is a
    /// name-table citizen like any other type's; its target-representation string is not.
    | IntrinsicRepr

/// The nominal identity of one type declaration: everything NOT kind-specific. Every member
/// of a `type … and …` group is claimed before any of the group's per-kind registrars run.
type TypeIdentity =
    {
        /// The short name as written (no arity suffix, because `Key.Name` carries that).
        Name: string
        /// Part of the CLAIM: `Foo` and `` Foo`1 `` are distinct, and may be different kinds.
        TyparArity: int
        /// Part of the CLAIM: `N.A.T` and `N.B.T` are two types, not one name contested twice.
        /// Also where it answers from: a bare name reaches it from inside, or via an `open`.
        Container: ModuleContainer
        Kind: TypeDeclKind
        /// The key stamped onto the kind-specific `*TypeInfo`, and the name token spelling it.
        DeclSite: NodeSite
        Key: TypeKey
        /// A use at offset `u` sees this claim iff `VisibleFrom <= u`. It is the first token of
        /// the claim's `type … and …` GROUP, or a `module rec` / `namespace rec` keyword.
        VisibleFrom: int
    }

/// An ACCEPTED type declaration, paired with the CST it was claimed from. Every per-kind
/// detail registrar is HANDED this identity rather than re-deriving name / arity / key.
[<NoEquality; NoComparison>]
type ClaimedTypeDefn =
    {
        Identity: TypeIdentity
        Defn: TypeDefn<SyntaxToken>
    }

type PassContextTypes =
    {
        /// Keyed by the type's own `TypeKey`, which is the WHOLE containment chain, not a name.
        /// Field types are RESOLVED at registration, against the types in scope at the declaration.
        Record: Dictionary<TypeKey, RecordTypeInfo>
        /// Keyed by `TypeKey` (see `Record`). Case field types are resolved at registration.
        Union: Dictionary<TypeKey, UnionTypeInfo>
        /// The declared structure (ctor-param annotations, `val` field types) is resolved at
        /// registration; MEMBER types start as placeholder TyVars, inferred from their bodies.
        Class: Dictionary<TypeKey, ClassTypeInfo>
        /// An enum is non-generic, so its claim is always `(container, name, 0)`, but still keyed
        /// by `TypeKey`, since two sibling modules may each declare one.
        Enum: Dictionary<TypeKey, EnumTypeInfo>
        /// An alias body is forced by the first thing that references it, at the latest when its
        /// group closes. Expansion is eager: downstream sees the underlying type longhand.
        Abbreviation: Dictionary<TypeKey, AbbreviationInfo>
        /// Reverse index: ctor name → case-info entries, each tagged with its declaring union.
        /// Scoped reads only, because a case is visible exactly where its declaring union is.
        CtorIndex: Dictionary<string, EqArray<UnionCaseInfo>>
        /// Reverse index: field name → the record types declaring it; visible where they are.
        FieldIndex: Dictionary<string, EqArray<RecordTypeInfo>>
        /// An intrinsic binding's contract-sourced `SymbolKey` → its target representation,
        /// from `type int = (# "System.Int32" #)`, plus the `class`-tag verdict. NOT
        /// transparent like `Abbreviation`: a use resolves to `TyConst key`, not the RHS.
        /// Key-addressed because a key projected back to a name loses its arity.
        IntrinsicReprKeys: Dictionary<TypeKey, IntrinsicReprInfo>
        /// This file's own intrinsics: bare declared name → `SymbolKey` qualified by the
        /// declaring `namespace`. The VALUE carries the arity, the table key does not. THE
        /// name → key index for intrinsics: every other intrinsic table is key-addressed, so
        /// a name is resolved here once and the key travels from there.
        IntrinsicKeys: Dictionary<string, TypeKey>
        /// Inline intrinsic-abbrevs carrying `with member …` augmentations
        /// (`type widget = (# "object" #) with member …`), keyed by the abbrev's CANON key, which
        /// is the namespace-homed identity a use site's `TyConst` carries, not its container-homed
        /// nominal claim. A name reaches it only through `IntrinsicKeys`.
        IntrinsicAbbrevHost: Dictionary<TypeKey, IntrinsicAbbrevInfo>
        /// Reverse index: record short name (NO arity suffix, as written) → the `TypeKey`s
        /// claiming it. A bare name yields a *candidate set*, never one entry.
        RecordNames: Dictionary<string, ResizeArray<TypeKey>>
        /// Reverse index: union short name → candidate `TypeKey`s. See `RecordNames`.
        UnionNames: Dictionary<string, ResizeArray<TypeKey>>
        /// Reverse index: class / interface short name → candidate `TypeKey`s.
        ClassNames: Dictionary<string, ResizeArray<TypeKey>>
        /// Reverse index: abbreviation short name → candidate `TypeKey`s. See `RecordNames`.
        AbbreviationNames: Dictionary<string, ResizeArray<TypeKey>>
        /// THE name table: short name → every `(container, name, arity)` claim under it, of any
        /// KIND. At most one type may hold a claim; several under one name are ranked.
        TypeClaims: Dictionary<string, ResizeArray<TypeIdentity>>
        /// The module / namespace scopes this file DECLARES, keyed by the dotted SOURCE path
        /// an `open` names them by (`"N"`, `"N.A"`), not the compiled name a `ModuleKey` holds.
        LocalContainers: Dictionary<string, ModuleContainer>
        /// The INVERSE of `LocalContainers`. A qualifier is written relative to a SCOPE (`A.T`
        /// inside `module N.B` means `N.A.T`), so resolving one needs that scope's path.
        LocalContainerPaths: Dictionary<ModuleContainer, string>
        /// The RECORD / UNION / CLASS short names this file declares, which is what a `module` of
        /// the same name collides with. Filled whole-file first: `module Foo` may precede `type Foo`.
        NominalTypeNames: HashSet<string>
        /// Each minted type `TypeKey` → the decl-site `NodeKey` that first minted it. A second
        /// DISTINCT declaration minting the same key means the mint dropped a containment.
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
            IntrinsicReprKeys = Dictionary<_, _>()
            IntrinsicKeys = Dictionary<_, _>()
            IntrinsicAbbrevHost = Dictionary<_, _>()
            RecordNames = Dictionary<_, _>()
            UnionNames = Dictionary<_, _>()
            ClassNames = Dictionary<_, _>()
            AbbreviationNames = Dictionary<_, _>()
            TypeClaims = Dictionary<_, _>()
            LocalContainers = Dictionary<_, _>()
            LocalContainerPaths = Dictionary<_, _>()
            NominalTypeNames = HashSet<_>()
            SymbolKeyOrigins = Dictionary<_, _>()
        }

/// The project-local type registries, keyed by `TypeKey`, so a NAME addresses a candidate set.
/// A by-name lookup takes a `UseSite` and answers from the claims visible there; `…ByKey` none.
module TypeRegistry =

    /// The identity key for a locally-declared intrinsic (`int`, `[]`, a user intrinsic-abbrev),
    /// stamped at registration. Deliberately WITHOUT a by-name mint fallback: registration
    /// stamps every `IntrinsicRepr` claim, so a miss means the caller is asking about a name
    /// that never claimed one, and a minted `Vesper`-homed arity-0 key would match nothing.
    let intrinsicKeyOf (types: PassContextTypes) (name: string) : TypeKey =
        match types.IntrinsicKeys.TryGetValue name with
        | true, k -> k
        | _ -> failwithf "Internal error: intrinsic '%s' has no stamped identity key" name

    /// `intrinsicKeyOf` for a caller that does not know whether `name` resolves to a local
    /// intrinsic at all: the lookup arm of the name → key index.
    let tryIntrinsicKeyOf (types: PassContextTypes) (name: string) : TypeKey voption =
        match types.IntrinsicKeys.TryGetValue name with
        | true, k -> ValueSome k
        | _ -> ValueNone

    // --- Key-addressed mechanism (shared by Record, Union and Class) --------------

    /// Register `info` under its own `TypeKey`, indexing that key under the short `name`.
    /// Idempotent: a re-register refreshes the entry without duplicating the index candidate.
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
    // F# adds each declaration and each `open` to the name environment in source order, last
    // wins. `claimRank` is that ordering; MAX over a candidate set is the rule.

    /// The scope this file declares under the dotted SOURCE `path`, as written INSIDE the scope
    /// whose own source path is `scope`: `scope.path` first, then ever-shorter prefixes.
    let private tryContainerOfPath (types: PassContextTypes) (scope: string) (path: string) : ModuleContainer voption =
        let rec go (scope: string) =
            let qualified = if scope.Length = 0 then path else scope + "." + path

            match types.LocalContainers.TryGetValue qualified with
            | true, h -> ValueSome h
            | false, _ ->
                if scope.Length = 0 then
                    ValueNone
                else
                    let cut = scope.LastIndexOf '.'
                    go (if cut < 0 then "" else scope.Substring(0, cut))

        go scope

    /// The module / namespace this `open` resolves to, if THIS file declares it. Its written path is
    /// resolved from the scope it is written in.
    let private openedContainer (types: PassContextTypes) (o: LocalOpen) : ModuleContainer voption =
        tryContainerOfPath types o.Scope o.Path

    /// The scope the dotted SOURCE `path` reaches when written INSIDE `enclosing`, and `enclosing`
    /// itself for an empty path. An EXACT descent, no walking outward.
    let private containerUnder
        (types: PassContextTypes)
        (enclosing: ModuleContainer)
        (path: string)
        : ModuleContainer voption =
        if path.Length = 0 then
            ValueSome enclosing
        else
            match types.LocalContainerPaths.TryGetValue enclosing with
            | true, basePath ->
                let qualified = if basePath.Length = 0 then path else basePath + "." + path

                match types.LocalContainers.TryGetValue qualified with
                | true, h -> ValueSome h
                | false, _ -> ValueNone
            | false, _ -> ValueNone

    /// ONE way a written name REACHES a container from a use site, and where it enters there.
    [<Struct; NoComparison>]
    type private ContainerReach =
        {
            /// The container reached: where the claim must be held for this reach to answer.
            Container: ModuleContainer
            /// How many `module`s enclose whatever ADDED the name (the enclosing scope, or
            /// the `open`). An inner scope is entered later, making resolution innermost-out.
            Depth: int
            /// An `open`'s own offset, where the reach fixes it. `ValueNone` when a SCOPE
            /// reaches directly: a declaration enters at its own `VisibleFrom`.
            Offset: int voption
        }

    /// EVERY way the written module `path` (EMPTY for a bare name) reaches a scope of this file
    /// from `useSite`. Empty for a path that does not reach a scope of this file (`System.Uri`).
    let private pathReaches (types: PassContextTypes) (useSite: UseSite) (path: string) : ContainerReach list =
        match useSite.Container with
        | ValueNone -> []
        | ValueSome here ->
            let reaches = ResizeArray()

            for h in here.SelfAndAncestors do
                match containerUnder types h path with
                | ValueSome reached ->
                    reaches.Add
                        {
                            Container = reached
                            Depth = h.Depth
                            Offset = ValueNone
                        }
                | ValueNone -> ()

            for o in useSite.Opens do
                match openedContainer types o with
                | ValueSome opened ->
                    match containerUnder types opened path with
                    | ValueSome reached ->
                        reaches.Add
                            {
                                Container = reached
                                Depth = o.ScopeDepth
                                Offset = ValueSome o.Offset
                            }
                    | ValueNone -> ()
                | ValueNone -> ()

            if path.Length > 0 then
                match types.LocalContainers.TryGetValue path with
                | true, reached ->
                    reaches.Add
                        {
                            Container = reached
                            Depth = 0
                            Offset = ValueNone
                        }
                | false, _ -> ()

            List.ofSeq reaches

    /// WHERE this claim enters the name environment at `useSite`, `ValueNone` if out of scope.
    /// The MAXIMUM over every reach that lands on the scope HOLDING the claim.
    let private claimRank
        (useSite: UseSite)
        (reaches: ContainerReach list)
        (claim: TypeIdentity)
        : BindingRank voption =
        if claim.VisibleFrom > useSite.Offset then
            ValueNone
        else
            match useSite.Container with
            // Nowhere to speak from: the whole-file view (`UseSite.unbounded`). Every claim is
            // in scope and none outranks another, so a caller that must choose takes the first.
            | ValueNone -> ValueSome { Depth = 0; Offset = 0 }
            | ValueSome _ ->
                let mutable best = ValueNone

                for r in reaches do
                    if r.Container = claim.Container then
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

    /// The max-rank claim on `written` that `admit`s at `useSite`. Every by-name lookup is this
    /// with a different `admit`; a qualified name reads from the scope its path reaches.
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

    /// Is the type `key` (claimed under the short name `name`) visible from `useSite`? The kind
    /// indexes map a name to KEYS, but the scoping facts live on the CLAIM.
    let private keyVisibleAt (types: PassContextTypes) (useSite: UseSite) (name: string) (key: TypeKey) : bool =
        (tryWinner types useSite (WrittenTypeName.bare name) (fun c -> c.Key = key)).IsSome

    /// The key `written` claims at EXACTLY this arity from `useSite`, restricted to THIS kind's
    /// index, so a same-named type of another kind shadows it into a MISS.
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

    /// What a name written WITHOUT type arguments resolves to at `useSite`: a NON-GENERIC type
    /// of that name, else the candidates' agreed arity, else NOTHING.
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
                // The generic claimants answer only if they agree on an arity.
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

    let private tryByTypeKey (table: Dictionary<TypeKey, 'T>) (key: TypeKey) : 'T voption =
        match table.TryGetValue key with
        | true, info -> ValueSome info
        | false, _ -> ValueNone

    // --- The name table -------------------------------------------------------------

    /// Accept a type declaration: claim `(Container, Name, Arity)` in the name table. The caller
    /// has already rejected a contested claim.
    let claimType (types: PassContextTypes) (id: TypeIdentity) : unit =
        match types.TypeClaims.TryGetValue id.Name with
        | true, claims -> claims.Add id
        | false, _ ->
            let claims = ResizeArray 1
            claims.Add id
            types.TypeClaims.[id.Name] <- claims

    /// The identity the WRITTEN name at `arity` MEANS at `useSite`: the winning claim. A
    /// qualified name (`A.T`) is the same lookup, its path saying which SCOPE to read from.
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

    /// THE duplicate-type-definition test: is `(container, name, arity)` already claimed, by any
    /// kind? It takes no use site, because the registration scan's own position scopes it.
    let isTypeClaimed (types: PassContextTypes) (container: ModuleContainer) (name: string) (arity: int) : bool =
        match types.TypeClaims.TryGetValue name with
        | true, claims -> claims.Exists(fun c -> c.TyparArity = arity && c.Container = container)
        | false, _ -> false

    /// The claim the written name reaches at `useSite` at ANY arity: the local/external
    /// precedence test. Arity-blind: a wrong-arity name is LOCAL, never an external namesake.
    let tryWrittenTypeClaimAnyArity
        (types: PassContextTypes)
        (useSite: UseSite)
        (written: WrittenTypeName)
        : TypeIdentity voption =
        tryWinner types useSite written (fun _ -> true)

    /// `tryWrittenTypeClaimAnyArity` for a name written with no qualifier.
    let tryTypeClaimAnyArity (types: PassContextTypes) (useSite: UseSite) (name: string) : TypeIdentity voption =
        tryWrittenTypeClaimAnyArity types useSite (WrittenTypeName.bare name)

    /// Does the written name reach a project-local type at `useSite`, at any arity?
    let isWrittenTypeNameInScope (types: PassContextTypes) (useSite: UseSite) (written: WrittenTypeName) : bool =
        (tryWrittenTypeClaimAnyArity types useSite written).IsSome

    /// `isWrittenTypeNameInScope` for a name written with no qualifier.
    let isTypeNameInScope (types: PassContextTypes) (useSite: UseSite) (name: string) : bool =
        isWrittenTypeNameInScope types useSite (WrittenTypeName.bare name)

    /// Record a module / namespace scope this file declares, under the dotted SOURCE path an
    /// `open` or a qualified name spells. Idempotent: every pass re-enters the same scopes.
    let noteLocalContainer (types: PassContextTypes) (path: string) (container: ModuleContainer) : unit =
        types.LocalContainers.[path] <- container
        types.LocalContainerPaths.[container] <- path

    let noteNominalTypeName (types: PassContextTypes) (name: string) : unit =
        types.NominalTypeNames.Add name |> ignore

    /// Does this file declare a record / union / class called `name`? The `…Module` suffix test.
    let isNominalTypeName (types: PassContextTypes) (name: string) : bool = types.NominalTypeNames.Contains name

    // --- Records / unions / classes / abbreviations -----------------------------------
    // All four are arity-overloadable and keyed by their own `TypeKey`, which carries the
    // arity in its `Name`.

    let registerRecord (types: PassContextTypes) (info: RecordTypeInfo) : unit =
        registerKeyed types.Record types.RecordNames info.Name info.TypeKey info

    /// Resolve a record by BARE short name; an arity-overloaded name does not resolve.
    let tryRecord (types: PassContextTypes) (useSite: UseSite) (name: string) : RecordTypeInfo voption =
        tryOfKey types.Record (tryKeyOfArglessName types types.RecordNames useSite (WrittenTypeName.bare name))

    /// Resolve a record by `(name, arity)`, matching the arity exactly, so a wrong arity misses.
    let tryRecordArity
        (types: PassContextTypes)
        (useSite: UseSite)
        (name: string)
        (arity: int)
        : RecordTypeInfo voption =
        tryOfKey types.Record (tryKeyOfArity types types.RecordNames useSite (WrittenTypeName.bare name) arity)

    /// Resolve a record by its project-local `SymbolKey`.
    let tryRecordByKey (types: PassContextTypes) (key: TypeKey) : RecordTypeInfo voption = tryByTypeKey types.Record key

    let registerClass (types: PassContextTypes) (info: ClassTypeInfo) : unit =
        registerKeyed types.Class types.ClassNames info.Name info.TypeKey info

    /// Resolve a class by the name as WRITTEN: bare (`T`), or qualified by its module (`A.T`)
    /// as a body outside `A` writes it.
    let tryWrittenClass
        (types: PassContextTypes)
        (useSite: UseSite)
        (written: WrittenTypeName)
        : ClassTypeInfo voption =
        tryOfKey types.Class (tryKeyOfArglessName types types.ClassNames useSite written)

    /// Resolve a class by BARE short name, for the recognition-only call sites.
    let tryClass (types: PassContextTypes) (useSite: UseSite) (name: string) : ClassTypeInfo voption =
        tryWrittenClass types useSite (WrittenTypeName.bare name)

    /// Resolve a class by `(name, arity)`, matching the arity exactly, so a wrong arity misses.
    let tryClassArity (types: PassContextTypes) (useSite: UseSite) (name: string) (arity: int) : ClassTypeInfo voption =
        tryOfKey types.Class (tryKeyOfArity types types.ClassNames useSite (WrittenTypeName.bare name) arity)

    /// Resolve a class by its project-local `SymbolKey`.
    let tryClassByKey (types: PassContextTypes) (key: TypeKey) : ClassTypeInfo voption = tryByTypeKey types.Class key

    /// True iff a class is registered under this key: the local-vs-external test a caller
    /// holding a `TyClass` key asks.
    let containsClassKey (types: PassContextTypes) (key: TypeKey) : bool = (tryByTypeKey types.Class key).IsSome

    /// A class, union *or* record by key, as the shared `IInterfaceImplHost`, so a union's or
    /// record's declared interfaces participate in subtyping like a class's.
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

    /// An enum needs no short-name index of its own, being never generic: its claim is always
    /// `(container, name, 0)` and carries the key.
    let registerEnum (types: PassContextTypes) (info: EnumTypeInfo) : unit = types.Enum.[info.TypeKey] <- info

    /// Resolve an enum by bare short name at `useSite`. Never generic, so the `(name, 0)` claim
    /// winning there IS the whole resolution.
    let tryEnum (types: PassContextTypes) (useSite: UseSite) (name: string) : EnumTypeInfo voption =
        match tryTypeClaim types useSite name 0 with
        | ValueNone -> ValueNone
        | ValueSome claim -> tryOfKey types.Enum (ValueSome claim.Key)

    /// Resolve an enum by its project-local `SymbolKey`.
    let tryEnumByKey (types: PassContextTypes) (key: TypeKey) : EnumTypeInfo voption = tryByTypeKey types.Enum key

    let registerAbbrev (types: PassContextTypes) (info: AbbreviationInfo) : unit =
        registerKeyed types.Abbreviation types.AbbreviationNames info.Name info.TypeKey info

    /// Resolve an abbreviation by BARE short name. Cross-kind precedence is NOT its business:
    /// a caller needing to know which kind owns a name asks the name table first.
    let tryAbbrev (types: PassContextTypes) (useSite: UseSite) (name: string) : AbbreviationInfo voption =
        tryOfKey
            types.Abbreviation
            (tryKeyOfArglessName types types.AbbreviationNames useSite (WrittenTypeName.bare name))

    /// Resolve an abbreviation by `(name, arity)`, matching the arity exactly, so a wrong arity misses.
    let tryAbbrevArity
        (types: PassContextTypes)
        (useSite: UseSite)
        (name: string)
        (arity: int)
        : AbbreviationInfo voption =
        tryOfKey
            types.Abbreviation
            (tryKeyOfArity types types.AbbreviationNames useSite (WrittenTypeName.bare name) arity)

    /// Resolve an abbreviation by its project-local `SymbolKey`.
    let tryAbbrevByKey (types: PassContextTypes) (key: TypeKey) : AbbreviationInfo voption =
        tryByTypeKey types.Abbreviation key

    /// Resolve an abbreviation by the `(name, arity)` a well-known identity SPELLS, rather
    /// than by that identity.
    let tryAbbrevSpelling (types: PassContextTypes) (useSite: UseSite) (spelling: TypeKey) : AbbreviationInfo voption =
        tryAbbrevArity types useSite spelling.Name spelling.TyparArity

    /// Register a union under its own `TypeKey`. See `registerRecord`.
    let registerUnion (types: PassContextTypes) (info: UnionTypeInfo) : unit =
        registerKeyed types.Union types.UnionNames info.Name info.TypeKey info

    /// Resolve a union by `(name, arity)`, matching the arity exactly, so a wrong arity misses.
    let tryUnion (types: PassContextTypes) (useSite: UseSite) (name: string) (arity: int) : UnionTypeInfo voption =
        tryOfKey types.Union (tryKeyOfArity types types.UnionNames useSite (WrittenTypeName.bare name) arity)

    /// Resolve a union by BARE short name, for the recognition-only call sites.
    let tryUnionBare (types: PassContextTypes) (useSite: UseSite) (name: string) : UnionTypeInfo voption =
        tryOfKey types.Union (tryKeyOfArglessName types types.UnionNames useSite (WrittenTypeName.bare name))

    /// Resolve a union by its project-local `TypeKey`.
    let tryUnionByKey (types: PassContextTypes) (key: TypeKey) : UnionTypeInfo voption = tryByTypeKey types.Union key

    /// A member-bearing nominal: its members, and the type parameters a member signature is
    /// instantiated against. Class, union and record share this shape.
    [<Struct>]
    type NominalDecl =
        {
            TypeKey: TypeKey
            TypeParams: EqArray<string * TyVarId>
            Members: TypeMemberInfo[]
        }

    let private nominalDecl
        (typeKey: TypeKey)
        (typeParams: EqArray<string * TyVarId>)
        (members: TypeMemberInfo[])
        : NominalDecl voption =
        ValueSome
            {
                TypeKey = typeKey
                TypeParams = typeParams
                Members = members
            }

    /// Resolve a class / union / record by its project-local `TypeKey`, whichever kind holds it.
    let tryNominalByKey (types: PassContextTypes) (key: TypeKey) : NominalDecl voption =
        match tryClassByKey types key with
        | ValueSome info -> nominalDecl info.TypeKey info.TypeParams info.Members
        | ValueNone ->
            match tryUnionByKey types key with
            | ValueSome info -> nominalDecl info.TypeKey info.TypeParams info.Members
            | ValueNone ->
                match tryRecordByKey types key with
                | ValueSome info -> nominalDecl info.TypeKey info.TypeParams info.Members
                | ValueNone -> ValueNone

    /// The same by NAME, resolved AS SEEN FROM `useSite`: a type declared below the reference
    /// does not answer for its name.
    let tryNominal (types: PassContextTypes) (useSite: UseSite) (name: string) : NominalDecl voption =
        match tryClass types useSite name with
        | ValueSome info -> nominalDecl info.TypeKey info.TypeParams info.Members
        | ValueNone ->
            match tryUnionBare types useSite name with
            | ValueSome info -> nominalDecl info.TypeKey info.TypeParams info.Members
            | ValueNone ->
                match tryRecord types useSite name with
                | ValueSome info -> nominalDecl info.TypeKey info.TypeParams info.Members
                | ValueNone -> ValueNone

    /// A member found on a nominal, alongside the declaration holding it: the `TypeKey` a call
    /// is minted against, and the type parameters the member's signature instantiates at.
    [<Struct>]
    type NominalMember =
        {
            Decl: NominalDecl
            Member: TypeMemberInfo
        }

    let private pickMember (memberName: string) (decl: NominalDecl) : NominalMember voption =
        match decl.Members |> Array.tryFind (fun m -> m.Name = memberName) with
        | Some m -> ValueSome { Decl = decl; Member = m }
        | None -> ValueNone

    /// `memberName` on the class / union / record under `key`, read by the ARITY-QUALIFIED key
    /// (``Name`arity``): an arity-overloaded type does not resolve by bare name.
    let tryNominalMemberByKey (types: PassContextTypes) (key: TypeKey) (memberName: string) : NominalMember voption =
        tryNominalByKey types key |> ValueOption.bind (pickMember memberName)

    /// `memberName` on the inline intrinsic-abbrev host filed under the intrinsic's CANON
    /// `key` — the key a use site's `TyConst` carries.
    let tryIntrinsicAbbrevMemberByKey
        (types: PassContextTypes)
        (key: TypeKey)
        (memberName: string)
        : NominalMember voption =
        match types.IntrinsicAbbrevHost.TryGetValue key with
        | true, info ->
            nominalDecl info.TypeKey info.TypeParams info.Members
            |> ValueOption.bind (pickMember memberName)
        | false, _ -> ValueNone

    /// `C.M`: the static `M` on the class / union / record `C` denotes, resolved AS SEEN FROM
    /// `useSite`. An instance member of that name misses, so a caller cannot mistake one for
    /// a qualified static access.
    let tryStaticMember
        (types: PassContextTypes)
        (useSite: UseSite)
        (typeName: string)
        (memberName: string)
        : NominalMember voption =
        tryNominal types useSite typeName
        |> ValueOption.bind (pickMember memberName)
        |> ValueOption.filter (fun nm -> nm.Member.IsStatic)

    /// The inline intrinsic-abbrev host a NAME denotes: the name is resolved to the intrinsic's
    /// canon key through `IntrinsicKeys`, and the host read by that key. A name that claimed no
    /// intrinsic identity misses here rather than matching a same-named entry, so a bare read
    /// cannot reach a host across the namespace or arity its key records.
    let tryIntrinsicAbbrevHostByCanon (types: PassContextTypes) (name: string) : IntrinsicAbbrevInfo voption =
        match tryIntrinsicKeyOf types name with
        | ValueNone -> ValueNone
        | ValueSome key ->
            match types.IntrinsicAbbrevHost.TryGetValue key with
            | true, info -> ValueSome info
            | false, _ -> ValueNone

    let private tryIntrinsicAbbrevHostByName (types: PassContextTypes) (name: string) : IInterfaceImplHost voption =
        tryIntrinsicAbbrevHostByCanon types name
        |> ValueOption.map (fun info -> info :> IInterfaceImplHost)

    /// A union, record or inline intrinsic-abbrev host by bare short name, as the shared
    /// `IInterfaceImplHost`. Classes are excluded, because they fill through their own path.
    let tryNonClassMemberHost (types: PassContextTypes) (useSite: UseSite) (name: string) : IInterfaceImplHost voption =
        match tryUnionBare types useSite name with
        | ValueSome info -> ValueSome(info :> IInterfaceImplHost)
        | ValueNone ->
            match tryRecord types useSite name with
            | ValueSome info -> ValueSome(info :> IInterfaceImplHost)
            | ValueNone -> tryIntrinsicAbbrevHostByName types name

    /// The key-addressed twin of the above, for what a DECLARATION claims, because two sibling
    /// modules may each declare `T`. Every kind answers by KEY, but by a DIFFERENT key for the
    /// intrinsic arm, which is why the name is still a parameter: an intrinsic binding carries
    /// two, the container-homed claim `key` identifying it here and the namespace-homed canon
    /// addressing the host table, which the name resolves to through `IntrinsicKeys`.
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
            | ValueNone -> tryIntrinsicAbbrevHostByName types name

    /// The declaring union of a registered case: the case carries its union's `TypeKey`, so no
    /// use site is needed, and a caller holding a case got it from a scoped read anyway.
    let unionOfCase (types: PassContextTypes) (info: UnionCaseInfo) : UnionTypeInfo =
        match tryOfKey types.Union (ValueSome info.UnionKey) with
        | ValueSome u -> u
        | ValueNone -> failwithf "Internal error: union case '%s' has no registered union '%s'" info.Name info.UnionName

    // --- The reverse name indexes: fields and union cases -----------------------------
    // A field / case name holds no claim of its own; its OWNER's claim scopes it.

    /// Is the union declaring `case` visible from `useSite`?
    let private caseVisibleAt (types: PassContextTypes) (useSite: UseSite) (case: UnionCaseInfo) : bool =
        keyVisibleAt types useSite case.UnionName case.UnionKey

    /// The records declaring a field `name` VISIBLE from `useSite`: the candidate set a record
    /// literal intersects over. `{ a = 1 }` written above `type R = { a: int }` matches none.
    let recordsWithField (types: PassContextTypes) (useSite: UseSite) (name: string) : RecordTypeInfo[] =
        match types.FieldIndex.TryGetValue name with
        | true, infos ->
            let hits = ResizeArray infos.Length

            for info in infos do
                if keyVisibleAt types useSite info.Name info.TypeKey then
                    hits.Add info

            hits.ToArray()
        | false, _ -> Array.empty

    /// The union cases named `name` VISIBLE from `useSite`. More than one is ambiguous; none
    /// leaves an uppercase ident an ordinary bound variable in a pattern, unresolved in an expression.
    let casesNamed (types: PassContextTypes) (useSite: UseSite) (name: string) : UnionCaseInfo[] =
        match types.CtorIndex.TryGetValue name with
        | true, infos ->
            let hits = ResizeArray infos.Length

            for case in infos do
                if caseVisibleAt types useSite case then
                    hits.Add case

            hits.ToArray()
        | false, _ -> Array.empty

    /// The ctor-vs-bound variable test: does `name` resolve to a union case visible from `useSite`?
    let isCaseName (types: PassContextTypes) (useSite: UseSite) (name: string) : bool =
        match types.CtorIndex.TryGetValue name with
        | true, infos -> infos |> EqArray.exists (caseVisibleAt types useSite)
        | false, _ -> false

    /// Is `qualifier.caseName` a *local* union-case reference visible from `useSite`?
    /// Arity-safe: `Choice.Choice1Of3` reaches ``Choice`3``.
    let localQualifiedCase (types: PassContextTypes) (useSite: UseSite) (qualifier: string) (caseName: string) : bool =
        match types.CtorIndex.TryGetValue caseName with
        | true, infos ->
            infos
            |> EqArray.exists (fun c -> c.UnionName = qualifier && caseVisibleAt types useSite c)
        | false, _ -> false

    /// Record the decl-site origin of a freshly-minted `SymbolKey`. Returns the PRIOR
    /// declaration's `NodeKey` if a DIFFERENT declaration already minted `key`.
    let recordKeyOrigin (types: PassContextTypes) (declKey: NodeKey) (key: SymbolKey) : NodeKey voption =
        match types.SymbolKeyOrigins.TryGetValue key with
        | true, prior when prior <> declKey -> ValueSome prior
        | true, _ -> ValueNone
        | false, _ ->
            types.SymbolKeyOrigins.[key] <- declKey
            ValueNone
