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
    /// name-table citizen like any other type's; its platform type id is not.
    | IntrinsicBinding
    /// A `[<Measure>]` declaration, in either shape: the body-less `type m` and the
    /// abbreviation `type v = m / s`. It claims its name like any other type, and a
    /// reference to it is admitted only in measure position.
    | Measure

/// The positional facts of one declaration group: where its claims become visible, and where
/// they enter the name environment within their depth. The two differ only under `rec`, which
/// hoists visibility to the scope's keyword and enters the declarations after its prelude.
[<Struct>]
type ClaimPlacement = { VisibleFrom: int; EntersAt: int }

/// The nominal identity of one type declaration: everything NOT kind-specific. Every member
/// of a `type … and …` group is claimed before any of the group's per-kind registrars run.
type TypeIdentity =
    {
        /// The short name as written (no arity suffix, because `Key.Name` carries that).
        Name: string
        /// Part of the CLAIM: `Foo` and `` Foo`1 `` are distinct, and may be different kinds.
        TyparArity: int
        /// Part of the CLAIM: `N.A.T` and `N.B.T` are two types, not one name contested twice.
        /// Also its reach: a bare name resolves to it from inside, or via an `open`.
        Container: ModuleContainer
        Kind: TypeDeclKind
        /// The key stamped onto the kind-specific `*TypeInfo`, and the name token spelling it.
        DeclSite: NodeSite
        Key: TypeKey
        /// A use at offset `u` sees this claim iff `VisibleFrom <= u`. It is the first token of
        /// the claim's `type … and …` GROUP, or a `module rec` / `namespace rec` keyword.
        VisibleFrom: int
        /// WHERE the claim enters the name environment within its depth: `VisibleFrom`, or
        /// `BindingRank.afterPrelude` under `rec`.
        EntersAt: int
    }

/// An ACCEPTED type declaration, paired with the CST it was claimed from. Every per-kind
/// detail registrar is HANDED this identity rather than re-deriving name / arity / key.
[<NoEquality; NoComparison>]
type ClaimedTypeDefn =
    {
        Identity: TypeIdentity
        Defn: TypeDefn<SyntaxToken>
    }

/// The claim a type name written WITHOUT type arguments takes at a use site.
[<RequireQualifiedAccess>]
type ArglessClaim =
    /// The max-rank non-generic claim, else the max-rank claim at the one arity claimed.
    | Takes of TypeIdentity
    /// Claims reach the use site at several arities, none of them 0 (FS1124). `arities` is
    /// ascending; `recovery` is the max-rank claim at the smallest arity.
    | Disagreement of arities: EqArray<int> * recovery: TypeIdentity
    | NoClaim

/// One type kind's entries, addressed by the type's own `TypeKey`: the WHOLE containment
/// chain, not a name. A name resolves to a key through the name table, `TypeClaims`.
type KindRegistry<'Info> = Dictionary<TypeKey, 'Info>

type PassContextTypes =
    {
        /// Field types are RESOLVED at registration, against the types in scope at the declaration.
        Record: KindRegistry<RecordTypeInfo>
        /// Case field types are resolved at registration.
        Union: KindRegistry<UnionTypeInfo>
        /// The declared structure (ctor-param annotations, `val` field types) is resolved at
        /// registration; MEMBER types start as placeholder TyVars, inferred from their bodies.
        Class: KindRegistry<ClassTypeInfo>
        /// An enum is non-generic, so its claim is always `(container, name, 0)`, but still keyed
        /// by `TypeKey`, since two sibling modules may each declare one.
        Enum: KindRegistry<EnumTypeInfo>
        /// An alias body is forced by the first thing that references it, at the latest when its
        /// group closes. Expansion is eager: downstream sees the underlying type longhand.
        Abbreviation: KindRegistry<AbbreviationInfo>
        /// A base measure carries no body; an abbreviation's term is forced by the first
        /// measure that references it.
        Measure: KindRegistry<MeasureInfo>
        /// Reverse index: ctor name → case-info entries, each tagged with its declaring union.
        /// Scoped reads only, because a case is visible exactly where its declaring union is.
        CtorIndex: Dictionary<string, EqArray<UnionCaseInfo>>
        /// Reverse index: field name → the record types declaring it; visible where they are.
        FieldIndex: Dictionary<string, EqArray<RecordTypeInfo>>
        /// An intrinsic binding's contract-sourced `SymbolKey` → its target representation,
        /// from `type int = (# "System.Int32" #)`, plus the `class`-tag verdict. NOT
        /// transparent like `Abbreviation`: a use resolves to `TyConst key`, not the RHS.
        /// Key-addressed because a key projected back to a name loses its arity.
        IntrinsicBindings: Dictionary<TypeKey, IntrinsicBindingInfo>
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
        /// THE name table: short name → every `(container, name, arity)` claim under it, of any
        /// KIND. At most one type may hold a claim; several under one name are ranked.
        TypeClaims: Dictionary<string, ResizeArray<TypeIdentity>>
        LocalContainers: LocalContainers
        /// Every module this file declares, with what its declaration states.
        Modules: Dictionary<ModuleKey, ModuleFacts>
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
            Measure = Dictionary<_, _>()
            CtorIndex = Dictionary<_, _>()
            FieldIndex = Dictionary<_, _>()
            IntrinsicBindings = Dictionary<_, _>()
            IntrinsicKeys = Dictionary<_, _>()
            IntrinsicAbbrevHost = Dictionary<_, _>()
            TypeClaims = Dictionary<_, _>()
            LocalContainers = Dictionary<_, _>()
            Modules = Dictionary<_, _>()
            NominalTypeNames = HashSet<_>()
            SymbolKeyOrigins = Dictionary<_, _>()
        }

/// The project-local type registries, keyed by `TypeKey`, so a NAME addresses a candidate set.
/// A by-name lookup takes a `UseSite` and reads the claims visible there; `…ByKey` takes none.
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

    // --- What a use site can see, and which candidate wins ----------------------------
    // F# adds each declaration and each `open` to the name environment in source order, last
    // wins. `claimRank` is that ordering; MAX over a candidate set is the rule.

    /// EVERY way the written module `path` (EMPTY for a bare name) reaches a scope of this file
    /// from `useSite`. Empty for a path that does not reach a scope of this file (`System.Uri`).
    let private pathReaches (types: PassContextTypes) (useSite: UseSite) (path: string) : ScopeEntry list =
        let reaches = ResizeArray()

        for e in useSite.Scopes do
            match ScopeResolution.tryContainerUnder types.LocalContainers e.Container path useSite.Offset with
            | ValueSome reached -> reaches.Add { Container = reached; Route = e.Route }
            | ValueNone -> ()

        if path.Length > 0 then
            match types.LocalContainers.TryGetValue path with
            | true, reached when reached.VisibleFrom <= useSite.Offset ->
                reaches.Add
                    {
                        Container = reached.Container
                        Route = ScopeRoute.Lexical 0
                    }
            | _ -> ()

        List.ofSeq reaches

    /// WHERE this claim enters the name environment at `useSite`, `ValueNone` if out of scope.
    /// The MAXIMUM over every reach that lands on the scope HOLDING the claim.
    let private claimRank (useSite: UseSite) (reaches: ScopeEntry list) (claim: TypeIdentity) : BindingRank voption =
        if claim.VisibleFrom > useSite.Offset then
            ValueNone
        else
            match useSite.Container with
            // No enclosing container: the whole-file view (`UseSite.unbounded`). Every claim is
            // in scope and none outranks another, so a caller that must choose takes the first.
            | ValueNone -> ValueSome { Depth = 0; Offset = 0 }
            | ValueSome _ ->
                BindingRank.maxOf (
                    seq {
                        for r in reaches do
                            if r.Container = claim.Container then
                                ScopeEntry.rankOf r claim.EntersAt
                    }
                )

    /// Every claim on `written` reaching `useSite`, at any arity, beside its rank, best rank
    /// first. Claims sharing a rank keep declaration order. A qualified `written.Path` narrows
    /// the scopes read to those the path reaches.
    let private rankedClaims
        (types: PassContextTypes)
        (useSite: UseSite)
        (written: WrittenTypeName)
        : struct (BindingRank * TypeIdentity) list =
        match types.TypeClaims.TryGetValue written.Name with
        | true, claims ->
            let reaches = pathReaches types useSite written.Path

            [
                for c in claims do
                    match claimRank useSite reaches c with
                    | ValueSome r -> struct (r, c)
                    | ValueNone -> ()
            ]
            |> List.sortByDescending (fun struct (r, _) -> r)
        | false, _ -> []

    /// `pick` applied to the max-rank claim on `written` at `useSite` that `pick` accepts,
    /// beside the rank it won at.
    let private tryPickWinnerRanked
        (types: PassContextTypes)
        (useSite: UseSite)
        (written: WrittenTypeName)
        (pick: TypeIdentity -> 'T voption)
        : struct (BindingRank * 'T) voption =
        let rec first (ranked: struct (BindingRank * TypeIdentity) list) =
            match ranked with
            | [] -> ValueNone
            | struct (r, c) :: rest ->
                match pick c with
                | ValueSome picked -> ValueSome(struct (r, picked))
                | ValueNone -> first rest

        first (rankedClaims types useSite written)

    let private tryPickWinner
        (types: PassContextTypes)
        (useSite: UseSite)
        (written: WrittenTypeName)
        (pick: TypeIdentity -> 'T voption)
        : 'T voption =
        match tryPickWinnerRanked types useSite written pick with
        | ValueSome(struct (_, picked)) -> ValueSome picked
        | ValueNone -> ValueNone

    /// The max-rank claim on `written` that `admit`s at `useSite`.
    let private tryWinner
        (types: PassContextTypes)
        (useSite: UseSite)
        (written: WrittenTypeName)
        (admit: TypeIdentity -> bool)
        : TypeIdentity voption =
        tryPickWinner types useSite written (fun c -> if admit c then ValueSome c else ValueNone)

    /// Every claim the written name reaches at `useSite`, at any arity, best rank first: the
    /// candidate set a qualified case (`Choice.Choice1Of3`) is looked up inside.
    let writtenTypeClaims (types: PassContextTypes) (useSite: UseSite) (written: WrittenTypeName) : TypeIdentity list =
        rankedClaims types useSite written |> List.map (fun struct (_, c) -> c)

    /// The claim a name written without type arguments takes from `claims`, which are ordered
    /// best rank first.
    let private arglessClaimOf (claims: TypeIdentity list) : ArglessClaim =
        let arities =
            claims |> List.map (fun c -> c.TyparArity) |> List.distinct |> List.sort

        let bestAt (arity: int) =
            claims |> List.find (fun c -> c.TyparArity = arity)

        match arities with
        | [] -> ArglessClaim.NoClaim
        | [ arity ]
        | (0 as arity) :: _ -> ArglessClaim.Takes(bestAt arity)
        | nearest :: _ -> ArglessClaim.Disagreement(EqArray.ofList arities, bestAt nearest)

    /// WHERE the type `key` (claimed under the short name `name`) enters the name environment
    /// at `useSite`; `ValueNone` when it is out of scope there.
    let private keyRankAt
        (types: PassContextTypes)
        (useSite: UseSite)
        (name: string)
        (key: TypeKey)
        : BindingRank voption =
        match
            tryPickWinnerRanked
                types
                useSite
                (WrittenTypeName.bare name)
                (fun c -> if c.Key = key then ValueSome() else ValueNone)
        with
        | ValueSome(struct (r, ())) -> ValueSome r
        | ValueNone -> ValueNone

    let private keyVisibleAt (types: PassContextTypes) (useSite: UseSite) (name: string) (key: TypeKey) : bool =
        (keyRankAt types useSite name key).IsSome

    let private tryDict (table: Dictionary<TypeKey, 'T>) (key: TypeKey) : 'T voption =
        match table.TryGetValue key with
        | true, info -> ValueSome info
        | false, _ -> ValueNone

    /// The type the abbreviation under `abbrevKey` ALIASES, once its body is filled: the body
    /// is a keyed type applied to the abbreviation's own type parameters, each exactly once
    /// (`Box<int>` is not an alias).
    let tryAliasedKey (types: PassContextTypes) (abbrevKey: TypeKey) : TypeKey voption =
        match tryDict types.Abbreviation abbrevKey with
        | ValueSome info ->
            match info.State with
            | FillState.Filled(TyKeyed(k, args)) when args.Length = info.TypeParams.Length ->
                let own = HashSet<TyVarId>()

                for tp in info.TypeParams do
                    own.Add tp.TyVar |> ignore

                let isAlias =
                    args
                    |> EqArray.forall (fun a ->
                        match a with
                        | TyVar tv -> own.Remove tv
                        | _ -> false
                    )

                if isAlias then ValueSome k else ValueNone
            | _ -> ValueNone
        | ValueNone -> ValueNone

    /// The identity of this file's type declared under `key`, at any use site.
    let tryIdentityByKey (types: PassContextTypes) (key: TypeKey) : TypeIdentity voption =
        match types.TypeClaims.TryGetValue key.Name with
        | true, claims ->
            match claims.FindIndex(fun c -> c.Key = key) with
            | -1 -> ValueNone
            | i -> ValueSome claims.[i]
        | false, _ -> ValueNone

    /// The key in `reg` the claim `c` stands for: its own key, or the key of the type an
    /// abbreviation claim aliases.
    let private keyInKind (types: PassContextTypes) (reg: KindRegistry<'T>) (c: TypeIdentity) : TypeKey voption =
        if reg.ContainsKey c.Key then
            ValueSome c.Key
        else
            match c.Kind with
            | TypeDeclKind.Abbreviation -> tryAliasedKey types c.Key |> ValueOption.filter reg.ContainsKey
            | _ -> ValueNone

    /// The key in `reg` that `written` at EXACTLY this arity stands for at `useSite`. Claims of
    /// another kind are skipped, not ranked; cross-kind precedence belongs to the caller,
    /// through the name table.
    let private tryKeyOfArity
        (types: PassContextTypes)
        (reg: KindRegistry<'T>)
        (useSite: UseSite)
        (written: WrittenTypeName)
        (arity: int)
        : TypeKey voption =
        tryPickWinner
            types
            useSite
            written
            (fun c ->
                if c.TyparArity = arity then
                    keyInKind types reg c
                else
                    ValueNone
            )

    /// The argless rule applied to the claims on `written` that `reg` holds: `reg`'s own
    /// claims are the whole candidate set, and they alone settle the arity.
    let private arglessClaimIn
        (types: PassContextTypes)
        (reg: KindRegistry<'T>)
        (useSite: UseSite)
        (written: WrittenTypeName)
        : ArglessClaim =
        let inKind (c: TypeIdentity) = (keyInKind types reg c).IsSome

        writtenTypeClaims types useSite written |> List.filter inKind |> arglessClaimOf

    /// The key in `reg` that `written` WITHOUT type arguments stands for at `useSite`: a
    /// NON-GENERIC type of that name, else the candidates' agreed arity. Candidates that
    /// disagree on arity recover to the nearest one, silently.
    let private tryKeyOfArglessName
        (types: PassContextTypes)
        (reg: KindRegistry<'T>)
        (useSite: UseSite)
        (written: WrittenTypeName)
        : TypeKey voption =
        match arglessClaimIn types reg useSite written with
        | ArglessClaim.Takes c
        | ArglessClaim.Disagreement(_, c) -> keyInKind types reg c
        | ArglessClaim.NoClaim -> ValueNone

    /// The claim the bare `name` takes at `useSite` in EXPRESSION position, where it denotes a
    /// CONSTRUCTOR: the argless rule over the CLASSES reaching the site. Where no class claims
    /// the name, every claim is the candidate set, so records still report an arity disagreement.
    let arglessExprClaim (types: PassContextTypes) (useSite: UseSite) (name: string) : ArglessClaim =
        let written = WrittenTypeName.bare name

        match arglessClaimIn types types.Class useSite written with
        | ArglessClaim.NoClaim -> arglessClaimOf (writtenTypeClaims types useSite written)
        | overClasses -> overClasses

    /// Chains onto a by-name resolution, whose miss passes straight through.
    let private tryOfKey (reg: KindRegistry<'T>) (key: TypeKey voption) : 'T voption =
        match key with
        | ValueSome k -> tryDict reg k
        | ValueNone -> ValueNone

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

    /// The claim on the written name whose arity is CLOSEST to `arity`, ties broken by scope
    /// rank. A name claimed at ANY arity resolves LOCAL here, so a wrong-arity write stays this
    /// file's type. Diverges from F#, which takes the max-rank claim at any arity.
    let tryWrittenTypeClaimNearestArity
        (types: PassContextTypes)
        (useSite: UseSite)
        (written: WrittenTypeName)
        (arity: int)
        : TypeIdentity voption =
        match writtenTypeClaims types useSite written with
        | [] -> ValueNone
        | claims -> ValueSome(claims |> List.minBy (fun c -> abs (c.TyparArity - arity)))

    /// Record a module / namespace scope this file declares, under the dotted SOURCE path an
    /// `open` or a qualified name spells. Idempotent: every pass re-enters the same scopes.
    let noteLocalContainer (types: PassContextTypes) (path: string) (container: LocalContainer) : unit =
        types.LocalContainers.[path] <- container

    let noteNominalTypeName (types: PassContextTypes) (name: string) : unit =
        types.NominalTypeNames.Add name |> ignore

    /// Does this file declare a record / union / class called `name`? The `…Module` suffix test.
    let isNominalTypeName (types: PassContextTypes) (name: string) : bool = types.NominalTypeNames.Contains name

    // --- Records / unions / classes / abbreviations -----------------------------------
    // All four are arity-overloadable and keyed by their own `TypeKey`, which carries the
    // arity in its `Name`.

    let registerRecord (types: PassContextTypes) (info: RecordTypeInfo) : unit = types.Record.[info.TypeKey] <- info

    /// Resolve a record by BARE short name; an arity-overloaded name does not resolve.
    let tryRecord (types: PassContextTypes) (useSite: UseSite) (name: string) : RecordTypeInfo voption =
        tryOfKey types.Record (tryKeyOfArglessName types types.Record useSite (WrittenTypeName.bare name))

    /// Resolve a record by `(name, arity)`, matching the arity exactly, so a wrong arity misses.
    let tryRecordArity
        (types: PassContextTypes)
        (useSite: UseSite)
        (name: string)
        (arity: int)
        : RecordTypeInfo voption =
        tryOfKey types.Record (tryKeyOfArity types types.Record useSite (WrittenTypeName.bare name) arity)

    /// Resolve a record by its project-local `SymbolKey`.
    let tryRecordByKey (types: PassContextTypes) (key: TypeKey) : RecordTypeInfo voption = tryDict types.Record key

    let registerClass (types: PassContextTypes) (info: ClassTypeInfo) : unit = types.Class.[info.TypeKey] <- info

    /// Resolve a class by the name as WRITTEN: bare (`T`), or qualified by its module (`A.T`)
    /// as a body outside `A` writes it.
    let tryWrittenClass
        (types: PassContextTypes)
        (useSite: UseSite)
        (written: WrittenTypeName)
        : ClassTypeInfo voption =
        tryOfKey types.Class (tryKeyOfArglessName types types.Class useSite written)

    /// Resolve a class by BARE short name, for the recognition-only call sites.
    let tryClass (types: PassContextTypes) (useSite: UseSite) (name: string) : ClassTypeInfo voption =
        tryWrittenClass types useSite (WrittenTypeName.bare name)

    /// Resolve a class by `(name, arity)`, matching the arity exactly, so a wrong arity misses.
    let tryClassArity (types: PassContextTypes) (useSite: UseSite) (name: string) (arity: int) : ClassTypeInfo voption =
        tryOfKey types.Class (tryKeyOfArity types types.Class useSite (WrittenTypeName.bare name) arity)

    /// Resolve a class by its project-local `SymbolKey`.
    let tryClassByKey (types: PassContextTypes) (key: TypeKey) : ClassTypeInfo voption = tryDict types.Class key

    /// True iff a class is registered under this key: the local-vs-external test a caller
    /// holding a `TyClass` key asks.
    let containsClassKey (types: PassContextTypes) (key: TypeKey) : bool = types.Class.ContainsKey key

    /// One leg of a member-host cascade: `info` widened to `IInterfaceImplHost`, else the
    /// result of the next probe.
    let inline private hostOr<'Info when 'Info :> IInterfaceImplHost>
        (info: 'Info voption)
        ([<InlineIfLambda>] next: unit -> IInterfaceImplHost voption)
        : IInterfaceImplHost voption =
        match info with
        | ValueSome info -> ValueSome(info :> IInterfaceImplHost)
        | ValueNone -> next ()

    /// A class, union *or* record by key, as the shared `IInterfaceImplHost`, so a union's or
    /// record's declared interfaces participate in subtyping like a class's.
    let tryInterfaceImplHostByKey (types: PassContextTypes) (key: TypeKey) : IInterfaceImplHost voption =
        hostOr
            (tryDict types.Class key)
            (fun () ->
                hostOr (tryDict types.Union key) (fun () -> hostOr (tryDict types.Record key) (fun () -> ValueNone))
            )

    /// An enum needs no short-name index of its own, being never generic: its claim is always
    /// `(container, name, 0)` and carries the key.
    let registerEnum (types: PassContextTypes) (info: EnumTypeInfo) : unit = types.Enum.[info.TypeKey] <- info

    /// Resolve an enum by bare short name at `useSite`. Never generic, so the `(name, 0)` claim
    /// winning there IS the whole resolution.
    let tryEnum (types: PassContextTypes) (useSite: UseSite) (name: string) : EnumTypeInfo voption =
        match tryTypeClaim types useSite name 0 with
        | ValueNone -> ValueNone
        | ValueSome claim -> tryDict types.Enum claim.Key

    /// Resolve an enum by its project-local `SymbolKey`.
    let tryEnumByKey (types: PassContextTypes) (key: TypeKey) : EnumTypeInfo voption = tryDict types.Enum key

    let registerAbbrev (types: PassContextTypes) (info: AbbreviationInfo) : unit =
        types.Abbreviation.[info.TypeKey] <- info

    /// Resolve an abbreviation by BARE short name. Cross-kind precedence is NOT its business:
    /// a caller needing to know which kind owns a name asks the name table first.
    let tryAbbrev (types: PassContextTypes) (useSite: UseSite) (name: string) : AbbreviationInfo voption =
        tryOfKey types.Abbreviation (tryKeyOfArglessName types types.Abbreviation useSite (WrittenTypeName.bare name))

    /// Resolve an abbreviation by `(name, arity)`, matching the arity exactly, so a wrong arity misses.
    let tryAbbrevArity
        (types: PassContextTypes)
        (useSite: UseSite)
        (name: string)
        (arity: int)
        : AbbreviationInfo voption =
        tryOfKey types.Abbreviation (tryKeyOfArity types types.Abbreviation useSite (WrittenTypeName.bare name) arity)

    /// Resolve an abbreviation by its project-local `SymbolKey`.
    let tryAbbrevByKey (types: PassContextTypes) (key: TypeKey) : AbbreviationInfo voption =
        tryDict types.Abbreviation key

    let registerMeasure (types: PassContextTypes) (info: MeasureInfo) : unit = types.Measure.[info.TypeKey] <- info

    let tryMeasureByKey (types: PassContextTypes) (key: TypeKey) : MeasureInfo voption = tryDict types.Measure key

    /// Resolve an abbreviation by the `(name, arity)` a well-known identity SPELLS, rather
    /// than by that identity.
    let tryAbbrevSpelling (types: PassContextTypes) (useSite: UseSite) (spelling: TypeKey) : AbbreviationInfo voption =
        tryAbbrevArity types useSite spelling.Name spelling.TyparArity

    /// Register a union under its own `TypeKey`. See `registerRecord`.
    let registerUnion (types: PassContextTypes) (info: UnionTypeInfo) : unit = types.Union.[info.TypeKey] <- info

    /// Resolve a union by `(name, arity)`, matching the arity exactly, so a wrong arity misses.
    let tryUnion (types: PassContextTypes) (useSite: UseSite) (name: string) (arity: int) : UnionTypeInfo voption =
        tryOfKey types.Union (tryKeyOfArity types types.Union useSite (WrittenTypeName.bare name) arity)

    /// Resolve a union by BARE short name, for the recognition-only call sites.
    let tryUnionBare (types: PassContextTypes) (useSite: UseSite) (name: string) : UnionTypeInfo voption =
        tryOfKey types.Union (tryKeyOfArglessName types types.Union useSite (WrittenTypeName.bare name))

    /// Resolve a union by its project-local `TypeKey`.
    let tryUnionByKey (types: PassContextTypes) (key: TypeKey) : UnionTypeInfo voption = tryDict types.Union key

    /// A member-bearing nominal: its members, and the type parameters a member signature is
    /// instantiated against. Class, union and record share this shape.
    [<Struct>]
    type NominalDecl =
        {
            TypeKey: TypeKey
            TypeParams: EqArray<DeclaredTypar>
            Members: TypeMemberInfo[]
        }

    let private nominalDecl
        (typeKey: TypeKey)
        (typeParams: EqArray<DeclaredTypar>)
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

    /// The same by NAME, resolved AS SEEN FROM `useSite`: only a type visible at that offset
    /// resolves.
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

    /// The inline intrinsic-abbrev host filed under the intrinsic's CANON `key` — the key
    /// a use site's `TyConst` carries — as a nominal decl.
    let tryIntrinsicAbbrevByKey (types: PassContextTypes) (key: TypeKey) : NominalDecl voption =
        match types.IntrinsicAbbrevHost.TryGetValue key with
        | true, info -> nominalDecl info.TypeKey info.TypeParams info.Members
        | false, _ -> ValueNone

    /// `memberName` on the intrinsic-abbrev host under `key`.
    let tryIntrinsicAbbrevMemberByKey
        (types: PassContextTypes)
        (key: TypeKey)
        (memberName: string)
        : NominalMember voption =
        tryIntrinsicAbbrevByKey types key |> ValueOption.bind (pickMember memberName)

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
        hostOr
            (tryUnionBare types useSite name)
            (fun () -> hostOr (tryRecord types useSite name) (fun () -> tryIntrinsicAbbrevHostByName types name))

    /// The addresses a type DECLARATION is reachable under: the container-homed nominal claim
    /// `Key`, and the declared bare `Name`, which reaches an intrinsic binding's
    /// namespace-homed canon key through `IntrinsicKeys`. Built by `PassContext` from a single
    /// declared `(name, arity)`.
    [<Struct>]
    type DeclaredTypeAddress = { Key: TypeKey; Name: string }

    /// The address-addressed twin of the above, for what a DECLARATION claims, because two
    /// sibling modules may each declare `T`.
    let tryNonClassMemberHostByDecl (types: PassContextTypes) (decl: DeclaredTypeAddress) : IInterfaceImplHost voption =
        hostOr
            (tryDict types.Union decl.Key)
            (fun () -> hostOr (tryDict types.Record decl.Key) (fun () -> tryIntrinsicAbbrevHostByName types decl.Name))

    /// The declaring union of a registered case: the case carries its union's `TypeKey`, so no
    /// use site is needed, and a caller holding a case got it from a scoped read anyway.
    let unionOfCase (types: PassContextTypes) (info: UnionCaseInfo) : UnionTypeInfo =
        match tryOfKey types.Union (ValueSome info.UnionKey) with
        | ValueSome u -> u
        | ValueNone -> failwithf "Internal error: union case '%s' has no registered union '%s'" info.Name info.UnionName

    // --- The reverse name indexes: fields and union cases -----------------------------
    // A field / case name holds no claim of its own; its OWNER's claim scopes it.

    /// WHERE the union declaring `case` enters the name environment at `useSite`.
    let private caseRankAt (types: PassContextTypes) (useSite: UseSite) (case: UnionCaseInfo) : BindingRank voption =
        keyRankAt types useSite case.UnionName case.UnionKey

    let private caseVisibleAt (types: PassContextTypes) (useSite: UseSite) (case: UnionCaseInfo) : bool =
        (caseRankAt types useSite case).IsSome

    /// The records declaring a field `name` VISIBLE from `useSite`, each with the rank its
    /// declaration enters at: the candidate set a record literal intersects over.
    /// `{ a = 1 }` written above `type R = { a: int }` matches none.
    let rankedRecordsWithField
        (types: PassContextTypes)
        (useSite: UseSite)
        (name: string)
        : struct (BindingRank * RecordTypeInfo)[] =
        match types.FieldIndex.TryGetValue name with
        | true, infos ->
            let hits = ResizeArray infos.Length

            for info in infos do
                match keyRankAt types useSite info.Name info.TypeKey with
                | ValueSome r -> hits.Add(struct (r, info))
                | ValueNone -> ()

            hits.ToArray()
        | false, _ -> Array.empty

    /// The union cases named `name` VISIBLE from `useSite`, each with the rank its declaring
    /// union enters at.
    let rankedCasesNamed
        (types: PassContextTypes)
        (useSite: UseSite)
        (name: string)
        : struct (BindingRank * UnionCaseInfo)[] =
        match types.CtorIndex.TryGetValue name with
        | true, infos ->
            let hits = ResizeArray infos.Length

            for case in infos do
                match caseRankAt types useSite case with
                | ValueSome r -> hits.Add(struct (r, case))
                | ValueNone -> ()

            hits.ToArray()
        | false, _ -> Array.empty

    /// The ctor-vs-bound variable test: does `name` resolve to a union case visible from `useSite`?
    let isCaseName (types: PassContextTypes) (useSite: UseSite) (name: string) : bool =
        match types.CtorIndex.TryGetValue name with
        | true, infos -> infos |> EqArray.exists (caseVisibleAt types useSite)
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
