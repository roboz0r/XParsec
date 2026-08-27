namespace XParsec.FSharp.SemanticAnalysis.Passes

open XParsec.FSharp.SemanticAnalysis
open ExternalTypeProbe

/// Resolution of a written name in F#'s order. The first segment is classified once against
/// the use site's environment; each later segment is looked up inside the entity already
/// found; the order of the lookups inside an entity is fixed by the syntactic position. Both
/// halves of the program answer every lookup, this file's own declarations first: `LocalScope`
/// over the registry, `IExternalSymbolResolver.Scope` over the referenced surfaces.
module NameResolutionLongIdent =

    /// The widest generic arity the NAME route probes for a spelling written without type
    /// args. A name index is unenumerable, so its arity suffix has to be guessed.
    [<Literal>]
    let MaxProbedQualifierArity = 4

    /// The generic arity a written spelling asks for.
    [<RequireQualifiedAccess>]
    type WrittenArity =
        /// Type args were written, so the count is known and a shape of any other arity
        /// declines. `Vesper.Fun` is declared at 2, 3, 4 and 5.
        | Exact of int
        /// A qualifier written without type args. Every arity a container publishes answers,
        /// narrowest first; the name route guesses `0 .. MaxProbedQualifierArity`.
        | Any

    module private WrittenArity =

        /// The arity suffixes the NAME route probes, narrowest first.
        let probes (arity: WrittenArity) : int list =
            match arity with
            | WrittenArity.Exact n -> [ n ]
            | WrittenArity.Any -> [ 0..MaxProbedQualifierArity ]

        let admits (arity: WrittenArity) (shape: ExternalTypeShape) : bool =
            match arity with
            | WrittenArity.Exact n -> shape.TyparArity = n
            | WrittenArity.Any -> true

    // --- The resolver -----------------------------------------------------------------

    [<RequireQualifiedAccess>]
    type Position =
        | Expression
        | Pattern

    /// A resolved item and the index of the first segment left over, which is a member of it.
    [<Struct; NoEquality; NoComparison>]
    type Resolution = { Item: ResolvedItem; Rest: int }

    let private resolved (item: ResolvedItem) (rest: int) : Resolution = { Item = item; Rest = rest }

    let private tryPickV (f: 'T -> 'U voption) (xs: 'T list) : 'U voption =
        let rec go xs =
            match xs with
            | [] -> ValueNone
            | x :: rest ->
                match f x with
                | ValueSome v -> ValueSome v
                | ValueNone -> go rest

        go xs

    let private isUnresolved (r: Resolution) : bool =
        match r.Item with
        | ResolvedItem.Unresolved _ -> true
        | _ -> false

    let private unresolvedInEnv (segment: string) (rest: int) : Resolution =
        resolved
            (ResolvedItem.Unresolved
                {
                    Segment = segment
                    Within = ResolutionScope.Environment
                })
            rest

    let private unresolvedInContainer (c: ModuleContainer) (segment: string) (rest: int) : Resolution =
        resolved
            (ResolvedItem.Unresolved
                {
                    Segment = segment
                    Within = ResolutionScope.Container c
                })
            rest

    let private unresolvedInType (t: ResolvedTypeRef) (segment: string) (rest: int) : Resolution =
        resolved
            (ResolvedItem.Unresolved
                {
                    Segment = segment
                    Within = ResolutionScope.Type t
                })
            rest

    /// The first item any step yields, else the first miss any step yields. A step's
    /// `Unresolved` result is a miss: remembered, while every later step still answers.
    let private tryFirstOf (steps: (unit -> Resolution voption) list) : Resolution voption =
        let mutable hit = ValueNone
        let mutable miss = ValueNone

        for step in steps do
            if hit.IsNone then
                match step () with
                | ValueSome r when isUnresolved r ->
                    if miss.IsNone then
                        miss <- ValueSome r
                | ValueSome r -> hit <- ValueSome r
                | ValueNone -> ()

        match hit with
        | ValueSome r -> ValueSome r
        | ValueNone -> miss

    /// `tryFirstOf`, with `fallback` when no step yields anything.
    let private firstOf (steps: (unit -> Resolution voption) list) (fallback: Resolution) : Resolution =
        match tryFirstOf steps with
        | ValueSome r -> r
        | ValueNone -> fallback

    /// The item a type name at the END of a written name denotes. In expression position a
    /// constructible type stands for its constructor: a class of this file, or a referenced
    /// class at arity 0. A generic referenced class is constructible only through the
    /// enclosing `TypeApp`, which resolves it at its exact arity.
    let private finalTypeItem (position: Position) (t: ResolvedTypeRef) : ResolvedItem =
        match position, t with
        | Position.Pattern, _ -> ResolvedItem.Type t
        | Position.Expression, ResolvedTypeRef.Local claim when claim.Kind = TypeDeclKind.Class -> ResolvedItem.Ctor t
        | Position.Expression, ResolvedTypeRef.External(_, ExternalTypeShape.Class info) when info.TyparArity = 0 ->
            ResolvedItem.Ctor t
        | Position.Expression, _ -> ResolvedItem.Type t

    let private enclosingOf (useSite: UseSite) : ModuleContainer =
        match useSite.Container with
        | ValueSome h -> h
        | ValueNone -> ModuleContainer.InNamespace NamespaceKey.Global

    let private childPath (c: ModuleContainer) (name: string) : string =
        match SymbolKeyOps.containerFullName c with
        | "" -> name
        | full -> full + "." + name

    /// The module `name` declared directly in `c`, in either half.
    let private subContainer (ctx: PassContext) (c: ModuleContainer) (name: string) : ModuleContainer voption =
        match TypeRegistry.tryContainerUnder ctx.Types c name with
        | ValueSome sub -> ValueSome sub
        | ValueNone -> ctx.Resolver.Scope.TryContainer(childPath c name)

    /// Every module or namespace `segment` denotes at `useSite`, nearest first: a module
    /// abbreviation's target, a child of an enclosing scope, a child of an opened scope, a root.
    let private firstSegmentContainers (ctx: PassContext) (useSite: UseSite) (segment: string) : ModuleContainer list =
        let found = ResizeArray<ModuleContainer>()

        let add (c: ModuleContainer voption) =
            match c with
            | ValueSome c when not (found.Contains c) -> found.Add c
            | _ -> ()

        let atPath (path: string) =
            add (LocalScope.tryContainer ctx path)
            add (ctx.Resolver.Scope.TryContainer path)

        match Map.tryFind segment ctx.Resolution.OpenScope.Abbrevs with
        | Some target -> atPath target
        | None ->
            for h in (enclosingOf useSite).SelfAndAncestors do
                add (TypeRegistry.tryContainerUnder ctx.Types h segment)
                add (ctx.Resolver.Scope.TryContainer(childPath h segment))

            for o in useSite.Opens do
                match TypeRegistry.openedContainer ctx.Types o with
                | ValueSome opened -> add (TypeRegistry.tryContainerUnder ctx.Types opened segment)
                | ValueNone -> ()

            for p in ctx.Resolution.OpenScope.Prefixes do
                atPath (p + "." + segment)

            atPath segment

        List.ofSeq found

    /// Every module or namespace the dotted `path` denotes at `useSite`, nearest first. An
    /// empty path denotes the containers a BARE name is read against.
    let private containersAtPath (ctx: PassContext) (useSite: UseSite) (path: string) : ModuleContainer list =
        match path.Length with
        | 0 -> ScopeContents.openedContainers ctx.Resolver.Scope ctx.Resolution.OpenScope.Prefixes
        | _ ->
            let segments = path.Split '.'

            let rec descend (cs: ModuleContainer list) (i: int) =
                if i = segments.Length then
                    cs
                else
                    descend
                        [
                            for c in cs do
                                match subContainer ctx c segments.[i] with
                                | ValueSome sub -> sub
                                | ValueNone -> ()
                        ]
                        (i + 1)

            descend (firstSegmentContainers ctx useSite segments.[0]) 1

    /// The referenced value the written `path`.`name` denotes at the use site: the first
    /// container the path denotes that declares `name`.
    let externalValueInScope
        (ctx: PassContext)
        (useSite: UseSite)
        (path: string)
        (name: string)
        : ExternalSymbol voption =
        ScopeContents.tryValueIn ctx.Resolver.Scope (containersAtPath ctx useSite path) name

    /// Every external union case a BARE `caseName` claims: a case, without
    /// `[<RequireQualifiedAccess>]`, of a union declared directly in an opened container.
    /// Two claims are an ambiguity for the caller to report. A written qualifier
    /// (`Color.Red`) resolves through `inType` instead.
    let externalCasesInScope (ctx: PassContext) (useSite: UseSite) (caseName: string) : EqArray<ExternalUnionCase> =
        EqArray.ofSeq
            [
                for c in containersAtPath ctx useSite "" do
                    for uc in (ctx.Resolver.Scope.UnionCasesNamed(c, caseName)).Underlying do
                        if not uc.IsRequireQualifiedAccess then
                            uc
            ]

    /// A WRITTEN type spelling against the referenced contracts: the types published by the
    /// containers the written path denotes, narrowest arity first, then the arity suffixes
    /// probed against the sources that key a type by NAME alone. The first half is the one
    /// route to a module-held type, whose compiled name `+`-nests where the source dots, and
    /// the one route to an arity wider than `MaxProbedQualifierArity`.
    let tryPickExternalWritten
        (ctx: PassContext)
        (useSite: UseSite)
        (arity: WrittenArity)
        (pick: TypeKey -> ExternalTypeShape -> 'T voption)
        (written: string)
        : 'T voption =
        let dot = written.LastIndexOf '.'
        let path = if dot < 0 then "" else written.Substring(0, dot)
        let name = if dot < 0 then written else written.Substring(dot + 1)

        // Depends on `TypesNamed` answering narrowest arity first.
        let pickAdmitted (c: ModuleContainer) : 'T voption =
            let declared = (ctx.Resolver.Scope.TypesNamed(c, name)).Underlying
            let mutable result = ValueNone
            let mutable i = 0

            while result.IsNone && i < declared.Length do
                let (struct (key, shape)) = declared.[i]

                if WrittenArity.admits arity shape then
                    result <- pick key shape

                i <- i + 1

            result

        let published = containersAtPath ctx useSite path |> tryPickV pickAdmitted

        match published with
        | ValueSome _ as hit -> hit
        | ValueNone ->
            tryPickExternalType
                ctx
                (WrittenArity.probes arity)
                (fun key shape ->
                    if WrittenArity.admits arity shape then
                        pick key shape
                    else
                        ValueNone
                )
                written

    /// `tryPickExternalWritten` unfiltered: the first hit at the arity asked for.
    let private classifyExternalWritten
        (ctx: PassContext)
        (useSite: UseSite)
        (arity: WrittenArity)
        (written: string)
        : struct (TypeKey * ExternalTypeShape) voption =
        tryPickExternalWritten ctx useSite arity (fun key shape -> ValueSome(struct (key, shape))) written

    // --- The contents of one entity -----------------------------------------------------

    let private valueIn
        (ctx: PassContext)
        (useSite: UseSite)
        (c: ModuleContainer)
        (name: string)
        : ResolvedValue voption =
        match LocalScope.tryValue ctx useSite c name with
        | ValueSome m -> ValueSome(ResolvedValue.Local m)
        | ValueNone -> ctx.Resolver.Scope.TryValue(c, name) |> ValueOption.map ResolvedValue.External

    /// Every claim on the case `name` declared directly in `c`. This file's own unions shadow
    /// every referenced one.
    let private casesIn
        (ctx: PassContext)
        (useSite: UseSite)
        (c: ModuleContainer)
        (name: string)
        : ResolvedUnionCase[] =
        match LocalScope.tryUnionCase ctx useSite c name with
        | ValueSome info -> [| ResolvedUnionCase.Local info |]
        | ValueNone ->
            [|
                for uc in (ctx.Resolver.Scope.UnionCasesNamed(c, name)).Underlying -> ResolvedUnionCase.External uc
            |]

    let private typesIn
        (ctx: PassContext)
        (useSite: UseSite)
        (c: ModuleContainer)
        (name: string)
        : ResolvedTypeRef list =
        match LocalScope.typesNamed ctx useSite c name with
        | [] ->
            [
                for struct (key, shape) in (ctx.Resolver.Scope.TypesNamed(c, name)).Underlying ->
                    ResolvedTypeRef.External(key, shape)
            ]
        | claims -> List.map ResolvedTypeRef.Local claims

    /// A union-case claim beside its declaring union's `[<RequireQualifiedAccess>]`.
    [<Struct; NoEquality; NoComparison>]
    type private CaseClaim =
        {
            Case: ResolvedUnionCase
            RequiresQualifiedAccess: bool
        }

    module private CaseClaim =

        let read (ctx: PassContext) (case: ResolvedUnionCase) : CaseClaim =
            let rqa =
                match case with
                | ResolvedUnionCase.Local info -> (TypeRegistry.unionOfCase ctx.Types info).IsRequireQualifiedAccess
                | ResolvedUnionCase.External uc -> uc.IsRequireQualifiedAccess

            {
                Case = case
                RequiresQualifiedAccess = rqa
            }

        /// A claim the caller has already filtered to a union without
        /// `[<RequireQualifiedAccess>]`.
        let plain (case: ResolvedUnionCase) : CaseClaim =
            {
                Case = case
                RequiresQualifiedAccess = false
            }

    /// One claim resolves, carrying its declaring union's `[<RequireQualifiedAccess>]` for
    /// the caller to report; several are ambiguous.
    let private caseAmong (name: string) (claims: CaseClaim[]) : ResolvedItem voption =
        match claims.Length with
        | 0 -> ValueNone
        | 1 ->
            let claim = claims.[0]
            ValueSome(ResolvedItem.UnionCase(claim.Case, claim.RequiresQualifiedAccess))
        | _ -> ValueSome(ResolvedItem.AmbiguousCase(name, claims |> Array.map (fun c -> c.Case)))

    /// A static member `name`, or the setter of a write-only property `name`, is declared.
    let private declaresStatic (members: TypeMemberInfo[]) (name: string) : bool =
        let setter = AccessorNames.setterName name

        members
        |> Array.exists (fun m -> m.IsStatic && (m.Name = name || m.Name = setter))

    /// `declaresStatic` over a referenced type's published members. An instance member does
    /// NOT answer: F# rejects `T.InstanceMember` with FS3214.
    let private declaresExternalStatic (ctx: PassContext) (key: TypeKey) (name: string) : bool =
        let anyStatic (n: string) =
            ctx.Provider.TryLookupMembers(key, n) |> EqArray.exists (fun m -> m.IsStatic)

        anyStatic name || anyStatic (AccessorNames.setterName name)

    /// `name` inside the type `t`: a union or enum case in either position; in expression
    /// position also a static member, which must be declared on the type. An abbreviation, an
    /// intrinsic repr and an unmodelled type admit a static unchecked, for Unification to
    /// resolve.
    let private inType
        (ctx: PassContext)
        (position: Position)
        (t: ResolvedTypeRef)
        (name: string)
        : ResolvedItem voption =
        let staticMember () =
            match position with
            | Position.Expression -> ValueSome(ResolvedItem.StaticMember(t, name))
            | Position.Pattern -> ValueNone

        let staticIf (declared: bool) =
            if declared then staticMember () else ValueNone

        match t with
        | ResolvedTypeRef.Local claim ->
            match claim.Kind with
            | TypeDeclKind.Union ->
                match TypeRegistry.tryUnionByKey ctx.Types claim.Key with
                | ValueSome u ->
                    match u.Cases |> Array.tryFind (fun c -> c.Name = name) with
                    | Some info -> ValueSome(ResolvedItem.UnionCase(ResolvedUnionCase.Local info, false))
                    | None -> staticIf (declaresStatic u.Members name)
                | ValueNone -> ValueNone
            | TypeDeclKind.Enum ->
                match TypeRegistry.tryEnumByKey ctx.Types claim.Key with
                | ValueSome e when e.HasCase name -> ValueSome(ResolvedItem.EnumCase(t, name))
                // `MyEnum.Nope` is FS0039. A static inherited from `System.Enum` would be
                // the only other answer, and the local type registry carries none.
                | _ -> ValueNone
            | TypeDeclKind.Class
            | TypeDeclKind.Record ->
                match TypeRegistry.tryNominalByKey ctx.Types claim.Key with
                | ValueSome decl -> staticIf (declaresStatic decl.Members name)
                | ValueNone -> ValueNone
            | TypeDeclKind.Abbreviation
            | TypeDeclKind.IntrinsicRepr -> staticMember ()
        | ResolvedTypeRef.External(key, shape) ->
            match shape with
            | ExternalTypeShape.Union(cases = cases; requiresQualifiedAccess = rqa) ->
                match EqArray.tryFind (fun (c: ExternalCaseShape) -> c.Name = name) cases with
                | ValueSome case ->
                    let uc: ExternalUnionCase =
                        {
                            UnionKey = key
                            Case = case
                            IsRequireQualifiedAccess = rqa
                        }

                    ValueSome(ResolvedItem.UnionCase(ResolvedUnionCase.External uc, false))
                | ValueNone -> staticIf (declaresExternalStatic ctx key name)
            | ExternalTypeShape.Enum(cases = cases) ->
                if cases |> EqArray.exists (fun c -> c.Name = name) then
                    ValueSome(ResolvedItem.EnumCase(t, name))
                else
                    // `E.Equals` reaches a static inherited from `System.Enum`, which the
                    // member table carries.
                    staticIf (declaresExternalStatic ctx key name)
            | ExternalTypeShape.Record _
            | ExternalTypeShape.Class _
            | ExternalTypeShape.IntrinsicInterface _ -> staticIf (declaresExternalStatic ctx key name)
            | ExternalTypeShape.Intrinsic _
            | ExternalTypeShape.Abbrev _
            | ExternalTypeShape.Unmodelled _ -> staticMember ()

    /// `names.[i..]` inside the module or namespace `c`. Expression position: value, case of a
    /// union without `[<RequireQualifiedAccess>]`, type, sub-module, then the case with it.
    /// Pattern position: case, value, type, sub-module. The first item wins; on a total miss
    /// the deepest miss is the answer.
    let rec private inContainer
        (ctx: PassContext)
        (useSite: UseSite)
        (position: Position)
        (c: ModuleContainer)
        (names: string[])
        (i: int)
        : Resolution =
        let name = names.[i]
        let next = i + 1
        let atEnd = next = names.Length

        let value () =
            valueIn ctx useSite c name
            |> ValueOption.map (fun v -> resolved (ResolvedItem.Value v) next)

        let caseWhere (admit: bool -> bool) () =
            casesIn ctx useSite c name
            |> Array.map (CaseClaim.read ctx)
            |> Array.filter (fun claim -> admit claim.RequiresQualifiedAccess)
            |> caseAmong name
            |> ValueOption.map (fun item -> resolved item next)

        // The case of a union without `[<RequireQualifiedAccess>]`.
        let plainCase = caseWhere not
        // The RQA case, expression position's last resort: it resolves, then reports FS0035.
        let rqaCase = caseWhere id
        // Pattern position admits both; the flag rides the item.
        let anyCase = caseWhere (fun _ -> true)

        let types () =
            match typesIn ctx useSite c name with
            | [] -> ValueNone
            | t :: _ when atEnd -> ValueSome(resolved (finalTypeItem position t) next)
            | (t :: _) as claims ->
                match claims |> tryPickV (fun t -> inType ctx position t names.[next]) with
                | ValueSome item -> ValueSome(resolved item (next + 1))
                // A type answered for the name but not the member: that is the miss.
                | ValueNone -> ValueSome(unresolvedInType t names.[next] (next + 1))

        let sub () =
            match subContainer ctx c name with
            | ValueSome s when atEnd -> ValueSome(resolved (ResolvedItem.ModuleOrNamespace s) next)
            | ValueSome s -> ValueSome(inContainer ctx useSite position s names next)
            | ValueNone -> ValueNone

        let steps =
            match position with
            | Position.Expression -> [ value; plainCase; types; sub; rqaCase ]
            | Position.Pattern -> [ anyCase; value; types; sub ]

        firstOf steps (unresolvedInContainer c name next)

    // --- The environment: what a bare first segment denotes -----------------------------

    /// A module-level value: one of this file's OPENED scopes, then the referenced surfaces
    /// through the `open`s and the prelude. The enclosing scopes' values are bound by the
    /// walk itself, in declaration order, so a `let` is in scope below its own body only.
    let private valueInEnv (ctx: PassContext) (useSite: UseSite) (name: string) : ResolvedValue voption =
        let local =
            useSite.Opens
            |> tryPickV (fun o ->
                match TypeRegistry.openedContainer ctx.Types o with
                | ValueSome opened -> LocalScope.tryValue ctx useSite opened name
                | ValueNone -> ValueNone
            )

        match local with
        | ValueSome m -> ValueSome(ResolvedValue.Local m)
        | ValueNone ->
            externalValueInScope ctx useSite "" name
            |> ValueOption.map ResolvedValue.External

    /// A bare union case: a case of a union without `[<RequireQualifiedAccess>]` visible at
    /// the use site. This file's own unions shadow every referenced one.
    let private caseInEnv (ctx: PassContext) (useSite: UseSite) (name: string) : ResolvedItem voption =
        let locals =
            TypeRegistry.casesNamed ctx.Types useSite name
            |> Array.filter (fun c -> not (TypeRegistry.unionOfCase ctx.Types c).IsRequireQualifiedAccess)

        // Both halves are filtered to unions without `[<RequireQualifiedAccess>]`.
        match locals with
        | [||] ->
            externalCasesInScope ctx useSite name
            |> EqArray.toArray
            |> Array.map (ResolvedUnionCase.External >> CaseClaim.plain)
            |> caseAmong name
        | _ ->
            locals
            |> Array.map (ResolvedUnionCase.Local >> CaseClaim.plain)
            |> caseAmong name

    /// A bare type name at any arity: this file's claim in scope, else the referenced contracts.
    let private typeInEnv (ctx: PassContext) (useSite: UseSite) (name: string) : ResolvedTypeRef voption =
        match TypeRegistry.tryTypeClaimAnyArity ctx.Types useSite name with
        | ValueSome claim -> ValueSome(ResolvedTypeRef.Local claim)
        | ValueNone ->
            classifyExternalWritten ctx useSite WrittenArity.Any name
            |> ValueOption.map (fun (struct (key, shape)) -> ResolvedTypeRef.External(key, shape))

    /// `names.[0]` as a type in the environment and `names.[1]` inside it: every claim of this
    /// file in scope under the name, then the referenced contracts at each arity. A type that
    /// answers for the name but not for the member is the miss returned.
    let private typeFirst
        (ctx: PassContext)
        (useSite: UseSite)
        (position: Position)
        (names: string[])
        : Resolution voption =
        let second = names.[1]

        let local =
            TypeRegistry.writtenTypeClaims ctx.Types useSite (WrittenTypeName.bare names.[0])
            |> List.map ResolvedTypeRef.Local

        let hit =
            match local |> tryPickV (fun t -> inType ctx position t second) with
            | ValueSome item -> ValueSome item
            | ValueNone ->
                tryPickExternalWritten
                    ctx
                    useSite
                    WrittenArity.Any
                    (fun key shape -> inType ctx position (ResolvedTypeRef.External(key, shape)) second)
                    names.[0]

        match hit with
        | ValueSome item -> ValueSome(resolved item 2)
        | ValueNone ->
            let found =
                match local with
                | t :: _ -> ValueSome t
                | [] ->
                    classifyExternalWritten ctx useSite WrittenArity.Any names.[0]
                    |> ValueOption.map (fun (struct (key, shape)) -> ResolvedTypeRef.External(key, shape))

            found |> ValueOption.map (fun t -> unresolvedInType t second 2)

    /// The referenced contracts probed by the folded spelling, a whole name at a time: the
    /// whole name as a type, then the prefix as a type and the last segment inside it.
    let private folded
        (ctx: PassContext)
        (useSite: UseSite)
        (position: Position)
        (names: string[])
        : Resolution voption =
        let n = names.Length
        let whole = String.concat "." names
        let prefix = String.concat "." names.[.. n - 2]
        let last = names.[n - 1]

        let asType () =
            match position with
            | Position.Expression ->
                classifyExternalWritten ctx useSite WrittenArity.Any whole
                |> ValueOption.map (fun (struct (key, shape)) ->
                    resolved (finalTypeItem position (ResolvedTypeRef.External(key, shape))) n
                )
            | Position.Pattern -> ValueNone

        let prefixMember () =
            tryPickExternalWritten
                ctx
                useSite
                WrittenArity.Any
                (fun key shape -> inType ctx position (ResolvedTypeRef.External(key, shape)) last)
                prefix
            |> ValueOption.map (fun item -> resolved item n)

        let prefixMiss () =
            classifyExternalWritten ctx useSite WrittenArity.Any prefix
            |> ValueOption.map (fun (struct (key, shape)) ->
                unresolvedInType (ResolvedTypeRef.External(key, shape)) last n
            )

        [ asType; prefixMember; prefixMiss ] |> tryPickV (fun step -> step ())

    /// The module path from `names.[0]`, each candidate scope tried in turn; the first item
    /// wins, else the first miss.
    let private modulePath
        (ctx: PassContext)
        (useSite: UseSite)
        (position: Position)
        (names: string[])
        : Resolution voption =
        tryFirstOf
            [
                for c in firstSegmentContainers ctx useSite names.[0] ->
                    fun () -> ValueSome(inContainer ctx useSite position c names 1)
            ]

    /// The readings of a several-segment name, in the order `position` tries them.
    let private qualifiedReadings
        (ctx: PassContext)
        (useSite: UseSite)
        (position: Position)
        (names: string[])
        : (unit -> Resolution voption) list =
        let viaModulePath () = modulePath ctx useSite position names
        let viaTypeFirst () = typeFirst ctx useSite position names
        let viaFolded () = folded ctx useSite position names

        match position with
        | Position.Expression -> [ viaModulePath; viaTypeFirst; viaFolded ]
        | Position.Pattern -> [ viaTypeFirst; viaModulePath; viaFolded ]

    /// A name in expression position whose first segment is not a lexically bound variable.
    /// Single segment: value, case, type. Several: `qualifiedReadings`.
    let resolveExpr (ctx: PassContext) (useSite: UseSite) (names: string[]) : Resolution =
        let first = names.[0]

        match valueInEnv ctx useSite first with
        | ValueSome v -> resolved (ResolvedItem.Value v) 1
        | ValueNone ->
            match names.Length with
            | 1 ->
                match caseInEnv ctx useSite first with
                | ValueSome item -> resolved item 1
                | ValueNone ->
                    match typeInEnv ctx useSite first with
                    | ValueSome t -> resolved (finalTypeItem Position.Expression t) 1
                    | ValueNone -> unresolvedInEnv first 1
            | n -> firstOf (qualifiedReadings ctx useSite Position.Expression names) (unresolvedInEnv first n)

    /// A name in pattern position. Single segment: a case visible at the use site, else a
    /// bound variable, which is the `Unresolved` answer. Several: `qualifiedReadings`.
    let resolvePattern (ctx: PassContext) (useSite: UseSite) (names: string[]) : Resolution =
        let first = names.[0]

        match names.Length with
        | 1 ->
            match caseInEnv ctx useSite first with
            | ValueSome item -> resolved item 1
            | ValueNone -> unresolvedInEnv first 1
        | n -> firstOf (qualifiedReadings ctx useSite Position.Pattern names) (unresolvedInEnv first n)

    /// A written type name at `arity`: a claim of this file in scope at the use site, at that
    /// arity else at any, then the referenced contracts at exactly that arity.
    let resolveType (ctx: PassContext) (useSite: UseSite) (written: WrittenTypeName) (arity: int) : ResolvedItem =
        let local =
            match TypeRegistry.tryWrittenTypeClaim ctx.Types useSite written arity with
            | ValueSome claim -> ValueSome claim
            | ValueNone -> TypeRegistry.tryWrittenTypeClaimAnyArity ctx.Types useSite written

        match local with
        | ValueSome claim -> ResolvedItem.Type(ResolvedTypeRef.Local claim)
        | ValueNone ->
            match
                tryPickExternalWritten
                    ctx
                    useSite
                    (WrittenArity.Exact arity)
                    (fun key shape -> ValueSome(ResolvedTypeRef.External(key, shape)))
                    written.Written
            with
            | ValueSome t -> ResolvedItem.Type t
            | ValueNone ->
                ResolvedItem.Unresolved
                    {
                        Segment = written.Name
                        Within = ResolutionScope.Environment
                    }
