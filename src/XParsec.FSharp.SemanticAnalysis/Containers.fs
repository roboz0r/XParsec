namespace XParsec.FSharp.SemanticAnalysis

/// The modules and namespaces a written spelling's qualifier denotes at a use site, and the
/// reads over them. Locally declared containers (`LocalScope` over the registry) and
/// referenced ones (`IExternalSymbolResolver.Scope`) are both in the result.
module NameResolutionContainers =

    /// The generic arity a written spelling carries.
    [<RequireQualifiedAccess>]
    type WrittenArity =
        /// Type args were written: only a shape of this arity is admitted. `Vesper.Fun` is
        /// declared at 2, 3, 4 and 5.
        | Exact of int<sigSlot>
        /// Written without type args: every arity a container publishes is admitted,
        /// narrowest first.
        | Any

    [<RequireQualifiedAccess>]
    module WrittenArity =

        let admits (arity: WrittenArity) (shape: ExternalTypeShape) : bool =
            match arity with
            | WrittenArity.Exact n -> shape.TyparArity = n
            | WrittenArity.Any -> true

    /// The segments a written spelling carries before its short name.
    [<RequireQualifiedAccess>]
    type Qualifier =
        /// No qualifier: the short name is read against the opened containers.
        | Bare
        /// The qualifier's segments, at least one, in written order: the short name is read
        /// against the containers they denote.
        | Path of string[]

    [<RequireQualifiedAccess>]
    module Qualifier =

        /// A stored dotted rendering whose empty form denotes no qualifier
        /// (`WrittenTypeName.Path`).
        let ofPath (path: string) : Qualifier =
            match path.Length with
            | 0 -> Qualifier.Bare
            | _ -> Qualifier.Path(path.Split '.')

        /// Written segments; empty denotes no qualifier.
        let ofSegments (segments: string[]) : Qualifier =
            match segments.Length with
            | 0 -> Qualifier.Bare
            | _ -> Qualifier.Path segments

    let private childPath (c: ModuleContainer) (name: string) : string =
        match SymbolKeyOps.containerFullName c with
        | "" -> name
        | full -> full + "." + name

    /// The module or namespace `name` declared directly in `c` at offset `at`: a module of
    /// this file declared above `at`, with where its name enters the environment, else a
    /// referenced container. A module of this file declared below `at` is out of scope there,
    /// and the referenced surface supplies the name instead.
    let private localOrReferenced
        (ctx: PassContext)
        (at: int)
        (c: ModuleContainer)
        (name: string)
        : struct (int voption * ModuleContainer) voption =
        match ScopeResolution.tryLocalSubContainer ctx.Types.LocalContainers c name with
        | ValueSome local when local.VisibleFrom <= at ->
            ValueSome(struct (ValueSome local.VisibleFrom, local.Container))
        | _ ->
            match ctx.Resolver.Scope.TryContainer(childPath c name) with
            | ValueSome ext -> ValueSome(struct (ValueNone, ext))
            | ValueNone -> ValueNone

    /// The module or namespace `name` declared directly in `c`, local or referenced. A module
    /// of this file declared below `useSite` is out of scope there.
    let subContainer
        (ctx: PassContext)
        (useSite: UseSite)
        (c: ModuleContainer)
        (name: string)
        : ModuleContainer voption =
        match localOrReferenced ctx useSite.Offset c name with
        | ValueSome(struct (_, sub)) -> ValueSome sub
        | ValueNone -> ValueNone

    /// The rank the module or namespace `name` takes when reached through `entry` at `useSite`:
    /// a module of this file enters at its own declaration, everything else where `entry` does.
    let private rankedSubContainer
        (ctx: PassContext)
        (useSite: UseSite)
        (entry: ScopeEntry)
        (name: string)
        : struct (BindingRank * ModuleContainer) voption =
        match localOrReferenced ctx useSite.Offset entry.Container name with
        | ValueSome(struct (ValueSome visibleFrom, sub)) -> ValueSome(struct (ScopeEntry.rankOf entry visibleFrom, sub))
        | ValueSome(struct (ValueNone, sub)) -> ValueSome(struct (ScopeEntry.rank entry, sub))
        | ValueNone -> ValueNone

    /// Every module or namespace `segment` denotes at `useSite`, best rank first. A module
    /// abbreviation binding the segment is one more candidate, entering where the abbreviation
    /// is written, so a real module declared below it reclaims the name below itself.
    let firstSegmentContainers (ctx: PassContext) (useSite: UseSite) (segment: string) : ModuleContainer list =
        let found = ResizeArray<struct (BindingRank * ModuleContainer)>()

        match Map.tryFind segment ctx.Resolution.Env.Aliases with
        | Some alias -> found.Add(struct (ScopeEntry.rank alias, alias.Container))
        | None -> ()

        for e in useSite.Scopes do
            match rankedSubContainer ctx useSite e segment with
            | ValueSome hit -> found.Add hit
            | ValueNone -> ()

        found
        |> Seq.sortByDescending (fun (struct (r, _)) -> r)
        |> Seq.map (fun (struct (_, c)) -> c)
        |> Seq.distinct
        |> List.ofSeq

    /// Every container to read a short name against at `useSite`, nearest first: for a bare
    /// spelling the scopes in force there, for a dotted `qualifier` the modules and namespaces
    /// its segments denote.
    let containersOf (ctx: PassContext) (useSite: UseSite) (qualifier: Qualifier) : ModuleContainer list =
        match qualifier with
        | Qualifier.Bare -> [ for e in useSite.Scopes -> e.Container ]
        | Qualifier.Path segments ->
            let rec descend (cs: ModuleContainer list) (i: int) =
                if i = segments.Length then
                    cs
                else
                    descend
                        [
                            for c in cs do
                                match subContainer ctx useSite c segments.[i] with
                                | ValueSome sub -> sub
                                | ValueNone -> ()
                        ]
                        (i + 1)

            descend (firstSegmentContainers ctx useSite segments.[0]) 1

    /// A WRITTEN type spelling against the referenced contracts: the types published by the
    /// containers the qualifier denotes, narrowest admitted arity first. Reaches a type
    /// declared in a module, whose compiled name `+`-nests where the source dots.
    let tryPickExternalWritten
        (ctx: PassContext)
        (useSite: UseSite)
        (arity: WrittenArity)
        (pick: TypeKey -> ExternalTypeShape -> 'T voption)
        (qualifier: Qualifier)
        (name: string)
        : 'T voption =
        ScopeContents.tryPickTypeIn
            ctx.Resolver.Scope
            (containersOf ctx useSite qualifier)
            (fun key shape ->
                if WrittenArity.admits arity shape then
                    pick key shape
                else
                    ValueNone
            )
            name
