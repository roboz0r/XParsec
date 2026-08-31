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
        | Exact of int
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

    /// The module or namespace `name` declared directly in `c`, local or referenced.
    let subContainer (ctx: PassContext) (c: ModuleContainer) (name: string) : ModuleContainer voption =
        match TypeRegistry.tryContainerUnder ctx.Types c name with
        | ValueSome sub -> ValueSome sub
        | ValueNone -> ctx.Resolver.Scope.TryContainer(childPath c name)

    /// Every module or namespace `segment` denotes at `useSite`, nearest first. A module
    /// abbreviation binds the segment outright: its target is the only result.
    let firstSegmentContainers (ctx: PassContext) (useSite: UseSite) (segment: string) : ModuleContainer list =
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
            for e in useSite.Scopes do
                add (subContainer ctx e.Container segment)

            atPath segment

        List.ofSeq found

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
                                match subContainer ctx c segments.[i] with
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
