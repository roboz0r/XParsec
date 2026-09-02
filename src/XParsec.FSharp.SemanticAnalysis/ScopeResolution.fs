namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic

/// A module or namespace THIS FILE declares.
[<Struct>]
type LocalContainer =
    {
        Container: ModuleContainer
        /// Where the name enters the environment of the declaring scope: a module's `module`
        /// keyword, hoisted to the enclosing `rec` scope's keyword where there is one, and
        /// `BindingRank.unpositioned` for a namespace.
        VisibleFrom: int
    }

/// The module / namespace scopes this file DECLARES, keyed by the dotted SOURCE path an `open`
/// writes (`"N"`, `"N.A"`).
type LocalContainers = Dictionary<string, LocalContainer>

/// Resolution of a written module path: what an `open`, a module abbreviation and a qualified
/// name reach, this file's own declarations first, then the referenced surfaces.
module ScopeResolution =

    /// The module or namespace `name` THIS FILE declares directly in `enclosing`, with where
    /// its name enters the environment.
    let tryLocalSubContainer
        (containers: LocalContainers)
        (enclosing: ModuleContainer)
        (name: string)
        : LocalContainer voption =
        let qualified = SymbolKeyOps.qualify (SymbolKeyOps.containerFullName enclosing) name

        match containers.TryGetValue qualified with
        | true, lc -> ValueSome lc
        | false, _ -> ValueNone

    /// The scope the dotted SOURCE `path` reaches when written INSIDE `enclosing` at offset
    /// `at`, and `enclosing` itself for an empty path. An EXACT descent, no walking outward.
    /// A module of this file declared below `at` is out of scope there.
    let tryContainerUnder
        (containers: LocalContainers)
        (enclosing: ModuleContainer)
        (path: string)
        (at: int)
        : ModuleContainer voption =
        if path.Length = 0 then
            ValueSome enclosing
        else
            let qualified = SymbolKeyOps.qualify (SymbolKeyOps.containerFullName enclosing) path

            match containers.TryGetValue qualified with
            | true, lc when lc.VisibleFrom <= at -> ValueSome lc.Container
            | _ -> ValueNone

    /// The scope the dotted SOURCE `path` denotes directly under `c` at offset `at`, this
    /// file's own declarations first, then the referenced surfaces.
    let private tryDescend
        (containers: LocalContainers)
        (scope: IScopeContents)
        (c: ModuleContainer)
        (path: string)
        (at: int)
        : ModuleContainer voption =
        match tryContainerUnder containers c path at with
        | ValueSome sub -> ValueSome sub
        | ValueNone -> scope.TryContainer(SymbolKeyOps.qualify (SymbolKeyOps.containerFullName c) path)

    /// The scope the dotted SOURCE `path` denotes as written at offset `at` inside the scope
    /// whose own source path is `under`: `under.path` first, then ever-shorter prefixes, then
    /// the root. Each candidate reads this file's own declarations first, then the referenced
    /// surfaces.
    let private tryContainerOfPath
        (containers: LocalContainers)
        (scope: IScopeContents)
        (under: string)
        (path: string)
        (at: int)
        : ModuleContainer voption =
        let rec go (under: string) =
            let qualified = SymbolKeyOps.qualify under path

            match containers.TryGetValue qualified with
            | true, lc when lc.VisibleFrom <= at -> ValueSome lc.Container
            | _ ->
                match scope.TryContainer qualified with
                | ValueSome h -> ValueSome h
                | ValueNone ->
                    if under.Length = 0 then
                        ValueNone
                    else
                        let cut = under.LastIndexOf '.'
                        go (if cut < 0 then "" else under.Substring(0, cut))

        go under

    /// The scope a dotted SOURCE `path` written at offset `at` in the scope `under` denotes,
    /// read under each of the `outer` scopes in turn and then through `under`'s own enclosing
    /// chain: the search an `open` and a module abbreviation share.
    let private tryReachFrom
        (containers: LocalContainers)
        (scope: IScopeContents)
        (outer: ScopeEntry list)
        (under: string)
        (path: string)
        (at: int)
        : ModuleContainer voption =
        let rec go (entries: ScopeEntry list) =
            match entries with
            | [] -> tryContainerOfPath containers scope under path at
            | e :: more ->
                match tryDescend containers scope e.Container path at with
                | ValueSome sub -> ValueSome sub
                | ValueNone -> go more

        go outer

    /// The environment one element's written `open`s and module abbreviations build.
    [<NoComparison>]
    type ScopeEnv =
        {
            /// Each `open` resolved to the scope it denotes, innermost-first, stamped with
            /// the rank it enters at. An unresolvable `open` is dropped.
            Opens: ScopeEntry list
            /// Alias → the module it binds and where the alias enters the name environment.
            /// A refused abbreviation binds nothing.
            Aliases: Map<string, ScopeEntry>
        }

    module ScopeEnv =
        let empty: ScopeEnv = { Opens = []; Aliases = Map.empty }

    /// The scope the written `path`, at offset `at` in the scope `under`, denotes in `env`: the
    /// aliases in it, then the `open`s in it, then `under`'s own enclosing chain. The search an
    /// `open` and a module abbreviation share, so an alias anchors either one.
    let resolveInEnv
        (containers: LocalContainers)
        (scope: IScopeContents)
        (env: ScopeEnv)
        (under: string)
        (path: string)
        (at: int)
        : ModuleContainer voption =
        let dot = path.IndexOf '.'

        let viaAlias =
            let anchor = if dot < 0 then path else path.Substring(0, dot)

            match Map.tryFind anchor env.Aliases with
            | Some e ->
                let rest = if dot < 0 then "" else path.Substring(dot + 1)
                tryDescend containers scope e.Container rest at
            | None -> ValueNone

        match viaAlias with
        | ValueSome c -> ValueSome c
        | ValueNone -> tryReachFrom containers scope env.Opens under path at

    /// Where a module abbreviation's written target lands. An abbreviation binds a MODULE, so a
    /// namespace target and a target that reaches nothing are both refused at the declaration.
    [<RequireQualifiedAccess; Struct; NoComparison>]
    type AbbrevTarget =
        | Module of container: ModuleContainer
        | Namespace
        | Unresolved

    /// Where `a`'s written target lands, read from `env`.
    let resolveAbbrevTarget
        (containers: LocalContainers)
        (scope: IScopeContents)
        (env: ScopeEnv)
        (a: LocalAbbrev)
        : AbbrevTarget =
        match resolveInEnv containers scope env a.Scope a.Path a.Offset with
        | ValueNone -> AbbrevTarget.Unresolved
        | ValueSome(ModuleContainer.InNamespace _) -> AbbrevTarget.Namespace
        | ValueSome c -> AbbrevTarget.Module c

    /// The environment `decls` builds, each declaration resolved oldest-first against the
    /// environment above it: an `open` and an abbreviation's target alike read the aliases and
    /// the `open`s above them before the writing scope's own enclosing chain.
    let resolveScopeDecls
        (containers: LocalContainers)
        (scope: IScopeContents)
        (decls: LocalScopeDecl list)
        : ScopeEnv =
        let mutable env = ScopeEnv.empty

        for d in List.rev decls do
            match d with
            | LocalScopeDecl.Open o ->
                match resolveInEnv containers scope env o.Scope o.Path o.Offset with
                | ValueSome c ->
                    env <-
                        { env with
                            Opens =
                                {
                                    Container = c
                                    Route =
                                        ScopeRoute.Opened
                                            {
                                                Depth = o.ScopeDepth
                                                Offset = o.Offset
                                            }
                                }
                                :: env.Opens
                        }
                | ValueNone -> ()
            | LocalScopeDecl.Abbrev a ->
                match resolveAbbrevTarget containers scope env a with
                | AbbrevTarget.Module c ->
                    env <-
                        { env with
                            Aliases =
                                Map.add
                                    a.Alias
                                    {
                                        Container = c
                                        Route =
                                            ScopeRoute.Opened
                                                {
                                                    Depth = a.ScopeDepth
                                                    Offset = a.Offset
                                                }
                                    }
                                    env.Aliases
                        }
                | AbbrevTarget.Namespace
                | AbbrevTarget.Unresolved -> ()

        env
