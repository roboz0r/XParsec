namespace XParsec.FSharp.SemanticAnalysis

/// The contents of one of THIS file's own modules or namespaces, as seen from a use site:
/// the local half of the queries `IScopeContents` provides over a published surface. Every
/// result honours file order, so a declaration below the use site is absent.
[<RequireQualifiedAccess>]
module LocalScope =

    /// The claim under which `key` was registered, if this file declares it.
    let private claimOf (ctx: PassContext) (key: TypeKey) : TypeIdentity voption =
        match ctx.Types.TypeClaims.TryGetValue key.Name with
        | true, claims ->
            let mutable found = ValueNone
            let mutable i = 0

            while found.IsNone && i < claims.Count do
                if claims.[i].Key = key then
                    found <- ValueSome claims.[i]

                i <- i + 1

            found
        | false, _ -> ValueNone

    let private declaredBefore (useSite: UseSite) (claim: TypeIdentity) : bool = claim.VisibleFrom <= useSite.Offset

    /// The module or namespace this file declares under the dotted SOURCE path.
    let tryContainer (ctx: PassContext) (sourcePath: string) : ModuleContainer voption =
        match ctx.Types.LocalContainers.TryGetValue sourcePath with
        | true, c -> ValueSome c.Container
        | false, _ -> ValueNone

    /// The `let` binding `name` declared directly in `container`, above `useSite` and not a
    /// binding of the `let` group whose own RHS the walk stands in (`PendingBindings`).
    let tryValue
        (ctx: PassContext)
        (useSite: UseSite)
        (container: ModuleContainer)
        (name: string)
        : LocalModuleMember voption =
        match ctx.Resolution.LocalModuleMembers.TryGetValue container with
        | true, members ->
            match members.TryGetValue name with
            | true, m when
                m.VisibleFrom <= useSite.Offset
                && not (ctx.Resolution.PendingBindings.Contains m.BindingSite)
                ->
                ValueSome m
            | _ -> ValueNone
        | false, _ -> ValueNone

    /// The union case `name` of a union declared directly in `container`, above `useSite`.
    let tryUnionCase
        (ctx: PassContext)
        (useSite: UseSite)
        (container: ModuleContainer)
        (name: string)
        : UnionCaseInfo voption =
        match ctx.Types.CtorIndex.TryGetValue name with
        | true, cases ->
            cases
            |> EqArray.tryFind (fun c ->
                SymbolKeyOps.tryModuleContainerOf c.UnionKey.Container = ValueSome container
                && (
                    match claimOf ctx c.UnionKey with
                    | ValueSome claim -> declaredBefore useSite claim
                    | ValueNone -> false
                )
            )
        | false, _ -> ValueNone

    /// Every type named `name` declared directly in `container` above `useSite`, one per
    /// generic arity.
    let typesNamed
        (ctx: PassContext)
        (useSite: UseSite)
        (container: ModuleContainer)
        (name: string)
        : TypeIdentity list =
        match ctx.Types.TypeClaims.TryGetValue name with
        | true, claims ->
            [
                for c in claims do
                    if c.Container = container && declaredBefore useSite c then
                        c
            ]
        | false, _ -> []
