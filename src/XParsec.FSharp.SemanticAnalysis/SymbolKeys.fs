namespace XParsec.FSharp.SemanticAnalysis

[<Measure>]
type tyVarId

/// A metavar id, dense and monotone within one file, so parallel arrays and side-tables key
/// by it directly.
type TyVarId = int<tyVarId>

/// Where a symbol PHYSICALLY lives, and never part of a key: nominal identity is the
/// containment chain + namespace + name. `Unstamped` is "no home": the compilation being
/// analysed, or a contract scrape.
[<RequireQualifiedAccess>]
type SymbolHome =
    | Unstamped
    | InAssembly of asm: AssemblyName
    /// `InAssembly` REFINED to the declaring source file. The path carries its own
    /// `Assembly`, so the assembly is read off the file, not supplied a second time.
    | InFile of file: AssemblyFilePath

    member this.AssemblyOption: AssemblyName voption =
        match this with
        | SymbolHome.Unstamped -> ValueNone
        | SymbolHome.InAssembly a -> ValueSome a
        | SymbolHome.InFile f -> f.Assembly

    /// `ValueNone` wherever the producer knew only the assembly: a contract view or
    /// a metadata scrape.
    member this.DeclaringFile: AssemblyFilePath voption =
        match this with
        | SymbolHome.Unstamped
        | SymbolHome.InAssembly _ -> ValueNone
        | SymbolHome.InFile f -> ValueSome f

/// A namespace, the root container. `Path` is SEGMENTED (`["System"; "Collections"]`), so
/// prefix relations are segment-list tests. The EMPTY path IS the global namespace.
type NamespaceKey =
    {
        Path: EqArray<string>
    }

    /// The dotted rendering (`"System.Collections"`; `""` for the global namespace), a
    /// BOUNDARY projection only. Identity comparisons use the segmented `Path`.
    member this.Dotted: string = System.String.Join(".", this.Path.Underlying)

    static member Global = { Path = EqArray.empty }

/// Also what holds a `BindingKey`, where `InNamespace` means the binding has NO declaring
/// module: a TOP-LEVEL `let`, or a flat package's export. No CLR type corresponds to it.
[<RequireQualifiedAccess>]
type ModuleContainer =
    | InNamespace of ns: NamespaceKey
    | InModule of parent: ModuleKey

    member this.Namespace: NamespaceKey =
        match this with
        | ModuleContainer.InNamespace ns -> ns
        | ModuleContainer.InModule parent -> parent.Namespace

    /// How many `module`s deep this scope is, counting a namespace body as 0.
    member this.Depth: int =
        match this with
        | ModuleContainer.InNamespace _ -> 0
        | ModuleContainer.InModule parent -> parent.Container.Depth + 1

    /// The scopes a bare name written HERE is searched in, innermost FIRST.
    member this.SelfAndAncestors: ModuleContainer list =
        match this with
        | ModuleContainer.InNamespace _ -> [ this ]
        | ModuleContainer.InModule parent -> this :: parent.Container.SelfAndAncestors

/// A module. NO arity, because modules are not generic.
and ModuleKey =
    {
        Container: ModuleContainer
        Name: string
    }

    member this.Namespace: NamespaceKey = this.Container.Namespace

/// A path in scope with no `open` written for it.
[<RequireQualifiedAccess>]
type ImplicitOpen =
    /// `[<assembly: AutoOpen("…")>]`: in scope for every file compiled against the assembly.
    | AssemblyAutoOpen of ns: NamespaceKey
    /// `[<AutoOpen>]` on the module itself: in scope wherever its enclosing scope is open.
    | AutoOpen of md: ModuleKey
    /// A file's own `namespace N` header, implicitly opened over the file's body alone.
    | CurrentFileScope of ns: NamespaceKey

    /// The scope this open resolves names in.
    member this.Container: ModuleContainer =
        match this with
        | ImplicitOpen.AssemblyAutoOpen ns
        | ImplicitOpen.CurrentFileScope ns -> ModuleContainer.InNamespace ns
        | ImplicitOpen.AutoOpen md -> ModuleContainer.InModule md

[<RequireQualifiedAccess>]
type TypeContainer =
    | InNamespace of ns: NamespaceKey
    /// `parent` identifies the module's COMPILED module class, with the `…Module` suffix already
    /// applied, never the source name an `open` writes.
    | InModule of parent: ModuleKey
    /// A CLR *nested* type such as `` List`1+Enumerator ``; the parser cannot declare one.
    /// Nesting is decoded STRUCTURALLY, because that `+` is a reflection DISPLAY convention.
    | InType of outer: TypeKey

/// Containment chain + plain SOURCE name (never `` `N ``-mangled) + generic ARITY. CAUTION:
/// `=` does NOT reconcile a capability's BCL platform key with its canonical key
/// (`` IEnumerable`1 `` vs `Vesper.Collections.seq`), so use `sameNominalKey`.
and TypeKey =
    {
        Container: TypeContainer
        Name: string
        /// This segment's OWN generic-parameter count. A nested type's outer carries its
        /// own; the CLR spells each segment separately (`` Outer`1+Inner`1 ``).
        TyparArity: int
    }

    /// A nested type reports its OUTER's namespace, as the CLR does.
    member this.Namespace: NamespaceKey =
        match this.Container with
        | TypeContainer.InNamespace ns -> ns
        | TypeContainer.InModule parent -> parent.Namespace
        | TypeContainer.InType outer -> outer.Namespace

/// WHERE a candidate binding ENTERS the name environment: `Depth` enclosing `module`s, then
/// `Offset` within that scope (a declaration's own position, or that of the `open` that
/// brought it in). Field-ordered structural comparison, so `max` IS the resolution rule.
[<Struct>]
type BindingRank = { Depth: int; Offset: int }

module BindingRank =

    /// Below every rank a written declaration or `open` takes.
    let floor: BindingRank =
        {
            Depth = System.Int32.MinValue
            Offset = System.Int32.MinValue
        }

    /// The offset a declaration of a `rec` scope enters at: after the scope's whole prelude,
    /// which FS3200 pins ahead of every declaration, so it outranks each same-scope `open`.
    let afterPrelude: int = System.Int32.MaxValue

    /// The offset of a declaration with no position of its own within a scope: below every
    /// written one, so a same-depth `open` outranks it.
    let unpositioned: int = System.Int32.MinValue

    /// The highest-ranked candidate: `ValueNone` for an empty sequence, and the FIRST of
    /// several sharing the winning rank.
    let bestRanked (candidates: struct (BindingRank * 'T) seq) : struct (BindingRank * 'T) voption =
        let mutable best = ValueNone

        for struct (r, c) in candidates do
            match best with
            | ValueSome(struct (b, _)) when b >= r -> ()
            | _ -> best <- ValueSome(struct (r, c))

        best

    /// The winning rank: the MAXIMUM of `ranks`, `ValueNone` for an empty sequence.
    let maxOf (ranks: BindingRank seq) : BindingRank voption =
        match bestRanked (Seq.map (fun r -> struct (r, ())) ranks) with
        | ValueSome(struct (r, ())) -> ValueSome r
        | ValueNone -> ValueNone

/// How a name found in a scope is positioned against the other names in force at a use site.
[<RequireQualifiedAccess; Struct; NoComparison>]
type ScopeRoute =
    /// The use site's own scope, or one enclosing it, `depth` `module`s deep. A declaration
    /// found there enters at its own position.
    | Lexical of depth: int
    /// An `open`: everything it brings in enters where the `open` keyword is.
    | Opened of rank: BindingRank
    /// In scope with no `open` written for it.
    | Ambient

/// One scope in force at a use site, and where a name found in it enters the name
/// environment.
[<Struct; NoComparison>]
type ScopeEntry =
    {
        Container: ModuleContainer
        Route: ScopeRoute
    }

module ScopeEntry =

    /// The rank a declaration entering at `entersAt` takes when reached through `entry`.
    let rankOf (entry: ScopeEntry) (entersAt: int) : BindingRank =
        match entry.Route with
        | ScopeRoute.Lexical depth -> { Depth = depth; Offset = entersAt }
        | ScopeRoute.Opened rank -> rank
        | ScopeRoute.Ambient -> BindingRank.floor

    /// The rank of the entry itself, and the sort key of `bestFirst`: what a declaration with
    /// no position of its own takes, every declaration of another assembly among them. A
    /// `Lexical` entry ranks below a same-depth `open` here; use `rankOf` where positions matter.
    let rank (entry: ScopeEntry) : BindingRank = rankOf entry BindingRank.unpositioned

    /// The entries of `scopes`, best rank first: the search order for a lookup that takes
    /// the first hit rather than the highest-ranked one.
    let bestFirst (scopes: ScopeEntry list) : ScopeEntry list = scopes |> List.sortByDescending rank

    /// `scopes` extended with `implicitOpens`, best rank first, one entry per scope. An
    /// `[<AutoOpen>]` module enters where its enclosing scope did; an assembly auto-open, a
    /// file's own `namespace` header and the root namespace enter at the floor.
    let withAmbient (implicitOpens: ImplicitOpen list) (scopes: ScopeEntry list) : ScopeEntry list =
        let acc = ResizeArray<ScopeEntry> scopes

        // Reads `acc`, so a nested `[<AutoOpen>]` finds the entry its enclosing one added:
        // `implicitOpens` runs outermost first.
        let routeOf (o: ImplicitOpen) : ScopeRoute =
            match o with
            | ImplicitOpen.AssemblyAutoOpen _
            | ImplicitOpen.CurrentFileScope _ -> ScopeRoute.Ambient
            | ImplicitOpen.AutoOpen md ->
                let activating =
                    BindingRank.bestRanked (
                        seq {
                            for e in acc do
                                if e.Container = md.Container then
                                    struct (rank e, e.Route)
                        }
                    )

                match activating with
                | ValueSome(struct (_, route)) -> route
                | ValueNone -> ScopeRoute.Ambient

        for o in implicitOpens do
            acc.Add
                {
                    Container = o.Container
                    Route = routeOf o
                }

        acc.Add
            {
                Container = ModuleContainer.InNamespace NamespaceKey.Global
                Route = ScopeRoute.Ambient
            }

        bestFirst (List.ofSeq acc) |> List.distinctBy (fun e -> e.Container)

/// WHERE a by-NAME lookup is resolved FROM: the position in the file, the module scope the use
/// sits in, and every scope in force there, `open`s included.
[<NoComparison>]
type UseSite =
    {
        Pos: SourcePos
        Container: ModuleContainer voption
        /// Best rank first (`ScopeEntry.rank` order), one entry per container.
        Scopes: ScopeEntry list
    }

    member this.Offset: int = this.Pos.Offset

module UseSite =

    /// A use site with an unbounded position and no enclosing module: every declaration is
    /// in scope.
    let unbounded: UseSite =
        {
            Pos = SourcePos.unbounded
            Container = ValueNone
            Scopes = []
        }

/// A type name AS WRITTEN: the path of the scope that QUALIFIES it (`"N.A"` in `N.A.T`, EMPTY
/// for a bare `T`) plus the short name. A SOURCE path, never a compiled module name (`ListModule`).
[<Struct>]
type WrittenTypeName =
    {
        Path: string
        Name: string
    }

    /// The name as the source spells it, for diagnostics.
    member this.Written: string =
        match this.Path.Length with
        | 0 -> this.Name
        | _ -> this.Path + "." + this.Name

module WrittenTypeName =

    let bare (name: string) : WrittenTypeName = { Path = ""; Name = name }

/// A module-level binding / operator. No `ArgSig`: modules do not overload.
type BindingKey = { Decl: ModuleContainer; Name: string }

/// A key's name AS SHOWN TO A HUMAN, containment chain and generic arity dropped. A LOSSY
/// projection OUT of an identity, never a route back INTO one: to ask a table, ask the KEY.
[<Struct>]
type DisplayName = | DisplayName of string

/// The name a declaration compiles to, carried only where it DIFFERS from the name its source
/// writes: `[<CompiledName>]` on a value, the `Module` suffix on a module. Generic arity is a
/// separate axis, rendered `` `N `` from `TypeKey.TyparArity` by `SymbolKeyOps.typeMetaName`.
[<Struct>]
type CompiledName =
    | CompiledName of string

    /// `ValueNone` where a declaration's source name and the name it emits as agree.
    static member OfPair(source: string, compiled: string) : CompiledName voption =
        if source = compiled then
            ValueNone
        else
            ValueSome(CompiledName compiled)

/// A PLACE (assembly + namespace): enough to mint a ref without re-resolving. A symbol's
/// declaring type is not here; containment is the key's job.
type SymbolOrigin =
    {
        Home: SymbolHome
        Namespace: NamespaceKey
    }

    static member Empty =
        {
            Home = SymbolHome.Unstamped
            Namespace = NamespaceKey.Global
        }

/// `Field` and `Property` are both VALUE members, diverging only at CLR emission: `ldfld`
/// on a field ref vs a `call` to the `get_X` getter; on JS both are a value access.
[<RequireQualifiedAccess>]
type MemberStorage =
    | Field
    | Property
    | Method

    member s.IsValueMember: bool = s <> MemberStorage.Method
