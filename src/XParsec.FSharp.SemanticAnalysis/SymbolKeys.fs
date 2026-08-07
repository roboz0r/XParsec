namespace XParsec.FSharp.SemanticAnalysis

[<Measure>]
type tyVarId

/// A metavar id, dense and monotone within one file, so parallel arrays and side-tables key
/// by it directly.
type TyVarId = int<tyVarId>

/// An assembly's SIMPLE name — no version, culture or public key.
[<Struct>]
type AssemblyName =
    | AssemblyName of name: string

    member this.Name = let (AssemblyName n) = this in n

/// Where a symbol PHYSICALLY lives — never part of a key: nominal identity is the
/// containment chain + namespace + name. `Unstamped` is "no home": the compilation being
/// analysed, or a contract scrape.
[<RequireQualifiedAccess>]
type Origin =
    | Unstamped
    | InAssembly of asm: AssemblyName
    /// `InAssembly` REFINED to the declaring source file. The path carries its own
    /// `BucketName`, so the assembly is read off the file, not supplied a second time.
    | InFile of file: OriginPath

    member this.AssemblyOption: string voption =
        match this with
        | Origin.Unstamped -> ValueNone
        | Origin.InAssembly a -> ValueSome a.Name
        | Origin.InFile f -> ValueSome f.BucketName

    /// `ValueNone` wherever the producer knew only the assembly — a `.fsi` contract view or
    /// a metadata scrape.
    member this.DeclaringFile: OriginPath voption =
        match this with
        | Origin.Unstamped
        | Origin.InAssembly _ -> ValueNone
        | Origin.InFile f -> ValueSome f

/// A namespace — the root holder. `Path` is SEGMENTED (`["System"; "Collections"]`), so
/// prefix relations are segment-list tests. The EMPTY path IS the global namespace.
type NamespaceKey =
    {
        Path: EqArray<string>
    }

    /// The dotted rendering (`"System.Collections"`; `""` for the global namespace) — a
    /// BOUNDARY projection only. Identity comparisons use the segmented `Path`.
    member this.Dotted: string = System.String.Join(".", this.Path.Underlying)

    static member Global = { Path = EqArray.empty }

/// Also what holds a `BindingKey`, where `InNamespace` means the binding has NO declaring
/// module — a TOP-LEVEL `let`, or a flat package's export. No CLR type corresponds to it.
[<RequireQualifiedAccess>]
type ModuleHolder =
    | InNamespace of ns: NamespaceKey
    | InModule of parent: ModuleKey

    member this.Namespace: NamespaceKey =
        match this with
        | ModuleHolder.InNamespace ns -> ns
        | ModuleHolder.InModule parent -> parent.Namespace

    /// How many `module`s deep this scope is — a namespace body is 0.
    member this.Depth: int =
        match this with
        | ModuleHolder.InNamespace _ -> 0
        | ModuleHolder.InModule parent -> parent.Holder.Depth + 1

    /// The scopes a bare name written HERE is searched in, innermost FIRST.
    member this.SelfAndAncestors: ModuleHolder list =
        match this with
        | ModuleHolder.InNamespace _ -> [ this ]
        | ModuleHolder.InModule parent -> this :: parent.Holder.SelfAndAncestors

/// A module. NO arity — modules are not generic.
and ModuleKey =
    {
        Holder: ModuleHolder
        Name: string
    }

    member this.Namespace: NamespaceKey = this.Holder.Namespace

[<RequireQualifiedAccess>]
type TypeHolder =
    | InNamespace of ns: NamespaceKey
    /// `parent` names the module's COMPILED holder type — the `…Module` suffix already
    /// applied, never the source name an `open` writes.
    | InModule of parent: ModuleKey
    /// A CLR *nested* type such as `` List`1+Enumerator ``; the parser cannot declare one.
    /// Nesting is decoded STRUCTURALLY — that `+` is a reflection DISPLAY convention.
    | InType of outer: TypeKey

/// Containment chain + plain SOURCE name (never `` `N ``-mangled) + generic ARITY. CAUTION:
/// `=` does NOT reconcile a capability's BCL platform key with its canonical key
/// (`` IEnumerable`1 `` vs `Vesper.Collections.seq`) — use `sameNominalKey`.
and TypeKey =
    {
        Holder: TypeHolder
        Name: string
        /// This segment's OWN generic-parameter count. A nested type's outer carries its
        /// own; the CLR spells each segment separately (`` Outer`1+Inner`1 ``).
        TyparArity: int
    }

    /// A nested type reports its OUTER's namespace, as the CLR does.
    member this.Namespace: NamespaceKey =
        match this.Holder with
        | TypeHolder.InNamespace ns -> ns
        | TypeHolder.InModule parent -> parent.Namespace
        | TypeHolder.InType outer -> outer.Namespace

/// WHERE a candidate binding ENTERS the name environment: `Depth` enclosing `module`s, then
/// `Offset` within that scope (a declaration's own position, or that of the `open` that
/// brought it in). Field-ordered structural comparison, so `max` IS the resolution rule.
[<Struct>]
type BindingRank = { Depth: int; Offset: int }

/// WHERE a by-NAME lookup speaks FROM: the position in the file, the module scope the use
/// sits in, and the `open`s that let a sibling's types in.
[<NoComparison>]
type UseSite =
    {
        Pos: SourcePos
        Holder: ModuleHolder voption
        Opens: LocalOpen list
    }

    member this.Offset: int = this.Pos.Offset

module UseSite =

    /// A read with no position and no enclosing module to speak from: every declaration is
    /// in scope.
    let unbounded: UseSite =
        {
            Pos = SourcePos.unbounded
            Holder = ValueNone
            Opens = []
        }

/// A type name AS WRITTEN: the path of the scope that QUALIFIES it (`"N.A"` in `N.A.T`, EMPTY
/// for a bare `T`) plus the short name. A SOURCE path, never a compiled holder (`ListModule`).
[<Struct>]
type WrittenTypeName =
    {
        Path: string
        Name: string
    }

    /// The name as the source spells it — for diagnostics.
    member this.Written: string =
        if this.Path.Length = 0 then
            this.Name
        else
            this.Path + "." + this.Name

module WrittenTypeName =

    let bare (name: string) : WrittenTypeName = { Path = ""; Name = name }

/// A module-level binding / operator. No `ArgSig`: modules do not overload.
type BindingKey = { Decl: ModuleHolder; Name: string }

/// A key's name AS SHOWN TO A HUMAN, containment chain and generic arity dropped. A LOSSY
/// projection OUT of an identity, never a route back INTO one: to ask a table, ask the KEY.
[<Struct>]
type DisplayName = | DisplayName of string

/// A PLACE (assembly + namespace) — enough to mint a ref without re-resolving. A symbol's
/// declaring type is not here; containment is the key's job.
type SymbolOrigin =
    {
        Home: Origin
        Namespace: NamespaceKey
    }

    static member Empty =
        {
            Home = Origin.Unstamped
            Namespace = NamespaceKey.Global
        }

/// `Field` and `Property` are both VALUE members, diverging only at CLR emission — `ldfld`
/// on a field ref vs a `call` to the `get_X` getter; on JS both are a value access.
[<RequireQualifiedAccess>]
type MemberStorage =
    | Field
    | Property
    | Method

    member s.IsValueMember = s <> MemberStorage.Method
