namespace XParsec.FSharp.SemanticAnalysis

/// Unit of measure tagging the dense metavar id so it can never be confused with a
/// `NodeKey`, a `RegionId`, or a raw array index. Erased to `int` at runtime.
[<Measure>]
type tyVarId

/// A stable, dense, monotone metavar id minted per file by `TypeStore`. Erased to
/// `int`; the arena keys its parallel arrays / side-tables by it.
type TyVarId = int<tyVarId>

// `SymbolOrigin` / `SymbolKey` / `MemberKind` live here (ahead of `SemType`)
// because the nominal `SemType` cases (`TyUnion` / `TyRecord` / `TyClass` / `TyEnum`) carry
// a `TypeKey` as their identity — pure string/EqArray records with no `SemType`
// dependency. `ExternalSymbols.fs` (which mints/decomposes these) compiles after.

/// A simple assembly name (the `ProjectInfo` reference-set key), wrapped so it can
/// never be confused with a namespace or a type name at the type level.
[<Struct>]
type AssemblyName =
    | AssemblyName of name: string

    member this.Name = let (AssemblyName n) = this in n

/// Where a symbol PHYSICALLY lives. This is not part of any key: nominal identity is
/// the containment chain + namespace + name, and within one compilation a fully
/// qualified name names at most one type. The assembly is a *function of* the identity,
/// carried on the resolved SHAPE (`SymbolOrigin`) and consulted only where a backend
/// needs a physical location (a CLR `AssemblyRef` scope, a JS import path).
/// `Unstamped` is the "no home / placeholder" state: the compilation being analysed,
/// plus the front-end-only / contract-scrape paths that have no home assembly at all,
/// and the pre-stamp placeholder a stamping wrapper (`ExternalSymbolProviders.stack`,
/// `ReferencedProject.wrap`) overwrites before the shape reaches a backend.
[<RequireQualifiedAccess>]
type Origin =
    | Unstamped
    | InAssembly of asm: AssemblyName

    /// The home assembly's simple name, or `ValueNone` on an `Unstamped` origin (no
    /// home: the compilation being analysed, a front-end-only / contract-scrape path, or
    /// a pre-stamp placeholder a stamping wrapper overwrites). TOTAL — the caller decides
    /// what "no home" means (fall back to local, or fail with its OWN import-path
    /// message), rather than a throwing accessor deciding for every consumer.
    member this.AssemblyOption: string voption =
        match this with
        | Origin.Unstamped -> ValueNone
        | Origin.InAssembly a -> ValueSome a.Name

/// A namespace — the root holder. `Path` is SEGMENTED (`["System"; "Collections"]`),
/// never a dotted string: the prefix relations the codebase needs (`StartsWith(ns + ".")`)
/// are segment-list prefix tests. A dotted string can only approximate them with a
/// last-dot cut, which mis-splits a name like `Vesper.Collections.seq`.
/// An EMPTY path is the global namespace — a real thing (CLR types live there), not a
/// sentinel: the global namespace is spelled by the empty path, never by `ns = ""`.
type NamespaceKey =
    {
        Path: EqArray<string>
    }

    /// The dotted rendering (`"System.Collections"`; `""` for the global namespace).
    /// The BOUNDARY projection — for CLR `TypeRef` namespace slots, provider store
    /// keys and diagnostics. Identity comparisons use the segmented `Path`.
    member this.Dotted: string = System.String.Join(".", this.Path.Underlying)

    /// The global namespace.
    static member Global = { Path = EqArray.empty }

/// What holds a module: a namespace, or an enclosing module (modules nest).
///
/// Also what holds a `BindingKey`. `InNamespace` in *that* position means the binding
/// has NO declaring module — it sits directly in the namespace. That is what a TOP-LEVEL
/// `let` is: a binding written outside any `module`, whose identity is its file's
/// namespace plus its name (the global namespace for a file with no header, so it
/// qualifies to the bare name). The EXTERNAL vocabulary produces the same shape — a TS
/// package's top-level export (`TsManifestProvider`, `nsPath = ""`) and a flat-package
/// contract extern (`ExternalSymbols.monoFrozen "printfn"`) are both exactly that.
///
/// No CLR type corresponds to it, so a backend homes such a binding on a holder of its own
/// choosing (the anonymous "Program" holder). That is an EMISSION choice and leaves the
/// identity alone: nothing about the key names the type a backend picked.
[<RequireQualifiedAccess>]
type ModuleHolder =
    | InNamespace of ns: NamespaceKey
    | InModule of parent: ModuleKey

    /// Walk to the namespace at the root of the chain.
    member this.Namespace: NamespaceKey =
        match this with
        | ModuleHolder.InNamespace ns -> ns
        | ModuleHolder.InModule parent -> parent.Namespace

    /// How many `module`s deep this scope is — a namespace body is 0. F# enters an inner
    /// scope AFTER its enclosing one, so a deeper scope's contribution to the name
    /// environment outranks everything the scopes above it added: this int is the whole of
    /// "a bare name resolves innermost-outward".
    member this.Depth: int =
        match this with
        | ModuleHolder.InNamespace _ -> 0
        | ModuleHolder.InModule parent -> parent.Holder.Depth + 1

    /// The scopes a bare name written HERE is searched in, innermost FIRST: this scope,
    /// then each enclosing module, then the namespace at the root.
    member this.SelfAndAncestors: ModuleHolder list =
        match this with
        | ModuleHolder.InNamespace _ -> [ this ]
        | ModuleHolder.InModule parent -> this :: parent.Holder.SelfAndAncestors

/// A module. NO arity — modules are not generic. That asymmetry with `TypeKey` is
/// the point: a module is simpler (no generics, no overloading) and richer in
/// containment (it holds modules *and* types).
and ModuleKey =
    {
        Holder: ModuleHolder
        Name: string
    }

    /// Walk the holder chain to the namespace at its root.
    member this.Namespace: NamespaceKey = this.Holder.Namespace

/// What holds a type.
[<RequireQualifiedAccess>]
type TypeHolder =
    | InNamespace of ns: NamespaceKey
    /// A type declared inside a `module` — `namespace N` + `module M` + `type T`.
    /// `parent` names the module's COMPILED holder type (the `…Module` suffix already
    /// applied), so the chain reads as the containment the CLR will eventually emit:
    /// `T` nested in `M`, `M` in namespace `N`.
    ///
    /// The holder is part of the CLAIM a declaration holds (`TypeIdentity.Holder`), so
    /// `N.A.T` and `N.B.T` are two distinct types — not one name contested twice.
    | InModule of parent: ModuleKey
    /// EXTERNAL ONLY — a CLR *nested* type. Unconstructible from Vesper source (the
    /// parser cannot declare a nested type); required to name
    /// `` System.Collections.Generic.List`1+Enumerator ``, the shape the duck-typed
    /// struct-enumerator path consumes. This case decodes the nesting STRUCTURALLY: the `+`
    /// in that display name is a reflection *display* convention, not a metadata name, so a
    /// nested `TypeRef` must chain through its enclosing type's `TypeRef` as ResolutionScope
    /// rather than be recovered by splitting a string on `+`.
    | InType of outer: TypeKey

/// A type definition — a nominal identity, and for a project-local type its
/// `TypeRegistry` key. Identity is the containment chain + the plain name + the generic
/// ARITY, and nothing else: F# overloads a type name on arity alone, so that int is the
/// whole of the discriminator a name needs. (NOT the typars themselves — a typar's name
/// and constraints are not identity-bearing, and a key minted from a contract, which has
/// no constraint detail, must still compare equal to one minted from source. The typar
/// DETAIL lives on `RecordTypeInfo` / `ClassTypeInfo` / …, the type ARGUMENTS on the
/// `SemType` — `TyUnion(key, args)`.)
///
/// `Name` is the PLAIN source name (`List`, `seq`, `[]`) — never the CLR `` `N ``-mangled
/// metadata spelling, which is a *rendering* of `(Name, Arity)` and lives only at the
/// metadata boundary (`SymbolKeyOps.typeMetaName`, the one renderer; `typeKeyOf`, the one
/// parser — inverses on the `InNamespace`/`InType` sublattice, which is the whole of what
/// a bare metadata name can express; see `typeKeyOf`). Because the arity is a FIELD, a
/// producer cannot forget to state it, and no consumer can be arity-blind by accident.
///
/// For a NESTED type `Name`/`TyparArity` are the innermost segment's own (`Enumerator`, 0); the
/// `+`-mangled reflection spelling is produced on demand, each segment rendering its OWN
/// count (`` List`1+Enumerator ``), which is the CLR rule.
///
/// CAUTION for type-identity comparisons in the unifier: structural `=` on two
/// `TypeKey`s does NOT reconcile a language capability's two nominal faces (its BCL
/// platform key vs. its canonical key — e.g. `IEnumerable`1` vs. `Vesper.Collections.seq`).
/// When comparing nominal heads for "same type" at a unify / subsume / overload seam, go
/// through `UnificationEngineCore.sameNominalKey` (or `capabilityCanonKey`), never a bare
/// `=`, or a capability spelled as its BCL face will read as a distinct type.
and TypeKey =
    {
        Holder: TypeHolder
        Name: string
        /// This segment's OWN generic-parameter count (0 ⇒ non-generic, and no `` `N ``
        /// when rendered). A nested type's outer carries its own count; the CLR spells
        /// each segment's separately (`` Outer`1+Inner`1 `` = one typar each).
        TyparArity: int
    }

    /// Walk the holder chain to the namespace at its root. A nested type reports its
    /// OUTER's namespace — which is exactly what the CLR does.
    member this.Namespace: NamespaceKey =
        match this.Holder with
        | TypeHolder.InNamespace ns -> ns
        | TypeHolder.InModule parent -> parent.Namespace
        | TypeHolder.InType outer -> outer.Namespace

/// WHERE a candidate binding ENTERS the name environment as seen from one use site. F#
/// builds that environment by descending the module tree and ADDING, in source order, each
/// declaration and each `open` — and the LAST thing added wins. So precedence is not a
/// hand-ordered cascade of special cases (innermost-first, open-beats-outer-decl,
/// last-open-wins); it is a single ordering, and this is it:
///
///   * `Depth` — how many `module`s enclose the scope that added the binding. An inner
///     scope is entered after its enclosing one, so it outranks everything above it. This
///     is what makes a bare name resolve innermost-outward, and it is what keeps a
///     `module rec` (where every declaration in the scope shares one offset) ordered.
///   * `Offset` — where in that scope the binding was added: a declaration's own
///     `VisibleFrom`, or the offset of the `open` that brought it in. Within one scope a
///     declaration and an `open` are ordered by nothing but the text, which is exactly how
///     F# orders them.
///
/// Comparison is structural and field-ordered — `Depth`, then `Offset` — so `max` IS the
/// resolution rule.
[<Struct>]
type BindingRank = { Depth: int; Offset: int }

/// WHERE a by-NAME lookup speaks FROM. A name is not an identity on its own — it is one
/// only as seen from somewhere — and "somewhere" in F# is three facts, so they travel as
/// one value rather than as arguments a caller can supply some of:
///
///   * `Pos` — the place in the file. Declaration scoping is file-ordered, so a claim
///     answers only at offsets at or after it (`TypeIdentity.VisibleFrom`).
///   * `Holder` — the module / namespace chain the use is nested in, INNERMOST last (the
///     `ModuleHolder` chain `ModuleRules.holderChain` builds from the use's containment).
///     A bare name resolves innermost-outward, so a use inside `module A` is not the same
///     use site as one at namespace level even at the same offset. A SIBLING module
///     contributes nothing to it: `module A`'s types are simply not in scope in `module B`.
///   * `Opens` — the `open`s in scope, which is how a sibling module's types get in.
///
/// `Pos` is a `SourcePos`, whose representation is private: a use site can therefore only
/// be pinned to a node that HAS a place in the file, and a counter-minted key cannot mint
/// one. `Holder` is `ValueNone` exactly for a read that speaks from nowhere — the
/// whole-file view (`UseSite.unbounded`).
[<NoComparison>]
type UseSite =
    {
        Pos: SourcePos
        Holder: ModuleHolder voption
        Opens: LocalOpen list
    }

    /// The offset a visibility test compares a claim's `VisibleFrom` against.
    member this.Offset: int = this.Pos.Offset

module UseSite =

    /// A read that sees EVERY declaration, wherever it sits and whatever holds it — for a
    /// query with no position and no enclosing module to speak from (an observer of the
    /// finished registry, a consumer that already holds a resolved key).
    let unbounded: UseSite =
        {
            Pos = SourcePos.unbounded
            Holder = ValueNone
            Opens = []
        }

/// A type name AS WRITTEN at a use site, split where the syntax splits it: the dotted SOURCE
/// path of the scope that QUALIFIES the name (`"A"` in `A.T`, `"N.A"` in `N.A.T`, EMPTY for a
/// bare `T`), and the short name itself. The two facts a by-name type lookup needs from the
/// syntax, and the only two — a written head says which SCOPE to look in and which NAME to
/// find there.
///
/// The path is a SOURCE path (what an `open` and a qualifier write), never a compiled holder
/// name (`ListModule`): the scope it names is recovered by resolving it against the scopes
/// this file declares, exactly as an `open`'s path is.
///
/// A BARE name is the empty path — not a separate case. A bare name and a qualified one
/// resolve by ONE rule (`TypeRegistry.claimRank`): the name enters the environment through a
/// scope, and the empty path names the enclosing scope itself.
[<Struct>]
type WrittenTypeName =
    {
        /// The qualifying scope's dotted SOURCE path; empty for a bare name.
        Path: string
        /// The short type name (the last segment as written, no arity suffix).
        Name: string
    }

    /// The name as the source spells it — for diagnostics.
    member this.Written: string =
        if this.Path.Length = 0 then
            this.Name
        else
            this.Path + "." + this.Name

module WrittenTypeName =

    /// A name written with no qualifier.
    let bare (name: string) : WrittenTypeName = { Path = ""; Name = name }

/// A module-level binding / operator. No `ArgSig`: modules do not overload.
///
/// `Decl` is a `ModuleHolder`, so the two states are distinguished BY CASE, not by an
/// empty string: `InModule m` is the ordinary module-qualified binding
/// (`Vesper.Unchecked.defaultof`), and `InNamespace ns` is the UNQUALIFIED one — a flat
/// package's export, which has no declaring module. A consumer matches on that case,
/// never on a `when ns <> ""` string guard.
///
/// A namespace holding a value is not F# — but this key names the EXTERNAL vocabulary
/// too, where a TS module's top-level export and a flat-package contract extern are
/// exactly that.
type BindingKey = { Decl: ModuleHolder; Name: string }

/// A key's name AS SHOWN TO A HUMAN — the result of `SymbolKeyOps.simpleName`, which
/// drops the containment chain and the generic arity. A LOSSY projection OUT of an
/// identity, and never a route back INTO one: nothing mints a key from it, and no table
/// is keyed by it.
///
/// A single-case DU, not an abbreviation, precisely so that recovering the string takes an
/// explicit `let (DisplayName s) = …`. Two consumers legitimately do:
///   * HUMAN — a diagnostic, an error message, display text;
///   * BACKEND NAME EMISSION — mangling an identifier the target actually emits (a JS
///     identifier, a CLR member name), whose names carry no generic arity.
/// Every other unwrap is treating a display string as an identity — the bug this type
/// exists to make greppable. To ask a table, ask the KEY (`TypeRegistry`'s `*ByKey`
/// helpers, `IntrinsicReprKeys` / `IntrinsicForwardRepr`); to recognise a well-known
/// intrinsic, match the KEY (`IntrinsicTypePatterns`).
[<Struct>]
type DisplayName = | DisplayName of string

/// Where a resolved symbol physically lives — enough for codegen to mint a ref
/// without re-resolving. It is the `key -> home` ORACLE: a `SymbolKey` names *what* a
/// symbol is, and the shape a provider resolves for that key carries, here, *where* it
/// is. Nothing else in the pipeline knows a symbol's assembly.
///
/// A symbol's DECLARING TYPE is not here: it is `MemberKey.Decl : TypeKey`, on the
/// key itself. An origin names a *place* (assembly + namespace); containment is the
/// key's job.
type SymbolOrigin =
    {
        Home: Origin
        Namespace: NamespaceKey
    }

    /// The default carried by symbols that don't (yet) record an origin —
    /// project-local / pre-stamp placeholder, global namespace.
    static member Empty =
        {
            Home = Origin.Unstamped
            Namespace = NamespaceKey.Global
        }

/// How an external member is STORED/accessed — the storage-and-shape axis,
/// orthogonal to the key-identity `MemberKind` above (which interns vtable slots).
/// `Field` and `Property` are both *value members* (no parameters, value in the
/// signature's `Return`); they diverge only at CLR emission — a `Field` reads via a
/// `FieldRef` + `ldfld`/`ldsfld`, a `Property` via its `get_X` getter `MemberRef` +
/// `call`. On JS both are a plain value access (a `Field` adds only `readonly`
/// fidelity, not yet modelled). `Method` is a function member (`call`/JS call).
/// Consumers that only care about value-vs-function read `IsValueMember` (here, or the
/// forwarding `ExternalMember.IsValueMember` / `ResolvedExternalMember.IsValueMember`)
/// rather than matching this directly.
[<RequireQualifiedAccess>]
type MemberStorage =
    | Field
    | Property
    | Method

    /// A value member (`Field`/`Property` — no parameters, value in the signature's
    /// `Return`) vs a `Method`. The canonical value-vs-function predicate; the
    /// `ExternalMember` / `ResolvedExternalMember` members of the same name forward here.
    member s.IsValueMember = s <> MemberStorage.Method
