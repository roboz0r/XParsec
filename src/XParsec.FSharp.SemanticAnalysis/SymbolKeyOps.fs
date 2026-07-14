namespace XParsec.FSharp.SemanticAnalysis

/// The generic `SymbolKey` ↔ compiled-name string algebra: arity-strip /
/// arity-qualify, namespace segmentation, key projection and minting. These have
/// nothing to do with the well-known runtime singletons (`RuntimeNames`) or with
/// external-symbol resolution (`module ExternalSymbols`); they operate purely on
/// `SymbolKey` / strings, so they live here — the one foundational home — right
/// after `SemanticInfo` defines `SymbolKey`.
///
/// This module deliberately precedes both `RuntimeNames` (which routes its
/// `bareName` recognition through here) and `ExternalSymbols` (whose provider
/// surface and `ExternalSymbol` builders consume these projections). Keeping the
/// algebra below the `IExternalSymbolProvider` interface is what lets that
/// interface be compiled *after* `Tast`, so it can name `TDecl` directly (the
/// inline-body channel) without a sibling interface + runtime cast.
[<RequireQualifiedAccess>]
module SymbolKeyOps =

    // --- Shared compiled-name string rules -------------------------------------------
    //
    // One definition each of the string rules identity minting / decomposition repeats:
    // strip the `` `N `` arity suffix, arity-qualify a simple name, segment a dotted
    // namespace. Everything below routes through these; `LocalSymbolKey` (internal,
    // compiles later) delegates its `arityName` here so the rules can't drift.

    /// Strip a trailing `` `N `` generic-arity suffix, returning the bare compiled
    /// name (namespace kept). The contract layer arity-suffixes generic compiled
    /// names, so a name may arrive as either `Vesper.Collections.List` or
    /// `Vesper.Collections.List`1`; recognition must accept both.
    let bareName (name: string) : string =
        let tick = name.IndexOf '`'
        if tick < 0 then name else name.Substring(0, tick)

    /// Arity-qualify a simple name (`List` + 1 ⇒ `` List`1 ``). Returns the name
    /// unchanged for a non-generic type (arity ≤ 0) or one that already carries a
    /// `` `N `` suffix — the single home for the `.NET`-style arity-name rule the
    /// registry key, the stamped `SymbolKey` name, and the emitted metadata name all
    /// share.
    let arityName (name: string) (arity: int) : string =
        if arity > 0 && not (name.Contains '`') then
            sprintf "%s`%d" name arity
        else
            name

    /// Split a fully-qualified compiled name at its last `.` into `(qualifier, last)`:
    /// `Vesper.Option` ⇒ `("Vesper", "Option")`; a name with no `.` ⇒ `("", name)`.
    /// PRIVATE and deliberately narrow: it is only ever applied to a name whose last
    /// segment is known to be the simple name (a type's or a binding's). The general
    /// "recover a namespace by splitting at the last dot" hazard — which mis-cut
    /// `Vesper.Collections.seq` into `ns = "Vesper"` — is gone: a `NamespaceKey` is
    /// segmented, so a namespace is carried, never re-derived.
    let private splitLastDot (compiled: string) : string * string =
        let i = compiled.LastIndexOf '.'

        if i < 0 then
            "", compiled
        else
            compiled.Substring(0, i), compiled.Substring(i + 1)

    /// The last `.`-separated segment of a compiled name (`Vesper.Option` ⇒
    /// `Option`), arity suffix stripped (`Choice`2` ⇒ `Choice`). Used to test a
    /// union's declaring type against a written qualifier (`Option.Some` /
    /// `Choice.Choice1Of2`). A name with no `.` is returned (bare) unchanged.
    let shortName (compiled: string) : string = bareName (snd (splitLastDot compiled))

    // --- Namespaces ------------------------------------------------------------------

    /// Segment a dotted namespace string. `""` ⇒ the EMPTY path, i.e. the global
    /// namespace — a real value, not a sentinel.
    let nsPath (dotted: string) : EqArray<string> =
        if System.String.IsNullOrEmpty dotted then
            EqArray.empty
        else
            EqArray.ofArray (dotted.Split '.')

    /// A `NamespaceKey` from the boundary spelling: a dotted namespace.
    let namespaceKey (dottedNs: string) : NamespaceKey = { Path = nsPath dottedNs }

    // --- The ONE `TypeKey` ↔ metadata-name renderer / parser --------------------------
    //
    // `MetadataSymbols` resolves an external type by its full metadata name
    // (`asm.GetType(name)`, `resolveTypeLocked`), and a NESTED type's reflection name is
    // `Ns.Outer`2+Inner`. The `+` is a reflection DISPLAY convention, not a metadata
    // name — which is why a `TypeRef` with a flat `Outer+Inner` name and an AssemblyRef
    // scope fails to bind (`TypeLoadException`) and must instead chain through the
    // enclosing type's `TypeRef` as its ResolutionScope. `TypeHolder.InType` is the one
    // representation of that nesting; these two functions are the one pair of
    // conversions to and from the display spelling. Both consumers — reflection lookup
    // by display name, and CLR `TypeRef` chaining — go through them.

    /// The `+`-joined nested chain WITHOUT the namespace (`` List`1+Enumerator ``); the
    /// bare `Name` for a top-level type. The simple-name half of `typeMetaName`.
    let rec typeNestedName (t: TypeKey) : string =
        match t.Holder with
        | TypeHolder.InType outer -> typeNestedName outer + "+" + t.Name
        | _ -> t.Name

    /// The declaring namespace of a type, dotted. A nested type reports its OUTER's
    /// namespace — which is what the CLR does.
    let typeNs (t: TypeKey) : string = t.Namespace.Dotted

    /// The full metadata/reflection name of a type — the string
    /// `MetadataSymbols.resolveTypeLocked` hands to `asm.GetType`, and the key every
    /// provider store face is addressed by. `Ns.Outer`2+Inner` for a nested type.
    /// THE renderer; there is exactly one.
    let typeMetaName (t: TypeKey) : string =
        let ns = typeNs t
        let simple = typeNestedName t
        if ns = "" then simple else ns + "." + simple

    /// Mint a `TypeKey` from the boundary spelling `(dotted ns, simple name)`, where
    /// `name` may carry the `+`-mangled nested chain a reflection display name
    /// produces. THE parser; there is exactly one. A module-held type is minted by
    /// `TypeRegistration.localTypeHolder` (the parser cannot produce one: a `+` chain is
    /// CLR nesting, and a `.` prefix is the namespace).
    let typeKeyOf (dottedNs: string) (name: string) : TypeKey =
        let ns = namespaceKey dottedNs

        if name.IndexOf '+' < 0 then
            {
                Holder = TypeHolder.InNamespace ns
                Name = name
            }
        else
            let parts = name.Split '+'

            let mutable k =
                {
                    Holder = TypeHolder.InNamespace ns
                    Name = parts.[0]
                }

            for i in 1 .. parts.Length - 1 do
                k <-
                    {
                        Holder = TypeHolder.InType k
                        Name = parts.[i]
                    }

            k

    /// Arity-qualify a minted `TypeKey`'s name. The test is against the WHOLE nested
    /// chain (`typeNestedName`), not the innermost segment: a nested type's arity is
    /// carried by its OUTER (`` List`1+Enumerator `` — the enumerator inherits `List`'s
    /// typar and has no `` `n `` of its own), so an outer that already carries a backtick
    /// means the requested arity is already spelled and must NOT be re-appended to the
    /// inner name.
    let private withArity (arity: int) (t: TypeKey) : TypeKey =
        if arity > 0 && not ((typeNestedName t).Contains '`') then
            { t with Name = arityName t.Name arity }
        else
            t

    // --- Modules ---------------------------------------------------------------------

    /// The full dotted name of a module (`Vesper.Collections`) — namespace path plus
    /// the module chain. The CLR compiles a module to a type of this name, so this is
    /// what `EmitExternalCall` and the JS import path consume.
    let rec moduleFullName (m: ModuleKey) : string =
        match m.Holder with
        | ModuleHolder.InNamespace ns ->
            let d = ns.Dotted
            if d = "" then m.Name else d + "." + m.Name
        | ModuleHolder.InModule parent -> moduleFullName parent + "." + m.Name

    /// A `ModuleKey` from its HOLDER + simple name. Modules nest, so the containment is
    /// the holder chain and never a dotted string: `moduleFullName` renders the chain,
    /// and nothing re-cuts that rendering back. Every producer knows which of its
    /// segments are the namespace and which are the enclosing modules at the point it
    /// builds the key — a dotted-name mint could only guess (last segment = module),
    /// which flattened every nested module into the namespace path.
    let moduleKeyOf (holder: ModuleHolder) (name: string) : ModuleKey = { Holder = holder; Name = name }

    /// The holder for something declared DIRECTLY in a namespace, from the boundary
    /// spelling `(dotted ns)`. In binding position it is the UNQUALIFIED binding
    /// (no declaring module) — a real holder, not a sentinel.
    let inNamespace (dottedNs: string) : ModuleHolder =
        ModuleHolder.InNamespace(namespaceKey dottedNs)

    /// A module declared directly in a namespace (`namespace Vesper` + `module
    /// Collections`), from the boundary spelling `(dotted ns, module name)`. The
    /// namespace and the module are named SEPARATELY — there is no dotted string to cut.
    let moduleInNamespace (dottedNs: string) (name: string) : ModuleKey = moduleKeyOf (inNamespace dottedNs) name

    // --- Smart constructors -----------------------------------------------------------

    /// `SymbolKey.Type` from the boundary spelling `(dotted ns, name)`.
    let typeKey (ns: string) (name: string) : SymbolKey = SymbolKey.Type(typeKeyOf ns name)

    /// The full dotted name of whatever holds a binding: the namespace for an
    /// unqualified one, the module's full name otherwise. The inverse of `bindingKeyOf`'s
    /// second argument.
    let holderFullName (h: ModuleHolder) : string =
        match h with
        | ModuleHolder.InNamespace ns -> ns.Dotted
        | ModuleHolder.InModule m -> moduleFullName m

    /// A `BindingKey` from its declaring HOLDER + simple name. `InNamespace` in holder
    /// position is the UNQUALIFIED binding (a flat package's export, a global extern);
    /// `InModule` the ordinary module-qualified one, nesting included.
    let bindingKeyOf (decl: ModuleHolder) (name: string) : BindingKey = { Decl = decl; Name = name }

    /// `SymbolKey.Binding` over a declaring `ModuleHolder`.
    let valueKey (decl: ModuleHolder) (name: string) : SymbolKey =
        SymbolKey.Binding(bindingKeyOf decl name)

    /// `SymbolKey.Binding` for a value in a module that sits directly in a namespace —
    /// the well-known-symbol spelling `(dotted ns, module, name)`, where the caller
    /// names the namespace and the module separately.
    let moduleValueKey (dottedNs: string) (declModule: string) (name: string) : SymbolKey =
        valueKey (ModuleHolder.InModule(moduleInNamespace dottedNs declModule)) name

    /// A `MemberKey` over a declaring `TypeKey`. The declaring slot is a `TypeKey` by
    /// construction, so the `failwithf "declaring key is not a TypeKey"` checks
    /// `ClrExternalMembers` used to carry are gone.
    let memberKeyOf (decl: TypeKey) (name: string) (argSig: EqArray<string>) (kind: MemberKind) : MemberKey =
        {
            Decl = decl
            Name = name
            ArgSig = argSig
            Kind = kind
        }

    /// `SymbolKey.Member` — the mechanical successor to the old
    /// `SymbolKey.MemberKey(decl, name, argSig, kind)`.
    let memberKey (decl: TypeKey) (name: string) (argSig: EqArray<string>) (kind: MemberKind) : SymbolKey =
        SymbolKey.Member(memberKeyOf decl name argSig kind)

    // --- Generic `SymbolKey` projection ----------------------------------------------
    //
    // These operate on any `SymbolKey` (decompose / mint); they have nothing to do
    // with the well-known runtime singletons, so they live here next to the mints.
    // `RuntimeNames` (which compiles after this file) keeps only the singleton
    // constants + recognisers and routes its `bareName` / `qualifiedName` needs here.

    /// The key's `name` component with the containment dropped but the `` `N `` arity
    /// suffix PRESERVED — the non-lossy, IDENTITY counterpart to `simpleName` (which
    /// strips arity for human display). Use this where a name feeds a canonical
    /// string-keyed repr map (`canonName` / `IntrinsicForwardRepr` /
    /// `IntrinsicReverseCanon` / the SRTP `primitiveSupports`) — the codegen/repr axis
    /// that is canon-name-keyed BY DESIGN and must NOT lose arity. For a *comparison*
    /// against a well-known intrinsic prefer the `TyBool`/`TyUnit`/`TyArray`/… active
    /// patterns (identity match) over `intrinsicName key = "…"`; reserve `simpleName`
    /// for human-facing diagnostics and backend name mangling.
    let intrinsicName (k: SymbolKey) : string =
        match k with
        | SymbolKey.Type t -> t.Name
        | SymbolKey.Binding b -> b.Name
        | SymbolKey.Member m -> m.Name

    /// The bare simple name (containment dropped, arity suffix stripped) of a key's
    /// name component. Dropping the arity is LOSSY, so this is for uses where the
    /// arity is genuinely not part of the identity: human-facing diagnostics, and
    /// backends whose names carry no generic arity (the JS emitter, CLR member-name
    /// mangling). It is NOT a registry lookup key — the `TypeRegistry` tables are keyed
    /// by the whole `TypeKey`, and a bare short name does not even resolve for an
    /// arity-overloaded type (`Foo`2`/`Foo`3`), so a `simpleName`-keyed lookup MISSES
    /// and mis-classifies the type as external. To resolve a key against a registry use
    /// the `*ByKey` helpers (`tryClassByKey` / `tryUnionByKey` / `tryRecordByKey` /
    /// `tryInterfaceImplHostByKey`), which take the key itself.
    let simpleName (k: SymbolKey) : string = bareName (intrinsicName k)

    /// The fully-qualified compiled name for an EXTERNAL nominal lookup
    /// (`externalUnionRef` / `externalRecordRef` / `externalClassRef`): the full
    /// metadata name for a type (arity suffix retained, nesting `+`-joined), the
    /// module-qualified name for a binding, the bare name for a member. The lookups
    /// normalise bare-vs-suffixed internally, so passing the arity-qualified form is
    /// safe for both the metadata layer (suffixed keys) and the contract layer (bare
    /// keys).
    let qualifiedName (k: SymbolKey) : string =
        match k with
        | SymbolKey.Type t -> typeMetaName t
        | SymbolKey.Binding b ->
            match holderFullName b.Decl with
            | "" -> b.Name
            | h -> h + "." + b.Name
        | SymbolKey.Member m -> m.Name

    // A nominal `SemType`'s `SymbolKey` participates in unification equality, so the
    // SAME type minted via different paths (use-site resolution, VesperLib contract
    // extraction, the `*Key` runtime constants, local registration) must compare
    // EQUAL. Identity is the containment chain — namespace + module* + name — and
    // nothing else, so a key minted from a bare compiled name and one minted from a
    // fully resolved external shape agree by construction. Codegen decides local vs
    // external by asking its own type table, not the key.

    /// Mint a nominal type key from a fully-qualified compiled name: the last `.`
    /// segment is the simple name, the prefix the namespace.
    let qualifiedTypeKeyOfT (compiled: string) (arity: int) : TypeKey =
        let ns, simple = splitLastDot compiled
        withArity arity (typeKeyOf ns simple)

    /// `qualifiedTypeKeyOfT` as a `SymbolKey`. THE mint for a fully-qualified compiled
    /// name held as a string — a platform repr, a fixed printf-sink name, a codegen
    /// bridge name, a metadata/contract scrape. Passing arity 0 for an already-suffixed
    /// generic name is lossless (`arityName` is a no-op on a suffixed name).
    let qualifiedTypeKey (compiled: string) (arity: int) : SymbolKey =
        SymbolKey.Type(qualifiedTypeKeyOfT compiled arity)

    /// Mint a nominal type key for an external type from its resolved shape's `origin`
    /// + the matched compiled name + arity. EVERY external-type producer (`Translate`,
    /// `InferResolve`, the VesperLib extractor's `mkNominal`) routes through this.
    ///
    /// The namespace comes from `compiled` itself whenever `compiled` is qualified; the
    /// `origin` supplies it only for a BARE `compiled`. Taking the namespace from the
    /// name it belongs to cannot mis-cut — a package-BLANKET origin (`Vesper`) stripped
    /// off the compiled name mis-cut `Vesper.Collections.seq` into `ns = "Vesper"` /
    /// `name = "Collections.seq"` and broke capability-key matching.
    let externalTypeKeyOf (origin: SymbolOrigin) (compiled: string) (arity: int) : TypeKey =
        if compiled.IndexOf '.' >= 0 then
            qualifiedTypeKeyOfT compiled arity
        else
            withArity arity (typeKeyOf origin.Namespace.Dotted compiled)

    let externalTypeKey (origin: SymbolOrigin) (compiled: string) (arity: int) : SymbolKey =
        SymbolKey.Type(externalTypeKeyOf origin compiled arity)

    /// The contract-sourced canon key for an intrinsic the VesperLib extractor
    /// publishes. An intrinsic is a nominal like any other, so its canon key is simply
    /// the key of its COMPILED name — which `registerTypeDecl` already arity-suffixed
    /// (`` Vesper.Collections.seq`1 ``), so the arity is IN the key and key equality is
    /// the whole identity test. Deliberately the same mint the use-site stamp takes
    /// (`TypeHeadStamp.useSiteTypeKey`'s `Intrinsic` arm) and the same one
    /// `TypeRegistry.IntrinsicKeys` stamps for a self-compiled intrinsic, so all three
    /// compare EQUAL by construction — no arity-blind matcher stands between them.
    /// (Arity 0: `compiled` carries the suffix already, and `arityName` is a no-op on a
    /// name that does — including the array's backtick-escaped `` ``[]`` ``.)
    let intrinsicCanonKey (compiled: string) : SymbolKey = qualifiedTypeKey compiled 0
