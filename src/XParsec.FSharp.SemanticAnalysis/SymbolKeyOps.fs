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
/// pre-freeze inline-body channel) without a sibling interface + runtime cast.
[<RequireQualifiedAccess>]
module SymbolKeyOps =

    // --- Shared compiled-name string rules -------------------------------------------
    //
    // One definition each of the string rules identity minting / decomposition repeats:
    // strip the `` `N `` arity suffix, arity-qualify a simple name, segment a dotted
    // namespace. Everything below routes through these; `LocalSymbolKey` (internal,
    // compiles later) delegates its `arityName` / `asmOf` here so the rules can't drift.

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

    /// The home-assembly `option` from an assembly *name* (`PassContext.AssemblyName`):
    /// `""` (the front-end-only / contract-scrape default) ⇒ `None`; a real name ⇒
    /// `Some`. The single home for the rule — `LocalSymbolKey.ofType` and the
    /// registration sites call it directly.
    let asmOf (assemblyName: string) : string option =
        if assemblyName = "" then None else Some assemblyName

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

    /// A `NamespaceKey` from the boundary spelling: a home-assembly `option` (as the
    /// providers and `PassContext` carry it) + a dotted namespace.
    let namespaceKey (asm: string option) (dottedNs: string) : NamespaceKey =
        {
            Origin = Origin.OfOption asm
            Path = nsPath dottedNs
        }

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

    /// The home assembly of a type, reached through its holder chain.
    let typeAsm (t: TypeKey) : string option = t.Origin.AsmOption

    /// The full metadata/reflection name of a type — the string
    /// `MetadataSymbols.resolveTypeLocked` hands to `asm.GetType`, and the key every
    /// provider store face is addressed by. `Ns.Outer`2+Inner` for a nested type.
    /// THE renderer; there is exactly one.
    let typeMetaName (t: TypeKey) : string =
        let ns = typeNs t
        let simple = typeNestedName t
        if ns = "" then simple else ns + "." + simple

    /// Mint a `TypeKey` from the boundary spelling `(asm, dotted ns, simple name)`,
    /// where `name` may carry the `+`-mangled nested chain a reflection display name
    /// produces. THE parser; there is exactly one. A module-held type is minted by
    /// `moduleTypeKey` (the parser cannot produce one: a `+` chain is CLR nesting, and
    /// a `.` prefix is the namespace).
    let typeKeyOf (asm: string option) (dottedNs: string) (name: string) : TypeKey =
        let ns = namespaceKey asm dottedNs

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
    /// inner name. This is exactly what the old `arityName simple arity` did when `simple`
    /// was the `+`-mangled string.
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

    /// A module from its full dotted name (`Vesper.Collections` ⇒ module `Collections`
    /// in namespace `Vesper`): the LAST segment is the module, the rest the namespace.
    /// This is the convention every producer of the old flat `ValueKey.ns` already
    /// obeyed — `SymbolProviders.qualifiedValueName` built exactly `Namespace.Holder`,
    /// and `ClrProvider` handed that string to `EmitExternalCall` as the module-as-type
    /// full name. `moduleFullName` inverts it exactly.
    let moduleKeyOf (asm: string option) (fullName: string) : ModuleKey =
        let ns, name = splitLastDot fullName

        {
            Holder = ModuleHolder.InNamespace(namespaceKey asm ns)
            Name = name
        }

    // --- Smart constructors mirroring the old tuple shapes ---------------------------

    /// `SymbolKey.Type` from the boundary triple — the mechanical successor to the old
    /// `SymbolKey.TypeKey(asm, ns, name)`.
    let typeKey (asm: string option) (ns: string) (name: string) : SymbolKey = SymbolKey.Type(typeKeyOf asm ns name)

    /// The full dotted name of whatever holds a binding: the namespace for an
    /// unqualified one, the module's full name otherwise. The inverse of `bindingKeyOf`'s
    /// second argument.
    let holderFullName (h: ModuleHolder) : string =
        match h with
        | ModuleHolder.InNamespace ns -> ns.Dotted
        | ModuleHolder.InModule m -> moduleFullName m

    /// A `BindingKey` from `(asm, declaring-module full name, simple name)`. An EMPTY
    /// module name mints the UNQUALIFIED binding (`Decl = InNamespace <global of asm>`) —
    /// the flat-package / global extern. That is a real holder, not a sentinel: it still
    /// carries the home assembly, which `Decl = ValueNone` could not.
    let bindingKeyOf (asm: string option) (declModule: string) (name: string) : BindingKey =
        {
            Decl =
                if declModule = "" then
                    ModuleHolder.InNamespace(namespaceKey asm "")
                else
                    ModuleHolder.InModule(moduleKeyOf asm declModule)
            Name = name
        }

    /// `SymbolKey.Binding` from the boundary triple — the mechanical successor to the
    /// old `SymbolKey.ValueKey(asm, ns, name)`.
    let valueKey (asm: string option) (declModule: string) (name: string) : SymbolKey =
        SymbolKey.Binding(bindingKeyOf asm declModule name)

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
    // with the well-known runtime singletons, so they live here next to `valueKeyOf`.
    // `RuntimeNames` (which compiles after this file) keeps only the singleton
    // constants + recognisers and routes its `bareName` / `qualifiedName` needs here.

    /// The home assembly `option` carried by a key, reached through its holder chain.
    /// For a nominal type this is the type's declaring assembly — invariant per type.
    /// Codegen branches local-vs-external on whether it equals the assembly being
    /// emitted.
    let keyAsm (k: SymbolKey) : string option = k.Origin.AsmOption

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
    /// mangling). It is NOT a registry lookup key — the `TypeRegistry` tables are
    /// arity-keyed and withdraw the bare alias for an overloaded name (`Foo`2`/
    /// `Foo`3`), so a `simpleName`-keyed lookup silently MISSES an overloaded type
    /// and mis-classifies it as external. To resolve a key against a registry use the
    /// `*ByKey` helpers (`tryClassByKey` / `tryUnionByKey` / `tryRecordByKey` /
    /// `tryInterfaceImplHostByKey`), which read the arity-qualified name verbatim.
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
    // EQUAL. Identity is the containment chain rooted at the type's **home assembly** —
    // invariant per type, so an external type's `origin.Assembly` and a self-host local
    // key's `PassContext.AssemblyName` agree. The codegen local/external branch reads
    // the root `Origin` to decide `TypeDef` vs `TypeRef`.

    /// Mint a nominal type key from a fully-qualified compiled name with an explicit
    /// home assembly (`asm`) but no `SymbolOrigin` in hand: the last `.` segment is the
    /// simple name. Produces the same containment `externalTypeKey` does, so a type
    /// minted either way compares equal.
    let qualifiedTypeKeyOfT (asm: string option) (compiled: string) (arity: int) : TypeKey =
        let ns, simple = splitLastDot compiled
        withArity arity (typeKeyOf asm ns simple)

    let qualifiedTypeKeyOf (asm: string option) (compiled: string) (arity: int) : SymbolKey =
        SymbolKey.Type(qualifiedTypeKeyOfT asm compiled arity)

    /// Mint a nominal type key for an external type from its resolved shape's
    /// `origin` (home assembly + namespace) + the matched compiled name + arity. EVERY
    /// external-type producer (`Translate`, `InferResolve`, the VesperLib extractor's
    /// `mkNominal`) routes through this so the same type carries the same home assembly
    /// across all of them.
    ///
    /// The namespace comes from `compiled` itself whenever `compiled` is qualified; the
    /// `origin` supplies the HOME ASSEMBLY and (only for a bare `compiled`) the
    /// namespace. This is the same convention `VesperLib.TypeTranslate` already mints
    /// under (`qualifiedTypeKeyOf (homeOf origin.Assembly) compiled …`), so the two
    /// producers now agree by construction.
    ///
    /// It is also what retires `ExternalSymbolProviders.originNsFor`: that helper existed
    /// SOLELY because the old mint took the namespace from a package-BLANKET origin
    /// (`Vesper`) and stripped it off the compiled name, which mis-cut
    /// `Vesper.Collections.seq` into `ns = "Vesper"` / `name = "Collections.seq"` and broke
    /// capability-key matching. Taking the namespace from the name it belongs to cannot
    /// mis-cut.
    let externalTypeKeyOf (origin: SymbolOrigin) (compiled: string) (arity: int) : TypeKey =
        if compiled.IndexOf '.' >= 0 then
            qualifiedTypeKeyOfT origin.Assembly compiled arity
        else
            withArity arity (typeKeyOf origin.Assembly origin.Namespace.Dotted compiled)

    let externalTypeKey (origin: SymbolOrigin) (compiled: string) (arity: int) : SymbolKey =
        SymbolKey.Type(externalTypeKeyOf origin compiled arity)

    /// Mint a `SymbolKey.Binding` from an assembly + fully-qualified compiled name: the
    /// last `.` segment is the simple name, the prefix the DECLARING MODULE's full name
    /// (`Vesper.Unchecked.defaultof` ⇒ module `Vesper.Unchecked`, binding `defaultof`).
    /// For a bare `printfn` (no `.`) the binding is unqualified — `Decl` is the
    /// `ModuleHolder.InNamespace` of the assembly's global namespace, which still carries
    /// the home assembly.
    /// Used by `mono`/`poly`/`polyWith` to default the symbol's `Key`; `stack`'s
    /// `stampSymbol` re-mints the key with the wrapping package's assembly once it
    /// stamps the origin.
    let valueKeyOf (asm: string option) (compiled: string) : SymbolKey =
        let declModule, name = splitLastDot compiled
        valueKey asm declModule name

    /// The contract-sourced canon key for an intrinsic the VesperLib extractor
    /// publishes: `asm = None` (an intrinsic's home is target-dependent, so its identity
    /// is asm-blind — the `sameTypeAsmBlind` convention), the namespace taken from the
    /// qualified `compiled` name (a `namespace Vesper` prim-type ⇒ `"Vesper"`; a
    /// `global`/flat-package extern ⇒ the global namespace), and the VERBATIM short name
    /// kept intact — NO arity suffix, so the array keeps its backtick `` ``[]`` ``
    /// spelling and `simpleName` recovers the bare codegen/repr key unchanged. Pairs the
    /// contract's own namespace with the identity name, replacing the deleted front-end
    /// name-set classifier (`RuntimeNames.intrinsicKey`) at the producer mint.
    let intrinsicCanonKey (compiled: string) (shortName: string) : SymbolKey =
        typeKey None (fst (splitLastDot compiled)) shortName

    /// `qualifiedTypeKeyOf` with no home assembly — the asm-blind paths (codegen
    /// self-type signatures projected by name; test-helper constructors; the
    /// MetadataSymbols/contract scrapes that have only a compiled name).
    let qualifiedTypeKey (compiled: string) (arity: int) : SymbolKey = qualifiedTypeKeyOf None compiled arity

    /// The store-face LOOKUP key for a fully-qualified COMPILED name held as a
    /// string (a platform repr, a fixed printf-sink name, a codegen bridge name).
    /// The single home for the mint's two invariants: arity 0 is lossless because
    /// a compiled generic name already carries its `` `N `` suffix (`arityName` is
    /// a no-op on a suffixed name), and the key is asm-blind BY DESIGN — store
    /// lookup is addressed by `(ns, arity-name)` and never consults the origin at all
    /// (`qualifiedName`, which every store face projects through, discards it), so
    /// an asm-blind key answers exactly the entries an asm-carrying one does. Use
    /// this, not `qualifiedTypeKey <name> 0`, wherever a bare compiled-name string
    /// must reach the key-addressed store face.
    let lookupKeyOfCompiledName (compiled: string) : SymbolKey = qualifiedTypeKeyOf None compiled 0
