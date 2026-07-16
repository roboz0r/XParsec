namespace XParsec.FSharp.SemanticAnalysis

/// The generic `SymbolKey` ↔ compiled-name string algebra: the metadata-name
/// renderer/parser pair, namespace segmentation, key projection and minting. These
/// have nothing to do with the well-known runtime singletons (`RuntimeNames`) or with
/// external-symbol resolution (`module ExternalSymbols`); they operate purely on
/// `SymbolKey` / strings, so they live here — the one foundational home — right
/// after `SemanticInfo` defines `SymbolKey`.
///
/// TWO AXES, and the whole file turns on keeping them apart:
///   * the IDENTITY axis — a `SymbolKey`. A `TypeKey` is `(holder, plain name, arity: int)`.
///     Nothing here ever packs an arity into a key's `Name`.
///   * the NAME axis — the CLR metadata / reflection spelling (`` List`1 ``,
///     `` Outer`1+Inner`1 ``), plus the compiled-name-addressed provider stores that
///     genuinely key on it. The `` `N `` is a CLR convention and lives ONLY here.
/// `typeSegmentName` / `typeMetaName` render an identity onto the name axis;
/// `typeKeyOfSegment` / `typeKeyOf` parse one back. They are inverses on the
/// `InNamespace` / `InType` SUBLATTICE, and only there: the renderer is total (it spells
/// an `InModule` holder too — `N.MModule+T`), but the parser cannot be. A `+` chain in a
/// bare metadata string is CLR nesting and nothing in the string says "module", so
/// `typeKeyOf` mints `InType`. That is right for its whole caller population, which is
/// bare IL — IL has no modules. An `InModule` key is minted ONLY from Vesper metadata
/// (the `.fsi` contract extractor, or local registration off the source containment),
/// where the module chain is a fact the producer HOLDS; it is never parsed out of a name.
///
/// This module deliberately precedes both `RuntimeNames` (which routes its
/// `bareName` recognition through here) and `ExternalSymbols` (whose provider
/// surface and `ExternalSymbol` builders consume these projections). Keeping the
/// algebra below the `IExternalSymbolProvider` interface is what lets that
/// interface be compiled *after* `Tast`, so it can name `TDecl` directly (the
/// inline-body channel) without a sibling interface + runtime cast.
[<RequireQualifiedAccess>]
module SymbolKeyOps =

    // --- The CLR metadata-name string rules ------------------------------------------
    //
    // The `` `N `` suffix is a CLR METADATA convention, NOT part of a nominal identity —
    // a `TypeKey` carries its arity as an INT. These are therefore *rendering* rules for
    // the metadata/reflection name axis (`typeMetaName`, the by-name provider probes, the
    // emitted `TypeDef` name), and the parse rule that recovers the int from a name that
    // arrives already-mangled. Nothing packs an arity into a key's `Name`.

    /// Strip a trailing `` `N `` generic-arity suffix from a COMPILED name string, giving
    /// the bare name. For the name axis only (a provider store keyed by compiled name may
    /// hold either spelling); a `SymbolKey`'s `Name` never carries a suffix to strip.
    let bareName (name: string) : string =
        let tick = name.IndexOf '`'
        if tick < 0 then name else name.Substring(0, tick)

    /// True iff `name` is an F#-BACKTICK-ESCAPED identifier (`` ``[]`` ``). Such a name
    /// cannot take a `` `N `` suffix — the backticks are the escape, so a suffix would be
    /// unreadable — which is why the array's contract/store spelling is the bare escaped
    /// string at every layer (`RuntimeNames.arrayContractName`), never `` ``[]```1 ``. It is
    /// therefore held at `TyparArity = 0` and rendered verbatim, and `arityName` / `parseArity`
    /// agree on that, which is what makes them exact inverses.
    let private isEscapedName (name: string) = name.Contains '`'

    /// Render `(name, arity)` as the CLR metadata spelling (`List` + 1 ⇒ `` List`1 ``);
    /// unchanged for a non-generic type (arity ≤ 0) or an escaped name (`isEscapedName`).
    /// THE one definition of the rule, shared by the key renderer (`typeSegmentName`), the
    /// emitted `TypeDef` name, and the by-name provider probes — so the name a type is
    /// stored under and the name it is looked up by cannot drift.
    ///
    /// The NAME axis, not the identity axis: a `SymbolKey`'s `Name` never carries an arity
    /// (it is an `int` field), so this must never be used to build one. It exists for the
    /// stores and metadata rows that are compiled-name-addressed by design.
    let arityName (name: string) (arity: int) : string =
        if arity > 0 && not (isEscapedName name) then
            sprintf "%s`%d" name arity
        else
            name

    /// The inverse of `arityName` on ONE metadata name segment: split a trailing
    /// `` `N `` into `(bare name, N)`; `(name, 0)` when there is none. The suffix is
    /// recognised only when every character after the backtick is a DIGIT and the name is
    /// not backtick-ESCAPED — so the array's `` ``[]`` `` parses as `("``[]``", 0)`, never
    /// as a mangled generic.
    let private parseArity (segment: string) : struct (string * int) =
        let tick = segment.LastIndexOf '`'

        if tick <= 0 || tick = segment.Length - 1 || segment.[tick - 1] = '`' then
            struct (segment, 0)
        else
            let mutable n = 0
            let mutable ok = true

            for i in tick + 1 .. segment.Length - 1 do
                let c = segment.[i]

                if c >= '0' && c <= '9' then
                    n <- n * 10 + int c - int '0'
                else
                    ok <- false

            if ok then
                struct (segment.Substring(0, tick), n)
            else
                struct (segment, 0)

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

    /// The CLR metadata spelling of ONE segment of a type key — its own name plus its own
    /// `` `N `` (`` List`1 ``). This is what a `TypeDef` / `TypeRef` row's name column
    /// carries, and the per-segment half of `typeNestedName`. THE renderer of the arity;
    /// `typeKeyOfSegment` is its exact inverse.
    let typeSegmentName (t: TypeKey) : string = arityName t.Name t.TyparArity

    /// Mint one segment of a type key from its CLR metadata name under `holder` — the
    /// parse half of `typeSegmentName`, for a producer that meets the metadata name one
    /// segment at a time (the reflection walker's `Type.DeclaringType` chain).
    let typeKeyOfSegment (holder: TypeHolder) (metaName: string) : TypeKey =
        let struct (bare, arity) = parseArity metaName

        {
            Holder = holder
            Name = bare
            TyparArity = arity
        }

    /// The `+`-joined chain of a module's COMPILED HOLDER-CLASS names, WITHOUT the
    /// namespace (`A+B` for `module A` ⊃ `module B`). A module compiles to a static class
    /// and a nested module to a class nested in it, so on the NAME axis a module chain is
    /// a nested-class chain — the same `+` a `TypeHolder.InType` chain renders.
    ///
    /// NOT `moduleFullName`, which `.`-joins AND prefixes the namespace: that renders a
    /// module as a QUALIFIED name (the JS import path, an `EmitExternalCall` target),
    /// whereas this renders it as the NESTING PREFIX of something it holds. The two must
    /// not be conflated — `.`-joining here is exactly the flattening that made
    /// `typeMetaName` non-injective (`N.A.T` and `N.B.T` both rendering `N.T`).
    let rec moduleNestedName (m: ModuleKey) : string =
        match m.Holder with
        | ModuleHolder.InNamespace _ -> m.Name
        | ModuleHolder.InModule parent -> moduleNestedName parent + "+" + m.Name

    /// The `+`-joined nested chain WITHOUT the namespace (`` List`1+Enumerator ``); the
    /// rendered `Name` for a top-level type. The simple-name half of `typeMetaName`.
    /// EACH segment renders its OWN arity (the CLR rule), so a generic type nested in a
    /// generic type spells both (`` Outer`1+Inner`1 ``).
    ///
    /// EXHAUSTIVE by design — a type held by a `module` is a class nested in that module's
    /// holder class, so it renders `+` exactly as a CLR-nested type does (`N.MModule+T`).
    /// That is the CLR truth, the string `asm.GetType` binds, and what makes this renderer
    /// INJECTIVE. Adding a `TypeHolder` case must break the build here rather than fall
    /// into a wildcard that silently drops the holder.
    let rec typeNestedName (t: TypeKey) : string =
        let self = typeSegmentName t

        match t.Holder with
        | TypeHolder.InNamespace _ -> self
        | TypeHolder.InModule m -> moduleNestedName m + "+" + self
        | TypeHolder.InType outer -> typeNestedName outer + "+" + self

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
    /// `name` may carry the `+`-mangled nested chain AND the `` `N `` suffixes a
    /// reflection display name produces — each segment's suffix is PARSED into that
    /// segment's `TyparArity` (`parseArity`), so the key round-trips exactly through
    /// `typeMetaName`. THE parser; there is exactly one.
    ///
    /// It mints `InNamespace` / `InType` and NOTHING else — a partial inverse, honestly:
    /// a bare metadata string cannot say whether a `+` segment is a class or a module's
    /// holder class, and its callers are the bare-IL population, which has no modules. An
    /// `InModule` key comes from a producer that HOLDS the module chain (the `.fsi`
    /// contract extractor, local registration), never from re-cutting a name.
    let typeKeyOf (dottedNs: string) (name: string) : TypeKey =
        let ns = TypeHolder.InNamespace(namespaceKey dottedNs)

        if name.IndexOf '+' < 0 then
            typeKeyOfSegment ns name
        else
            let parts = name.Split '+'
            let mutable k = typeKeyOfSegment ns parts.[0]

            for i in 1 .. parts.Length - 1 do
                k <- typeKeyOfSegment (TypeHolder.InType k) parts.[i]

            k

    /// Resolve a WRITTEN dotted type name against a compiled-name identity index whose keys
    /// are the canonical `typeMetaName` renderings, falling back through module CONTAINMENT
    /// for the one spelling that is NOT that rendering: the DOTTED source form of a
    /// module-held type (`Test.A.M.T` for `T` in `module M`), whose canonical key spells
    /// `Test.A.M+T`. On an exact-index miss, split the LAST dot — if the prefix names a
    /// module this unit declares (`moduleHolder`), the suffix is the type that module holds:
    /// mint the candidate key under that holder, render it, and re-look-it-up in the index,
    /// so the value returned is ALWAYS the registered one and a spelling that names nothing
    /// resolves to nothing.
    ///
    /// This is exactly F#'s name-resolution question — "what does `A.B.C` denote when `A.B`
    /// is a module?" — stated ONCE for both signature projectors into the provider surface:
    /// the `.fsi` contract extractor (`VesperLib.ExtractCtx.tryTypeKey`, index value
    /// `TypeKey`) and the frozen-impl projector (`FrozenSignature.typeShapeByName`, index
    /// value `SymbolKey`), which differ only in the index's value type (`'T`). A type NESTED
    /// IN A TYPE (`Outer.Inner`, `TypeHolder.InType`) is deliberately NOT reached: only
    /// module holders are indexed, matching both callers — the `InType` extension point when
    /// the corpus needs a written `Outer.Inner` cross-unit name.
    let tryDottedModuleHeld
        (exact: string -> 'T voption)
        (moduleHolder: string -> TypeHolder voption)
        (probe: string)
        : 'T voption =
        match exact probe with
        | ValueSome _ as hit -> hit
        | ValueNone ->
            let dot = probe.LastIndexOf '.'

            if dot <= 0 || dot = probe.Length - 1 then
                ValueNone
            else
                match moduleHolder (probe.Substring(0, dot)) with
                | ValueSome holder -> exact (typeMetaName (typeKeyOfSegment holder (probe.Substring(dot + 1))))
                | ValueNone -> ValueNone

    /// Mint a type key under a HOLDER from a BARE source name and its arity as an INT — for
    /// a producer that holds the containment AND the count (a declared typar list): the
    /// contract extractor's module chain, local registration. It must never pack the count
    /// into a string and have the parser dig it back out.
    ///
    /// An ESCAPED name (`` ``[]`` ``) is the one place the count does not survive the name
    /// axis: it can carry no `` `N `` (`arityName` / `parseArity` agree on that), so a
    /// producer that DOES hold the count must still hold the key at `TyparArity = 0` — otherwise
    /// the array's identity would differ between the producer that declared it (`type
    /// ``[]``<'T>`, arity 1) and every producer that meets it as a name (arity 0), and the
    /// two would not compare equal.
    let typeKeyOfHolder (holder: TypeHolder) (name: string) (arity: int) : TypeKey =
        {
            Holder = holder
            Name = name
            TyparArity = if isEscapedName name then 0 else arity
        }

    /// `typeKeyOfHolder` for a type declared directly in a namespace, from the boundary
    /// spelling `(dotted ns)`.
    let typeKeyOfArity (dottedNs: string) (name: string) (arity: int) : TypeKey =
        typeKeyOfHolder (TypeHolder.InNamespace(namespaceKey dottedNs)) name arity

    /// Supply an arity the compiled NAME did not spell, for the mints whose input is a
    /// metadata/contract name string plus a separately-known count. Applies to the
    /// INNERMOST segment, and only when no segment of the chain spelled one of its own: a
    /// name that spells its arities is authoritative (a nested `` List`1+Enumerator `` says
    /// `List` owns the typar and `Enumerator` owns none, while the caller's `arity` is the
    /// shape's TOTAL and would double-count it). An ESCAPED name spells no arity and can
    /// take none (`isEscapedName`), so it stays at 0 — which is what keeps a key minted
    /// here equal to the one minted from the same contract name with no arity in hand.
    let rec private spelledArity (t: TypeKey) : bool =
        t.TyparArity > 0
        || isEscapedName t.Name
        || (
            match t.Holder with
            | TypeHolder.InType outer -> spelledArity outer
            | _ -> false
        )

    let private withArity (arity: int) (t: TypeKey) : TypeKey =
        if arity > 0 && not (spelledArity t) then
            { t with TyparArity = arity }
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

    /// `SymbolKey.Type` from `(dotted ns, BARE name, arity)` — `typeKeyOfArity` boxed.
    let typeKeyArity (ns: string) (name: string) (arity: int) : SymbolKey =
        SymbolKey.Type(typeKeyOfArity ns name arity)

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
    /// construction: no consumer may re-check that a member's declarer is a type, because
    /// the type system already says so.
    let memberKeyOf
        (decl: TypeKey)
        (name: string)
        (argSig: EqArray<FrozenType>)
        (methodTyparArity: int)
        (kind: MemberKind)
        : MemberKey =
        {
            Decl = decl
            Name = name
            ArgSig = argSig
            MethodTyparArity = methodTyparArity
            Kind = kind
        }

    /// `SymbolKey.Member` over a declaring `TypeKey` — the widened `memberKeyOf`, for the
    /// IR positions that still carry the wide key.
    let memberKey
        (decl: TypeKey)
        (name: string)
        (argSig: EqArray<FrozenType>)
        (methodTyparArity: int)
        (kind: MemberKind)
        : SymbolKey =
        SymbolKey.Member(memberKeyOf decl name argSig methodTyparArity kind)

    /// Narrow a wide `SymbolKey` to the `MemberKey` a member position REQUIRES. Only for
    /// the IR seam: the provider's own entries (`ExternalMember.Key`) are `MemberKey` by
    /// construction, so a consumer holding one reads its fields directly and never comes
    /// here. The IR's `TExpr.ExternalMember` / `MethodCall` / `Disposal` payloads are still
    /// `SymbolKey` (member positions, not nominal heads — those now carry a `TypeKey`), so
    /// the narrowing is a real — if unreachable — runtime check; it is stated ONCE here rather than at each
    /// backend, which is what keeps the answer to "a non-member key in a member position"
    /// from differing per site.
    let asMemberKey (what: string) (k: SymbolKey) : MemberKey =
        match k with
        | SymbolKey.Member m -> m
        | other -> failwithf "%s: expected a MemberKey, got %A" what other

    /// The declaring TYPE of a member position's key — `asMemberKey` + `.Decl`, the shape
    /// every backend's "which type declares this member?" read takes.
    let declTypeKeyOf (what: string) (k: SymbolKey) : TypeKey = (asMemberKey what k).Decl

    // --- Generic `SymbolKey` projection ----------------------------------------------
    //
    // These operate on any `SymbolKey` (decompose / mint); they have nothing to do
    // with the well-known runtime singletons, so they live here next to the mints.
    // `RuntimeNames` (which compiles after this file) keeps only the singleton
    // constants + recognisers and routes its `bareName` / `qualifiedName` needs here.

    /// The key's `name` component with the containment dropped. It is the PLAIN SOURCE
    /// name — a key's `Name` never carries a `` `N `` (the arity is `TypeKey.TyparArity`, an
    /// int), so nothing is packed in it to strip. Use this where a name feeds a canonical
    /// string-keyed repr map (the platform-repr axis, the SRTP `primitiveSupports`) — a
    /// name axis that is string-keyed BY DESIGN. For a *comparison* against a well-known
    /// intrinsic prefer the `TyBool`/`TyUnit`/`TyArray`/… active patterns (identity match)
    /// over `intrinsicName key = "…"`.
    let intrinsicName (k: SymbolKey) : string =
        match k with
        | SymbolKey.Type t -> t.Name
        | SymbolKey.Binding b -> b.Name
        | SymbolKey.Member m -> m.Name

    /// The key's name for HUMAN DISPLAY: containment dropped, and — for a generic type —
    /// the arity along with it, since the count lives beside the name rather than in it.
    /// That makes this a LOSSY projection, and it must NEVER be a route back to a key, a
    /// lookup name, a canon name or an intrinsic repr: an arity-overloaded name
    /// (`Point<'a,'b>` / `Point<'a,'b,'c>`) does not resolve from it at all. To resolve a
    /// key against the registry use the `*ByKey` helpers (`tryClassByKey` /
    /// `tryUnionByKey` / `tryRecordByKey` / `tryInterfaceImplHostByKey`), which take the
    /// key itself; to reach an intrinsic's platform repr ask the key-addressed
    /// `IntrinsicReprKeys` / `IntrinsicForwardRepr`; to recognise a well-known intrinsic
    /// match the key (`IntrinsicTypePatterns`).
    ///
    /// The `DisplayName` wrapper is what makes that a COMPILE ERROR rather than a rule: no
    /// mint and no table accepts one, so the only way back to a string is an explicit
    /// `let (DisplayName s) = …`. The two consumers that legitimately unwrap are diagnostics
    /// / display and BACKEND NAME EMISSION (mangling an identifier the target actually
    /// emits, whose names carry no generic arity). The name-axis sites that genuinely need
    /// a string — a compiled-name-addressed store, a platform-repr map — take
    /// `intrinsicName`, which stays non-lossy.
    let simpleName (k: SymbolKey) : DisplayName = DisplayName(intrinsicName k)

    /// `simpleName` for a caller already holding the narrow `TypeKey` a nominal head
    /// carries — same projection, no widening detour.
    let typeSimpleName (t: TypeKey) : DisplayName = DisplayName t.Name

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
    let qualifiedTypeKeyOf (compiled: string) (arity: int) : TypeKey =
        let ns, simple = splitLastDot compiled
        withArity arity (typeKeyOf ns simple)

    /// `qualifiedTypeKeyOf` as a `SymbolKey`. THE mint for a fully-qualified compiled
    /// name held as a string — a platform repr, a fixed printf-sink name, a codegen
    /// bridge name, a metadata/contract scrape. Passing arity 0 for an already-suffixed
    /// generic name is lossless: the suffix is PARSED into `TyparArity`, so the key is the
    /// same one the caller would get by handing the bare name and the count.
    let qualifiedTypeKey (compiled: string) (arity: int) : SymbolKey =
        SymbolKey.Type(qualifiedTypeKeyOf compiled arity)

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
            qualifiedTypeKeyOf compiled arity
        else
            withArity arity (typeKeyOf origin.Namespace.Dotted compiled)

    let externalTypeKey (origin: SymbolOrigin) (compiled: string) (arity: int) : SymbolKey =
        SymbolKey.Type(externalTypeKeyOf origin compiled arity)

    /// The contract-sourced canon key for an intrinsic the VesperLib extractor
    /// publishes. An intrinsic is a nominal like any other, so its canon key is simply
    /// the key of its COMPILED name — which the extractor spells arity-suffixed
    /// (`` Vesper.Collections.seq`1 ``), so the `` `1 `` PARSES into `TyparArity` and the arity
    /// is in the key. Deliberately the same mint the use-site stamp takes
    /// (`TypeHeadStamp.useSiteTypeKey`'s `Intrinsic` arm) and the same one
    /// `TypeRegistry.IntrinsicKeys` stamps for a self-compiled intrinsic, so all three
    /// compare EQUAL by construction — no arity-blind matcher stands between them.
    /// (Arity 0 to `qualifiedTypeKeyOf`: the count is already spelled in `compiled`.)
    let intrinsicCanonKey (compiled: string) : TypeKey = qualifiedTypeKeyOf compiled 0
