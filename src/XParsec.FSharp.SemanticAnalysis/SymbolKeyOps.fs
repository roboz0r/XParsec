namespace XParsec.FSharp.SemanticAnalysis

/// The generic `SymbolKey` ↔ compiled-name string algebra: arity-strip /
/// arity-qualify, qualified-name split, key projection and minting. These have
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
    // One definition each of the three string rules identity minting / decomposition
    // repeats: strip the `` `N `` arity suffix,
    // arity-qualify a simple name, split a qualified name at its last `.`. Everything
    // below routes through these; `LocalSymbolKey` (internal, compiles later) delegates
    // its `arityName` / `asmOf` here so the rules can't drift.

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

    /// Split a fully-qualified compiled name at its last `.` into `(ns, simpleName)`:
    /// `Vesper.Option` ⇒ `("Vesper", "Option")`; a name with no `.` ⇒ `("", name)`.
    let private splitQualified (compiled: string) : string * string =
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
    let shortName (compiled: string) : string =
        bareName (snd (splitQualified compiled))

    /// Mint a `SymbolKey.ValueKey` from an assembly + fully-qualified compiled
    /// name by splitting at the last `.`: everything before becomes the `ns`
    /// (module path), the last segment the simple `name`. For a bare `printfn`
    /// (no `.`) the `ns` is empty. Used by `mono`/`poly`/`polyWith` to default the
    /// symbol's `Key`; `stack`'s `stampSymbol` re-mints the key with the wrapping
    /// package's assembly once it stamps the origin.
    let valueKeyOf (asm: string option) (compiled: string) : SymbolKey =
        let ns, name = splitQualified compiled
        SymbolKey.ValueKey(asm, ns, name)

    // --- Generic `SymbolKey` ↔ string projection + minting --------------------------
    //
    // These operate on any `SymbolKey` (decompose / mint); they have nothing to do
    // with the well-known runtime singletons, so they live here next to `valueKeyOf`.
    // `RuntimeNames` (which compiles after this file) keeps only the singleton
    // constants + recognisers and routes its `bareName` / `qualifiedName` needs here.

    /// The home assembly `option` carried by a key. For a nominal `TypeKey` this
    /// is the type's declaring assembly — `Some <home>`, invariant per type.
    /// Codegen branches local-vs-external on
    /// whether it equals the assembly being emitted.
    let rec keyAsm (k: SymbolKey) : string option =
        match k with
        | SymbolKey.TypeKey(asm, _, _)
        | SymbolKey.ValueKey(asm, _, _) -> asm
        | SymbolKey.MemberKey(decl, _, _, _) -> keyAsm decl

    /// The bare simple name (namespace dropped, arity suffix stripped) of a key's
    /// name component. Dropping the arity is LOSSY, so this is for uses where the
    /// arity is genuinely not part of the identity: human-facing diagnostics, and
    /// backends whose names carry no generic arity (the JS emitter, CLR member-name
    /// mangling). It is NOT a registry lookup key — the `TypeRegistry` tables are
    /// arity-keyed and withdraw the bare alias for an overloaded name (`Foo`2`/
    /// `Foo`3`), so a `simpleName`-keyed lookup silently MISSES an overloaded type
    /// and mis-classifies it as external. To resolve a key against a registry use the
    /// `*ByKey` helpers (`tryClassByKey` / `tryUnionByKey` / `tryRecordByKey` /
    /// `tryInterfaceImplHostByKey`), which read the arity-qualified name verbatim.
    let simpleName (k: SymbolKey) : string =
        let n =
            match k with
            | SymbolKey.TypeKey(_, _, n)
            | SymbolKey.ValueKey(_, _, n) -> n
            | SymbolKey.MemberKey(_, n, _, _) -> n

        bareName n

    /// The key's `name` component with the namespace dropped but the `` `N `` arity
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
        | SymbolKey.TypeKey(_, _, n)
        | SymbolKey.ValueKey(_, _, n) -> n
        | SymbolKey.MemberKey(_, n, _, _) -> n

    /// The fully-qualified compiled name for an EXTERNAL nominal lookup
    /// (`externalUnionRef` / `externalRecordRef` / `externalClassRef`): `ns.name`
    /// with the arity suffix retained. The lookups normalise bare-vs-suffixed
    /// internally, so passing the arity-qualified form is safe for both the
    /// metadata layer (suffixed keys) and the contract layer (bare keys).
    let qualifiedName (k: SymbolKey) : string =
        match k with
        | SymbolKey.TypeKey(_, ns, n)
        | SymbolKey.ValueKey(_, ns, n) -> if ns = "" then n else ns + "." + n
        | SymbolKey.MemberKey(_, n, _, _) -> n

    // A nominal `SemType`'s `SymbolKey` participates in unification equality, so the
    // SAME type minted via different paths (use-site resolution, VesperLib contract
    // extraction, the `*Key` runtime constants, local registration) must compare
    // EQUAL. Identity is `(asm, ns, name)` where `asm` is the type's **home
    // assembly** (Phase 6) — invariant per type, so an external type's
    // `origin.Assembly` and a self-host local key's `PassContext.AssemblyName`
    // agree. The codegen local/external branch reads `asm` directly to decide
    // `TypeDef` vs `TypeRef`.

    /// Mint a nominal `TypeKey` for an external type from its resolved shape's
    /// `origin` (home assembly + namespace split) + the matched compiled name +
    /// arity. EVERY external-type producer (`Translate`, `InferResolve`, the
    /// VesperLib extractor's `mkNominal`) routes through this so the same type
    /// carries the same home assembly across all of them (Phase 6).
    let externalTypeKey (origin: SymbolOrigin) (compiled: string) (arity: int) : SymbolKey =
        let simple = SymbolOrigin.StripNamespace origin.Namespace compiled
        SymbolKey.TypeKey(origin.Assembly, origin.Namespace, arityName simple arity)

    /// Mint a nominal `TypeKey` from a fully-qualified compiled name with an
    /// explicit home assembly (`asm`) but no `SymbolOrigin` in hand: split off the
    /// last `.` segment as the simple name. Produces the same `(asm, ns, name)`
    /// split `externalTypeKey` does, so a type minted either way compares equal.
    let qualifiedTypeKeyOf (asm: string option) (compiled: string) (arity: int) : SymbolKey =
        let ns, simple = splitQualified compiled
        SymbolKey.TypeKey(asm, ns, arityName simple arity)

    /// The contract-sourced canon `SymbolKey` for an intrinsic the VesperLib extractor
    /// publishes: `asm = None` (an intrinsic's home is target-dependent, so its identity
    /// is asm-blind — the `sameTypeAsmBlind` convention), the namespace taken from the
    /// qualified `compiled` name (a `namespace Vesper` prim-type ⇒ `"Vesper"`; a
    /// `global`/flat-package extern ⇒ `""`), and the VERBATIM short name kept intact — NO
    /// arity suffix, so the array keeps its backtick `` ``[]`` `` spelling and
    /// `simpleName` recovers the bare codegen/repr key unchanged. Pairs the contract's own
    /// namespace with the identity name, replacing the deleted front-end name-set
    /// classifier (`RuntimeNames.intrinsicKey`) at the producer mint.
    let intrinsicCanonKey (compiled: string) (shortName: string) : SymbolKey =
        SymbolKey.TypeKey(None, fst (splitQualified compiled), shortName)

    /// `qualifiedTypeKeyOf` with no home assembly — the asm-blind paths (codegen
    /// self-type signatures projected by name; test-helper constructors; the
    /// MetadataSymbols/contract scrapes that have only a compiled name).
    let qualifiedTypeKey (compiled: string) (arity: int) : SymbolKey = qualifiedTypeKeyOf None compiled arity

    /// The store-face LOOKUP key for a fully-qualified COMPILED name held as a
    /// string (a platform repr, a fixed printf-sink name, a codegen bridge name).
    /// The single home for the mint's two invariants: arity 0 is lossless because
    /// a compiled generic name already carries its `` `N `` suffix (`arityName` is
    /// a no-op on a suffixed name), and the key is asm-blind BY DESIGN — store
    /// lookup is addressed by `(ns, arity-name)` and never consults `asm` at all
    /// (`qualifiedName`, which every store face projects through, discards it), so
    /// an asm-blind key answers exactly the entries an asm-carrying one does. Use
    /// this, not `qualifiedTypeKey <name> 0`, wherever a bare compiled-name string
    /// must reach the key-addressed store face.
    let lookupKeyOfCompiledName (compiled: string) : SymbolKey = qualifiedTypeKeyOf None compiled 0
