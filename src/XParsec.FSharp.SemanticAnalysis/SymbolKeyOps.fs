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
    /// name component — the bare name the front-end `TypeRegistry` (`tryRecord` /
    /// `tryClass` / `tryUnion`) keys on.
    let simpleName (k: SymbolKey) : string =
        let n =
            match k with
            | SymbolKey.TypeKey(_, _, n)
            | SymbolKey.ValueKey(_, _, n) -> n
            | SymbolKey.MemberKey(_, n, _, _) -> n

        bareName n

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

    /// `qualifiedTypeKeyOf` with no home assembly — the asm-blind paths (codegen
    /// self-type signatures projected by name; test-helper constructors; the
    /// MetadataSymbols/contract scrapes that have only a compiled name).
    let qualifiedTypeKey (compiled: string) (arity: int) : SymbolKey = qualifiedTypeKeyOf None compiled arity
