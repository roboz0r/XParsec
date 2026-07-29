namespace XParsec.FSharp.SemanticAnalysis

/// A cache-wrapped freeze: the store side of the per-file compile cache, wired to `FrozenCodec`
/// and `Compression` but NOT to `Freeze` — the freeze is passed as a thunk, so this module is a
/// pure store adapter that neither runs the front end nor knows how a frozen tree is produced.
///
/// The module is deliberately QUERY-AGNOSTIC: which derivation this blob is (the `QueryId.Freeze`
/// tag), which compiler version produced it, and the input hash all live in the `CacheKey` the
/// caller assembles (`Query = QueryId.Freeze`, `CodeVersion = Cache.CodeVersion`, `Input` from
/// `Hashing.fileInputHash` over the compilation's `Hashing.compilationDigest`). This module only
/// maps that key through the store.
[<RequireQualifiedAccess>]
module FrozenCache =

    /// Return the frozen file for `key`, consulting `store` first.
    ///
    /// HIT (`TryLoad` yields the blob): decompress + thaw the cached blob and return it —
    /// `produce` is NOT run. MISS: run `produce`, then flatten + compress + store the result
    /// under `key` before returning it. Store side effect aside, the function is pure: a HIT and
    /// a MISS yield trees that emit byte-identical output (the flatten/thaw round-trip is
    /// codegen-invariant, and compression round-trips exactly), which is the cache's soundness
    /// contract.
    let freeze (store: ICacheStore) (key: CacheKey) (produce: unit -> FrozenPools) : FrozenPools =
        match store.TryLoad key with
        | ValueSome blob -> FrozenCodec.thaw (Compression.decompress blob)
        | ValueNone ->
            let f = produce ()
            store.Store key (Compression.compress (FrozenCodec.flatten f))
            f

    /// The error-aware `freeze`: for a front end that can FAIL (a parse error or an
    /// error-severity diagnostic), which `freeze`'s unconditional store must not commit — an
    /// errored compile is not a cacheable derivation, and storing it would serve a stale
    /// failure (or a partial tree) back on the next run.
    ///
    /// HIT: same as `freeze` — decompress + thaw, `produce` not run. A hit is only ever
    /// reachable if a PRIOR run stored under this key, and only an `Ok` ever stores, so a hit
    /// is always a successful frozen tree; it is returned as `Ok`. MISS: run `produce`; on
    /// `Ok f` flatten + compress + store `f` (the same store path as `freeze`) then return
    /// `Ok f`; on `Error e` store NOTHING and return `Error e`. The cache is thus sound in
    /// both directions — a hit re-emits byte-identically (the round-trip is codegen-invariant),
    /// and a failure never poisons the store.
    let freezeResult
        (store: ICacheStore)
        (key: CacheKey)
        (produce: unit -> Result<FrozenPools, 'e>)
        : Result<FrozenPools, 'e> =
        match store.TryLoad key with
        | ValueSome blob -> Ok(FrozenCodec.thaw (Compression.decompress blob))
        | ValueNone ->
            match produce () with
            | Ok f ->
                store.Store key (Compression.compress (FrozenCodec.flatten f))
                Ok f
            | Error e -> Error e
