namespace XParsec.FSharp.SemanticAnalysis

/// A cache-wrapped freeze: the store side of the per-file compile cache, wired to `FrozenCodec`
/// and `Compression` but NOT to `Freeze` — the freeze is passed as a thunk, so this module is a
/// pure store adapter that neither runs the front end nor knows how a frozen tree is produced.
///
/// The module is deliberately QUERY-AGNOSTIC: which derivation this blob is (the `QueryId.Freeze`
/// tag), which compiler version produced it, and the input hash all live in the `CacheKey` the
/// caller assembles (`Query = QueryId.Freeze`, `CodeVersion = Cache.CodeVersion`, `Input` from
/// `Hashing.fileInputHash`). This module only maps that key through the store.
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
    let freeze (store: ICacheStore) (key: CacheKey) (produce: unit -> Frozen.TastFile) : Frozen.TastFile =
        match store.TryLoad key with
        | ValueSome blob -> FrozenCodec.thaw (Compression.decompress blob)
        | ValueNone ->
            let f = produce ()
            store.Store key (Compression.compress (FrozenCodec.flatten f))
            f
