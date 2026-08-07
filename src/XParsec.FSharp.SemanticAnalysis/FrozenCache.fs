namespace XParsec.FSharp.SemanticAnalysis

[<RequireQualifiedAccess>]
module FrozenCache =

    let freeze (store: ICacheStore) (key: CacheKey) (produce: unit -> FrozenPools) : FrozenPools =
        match store.TryLoad key with
        | ValueSome blob -> FrozenCodec.thaw (Compression.decompress blob)
        | ValueNone ->
            let f = produce ()
            store.Store key (Compression.compress (FrozenCodec.flatten f))
            f

    /// An errored compile is not a cacheable derivation, so `Error` stores nothing.
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
