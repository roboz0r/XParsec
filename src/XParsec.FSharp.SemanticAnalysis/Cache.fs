namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open System.IO
open System.Text

/// Names the derivation a cached blob is the output of. An enum so it folds into cache-key
/// equality and into a filesystem path segment for free. `Signature`/`Lex` are declared ahead
/// of their producers because the store is agnostic to which derivation writes it.
type QueryId =
    | Freeze = 0
    | Signature = 1
    | Lex = 2

/// The hash of a query's inputs (its source folded with dependency-artifact hashes), TRANSPORTED
/// as part of a cache key — this type never computes it. Held as lowercase hex so the value is
/// structurally equatable (usable as a dictionary key) and is itself a filesystem-safe path
/// segment, needing no escaping when it addresses a blob on disk.
[<Struct>]
type InputHash =
    {
        Hex: string
    }

    override this.ToString() = this.Hex

module InputHash =

    /// Normalise a hex string to the store's canonical lowercase form, so two spellings of the
    /// same hash address the same blob.
    let ofHex (hex: string) : InputHash = { Hex = hex.ToLowerInvariant() }

    /// Render raw hash bytes as the lowercase-hex value the store keys on.
    let ofBytes (bytes: byte[]) : InputHash =
        let sb = StringBuilder(bytes.Length * 2)

        for b in bytes do
            sb.Append(b.ToString("x2")) |> ignore

        { Hex = sb.ToString() }

/// The content address of a cached blob. Named fields — never a wide tuple — so each guard is
/// explicit: which derivation produced the blob, which compiler version produced it (poison
/// guard), and the hash of the inputs it was derived from.
type CacheKey =
    {
        Query: QueryId
        CodeVersion: int
        Input: InputHash
    }

/// A content-addressed byte store: a blob is retrieved only by the exact key it was stored under.
/// A miss (a key never stored) yields `ValueNone`; there is no listing or eviction — the cache is
/// dumb by design at this stage.
type ICacheStore =
    abstract member TryLoad: CacheKey -> byte[] voption
    abstract member Store: CacheKey -> byte[] -> unit

module Cache =

    /// Bumped whenever a change to a cached derivation must invalidate every blob it has ever
    /// produced — a fix to `freeze` cannot be allowed to read back its own stale output. One
    /// definition site, folded into every key so a blob from a prior compiler is unreachable.
    ///
    /// A change to the WIRE SHAPE counts, not just to what `freeze` computes: the input hash
    /// says what a blob was derived from, never how it was encoded, so a `FrozenCodec` field
    /// that changes order or representation lands a stale blob at the same
    /// `FileSystemStore` path and misparses. Bump on any edit to `writePools`/`readPools`.
    [<Literal>]
    let CodeVersion = 8

    /// A process-lifetime store backed by a dictionary. `CacheKey` is a record, so its structural
    /// equality keys the map directly with no custom comparer.
    type InMemoryStore() =
        let map = Dictionary<CacheKey, byte[]>()

        interface ICacheStore with
            member _.TryLoad(key) =
                match map.TryGetValue key with
                | true, bytes -> ValueSome bytes
                | false, _ -> ValueNone

            member _.Store key bytes = map[key] <- bytes

    /// A content-addressed store rooted at `root`, laid out as
    /// `<root>/<QueryId>/<codeVersion>/<hexInputHash>.bin`. Every segment is derived from the key
    /// and is filesystem-safe (the query is an enum case name, the hash is hex), so the key maps
    /// to a path with no escaping. A miss is a missing file, returned as `ValueNone`; storing
    /// creates the parent directories on demand.
    type FileSystemStore(root: string) =
        let pathOf (key: CacheKey) =
            Path.Combine(root, string key.Query, string key.CodeVersion, key.Input.Hex + ".bin")

        interface ICacheStore with
            member _.TryLoad(key) =
                let path = pathOf key

                if File.Exists path then
                    ValueSome(File.ReadAllBytes path)
                else
                    ValueNone

            member _.Store key bytes =
                let path = pathOf key
                Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
                File.WriteAllBytes(path, bytes)
