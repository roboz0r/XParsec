namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open System.IO
open System.Text

type QueryId =
    | Freeze = 0
    | Signature = 1
    | Lex = 2

/// The hash of a query's inputs; this type never computes it. Lowercase hex, so the value
/// doubles as a filesystem-safe path segment needing no escaping.
[<Struct>]
type InputHash =
    {
        Hex: string
    }

    override this.ToString() = this.Hex

module InputHash =

    let ofHex (hex: string) : InputHash = { Hex = hex.ToLowerInvariant() }

    let ofBytes (bytes: byte[]) : InputHash =
        let sb = StringBuilder(bytes.Length * 2)

        for b in bytes do
            sb.Append(b.ToString("x2")) |> ignore

        { Hex = sb.ToString() }

type CacheKey =
    {
        Query: QueryId
        CodeVersion: int
        Input: InputHash
    }

type ICacheStore =
    abstract member TryLoad: CacheKey -> byte[] voption
    abstract member Store: CacheKey -> byte[] -> unit

module Cache =

    /// Folded into every key: bumping it makes every blob a prior compiler wrote unreachable. A
    /// change to the ENCODING counts as much as one to the computed value, because an input
    /// hash says what a blob came from, never how it was written, so a reordered wire field
    /// misparses.
    [<Literal>]
    let CodeVersion = 32

    type InMemoryStore() =
        let map = Dictionary<CacheKey, byte[]>()

        interface ICacheStore with
            member _.TryLoad(key) =
                match map.TryGetValue key with
                | true, bytes -> ValueSome bytes
                | false, _ -> ValueNone

            member _.Store key bytes = map[key] <- bytes

    /// Lays a key out as `<root>/<query>/<codeVersion>/<hexInputHash>.bin`. Every segment
    /// comes from the key and is filesystem-safe unescaped.
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
