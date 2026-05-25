namespace XParsec.FSharp.SemanticAnalysis

open System.IO
open XParsec.Toml

/// Layer 1 of the symbol-resolution stack (symbol-resolution-plan §5): a
/// *referenced project*, declared by its `manifest.toml`. A package's `[core]`
/// table names the namespace and lists its contract `.fsi` files in compile
/// order; this module parses each into one accumulating `ExtractCtx` (reusing
/// `FSharpLib`'s extractor) and exposes the result as an `IExternalSymbolProvider`
/// whose symbols carry the package `Origin` (assembly simple name + namespace) —
/// the first real consumer of the P0 identity surface.
///
/// The `.fsi` is the *target-agnostic contract* (`type int = extern`); the
/// matching `.fs` is the *per-target binding* (`type int = (# "System.Int32" #)`).
/// Resolution needs only the `.fsi`; the absent `.fs` is a codegen-side concern
/// (symbol-resolution-plan §5.2/§5.3), not a resolution failure here.
module ReferencedProject =

    /// A parsed package `manifest.toml`'s `[core]` table. Mirrors the schema used
    /// across `src/Vesper.*` (and consumed today by the parser golden tests).
    type Manifest =
        {
            /// Package / assembly simple name — `[core] name` when present, else
            /// the manifest's directory name (`src/Vesper.Core` ⇒ `"Vesper.Core"`).
            /// `Vesper.Core`/`Vesper.Printf` omit `name`; the dir name is the
            /// package identity in both cases.
            Name: string
            /// `[core] namespace` — the namespace the package's symbols live in
            /// (and the implicit auto-open prefix for short-name resolution).
            Namespace: string
            /// Contract `.fsi` files in compile order (`[core] files`).
            Files: string list
            /// Target-binding `.fs` files (`[core] impl`) — the per-target bodies.
            /// Carried for the codegen layer (the missing-`.fs` gap, §5.3); not
            /// needed to resolve symbols.
            Impl: string list
        }

    let private asString (v: TomlValue) : string option =
        match v with
        | TomlValue.String s -> Some s
        | _ -> None

    let private asTable (v: TomlValue) : TomlTable option =
        match v with
        | TomlValue.Table t
        | TomlValue.InlineTable t -> Some t
        | _ -> None

    let private findString (t: TomlTable) (key: string) : string option =
        Map.tryFind key t |> Option.bind asString

    let private findStringList (t: TomlTable) (key: string) : string list option =
        match Map.tryFind key t with
        | Some(TomlValue.Array xs) -> xs |> List.choose asString |> Some
        | _ -> None

    /// Parse a package `manifest.toml` document. `dirName` is the manifest's
    /// directory name, used as the assembly name when `[core]` carries no `name`.
    let parseManifest (dirName: string) (doc: TomlDocument) : Result<Manifest, string> =
        match Map.tryFind "core" doc |> Option.bind asTable with
        | None -> Error "manifest.toml: missing [core] table"
        | Some core ->
            match findString core "namespace", findStringList core "files" with
            | None, _ -> Error "manifest.toml: [core] missing `namespace`"
            | _, None -> Error "manifest.toml: [core] missing `files = [...]`"
            | Some ns, Some files ->
                Ok
                    {
                        Name = findString core "name" |> Option.defaultValue dirName
                        Namespace = ns
                        Files = files
                        Impl = findStringList core "impl" |> Option.defaultValue []
                    }

    /// Read + parse the manifest at `manifestPath` (the path to a `manifest.toml`).
    let loadManifest (manifestPath: string) : Result<Manifest, string> =
        if not (File.Exists manifestPath) then
            Error(sprintf "Manifest not found: %s" manifestPath)
        else
            let dirName =
                Path.GetFileName(Path.TrimEndingDirectorySeparator(Path.GetDirectoryName manifestPath))

            match Toml.parse (File.ReadAllText manifestPath) with
            | Error e -> Error(sprintf "Manifest parse error (%s): %s" manifestPath e)
            | Ok doc -> parseManifest dirName doc

    /// Wrap the extractor's provider so (a) every resolved descriptor carries the
    /// package `Origin` (the extractor records `SymbolOrigin.Empty`; the manifest
    /// knows the assembly + namespace — symbol-resolution-plan §5.1), and (b) the
    /// package's implicit prelude — its `[<AutoOpen>]` modules plus the namespace
    /// itself — is surfaced as `IAmbientOpenScope`. Short-name resolution is *not*
    /// a provider-internal retry any more: the pipeline seeds these `ambient`
    /// prefixes into the open scope and probes them BEHIND explicit `open`s, so an
    /// explicit `open` can shadow a prelude name (symbol-resolution-handoff.md, open-resolution).
    let private wrap
        (origin: SymbolOrigin)
        (ambient: string list)
        (inner: IExternalSymbolProvider)
        : IExternalSymbolProvider =
        { new IExternalSymbolProvider with
            member _.TryLookup name =
                match inner.TryLookup name with
                | ValueSome s -> ValueSome { s with Origin = origin }
                | ValueNone -> ValueNone

            member _.TryLookupType name =
                match inner.TryLookupType name with
                // Only `Class` carries an origin slot; Abbrev/Record/Union don't.
                | ValueSome(ExternalTypeShape.Class(arity, isInterface, _)) ->
                    ValueSome(ExternalTypeShape.Class(arity, isInterface, origin))
                | other -> other

            member _.TryLookupMember(typeName, memberName) =
                match inner.TryLookupMember(typeName, memberName) with
                | ValueSome mem -> ValueSome { mem with Origin = origin }
                | ValueNone -> ValueNone

          interface IAmbientOpenScope with
              member _.AmbientOpenPrefixes = ambient
        }

    /// Stand up a referenced project (layer 1) from its `manifest.toml`: parse
    /// each contract `.fsi` in `files` order into one accumulating `ExtractCtx`,
    /// then expose it as a provider whose symbols carry the package `Origin`.
    /// Returns per-file parse diagnostics alongside the provider (a file that
    /// fails to parse contributes no symbols but does not abort the build).
    let buildProvider
        (manifestPath: string)
        : Result<IExternalSymbolProvider * (FSharpLib.LibFile * string) list, string> =
        match loadManifest manifestPath with
        | Error e -> Error e
        | Ok manifest ->
            let dir = Path.GetDirectoryName manifestPath
            let ctx = FSharpLib.ExtractCtx.empty ()

            for rel in manifest.Files do
                let file: FSharpLib.LibFile =
                    {
                        BucketName = manifest.Name
                        Relative = rel
                        Absolute = Path.Combine(dir, rel)
                    }

                match FSharpLib.parseFileFull file with
                | Error e -> ctx.Diagnostics.Add(file, e)
                | Ok parsed -> FSharpLib.extractSymbols ctx parsed

            let origin =
                {
                    Assembly = Some manifest.Name
                    Namespace = manifest.Namespace
                    DeclaringType = None
                }

            // The contract's implicit prelude: its `[<AutoOpen>]` modules (most
            // specific, e.g. `Vesper.ArithmeticOperators`) ahead of the package
            // namespace itself (`Vesper`, so `int` finds `Vesper.int`). Both are
            // probed behind explicit `open`s (symbol-resolution-handoff.md, open-resolution).
            let ambient =
                List.ofSeq ctx.AutoOpenPrefixes
                @ (if manifest.Namespace.Length > 0 then
                       [ manifest.Namespace ]
                   else
                       [])

            Ok(wrap origin ambient (FSharpLib.ExtractCtx.toProvider ctx), List.ofSeq ctx.Diagnostics)

    /// Lazy cache keyed by the (normalised) manifest path so repeated callers
    /// parse a package's `.fsi` set at most once. Mirrors `FSharpLib.defaultProvider`.
    let private cached =
        System.Collections.Concurrent.ConcurrentDictionary<
            string,
            Lazy<Result<IExternalSymbolProvider * (FSharpLib.LibFile * string) list, string>>
         >(
            System.StringComparer.Ordinal
        )

    /// Production-path entry point: caches `buildProvider` per manifest path.
    /// Tests that need a fresh provider should call `buildProvider`.
    let provider (manifestPath: string) : Result<IExternalSymbolProvider * (FSharpLib.LibFile * string) list, string> =
        let normalised = Path.GetFullPath manifestPath
        cached.GetOrAdd(normalised, (fun p -> lazy (buildProvider p))).Value
