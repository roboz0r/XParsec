namespace XParsec.FSharp.SemanticAnalysis

open System.IO
open XParsec.Toml
open XParsec.FSharp
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// Manifest loading + per-file parsing. Reads `<libRoot>/manifest.toml`
/// (the root) plus each bucket's `manifest.toml`, topo-sorts buckets by
/// `depends-on`, and returns a flat ordered list of files ready for the
/// extractor. The per-file `parseFile` / `parseFileFull` shims route `.fsi`
/// through the signature-file parser and the (rare) `.fs` through the
/// implementation parser.
module VesperLibManifest =

    type BucketEntry =
        {
            Name: string
            /// Directory name relative to the lib root.
            Path: string
            Description: string
            /// Direct dependencies (other bucket names). Transitive closure
            /// is computed by the consumer.
            DependsOn: string list
        }

    type RootManifest =
        {
            UpstreamCommit: string
            UpstreamTag: string
            UpstreamPath: string
            Buckets: BucketEntry list
        }

    /// One file resolved through the manifests. Order in a `LoadedLib`
    /// reflects the topological bucket order followed by each bucket's
    /// manifest's `files` array.
    type LibFile =
        {
            BucketName: string
            /// Relative path from the bucket directory (e.g. `"math/z.fsi"`).
            Relative: string
            Absolute: string
        }

    type LoadedLib =
        {
            Root: RootManifest
            Files: LibFile list
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

    let private asArray (v: TomlValue) : TomlValue list option =
        match v with
        | TomlValue.Array xs -> Some xs
        | _ -> None

    let private findString (t: TomlTable) (key: string) : string option =
        Map.tryFind key t |> Option.bind asString

    let private findStringList (t: TomlTable) (key: string) : string list option =
        match Map.tryFind key t with
        | Some(TomlValue.Array xs) -> xs |> List.choose asString |> Some
        | _ -> None

    let private parseRootManifest (doc: TomlDocument) : Result<RootManifest, string> =
        let upstream = Map.tryFind "upstream" doc |> Option.bind asTable
        let buckets = Map.tryFind "bucket" doc |> Option.bind asArray

        match upstream, buckets with
        | None, _ -> Error "manifest.toml: missing [upstream] table"
        | _, None -> Error "manifest.toml: missing [[bucket]] array"
        | Some up, Some bs ->
            let bucketEntries =
                bs
                |> List.choose (fun b ->
                    match asTable b with
                    | None -> None
                    | Some t ->
                        match findString t "name", findString t "path" with
                        | Some n, Some p ->
                            Some
                                {
                                    Name = n
                                    Path = p
                                    Description = findString t "description" |> Option.defaultValue ""
                                    DependsOn = findStringList t "depends-on" |> Option.defaultValue []
                                }
                        | _ -> None
                )

            Ok
                {
                    UpstreamCommit = findString up "commit" |> Option.defaultValue ""
                    UpstreamTag = findString up "tag" |> Option.defaultValue ""
                    UpstreamPath = findString up "path" |> Option.defaultValue ""
                    Buckets = bucketEntries
                }

    let private parseBucketManifest (doc: TomlDocument) : Result<string list, string> =
        match Map.tryFind "files" doc with
        | Some(TomlValue.Array xs) -> xs |> List.choose asString |> Ok
        | _ -> Error "bucket manifest.toml: missing `files = [...]`"

    /// Topologically sorts buckets by their `DependsOn` graph. Within a
    /// depth tier, original input order is preserved. Errors on cycles and
    /// references to unknown buckets.
    let private topoSort (buckets: BucketEntry list) : Result<BucketEntry list, string> =
        let byName = buckets |> List.map (fun b -> b.Name, b) |> Map.ofList
        let mutable sorted = []
        let mutable visiting = Set.empty
        let mutable visited = Set.empty
        let mutable err = None

        let rec visit name =
            if err.IsSome then
                ()
            elif Set.contains name visited then
                ()
            elif Set.contains name visiting then
                err <- Some(sprintf "Bucket dependency cycle through '%s'" name)
            else
                match Map.tryFind name byName with
                | None -> err <- Some(sprintf "Unknown bucket '%s' in depends-on" name)
                | Some b ->
                    visiting <- Set.add name visiting

                    for d in b.DependsOn do
                        visit d

                    visiting <- Set.remove name visiting
                    visited <- Set.add name visited
                    sorted <- b :: sorted

        for b in buckets do
            visit b.Name

        match err with
        | Some e -> Error e
        | None -> Ok(List.rev sorted)

    /// Reads `<libRoot>/manifest.toml` plus each per-bucket manifest;
    /// returns the topologically-sorted flat list of files to feed the
    /// type-checker.
    let loadAll (libRoot: string) : Result<LoadedLib, string> =
        let rootPath = Path.Combine(libRoot, "manifest.toml")

        if not (File.Exists rootPath) then
            Error(sprintf "Root manifest not found: %s" rootPath)
        else
            match Toml.parse (File.ReadAllText rootPath) with
            | Error e -> Error(sprintf "Root manifest parse error: %s" e)
            | Ok doc ->
                match parseRootManifest doc with
                | Error e -> Error e
                | Ok root ->
                    match topoSort root.Buckets with
                    | Error e -> Error e
                    | Ok ordered ->
                        let mutable err = None
                        let files = ResizeArray()

                        for bucket in ordered do
                            if err.IsNone then
                                let bp = Path.Combine(libRoot, bucket.Path, "manifest.toml")

                                if not (File.Exists bp) then
                                    err <- Some(sprintf "Bucket manifest not found: %s" bp)
                                else
                                    match Toml.parse (File.ReadAllText bp) with
                                    | Error e ->
                                        err <- Some(sprintf "Bucket manifest parse error (%s): %s" bucket.Name e)
                                    | Ok bdoc ->
                                        match parseBucketManifest bdoc with
                                        | Error e -> err <- Some(sprintf "%s bucket: %s" bucket.Name e)
                                        | Ok fileList ->
                                            for rel in fileList do
                                                files.Add
                                                    {
                                                        BucketName = bucket.Name
                                                        Relative = rel
                                                        Absolute = Path.Combine(libRoot, bucket.Path, rel)
                                                    }

                        match err with
                        | Some e -> Error e
                        | None ->
                            Ok
                                {
                                    Root = root
                                    Files = List.ofSeq files
                                }

    /// The lexer's token table and source text are retained so subsequent
    /// passes can extract identifier text off a `SyntaxToken`.
    type ParsedFile =
        {
            File: LibFile
            Input: string
            Lexed: Lexed
            Ast: FSharpAst<SyntaxToken>
        }

    /// Force-load the parser's `ObjectConstruction` ref so attribute
    /// parsing succeeds even when the only entry points hit are
    /// signature-file parsers. The init lives behind a `do` at the head
    /// of `ImplementationFile.pNamedModule`, which a pure-signature path
    /// may never touch.
    do ObjectConstruction.init ()

    /// Parse one `.fsi` file via XParsec.FSharp's signature-file parser.
    /// `.fs` files (rare — only when no signature exists, e.g. `SI.fs`)
    /// are routed through the implementation parser.
    let parseFileFull (file: LibFile) : Result<ParsedFile, string> =
        let raw = File.ReadAllText file.Absolute
        let input = raw.Replace("\r\n", "\n")

        match Lexing.lexString input with
        | Error _ -> Error(sprintf "Lex error in %s" file.Relative)
        | Ok lexed ->
            let reader = Reader.ofLexed lexed input Set.empty

            let result =
                if file.Relative.EndsWith ".fsi" then
                    FSharpAst.parseSignature reader
                else
                    FSharpAst.parse reader

            match result with
            | Ok ast ->
                Ok
                    {
                        File = file
                        Input = input
                        Lexed = lexed
                        Ast = ast
                    }
            | Error e -> Error(ErrorFormatting.splitAndFormatTokenErrors e)

    let parseFile (file: LibFile) : Result<FSharpAst<SyntaxToken>, string> =
        parseFileFull file |> Result.map (fun p -> p.Ast)
