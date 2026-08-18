namespace XParsec.FSharp.SemanticAnalysis

open System.IO

/// Every source file a package manifest NAMES, read off disk and parsed ONCE through the front
/// end a compiling caller uses, with what the read produced kept beside the path that produced
/// it.
module PackageSource =

    /// Why a path a manifest LISTED yielded no tree. Each is a finding about the manifest or
    /// the file, never a reason to read one fewer file than the manifest claimed.
    [<RequireQualifiedAccess>]
    type FileFault =
        /// The manifest lists a path that is not on disk.
        | Missing
        /// The file is there and the parser got no tree out of it.
        | Unparsed of ParseChain.ParseFailure

    [<RequireQualifiedAccess>]
    module FileFault =

        /// The fault as the parse failure it reads as. An absent path has no token stream
        /// either, so both faults travel the one channel.
        let toFailure (package: string) (relative: string) (fault: FileFault) : ParseChain.ParseFailure =
            match fault with
            | FileFault.Unparsed failure -> failure
            | FileFault.Missing ->
                {
                    Lexed = ValueNone
                    Diagnostics =
                        [
                            Diagnostic.nowhere (Kind.PackageSet(PackageSetFault.FileMissing(package, relative)))
                        ]
                }

    /// A path a manifest NAMED and what reading it produced. The two travel together: a
    /// diagnostic must echo the name whether or not a tree came out, and the name is the whole
    /// of what a fault has to say.
    [<NoEquality; NoComparison>]
    type ReadFile<'Tree> =
        {
            /// As the manifest spells it, which is what a diagnostic must echo.
            Relative: string
            /// The canonical `/`-separated name this file's anchors carry, which the manifest
            /// may spell differently (`.\math\z.fs` ⇒ `math/z.fs`).
            Id: AssemblyFileId
            Outcome: Result<'Tree, FileFault>
        }

    /// One compilation unit as READ: its implementation file, and the signature file that
    /// publishes it. `ValueNone` for an implementation file that owes no signature file, as in
    /// F# itself. Each half keeps its `Outcome`, so a per-file fault reports from the unit
    /// that names the file.
    [<NoEquality; NoComparison>]
    type ParsedSource =
        {
            Signature: ReadFile<ParseChain.ParsedSignature> voption
            Implementation: ReadFile<ParseChain.ParsedFile>
        }

    /// One `[core] files` unit. `UnpairedSignature` is a signature file this target ships no
    /// implementation for: the conformance gate refuses it, but the referencing route resolves
    /// and publishes it, so it stays representable.
    [<RequireQualifiedAccess; NoEquality; NoComparison>]
    type PackageUnit =
        | Source of ParsedSource
        | UnpairedSignature of ReadFile<ParseChain.ParsedSignature>

    /// Every path one manifest lists, read and parsed ONCE and PAIRED once, in `[core] files`
    /// order, each entry carrying what the read produced. No `runtime` (never F#).
    [<NoEquality; NoComparison>]
    type ParsedPackage =
        {
            Manifest: ReferencedProject.Manifest
            Units: PackageUnit list
        }

    /// Read and parse every path `manifest` lists, and pair the two halves. An absent or
    /// unparseable path faults and is never dropped: reading one file fewer than the manifest
    /// claimed would resolve, and CACHE, against a smaller package.
    let readPackage (manifest: ReferencedProject.Manifest) : ParsedPackage =
        let read (parse: string -> Result<'Tree, ParseChain.ParseFailure>) (relative: string) : ReadFile<'Tree> =
            let absolute = Path.Combine(manifest.Dir, relative)

            {
                Relative = relative
                Id = AssemblyFileId.ofPathUnder manifest.Dir relative
                Outcome =
                    if not (File.Exists absolute) then
                        Error FileFault.Missing
                    else
                        parse (File.ReadAllText absolute) |> Result.mapError FileFault.Unparsed
            }

        let readSignature = read (ParseChain.parseSignature Set.empty)
        let readImplementation = read (ParseChain.parse Set.empty)

        // Manifest parsing enforces `.fsi`-immediately-before-companion-`.fs`, so adjacency
        // IS the pairing: a `.fsi` pairs with the next entry exactly when their keys agree.
        let key = ReferencedProject.pairingKey manifest.Target

        let rec units (files: ReferencedProject.ManifestFile list) : PackageUnit list =
            match files with
            | [] -> []
            | file :: rest ->
                match file.Kind with
                | SourceFileKind.Implementation ->
                    PackageUnit.Source
                        {
                            Signature = ValueNone
                            Implementation = readImplementation file.Relative
                        }
                    :: units rest
                | SourceFileKind.Signature ->
                    let signature = readSignature file.Relative

                    match rest with
                    | companion :: rest when
                        companion.Kind = SourceFileKind.Implementation
                        && key companion.Relative = key file.Relative
                        ->
                        PackageUnit.Source
                            {
                                Signature = ValueSome signature
                                Implementation = readImplementation companion.Relative
                            }
                        :: units rest
                    | _ -> PackageUnit.UnpairedSignature signature :: units rest

        {
            Manifest = manifest
            Units = units manifest.Files
        }
