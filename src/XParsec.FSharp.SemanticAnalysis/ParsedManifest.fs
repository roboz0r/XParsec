namespace XParsec.FSharp.SemanticAnalysis

open System.IO

/// Why a path the manifest lists yielded no tree.
[<RequireQualifiedAccess>]
type FileFault =
    /// The manifest lists a path that is not on disk. The read has no text, so the fault is
    /// exhausted by the diagnostic naming it.
    | Missing of Diagnostic
    /// The file is there and the parser got no tree out of it.
    | Unparsed of ParseChain.ParseFailure

[<RequireQualifiedAccess>]
module FileFault =

    /// The diagnostic naming a path the manifest lists and disk does not hold.
    let missing (package: string) (relative: string) : FileFault =
        FileFault.Missing(Diagnostic.nowhere (Kind.PackageSet(PackageSetFault.FileMissing(package, relative))))

    let diagnostics (fault: FileFault) : Diagnostic list =
        match fault with
        | FileFault.Missing d -> [ d ]
        | FileFault.Unparsed failure -> failure.Diagnostics

/// A path the manifest lists and what reading it produced. A diagnostic must echo the path
/// whether or not a tree came out.
[<NoEquality; NoComparison>]
type ReadFile<'Tree> =
    {
        /// As the manifest spells it.
        Relative: string
        /// The canonical `/`-separated name this file's anchors carry, which the manifest
        /// may spell differently (`.\math\z.fs` ⇒ `math/z.fs`).
        Id: AssemblyFileId
        Outcome: Result<'Tree, FileFault>
    }

/// One compilation unit as READ. Each half keeps its `Outcome`, so a fault reports against the
/// path it came from. `ValueNone` for an implementation file that owes no signature file, as in
/// F# itself.
type ReadSourceUnit = SourceUnit<ReadFile<ParseChain.ParsedSignature>, ReadFile<ParseChain.ParsedImplementation>>

/// Every source file the manifest lists, read off disk and parsed ONCE through the front end a
/// compiling caller uses, in `[core] files` order, with what the read produced kept beside the
/// path that produced it. `[core] runtime` assets are never F#, so they are left unread.
[<NoEquality; NoComparison>]
type ParsedManifest =
    {
        Manifest: ReferencedProject.Manifest
        Units: ReadSourceUnit list
    }

[<RequireQualifiedAccess>]
module ParsedManifest =

    /// Read and parse every path `manifest` lists, unit by unit as the manifest parse paired
    /// them. An absent or unparseable path faults and is retained, so the read resolves and
    /// CACHES against exactly the file set the manifest lists.
    let ofManifest (manifest: ReferencedProject.Manifest) : ParsedManifest =
        let read (parse: string -> Result<'Tree, ParseChain.ParseFailure>) (relative: string) : ReadFile<'Tree> =
            let absolute = Path.Combine(manifest.Dir, relative)

            {
                Relative = relative
                Id = AssemblyFileId.ofPathUnder manifest.Dir relative
                Outcome =
                    if not (File.Exists absolute) then
                        Error(FileFault.missing manifest.Name relative)
                    else
                        parse (File.ReadAllText absolute) |> Result.mapError FileFault.Unparsed
            }

        let readSignature = read (ParseChain.parseSignature Set.empty)
        let readImplementation = read (ParseChain.parse Set.empty)

        {
            Manifest = manifest
            Units =
                [
                    for unit in manifest.Units ->
                        {
                            Signature =
                                unit.Signature
                                |> ValueOption.map (fun (s: ReferencedProject.ManifestFile) -> readSignature s.Relative)
                            Implementation = readImplementation unit.Implementation.Relative
                        }
                ]
        }
