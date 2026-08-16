namespace XParsec.FSharp.SemanticAnalysis

open System.IO

/// Every source file a package manifest NAMES, read off disk and parsed ONCE through the front
/// end a compiling caller uses, with what the read produced kept beside the path that produced
/// it.
module PackageSource =

    /// Which half of a compilation unit a path is. The EXTENSION decides.
    [<RequireQualifiedAccess>]
    type Half =
        | Signature
        | Implementation

    [<RequireQualifiedAccess>]
    module Half =

        let ofPath (relative: string) : Half =
            if relative.EndsWith ".fsi" then
                Half.Signature
            else
                Half.Implementation

        let describe (half: Half) : string =
            match half with
            | Half.Signature -> "a signature file"
            | Half.Implementation -> "an implementation file"

    /// Why a path a manifest NAMED yielded no tree. Each is a finding about the manifest or
    /// the file, never a reason to read one fewer file than the manifest claimed.
    [<RequireQualifiedAccess>]
    type FileFault =
        /// The manifest names a path that is not on disk.
        | Missing
        /// The file is there and the parser got no tree out of it.
        | Unparsed of ParseChain.ParseFailure
        /// A `.fs` listed under `[core] files`, or a `.fsi` under `[core] impl`: the halves
        /// are mispaired, and reading it as the list's half would publish an empty surface.
        | WrongHalf of expected: Half

    [<RequireQualifiedAccess>]
    module FileFault =

        /// The fault as the parse failure it reads as. A path that is absent or the wrong half
        /// has no token stream either, so all three faults travel the one channel.
        let toFailure (package: string) (relative: string) (fault: FileFault) : ParseChain.ParseFailure =
            let setFault (f: PackageSetFault) : ParseChain.ParseFailure =
                {
                    Lexed = ValueNone
                    Diagnostics = [ Diagnostic.nowhere (Kind.PackageSet f) ]
                }

            match fault with
            | FileFault.Unparsed failure -> failure
            | FileFault.Missing -> setFault (PackageSetFault.FileMissing(package, relative))
            | FileFault.WrongHalf expected ->
                setFault (PackageSetFault.FileWrongHalf(package, relative, Half.describe expected))

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

    /// One `[core] files` entry: the signature file, and the `[core] impl` implementation file
    /// the manifest's own pairing key marries it to. `ValueNone` for a signature file this
    /// target ships no implementation for.
    [<NoEquality; NoComparison>]
    type SignatureEntry =
        {
            Signature: ReadFile<ParseChain.ParsedSignature>
            Companion: ReadFile<ParseChain.ParsedFile> voption
        }

    /// One `[core] impl` entry: the implementation file, and the `[core] files` signature file
    /// that publishes it. `ValueNone` for an implementation file that owes no signature file,
    /// as in F# itself.
    [<NoEquality; NoComparison>]
    type ImplementationEntry =
        {
            Implementation: ReadFile<ParseChain.ParsedFile>
            Companion: ReadFile<ParseChain.ParsedSignature> voption
        }

    /// Every path one manifest names, read and parsed ONCE and PAIRED once: `[core] files` in
    /// declared order, then `[core] impl`, each entry carrying what the read produced and the
    /// file across the pairing. No `sig-only` (a subset of `files`) and no `runtime` (never F#).
    [<NoEquality; NoComparison>]
    type ParsedPackage =
        {
            Manifest: ReferencedProject.Manifest
            Signatures: SignatureEntry list
            Implementations: ImplementationEntry list
        }

    /// Read and parse every path `manifest` names, and pair the two lists. An absent, unparseable
    /// or wrong-half path faults and is never dropped: reading one file fewer than the manifest
    /// claimed would resolve, and CACHE, against a smaller package.
    let readPackage (manifest: ReferencedProject.Manifest) : ParsedPackage =
        let read
            (want: Half)
            (parse: string -> Result<'Tree, ParseChain.ParseFailure>)
            (relative: string)
            : ReadFile<'Tree> =
            let absolute = Path.Combine(manifest.Dir, relative)

            {
                Relative = relative
                Id = AssemblyFileId.ofPathUnder manifest.Dir relative
                Outcome =
                    if Half.ofPath relative <> want then
                        Error(FileFault.WrongHalf want)
                    elif not (File.Exists absolute) then
                        Error FileFault.Missing
                    else
                        parse (File.ReadAllText absolute) |> Result.mapError FileFault.Unparsed
            }

        let signatures =
            manifest.Files
            |> List.map (read Half.Signature (ParseChain.parseSignature Set.empty))

        let implementations =
            manifest.Impl
            |> List.map (read Half.Implementation (ParseChain.parse Set.empty))

        // The pairing, taken once and handed to both sides. A later entry wins a key clash.
        let key = ReferencedProject.pairingKey manifest

        let implementationByKey =
            System.Collections.Generic.Dictionary<string, _>(System.StringComparer.Ordinal)

        let signatureByKey =
            System.Collections.Generic.Dictionary<string, _>(System.StringComparer.Ordinal)

        for implementation in implementations do
            implementationByKey.[key implementation.Relative] <- implementation

        for signature in signatures do
            signatureByKey.[key signature.Relative] <- signature

        let companion
            (table: System.Collections.Generic.Dictionary<string, 'Other>)
            (relative: string)
            : 'Other voption =
            match table.TryGetValue(key relative) with
            | true, other -> ValueSome other
            | _ -> ValueNone

        {
            Manifest = manifest
            Signatures =
                [
                    for signature in signatures ->
                        {
                            Signature = signature
                            Companion = companion implementationByKey signature.Relative
                        }
                ]
            Implementations =
                [
                    for implementation in implementations ->
                        {
                            Implementation = implementation
                            Companion = companion signatureByKey implementation.Relative
                        }
                ]
        }
