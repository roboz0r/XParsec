namespace XParsec.FSharp.SemanticAnalysis

// The units a PACKAGE compiles, as against the list a driver is handed: the manifest's `.fs`
// order, each implementation file under the signature file the package READ married it to.

module PackageUnits =

    /// Every implementation file as a compilation unit, in manifest order, under the signature
    /// file the package paired it with. One with no signature file publishes the surface it
    /// infers; one whose EITHER half arrived without a tree is `Error`, never a smaller unit.
    let ofPackage
        (pkg: PackageSource.ParsedPackage)
        : Result<AssemblyFiles.ParsedUnit, AssemblyFiles.UnparsedFile> list =
        let package = pkg.Manifest.Name

        let unread (file: PackageSource.ReadFile<'Tree>) (fault: PackageSource.FileFault) : AssemblyFiles.UnparsedFile =
            {
                Id = file.Id
                Failure = PackageSource.FileFault.toFailure package file.Relative fault
            }

        let half (file: PackageSource.ReadFile<'Tree>) (parsed: 'Tree) : AssemblyFiles.ParsedHalf<'Tree> =
            { Id = file.Id; Parsed = parsed }

        [
            for entry in pkg.Implementations do
                match entry.Implementation.Outcome with
                | Error fault -> yield Error(unread entry.Implementation fault)
                | Ok parsedImplementation ->
                    let implementation = half entry.Implementation parsedImplementation

                    match entry.Companion with
                    | ValueNone ->
                        yield
                            Ok
                                {
                                    Implementation = implementation
                                    Signature = ValueNone
                                }
                    | ValueSome companion ->
                        match companion.Outcome with
                        | Ok parsedSignature ->
                            yield
                                Ok
                                    {
                                        Implementation = implementation
                                        Signature = ValueSome(half companion parsedSignature)
                                    }
                        | Error fault -> yield Error(unread companion fault)
        ]

    /// `ofPackage` for a caller holding only the path: it reads the package itself. Whether the
    /// pairing conforms is the caller's own check.
    let ofManifest
        (mp: ReferencedProject.ManifestPath)
        : Result<Result<AssemblyFiles.ParsedUnit, AssemblyFiles.UnparsedFile> list, PackageSetFault> =
        ReferencedProject.loadManifest mp
        |> Result.map (PackageSource.readPackage >> ofPackage)
