namespace XParsec.FSharp.SemanticAnalysis

// The units a PACKAGE compiles, as against the list a driver is handed: the manifest's `.fs`
// order, each implementation file under the signature file the package READ married it to.

module PackageUnits =

    /// Every implementation file as a compilation unit, in manifest order, under the signature
    /// file the package paired it with. One with no signature file publishes the surface it
    /// infers; one whose EITHER half arrived without a tree is `Error`, never a smaller unit.
    /// Blind to an unpaired signature file, which the conformance gate separately refuses.
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

        let ofSource
            (source: PackageSource.ParsedSource)
            : Result<AssemblyFiles.ParsedUnit, AssemblyFiles.UnparsedFile> =
            match source.Implementation.Outcome with
            | Error fault -> Error(unread source.Implementation fault)
            | Ok parsedImplementation ->
                let implementation = half source.Implementation parsedImplementation

                match source.Signature with
                | ValueNone ->
                    Ok
                        {
                            Implementation = implementation
                            Signature = ValueNone
                        }
                | ValueSome signature ->
                    match signature.Outcome with
                    | Ok parsedSignature ->
                        Ok
                            {
                                Implementation = implementation
                                Signature = ValueSome(half signature parsedSignature)
                            }
                    | Error fault -> Error(unread signature fault)

        [
            for unit in pkg.Units do
                match unit with
                | PackageSource.PackageUnit.Source source -> ofSource source
                | PackageSource.PackageUnit.UnpairedSignature _ -> ()
        ]

    /// `ofPackage` for a caller holding only the path: it reads the package itself. Whether the
    /// pairing conforms is the caller's own check.
    let ofManifest
        (mp: ReferencedProject.ManifestPath)
        : Result<Result<AssemblyFiles.ParsedUnit, AssemblyFiles.UnparsedFile> list, PackageSetFault> =
        ReferencedProject.loadManifest mp
        |> Result.map (PackageSource.readPackage >> ofPackage)
