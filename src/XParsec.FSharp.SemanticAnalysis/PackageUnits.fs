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
        [
            for unit in pkg.Units do
                match AssemblyFiles.ClassifiedUnit.ofPackageUnit pkg.Manifest.Name unit with
                | AssemblyFiles.ClassifiedUnit.Parsed u -> Ok u
                | AssemblyFiles.ClassifiedUnit.Faulted(leading, _) -> Error leading
                | AssemblyFiles.ClassifiedUnit.Unpaired _ -> ()
        ]

    /// `ofPackage` for a caller holding only the path: it reads the package itself. Whether the
    /// pairing conforms is the caller's own check.
    let ofManifest
        (mp: ReferencedProject.ManifestPath)
        : Result<Result<AssemblyFiles.ParsedUnit, AssemblyFiles.UnparsedFile> list, PackageSetFault> =
        ReferencedProject.loadManifest mp
        |> Result.map (PackageSource.readPackage >> ofPackage)
