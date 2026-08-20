namespace XParsec.FSharp.SemanticAnalysis

/// An assembly's inputs as the front end takes them: what it emits into, and its units in
/// compile order.
[<NoEquality; NoComparison>]
type AssemblySources =
    {
        Assembly: CompilingAssembly
        Units: Result<AssemblyFiles.ParsedUnit, AssemblyFiles.UnparsedFile> list
    }

[<RequireQualifiedAccess>]
module AssemblySources =

    /// Name and target off the manifest, units in its file order.
    let ofPackage (pkg: PackageSource.ParsedPackage) : AssemblySources =
        {
            Assembly =
                {
                    Name = AssemblyName pkg.Manifest.Name
                    Target = pkg.Manifest.Target
                }
            Units = PackageUnits.ofPackage pkg
        }

    /// `ofPackage` for a caller holding only the path: it reads the package itself. Whether
    /// the pairing conforms is the caller's own check.
    let ofManifest (mp: ReferencedProject.ManifestPath) : Result<AssemblySources, PackageSetFault> =
        ReferencedProject.loadManifest mp
        |> Result.map (PackageSource.readPackage >> ofPackage)

    /// Sources held as TEXT under a caller-supplied assembly: a driver handed a string, a
    /// test. A half with no tree faults its own unit alone.
    let synthetic
        (name: string)
        (target: string)
        (compilationDefines: Set<string>)
        (units: AssemblyFiles.SourceUnit list)
        : AssemblySources =
        {
            Assembly =
                {
                    Name = AssemblyName name
                    Target = target
                }
            Units = List.map (AssemblyFiles.parseUnit compilationDefines) units
        }
