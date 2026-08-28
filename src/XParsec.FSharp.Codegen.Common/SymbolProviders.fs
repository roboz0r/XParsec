namespace XParsec.FSharp.Codegen.Common

open System.IO
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

/// Builds the symbol-resolution provider stack.
module SymbolProviders =

    /// Layer-2 FACTORY over the intrinsic axis composed from the layer-1 providers, which is
    /// what `type int = (# "System.Int32" #)` declares, both directions. A factory, not a
    /// fixed list, so that axis can seed it.
    type PlatformMetadataFactory = PackageProviders.PlatformMetadataFactory

    /// A package's manifest for the compiling target, as resolved from its directory.
    type ManifestPath = ReferencedProject.ManifestPath

    /// A parsed package manifest.
    type Manifest = ReferencedProject.Manifest

    /// The package stack for a compilation that IS a package: its declared references, then the
    /// package's OWN directory last. That `.fsi` route is the only channel a prior file's
    /// `type int32 = int` reaches a later one through: freezing carries no abbreviation.
    let selfStack (selfPackage: string option) (packageDirs: string list) : string list =
        match selfPackage with
        | Some p -> packageDirs @ [ p ]
        | None -> packageDirs

    /// Compose the layer-1 contract stack ahead of a caller-supplied layer-2 FACTORY. Common
    /// is platform-neutral; each backend injects its own reader. `readManifest` is the
    /// per-manifest read, so a caller already holding a package's trees hands them over
    /// instead of reading them again.
    let buildContractWith
        (readManifest: Manifest -> ParsedManifest)
        (platformMetadata: PlatformMetadataFactory)
        (target: string)
        (packageDirs: string list)
        : PackageProviders.AnalysedManifest =
        match ReferencedProject.resolveAll target packageDirs with
        | Result.Ok resolved -> PackageProviders.composeContractWith readManifest platformMetadata resolved
        | Result.Error fault -> PackageProviders.AnalysedManifest.ofSetFault fault

    /// `buildContractWith`, reading every package from disk.
    let buildContract
        (platformMetadata: PlatformMetadataFactory)
        (target: string)
        (packageDirs: string list)
        : PackageProviders.AnalysedManifest =
        buildContractWith ParsedManifest.ofManifest platformMetadata target packageDirs
