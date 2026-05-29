namespace XParsec.FSharp.Codegen.Clr

open System.IO
open System.Reflection

/// The PE-to-disk side of codegen: serialised bytes, the in-place write, and a
/// runnable framework-dependent bundle (PE + `runtimeconfig.json` + the
/// referenced assemblies the shared framework does not carry).
module Materialise =

    /// The serialised PE bytes.
    let toBytes (artifact: ClrArtifact) : byte[] = artifact.Pe.ToArray()

    /// The only side effect: write the PE to `OutputPath` when one is set.
    let materialise (artifact: ClrArtifact) : unit =
        match artifact.OutputPath with
        | Some path ->
            use stream = new FileStream(path, FileMode.Create, FileAccess.Write)
            artifact.Pe.WriteContentTo(stream)
        | None -> ()

    /// TFM + shared-framework version read straight off the host runtime: the
    /// emitted `AssemblyRef`s bind against exactly the assemblies loaded in this
    /// process, so the produced app must run on the same major.
    let private hostFramework () : string * string =
        let v = System.Environment.Version
        sprintf "net%d.%d" v.Major v.Minor, sprintf "%d.%d.0" v.Major v.Minor

    /// The `runtimeconfig.json` a framework-dependent console app needs beside
    /// its dll. `rollForward: Major` lets it run on a newer installed runtime.
    let private runtimeConfigJson (tfm: string) (frameworkVersion: string) : string =
        System.String.Join(
            "\n",
            [
                "{"
                "  \"runtimeOptions\": {"
                sprintf "    \"tfm\": \"%s\"," tfm
                "    \"rollForward\": \"Major\","
                "    \"framework\": {"
                "      \"name\": \"Microsoft.NETCore.App\","
                sprintf "      \"version\": \"%s\"" frameworkVersion
                "    }"
                "  }"
                "}"
                ""
            ]
        )

    /// Materialise a *runnable* framework-dependent app: the PE, its
    /// `runtimeconfig.json`, and a copy of every referenced assembly the shared
    /// framework does *not* carry, into the PE's directory. After this,
    /// `dotnet <OutputPath>` runs the program. Requires `OutputPath`.
    let materialiseApp (project: ProjectInfo) (artifact: ClrArtifact) : unit =
        match artifact.OutputPath with
        | None -> failwith "Codegen.materialiseApp: ProjectInfo.OutputPath must be set"
        | Some dllPath ->
            let dir = Path.GetDirectoryName dllPath
            Directory.CreateDirectory dir |> ignore
            materialise artifact

            let tfm, frameworkVersion =
                match project.TargetFramework with
                | Some t ->
                    let v = System.Environment.Version
                    t, sprintf "%d.%d.0" v.Major v.Minor
                | None -> hostFramework ()

            File.WriteAllText(
                Path.Combine(dir, project.AssemblyName + ".runtimeconfig.json"),
                runtimeConfigJson tfm frameworkVersion
            )

            // The source for a simple name is the `ProjectInfo.References` entry
            // that supplied it; for the two host-resolved fallbacks the provider
            // allows — FSharp.Core (the cold-printf island) and Vesper.Printf (the
            // happy-path formatter) — the host-loaded copy, unless a reference
            // already overrides it. A name with no source (the BCL) resolves from
            // the shared framework and is skipped. A reference the PE never bound
            // against is absent from the set, so a happy-path bundle stays
            // FSharp.Core-free.
            let referenceSources =
                let fromProject =
                    project.References
                    |> List.map (fun path -> AssemblyName.GetAssemblyName(path).Name, path)
                    |> Map.ofList

                let withFallback name (hostPath: unit -> string) m =
                    if Map.containsKey name m then
                        m
                    else
                        Map.add name (hostPath ()) m

                fromProject
                |> withFallback "FSharp.Core" (fun () -> typeof<Microsoft.FSharp.Core.Unit>.Assembly.Location)
                |> withFallback "Vesper.Printf" (fun () -> typeof<Vesper.PrintfRuntime>.Assembly.Location)

            for refName in artifact.ReferencedAssemblies do
                match Map.tryFind refName referenceSources with
                | Some src ->
                    // The loader probes the app base by *simple name*, so the
                    // destination file is always `<simpleName>.dll`.
                    let dst = Path.Combine(dir, refName + ".dll")

                    if
                        not (
                            System.String.Equals(
                                Path.GetFullPath src,
                                Path.GetFullPath dst,
                                System.StringComparison.OrdinalIgnoreCase
                            )
                        )
                    then
                        File.Copy(src, dst, true)
                | None -> ()
