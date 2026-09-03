namespace XParsec.FSharp.Codegen.Clr

open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.PortableExecutable

/// The PE-to-disk side of codegen: serialised bytes, the in-place write, and a
/// runnable framework-dependent bundle (PE + `runtimeconfig.json` + the
/// referenced assemblies the shared framework does not carry).
module Materialise =

    let private referencedAssemblyNames (path: string) : string list =
        use fs = File.OpenRead path
        use pe = new PEReader(fs)
        let md = pe.GetMetadataReader()

        [
            for h in md.AssemblyReferences -> md.GetString((md.GetAssemblyReference h).Name)
        ]

    let toBytes (artifact: ClrArtifact) : byte[] = artifact.Pe.ToArray()

    /// Write the PE to `OutputPath`, if one is set.
    let materialise (artifact: ClrArtifact) : unit =
        match artifact.OutputPath with
        | Some path ->
            use stream = new FileStream(path, FileMode.Create, FileAccess.Write)
            artifact.Pe.WriteContentTo(stream)
        | None -> ()

    /// TFM + framework version off the host runtime: the emitted `AssemblyRef`s bind
    /// against this process's own assemblies, so the app must run on the same major.
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

    /// The PE, its `runtimeconfig.json` and a copy of every referenced assembly the
    /// shared framework does not carry, into the PE's directory, after which
    /// `dotnet <OutputPath>` runs the program. Requires `OutputPath`.
    let materialiseApp (artifact: ClrArtifact) : unit =
        let project = artifact.Project

        match artifact.OutputPath with
        | None -> failwith "materialiseApp: ProjectInfo.OutputPath must be set"
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

            // Where to copy a simple name from: its `ProjectInfo.References` entry, or for
            // FSharp.Core alone the host-loaded copy when no reference overrides it. A name
            // with no source is a BCL name, resolved from the shared framework, so skipped.
            let referenceSources =
                let withFallback name (hostPath: unit -> string) m =
                    if Map.containsKey name m then
                        m
                    else
                        Map.add name (hostPath ()) m

                ProjectInfo.referenceSources project
                |> withFallback "FSharp.Core" (fun () -> typeof<Microsoft.FSharp.Core.Unit>.Assembly.Location)

            // Ship set closed over transitive references, because a `%A` program's PE references
            // `Vesper.Printf`, which needs `Vesper.Core` beside it or `%A` throws
            // `FileNotFoundException`. A name with no source ends the walk.
            let shipNames =
                let rec close (seen: Set<string>) (frontier: string list) : Set<string> =
                    match frontier with
                    | [] -> seen
                    | name :: rest when Set.contains name seen -> close seen rest
                    | name :: rest ->
                        let seen = Set.add name seen

                        let more =
                            match Map.tryFind name referenceSources with
                            | Some src when File.Exists src -> referencedAssemblyNames src
                            | _ -> []

                        close seen (more @ rest)

                close Set.empty (List.ofSeq artifact.ReferencedAssemblies)

            for refName in shipNames do
                match Map.tryFind refName referenceSources with
                | Some src ->
                    // The loader probes the app base by simple name: `<simpleName>.dll`.
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
