namespace XParsec.FSharp.Codegen.Clr

open System
open System.IO
open System.Runtime.InteropServices

/// Locates the installed .NET SDK's reference pack for a target TFM
/// (`Microsoft.NETCore.App.Ref`). MSBuild-shaped in spirit: the REAL mechanism is
/// an explicit `dllPaths` set (a `dotnet build` integration hands us the resolved
/// reference list). This resolver is only the no-MSBuild CONVENIENCE for driving a
/// compilation against a pinned TFM without an MSBuild invocation.
module RefPack =

    let private isWindows = RuntimeInformation.IsOSPlatform OSPlatform.Windows

    /// The directory containing the `dotnet` executable on `PATH`, if any.
    let private dotnetDirOnPath () : string option =
        let exe = if isWindows then "dotnet.exe" else "dotnet"

        match Environment.GetEnvironmentVariable "PATH" with
        | null -> None
        | path ->
            path.Split(Path.PathSeparator, StringSplitOptions.RemoveEmptyEntries)
            |> Array.tryPick (fun dir ->
                if File.Exists(Path.Combine(dir, exe)) then
                    Some dir
                else
                    None
            )

    /// dotnet-root discovery order: `DOTNET_ROOT`; then the directory of the
    /// `dotnet` on `PATH`; then the platform-standard install locations.
    let private candidateRoots () : string list =
        [
            match Environment.GetEnvironmentVariable "DOTNET_ROOT" with
            | null
            | "" -> ()
            | r -> r

            match dotnetDirOnPath () with
            | Some d -> d
            | None -> ()

            if isWindows then
                @"C:\Program Files\dotnet"
            else
                "/usr/share/dotnet"
                "/usr/local/share/dotnet"
                "/usr/lib/dotnet"
        ]

    /// A pack version directory's numeric prefix as a `System.Version`. A preview
    /// suffix (`9.0.0-preview.1`) orders on the part before `-`; it therefore ties
    /// with its own release (`9.0.0`), which is acceptable — the two do not coexist
    /// in a real install.
    let private parseVersion (name: string) : Version option =
        let core =
            match name.IndexOf '-' with
            | -1 -> name
            | i -> name.Substring(0, i)

        match Version.TryParse core with
        | true, v -> Some v
        | _ -> None

    /// The TFM's major version (`net8.0` → `8`).
    let private tfmMajor (tfm: string) : int option =
        let s = if tfm.StartsWith "net" then tfm.Substring 3 else tfm

        match s.Split '.' |> Array.tryHead with
        | Some m ->
            match Int32.TryParse m with
            | true, n -> Some n
            | _ -> None
        | None -> None

    /// Resolve the reference-assembly `.dll` set for `tfm` from the installed SDK's
    /// `Microsoft.NETCore.App.Ref` pack. Among pack version directories whose
    /// `ref/<tfm>` subdirectory exists and whose major matches the TFM's, the HIGHEST
    /// by `System.Version` ordering wins. `Error` names what was probed.
    let resolve (tfm: string) : Result<string list, string> =
        match tfmMajor tfm with
        | None -> Error(sprintf "RefPack: cannot parse a major version from TFM '%s'" tfm)
        | Some major ->
            let roots = candidateRoots ()
            let packRel = Path.Combine("packs", "Microsoft.NETCore.App.Ref")

            // Root discovery order is a PRIORITY order (an explicitly-set `DOTNET_ROOT`
            // must not be outvoted by a higher pack version in a standard install
            // location — the dotnet host itself treats it as an override). Version
            // ordering picks the best pack only WITHIN the first root that has one.
            let bestIn (root: string) : string option =
                let packDir = Path.Combine(root, packRel)

                if Directory.Exists packDir then
                    Directory.GetDirectories packDir
                    |> Array.choose (fun verDir ->
                        let refDir = Path.Combine(verDir, "ref", tfm)

                        match parseVersion (Path.GetFileName verDir) with
                        | Some v when v.Major = major && Directory.Exists refDir -> Some(v, refDir)
                        | _ -> None
                    )
                    |> Array.sortByDescending fst
                    |> Array.tryHead
                    |> Option.map snd
                else
                    None

            match roots |> List.tryPick bestIn with
            | Some refDir ->
                match Directory.GetFiles(refDir, "*.dll") |> Array.toList with
                | [] -> Error(sprintf "RefPack: ref pack directory '%s' contains no .dll files" refDir)
                | dlls -> Ok dlls
            | None ->
                Error(
                    sprintf
                        "RefPack: no Microsoft.NETCore.App.Ref pack with a ref/%s (major %d) found. Probed roots: [%s]; pack subpath '%s'. Supply explicit dllPaths instead."
                        tfm
                        major
                        (String.concat "; " roots)
                        packRel
                )
