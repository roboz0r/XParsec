module XParsec.FSharp.Codegen.Clr.Tests.ClrDriverTests

open System
open System.IO
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The `ClrDriver` acceptance gate: unlike every other fixture in this suite (which
// reflects the BCL off the compiler HOST's runtime assemblies), the driver builds
// its metadata leaf from an EXPLICIT reference set — here the pinned `net8.0` ref
// pack. The end-to-end run then proves the emitted `System.Console` (ref-pack)
// `AssemblyRef` binds against the shared framework at runtime, rather than the
// host's `System.Private.CoreLib` (impl) identity.

/// The simple names the PE at `path` declares an `AssemblyRef` to.
let private assemblyRefNames (path: string) : string list =
    use fs = File.OpenRead path
    use pe = new PEReader(fs)
    let md = pe.GetMetadataReader()

    [
        for h in md.AssemblyReferences -> md.GetString((md.GetAssemblyReference h).Name)
    ]

[<Tests>]
let tests =
    testList
        "ClrDriver"
        [
            // A BCL-only static call (no function value / list / printf, so no Vesper
            // runtime dependency) compiled against the pinned net8.0 ref pack, run on
            // disk, with its AssemblyRefs inspected — the ref-set isolation gate.
            test "compiles + runs a program against the net8.0 ref pack, binding ref-pack AssemblyRefs" {
                let bclReferences =
                    match RefPack.resolve "net8.0" with
                    | Result.Ok dlls -> dlls
                    | Result.Error e -> failtestf "net8.0 ref pack unavailable: %s" e

                let outDir = tmpDir "clr-driver-refpack"

                let project =
                    { ProjectInfo.app "ClrDriverRefPack" outDir with
                        TargetFramework = Some "net8.0"
                    }

                let inputs =
                    {
                        Project = project
                        Manifests = [ vesperCoreManifest ]
                        BclReferences = bclReferences
                    }

                let artifact =
                    match ClrDriver.compileApp inputs "System.Console.WriteLine \"hello\"" with
                    | Ok a -> a
                    | Error ds ->
                        failtestf
                            "driver compile failed: %s"
                            (ds |> List.map (fun d -> d.Message) |> String.concat "\n")

                let dllPath =
                    match artifact.OutputPath with
                    | Some p -> p
                    | None -> failtest "expected an OutputPath"

                let exitCode, output = runOnDisk dllPath
                Expect.equal exitCode 0 (sprintf "dotnet exits 0 (output was: %s)" output)
                Expect.equal (output.Trim()) "hello" "the ref-pack app prints hello"

                let refs = assemblyRefNames dllPath

                Expect.contains refs "System.Console" "System.Console.WriteLine binds the ref-pack System.Console"

                Expect.isFalse
                    (List.contains "System.Private.CoreLib" refs)
                    (sprintf "no host impl (System.Private.CoreLib) AssemblyRef; refs were: %A" refs)
            }

            // The driver is production surface: a type error is returned as
            // Severity.Error diagnostics, never a `failwith`.
            test "a type error returns Error diagnostics (no exception)" {
                let inputs =
                    {
                        Project = ProjectInfo.defaults "ClrDriverTypeError"
                        Manifests = [ vesperCoreManifest ]
                        BclReferences =
                            match RefPack.resolve "net8.0" with
                            | Result.Ok dlls -> dlls
                            | Result.Error e -> failtestf "net8.0 ref pack unavailable: %s" e
                    }

                // `1 + "x"`: an int/string operand mismatch the front end rejects.
                match ClrDriver.compile inputs "let x = 1 + \"x\"" with
                | Ok _ -> failtest "expected the type error to be returned as diagnostics"
                | Error ds ->
                    Expect.isNonEmpty ds "at least one diagnostic"
                    Expect.all ds Diagnostic.isError "all returned diagnostics are errors"
            }
        ]
