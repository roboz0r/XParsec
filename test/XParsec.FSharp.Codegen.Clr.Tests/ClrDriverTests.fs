module XParsec.FSharp.Codegen.Clr.Tests.ClrDriverTests

open System
open System.IO
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The `ClrDriver` acceptance gate: unlike the rest of this suite, which reads the BCL off
// the compiler HOST's runtime assemblies, the driver builds its metadata reader from an
// EXPLICIT reference set, in this case the pinned `net8.0` ref pack.

/// The simple names the PE at `path` declares an `AssemblyRef` to.
let private assemblyRefNames (path: string) : string list =
    use fs = File.OpenRead path
    use pe = new PEReader(fs)
    let md = pe.GetMetadataReader()

    [
        for h in md.AssemblyReferences -> md.GetString((md.GetAssemblyReference h).Name)
    ]

/// The compilation `project` describes against the ref pack its own TFM names, or a test
/// failure carrying why the pack could not be resolved.
let private compilationFor (project: ProjectInfo) : ClrCompilation =
    match ClrCompilation.forTfm project [ vesperCorePackage ] Set.empty with
    | Ok inputs -> inputs
    | Error ds -> failtestf "ref pack unavailable:\n%s" (AssemblyFiles.AnchoredDiagnostic.renderAll ds)

[<Tests>]
let tests =
    testList
        "ClrDriver"
        [
            // No function value, list or printf, so the program has no Vesper runtime
            // dependency: a BCL-only static call, run on disk with its refs inspected.
            test "compiles + runs a program against the net8.0 ref pack, binding ref-pack AssemblyRefs" {
                let outDir = tmpDir "clr-driver-refpack"

                let inputs =
                    compilationFor
                        { ProjectInfo.app "ClrDriverRefPack" outDir with
                            TargetFramework = Some "net8.0"
                        }

                let source =
                    oneSource inputs.Project.AssemblyName "System.Console.WriteLine \"hello\""

                let artifact =
                    match ClrDriver.compile inputs source with
                    | Ok a -> a
                    | Error ds -> failtestf "driver compile failed:\n%s" (AssemblyFiles.AnchoredDiagnostic.renderAll ds)

                Codegen.materialiseApp artifact

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

            // The driver is production surface, so it raises no exception of its own.
            test "a type error returns Error diagnostics (no exception)" {
                let inputs =
                    compilationFor
                        { ProjectInfo.defaults "ClrDriverTypeError" with
                            TargetFramework = Some "net8.0"
                        }

                // `1 + "x"`: an int/string operand mismatch the front end rejects.
                let source = oneSource inputs.Project.AssemblyName "let x = 1 + \"x\""

                match ClrDriver.compile inputs source with
                | Ok _ -> failtest "expected the type error to be returned as diagnostics"
                | Error ds ->
                    Expect.isNonEmpty ds "at least one diagnostic"
                    Expect.all ds (fun d -> Diagnostic.isError d.Diagnostic) "all returned diagnostics are errors"
            }

            test "a compilation whose project sets no TargetFramework is refused" {
                match ClrCompilation.forTfm (ProjectInfo.defaults "ClrDriverNoTfm") [] Set.empty with
                | Ok _ -> failtest "expected the missing TFM to be refused"
                | Error ds ->
                    Expect.all ds (fun d -> Diagnostic.isError d.Diagnostic) "the refusal is an error"

                    Expect.stringContains
                        (AssemblyFiles.AnchoredDiagnostic.renderAll ds)
                        "TargetFramework"
                        "the message names the field that was not set"
            }
        ]
