module XParsec.FSharp.Codegen.Clr.Tests.SelfHostR4Tests

open System
open System.Reflection
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// R4 (docs/selfhost-handoff.md): the bootstrap plumbing is off FSharp.Core. Two
// things lock that in:
//   1. The canonical sample's on-disk bundle ships only the `Vesper.*` libraries it
//      binds against — no `FSharp.Core.dll` — and still runs. `materialiseApp` copies
//      exactly the assemblies the emitted PE references (`ReferencedAssemblies`), so
//      an FSharp.Core-free PE produces an FSharp.Core-free bundle.
//   2. The emitted FSharp.Core *reference identity* is read off the referenced file
//      (`ProjectInfo.References`), not hard-wired to whatever the compiler host
//      loaded — so the "host build dep" no longer leaks into emitted programs.

[<Tests>]
let tests =
    let fullSample =
        "let inline sum xs = List.fold (+) 0 xs\nlet nums = [1; 2; 3; 4; 5]\nprintfn \"%d\" (sum nums)"

    testList
        "SelfHostR4"
        [
            test
                "the canonical sample's on-disk bundle ships Vesper.Core + Vesper.List + Vesper.Printf, no FSharp.Core.dll" {
                let outDir = tmpDir "selfhost-r4-bundle"
                let project = withCore (ProjectInfo.app "XParsecR4Bundle" outDir)
                let artifact = compileSourceTo project fullSample

                // The PE references the Vesper libraries and nothing of FSharp.Core.
                Expect.isEmpty
                    artifact.FSharpCoreDependencies
                    (sprintf "the canonical sample pins no FSharp.Core (%A)" artifact.FSharpCoreDependencies)

                // Deterministic regardless of a prior run leaving the dll behind.
                let fsCoreDst = IO.Path.Combine(outDir, "FSharp.Core.dll")

                if IO.File.Exists fsCoreDst then
                    IO.File.Delete fsCoreDst

                Codegen.materialiseApp project artifact

                Expect.isTrue
                    (IO.File.Exists(IO.Path.Combine(outDir, "Vesper.Core.dll")))
                    "Vesper.Core.dll (Fun) shipped"

                Expect.isTrue
                    (IO.File.Exists(IO.Path.Combine(outDir, "Vesper.List.dll")))
                    "Vesper.List.dll (the cons-list) shipped"

                Expect.isTrue
                    (IO.File.Exists(IO.Path.Combine(outDir, "Vesper.Printf.dll")))
                    "Vesper.Printf.dll (the happy-path formatter) shipped"

                Expect.isFalse (IO.File.Exists fsCoreDst) "no FSharp.Core.dll in the bundle"

                let exitCode, output = runOnDisk (IO.Path.Combine(outDir, "XParsecR4Bundle.dll"))
                Expect.equal exitCode 0 (sprintf "dotnet exits 0 (output was: %s)" output)
                Expect.equal (output.Trim()) "15" "the FSharp.Core-free bundle runs"
            }

            // A happy-path bundle binds Vesper.Printf but no list / function value, so
            // it ships Vesper.Printf and nothing else of the Vesper.* set.
            test "a happy-path bundle ships Vesper.Printf but neither Vesper.Core nor Vesper.List" {
                let outDir = tmpDir "selfhost-r4-happy-bundle"
                let project = withCore (ProjectInfo.app "XParsecR4Happy" outDir)
                let artifact = compileSourceTo project "printfn \"%d\" 42"

                for stale in [ "FSharp.Core.dll"; "Vesper.Core.dll"; "Vesper.List.dll" ] do
                    let p = IO.Path.Combine(outDir, stale)

                    if IO.File.Exists p then
                        IO.File.Delete p

                Codegen.materialiseApp project artifact

                Expect.isTrue (IO.File.Exists(IO.Path.Combine(outDir, "Vesper.Printf.dll"))) "Vesper.Printf.dll shipped"
                Expect.isFalse (IO.File.Exists(IO.Path.Combine(outDir, "FSharp.Core.dll"))) "no FSharp.Core.dll"

                Expect.isFalse
                    (IO.File.Exists(IO.Path.Combine(outDir, "Vesper.Core.dll")))
                    "no Vesper.Core.dll — the program forms no function value"

                Expect.isFalse
                    (IO.File.Exists(IO.Path.Combine(outDir, "Vesper.List.dll")))
                    "no Vesper.List.dll — the program has no list"
            }

            // R4 — the emitted FSharp.Core reference identity comes from the referenced
            // file, not the compiler host. (Numerically the same here, since the
            // reference *is* the host's FSharp.Core; the assertion proves the version is
            // sourced from the file the project references rather than guessed.)
            test "an FSharp.Core in References drives the emitted reference identity" {
                let fsCorePath = typeof<Microsoft.FSharp.Core.Unit>.Assembly.Location

                let project =
                    { ProjectInfo.defaults "XParsecR4Identity" with
                        References = [ fsCorePath ]
                    }

                let src = "printfn \"%A\" 42"
                let lexed, file = parseFile src
                let tast = Pipeline.analyse MockBuiltins.provider src lexed file
                let artifact = Codegen.compile MockBuiltins.provider project tast

                Expect.contains artifact.ReferencedAssemblies "FSharp.Core" "the %A cold path references FSharp.Core"

                let asm = loadAssembly (Codegen.toBytes artifact)

                let fsRef =
                    asm.GetReferencedAssemblies() |> Array.find (fun a -> a.Name = "FSharp.Core")

                let expected = AssemblyName.GetAssemblyName(fsCorePath).Version

                Expect.equal
                    fsRef.Version
                    expected
                    "the emitted FSharp.Core ref version is read off the referenced file"
            }
        ]
