module XParsec.FSharp.Codegen.Clr.Tests.CrossAssemblyEscapeTests

open System.IO
open System.Runtime.Loader
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// A separately-built PRODUCER package exports module functions a CONSUMER binds and `call`s
// through real `AssemblyRef` member-refs: an export that also escapes intra-assembly
// (`addOne`), a tupled group (`addPair(int, int)`), and a lone unit group (`getUnit()`).

/// The producer contract, the arity the consumer reconciles its arguments against.
/// `addPair`'s `*` group is TUPLED (2 flat params); `getUnit`'s is the lone-erasable `unit`.
let private producerFsi =
    String.concat
        "\n"
        [
            "namespace Vesper"
            ""
            "module Producer ="
            "    /// Used higher-order inside the producer (`bumpTwice`) yet exported:"
            "    /// the public-function escape gap. Must still emit a flat static method."
            "    val addOne: x: int -> int"
            "    /// A TUPLED source group → 2 flat CLR params cross-assembly."
            "    val addPair: int * int -> int"
            "    /// A LONE unit group → a parameterless method cross-assembly."
            "    val getUnit: unit -> int"
            "    /// Drives the intra-assembly higher-order use of `addOne`."
            "    val bumpTwice: x: int -> int"
        ]

/// `bumpTwice` passes `addOne` as a value to `applyTwice`, so `addOne` ESCAPES
/// intra-assembly while still being exported. It must therefore keep its flat static method.
let private producerFs =
    String.concat
        "\n"
        [
            "namespace Vesper"
            ""
            "module Producer ="
            "    let addOne x = x + 1"
            "    let addPair (x, y) = x + y"
            "    let getUnit () = 42"
            "    let applyTwice g x = g (g x)"
            "    let bumpTwice x = applyTwice addOne x"
        ]

/// `name` is omitted so it defaults to the directory name (`loadManifest` requires
/// the two match); the consumer references the producer DLL by that same name.
let private producerManifestToml =
    String.concat "\n" [ "[core]"; "files = [\"producer.fsi\"]"; "impl = [\"producer.fs\"]" ]

/// `tmp/EscapeProducer/` — the directory name IS the producer package / assembly
/// name (so the `.fsi`-recorded home assembly matches the emitted DLL's identity).
let private producerDir = tmpDir "EscapeProducer"
let private producerManifestPath = Path.Combine(producerDir, "manifest.toml")

/// Write the producer sources, build `EscapeProducer.dll` through this backend (Vesper.Core
/// injected for `+` / `Vesper.Fun`), load it into the Default ALC, and return its path. Built
/// once. The build itself exercises `bridgeStaticFnEscapes` on the producer side.
let private producerDll: Lazy<string> =
    lazy
        (File.WriteAllText(Path.Combine(producerDir, "producer.fsi"), producerFsi)
         File.WriteAllText(Path.Combine(producerDir, "producer.fs"), producerFs)
         File.WriteAllText(producerManifestPath, producerManifestToml)

         let outPath = Path.Combine(producerDir, "EscapeProducer.dll")

         let project =
             { ProjectInfo.library "EscapeProducer" with
                 OutputPath = Some outPath
                 References = [ vesperCoreDll.Value ]
             }

         let provider = ClrSymbolProviders.buildContract [ vesperCoreManifest ]
         let lexed, file = parseFile producerFs

         let tast =
             Pipeline.analyseFor project.AssemblyName provider (Hashing.originSourceOfText lexed) file

         let errs = tast.Residue.Diagnostics |> Diagnostic.errors

         if not (List.isEmpty errs) then
             failwithf "EscapeProducer build: %s" (errs |> List.map (fun d -> d.Message) |> String.concat "; ")

         let artifact = Codegen.compile provider project tast
         Codegen.materialise artifact
         AssemblyLoadContext.Default.LoadFromAssemblyPath outPath |> ignore
         outPath)

/// Compile a consumer against the default contract stack PLUS the producer manifest (so its
/// module functions resolve, carrying the `ValRepr`), run it in-process, and assert stdout.
let private runConsumer (expected: string list) (src: string) : unit =
    let dll = producerDll.Value

    let provider =
        ClrSymbolProviders.buildContract (defaultManifests @ [ producerManifestPath ])

    let baseProject = withCore (ProjectInfo.defaults "EscapeConsumer")

    let project =
        { baseProject with
            References = baseProject.References @ [ dll ]
        }

    let lexed, file = parseFile src

    let tast =
        Pipeline.analyseFor project.AssemblyName provider (Hashing.originSourceOfText lexed) file

    let errs = tast.Residue.Diagnostics |> Diagnostic.errors

    if not (List.isEmpty errs) then
        failwithf "EscapeConsumer analysis: %s" (errs |> List.map (fun d -> d.Message) |> String.concat "; ")

    let artifact = Codegen.compile provider project tast
    let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
    let actual = output.Replace("\r", "").Trim()
    let want = String.concat "\n" expected

    if exitCode <> 0 then
        failwithf "expected exit 0 but got %d for the consumer\n--- stdout ---\n%s" exitCode actual

    if actual <> want then
        failwithf "expected %A but got %A from the cross-assembly consumer" want actual

[<Tests>]
let tests =
    testList
        "CrossAssemblyEscape"
        [
            test "a consumer binds a producer's escaping exported function + tupled/unit module functions" {
                runConsumer
                    [
                        "6" // Producer.addOne 5 — the escaping export's flat static method
                        "7" // Producer.bumpTwice 5 = addOne (addOne 5) — the intra-assembly higher-order use
                        "7" // Producer.addPair (3, 4) — tupled group → flat addPair(int, int)
                        "42" // Producer.getUnit () — lone unit group → parameterless getUnit()
                    ]
                    (String.concat
                        "\n"
                        [
                            "open Vesper"
                            "printfn \"%d\" (Producer.addOne 5)"
                            "printfn \"%d\" (Producer.bumpTwice 5)"
                            "printfn \"%d\" (Producer.addPair (3, 4))"
                            "printfn \"%d\" (Producer.getUnit ())"
                        ])
            }
        ]
