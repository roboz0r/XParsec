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
            "    /// A published abbreviation: a consumer writes the name, and every writing"
            "    /// expands to `int * int`. No type is emitted for it."
            "    type Pair = int * int"
            "    val sumPair: p: Pair -> int"
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
            "    type Pair = int * int"
            "    let sumPair (p: Pair) ="
            "        let (a, b) = p"
            "        a + b"
        ]

/// `name` is omitted so it defaults to the directory name, which is what the consumer
/// references the producer DLL by. `int` is Vesper.Core's, and a contract resolves only what its
/// own dependencies declare, so this fixture under `tmp/` spells the way back to `src/`.
let private producerManifestToml =
    String.concat
        "\n"
        [
            "[core]"
            "depends-on = [\"../../src/Vesper.Core\"]"
            "files = [\"producer.fsi\", \"producer.fs\"]"
        ]

/// `tmp/EscapeProducer/` — the directory name IS the producer package / assembly
/// name (so the `.fsi`-recorded home assembly matches the emitted DLL's identity).
let private producerDir = tmpDir "EscapeProducer"
let private producerManifestPath = Path.Combine(producerDir, "manifest.clr.toml")

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

         let provider = ClrSymbolProviders.buildContract [ vesperCorePackage ]
         let artifact = compileAgainst provider project producerFs

         Codegen.materialise artifact
         AssemblyLoadContext.Default.LoadFromAssemblyPath outPath |> ignore
         outPath)

/// Compile a consumer against the default contract stack PLUS the producer manifest (so its
/// module functions resolve, carrying the `ValRepr`), run it in-process, and assert stdout.
let private runConsumer (expected: string list) (src: string) : unit =
    let dll = producerDll.Value

    let provider = ClrSymbolProviders.buildContract (defaultPackages @ [ producerDir ])

    let baseProject = withCore (ProjectInfo.defaults "EscapeConsumer")

    let project =
        { baseProject with
            References = baseProject.References @ [ dll ]
        }

    let artifact = compileAgainst provider project src

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

            // The producer's `.fsi` publishes `Pair` as an abbreviation and its DLL carries no
            // type for it. The consumer writes the name qualified and bare; both expand to
            // `int * int`, which is what `sumPair` takes across the boundary.
            test "a consumer writes a producer's published type abbreviation, which expands to its body" {
                let typeNames = PeInspection.peTypeDefNames (File.ReadAllBytes producerDll.Value)

                Expect.isFalse
                    (typeNames |> List.exists (fun n -> n = "Pair" || n.EndsWith ".Pair"))
                    (sprintf "no type is emitted for the abbreviation `Pair`; types = %A" typeNames)

                runConsumer
                    [
                        "7" // sumPair through a `Producer.Pair`-annotated value
                        "30" // sumPair through a bare `Pair` under `open Producer`
                    ]
                    (String.concat
                        "\n"
                        [
                            "open Vesper"
                            "let p: Producer.Pair = (3, 4)"
                            "printfn \"%d\" (Producer.sumPair p)"
                            "open Producer"
                            "let q: Pair = (10, 20)"
                            "printfn \"%d\" (sumPair q)"
                        ])
            }
        ]
