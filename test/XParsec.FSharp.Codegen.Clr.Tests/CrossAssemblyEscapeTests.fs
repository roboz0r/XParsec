module XParsec.FSharp.Codegen.Clr.Tests.CrossAssemblyEscapeTests

open System.IO
open System.Runtime.Loader
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The cross-assembly RUNTIME gate the compiled-form plan left deferred: a separately-built PRODUCER package exports module functions whose flat
// compiled signatures a CONSUMER assembly binds and `call`s through real
// `AssemblyRef` member-refs. It exercises, end-to-end across two emitted DLLs:
//
//   * the escape gap — `addOne` is used higher-order *inside the producer*
//     (`bumpTwice`), so pre-fix it was demoted ENTIRELY to a closure and its flat
//     static method never existed; the consumer's `Producer.addOne` member-ref
//     then bound nothing → `MissingMethodException` at JIT. `forceExportedStaticFns`
//     keeps the flat method, so the consumer binds it.
//   * Step C tupled-group flattening — `addPair (x, y)` (`int * y: int ->` in the
//     `.fsi`) binds a 2-flat-param member-ref `addPair(int, int)`, not a single
//     `ValueTuple` param.
//   * Step C lone-unit erasure — `getUnit ()` (`unit ->`) binds a parameterless
//     member-ref `getUnit()`.
//
// The producer is built through this repo's own backend (like the `buildPackage`
// Vesper.* fixtures) and loaded into the Default ALC so a fresh-ALC consumer run
// resolves it by simple name. Its `.fsi`/`.fs`/`manifest.toml` are written to the
// repo `tmp/` dir so the consumer's contract provider extracts the producer's
// `ValRepr`/`CompiledForm` from the recorded arity — the exact Step C path.

/// The producer contract: the arity the consumer reconciles its call spines
/// against. `addPair`'s `*`-separated group is a TUPLED group (2 flat params);
/// `getUnit`'s `unit` group is the lone-erasable shape.
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
            "    /// A TUPLED source group → 2 flat CLR params cross-assembly (Step C)."
            "    val addPair: int * int -> int"
            "    /// A LONE unit group → a parameterless method cross-assembly (Step C)."
            "    val getUnit: unit -> int"
            "    /// Drives the intra-assembly higher-order use of `addOne`."
            "    val bumpTwice: x: int -> int"
        ]

/// The producer impl. `bumpTwice` passes `addOne` as a value to `applyTwice`, so
/// `addOne` ESCAPES intra-assembly — the condition that, pre-fix, dropped its flat
/// static method.
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
    String.concat
        "\n"
        [
            "[core]"
            "namespace = \"Vesper\""
            "files = [\"producer.fsi\"]"
            "impl = [\"producer.fs\"]"
        ]

/// `tmp/EscapeProducer/` — the directory name IS the producer package / assembly
/// name (so the `.fsi`-recorded home assembly matches the emitted DLL's identity).
let private producerDir = tmpDir "EscapeProducer"
let private producerManifestPath = Path.Combine(producerDir, "manifest.toml")

/// Write the producer sources, build `EscapeProducer.dll` through this backend
/// (Vesper.Core injected for `+` / `Vesper.Fun`), load it into the Default ALC, and
/// return its path. `lazy`, built once. Building it is itself the first time a
/// Vesper package has an intra-assembly escaping exported function — so the build
/// exercises `bridgeStaticFnEscapes` on the producer side.
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

         let provider = SymbolProviders.buildContract [ vesperCoreManifest ]
         let lexed, file = parseFile producerFs
         let tast = Pipeline.analyseFor project.AssemblyName provider producerFs lexed file

         let errs = tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

         if not (List.isEmpty errs) then
             failwithf "EscapeProducer build: %s" (errs |> List.map (fun d -> d.Message) |> String.concat "; ")

         let artifact = Codegen.compile provider project tast
         Codegen.materialise artifact
         AssemblyLoadContext.Default.LoadFromAssemblyPath outPath |> ignore
         outPath)

/// Compile a consumer program against the default contract stack PLUS the producer
/// manifest (so its module functions resolve, carrying the Step C `ValRepr`) with
/// the producer DLL referenced, run it in-process, and assert stdout.
let private runConsumer (expected: string list) (src: string) : unit =
    let dll = producerDll.Value

    let provider =
        SymbolProviders.buildContract (defaultManifests @ [ producerManifestPath ])

    let baseProject = withCore (ProjectInfo.defaults "EscapeConsumer")

    let project =
        { baseProject with
            References = baseProject.References @ [ dll ]
        }

    let lexed, file = parseFile src
    let tast = Pipeline.analyseFor project.AssemblyName provider src lexed file

    let errs = tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)

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
                        "6" // Producer.addOne 5 — the escape-gap method; pre-fix this member-ref bound nothing
                        "7" // Producer.bumpTwice 5 = addOne (addOne 5) — the producer's intra-assembly higher-order use
                        "7" // Producer.addPair (3, 4) — tupled group → flat addPair(int, int) (Step C)
                        "42" // Producer.getUnit () — lone unit group → parameterless getUnit() (Step C)
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
