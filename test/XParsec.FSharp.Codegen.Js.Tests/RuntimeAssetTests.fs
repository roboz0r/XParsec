module XParsec.FSharp.Codegen.Js.Tests.RuntimeAssetTests

open System
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// The committed runtime assets import EACH OTHER: `Vesper.Seq.mjs` names `./Vesper.Array.mjs`
// and `./Vesper.Core.mjs`. A program reaching Seq without reaching either directly must ship
// all three, or the written output carries a specifier Node cannot resolve.

/// `jsPackages` plus `Vesper.Seq`'s: the contract of a program that CONSUMES the Seq
/// package, so `Seq.*` resolves to the committed asset rather than to a compiled module.
let private seqConsumerContract: Lazy<SymbolProviders.Contract> =
    lazy JsNativeSymbols.jsNativeContract (jsPackages @ [ srcPackage "Vesper.Seq" ])

/// Compile `input` against `seqConsumerContract`, writing into `tmp/<name>/` so the artifact
/// and the assets it selects can be run under Node exactly as materialised.
let private compileSeqConsumer (name: string) (input: string) : JsArtifact =
    let project =
        { JsProjectInfo.defaults name with
            OutputPath = Some(IO.Path.Combine(tmpDir name, name + ".mjs"))
            Source = Some(jsSource (name + ".fsx") input)
        }

    Codegen.compileWith seqConsumerContract.Value project (frozenImplJs seqConsumerContract.Value.Provider input)

/// `Seq.truncate` and `Seq.toArray` are the only external functions; nothing here names the
/// Array or Core packages, whose assets `Vesper.Seq.mjs` imports on its own account. The
/// source is an ARRAY, which carries the `seq<'T>` capability — built by the raw `newarr`
/// intrinsic, since an array LITERAL lowers through a module this contract does not serve.
let private seqConsumer =
    String.concat
        "\n"
        [
            "let build () ="
            "    let a : int[] = (# \"newarr !0\" type (int) 5 : int[] #)"
            "    a.[0] <- 3"
            "    a.[1] <- 1"
            "    a.[2] <- 4"
            "    a.[3] <- 1"
            "    a.[4] <- 5"
            "    a"
            "let first3 = Seq.toArray (Seq.truncate 3 (build ()))"
            "printfn \"%d\" first3.Length"
        ]

let private fileNames (assets: JsRuntimeModule list) : string list =
    assets |> List.map (fun a -> a.FileName)

/// A `JsImports` over `runtime` with `assembly`'s asset referenced once, as a type import.
let private importsReferencing (runtime: (string * JsRuntimeModule) list) (assembly: string) : JsImports =
    let imports = JsImports.create (Map.ofList runtime)
    JsImports.addTypeRef imports (JsHome.ofAssembly assembly) "T" |> ignore
    imports

[<Tests>]
let tests =
    testList
        "Codegen.Js runtime assets"
        [
            test "an asset's own imports are read off its text" {
                let asset =
                    JsRuntimeModule.ofSource
                        "Vesper.Seq.mjs"
                        (IO.File.ReadAllText(srcFile "Vesper.Seq" "Vesper.Seq.mjs"))

                Expect.equal
                    asset.Imports
                    [ JsModulePath.asset "Vesper.Array.mjs"; JsModulePath.asset "Vesper.Core.mjs" ]
                    "the two sibling assets its import block names"
            }

            test "a bare specifier is the host's to resolve, so it is not an import of ours" {
                let asset =
                    JsRuntimeModule.ofSource
                        "host.mjs"
                        "import { readFileSync } from \"node:fs\";\nexport const marker = \"./decoy.mjs\";\n"

                Expect.isEmpty asset.Imports "neither the node builtin nor a specifier-shaped string value"
            }

            test "selecting an asset selects the assets it imports" {
                let runtime =
                    [
                        "Leaf", JsRuntimeModule.ofSource "Leaf.mjs" ""
                        "Mid", JsRuntimeModule.ofSource "Mid.mjs" "import { l } from \"./Leaf.mjs\";\n"
                        "Top", JsRuntimeModule.ofSource "Top.mjs" "import { m } from \"./Mid.mjs\";\n"
                        "Unreached", JsRuntimeModule.ofSource "Unreached.mjs" ""
                    ]

                Expect.equal
                    (JsImports.assets (importsReferencing runtime "Top") |> fileNames)
                    [ "Leaf.mjs"; "Mid.mjs"; "Top.mjs" ]
                    "the closure over asset→asset imports, and only what it reaches"
            }

            test "an asset importing a module no package ships is a compile-time failure" {
                let runtime =
                    [
                        "Ghost", JsRuntimeModule.ofSource "Ghost.mjs" "import { x } from \"./Missing.mjs\";\n"
                    ]

                Expect.throws
                    (fun () -> JsImports.assets (importsReferencing runtime "Ghost") |> ignore)
                    "a dangling specifier faults here, not when Node loads the written output"
            }

            test "a program reaching Vesper.Seq ships the assets Vesper.Seq.mjs imports" {
                let artifact = compileSeqConsumer "seq-asset-closure" seqConsumer

                // The premise: Seq is the only asset this program NAMES.
                Expect.equal
                    (artifact.ImportedModules |> List.filter (fun p -> p.Package.IsNone))
                    [ JsModulePath.asset "Vesper.Seq.mjs" ]
                    "neither Array nor Core is imported by the program itself"

                for fileName in [ "Vesper.Array.mjs"; "Vesper.Core.mjs"; "Vesper.Seq.mjs" ] do
                    Expect.contains (fileNames artifact.RuntimeModules) fileName "shipped beside the output"
            }

            test "the materialised program and its assets run under Node" {
                let artifact = compileSeqConsumer "seq-asset-closure-node" seqConsumer
                Codegen.materialise artifact

                match artifact.OutputPath |> Option.bind runNode with
                | None -> skiptest "node not found on PATH"
                | Some(code, out) ->
                    Expect.equal code 0 (sprintf "node exits 0 (%s)" out)
                    Expect.equal (out.Replace("\r", "").Trim()) "3" "truncate 3 of a 5-element sequence"
            }
        ]
