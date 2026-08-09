module XParsec.FSharp.Codegen.Clr.Tests.SelfPackageReverseCanonTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

/// A package that DECLARES a primitive must resolve BCL members over it exactly as a consumer
/// of that package does. A package's extraction leaf is seeded from its DEPENDENCIES, so
/// without a self manifest `System.String -> Vesper.string` holds for everyone EXCEPT Core.
module SelfPackageReverseCanonTests =

    let private probeSource =
        """
namespace Vesper

module ConcatProbe =

    let inline probeConcat (x: string) (y: string) : string = System.String.Concat(x, y)
"""

    let private coreFilesPlusProbe () =
        let implFiles =
            match ReferencedProject.loadManifest vesperCoreManifest with
            | Ok m -> ReferencedProject.resolveImpl Target.Clr m
            | Error e -> failwithf "cannot load Vesper.Core manifest: %s" e

        (implFiles
         |> List.map (fun rel -> vesperCoreSource rel, System.IO.File.ReadAllText(vesperCoreSource rel)))
        @ [ "concat-probe.fs", probeSource ]

    /// Compiled AS Vesper.Core, the probe appended to Core's real `impl` list, so the probe's
    /// `string` is the `TyConst Vesper.string` Core's own `.fs` binds.
    let private compileProbeAsCore (selfManifest: string option) =
        ClrDriver.compileAssemblyWith
            Pipeline.analyseFor
            []
            (ClrSymbolProviders.buildContractForSelf selfManifest Target.Clr [])
            (ProjectInfo.library "Vesper.Core")
            (coreFilesPlusProbe ())

    [<Tests>]
    let tests =
        testList
            "SelfPackageReverseCanon"
            [
                test "without the self manifest a BCL call over Vesper.string finds no overload" {
                    match compileProbeAsCore None with
                    | Ok _ -> failtest "expected no overload; otherwise the seed would be guarding nothing"
                    | Error diags ->
                        let text = diags |> List.map (fun d -> d.Diagnostic.Message) |> String.concat "\n"

                        Expect.stringContains
                            text
                            "overload of 'Concat'"
                            (sprintf "expected the overload failure the seed fixes, got:\n%s" text)
                }

                test "with the self manifest it resolves, as it does for a consumer" {
                    match compileProbeAsCore (Some vesperCoreManifest) with
                    | Ok _ -> ()
                    | Error diags ->
                        let text = diags |> List.map (fun d -> d.Diagnostic.Message) |> String.concat "\n"
                        failtestf "seeded compile should succeed, got:\n%s" text
                }

                // `string` motivated the seed; `int`/`obj`/`exn` ride the same map and are
                // asserted here so a partial seed cannot pass.
                test "the self axis is the axis a consumer of the package sees" {
                    let selfAxis =
                        ClrSymbolProviders.selfReverseCanon Target.Clr (Some vesperCoreManifest)

                    let consumerAxis =
                        (ClrSymbolProviders.buildContract [ vesperCoreManifest ]).IntrinsicReverseCanon

                    Expect.equal selfAxis consumerAxis "self and consumer resolve BCL names through one map"

                    for platform in [ "System.String"; "System.Int32"; "System.Object"; "System.Exception" ] do
                        Expect.isTrue
                            (selfAxis.ContainsKey platform)
                            (sprintf "%s reconciles to a Vesper canon" platform)
                }
            ]
