module XParsec.FSharp.Codegen.Clr.Tests.SelfPackageReverseCanonTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

/// A package that DECLARES a primitive must resolve BCL members over it exactly as a
/// consumer of that package does. `composeOrdered` seeds a package's extraction leaf from
/// its DEPENDENCIES and the final composite from every built provider — itself included —
/// so without `ClrCompilation.SelfManifest` the `{ platform -> canon }` map holds
/// `System.String -> Vesper.string` for everyone EXCEPT the package that states it.
///
/// The probe is appended to Vesper.Core's real `impl` list and compiled as Core, so its
/// `string` is the `TyConst Vesper.string` Core's own `.fs` binds — the situation
/// `prim-types-string.fs` is in, not a reconstruction of it. Both directions are asserted:
/// unseeded it must FAIL, or the test would pass for a reason unrelated to the seed.
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
                    | Ok _ -> failtest "expected no overload — the seed would then be guarding nothing"
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

                // The seed's whole claim is that the two views agree. `string` is the one
                // that motivated it; `int`/`obj`/`exn` ride the same map and are asserted
                // here so a partial seed cannot pass.
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
