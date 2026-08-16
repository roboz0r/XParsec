module XParsec.FSharp.Codegen.Clr.Tests.SelfPackageIntrinsicsTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

/// A package that DECLARES a primitive must resolve BCL members over it exactly as a consumer
/// of that package does. A package's extraction reader is seeded from its DEPENDENCIES, so
/// without a self manifest `System.String -> Vesper.string` holds for everyone EXCEPT Core.
module SelfPackageIntrinsicsTests =

    let private probeSource =
        """
namespace Vesper

module ConcatProbe =

    let inline probeConcat (x: string) (y: string) : string = System.String.Concat(x, y)
"""

    let private coreFilesPlusProbe () =
        let coreUnits =
            match
                ReferencedProject.resolveManifest Target.Clr vesperCorePackage
                |> Result.mapError PackageSetFault.describe
                |> Result.bind PackageUnits.ofManifest
            with
            | Ok units -> units
            | Error e -> failwithf "cannot load Vesper.Core manifest: %s" e

        coreUnits
        @ [
            AssemblyFiles.parseUnit
                Set.empty
                (AssemblyFiles.SourceUnit.ofImplementation (
                    AssemblyFiles.SourceFile.ofText "concat-probe.fs" probeSource
                ))
        ]

    /// Compiled AS Vesper.Core, the probe appended to Core's real `impl` list, so the probe's
    /// `string` is the `TyConst Vesper.string` Core's own `.fs` binds.
    let private compileProbeAsCore (selfManifest: string option) =
        // GATED, so a contract that failed to resolve is reported as itself rather than as the
        // missing-overload verdict this test is about.
        ClrSymbolProviders.contractForSelf selfManifest []
        |> SymbolProviders.Contract.gate
        |> Result.bind (fun contract ->
            ClrDriver.compileAssemblyWith
                []
                contract.Provider
                (ProjectInfo.library "Vesper.Core")
                (coreFilesPlusProbe ())
        )

    [<Tests>]
    let tests =
        testList
            "SelfPackageIntrinsics"
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
                    match compileProbeAsCore (Some vesperCorePackage) with
                    | Ok _ -> ()
                    | Error diags ->
                        let text = diags |> List.map (fun d -> d.Diagnostic.Message) |> String.concat "\n"
                        failtestf "seeded compile should succeed, got:\n%s" text
                }

                // `string` motivated the seed; `int`/`obj`/`exn` ride the same axis and are
                // asserted here so a partial seed cannot pass.
                test "the self axis is the axis a consumer of the package sees" {
                    let selfAxis = ClrSymbolProviders.selfIntrinsics (Some vesperCorePackage)

                    let consumerAxis =
                        (ClrSymbolProviders.buildContract [ vesperCorePackage ]).IntrinsicTypeMap

                    Expect.equal selfAxis consumerAxis "self and consumer resolve BCL names through one axis"

                    for platform in [ "System.String"; "System.Int32"; "System.Object"; "System.Exception" ] do
                        Expect.isNonEmpty
                            (IntrinsicTypeMap.canonsOf platform selfAxis)
                            (sprintf "%s reconciles to a Vesper canon" platform)
                }
            ]
