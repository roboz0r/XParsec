module XParsec.FSharp.Codegen.Clr.Tests.SymbolProviderWiringTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The `ClrSymbolProviders.buildContract` composite is the one stack threaded through both phases,
// and it has no mock backstop: operators, `hash`, `failwith`, printf and `List.fold` all
// resolve from the `Vesper.*` signature files.

[<Tests>]
let tests =
    testList
        "SymbolProviderWiring"
        [
            test
                "buildContract [] resolves no values (the List.fold backstop is gone); List.fold comes from the contract" {
                // With no manifests the stack is the metadata reader alone, and metadata
                // resolves no values at all.
                let empty = ClrSymbolProviders.buildContract []

                Expect.isTrue
                    (empty.TryLookup "List.fold" |> ValueOption.isNone)
                    "List.fold no longer comes from a backstop (it is a Vesper.List contract symbol now)"

                Expect.isTrue
                    (empty.TryLookup "op_Addition" |> ValueOption.isNone)
                    "op_Addition no longer comes from the backstop (it is a contract symbol now)"

                Expect.isTrue (empty.TryLookup "no.such.symbol" |> ValueOption.isNone) "unknown name misses"

                // With the Vesper.List manifest, `fold` resolves under its SOURCE-qualified
                // name (`Vesper.Collections.List.fold`, not compiled `ListModule.fold`).
                let contract =
                    ClrSymbolProviders.buildContract [ vesperListPackage; vesperCorePackage ]

                match contract.TryLookup "Vesper.Collections.List.fold" with
                | ValueSome _ -> ()
                | ValueNone -> failtest "List.fold resolves (source-qualified) from the Vesper.List contract"
            }

            test "the Vesper.Core manifest layer adds type + operator resolution from the contract" {
                let provider = ClrSymbolProviders.buildContract [ vesperCorePackage ]

                // The provider answers the QUALIFIED name; short names come from the
                // ambient open scope. `int` is an `extern` paired with a `.fs`
                // `(# "System.Int32" #)`, so it surfaces `Intrinsic`, not opaque `Class`.
                match provider.TryLookupType "Vesper.int" |> ExternalSymbols.typeShapeOf with
                | ValueSome(ExternalTypeShape.Intrinsic {
                                                            Id = {
                                                                     Canon = canon
                                                                     Platform = IntrinsicPlatform.Repr platform
                                                                 }
                                                        }) ->
                    Expect.equal canon (RuntimeNames.intKey) "int's canon identity is the `.fsi` name"

                    Expect.equal
                        platform
                        "System.Int32"
                        "int's platform name is its prim-types-min `.fs` CLI representation"
                | other -> failtestf "expected Vesper.int as an Intrinsic shape from the manifest layer, got %A" other

                // An operator resolves only under its qualified `[<AutoOpen>]`-module name,
                // so a bare `op_Addition` misses the provider while the qualified name hits.
                Expect.isTrue
                    (provider.TryLookup "op_Addition" |> ValueOption.isNone)
                    "bare op_Addition is a provider miss (no mock backstop)"

                match provider.TryLookup "Vesper.ArithmeticOperators.op_Addition" with
                | ValueSome _ -> ()
                | ValueNone -> failtest "op_Addition resolves (qualified) from the Vesper.Core contract"
            }

            test "a program compiles + runs through the contract-backed stack" {
                // One contract-backed provider across both phases, with operators and
                // printf coming from the contract.
                let _, artifact =
                    compileSourceWith defaultPackages "ManifestWiring" "printfn \"%d\" (1 + 2)"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "3" "1 + 2 printed through the contract-backed stack"
            }
        ]
