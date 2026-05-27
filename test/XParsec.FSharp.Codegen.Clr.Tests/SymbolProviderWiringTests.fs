module XParsec.FSharp.Codegen.Clr.Tests.SymbolProviderWiringTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Wiring (symbol-resolution-plan §5 / handoff §6) + contract-as-provider demotion
// (handoff): the `SymbolProviders.build` composite stack is the single declaration
// threaded through both phases. The demotion is now TOTAL — `MockBuiltins` is gone
// from the stack entirely (no `List.fold` backstop). Operators / `hash` / `failwith`
// / printf / `List.fold` all resolve from the `Vesper.*` `.fsi` contracts. These
// tests pin (a) an empty manifest set resolves NOTHING (no backstop), while the
// `List.fold` source-qualified name resolves from the `Vesper.List` contract, and
// (b) a contract-backed stack compiles + runs a real program end to end.

[<Tests>]
let tests =
    testList
        "SymbolProviderWiring"
        [
            test "build [] resolves no values (the List.fold backstop is gone); List.fold comes from the contract" {
                // The demoted stack is `composite [ MetadataSymbols ]` — the metadata
                // layer (P2) resolves no values, and there is no longer any mock
                // backstop, so an empty manifest set resolves nothing.
                let empty = SymbolProviders.build []

                Expect.isTrue
                    (empty.TryLookup "List.fold" |> ValueOption.isNone)
                    "List.fold no longer comes from a backstop (it is a Vesper.List contract symbol now)"

                Expect.isTrue
                    (empty.TryLookup "op_Addition" |> ValueOption.isNone)
                    "op_Addition no longer comes from the backstop (it is a contract symbol now)"

                Expect.isTrue (empty.TryLookup "no.such.symbol" |> ValueOption.isNone) "unknown name misses"

                // With the Vesper.List manifest, `fold` resolves under its
                // source-qualified name (`List.fold`, not the compiled
                // `ListModule.fold`) — the ModuleSuffix source-name alias
                // (`VesperLib.extractValSig`) the front end probes through the ambient.
                let contract = SymbolProviders.build [ vesperListManifest; vesperCoreManifest ]

                match contract.TryLookup "Vesper.Collections.List.fold" with
                | ValueSome _ -> ()
                | ValueNone -> failtest "List.fold resolves (source-qualified) from the Vesper.List contract"
            }

            test "the Vesper.Core manifest layer adds type + operator resolution from the contract" {
                let provider = SymbolProviders.build [ vesperCoreManifest ]

                // Layer 1 (manifest) contributes the `int` type with its origin.
                // Short-name resolution moved out of the provider into the ambient
                // open scope (O3), so the provider answers the qualified name.
                match provider.TryLookupType "Vesper.int" with
                | ValueSome(ExternalTypeShape.Class info) ->
                    Expect.equal info.Origin.Assembly (Some "Vesper.Core") "int resolves through the manifest layer"
                | other -> failtestf "expected Vesper.int as a Class shape from the manifest layer, got %A" other

                // Operators now resolve from the contract — but only under their
                // *qualified* `[<AutoOpen>]`-module name (the pipeline reaches the
                // bare name via the ambient open scope, not a direct provider probe).
                // So a bare `op_Addition` is a miss while the qualified name hits.
                Expect.isTrue
                    (provider.TryLookup "op_Addition" |> ValueOption.isNone)
                    "bare op_Addition is a provider miss (no mock backstop)"

                match provider.TryLookup "Vesper.ArithmeticOperators.op_Addition" with
                | ValueSome _ -> ()
                | ValueNone -> failtest "op_Addition resolves (qualified) from the Vesper.Core contract"
            }

            test "a program compiles + runs through the contract-backed stack" {
                // Both phases share the one contract-backed provider; the program
                // still prints 3, proving the composite stack drives a real compile
                // with operators + printf sourced from the contract (no mock).
                let _, artifact =
                    compileSourceWith defaultManifests "ManifestWiring" "printfn \"%d\" (1 + 2)"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "3" "1 + 2 printed through the contract-backed stack"
            }
        ]
