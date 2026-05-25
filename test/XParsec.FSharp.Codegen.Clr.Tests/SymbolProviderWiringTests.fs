module XParsec.FSharp.Codegen.Clr.Tests.SymbolProviderWiringTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// P1 wiring (symbol-resolution-plan §5 / handoff §6): the `SymbolProviders.build`
// composite stack is the single declaration threaded through both phases. These
// tests pin (a) values fall through the metadata layer (P2) to the `MockBuiltins`
// backstop when no manifest owns them, and (b) a `Vesper.Core`-manifest-backed
// stack still compiles + runs a real program end to end — the manifest layer
// *adds* type resolution on top, while the contract's `[<AutoOpen>]` operators
// fall through to the backstop.

[<Tests>]
let tests =
    testList
        "SymbolProviderWiring"
        [
            test "build [] falls through to the MockBuiltins backstop for values" {
                // The stack is `composite [ MetadataSymbols ; MockBuiltins ]`; the
                // metadata layer (P2) resolves no values, so operators resolve via
                // the backstop and an unknown name misses through both.
                let provider = SymbolProviders.build []

                match provider.TryLookup "op_Addition" with
                | ValueSome _ -> ()
                | ValueNone -> failtest "op_Addition should resolve through the backstop"

                Expect.isTrue (provider.TryLookup "no.such.symbol" |> ValueOption.isNone) "unknown name misses"
            }

            test "the Vesper.Core manifest layer adds type resolution over the backstop" {
                let provider = SymbolProviders.build [ vesperCoreManifest ]

                // Layer 1 (manifest) contributes the `int` type with its origin.
                // Short-name resolution moved out of the provider into the ambient
                // open scope (O3), so the provider answers the qualified name.
                match provider.TryLookupType "Vesper.int" with
                | ValueSome(ExternalTypeShape.Class(_, _, origin)) ->
                    Expect.equal origin.Assembly (Some "Vesper.Core") "int resolves through the manifest layer"
                | other -> failtestf "expected Vesper.int as a Class shape from the manifest layer, got %A" other

                // …while operators still resolve through the lower-priority backstop
                // (they live in the contract's `[<AutoOpen>]` modules, so the
                // namespace fallback doesn't reach them).
                match provider.TryLookup "op_Addition" with
                | ValueSome _ -> ()
                | ValueNone -> failtest "op_Addition should still resolve through the backstop"
            }

            test "a program compiles + runs through the Vesper.Core-manifest-backed stack" {
                // Both phases share the one manifest-backed provider; the program
                // still prints 3, proving the composite stack drives a real compile.
                let _, artifact =
                    compileSourceWith [ vesperCoreManifest ] "P1ManifestWiring" "printfn \"%d\" (1 + 2)"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "3" "1 + 2 printed through the manifest-backed stack"
            }
        ]
