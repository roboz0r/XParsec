module XParsec.FSharp.SemanticAnalysis.Tests.SubtypeExternalInterfaceKeyTests

open System.Collections.Generic
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// `Tests.Shapes+IShape` re-cut with `qualifiedTypeKeyOf` yields the CLR-nested `InType`
// identity, not the `InModule` one the declaring package registered, so the two fixtures
// below differ only in where the interface is declared.

let private testsNs = SymbolKeyOps.namespaceKey "Tests"

let private shapesModule =
    SymbolKeyOps.moduleKeyOf (SymbolKeyOps.inNamespace "Tests") "Shapes"

/// `Tests.Shapes+IShape`: an interface whose container is a MODULE.
let private moduleHeldIfaceKey =
    SymbolKeyOps.typeKeyOfContainer (TypeContainer.InModule shapesModule) "IShape" 0

/// `Tests.IShape`: the same interface directly under the namespace, as a control.
let private nsHeldIfaceKey =
    SymbolKeyOps.typeKeyOfContainer (TypeContainer.InNamespace testsNs) "IShape" 0

let private widgetKey =
    SymbolKeyOps.typeKeyOfContainer (TypeContainer.InNamespace testsNs) "Widget" 0

let private ifaceShape =
    ExternalTypeShape.Class(ExternalClassShape.basic (0, true, SymbolOrigin.Empty))

/// A class implementing exactly the one interface.
let private widgetShape (ifaceKey: TypeKey) =
    ExternalTypeShape.Class(
        { ExternalClassShape.basic (0, false, SymbolOrigin.Empty) with
            FrozenInterfaces = EqArray.singleton (FrozenNominal.OfClass(ifaceKey, EqArray.empty))
        }
    )

/// Key-INDEXED channels, because name-indexed ones derive their keys with
/// `qualifiedTypeKeyOf` and so cannot express an `InModule` identity at all.
let private providerFor (ifaceKey: TypeKey) : IExternalSymbolProvider =
    let shapes = Dictionary<TypeKey, ExternalTypeShape>()
    shapes.[widgetKey] <- widgetShape ifaceKey
    shapes.[ifaceKey] <- ifaceShape

    let byName = Dictionary<string, TypeKey>()
    byName.[SymbolKeyOps.typeMetaName widgetKey] <- widgetKey
    byName.[SymbolKeyOps.typeMetaName ifaceKey] <- ifaceKey

    ExternalSymbolProviders.ofKeyedChannels (
        ExternalSymbolProviders.KeyedChannels.ofKeyIndexes
            { ExternalSymbolProviders.KeyIndexedChannels.empty with
                ShapesByKey = shapes
                ResolveTypeName =
                    fun n ->
                        match byName.TryGetValue n with
                        | true, k -> ValueSome k
                        | _ -> ValueNone
            }
    )

let private ctxFor (ifaceKey: TypeKey) : PassContext =
    let lexed, _ = parseFile "let x = 1"
    PassContext(providerFor ifaceKey, LexedFile.ofText lexed, CompilingAssembly.none)

// `SemType.TyClass`, never `TestHelpers`' shadow: that shim mints its key with
// `qualifiedTypeKeyOf`, the very re-cut these tests exist to distinguish.
let private upcastsTo (ifaceKey: TypeKey) (tgtKey: TypeKey) : bool =
    UnificationEngineCore.tryUpcastWitness (ctxFor ifaceKey) (SemType.TyClass(widgetKey, EqArray.empty)) tgtKey
    |> ValueOption.isSome

let private upcastsToIface (ifaceKey: TypeKey) : bool = upcastsTo ifaceKey ifaceKey

[<Tests>]
let tests =
    testList
        "SubtypeExternalInterfaceKey"
        [
            // The control: `qualifiedTypeKeyOf` round-trips a namespace-held name, so this
            // arm passed before the walk took the registered key too.
            test "a namespace-held external interface is reached by the subtype walk" {
                Expect.isTrue (upcastsToIface nsHeldIfaceKey) "Widget :> Tests.IShape"
            }

            test "a module-held external interface is reached by the subtype walk" {
                Expect.isTrue (upcastsToIface moduleHeldIfaceKey) "Widget :> Tests.Shapes+IShape"
            }

            // The mechanism the test above guards: both keys render to the same metadata
            // name, so a walk that re-cuts the name gets an identity the provider cannot
            // answer for and the recursion stops one level in.
            test "re-cutting a module-held interface's compiled name yields an unequal key" {
                let compiled = SymbolKeyOps.typeMetaName moduleHeldIfaceKey
                Expect.equal compiled "Tests.Shapes+IShape" "the module segment renders with `+`"

                let reCut = SymbolKeyOps.qualifiedTypeKeyOf compiled 0
                Expect.equal (SymbolKeyOps.typeMetaName reCut) compiled "the re-cut key renders the same name"
                Expect.notEqual reCut moduleHeldIfaceKey "but it is a different identity (InType, not InModule)"
            }

            // The surfaced supertype IS the identity the declaring package published, not one
            // derived from its rendering, so the re-cut spelling denotes a different type and
            // the upcast to it must fail.
            test "a module-held interface is not reached by the re-cut spelling of its own name" {
                let reCut =
                    SymbolKeyOps.qualifiedTypeKeyOf (SymbolKeyOps.typeMetaName moduleHeldIfaceKey) 0

                Expect.isFalse (upcastsTo moduleHeldIfaceKey reCut) "Widget does not implement the CLR-nested IShape"
            }
        ]
