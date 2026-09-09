module XParsec.FSharp.SemanticAnalysis.Tests.KeyIndexedChannelsTests

open System.Collections.Generic
open Vesper
open Expecto
open XParsec.FSharp.SemanticAnalysis

// A key-indexed source publishes index signatures under the identity it registered. The
// module-held fixture is the one a name-indexed source cannot express: `Tests.Shapes+Bag`
// re-cut from its rendering is an `InType` identity, unequal to the `InModule` one.

let private shapesModule =
    SymbolKeyOps.moduleKeyOf (SymbolKeyOps.inNamespace "Tests") "Shapes"

let private bagKey =
    SymbolKeyOps.typeKeyOfContainer (TypeContainer.InModule shapesModule) "Bag" 0<typeSlot>

let private plainKey =
    SymbolKeyOps.typeKeyOfContainer (TypeContainer.InNamespace(SymbolKeyOps.namespaceKey "Tests")) "Plain" 0<typeSlot>

let private stringTy = FTConst(RuntimeNames.opaqueKey "string", Block.empty)
let private intTy = FTConst(RuntimeNames.opaqueKey "int", Block.empty)

let private provider: IExternalSymbolProvider =
    let byKey =
        Dictionary<TypeKey, (FrozenType * FrozenType) list>(HashIdentity.Structural)

    byKey.[bagKey] <- [ stringTy, intTy ]

    ExternalSymbolProviders.ofKeyIndexedChannels
        { ExternalSymbolProviders.KeyIndexedChannels.empty with
            IndexSignaturesByKey = byKey
        }

[<Tests>]
let tests =
    testList
        "KeyIndexedChannels"
        [
            test "a module-held type's index signature is read by its registered key" {
                Expect.equal (provider.TryLookupIndexSignature bagKey) [ stringTy, intTy ] "{ [k: string]: int }"
            }

            test "an unregistered key has no index signature" {
                Expect.isEmpty (provider.TryLookupIndexSignature plainKey) "Plain declares none"
            }

            test "the re-cut spelling of a module-held name is a different identity" {
                let reCut = SymbolKeyOps.qualifiedTypeKeyOf (SymbolKeyOps.typeMetaName bagKey) 0

                Expect.isEmpty (provider.TryLookupIndexSignature reCut) "Tests.Shapes+Bag as an InType key"
            }
        ]
