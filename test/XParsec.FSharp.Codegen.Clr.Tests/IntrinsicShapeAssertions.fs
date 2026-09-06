/// Assertions over the shape a Vesper primitive resolves to through a symbol store.
module XParsec.FSharp.Codegen.Clr.Tests.IntrinsicShapeAssertions

open Expecto
open XParsec.FSharp.SemanticAnalysis

/// `Vesper.int` resolves through `store` as an `Intrinsic` whose canon identity is
/// `RuntimeNames.intKey` and whose platform type is `System.Int32`: the `.fsi` name paired
/// with the prim-types-min `.fs` `(# "System.Int32" #)` representation. Returns the canon
/// key and the platform id for further assertions.
let assertVesperIntIsBclInt32 (store: IExternalSymbolStore) : TypeKey * PlatformTypeId =
    match ExternalSymbols.tryMetaType store "Vesper.int" with
    | ValueSome(ExternalTypeShape.Intrinsic {
                                                Id = {
                                                         Canon = canon
                                                         Platform = IntrinsicPlatform.Bound platform
                                                     }
                                            }) ->
        Expect.equal canon RuntimeNames.intKey "int's canon identity is the `.fsi` name"
        Expect.equal platform (PlatformTypeId "System.Int32") "int's platform type is the BCL Int32"
        canon, platform
    | other -> failtestf "expected Vesper.int as an Intrinsic shape, got %A" other
