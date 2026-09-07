namespace XParsec.FSharp.SemanticAnalysis

open Vesper

/// The primitive `SemType`s the front end mints directly: the types literals and built-in
/// constructs infer to, each a `TyConst` over a `RuntimeNames` key rather than a spelling.
module BuiltinTypes =

    let tyInt: SemType = TyConst(RuntimeNames.intKey, Block.empty)
    let tyInt64: SemType = TyConst(RuntimeNames.int64Key, Block.empty)
    let tyByte: SemType = TyConst(RuntimeNames.byteKey, Block.empty)
    let tySByte: SemType = TyConst(RuntimeNames.sbyteKey, Block.empty)
    let tyInt16: SemType = TyConst(RuntimeNames.int16Key, Block.empty)
    let tyUInt16: SemType = TyConst(RuntimeNames.uint16Key, Block.empty)
    let tyUInt32: SemType = TyConst(RuntimeNames.uint32Key, Block.empty)
    let tyUInt64: SemType = TyConst(RuntimeNames.uint64Key, Block.empty)
    let tyNativeInt: SemType = TyConst(RuntimeNames.nativeintKey, Block.empty)
    let tyUNativeInt: SemType = TyConst(RuntimeNames.unativeintKey, Block.empty)
    let tyFloat: SemType = TyConst(RuntimeNames.floatKey, Block.empty)
    let tyFloat32: SemType = TyConst(RuntimeNames.float32Key, Block.empty)
    let tyBool: SemType = TyConst(RuntimeNames.boolKey, Block.empty)
    let tyChar: SemType = TyConst(RuntimeNames.charKey, Block.empty)
    let tyDecimal: SemType = TyConst(RuntimeNames.decimalKey, Block.empty)
    let tyUnit: SemType = TyConst(RuntimeNames.unitKey, Block.empty)
    let tyString: SemType = TyConst(RuntimeNames.stringKey, Block.empty)
    let tyObj: SemType = TyConst(RuntimeNames.objKey, Block.empty)
