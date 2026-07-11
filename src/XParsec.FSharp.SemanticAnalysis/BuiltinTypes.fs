namespace XParsec.FSharp.SemanticAnalysis

/// The primitive `SemType`s the front end mints directly — the types literals and
/// built-in constructs infer to (`Unification` / `Freeze`). Each is a `TyConst`
/// over a `RuntimeNames` key, so identity is the key, never the spelling; codegen
/// maps each key to its target representation via `IntrinsicRepr`.
module BuiltinTypes =

    let tyInt: SemType = TyConst(RuntimeNames.intKey, EqArray.empty)
    let tyInt64: SemType = TyConst(RuntimeNames.int64Key, EqArray.empty)
    let tyByte: SemType = TyConst(RuntimeNames.byteKey, EqArray.empty)
    let tySByte: SemType = TyConst(RuntimeNames.primitiveKey "sbyte", EqArray.empty)
    let tyInt16: SemType = TyConst(RuntimeNames.primitiveKey "int16", EqArray.empty)
    let tyUInt16: SemType = TyConst(RuntimeNames.primitiveKey "uint16", EqArray.empty)
    let tyUInt32: SemType = TyConst(RuntimeNames.uint32Key, EqArray.empty)
    let tyUInt64: SemType = TyConst(RuntimeNames.primitiveKey "uint64", EqArray.empty)

    let tyNativeInt: SemType =
        TyConst(RuntimeNames.primitiveKey "nativeint", EqArray.empty)

    let tyUNativeInt: SemType =
        TyConst(RuntimeNames.primitiveKey "unativeint", EqArray.empty)

    let tyFloat: SemType = TyConst(RuntimeNames.floatKey, EqArray.empty)
    let tyFloat32: SemType = TyConst(RuntimeNames.primitiveKey "float32", EqArray.empty)
    let tyBool: SemType = TyConst(RuntimeNames.boolKey, EqArray.empty)
    let tyChar: SemType = TyConst(RuntimeNames.charKey, EqArray.empty)
    let tyDecimal: SemType = TyConst(RuntimeNames.decimalKey, EqArray.empty)
    let tyUnit: SemType = TyConst(RuntimeNames.unitKey, EqArray.empty)
    let tyString: SemType = TyConst(RuntimeNames.stringKey, EqArray.empty)
