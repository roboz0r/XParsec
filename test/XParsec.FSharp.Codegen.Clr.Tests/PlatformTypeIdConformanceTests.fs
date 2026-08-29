module XParsec.FSharp.Codegen.Clr.Tests.PlatformTypeIdConformanceTests

open Expecto

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr

// A primitive's CLR platform type id is authored once, in its `.fs` declaration
// (`type int = (# "System.Int32" #)`), and nothing forces the IL encoder to know that
// string: a mismatch surfaces only as a `failwithf` at the first site that encodes it.

let private intrinsics =
    (ClrSymbolProviders.buildContract [ TestHelpers.vesperCorePackage ]).IntrinsicTypeMap

/// A bare `.fsi` canon name (`int`) under the qualified key (`Vesper.int`) the axis uses.
let private tryPlatform (canon: string) : PlatformTypeId option =
    match IntrinsicTypeMap.tryPlatformTypeId (RuntimeNames.primitiveKey canon) intrinsics with
    | ValueSome typeId -> Some typeId
    | ValueNone -> None

/// The scalar primitives the IL encoder writes DIRECTLY, by their `.fsi` canon name.
/// Aliases (`int32`, `uint`) are abbreviations that dealias to these, so carry no binding.
let private directScalarCanons =
    [
        "int"
        "uint32"
        "int64"
        "uint64"
        "sbyte"
        "byte"
        "int16"
        "uint16"
        "float"
        "float32"
        "bool"
        "char"
        "string"
        // The one pair whose type id is an IL signature spelling rather than a BCL name:
        // `native int` / `unsigned native int` are element types, so no `TypeRef`.
        "nativeint"
        "unativeint"
    ]

[<Tests>]
let tests =
    testList
        "PlatformTypeIdConformance"
        [
            test "every directly-encodable scalar primitive's extracted type id is encodable" {
                for canon in directScalarCanons do
                    match tryPlatform canon with
                    | Some typeId ->
                        Expect.isTrue
                            (PlatformTypeIds.isEncodableValueType typeId)
                            (sprintf
                                "scalar primitive '%s' extracts type id '%s', which the IL encoder cannot encode"
                                canon
                                typeId.Value)
                    | None ->
                        failtestf "scalar primitive '%s' has no extracted CLR type id in the contract forward map" canon
            }

            test "the TypeRef-backed scalars are extracted but NOT direct value types" {
                // These two are written through a dedicated `TypeRef` arm, so they are
                // deliberately absent from the direct-encoding table.
                Expect.equal (tryPlatform "decimal") (Some(PlatformTypeId "System.Decimal")) "decimal → System.Decimal"
                Expect.equal (tryPlatform "unit") (Some(PlatformTypeId "System.ValueTuple")) "unit → System.ValueTuple"

                Expect.isFalse
                    (PlatformTypeIds.isEncodableValueType (PlatformTypeId "System.Decimal"))
                    "System.Decimal is TypeRef-backed, not a direct value type"

                Expect.isFalse
                    (PlatformTypeIds.isEncodableValueType (PlatformTypeId "System.ValueTuple"))
                    "System.ValueTuple is TypeRef-backed, not a direct value type"
            }
        ]
