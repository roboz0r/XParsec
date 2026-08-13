module XParsec.FSharp.Codegen.Clr.Tests.IntrinsicReprConformanceTests

open Expecto

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr

// A primitive's CLR representation is authored once, in its `.fs` declaration
// (`type int = (# "System.Int32" #)`), and nothing forces the IL encoder to know that
// string: a mismatch surfaces only as a `failwithf` at the first site that encodes it.

let private intrinsics =
    (ClrSymbolProviders.buildContract [ TestHelpers.vesperCorePackage ]).IntrinsicTypeMap

/// A bare `.fsi` canon name (`int`) under the qualified key (`Vesper.int`) the axis uses.
let private tryRepr (canon: string) : string option =
    match IntrinsicTypeMap.tryPlatformRepr (RuntimeNames.primitiveKey canon) intrinsics with
    | ValueSome repr -> Some repr
    | ValueNone -> None

/// The scalar primitives the IL encoder writes DIRECTLY, by their `.fsi` canon name.
/// Aliases (`int32`, `uint`) are abbreviations that dealias to these, so carry no repr.
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
        // The one pair whose repr is an IL signature spelling rather than a BCL name:
        // `native int` / `unsigned native int` are element types, so no `TypeRef`.
        "nativeint"
        "unativeint"
    ]

[<Tests>]
let tests =
    testList
        "IntrinsicReprConformance"
        [
            test "every directly-encodable scalar primitive's extracted repr is encodable" {
                for canon in directScalarCanons do
                    match tryRepr canon with
                    | Some repr ->
                        Expect.isTrue
                            (IntrinsicRepr.isEncodableValueType repr)
                            (sprintf
                                "scalar primitive '%s' extracts repr '%s', which the IL encoder cannot encode"
                                canon
                                repr)
                    | None ->
                        failtestf "scalar primitive '%s' has no extracted CLR repr in the contract forward map" canon
            }

            test "the TypeRef-backed scalars are extracted but NOT direct value types" {
                // These two are written through a dedicated `TypeRef` arm, so they are
                // deliberately absent from the direct-encoding table.
                Expect.equal (tryRepr "decimal") (Some "System.Decimal") "decimal → System.Decimal"
                Expect.equal (tryRepr "unit") (Some "System.ValueTuple") "unit → System.ValueTuple"

                Expect.isFalse
                    (IntrinsicRepr.isEncodableValueType "System.Decimal")
                    "System.Decimal is TypeRef-backed, not a direct value type"

                Expect.isFalse
                    (IntrinsicRepr.isEncodableValueType "System.ValueTuple")
                    "System.ValueTuple is TypeRef-backed, not a direct value type"
            }
        ]
